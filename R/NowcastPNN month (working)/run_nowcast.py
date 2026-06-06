"""
Nowcast execution using trained Negative Binomial model
Loads saved model and generates predictions for incomplete data (2016-2023)
"""

import os
import pandas as pd
import numpy as np
import torch
import torch.nn as nn
from datetime import datetime

from data_processing import BirthDataProcessor
from birth_nowcast_models import BirthNowcastPNN

def main():
    print("🎯 BIRTH NOWCAST PREDICTION")
    print("=" * 50)
    print(f"Started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    # Load trained model
    model_path = './checkpoints/negative_binomial_model_final.pt'
    print(f"Loading model from: {model_path}")
    
    checkpoint = torch.load(model_path, map_location='cpu', weights_only=False)
    model_config = checkpoint['model_config']
    training_config = checkpoint['training_config']
    processor_state = checkpoint['data_processor_state']
    
    # Initialize data processor with saved configuration
    processor = BirthDataProcessor(
        past_units=processor_state['past_units'],
        max_delay=processor_state['max_delay'],
        min_births_threshold=processor_state['min_births_threshold'],
        nowcast_cutoff_year=processor_state['nowcast_cutoff_year'],
        current_date=pd.to_datetime(training_config['current_date'])
    )
    
    # Load original data (only for building triangles)
    data_path = os.path.join('../../datasets', 'monthly_births.parquet')
    df = pd.read_parquet(data_path)
    # Filtra subito per il periodo di interesse (2016-2023)
    df = df[(df['year_occ'] >= 2016) & (df['year_occ'] <= 2023)]
    # Applica gli stessi filtri del training
    df = processor.filter_reproductive_ages(df)
    df = processor.apply_birth_threshold(df)
    
    # Restore processor state (encoders, baselines, etc.)
    processor.le_mun = processor_state['le_municipality']
    processor.le_sex = processor_state['le_sex']
    processor.le_age = processor_state['le_age']
    processor.municipality_baselines = processor_state['municipality_baselines']
    processor.enable_log_transform(False)
    
    # Load population data
    processor.load_population_data()
    
    # Initialize model
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    model = BirthNowcastPNN(
        past_units=model_config['past_units'],
        max_delay=model_config['max_delay'],
        n_municipalities=model_config['n_municipalities'],
        n_age_groups=model_config['n_age_groups'],
        hidden_units=model_config['hidden_units'],
        embedding_dims=model_config['embedding_dims'],
        dropout_probs=model_config['dropout_probs']
    ).to(device)
    
    # Load model weights
    model.load_state_dict(checkpoint['model_state_dict'])
    print("✓ Model loaded successfully")
    
    # Prepare data for nowcast
    cutoff_dt = processor.current_date
    D = processor.max_delay
    P = processor.past_units
    
    # Use original data directly (apply same filtering as training)
    df_all = df.copy()
    df_all['year_occ'] = pd.to_numeric(df_all['year_occ'], errors='coerce')
    df_all['month_occ'] = pd.to_numeric(df_all['month_occ'], errors='coerce')
    df_all['year_reg'] = pd.to_numeric(df_all['year_reg'], errors='coerce')
    df_all['month_reg'] = pd.to_numeric(df_all['month_reg'], errors='coerce')
    df_all['occ_id'] = df_all['year_occ']*12 + df_all['month_occ']
    df_all['reg_id'] = df_all['year_reg']*12 + df_all['month_reg']
    df_all['delay'] = df_all['reg_id'] - df_all['occ_id']
    
    def _build_triangle_for_nowcast(y_star, m_star, group_id, sex, age, max_obs_delay):
        tri = np.zeros((P, D), dtype=np.float32)
        ref_id = y_star*12 + m_star
        g = df_all[(df_all['group_id'] == group_id) &
                   (df_all['sex'] == sex) &
                   (df_all['age'] == age)]
        if g.empty:
            return tri
        for i in range(P):
            past_id = ref_id - (P-1-i)
            gi = g[g['occ_id'] == past_id]
            if gi.empty:
                continue
            gi = gi[(gi['delay'] >= 0) & (gi['delay'] <= max_obs_delay) & (gi['delay'] < D)]
            if gi.empty:
                continue
            by_d = gi.groupby('delay', as_index=False)['births'].sum()
            tri[i, by_d['delay'].values.astype(int)] = by_d['births'].values.astype(np.float32)
        return tri

    def _observed_so_far(y_star, m_star, group_id, sex, age, max_obs_delay):
        mask = (
            (df_all['year_occ'] == y_star) &
            (df_all['month_occ'] == m_star) &
            (df_all['group_id'] == group_id) &
            (df_all['sex'] == sex) &
            (df_all['age'] == age) &
            (df_all['delay'] >= 0) &
            (df_all['delay'] <= max_obs_delay)
        )
        return float(df_all.loc[mask, 'births'].sum())
    
    # Build target list for incomplete months (2016-2023)
    print("📋 Identifying incomplete months (2016-2023)...")
    target_rows = []
    cutoff_y, cutoff_m = cutoff_dt.year, cutoff_dt.month
    t_cut = cutoff_y*12 + cutoff_m
    
    for y in range(2016, 2024):
        for m in range(1, 13):
            t_star = y*12 + m
            if t_star > t_cut:
                continue
            months_since = (cutoff_y - y)*12 + (cutoff_m - m)
            max_obs = int(min(max(months_since, 0), D))
            if max_obs >= D:
                continue
            month_g = df_all[(df_all['year_occ'] == y) & (df_all['month_occ'] == m)]
            if month_g.empty:
                continue
            existing = month_g[['group_id', 'sex', 'age']].drop_duplicates()
            for _, r in existing.iterrows():
                target_rows.append((y, m, r['group_id'], r['sex'], r['age'], max_obs))
    
    print(f"✓ Targets to predict: {len(target_rows):,}")
    
    # Generate predictions
    mc_samples = training_config.get('nowcast_mc_samples', 200)
    batch_size = min(4096, max(512, training_config.get('batch_size', 32)*4))
    
    model.eval()
    for mdr in model.modules():
        if isinstance(mdr, nn.Dropout):
            mdr.train()
    
    records = []
    
    for i in range(0, len(target_rows), batch_size):
        chunk = target_rows[i:i+batch_size]
        
        tris = []
        group_ids = []
        sex_ids = []
        age_ids = []
        pop_offs = []
        meta = []
        
        for (y, m, group_id, sex, age, max_obs) in chunk:
            # Salta target con valori nulli o non visti dagli encoder
            if (
                (group_id is None or sex is None or age is None)
                or (group_id not in processor.le_mun.classes_)
                or (sex not in processor.le_sex.classes_)
                or (age not in processor.le_age.classes_)
            ):
                print(f"[WARN] Skipping target with unknown label: group_id={group_id}, sex={sex}, age={age}")
                continue
            tri = _build_triangle_for_nowcast(y, m, group_id, sex, age, max_obs)
            tris.append(tri)
            group_ids.append(processor.le_mun.transform([group_id])[0])
            sex_ids.append(processor.le_sex.transform([sex])[0])
            age_ids.append(processor.le_age.transform([age])[0])
            po = processor.get_population_offset(group_id, sex, age, y)
            pop_offs.append(np.log(po) if getattr(processor, 'use_population_offset', False) else 0.0)
            meta.append((y, m, group_id, sex, age, max_obs))
        
        if len(tris) == 0:
            print(f"  [WARN] No valid targets in this batch, skipping.")
            continue
        X = torch.tensor(np.stack(tris), dtype=torch.float32, device=device)
        MID = torch.tensor(group_ids, dtype=torch.long, device=device)
        SID = torch.tensor(sex_ids, dtype=torch.long, device=device)
        AID = torch.tensor(age_ids, dtype=torch.long, device=device)
        POP = torch.tensor(pop_offs, dtype=torch.float32, device=device)
        
        with torch.no_grad():
            samples = np.zeros((len(tris), mc_samples), dtype=np.float32)
            for s in range(mc_samples):
                dist = model(X, MID, SID, AID, POP)
                samples[:, s] = dist.sample().cpu().numpy()
        
        mean_pred = samples.mean(axis=1)
        std_pred = samples.std(axis=1)
        lo = np.percentile(samples, 2.5, axis=1)
        hi = np.percentile(samples, 97.5, axis=1)
        
        for j, (y, m, group_id, sex, age, max_obs) in enumerate(meta):
            obs = _observed_so_far(y, m, group_id, sex, age, max_obs)
            rec = {
                'year_occ': y,
                'month_occ': m,
                'municipality': group_id,
                'sex': sex,
                'age_group': age,
                'max_observable_delay': max_obs,
                'observed_so_far': obs,
                'pred_missing_mean': float(mean_pred[j]),
                'pred_missing_std': float(std_pred[j]),
                'pred_missing_lo95': float(lo[j]),
                'pred_missing_hi95': float(hi[j]),
                'total_corrected_mean': float(obs + mean_pred[j]),
                'total_corrected_lo95': float(obs + lo[j]),
                'total_corrected_hi95': float(obs + hi[j]),
            }
            records.append(rec)
        
        print(f"  Processed {min(i+batch_size, len(target_rows)):,}/{len(target_rows):,}")
    
    # Save results
    nowcast_df = pd.DataFrame.from_records(records)
    print(f"\n✅ Nowcast predictions: {len(nowcast_df):,} rows")
    
    # Annual aggregation
    annual = nowcast_df.groupby('year_occ', as_index=False).agg(
        registered_births=('observed_so_far', 'sum'),
        estimated_missing=('pred_missing_mean', 'sum'),
        var_missing=('pred_missing_std', lambda s: np.sum(s.values**2))
    )
    annual['total_corrected'] = annual['registered_births'] + annual['estimated_missing']
    annual['uncertainty_std'] = np.sqrt(annual['var_missing'])
    annual['ci_lower'] = annual['total_corrected'] - 1.96*annual['uncertainty_std']
    annual['ci_upper'] = annual['total_corrected'] + 1.96*annual['uncertainty_std']
    annual['percent_estimated'] = 100.0*annual['estimated_missing']/annual['total_corrected'].replace(0, np.nan)
    
    # Save files
    os.makedirs('./predictions', exist_ok=True)
    nowcast_csv = './predictions/nowcast_monthly_2016_2023.csv'
    annual_csv = './predictions/nowcast_annual_2016_2023.csv'
    nowcast_df.to_csv(nowcast_csv, index=False)
    annual.to_csv(annual_csv, index=False)
    
    print(f"💾 Saved monthly predictions: {nowcast_csv}")
    print(f"💾 Saved annual predictions: {annual_csv}")
    
    # Display results
    print(f"\n📊 ANNUAL NOWCAST RESULTS:")
    for _, row in annual.iterrows():
        year = int(row['year_occ'])
        reg = row['registered_births']
        est = row['estimated_missing']
        tot = row['total_corrected']
        pct = row['percent_estimated']
        std = row['uncertainty_std']
        print(f"   {year}: {reg:,.0f} registered + {est:,.0f} estimated = {tot:,.0f} total ({pct:.1f}% est) ± {std:,.0f}")
    
    print(f"\n✅ Nowcast completed at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")


if __name__ == "__main__":
    main()
