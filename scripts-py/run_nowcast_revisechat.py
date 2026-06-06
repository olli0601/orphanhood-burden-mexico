#!/usr/bin/env python3
"""
Nowcast execution with 95% CIs via parametric bootstrap (Negative Binomial samples)
- One forward pass per batch (dropout OFF), then vectorized sampling from NB
- CI controlled by env: NOWCAST_CI_SAMPLES (default 128), NOWCAST_BATCH_SIZE
- Verbose progress prints & timings to understand where time is spent
"""

import os
import math
import gc
import time
from datetime import datetime
from typing import Dict, List, Tuple

import numpy as np
import pandas as pd
import torch

# Optional memory reporting
try:
    import psutil
    _PROC = psutil.Process(os.getpid())
    def _mem_gb():
        return _PROC.memory_info().rss / 1e9
except Exception:
    _PROC = None
    def _mem_gb():
        return float("nan")

from data_processing import BirthDataProcessor
from birth_nowcast_models import BirthNowcastPNN


def _fmt_secs(s: float) -> str:
    if s < 60:
        return f"{s:.1f}s"
    m = int(s // 60)
    return f"{m}m {s - 60*m:.0f}s"


def months_between(y1: int, m1: int, y2: int, m2: int) -> int:
    return max(0, (y2 - y1) * 12 + (m2 - m1))


def build_triangle_for_nowcast(df_all: pd.DataFrame,
                               P: int,
                               D: int,
                               y_star: int,
                               m_star: int,
                               group_id: str,
                               sex: str,
                               age: str,
                               max_obs_delay: int) -> np.ndarray:
    tri = np.zeros((P, D), dtype=np.float32)
    ref_id = y_star * 12 + m_star

    g = df_all[(df_all["group_id"] == group_id) &
               (df_all["sex"] == sex) &
               (df_all["age"] == age)]
    if g.empty:
        return tri

    for i in range(P):
        occ_id = ref_id - i
        y_i = occ_id // 12
        m_i = occ_id % 12
        if m_i == 0:
            y_i -= 1
            m_i = 12

        gi = g[(g["year_occ"] == y_i) & (g["month_occ"] == m_i)]
        if gi.empty:
            continue

        if i == 0:
            gi = gi[(gi["delay"] >= 0) & (gi["delay"] <= max_obs_delay) & (gi["delay"] < D)]
        else:
            gi = gi[(gi["delay"] >= 0) & (gi["delay"] < D)]
        if gi.empty:
            continue

        by_d = gi.groupby("delay", as_index=False)["births"].sum()
        tri[i, by_d["delay"].astype(int).to_numpy()] = by_d["births"].astype(np.float32).to_numpy()

    return tri


def observed_so_far(df_all: pd.DataFrame,
                    y_star: int,
                    m_star: int,
                    group_id: str,
                    sex: str,
                    age: str,
                    max_obs_delay: int) -> float:
    mask = ((df_all["year_occ"] == y_star) &
            (df_all["month_occ"] == m_star) &
            (df_all["group_id"] == group_id) &
            (df_all["sex"] == sex) &
            (df_all["age"] == age) &
            (df_all["delay"] >= 0) &
            (df_all["delay"] <= max_obs_delay))
    return float(df_all.loc[mask, "births"].sum())


def main():
    t0 = time.perf_counter()
    print("🎯 BIRTH NOWCAST PREDICTION (bootstrap CI)")
    print("=" * 68)
    print(f"Started at: {datetime.now():%Y-%m-%d %H:%M:%S}")
    print(f"Versions: torch {torch.__version__} | pandas {pd.__version__} | numpy {np.__version__}")

    # ---------- Config ----------
    model_path = os.environ.get("NOWCAST_CHECKPOINT", "./checkpoints/negative_binomial_model_final.pt")
    ci_samples = int(os.environ.get("NOWCAST_CI_SAMPLES", "128"))
    batch_size_override = os.environ.get("NOWCAST_BATCH_SIZE", None)
    print(f"\nArgs:")
    print(f"  checkpoint: {model_path}")
    print(f"  CI samples: {ci_samples}")
    print(f"  NOWCAST_BATCH_SIZE override: {batch_size_override}")
    print(f"  Proc RAM now: { _mem_gb():.2f} GB")

    # ---------- Load checkpoint ----------
    t = time.perf_counter()
    checkpoint = torch.load(model_path, map_location="cpu", weights_only=False)
    model_config = checkpoint["model_config"]
    training_config = checkpoint["training_config"]
    processor_state = checkpoint["data_processor_state"]
    print(f"✓ Loaded checkpoint in {_fmt_secs(time.perf_counter()-t)}")

    # ---------- Recreate data processor ----------
    processor = BirthDataProcessor(
        past_units=processor_state["past_units"],
        max_delay=processor_state["max_delay"],
        min_births_threshold=processor_state["min_births_threshold"],
        nowcast_cutoff_year=processor_state["nowcast_cutoff_year"],
        current_date=pd.to_datetime(training_config["current_date"]),
    )

    # ---------- Load data ----------
    data_path = os.path.join("../../datasets", "monthly_births.parquet")
    t = time.perf_counter()
    df = pd.read_parquet(data_path)
    print(f"\nLoaded births parquet: {len(df):,} rows | {_fmt_secs(time.perf_counter()-t)} | RAM { _mem_gb():.2f} GB")

    # ---------- Filter like training ----------
    t = time.perf_counter()
    df = df[(df["year_occ"] >= 2016) & (df["year_occ"] <= 2023)]
    print(f"Filter years 2016–2023 → {len(df):,} rows")

    df = processor.filter_reproductive_ages(df)
    df = processor.apply_birth_threshold(df)
    processor.le_mun = processor_state["le_municipality"]
    processor.le_sex = processor_state["le_sex"]
    processor.le_age = processor_state["le_age"]
    processor.municipality_baselines = processor_state["municipality_baselines"]
    processor.enable_log_transform(False)
    print(f"Filtering done in {_fmt_secs(time.perf_counter()-t)} | RAM { _mem_gb():.2f} GB")

    # ---------- Population (optional) ----------
    t = time.perf_counter()
    processor.load_population_data()
    use_pop = getattr(processor, "use_population_offset", False)
    print(f"Population offset: {'ON' if use_pop else 'OFF'} | {_fmt_secs(time.perf_counter()-t)} | RAM { _mem_gb():.2f} GB")

    # ---------- Device & model ----------
    if torch.cuda.is_available():
        device = torch.device("cuda")
        print(f"\n✅ Using GPU (CUDA): {torch.cuda.get_device_name(0)}")
    else:
        device = torch.device("cpu")
        print("\n⚠️ Using CPU")
    print(f"Device selected: {device}")

    D = processor.max_delay
    P = processor.past_units
    print(f"Model config: past_units={P}, max_delay={D}")

    t = time.perf_counter()
    model = BirthNowcastPNN(
        past_units=model_config["past_units"],
        max_delay=model_config["max_delay"],
        n_municipalities=model_config["n_municipalities"],
        n_age_groups=model_config["n_age_groups"],
        hidden_units=model_config["hidden_units"],
        embedding_dims=model_config["embedding_dims"],
        dropout_probs=model_config["dropout_probs"],
    ).to(device)
    model.load_state_dict(checkpoint["model_state_dict"])
    model.eval()
    print(f"✓ Model built & loaded in {_fmt_secs(time.perf_counter()-t)} | RAM { _mem_gb():.2f} GB")

    # ---------- Prepare frame with delays ----------
    t = time.perf_counter()
    df_all = df.copy()
    for col in ["year_occ", "month_occ", "year_reg", "month_reg"]:
        df_all[col] = pd.to_numeric(df_all[col], errors="coerce")
    df_all["occ_id"] = df_all["year_occ"] * 12 + df_all["month_occ"]
    df_all["reg_id"] = df_all["year_reg"] * 12 + df_all["month_reg"]
    df_all["delay"] = df_all["reg_id"] - df_all["occ_id"]
    print(f"✓ Added delay columns in {_fmt_secs(time.perf_counter()-t)} | RAM { _mem_gb():.2f} GB")

    # ---------- Build target rows ----------
    cutoff_dt = processor.current_date
    target_rows: List[Tuple[int, int, str, str, str, int]] = []
    t = time.perf_counter()
    for y in range(2016, 2024):
        year_t = time.perf_counter()
        added = 0
        for m in range(1, 13):
            max_obs = min(D - 1, months_between(y, m, cutoff_dt.year, cutoff_dt.month))
            if max_obs <= 0:
                continue
            month_g = df_all[(df_all["year_occ"] == y) & (df_all["month_occ"] == m)]
            if month_g.empty:
                continue
            combos = month_g[["group_id", "sex", "age"]].drop_duplicates()
            for _, r in combos.iterrows():
                target_rows.append((y, m, r["group_id"], r["sex"], r["age"], max_obs))
                added += 1
        print(f"  • Year {y}: +{added:,} targets | { _fmt_secs(time.perf_counter()-year_t)} | RAM { _mem_gb():.2f} GB")
    print(f"✓ Targets to predict: {len(target_rows):,} | Built in {_fmt_secs(time.perf_counter()-t)}")

    # ---------- Inference loop ----------
    default_batch = min(8192, max(2048, int(training_config.get("batch_size", 32)) * 16))
    batch_size = int(batch_size_override) if batch_size_override else default_batch
    print(f"\nBatch size: {batch_size} | CI samples per cell: {ci_samples}")
    print("-"*68)

    records: List[Dict] = []
    pop_cache: Dict[Tuple[str, str, str, int], float] = {}
    n_total = len(target_rows)
    t_loop0 = time.perf_counter()
    last_report = t_loop0
    processed = 0
    report_every = max(1, 10_000 // batch_size)  # print every N batches

    for bstart in range(0, n_total, batch_size):
        t_batch0 = time.perf_counter()
        chunk = target_rows[bstart:bstart + batch_size]

        # Build batch lists
        tris = []
        group_ids = []
        sex_ids = []
        age_ids = []
        pop_logs = []
        meta = []

        for (y, m, gid, sex, age, max_obs) in chunk:
            if ((gid not in processor.le_mun.classes_) or
                (sex not in processor.le_sex.classes_) or
                (age not in processor.le_age.classes_)):
                continue

            tri = build_triangle_for_nowcast(df_all, P, D, y, m, gid, sex, age, max_obs)
            tris.append(tri)
            group_ids.append(int(processor.le_mun.transform([gid])[0]))
            sex_ids.append(int(processor.le_sex.transform([sex])[0]))
            age_ids.append(int(processor.le_age.transform([age])[0]))

            if use_pop:
                key = (gid, sex, age, y)
                if key in pop_cache:
                    pop_val = pop_cache[key]
                else:
                    pop_val = processor.get_population_offset(gid, sex, age, y)
                    pop_cache[key] = pop_val
                pop_logs.append(float(np.log(max(pop_val, 1.0))))
            else:
                pop_logs.append(0.0)

            meta.append((y, m, gid, sex, age, max_obs))

        if not tris:
            continue

        # Tensors
        t_prep0 = time.perf_counter()
        X   = torch.tensor(np.stack(tris), dtype=torch.float32, device=device)
        MID = torch.tensor(group_ids, dtype=torch.long,   device=device)
        SID = torch.tensor(sex_ids,   dtype=torch.long,   device=device)
        AID = torch.tensor(age_ids,   dtype=torch.long,   device=device)
        POP = torch.tensor(pop_logs,  dtype=torch.float32,device=device)
        prep_secs = time.perf_counter() - t_prep0

        # Forward + sampling + stats (all on device)
        t_fwd0 = time.perf_counter()
        with torch.no_grad():
            dist = model(X, MID, SID, AID, POP)
            samples = dist.sample((ci_samples,))           # (S, batch)
            mean_pred = samples.mean(dim=0)                # (batch,)
            std_pred  = samples.std(dim=0, unbiased=False)
            qs = torch.tensor([0.025, 0.975], device=samples.device)
            lohi = torch.quantile(samples, qs, dim=0)      # (2, batch)
            lo, hi = lohi[0], lohi[1]
        fwd_secs = time.perf_counter() - t_fwd0

        # Bring small tensors to CPU once
        t_cpu0 = time.perf_counter()
        mean_pred = mean_pred.cpu().numpy()
        std_pred  = std_pred.cpu().numpy()
        lo        = lo.cpu().numpy()
        hi        = hi.cpu().numpy()
        cpu_secs = time.perf_counter() - t_cpu0

        # Collect rows
        for j, (y, m, gid, sex, age, max_obs) in enumerate(meta):
            obs = observed_so_far(df_all, y, m, gid, sex, age, max_obs)
            records.append({
                "year_occ": y,
                "month_occ": m,
                "municipality": gid,
                "sex": sex,
                "age_group": age,
                "max_observable_delay": max_obs,
                "observed_so_far": obs,
                "pred_missing_mean": float(mean_pred[j]),
                "pred_missing_std": float(std_pred[j]),
                "pred_missing_lo95": float(lo[j]),
                "pred_missing_hi95": float(hi[j]),
                "total_corrected_mean": float(obs + mean_pred[j]),
                "total_corrected_lo95": float(obs + lo[j]),
                "total_corrected_hi95": float(obs + hi[j]),
            })

        processed += len(meta)
        batch_secs = time.perf_counter() - t_batch0
        if ((bstart // batch_size) % report_every == 0) or (time.perf_counter()-last_report > 30):
            elapsed = time.perf_counter() - t_loop0
            rate = processed / max(elapsed, 1e-6)
            remaining = n_total - processed
            eta = remaining / max(rate, 1e-9)
            print(f"[Batch {bstart//batch_size + 1:>4}] "
                  f"prep {prep_secs:.2f}s | fwd+CI {fwd_secs:.2f}s | cpu {cpu_secs:.2f}s | "
                  f"rows {processed:,}/{n_total:,} ({100*processed/n_total:.1f}%) | "
                  f"rate {rate:,.0f}/s | ETA {_fmt_secs(eta)} | RAM { _mem_gb():.2f} GB")
            last_report = time.perf_counter()

        # Free batch buffers ASAP
        del tris, group_ids, sex_ids, age_ids, pop_logs, meta, X, MID, SID, AID, POP, samples
        gc.collect()

    # ---------- Aggregate & save ----------
    t = time.perf_counter()
    pred_df = pd.DataFrame(records)
    if pred_df.empty:
        print("No predictions produced. Check your filters and data paths.")
        return

    print(f"\nAssembled pred_df: {len(pred_df):,} rows | RAM { _mem_gb():.2f} GB")
    monthly = (pred_df
               .groupby(["year_occ", "month_occ"], as_index=False)
               .agg(registered_births=("observed_so_far", "sum"),
                    estimated_missing=("pred_missing_mean", "sum"),
                    uncertainty_std=("pred_missing_std", "mean")))
    monthly["total_corrected"] = monthly["registered_births"] + monthly["estimated_missing"]
    monthly["percent_estimated"] = 100.0 * monthly["estimated_missing"] / monthly["total_corrected"].clip(lower=1.0)

    annual = (monthly
              .groupby("year_occ", as_index=False)
              .agg(registered_births=("registered_births", "sum"),
                   estimated_missing=("estimated_missing", "sum"),
                   uncertainty_std=("uncertainty_std", "mean")))
    annual["total_corrected"] = annual["registered_births"] + annual["estimated_missing"]
    annual["percent_estimated"] = 100.0 * annual["estimated_missing"] / annual["total_corrected"].clip(lower=1.0)
    annual = annual.rename(columns={"year_occ": "year"})

    os.makedirs("./results", exist_ok=True)
    ts = datetime.now().strftime("%Y%m%d_%H%M%S")
    p1 = f"./results/nowcast_records_{ts}.csv"
    p2 = f"./results/nowcast_monthly_{ts}.csv"
    p3 = f"./results/nowcast_annual_{ts}.csv"
    pred_df.to_csv(p1, index=False)
    monthly.to_csv(p2, index=False)
    annual.to_csv(p3, index=False)

    print("\n📄 Saved:")
    print(f"  {p1}")
    print(f"  {p2}")
    print(f"  {p3}")
    print(f"\n🧾 Annual summary (2016–2023):")
    for _, row in annual.sort_values('year').iterrows():
        year = int(row["year"])
        reg = row["registered_births"]
        est = row["estimated_missing"]
        tot = row["total_corrected"]
        pct = row["percent_estimated"]
        std = row["uncertainty_std"]
        print(f"  {year}: {reg:,.0f} registered + {est:,.0f} estimated = {tot:,.0f} total "
              f"({pct:.1f}% est) ± {std:,.0f}")

    total_secs = time.perf_counter() - t0
    print(f"\n✅ Done at: {datetime.now():%Y-%m-%d %H:%M:%S} | Total time: {_fmt_secs(total_secs)} | RAM { _mem_gb():.2f} GB")


if __name__ == "__main__":
    main()
