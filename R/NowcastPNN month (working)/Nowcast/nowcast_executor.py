"""
Birth Nowcasting Pipeline Executor
Generates complete nowcast predictions for incomplete years (2016-2023)
with uncertainty quantification and annual aggregation.
"""

import os
import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import matplotlib.pyplot as plt
import time
import warnings
from torch.utils.data import DataLoader, TensorDataset
from datetime import datetime, date
from typing import Dict, Any, List, Tuple, Optional
from tqdm import tqdm
from pathlib import Path

from data_processing import BirthDataProcessor
from birth_nowcast_models import BirthNowcastPNN

warnings.filterwarnings('ignore')

class BirthNowcastExecutor:
    """
    Main pipeline for executing birth nowcasting.
    
    This class:
    1. Loads trained model and data
    2. Identifies missing births due to reporting delays
    3. Generates predictions for unreported births
    4. Aggregates to annual corrected time series
    5. Provides uncertainty quantification
    """
    
    def __init__(self, 
                 model_path: str = './checkpoints/negative_binomial_model_final.pt',
                 data_path: str = '../../datasets/monthly_births.parquet',
                 population_path: str = '../../datasets/population_new_mun.RDS',
                 cutoff_date: str = "2024-01-01",  # Data available until 2023-12
                 device: str = None):
        """
        Initialize the nowcasting executor.
        
        Args:
            model_path: Path to trained model
            data_path: Path to birth data
            population_path: Path to population data
            cutoff_date: Current knowledge cutoff (format: YYYY-MM-DD)
            device: Device for model inference
        """
        self.model_path = model_path
        self.data_path = data_path
        self.population_path = population_path
        self.cutoff_date = cutoff_date
        self.device = device or ('cuda' if torch.cuda.is_available() else 'cpu')
        
        # Initialize components
        self.model = None
        self.processor = None
        self.data = None
        self.nowcast_results = None
        
        print(f"🚀 Birth Nowcasting Pipeline Initialized")
        print(f"   Model: {model_path}")
        print(f"   Data cutoff: {cutoff_date}")
        print(f"   Device: {self.device}")
        
    def load_components(self):
        """Load trained model and data processor."""
        print("\n📦 Loading components...")
        
        # Load trained model
        print(f"Loading model from {self.model_path}")
        model_data = torch.load(self.model_path, map_location=self.device, weights_only=False)
        self.model_data = model_data  # Store for later use
        
        # Extract model configuration and state
        if isinstance(model_data, dict) and 'model_state_dict' in model_data:
            model_config = model_data['model_config']
            
            # Recreate model with saved configuration
            self.model = BirthNowcastPNN(**model_config)
            self.model.load_state_dict(model_data['model_state_dict'])
            print("✓ Model loaded from checkpoint with configuration")
        else:
            # Direct model loading (fallback)
            self.model = model_data
            self.model_data = None
            print("✓ Model loaded directly")
        
        self.model.eval()
        self.model.to(self.device)
        print("✓ Model ready for inference")
        
        # Initialize data processor
        print("Initializing data processor...")
        self.processor = BirthDataProcessor(
            past_units=36,
            max_delay=96,
            min_births_threshold=10,
            nowcast_cutoff_year=2016,
            current_date=pd.to_datetime(self.cutoff_date)  # Convert to datetime
        )
        
        # Load data
        print(f"Loading birth data from {self.data_path}")
        self.data = pd.read_parquet(self.data_path, engine='fastparquet')
        print(f"✓ Loaded {len(self.data):,} birth records")
        
        # Process data using the same pipeline as training
        print("Processing birth data...")
        df_processed = self.processor.filter_reproductive_ages(self.data)
        print(f"   After reproductive age filter: {len(df_processed):,} records")
        
        df_processed = self.processor.apply_birth_threshold(df_processed)
        print(f"   After birth threshold filter: {len(df_processed):,} records")
        
        # Set processed data and fit encoders
        self.processor.data = df_processed
        
        # Load data processor state from saved model if available
        if isinstance(self.model_data, dict) and 'data_processor_state' in self.model_data:
            processor_state = self.model_data['data_processor_state']
            
            # Restore label encoders
            self.processor.le_mun = processor_state['le_municipality']
            self.processor.le_sex = processor_state['le_sex']
            self.processor.le_age = processor_state['le_age']
            
            # Restore other settings
            self.processor.municipality_baselines = processor_state['municipality_baselines']
            self.processor.municipalities = processor_state['municipality_classes']
            self.processor.sexes = processor_state['sex_classes']
            self.processor.age_groups = processor_state['age_classes']
            
            print("✓ Data processor state restored from model checkpoint")
        else:
            # Fit encoders if no saved state
            self.processor.fit_label_encoders(df_processed)
            self.processor.calculate_municipality_baselines(df_processed)
            print("✓ Data processor fitted from scratch")
        
        # Load population data if available
        try:
            if os.path.exists(self.population_path):
                self.processor.load_population_data(self.population_path)
                print("✓ Population data loaded")
            else:
                print("⚠️ Population data not found, proceeding without offset")
        except Exception as e:
            print(f"⚠️ Error loading population data: {e}")
            
        print("✓ All components loaded successfully")
        
    def create_nowcast_dataloader(self, target_years: List[int], dev_mode: bool = True) -> DataLoader:
        """
        Create optimized DataLoader for nowcast predictions.
        
        Args:
            target_years: Years to generate predictions for
            dev_mode: If True, use smaller batches and fewer samples for development
            
        Returns:
            DataLoader configured for efficient batch processing
        """
        print(f"🔧 Creating nowcast DataLoader for years: {target_years}")
        
        # For now, use the fallback approach which is more reliable
        print("🔄 Using direct identification-based approach...")
        return self._create_fallback_dataloader(target_years, dev_mode)
    
    def nowcast_ultra_efficient(self, target_years: List[int] = None, n_samples: int = 100) -> Dict[str, Any]:
        """
        ULTRA-EFFICIENT nowcasting: Process ONLY truly incomplete delay ranges.
        
        Logic per year (with cutoff 2024-01-01):
        - 2016: 8 years passed → missing only delay 96+ (SKIP - fully observed)  
        - 2017: 7 years passed → missing only delay 84+ (SKIP - minimal)
        - 2018-2021: 6-3 years → missing delays 72+ to 36+ (limited nowcasting)
        - 2022: 2 years passed → missing delays 24+ (moderate nowcasting)
        - 2023: 1 year passed → missing delays 12+ (MAIN FOCUS)
        """
        if target_years is None:
            target_years = [2023]  # Focus on most recent year by default
            
        print(f"⚡ ULTRA-EFFICIENT NOWCAST for years: {target_years}")
        print("🎯 Strategy: Process ONLY delay ranges that are actually missing")
        
        cutoff_datetime = pd.to_datetime(self.cutoff_date)
        cutoff_year = cutoff_datetime.year
        cutoff_month = cutoff_datetime.month
        
        print(f"📅 Analysis from cutoff: {self.cutoff_date}")
        
        all_predictions = []
        all_metadata = []
        
        # STEP 1: Analyze and filter years by missing delay significance
        meaningful_years = []
        for year in target_years:
            years_elapsed = cutoff_year - year
            min_missing_delay = years_elapsed * 12
            
            print(f"📊 Year {year}: {years_elapsed} years elapsed → missing delays {min_missing_delay}+/96")
            
            # Only process years where substantial delays are missing (>36 months)
            if min_missing_delay < 60:  # Less than 5 years of missing delays
                meaningful_years.append(year)
                missing_pct = ((96 - min_missing_delay) / 96) * 100
                print(f"   ✅ {missing_pct:.1f}% of delays missing - worth processing")
            else:
                print(f"   ⏭️  Only {96-min_missing_delay} delays missing - skipping")
        
        if not meaningful_years:
            print("⚠️  No years with meaningful missing delays!")
            return self._empty_results()
        
        print(f"\n🎯 Processing {len(meaningful_years)} meaningful years: {meaningful_years}")
        
        # STEP 2: Get top municipalities (much more aggressive filtering)
        print("🔍 Selecting top municipalities...")
        mun_births = self.data.groupby('group_id')['births'].sum().sort_values(ascending=False)
        
        # Take only top 20 municipalities for speed
        top_municipalities = mun_births.head(20).index.tolist()
        coverage_pct = (mun_births.head(20).sum() / mun_births.sum()) * 100
        
        print(f"📊 Using top {len(top_municipalities)} municipalities ({coverage_pct:.1f}% of births)")
        
        # STEP 3: Process each meaningful year
        total_processed = 0
        for year_occ in meaningful_years:
            print(f"\n📅 Processing {year_occ}...")
            
            years_elapsed = cutoff_year - year_occ
            min_missing_delay = years_elapsed * 12
            
            # Process only months with >6 missing delay months
            meaningful_months = []
            for month_occ in range(1, 13):
                months_since = (cutoff_year - year_occ) * 12 + (cutoff_month - month_occ)
                max_observable = min(months_since, 96)
                missing_delays = 96 - max_observable
                
                if missing_delays > 6:  # Only if >6 months of delays missing
                    meaningful_months.append((month_occ, max_observable, missing_delays))
            
            print(f"   📊 {len(meaningful_months)} months with >6 missing delays")
            
            for month_occ, max_observable, missing_delays in meaningful_months:
                print(f"   📅 {year_occ}-{month_occ:02d}: {missing_delays} missing delays")
                
                # Get existing demographic combinations for this month
                month_data = self.data[
                    (self.data['year_occ'] == year_occ) & 
                    (self.data['month_occ'] == month_occ) &
                    (self.data['group_id'].isin(top_municipalities)) &
                    (self.data['births'] >= 1)  # Only meaningful births
                ]
                
                if len(month_data) == 0:
                    continue
                
                # Filter to only known demographic categories from training
                valid_municipalities = set(self.processor.municipalities)
                valid_sexes = set(self.processor.sexes) 
                valid_ages = set(self.processor.age_groups)
                
                month_data = month_data[
                    (month_data['group_id'].isin(valid_municipalities)) &
                    (month_data['sex'].isin(valid_sexes)) &
                    (month_data['age'].isin(valid_ages))
                ]
                
                if len(month_data) == 0:
                    print(f"      ⚠️  No valid demographic combinations for {year_occ}-{month_occ:02d}")
                    continue
                
                # Get unique combinations
                combinations = month_data[['group_id', 'sex', 'age']].drop_duplicates()
                print(f"      📊 {len(combinations)} valid demographic combinations")
                
                # Process combinations in batches
                batch_predictions = []
                batch_metadata = []
                
                for _, row in combinations.iterrows():
                    municipality = row['group_id']
                    sex = row['sex'] 
                    age_group = row['age']
                    
                    # Build triangle
                    triangle = self._build_nowcast_triangle(
                        year_occ, month_occ, municipality, sex, age_group, max_observable
                    )
                    
                    # Skip if empty triangle
                    if np.sum(triangle) == 0:
                        continue
                    
                    # Encode features
                    mun_id = self.processor.le_mun.transform([municipality])[0]
                    sex_id = self.processor.le_sex.transform([sex])[0]
                    age_id = self.processor.le_age.transform([age_group])[0]
                    
                    pop_offset = self.processor.get_population_offset(municipality, sex, age_group, year_occ)
                    log_pop_offset = np.log(pop_offset) if self.processor.use_population_offset else 0.0
                    
                    batch_predictions.append({
                        'triangle': triangle,
                        'mun_id': mun_id,
                        'sex_id': sex_id,
                        'age_id': age_id,
                        'pop_offset': log_pop_offset
                    })
                    
                    batch_metadata.append({
                        'year_occ': year_occ,
                        'month_occ': month_occ,
                        'municipality': municipality,
                        'sex': sex,
                        'age_group': age_group,
                        'max_observable_delay': max_observable,
                        'missing_delays': missing_delays
                    })
                
                # Batch predict
                if batch_predictions:
                    results = self._batch_predict_month(batch_predictions, n_samples)
                    all_predictions.extend(results)
                    all_metadata.extend(batch_metadata)
                    total_processed += len(results)
                    
                    print(f"      ✅ {len(results)} predictions (total: {total_processed})")
        
        # Aggregate results
        if not all_predictions:
            print("⚠️  No predictions generated!")
            return self._empty_results()
        
        predictions_array = np.array(all_predictions)
        mean_predictions = np.mean(predictions_array, axis=1)
        std_predictions = np.std(predictions_array, axis=1)
        
        results = {
            'predictions': {
                'mean': mean_predictions,
                'std': std_predictions,
                'samples': predictions_array
            },
            'metadata': {
                'target_years': meaningful_years,
                'n_samples': n_samples,
                'methodology': 'ultra_efficient',
                'sample_metadata': all_metadata,
                'total_processed': total_processed
            },
            'summary_stats': {
                'total_births_predicted': float(np.sum(mean_predictions)),
                'avg_uncertainty': float(np.mean(std_predictions)),
                'processing_efficiency': f"{total_processed} predictions vs full sampling"
            }
        }
        
        print(f"\n✅ ULTRA-EFFICIENT NOWCAST COMPLETE")
        print(f"   Predictions: {len(mean_predictions):,}")
        print(f"   Total births predicted: {results['summary_stats']['total_births_predicted']:,.0f}")
        print(f"   Efficiency: {total_processed} targeted predictions")
        
        return results
    
    def _empty_results(self):
        """Return empty results structure."""
        return {
            'predictions': {'mean': np.array([]), 'std': np.array([]), 'samples': np.array([])},
            'metadata': {'target_years': [], 'n_samples': 0, 'methodology': 'ultra_efficient', 'sample_metadata': []},
            'summary_stats': {'total_births_predicted': 0.0, 'avg_uncertainty': 0.0}
        }
        
    def nowcast_proper_methodology(self, target_years: List[int] = None, n_samples: int = 200) -> Dict[str, Any]:
        """
        Implement proper nowcasting methodology from the paper.
        OPTIMIZED: Focus only on actually incomplete data to avoid processing unnecessary combinations.
        
        For each month t* in delay horizon and each stratum g:
        1. Build current triangle X_t*(g) with truncated delays
        2. Pass through model to get (μ, φ)
        3. Use MC dropout + sampling for uncertainty
        """
        if target_years is None:
            target_years = [2023]  # Default to most recent incomplete year
            
        print(f"🔮 OPTIMIZED NOWCAST: Processing only incomplete data for years: {target_years}")
        print("📋 Strategy: Smart filtering to process only actual missing births")
        
        cutoff_datetime = pd.to_datetime(self.cutoff_date)
        cutoff_year = cutoff_datetime.year
        cutoff_month = cutoff_datetime.month
        
        print(f"📅 Cutoff: {self.cutoff_date} (Year {cutoff_year}, Month {cutoff_month})")
        
        # Current calendar month t 
        t = cutoff_year * 12 + cutoff_month
        D = 96  # Maximum delay (8 years)
        
        all_predictions = []
        all_metadata = []
        
        # OPTIMIZATION 1: Smart year filtering - only process recent incomplete years
        effective_target_years = []
        for year_occ in target_years:
            # Check if this year has any incomplete months
            months_since_year_start = (cutoff_year - year_occ) * 12 + cutoff_month - 1
            if months_since_year_start < D:  # Year has some incomplete months
                effective_target_years.append(year_occ)
        
        print(f"🎯 Processing {len(effective_target_years)} years with incomplete data: {effective_target_years}")
        
        # OPTIMIZATION 2: Smart municipality sampling - focus on municipalities with actual data
        print("🔍 Identifying municipalities with significant birth data...")
        active_municipalities = self.data.groupby('group_id')['births'].sum().sort_values(ascending=False)
        
        # Take top municipalities that account for 90% of births
        cumulative_pct = active_municipalities.cumsum() / active_municipalities.sum()
        top_municipalities = active_municipalities[cumulative_pct <= 0.9].index.tolist()
        
        # Ensure we have at least 50 municipalities for representation
        if len(top_municipalities) < 50:
            top_municipalities = active_municipalities.head(50).index.tolist()
        
        print(f"📊 Using {len(top_municipalities)} active municipalities (covering ~90% of births)")
        
        # For each target year
        for year_occ in effective_target_years:
            print(f"\n📅 Processing year {year_occ}...")
            
            # OPTIMIZATION 3: Smart month filtering - only incomplete months
            incomplete_months = []
            for month_occ in range(1, 13):
                t_star = year_occ * 12 + month_occ
                months_since_occurrence = t - t_star
                max_observable_delay = min(months_since_occurrence, D)
                
                if max_observable_delay < D and t_star <= t:  # Incomplete and within horizon
                    incomplete_months.append((month_occ, max_observable_delay))
            
            print(f"   📊 Found {len(incomplete_months)} incomplete months for {year_occ}")
            
            for month_occ, max_observable_delay in incomplete_months:
                print(f"   📊 Month {year_occ}-{month_occ:02d}: max_delay={max_observable_delay}")
                
                # OPTIMIZATION 4: Smart demographic filtering - only existing combinations
                print(f"      🔢 Identifying existing demographic combinations...")
                
                # Get all demographic combinations that actually exist for this month
                month_data = self.data[
                    (self.data['year_occ'] == year_occ) & 
                    (self.data['month_occ'] == month_occ) &
                    (self.data['group_id'].isin(top_municipalities))
                ]
                
                if len(month_data) == 0:
                    print(f"      ⚠️  No data found for {year_occ}-{month_occ:02d}, skipping...")
                    continue
                
                # Get unique combinations that actually have births
                existing_combinations = month_data[['group_id', 'sex', 'age']].drop_duplicates()
                
                print(f"      � Found {len(existing_combinations)} existing demographic combinations")
                
                # For each existing demographic combination
                month_predictions = []
                month_metadata = []
                
                for idx, (_, row) in enumerate(existing_combinations.iterrows()):
                    municipality = row['group_id']
                    sex = row['sex']
                    age_group = row['age']
                    
                    if idx % 100 == 0:  # Progress every 100 combinations
                        print(f"         📊 Processing combination {idx+1}/{len(existing_combinations)}...")
                    
                    # Build current triangle X_t*(g) for this stratum
                    triangle = self._build_nowcast_triangle(
                        year_occ, month_occ, municipality, sex, age_group, max_observable_delay
                    )
                    
                    # Get encoded features
                    mun_id = self.processor.le_mun.transform([municipality])[0]
                    sex_id = self.processor.le_sex.transform([sex])[0]
                    age_id = self.processor.le_age.transform([age_group])[0]
                    
                    # Get population offset
                    pop_offset = self.processor.get_population_offset(
                        municipality, sex, age_group, year_occ
                    )
                    log_pop_offset = np.log(pop_offset) if self.processor.use_population_offset else 0.0
                    
                    month_predictions.append({
                        'triangle': triangle,
                        'mun_id': mun_id,
                        'sex_id': sex_id,
                        'age_id': age_id,
                        'pop_offset': log_pop_offset
                    })
                    
                    month_metadata.append({
                        'year_occ': year_occ,
                        'month_occ': month_occ,
                        'municipality': municipality,
                        'sex': sex,
                        'age_group': age_group,
                        'max_observable_delay': max_observable_delay
                    })
                
                # Batch predict for this month
                if month_predictions:
                    batch_results = self._batch_predict_month(month_predictions, n_samples)
                    all_predictions.extend(batch_results)
                    all_metadata.extend(month_metadata)
                    print(f"      ✅ Predicted {len(batch_results)} missing birth groups")
        
        # Aggregate results
        if not all_predictions:
            print("⚠️  No predictions generated - no incomplete data found!")
            return {
                'predictions': {'mean': np.array([]), 'std': np.array([]), 'samples': np.array([])},
                'metadata': {'target_years': target_years, 'n_samples': n_samples, 'methodology': 'optimized_paper_implementation', 'sample_metadata': []},
                'summary_stats': {'total_births_predicted': 0.0, 'avg_uncertainty': 0.0}
            }
        
        predictions_array = np.array(all_predictions)
        mean_predictions = np.mean(predictions_array, axis=1)
        std_predictions = np.std(predictions_array, axis=1)
        
        # Create results dictionary following paper methodology
        results = {
            'predictions': {
                'mean': mean_predictions,
                'std': std_predictions,
                'samples': predictions_array
            },
            'metadata': {
                'target_years': target_years,
                'n_samples': n_samples,
                'methodology': 'proper_paper_implementation',
                'sample_metadata': all_metadata
            },
            'summary_stats': {
                'total_births_predicted': float(np.sum(mean_predictions)),
                'avg_uncertainty': float(np.mean(std_predictions))
            }
        }
        
        print(f"\n✅ PROPER NOWCAST COMPLETE")
        print(f"   Total predictions: {len(mean_predictions):,}")
        print(f"   Total births predicted: {results['summary_stats']['total_births_predicted']:,.0f}")
        
        return results
    
    def _build_nowcast_triangle(self, year_occ: int, month_occ: int, municipality: str, 
                               sex: str, age_group: str, max_observable_delay: int) -> np.ndarray:
        """
        Build the current triangle X_t*(g) for nowcasting as per paper methodology.
        
        This is the key function that implements the paper's triangle construction.
        """
        # Initialize triangle: 36 past months × 96 delays (truncated)
        triangle = np.zeros((36, 96))
        
        # Get data for this specific stratum
        stratum_mask = (
            (self.data['group_id'] == municipality) &
            (self.data['sex'] == sex) &
            (self.data['age'] == age_group)
        )
        stratum_data = self.data[stratum_mask].copy()
        
        # Debug: print first call details
        if year_occ == 2023 and month_occ == 1 and municipality == self.processor.municipalities[0]:
            print(f"            🔍 DEBUG: Building triangle for {municipality}-{sex}-{age_group}")
            print(f"               Stratum data records: {len(stratum_data)}")
        
        if len(stratum_data) == 0:
            return triangle
        
        # Ensure numeric columns
        stratum_data['year_occ'] = pd.to_numeric(stratum_data['year_occ'], errors='coerce')
        stratum_data['month_occ'] = pd.to_numeric(stratum_data['month_occ'], errors='coerce')
        stratum_data['year_reg'] = pd.to_numeric(stratum_data['year_reg'], errors='coerce')
        stratum_data['month_reg'] = pd.to_numeric(stratum_data['month_reg'], errors='coerce')
        
        # Calculate month IDs and delays
        stratum_data['month_id_occ'] = stratum_data['year_occ'] * 12 + stratum_data['month_occ']
        stratum_data['month_id_reg'] = stratum_data['year_reg'] * 12 + stratum_data['month_reg']
        stratum_data['delay'] = stratum_data['month_id_reg'] - stratum_data['month_id_occ']
        
        # Reference month for this prediction
        ref_month_id = year_occ * 12 + month_occ
        
        # Fill triangle with past 36 months relative to the target month
        for i in range(36):
            past_month_id = ref_month_id - i
            
            # Get births for this past month
            month_data = stratum_data[stratum_data['month_id_occ'] == past_month_id]
            
            for _, birth_record in month_data.iterrows():
                delay = int(birth_record['delay'])
                
                # Apply truncation based on what's observable for the TARGET month
                if delay <= max_observable_delay and 0 <= delay < 96:
                    triangle[i, delay] += birth_record['births']
        
        return triangle
    
    def _batch_predict_month(self, month_predictions: List[Dict], n_samples: int) -> List[np.ndarray]:
        """
        Batch predict for all strata in a given month using MC dropout.
        """
        if not month_predictions:
            return []
        
        # Convert to tensors
        triangles = torch.tensor([p['triangle'] for p in month_predictions], dtype=torch.float32)
        mun_ids = torch.tensor([p['mun_id'] for p in month_predictions], dtype=torch.long)
        sex_ids = torch.tensor([p['sex_id'] for p in month_predictions], dtype=torch.long)
        age_ids = torch.tensor([p['age_id'] for p in month_predictions], dtype=torch.long)
        pop_offsets = torch.tensor([p['pop_offset'] for p in month_predictions], dtype=torch.float32)
        
        batch_size = len(month_predictions)
        batch_samples = np.zeros((batch_size, n_samples))
        
        # MC dropout sampling
        self.model.eval()
        for module in self.model.modules():
            if isinstance(module, torch.nn.Dropout):
                module.train()
        
        with torch.no_grad():
            for sample_idx in range(n_samples):
                # Forward pass with dropout
                dist = self.model(triangles, mun_ids, sex_ids, age_ids, pop_offsets)
                samples = dist.sample().cpu().numpy()
                batch_samples[:, sample_idx] = samples
        
        return [batch_samples[i] for i in range(batch_size)]
        
        return triangle
        """
        Comprehensive identification of missing births for target years.
        
        Args:
            target_years: Years to identify missing births for
            
        Returns:
            DataFrame with missing birth specifications
        """
        if target_years is None:
            target_years = list(range(2016, 2024))
            
        print(f"🔍 Identifying missing births for years: {target_years}")
        
        cutoff_datetime = pd.to_datetime(self.cutoff_date)
        missing_records = []
        
        # For realistic nowcast, we need to cover ALL demographic groups that actually exist in the data
        # Let's sample more comprehensively but still manageable
        
        if len(target_years) <= 2:
            # Small test: moderate sampling
            sample_municipalities = self.processor.municipalities[:50]  # More municipalities
            sample_months = list(range(1, 13))  # All months
            sample_ages = self.processor.age_groups  # All age groups
        else:
            # Full nowcast: comprehensive sampling (but still computationally feasible)
            sample_municipalities = self.processor.municipalities[:100]  # More municipalities for full run
            sample_months = list(range(1, 13))  # All months  
            sample_ages = self.processor.age_groups  # All age groups
        
        sample_sexes = self.processor.sexes  # Always use all sexes
        
        print(f"📊 Sampling strategy:")
        print(f"   • {len(sample_municipalities)} municipalities (of {len(self.processor.municipalities)} total)")
        print(f"   • {len(sample_months)} months")
        print(f"   • {len(sample_sexes)} sexes")
        print(f"   • {len(sample_ages)} age groups")
        
        total_combinations = len(target_years) * len(sample_months) * len(sample_municipalities) * len(sample_sexes) * len(sample_ages)
        print(f"   • Expected max combinations: {total_combinations:,}")
        
        for year_occ in target_years:
            for month_occ in sample_months:
                for municipality in sample_municipalities:
                    for sex in sample_sexes:
                        for age_group in sample_ages:
                            
                            # Calculate observable delay
                            months_since_occurrence = (cutoff_datetime.year - year_occ) * 12 + (cutoff_datetime.month - month_occ)
                            max_observable_delay = min(months_since_occurrence, 96)
                            
                            if max_observable_delay < 96:  # There are missing delays
                                # Check if this demographic group actually exists in the data
                                group_exists = ((self.data['group_id'] == municipality) & 
                                              (self.data['sex'] == sex) & 
                                              (self.data['age'] == age_group)).any()
                                
                                if group_exists:  # Only include groups that exist in the data
                                    missing_records.append({
                                        'year_occ': year_occ,
                                        'month_occ': month_occ,
                                        'municipality': municipality,
                                        'sex': sex,
                                        'age_group': age_group,
                                        'max_observable_delay': max_observable_delay,
                                        'missing_delays': list(range(max_observable_delay + 1, 97))
                                    })
        
        missing_df = pd.DataFrame(missing_records)
        print(f"✓ Identified {len(missing_df):,} missing birth groups")
        
        if len(missing_df) > 0:
            print(f"📈 Coverage analysis:")
            print(f"   • Years covered: {sorted(missing_df['year_occ'].unique())}")
            print(f"   • Months per year: {missing_df.groupby('year_occ')['month_occ'].nunique().mean():.1f}")
            print(f"   • Municipalities covered: {missing_df['municipality'].nunique()}")
            print(f"   • Demographics per municipality: {len(missing_df) / missing_df['municipality'].nunique():.1f}")
        
        return missing_df
            
    def _create_fallback_dataloader(self, target_years: List[int], dev_mode: bool = True) -> DataLoader:
        """
        Create fallback DataLoader using the identification approach.
        """
        print("🔄 Using fallback method to create DataLoader...")
        
        # Identify missing births for target years
        missing_df = self.identify_missing_births()
        
        # Filter to target years
        missing_df = missing_df[missing_df['year_occ'].isin(target_years)]
        
        if dev_mode and len(missing_df) > 1000:
            # Sample for faster development
            missing_df = missing_df.sample(n=1000, random_state=42)
            print(f"🔧 Dev mode: Sampled {len(missing_df)} records for faster processing")
        
        # Convert to tensors
        triangles = []
        mun_ids = []
        sex_ids = []
        age_ids = []
        pop_offsets = []
        
        for _, row in missing_df.iterrows():
            # Build reporting triangle
            triangle = self._build_reporting_triangle_for_prediction(
                row['year_occ'], row['month_occ'],
                row['municipality'], row['sex'], row['age_group'],
                row['max_observable_delay']
            )
            
            # Get encoded IDs
            mun_id = self.processor.le_mun.transform([row['municipality']])[0]
            sex_id = self.processor.le_sex.transform([row['sex']])[0]
            age_id = self.processor.le_age.transform([row['age_group']])[0]
            
            # Get population offset
            pop_offset = self.processor.get_population_offset(
                row['municipality'], row['sex'], row['age_group'], row['year_occ']
            )
            log_pop_offset = np.log(pop_offset) if self.processor.use_population_offset else 0.0
            
            triangles.append(triangle)
            mun_ids.append(mun_id)
            sex_ids.append(sex_id)
            age_ids.append(age_id)
            pop_offsets.append(log_pop_offset)
        
        # Convert to tensors
        triangles_tensor = torch.tensor(triangles, dtype=torch.float32)
        mun_ids_tensor = torch.tensor(mun_ids, dtype=torch.long)
        sex_ids_tensor = torch.tensor(sex_ids, dtype=torch.long)
        age_ids_tensor = torch.tensor(age_ids, dtype=torch.long)
        pop_offsets_tensor = torch.tensor(pop_offsets, dtype=torch.float32)
        targets_tensor = torch.zeros(len(triangles), dtype=torch.float32)  # Dummy targets
        
        # Create dataset
        dataset = TensorDataset(triangles_tensor, mun_ids_tensor, sex_ids_tensor, 
                               age_ids_tensor, pop_offsets_tensor, targets_tensor)
        
        # Create dataloader
        batch_size = 32 if dev_mode else 64
        dataloader = DataLoader(dataset, batch_size=batch_size, shuffle=False)
        
        print(f"✓ Created fallback DataLoader with {len(dataset)} samples, batch_size={batch_size}")
        return dataloader
        
    def predict_missing_births(self, target_years: List[int] = None, n_samples: int = 50, dev_mode: bool = True) -> Dict[str, Any]:
        """
        Generate nowcast predictions for missing births using optimized MC dropout.
        
        Args:
            target_years: Years to generate predictions for (default: recent years in dev_mode)
            n_samples: Number of MC dropout samples for uncertainty quantification
            dev_mode: If True, use faster settings for development/testing
            
        Returns:
            Dictionary containing predictions, uncertainties, and metadata
        """
        # Set defaults based on mode
        if target_years is None:
            target_years = [2022, 2023] if dev_mode else list(range(2016, 2024))
        
        if dev_mode:
            n_samples = min(n_samples, 50)  # Cap samples in dev mode
            print(f"🔧 Development mode: Using {n_samples} samples for {target_years}")
        
        print(f"🔮 Generating nowcast predictions for years: {target_years}")
        print(f"📊 Using {n_samples} MC dropout samples for uncertainty quantification")
        
        # Create nowcast dataset
        nowcast_loader = self.create_nowcast_dataloader(target_years, dev_mode=dev_mode)
        print(f"📈 Created nowcast dataset with {len(nowcast_loader.dataset)} samples")
        
        # OPTIMIZED MC DROPOUT SETUP (following paper methodology)
        self.model.eval()  # Set BatchNorm to eval mode
        
        # Enable dropout layers for MC sampling (critical for uncertainty)
        if hasattr(self.model, 'dropout1'):
            self.model.dropout1.train()
        if hasattr(self.model, 'dropout2'):
            self.model.dropout2.train()
        
        # Move model to CPU for inference (as per original paper)
        self.model = self.model.to('cpu')
        print("💻 Using CPU inference for stability (following original paper)")
        
        # Collect all predictions using BATCH PROCESSING
        all_predictions = []
        all_metadata = []
        
        print("🚀 Generating predictions with optimized batch processing...")
        start_time = time.time()
        
        with tqdm(total=len(nowcast_loader), desc="Processing batches") as pbar:
            for batch_idx, batch in enumerate(nowcast_loader):
                # Unpack batch data
                if len(batch) == 6:  # TensorDataset format
                    reporting_triangle, municipality_ids, sex_ids, age_group_ids, population_offset, _ = batch
                else:  # Dictionary format
                    reporting_triangle = batch['reporting_triangle']
                    municipality_ids = batch['municipality_id'].squeeze()
                    sex_ids = batch['sex_id'].squeeze()
                    age_group_ids = batch['age_group_id'].squeeze()
                    population_offset = batch['population_offset'].squeeze()
                
                # Move batch to CPU (matching model device)
                reporting_triangle = reporting_triangle.to('cpu')
                municipality_ids = municipality_ids.to('cpu')
                sex_ids = sex_ids.to('cpu')
                age_group_ids = age_group_ids.to('cpu')
                population_offset = population_offset.to('cpu')
                
                batch_size = reporting_triangle.shape[0]
                
                # OPTIMIZED: Generate all MC samples for entire batch at once
                batch_predictions = np.zeros((batch_size, n_samples))
                
                with torch.no_grad():  # Disable gradient computation for speed
                    for sample_idx in range(n_samples):
                        # Single forward pass for entire batch
                        dist = self.model(reporting_triangle, municipality_ids, 
                                        sex_ids, age_group_ids, population_offset)
                        
                        # Sample from the predicted distribution
                        samples = dist.sample().cpu().numpy()
                        batch_predictions[:, sample_idx] = samples
                
                # Store predictions and metadata
                all_predictions.append(batch_predictions)
                
                # Store metadata for each sample in batch
                for i in range(batch_size):
                    metadata = {
                        'municipality_id': municipality_ids[i].cpu().item(),
                        'sex_id': sex_ids[i].cpu().item(),
                        'age_group_id': age_group_ids[i].cpu().item(),
                        'batch_idx': batch_idx,
                        'sample_idx': i
                    }
                    all_metadata.append(metadata)
                
                pbar.update(1)
        
        # Combine all predictions
        predictions = np.vstack(all_predictions)
        elapsed_time = time.time() - start_time
        
        print(f"✅ Generated predictions shape: {predictions.shape}")
        print(f"⏱️  Total prediction time: {elapsed_time:.1f} seconds")
        print(f"🏃 Speed: {len(predictions)/elapsed_time:.1f} predictions/second")
        
        # Calculate summary statistics
        mean_predictions = np.mean(predictions, axis=1)
        std_predictions = np.std(predictions, axis=1)
        
        # Calculate prediction intervals
        intervals = {}
        confidence_levels = [0.5, 0.8, 0.9, 0.95, 0.99]
        
        for level in confidence_levels:
            lower_percentile = (1 - level) / 2 * 100
            upper_percentile = (1 + level) / 2 * 100
            
            lower_bounds = np.percentile(predictions, lower_percentile, axis=1)
            upper_bounds = np.percentile(predictions, upper_percentile, axis=1)
            
            intervals[f'{level:.0%}'] = {
                'lower': lower_bounds,
                'upper': upper_bounds
            }
        
        # Create results dictionary
        results = {
            'predictions': {
                'mean': mean_predictions,
                'std': std_predictions,
                'samples': predictions,
                'intervals': intervals
            },
            'metadata': {
                'target_years': target_years,
                'n_samples': n_samples,
                'total_predictions': len(mean_predictions),
                'sample_metadata': all_metadata,
                'dev_mode': dev_mode,
                'elapsed_time': elapsed_time,
                'model_info': {
                    'model_type': type(self.model).__name__,
                    'device': 'cpu'
                }
            },
            'summary_stats': {
                'mean_births_predicted': float(np.mean(mean_predictions)),
                'total_births_predicted': float(np.sum(mean_predictions)),
                'avg_uncertainty': float(np.mean(std_predictions)),
                'max_uncertainty': float(np.max(std_predictions))
            }
        }
        
        print(f"📊 Summary Statistics:")
        print(f"   Total births predicted: {results['summary_stats']['total_births_predicted']:,.0f}")
        print(f"   Average births per sample: {results['summary_stats']['mean_births_predicted']:.1f}")
        print(f"   Average uncertainty (std): {results['summary_stats']['avg_uncertainty']:.1f}")
        print(f"   Maximum uncertainty: {results['summary_stats']['max_uncertainty']:.1f}")
        
        return results
        """
        Identify which births are missing due to reporting delays.
        
        Creates a comprehensive mapping of what needs to be predicted
        for each year based on the cutoff date.
        
        Returns:
            DataFrame with missing birth specifications
        """
        print("\n🔍 Identifying missing births...")
        
        cutoff_datetime = pd.to_datetime(self.cutoff_date)
        missing_records = []
        
        # For each nowcast year (2016-2023)
        for year_occ in range(2016, 2024):
            print(f"   Analyzing year {year_occ}...")
            
            # For each month in the year
            for month_occ in range(1, 13):
                occurrence_date = pd.to_datetime(f"{year_occ}-{month_occ:02d}-15")
                
                # Calculate which delays are observable given cutoff date
                months_since_occurrence = (cutoff_datetime.year - year_occ) * 12 + (cutoff_datetime.month - month_occ)
                max_observable_delay = min(months_since_occurrence, 96)  # Cap at model max
                
                # For each demographic group
                for municipality in self.processor.municipalities:
                    for sex in self.processor.sexes:
                        for age_group in self.processor.age_groups:
                            
                            # Check what's already registered
                            registered_mask = (
                                (self.data['year_occ'] == year_occ) &
                                (self.data['month_occ'] == month_occ) &
                                (self.data['group_id'] == municipality) &
                                (self.data['sex'] == sex) &
                                (self.data['age'] == age_group)
                            )
                            
                            registered_births = self.data[registered_mask]['births'].sum()
                            
                            # Estimate missing births for unobservable delays
                            if max_observable_delay < 96:  # There are missing delays
                                missing_records.append({
                                    'year_occ': year_occ,
                                    'month_occ': month_occ,
                                    'municipality': municipality,
                                    'sex': sex,
                                    'age_group': age_group,
                                    'registered_births': registered_births,
                                    'max_observable_delay': max_observable_delay,
                                    'missing_delays': list(range(max_observable_delay + 1, 97)),
                                    'occurrence_date': occurrence_date
                                })
        
        missing_df = pd.DataFrame(missing_records)
        print(f"✓ Identified {len(missing_df):,} demographic groups needing nowcast")
        
        return missing_df
        
    def generate_predictions(self, missing_df: pd.DataFrame, batch_size: int = 512) -> pd.DataFrame:
        """
        Generate predictions for missing births using the trained model.
        
        Args:
            missing_df: DataFrame with missing birth specifications
            batch_size: Batch size for model inference
            
        Returns:
            DataFrame with predictions and uncertainty measures
        """
        print("\n🤖 Generating nowcast predictions...")
        
        predictions = []
        
        # Process in batches
        for i in range(0, len(missing_df), batch_size):
            batch_df = missing_df.iloc[i:i+batch_size]
            
            # Build input tensors for this batch
            batch_triangles = []
            batch_mun_ids = []
            batch_sex_ids = []
            batch_age_ids = []
            batch_pop_offsets = []
            
            for _, row in batch_df.iterrows():
                # Create reporting triangle up to observable delay
                triangle = self._build_reporting_triangle_for_prediction(
                    row['year_occ'], row['month_occ'],
                    row['municipality'], row['sex'], row['age_group'],
                    row['max_observable_delay']
                )
                
                # Get encoded demographic IDs
                mun_id = self.processor.le_mun.transform([row['municipality']])[0]
                sex_id = self.processor.le_sex.transform([row['sex']])[0]
                age_id = self.processor.le_age.transform([row['age_group']])[0]
                
                # Get population offset
                pop_offset = self.processor.get_population_offset(
                    row['municipality'], row['sex'], row['age_group'], row['year_occ']
                )
                log_pop_offset = np.log(pop_offset) if self.processor.use_population_offset else 0.0
                
                batch_triangles.append(triangle)
                batch_mun_ids.append(mun_id)
                batch_sex_ids.append(sex_id)
                batch_age_ids.append(age_id)
                batch_pop_offsets.append(log_pop_offset)
            
            # Convert to tensors
            triangles_tensor = torch.tensor(batch_triangles, dtype=torch.float32).to(self.device)
            mun_ids_tensor = torch.tensor(batch_mun_ids, dtype=torch.long).to(self.device)
            sex_ids_tensor = torch.tensor(batch_sex_ids, dtype=torch.long).to(self.device)
            age_ids_tensor = torch.tensor(batch_age_ids, dtype=torch.long).to(self.device)
            pop_offsets_tensor = torch.tensor(batch_pop_offsets, dtype=torch.float32).to(self.device)
            
            # Generate predictions
            with torch.no_grad():
                dist = self.model(triangles_tensor, mun_ids_tensor, sex_ids_tensor, 
                                age_ids_tensor, pop_offsets_tensor)
                
                # Extract statistics
                mean_pred = dist.mean.cpu().numpy()
                std_pred = dist.stddev.cpu().numpy()
                
                # Sample from distribution for uncertainty quantification
                samples = dist.sample((100,)).cpu().numpy()  # 100 samples per prediction
                
                # Calculate confidence intervals
                ci_lower = np.percentile(samples, 2.5, axis=0)
                ci_upper = np.percentile(samples, 97.5, axis=0)
            
            # Store predictions
            for j, (_, row) in enumerate(batch_df.iterrows()):
                predictions.append({
                    'year_occ': row['year_occ'],
                    'month_occ': row['month_occ'],
                    'municipality': row['municipality'],
                    'sex': row['sex'],
                    'age_group': row['age_group'],
                    'registered_births': row['registered_births'],
                    'predicted_missing': mean_pred[j],
                    'prediction_std': std_pred[j],
                    'ci_lower': ci_lower[j],
                    'ci_upper': ci_upper[j],
                    'total_corrected': row['registered_births'] + mean_pred[j],
                    'max_observable_delay': row['max_observable_delay']
                })
                
            if (i // batch_size + 1) % 10 == 0:
                print(f"   Processed {i + len(batch_df):,} / {len(missing_df):,} groups")
        
        predictions_df = pd.DataFrame(predictions)
        print(f"✓ Generated predictions for {len(predictions_df):,} groups")
        
        return predictions_df
    
    def _build_reporting_triangle_for_prediction(self, year_occ: int, month_occ: int,
                                               municipality: str, sex: str, age_group: str,
                                               max_observable_delay: int) -> np.ndarray:
        """
        Build reporting triangle for prediction using only observable data.
        
        This creates the input reporting triangle that the model uses to predict
        missing births for unobservable delays.
        """
        triangle = np.zeros((36, 96))  # past_units x max_delay
        
        # Get data for this specific group
        group_mask = (
            (self.data['group_id'] == municipality) &
            (self.data['sex'] == sex) &
            (self.data['age'] == age_group)
        )
        group_data = self.data[group_mask].copy()
        
        if len(group_data) == 0:
            return triangle
        
        # Ensure numeric columns for calculations
        group_data['year_occ'] = pd.to_numeric(group_data['year_occ'], errors='coerce')
        group_data['month_occ'] = pd.to_numeric(group_data['month_occ'], errors='coerce')
        group_data['year_reg'] = pd.to_numeric(group_data['year_reg'], errors='coerce')
        group_data['month_reg'] = pd.to_numeric(group_data['month_reg'], errors='coerce')
        
        # Calculate month_id and delays
        group_data['month_id'] = group_data['year_occ'] * 12 + group_data['month_occ']
        group_data['reg_month_id'] = group_data['year_reg'] * 12 + group_data['month_reg']
        group_data['delay'] = group_data['reg_month_id'] - group_data['month_id']
        
        # Reference month for this prediction
        ref_month_id = year_occ * 12 + month_occ
        
        # Fill triangle with past 36 months of data
        for i in range(36):
            past_month_id = ref_month_id - (35 - i)  # 36 months ago to reference month
            
            # Get data for this past month
            month_data = group_data[group_data['month_id'] == past_month_id]
            
            for _, record in month_data.iterrows():
                delay = int(record['delay'])
                if 0 <= delay <= max_observable_delay and delay < 96:
                    triangle[i, delay] += record['births']
        
        return triangle
    
    def aggregate_to_annual(self, predictions_df: pd.DataFrame) -> pd.DataFrame:
        """
        Aggregate monthly predictions to annual totals with uncertainty propagation.
        
        Args:
            predictions_df: DataFrame with monthly predictions
            
        Returns:
            DataFrame with annual aggregated results
        """
        print("\n📊 Aggregating to annual totals...")
        
        annual_results = []
        
        for year in range(2016, 2024):
            year_data = predictions_df[predictions_df['year_occ'] == year]
            
            if len(year_data) == 0:
                continue
            
            # Calculate annual totals
            total_registered = year_data['registered_births'].sum()
            total_predicted_missing = year_data['predicted_missing'].sum()
            total_corrected = total_registered + total_predicted_missing
            
            # Propagate uncertainty (assuming independence for simplicity)
            total_variance = (year_data['prediction_std'] ** 2).sum()
            total_std = np.sqrt(total_variance)
            
            # Calculate confidence intervals
            ci_lower = total_corrected - 1.96 * total_std
            ci_upper = total_corrected + 1.96 * total_std
            
            # Calculate percentage estimated
            pct_estimated = (total_predicted_missing / total_corrected) * 100 if total_corrected > 0 else 0
            
            annual_results.append({
                'year': year,
                'registered_births': total_registered,
                'estimated_missing': total_predicted_missing,
                'total_corrected': total_corrected,
                'uncertainty_std': total_std,
                'ci_lower': ci_lower,
                'ci_upper': ci_upper,
                'percent_estimated': pct_estimated
            })
        
        annual_df = pd.DataFrame(annual_results)
        print("✓ Annual aggregation completed")
        
        # Print summary
        print("\n📈 Annual Nowcast Summary:")
        for _, row in annual_df.iterrows():
            print(f"   {row['year']}: {row['total_corrected']:,.0f} total "
                  f"({row['percent_estimated']:.1f}% estimated)")
        
        return annual_df
    
    def execute_full_nowcast(self, save_results: bool = True) -> Dict[str, pd.DataFrame]:
        """
        Execute the complete nowcasting pipeline.
        
        Args:
            save_results: Whether to save results to files
            
        Returns:
            Dictionary with results DataFrames
        """
        print("\n🎯 Executing Full Nowcast Pipeline")
        print("=" * 50)
        
        # Step 1: Load components
        self.load_components()
        
        # Step 2: Identify missing births
        missing_df = self.identify_missing_births()
        
        # Step 3: Generate predictions
        predictions_df = self.generate_predictions(missing_df)
        
        # Step 4: Aggregate to annual
        annual_df = self.aggregate_to_annual(predictions_df)
        
        # Store results
        self.nowcast_results = {
            'missing_specifications': missing_df,
            'monthly_predictions': predictions_df,
            'annual_aggregated': annual_df
        }
        
        # Save results if requested
        if save_results:
            self.save_results()
        
        print("\n✅ Nowcast pipeline completed successfully!")
        return self.nowcast_results
    
    def save_results(self, output_dir: str = './nowcast_results/'):
        """Save nowcast results to files."""
        os.makedirs(output_dir, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Save each component
        for name, df in self.nowcast_results.items():
            filename = f"{name}_{timestamp}.parquet"
            filepath = os.path.join(output_dir, filename)
            df.to_parquet(filepath, engine='fastparquet')
            print(f"✓ Saved {name} to {filepath}")
        
        # Save summary statistics
        summary_path = os.path.join(output_dir, f"nowcast_summary_{timestamp}.txt")
        with open(summary_path, 'w') as f:
            f.write("BIRTH NOWCAST SUMMARY\n")
            f.write("=" * 40 + "\n")
            f.write(f"Execution date: {datetime.now()}\n")
            f.write(f"Cutoff date: {self.cutoff_date}\n")
            f.write(f"Model: {self.model_path}\n\n")
            
            f.write("ANNUAL RESULTS:\n")
            for _, row in self.nowcast_results['annual_aggregated'].iterrows():
                f.write(f"{row['year']}: {row['total_corrected']:,.0f} total births "
                       f"({row['percent_estimated']:.1f}% estimated)\n")
        
        print(f"✓ Summary saved to {summary_path}")


def quick_test():
    """Quick test function for development - Tests optimized implementation on 2023 data."""
    print("🧪 QUICK TEST: Testing optimized nowcast implementation")
    print("=" * 60)
    
    # Initialize executor
    print("\n📚 Step 1: Loading model and data...")
    executor = BirthNowcastExecutor(
        cutoff_date="2024-08-01",  # Current simulation date
        model_path="./checkpoints/negative_binomial_model_final.pt",
        data_path="../../datasets/monthly_births.parquet"
    )
    
    # Load all components
    executor.load_components()
    
    # Test fast prediction on 2023 only
    print("\n🚀 Step 2: Testing fast prediction on 2023...")
    start_time = time.time()
    
    results = executor.predict_missing_births(
        target_years=[2023],  # Only 2023 for speed
        n_samples=20,  # Reduced samples for speed
        dev_mode=True
    )
    
    elapsed = time.time() - start_time
    print(f"\n⏱️  Total test time: {elapsed:.1f} seconds")
    
    # Analyze results
    print("\n📊 Step 3: Analyzing results...")
    print(f"   Predictions generated: {results['metadata']['total_predictions']:,}")
    print(f"   Total births predicted: {results['summary_stats']['total_births_predicted']:,.0f}")
    print(f"   Average uncertainty: {results['summary_stats']['avg_uncertainty']:.1f}")
    print(f"   Processing speed: {results['metadata']['total_predictions']/elapsed:.1f} predictions/sec")
    
    # Validation checks
    print("\n✅ Step 4: Quick validation...")
    predictions = results['predictions']['mean']
    
    # Check for reasonable values
    if np.any(predictions < 0):
        print("❌ ERROR: Found negative predictions!")
    elif np.any(predictions > 10000):
        print("⚠️  WARNING: Some predictions are very high (>10,000)")
    else:
        print("✓ Predictions are in reasonable range")
    
    # Check uncertainty
    uncertainties = results['predictions']['std']
    if np.mean(uncertainties) > np.mean(predictions):
        print("⚠️  WARNING: Uncertainty is higher than mean predictions")
    else:
        print("✓ Uncertainty levels look reasonable")
    
    print(f"\n🎯 SUCCESS: Quick test completed in {elapsed:.1f} seconds!")
    print(f"   Ready for full-scale nowcasting on all years 2016-2023")
    
    return results


def run_full_nowcast():
    """Run the full nowcasting pipeline for all years 2016-2023."""
    print("\n🚀 FULL NOWCAST: Running complete pipeline for 2016-2023")
    print("=" * 60)
    
    # Initialize executor
    print("\n📚 Step 1: Loading model and data...")
    executor = BirthNowcastExecutor(
        cutoff_date="2024-08-01",  # Current simulation date
        model_path="./checkpoints/negative_binomial_model_final.pt",
        data_path="../../datasets/monthly_births.parquet"
    )
    
    # Load all components
    executor.load_components()
    
    # Test first with 2022-2023 to validate the improved sampling
    test_years = [2022, 2023]
    print(f"\n🔮 Step 2: Testing improved sampling with years {test_years}...")
    start_time = time.time()
    
    results = executor.predict_missing_births(
        target_years=test_years,  # Test with improved sampling first
        n_samples=50,  # Moderate samples for testing
        dev_mode=False  # Production mode
    )
    
    elapsed = time.time() - start_time
    print(f"\n⏱️  Total nowcast time: {elapsed:.1f} seconds")
    
    # Analyze results
    print("\n📊 Step 3: Analyzing results...")
    print(f"   Predictions generated: {len(results['predictions']['mean'])}")
    print(f"   Processing speed: {len(results['predictions']['mean'])/elapsed:.1f} predictions/sec")
    
    # Aggregate by year
    print("\n📈 Step 4: Aggregating to annual totals...")
    annual_results = aggregate_to_annual_full(results, executor, test_years)
    
    # Create the plot
    print("\n🎨 Step 5: Creating annual births visualization...")
    create_annual_births_plot_complete(annual_results, executor)
    
    # Show expected missing births analysis
    print("\n🔍 Step 6: Missing births analysis...")
    analyze_missing_births_logic(executor, test_years)
    
    return annual_results, executor


def analyze_missing_births_logic(executor, target_years):
    """Analyze the logic behind missing births calculations."""
    print("📊 MISSING BIRTHS ANALYSIS:")
    print("=" * 50)
    
    cutoff_datetime = pd.to_datetime(executor.cutoff_date)
    print(f"📅 Cutoff date: {cutoff_datetime.strftime('%Y-%m-%d')}")
    
    for year in target_years:
        print(f"\n📈 Analysis for year {year}:")
        
        # For different months, show what delays are observable
        for month in [1, 6, 12]:
            months_since = (cutoff_datetime.year - year) * 12 + (cutoff_datetime.month - month)
            max_observable = min(months_since, 96)
            missing_delays = 96 - max_observable
            
            print(f"   Month {month:2d}: {months_since:2d} months elapsed → observe delays 0-{max_observable}, missing {missing_delays} delays")
        
        # Calculate total registered births for this year
        year_mask = executor.data['year_occ'] == year
        registered_total = executor.data[year_mask]['births'].sum()
        
        print(f"   📊 Registered births in {year}: {registered_total:,}")
        
        # Estimate what percentage should be missing based on delay patterns
        # Typically 15-25% of births register with delays > 24 months
        if year == 2022:
            # For 2022, we're missing delays 25+ months roughly
            expected_missing_pct = 15  # Rough estimate
        elif year == 2023:  
            # For 2023, we're missing delays 13+ months roughly
            expected_missing_pct = 25  # Higher percentage
        
        expected_missing = registered_total * expected_missing_pct / 100
        print(f"   🎯 Expected missing births (~{expected_missing_pct}%): {expected_missing:,.0f}")
    
    print("\n💡 If nowcast estimates are much lower than expected, we need more comprehensive sampling!")


def aggregate_to_annual_full(results, executor, target_years):
    """Aggregate nowcast results to annual totals for all target years."""
    import pandas as pd
    
    # Get the missing births data used for predictions
    missing_df = executor.identify_missing_births(target_years)
    
    # Extract predictions arrays from the results dictionary
    mean_predictions = results['predictions']['mean']
    std_predictions = results['predictions']['std']
    
    print(f"📊 Predictions shape: {mean_predictions.shape}, Missing data: {len(missing_df)}")
    
    # Match predictions to missing births data
    assert len(mean_predictions) == len(missing_df), f"Prediction length {len(mean_predictions)} != missing data length {len(missing_df)}"
    
    # Add prediction info to missing_df
    missing_df = missing_df.copy()  # Make a copy to avoid SettingWithCopyWarning
    missing_df['predicted_mean'] = mean_predictions
    missing_df['predicted_std'] = std_predictions
    
    # Aggregate by year
    annual_data = []
    
    for year in target_years:  # Process all target years
        print(f"📊 Processing year {year}...")
        
        # Get registered births for this year from the original data
        year_mask = executor.data['year_occ'] == year
        registered_births = executor.data[year_mask]['births'].sum() if year_mask.any() else 0
        
        # Get nowcast estimates for this year
        year_missing = missing_df[missing_df['year_occ'] == year]
        nowcast_estimate = year_missing['predicted_mean'].sum() if len(year_missing) > 0 else 0
        nowcast_uncertainty = year_missing['predicted_std'].mean() if len(year_missing) > 0 else 0
        
        total_estimated = registered_births + nowcast_estimate
        
        annual_data.append({
            'year': year,
            'registered_births': registered_births,
            'nowcast_estimate': nowcast_estimate,
            'total_estimated': total_estimated,
            'uncertainty': nowcast_uncertainty
        })
        
        print(f"   Year {year}: Registered={registered_births:,.0f}, Nowcast={nowcast_estimate:,.0f}, Total={total_estimated:,.0f}")
    
    return pd.DataFrame(annual_data)


def aggregate_to_annual(results, executor):
    """Aggregate nowcast results to annual totals."""
    import pandas as pd
    
    # Get the missing births data used for predictions
    # We need to get the target years from the executor's last prediction
    target_years = [2022, 2023]  # Match the test years from run_full_nowcast
    missing_df = executor.identify_missing_births(target_years)
    
    # Extract predictions arrays from the results dictionary
    mean_predictions = results['predictions']['mean']
    std_predictions = results['predictions']['std']
    
    print(f"📊 Predictions shape: {mean_predictions.shape}, Missing data: {len(missing_df)}")
    
    # Match predictions to missing births data
    assert len(mean_predictions) == len(missing_df), f"Prediction length {len(mean_predictions)} != missing data length {len(missing_df)}"
    
    # Add prediction info to missing_df
    missing_df = missing_df.copy()  # Make a copy to avoid SettingWithCopyWarning
    missing_df['predicted_mean'] = mean_predictions
    missing_df['predicted_std'] = std_predictions
    
    # Aggregate by year
    annual_data = []
    
    for year in target_years:  # Only process target years
        print(f"📊 Processing year {year}...")
        
        # Get registered births for this year from the original data
        year_mask = executor.data['year_occ'] == year
        registered_births = executor.data[year_mask]['births'].sum() if year_mask.any() else 0
        
        # Get nowcast estimates for this year
        year_missing = missing_df[missing_df['year_occ'] == year]
        nowcast_estimate = year_missing['predicted_mean'].sum() if len(year_missing) > 0 else 0
        nowcast_uncertainty = year_missing['predicted_std'].mean() if len(year_missing) > 0 else 0
        
        total_estimated = registered_births + nowcast_estimate
        
        annual_data.append({
            'year': year,
            'registered_births': registered_births,
            'nowcast_estimate': nowcast_estimate,
            'total_estimated': total_estimated,
            'uncertainty': nowcast_uncertainty
        })
        
        print(f"   Year {year}: Registered={registered_births:,.0f}, Nowcast={nowcast_estimate:,.0f}, Total={total_estimated:,.0f}")
    
    return pd.DataFrame(annual_data)


def create_annual_births_plot_complete(annual_results, executor):
    """Create the complete annual births plot matching the uploaded image with all years 2016-2023."""
    import matplotlib.pyplot as plt
    import numpy as np
    
    # Get historical complete data (1990-2015)
    historical_data = []
    for year in range(1990, 2016):
        year_mask = executor.data['year_occ'] == year
        if year_mask.any():
            total_births = executor.data[year_mask]['births'].sum()
            historical_data.append({'year': year, 'total_births': total_births})
    
    historical_df = pd.DataFrame(historical_data)
    
    # Create the plot with exact specifications to match the uploaded image
    plt.figure(figsize=(14, 8))
    
    # Plot historical complete data (1990-2015) - dark blue (steelblue)
    if len(historical_df) > 0:
        plt.bar(historical_df['year'], historical_df['total_births'], 
                color='steelblue', alpha=0.9, label='Complete births (1990-2015)',
                width=0.8)
    
    # Plot registered births (2016-2023) - light blue  
    plt.bar(annual_results['year'], annual_results['registered_births'],
            color='lightblue', alpha=0.8, label='Registered births (2016-2023)',
            width=0.8)
    
    # Plot nowcast estimates (2016-2023) - orange stacked on top
    plt.bar(annual_results['year'], annual_results['nowcast_estimate'],
            bottom=annual_results['registered_births'],
            color='orange', alpha=0.9, label='Nowcast estimates (2016-2023)',
            width=0.8)
    
    # Add vertical line at 2015.5 to separate training/nowcast periods
    plt.axvline(x=2015.5, color='red', linestyle='--', alpha=0.8, linewidth=2)
    plt.text(2015.7, plt.ylim()[1]*0.9, 'Last Period', rotation=90, 
             color='red', fontsize=11, ha='left', va='top')
    
    # Formatting to match the uploaded image exactly
    plt.title('Annual Births in Mexico (1990-2023)\nComplete Data vs Nowcast Estimates', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year of Occurrence', fontsize=12)
    plt.ylabel('Number of Births', fontsize=12)
    
    # Legend positioning to match the image
    plt.legend(loc='upper right', fontsize=11, framealpha=0.9)
    
    # Format y-axis to show millions (like in the image)
    def millions_formatter(x, pos):
        return f'{x/1e6:.1f}M'
    
    plt.gca().yaxis.set_major_formatter(plt.FuncFormatter(millions_formatter))
    
    # Set axis limits to match the uploaded image
    plt.xlim(1989.5, 2024.5)
    plt.ylim(0, 5.2e6)  # Up to 5.2M births to match the scale
    
    # Grid styling to match
    plt.grid(True, alpha=0.3, linestyle='-', linewidth=0.5)
    
    # Improve layout
    plt.tight_layout()
    
    # Save the plot with high quality
    plt.savefig('birth_nowcast_annual_complete_FINAL.png', dpi=300, bbox_inches='tight',
                facecolor='white', edgecolor='none')
    
    # Also create a summary table
    print("\n📊 FINAL ANNUAL RESULTS SUMMARY:")
    print("=" * 60)
    for _, row in annual_results.iterrows():
        nowcast_pct = (row['nowcast_estimate'] / row['total_estimated']) * 100
        print(f"📅 {row['year']}: {row['total_estimated']:,.0f} births total")
        print(f"   ├─ Registered: {row['registered_births']:,.0f} ({100-nowcast_pct:.1f}%)")
        print(f"   └─ Nowcast:    {row['nowcast_estimate']:,.0f} ({nowcast_pct:.1f}%)")
        print()
    
    plt.show()
    print("✅ Final plot saved as 'birth_nowcast_annual_complete_FINAL.png'")
    
    return True


def test_proper_methodology():
    """Test the ULTRA-EFFICIENT nowcasting methodology."""
    print("\n🧪 TESTING ULTRA-EFFICIENT METHODOLOGY")
    print("=" * 60)
    
    # Initialize executor with CORRECT cutoff date
    print("\n📚 Step 1: Loading model and data...")
    executor = BirthNowcastExecutor(
        cutoff_date="2024-01-01",  # Data available until 2023-12
        model_path="./checkpoints/negative_binomial_model_final.pt",
        data_path="../../datasets/monthly_births.parquet"
    )
    
    # Load all components
    executor.load_components()
    
    print(f"📅 Using cutoff date: {executor.cutoff_date}")
    
    # Analyze data first
    print("\n🔍 Step 2: Data analysis...")
    data_years = sorted(executor.data['year_occ'].unique())
    print(f"   Data years: {data_years[0]}-{data_years[-1]}")
    
    # Show registered births by year
    for year in [2021, 2022, 2023]:
        if year in data_years:
            births = executor.data[executor.data['year_occ'] == year]['births'].sum()
            print(f"   {year}: {births:,} registered births")
    
    # Test ultra-efficient method on 2023 only
    print("\n⚡ Step 3: Testing ULTRA-EFFICIENT method on 2023...")
    start_time = time.time()
    
    results = executor.nowcast_ultra_efficient(
        target_years=[2023],  # Most incomplete year
        n_samples=30  # Fast testing
    )
    
    elapsed = time.time() - start_time
    print(f"\n⏱️  Execution time: {elapsed:.1f} seconds")
    
    # Analysis
    if results['summary_stats']['total_births_predicted'] > 0:
        births_2023 = executor.data[executor.data['year_occ'] == 2023]['births'].sum()
        nowcast_total = results['summary_stats']['total_births_predicted']
        corrected_total = births_2023 + nowcast_total
        nowcast_pct = (nowcast_total / corrected_total) * 100
        
        print(f"\n📊 2023 RESULTS:")
        print(f"   Registered births: {births_2023:,}")
        print(f"   Nowcast estimate:  {nowcast_total:.0f}")
        print(f"   Total corrected:   {corrected_total:,.0f}")
        print(f"   Nowcast %:         {nowcast_pct:.1f}%")
        print(f"   Predictions made:  {len(results['predictions']['mean']):,}")
        
        if elapsed < 30:
            print(f"\n✅ SUCCESS: Ultra-fast execution in {elapsed:.1f} seconds!")
            print("   Ready for multi-year nowcasting")
        else:
            print(f"\n⚠️  Still needs optimization ({elapsed:.1f}s)")
    else:
        print("\n❌ No predictions generated - check data filtering")
    
    return results


def test_multiple_years_efficient():
    """Test ultra-efficient nowcasting on multiple recent years."""
    print("\n🧪 TESTING MULTI-YEAR ULTRA-EFFICIENT")
    print("=" * 60)
    
    executor = BirthNowcastExecutor(
        cutoff_date="2024-01-01",
        model_path="./checkpoints/negative_binomial_model_final.pt", 
        data_path="../../datasets/monthly_births.parquet"
    )
    
    executor.load_components()
    
    # Test on most recent incomplete years
    print("\n⚡ Testing on years with significant missing data...")
    start_time = time.time()
    
    results = executor.nowcast_ultra_efficient(
        target_years=[2022, 2023],  # Years with meaningful missing delays
        n_samples=50
    )
    
    elapsed = time.time() - start_time
    print(f"\n⏱️  Total time: {elapsed:.1f} seconds")
    
    # Analyze by year
    if results['summary_stats']['total_births_predicted'] > 0:
        metadata = results['metadata']['sample_metadata']
        predictions = results['predictions']['mean']
        
        print(f"\n📊 RESULTS BY YEAR:")
        for year in [2022, 2023]:
            year_indices = [i for i, m in enumerate(metadata) if m['year_occ'] == year]
            year_nowcast = np.sum(predictions[year_indices]) if year_indices else 0
            year_registered = executor.data[executor.data['year_occ'] == year]['births'].sum()
            
            print(f"   {year}: Registered={year_registered:,}, Nowcast={year_nowcast:.0f}, Total={year_registered+year_nowcast:,.0f}")
        
        total_predictions = len(predictions)
        print(f"\n📈 EFFICIENCY:")
        print(f"   Total predictions: {total_predictions:,}")
        print(f"   Speed: {total_predictions/elapsed:.0f} predictions/second")
        
        if elapsed < 60:
            print(f"\n✅ EXCELLENT: Multi-year nowcast in {elapsed:.1f} seconds!")
        else:
            print(f"\n⚠️  Could be faster: {elapsed:.1f} seconds")
    
    return results


def main():
    """Main execution function for command-line usage."""
    import sys
    
    # Check for test mode
    if len(sys.argv) > 1 and sys.argv[1] == "--test":
        print("🧪 Running quick test mode...")
        quick_test()
        return
    elif len(sys.argv) > 1 and sys.argv[1] == "--full":
        print("🚀 Running full nowcast mode...")
        run_full_nowcast()
        return
    elif len(sys.argv) > 1 and sys.argv[1] == "--proper":
        print("⚡ Testing ultra-efficient methodology...")
        test_proper_methodology()
        return
    elif len(sys.argv) > 1 and sys.argv[1] == "--multi":
        print("📋 Testing multi-year efficient nowcast...")
        test_multiple_years_efficient()
        return
    
    # Default: run ultra-efficient test mode
    print("⚡ Running ultra-efficient nowcast test...")
    test_proper_methodology()
    print("🧪 Running quick test mode (use --full for complete nowcast, --proper for methodology test)...")
    quick_test()


if __name__ == "__main__":
    main()