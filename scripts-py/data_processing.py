"""
Data Processing Module for Birth Nowcasting
Handles creation of reporting triangles and data preparation for the NowcastPNN model
"""

import pandas as pd
import numpy as np
import torch
from torch.utils.data import Dataset, DataLoader
from sklearn.preprocessing import LabelEncoder
from typing import Dict, Tuple, Optional, List
from datetime import datetime
import warnings
import pyreadr
warnings.filterwarnings('ignore')


class BirthDataProcessor:
    """
    Processes raw birth registration data into reporting triangles for nowcasting.
    
    The reporting triangle is a matrix of size [past_units, max_delay] where:
    - past_units: number of past months to consider
    - max_delay: maximum reporting delay in months
    """
    
    def __init__(self, 
                 past_units: int = 36,
                 max_delay: int = 96,
                 min_births_threshold: int = 10,
                 nowcast_cutoff_year: int = 2016,
                 current_date = None):
        """
        Initialize the data processor.
        
        Args:
            past_units: Number of past months to include in reporting triangles
            max_delay: Maximum reporting delay to consider (months)
            min_births_threshold: Minimum number of births to include a group
            nowcast_cutoff_year: Year from which to start nowcasting (vs training on complete data)
            current_date: Current date for nowcasting calculations (None = use latest data date)
        """
        self.past_units = past_units
        self.max_delay = max_delay
        self.max_delay_months = max_delay
        self.min_births_threshold = min_births_threshold
        self.nowcast_cutoff_year = nowcast_cutoff_year
        self.current_date = current_date
        
        # Label encoders for categorical variables
        self.le_mun = LabelEncoder()
        self.le_sex = LabelEncoder()
        self.le_age = LabelEncoder()
        
        # Normalization baselines (calculated from training period)
        self.municipality_baselines = None
        
        # Population data for offset
        self.population_data = None
        self.use_population_offset = False
        self.use_log_transform = False
        
        # Metadata
        self.n_municipalities = 0
        self.n_sexes = 0
        self.n_age_groups = 0
        
        # Data containers
        self.data = None
        self.municipalities = None
        self.sexes = None
        self.age_groups = None
        self.date_range = None
    
    def filter_reproductive_ages(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Filter to reproductive ages: females 15-44, males 15-69
        """
        print("Filtering to reproductive ages...")
        initial_count = len(df)
        
        # Define reproductive age ranges
        female_ages = ['15-19', '20-24', '25-29', '30-34', '35-39', '40-44']
        male_ages = ['15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69']
        
        # Filter by sex and age
        female_mask = (df['sex'] == 'female') & (df['age'].isin(female_ages))
        male_mask = (df['sex'] == 'male') & (df['age'].isin(male_ages))
        
        df_filtered = df[female_mask | male_mask].copy()
        
        print(f"Filtered from {initial_count:,} to {len(df_filtered):,} records")
        return df_filtered
    
    def apply_birth_threshold(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Applica una soglia minima sul numero di record per (group_id, sex, age).
        Versione veloce senza set_index/isin.
        """
        print(f"Applying minimum births threshold of {self.min_births_threshold} (by row count)...")
        initial_count = len(df)

        # Sanity check colonne
        for col in ['group_id', 'sex', 'age']:
            if col not in df.columns:
                raise KeyError(f"Column '{col}' not found in dataframe at apply_birth_threshold")

        # Conta righe per gruppo in modo vectorized
        counts = (
            df.groupby(['group_id', 'sex', 'age'], observed=True)
            .size()
            .reset_index(name='n')
        )

        valid = counts.loc[counts['n'] >= self.min_births_threshold, ['group_id', 'sex', 'age']]
        df_filtered = df.merge(valid, on=['group_id', 'sex', 'age'], how='inner')

        print(f"Filtered from {initial_count:,} to {len(df_filtered):,} records")
        print(f"Kept {len(valid):,} groups with ≥ {self.min_births_threshold} rows")
        return df_filtered

    
    def fit_label_encoders(self, df: pd.DataFrame):
        """
        Fit label encoders for categorical variables.
        """
        print("Fitting label encoders...")
        
        # Fit encoders
        self.le_mun.fit(df['group_id'].unique())
        self.le_sex.fit(df['sex'].unique())
        self.le_age.fit(df['age'].unique())
        
        # Store metadata
        self.n_municipalities = len(self.le_mun.classes_)
        self.n_sexes = len(self.le_sex.classes_)
        self.n_age_groups = len(self.le_age.classes_)
        
        # Store unique values
        self.municipalities = list(self.le_mun.classes_)
        self.sexes = list(self.le_sex.classes_)
        self.age_groups = list(self.le_age.classes_)
        
        print(f"Encoded {self.n_municipalities} municipalities")
        print(f"Encoded {self.n_sexes} sex categories: {list(self.le_sex.classes_)}")
        print(f"Encoded {self.n_age_groups} age groups: {list(self.le_age.classes_)}")

    def calculate_municipality_baselines(self, df: pd.DataFrame):
        """
        Calculate baseline birth rates for each municipality-sex-age combination
        using complete historical data (1990-2015) for normalization.
        """
        print("Calculating municipality baselines for normalization...")
        
        # Filter to training period (complete data)
        training_data = df[df['year_occ'] <= 2015].copy()
        
        # Calculate average births per month for each municipality-sex-age combination
        baselines = (training_data.groupby(['group_id', 'sex', 'age'])['births']
                    .mean()
                    .reset_index()
                    .rename(columns={'births': 'baseline'}))
        
        # Add small constant to avoid division by zero
        baselines['baseline'] = baselines['baseline'] + 1.0
        
        # Store as dictionary for fast lookup
        self.municipality_baselines = {}
        for _, row in baselines.iterrows():
            key = (row['group_id'], row['sex'], row['age'])
            self.municipality_baselines[key] = row['baseline']
        
        print(f"Calculated baselines for {len(self.municipality_baselines)} municipality-sex-age combinations")
        print(f"Baseline range: {baselines['baseline'].min():.1f} - {baselines['baseline'].max():.1f}")

    def normalize_births(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Normalize births by municipality baseline to reduce scale differences.
        Optimized version using vectorized operations.
        """
        if self.municipality_baselines is None:
            print("Warning: No baselines calculated. Skipping normalization.")
            return df
        
        print("Applying normalization (vectorized)...")
        
        # Convert baselines dictionary to DataFrame for fast merging
        baseline_df = pd.DataFrame([
            {'group_id': k[0], 'sex': k[1], 'age': k[2], 'baseline': v}
            for k, v in self.municipality_baselines.items()
        ])
        
        # Use merge for fast vectorized lookup (much faster than apply)
        df_norm = df.merge(baseline_df, on=['group_id', 'sex', 'age'], how='left')
        
        # Fill missing baselines with 1.0 and normalize
        df_norm['baseline'] = df_norm['baseline'].fillna(1.0)
        df_norm['births_original'] = df_norm['births']  # Keep original
        df_norm['births'] = df_norm['births'] / df_norm['baseline']  # Vectorized division
        
        # Drop the baseline column (no longer needed)
        df_norm = df_norm.drop(columns=['baseline'])
        
        print(f"Normalization completed. Range: {df_norm['births'].min():.3f} - {df_norm['births'].max():.3f}")
        
        return df_norm

    def load_population_data(self, population_path: str = '../../datasets/population_new_mun.RDS'):
        """
        Load and process population data for offset calculation.
        """
        print("Loading population data...")
        
        try:
            # Load R dataset
            result = pyreadr.read_r(population_path)
            pop_df = result[None]  # Default key for single dataframe
            
            print(f"Raw population data: {len(pop_df):,} records")
            print("Columns:", list(pop_df.columns))
            
            # Clean and standardize column names (adapt based on actual structure)
            if 'year' in pop_df.columns:
                # Convert years to int instead of float
                pop_df['year'] = pop_df['year'].astype(int)
                
            # Identify the correct column names (may need adjustment based on actual data)
            municipality_col = None
            year_col = None
            sex_col = None  
            age_col = None
            population_col = None
            
            for col in pop_df.columns:
                if 'mun' in col.lower() or 'group_id' in col.lower():
                    municipality_col = col
                elif 'year' in col.lower() or 'ano' in col.lower():
                    year_col = col
                elif 'sex' in col.lower() or 'sexo' in col.lower():
                    sex_col = col
                elif 'age' in col.lower() or 'edad' in col.lower():
                    age_col = col
                elif 'pop' in col.lower() or 'poblacion' in col.lower() or col.lower() in ['population', 'n']:
                    population_col = col
            
            print(f"Detected columns: municipality={municipality_col}, year={year_col}, sex={sex_col}, age={age_col}, population={population_col}")
            
            # Rename columns to standard names
            column_mapping = {}
            if municipality_col: column_mapping[municipality_col] = 'group_id'
            if year_col: column_mapping[year_col] = 'year'
            if sex_col: column_mapping[sex_col] = 'sex'
            if age_col: column_mapping[age_col] = 'age'
            if population_col: column_mapping[population_col] = 'population'
            
            pop_df = pop_df.rename(columns=column_mapping)
            
            # Standardize sex values to match birth data
            if 'sex' in pop_df.columns:
                pop_df['sex'] = pop_df['sex'].str.lower().replace({
                    'm': 'male', 'f': 'female', 'h': 'male', 'mujer': 'female', 'hombre': 'male'
                })
            
            # Filter to reproductive ages (same as birth data)
            if 'age' in pop_df.columns and 'sex' in pop_df.columns:
                female_ages = ['15-19', '20-24', '25-29', '30-34', '35-39', '40-44']
                male_ages = ['15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69']
                
                # Filter to reproductive ages only
                female_mask = (pop_df['sex'] == 'female') & (pop_df['age'].isin(female_ages))
                male_mask = (pop_df['sex'] == 'male') & (pop_df['age'].isin(male_ages))
                pop_df = pop_df[female_mask | male_mask].copy()
                
                print(f"Filtered to reproductive ages: {len(pop_df):,} records")
            
            # Convert years to int and ensure population is numeric
            if 'year' in pop_df.columns:
                pop_df['year'] = pop_df['year'].fillna(0).astype(int)
            if 'population' in pop_df.columns:
                pop_df['population'] = pd.to_numeric(pop_df['population'], errors='coerce').fillna(0)
            
            # Store processed population data
            self.population_data = pop_df
            self.use_population_offset = True
            
            print(f"✅ Population data loaded: {len(pop_df):,} records")
            print(f"Years available: {sorted(pop_df['year'].unique())}")
            print(f"Municipalities: {pop_df['group_id'].nunique()}")
            
            return pop_df
            
        except Exception as e:
            print(f"❌ Error loading population data: {e}")
            print("Continuing without population offset...")
            self.use_population_offset = False
            return None

    def get_population_offset(self, group_id: str, sex: str, age: str, year: int) -> float:
        """
        Get population offset for a specific municipality-sex-age-year combination.
        """
        if not self.use_population_offset or self.population_data is None:
            return 1.0  # No offset
        
        # Find matching population record
        mask = (
            (self.population_data['group_id'] == group_id) &
            (self.population_data['sex'] == sex) &
            (self.population_data['age'] == age) &
            (self.population_data['year'] == year)
        )
        
        matching_records = self.population_data[mask]
        
        if len(matching_records) > 0:
            population = matching_records['population'].iloc[0]
            return max(population, 1.0)  # Minimum 1 to avoid log(0)
        else:
            # No exact match - try to find closest year or use default
            year_mask = (
                (self.population_data['group_id'] == group_id) &
                (self.population_data['sex'] == sex) &
                (self.population_data['age'] == age)
            )
            year_records = self.population_data[year_mask]
            
            if len(year_records) > 0:
                # Use closest available year
                closest_year_idx = (year_records['year'] - year).abs().idxmin()
                population = year_records.loc[closest_year_idx, 'population']
                return max(population, 1.0)
            else:
                return 100.0  # Default population estimate

    def enable_log_transform(self, enable: bool = True):
        """
        Enable or disable log transformation of birth counts.
        
        NOTE: For Negative Binomial models, set enable=False to work with raw counts.
        For Heteroscedastic Normal models, set enable=True to work in log-space.
        """
        self.use_log_transform = enable
        if enable:
            print(f"✓ Log transformation enabled (for Heteroscedastic Normal)")
        else:
            print(f"✓ Log transformation disabled (for Negative Binomial - raw counts)")

    def transform_births(self, births: np.ndarray) -> np.ndarray:
        """
        Apply log transformation to births if enabled.
        For Negative Binomial: returns raw counts (no transformation)
        For Heteroscedastic Normal: returns log(1 + births)
        """
        if self.use_log_transform:
            return np.log1p(births)  # log(1 + births) for Heteroscedastic Normal
        return births.astype(np.float32)  # Raw counts for Negative Binomial

    def inverse_transform_births(self, transformed_births: np.ndarray) -> np.ndarray:
        """
        Inverse transformation to get back birth counts.
        For Negative Binomial: no transformation needed
        For Heteroscedastic Normal: exp(log_births) - 1
        """
        if self.use_log_transform:
            return np.expm1(transformed_births)  # exp(log_births) - 1
        return transformed_births  # Already raw counts

    def create_reporting_triangles(self, period_filter: str = 'all') -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        """
        Create reporting triangles for different periods with log transform and population offset.
        
        Args:
            period_filter: Filter for data period ('training', 'nowcasting', 'all')
            
        Returns:
            Tuple of (triangles, group_ids, sex_codes, age_codes, population_offsets)
        """
        all_triangles = []
        all_group_ids = []
        all_sex_codes = []
        all_age_codes = []
        all_population_offsets = []
        
        print(f"Creating reporting triangles for period: {period_filter}")
        
        for mun_id in self.municipalities:
            mun_data = self.data[self.data['group_id'] == mun_id]
            
            for sex in self.sexes:
                sex_data = mun_data[mun_data['sex'] == sex]
                
                for age_group in self.age_groups:
                    age_data = sex_data[sex_data['age'] == age_group]
                    
                    if len(age_data) == 0:
                        continue
                    
                    # Filter by period if needed
                    if period_filter == 'training':
                        age_data = age_data[age_data['year_occ'] < self.nowcast_cutoff_year]
                    elif period_filter == 'nowcasting':
                        age_data = age_data[age_data['year_occ'] >= self.nowcast_cutoff_year]
                    
                    if len(age_data) == 0:
                        continue
                    
                    # Get population offset for this group
                    # Use middle year of the data for population reference
                    ref_year = int(age_data['year_occ'].median()) if len(age_data) > 0 else 2010
                    population_offset = self.get_population_offset(mun_id, sex, age_group, ref_year)
                    log_population_offset = np.log(population_offset) if self.use_population_offset else 0.0
                    
                    # Create triangle for this group
                    triangle = self._create_triangle_for_group(age_data, period_filter)
                    
                    if triangle is not None:
                        all_triangles.append(triangle)
                        all_group_ids.append(self.le_mun.transform([mun_id])[0])
                        all_sex_codes.append(self.le_sex.transform([sex])[0])
                        all_age_codes.append(self.le_age.transform([age_group])[0])
                        all_population_offsets.append(log_population_offset)
        
        print(f"Created {len(all_triangles)} triangles for {period_filter} period")
        
        return (
            np.array(all_triangles),
            np.array(all_group_ids, dtype=int),
            np.array(all_sex_codes, dtype=int),
            np.array(all_age_codes, dtype=int),
            np.array(all_population_offsets, dtype=np.float32)
        )
    
    def _create_triangle_for_group(self, group_data: pd.DataFrame, period_filter: str = 'all') -> Optional[np.ndarray]:
        """Create reporting triangle for a specific group."""
        if len(group_data) == 0:
            return None
        
        # Calculate month_id and delay if not present
        if 'month_id' not in group_data.columns:
            group_data = group_data.copy()
            group_data['month_id'] = group_data['year_occ'] * 12 + group_data['month_occ']
        
        # Use existing delay if available, otherwise calculate
        if 'delay' not in group_data.columns or group_data['delay'].isna().any():
            group_data = group_data.copy()
            group_data['reg_month_id'] = group_data['year_reg'] * 12 + group_data['month_reg']
            group_data['delay'] = group_data['reg_month_id'] - group_data['month_id']
        
        # Extract months and delays
        group_months = group_data['month_id'].values
        group_delays = group_data['delay'].values
        group_births = group_data['births'].values
        
        # Define triangle dimensions
        M = self.past_units  # Number of months to include
        D = self.max_delay_months  # Maximum delay
        
        # For training period: use all available months
        # For nowcasting period: limit to observable months based on current date
        if period_filter == 'nowcasting' and self.current_date is not None:
            current_month_id = self.current_date.year * 12 + self.current_date.month
            max_observable_month = current_month_id - 1  # Last complete month
        else:
            max_observable_month = group_months.max()
        
        # Select the last M months that are available
        end_month = min(max_observable_month, group_months.max())
        start_month = end_month - M + 1
        
        # Initialize triangle
        triangle = np.zeros((M, D))
        
        # Fill triangle with birth counts (apply log transform if enabled)
        for month, delay, births in zip(group_months, group_delays, group_births):
            if start_month <= month <= end_month and 0 <= delay < D:
                month_idx = int(month - start_month)
                delay_idx = int(delay)
                
                # Apply log transformation if enabled
                births_transformed = self.transform_births(np.array([births]))[0]
                triangle[month_idx, delay_idx] += births_transformed
        
        # For nowcasting period, mask future observations that wouldn't be observable yet
        if period_filter == 'nowcasting' and self.current_date is not None:
            for month_idx in range(M):
                target_month = start_month + month_idx
                max_observable_delay = self._calculate_max_observable_delay(target_month, self.current_date)
                # Mask delays that are not yet observable
                max_delay_int = int(max_observable_delay)
                if max_delay_int < D:
                    triangle[month_idx, max_delay_int+1:] = 0
        
        return triangle
    
    def _calculate_max_observable_delay(self, occurrence_month: int, current_date: datetime) -> int:
        """Calculate the maximum observable delay for a given occurrence month."""
        current_month_id = current_date.year * 12 + current_date.month
        max_delay = current_month_id - occurrence_month - 1  # -1 because we need complete months
        return max(0, min(max_delay, self.max_delay_months))
    
    def get_feature_dimensions(self) -> Dict[str, int]:
        """Get dimensions for embedding layers."""
        return {
            'n_municipalities': self.n_municipalities,
            'n_sexes': self.n_sexes,
            'n_age_groups': self.n_age_groups,
            'triangle_shape': (self.past_units, self.max_delay)
        }


class BirthDataset(Dataset):
    """PyTorch dataset for birth nowcasting data with population offset support."""
    
    def __init__(self, triangles: np.ndarray, group_ids: np.ndarray, 
                 sex_codes: np.ndarray, age_codes: np.ndarray, 
                 population_offsets: Optional[np.ndarray] = None,
                 use_log_transform: bool = False):
        """
        Initialize dataset.
        
        Args:
            triangles: Reporting triangles [N, M, D]
            group_ids: Municipality IDs [N]
            sex_codes: Sex codes [N]
            age_codes: Age group codes [N]
            population_offsets: Log population offsets [N] (optional)
            use_log_transform: Whether to apply log transformation to targets
        """
        self.triangles = torch.FloatTensor(triangles)
        self.group_ids = torch.LongTensor(group_ids)
        self.sex_codes = torch.LongTensor(sex_codes)
        self.age_codes = torch.LongTensor(age_codes)
        self.use_log_transform = use_log_transform
        
        # Population offset (log-transformed)
        if population_offsets is not None:
            self.population_offsets = torch.FloatTensor(population_offsets)
        else:
            self.population_offsets = torch.zeros(len(triangles))
        
    def __len__(self) -> int:
        return len(self.triangles)
    
    def transform_births(self, births: np.ndarray) -> np.ndarray:
        """
        Apply transformation to births if enabled.
        For Negative Binomial: returns raw counts (no transformation)
        For Heteroscedastic Normal: returns log(1 + births)
        """
        if self.use_log_transform:
            return np.log1p(births)  # log(1 + births) for Heteroscedastic Normal
        return births.astype(np.float32)  # Raw counts for Negative Binomial
    
    def __getitem__(self, idx: int) -> Dict[str, torch.Tensor]:
        # Create target births by summing all observed births in the triangle
        target_births = self.triangles[idx].sum()
        
        # Apply log transformation to target births if enabled (for heteroscedastic model)
        if self.use_log_transform:
            target_births = self.transform_births(np.array([target_births]))[0]
        
        return {
            'reporting_triangle': self.triangles[idx],
            'municipality_id': self.group_ids[idx],
            'sex_id': self.sex_codes[idx],
            'age_group_id': self.age_codes[idx],
            'target_births': target_births,
            'population_offset': self.population_offsets[idx]
        }


def create_data_loaders(triangles: np.ndarray, group_ids: np.ndarray,
                       sex_codes: np.ndarray, age_codes: np.ndarray,
                       population_offsets: Optional[np.ndarray] = None,
                       batch_size: int = 32, train_split: float = 0.8,
                       use_log_transform: bool = False) -> Tuple[DataLoader, DataLoader]:
    """Create training and validation data loaders with population offset support."""
    
    # Create dataset
    dataset = BirthDataset(triangles, group_ids, sex_codes, age_codes, 
                          population_offsets, use_log_transform)
    
    # Split into train/val
    n_samples = len(dataset)
    n_train = int(n_samples * train_split)
    
    train_indices = torch.randperm(n_samples)[:n_train]
    val_indices = torch.randperm(n_samples)[n_train:]
    
    train_dataset = torch.utils.data.Subset(dataset, train_indices)
    val_dataset = torch.utils.data.Subset(dataset, val_indices)
    
    # Create data loaders
    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=batch_size, shuffle=False)
    
    return train_loader, val_loader
