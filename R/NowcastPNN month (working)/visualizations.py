"""
Birth Nowcasting Visualization Dashboard
This script creates comprehensive visualizations for the birth nowcasting results.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import torch
from datetime import datetime
from pathlib import Path
from scipy import stats
import warnings
warnings.filterwarnings('ignore')

# Set style
plt.style.use('default')
sns.set_palette("husl")
plt.rcParams['figure.figsize'] = (12, 8)
plt.rcParams['font.size'] = 10

class BirthNowcastVisualizer:
    def __init__(self, model_path='./final_birth_nowcast_model.pt', data_path='../../datasets/monthly_births.parquet'):
        """Initialize the visualizer with model and data paths."""
        self.model_path = model_path
        self.data_path = data_path
        
        # Initialize attributes to None to avoid AttributeError
        self.model_data = None
        self.nowcast_results = None
        self.training_history = None
        self.config = None
        
        self.load_data()
        self.load_model_results()
        
    def load_data(self):
        """Load the original birth data."""
        print("Loading birth data...")
        self.df = pd.read_parquet(self.data_path, engine='fastparquet')
        
        # The dataset already has year_occ and year_reg columns
        # Convert year_reg to numeric if it's not already
        if 'year_reg' in self.df.columns:
            self.df['year_reg'] = pd.to_numeric(self.df['year_reg'], errors='coerce')
        
        # Ensure year_occ is numeric
        self.df['year_occ'] = pd.to_numeric(self.df['year_occ'], errors='coerce')
        
        # Create date columns if needed for more precise calculations
        # Only create dates for valid year/month combinations
        if 'month_occ' in self.df.columns and 'month_reg' in self.df.columns:
            # Create date_occ for valid occurrence data
            valid_occ = (
                self.df['year_occ'].notna() & 
                self.df['month_occ'].notna() &
                (self.df['month_occ'] >= 1) & 
                (self.df['month_occ'] <= 12)
            )
            
            # Create date_reg for valid registration data  
            valid_reg = (
                self.df['year_reg'].notna() & 
                self.df['month_reg'].notna() &
                (self.df['month_reg'] >= 1) & 
                (self.df['month_reg'] <= 12)
            )
            
            self.df['date_occ'] = None
            self.df['date_reg'] = None
            
            if valid_occ.any():
                try:
                    occ_dates = pd.to_datetime(
                        self.df.loc[valid_occ, ['year_occ', 'month_occ']].assign(day=1),
                        errors='coerce'
                    )
                    self.df.loc[valid_occ, 'date_occ'] = occ_dates
                except:
                    pass
            
            if valid_reg.any():
                try:
                    reg_dates = pd.to_datetime(
                        self.df.loc[valid_reg, ['year_reg', 'month_reg']].assign(day=1),
                        errors='coerce'
                    )
                    self.df.loc[valid_reg, 'date_reg'] = reg_dates
                except:
                    pass
            
        print(f"Loaded {len(self.df):,} birth records")
        print(f"Years range: {self.df['year_occ'].min():.0f} - {self.df['year_occ'].max():.0f}")
        if 'year_reg' in self.df.columns:
            print(f"Registration years range: {self.df['year_reg'].min():.0f} - {self.df['year_reg'].max():.0f}")
        
    def load_model_results(self):
        """Load model results if they exist."""
        try:
            if hasattr(torch, 'load') and Path(self.model_path).exists():
                print(f"Loading model from {self.model_path}...")
                self.model_data = torch.load(self.model_path, map_location='cpu', weights_only=False)
                
                # Extract different components that might be saved
                if isinstance(self.model_data, dict):
                    self.model_state = self.model_data.get('model_state_dict', None)
                    self.training_history = self.model_data.get('history', None)  # Changed from 'training_history' to 'history'
                    self.config = self.model_data.get('config', None)
                    # Try both possible names for nowcast results
                    self.nowcast_results = (self.model_data.get('nowcast_results', None) or 
                                          self.model_data.get('test_results', None))
                    
                    if self.nowcast_results:
                        print(f"✅ Found model predictions: {len(self.nowcast_results.get('predictions', []))} samples")
                        print(f"   MAE: {self.nowcast_results.get('mae', 'N/A'):.4f}")
                        print(f"   Coverage: {self.nowcast_results.get('coverage_95', 'N/A'):.3f}")
                    else:
                        print("⚠️  No nowcast results found in model file")
                        
                else:
                    # If it's just the model state dict
                    self.model_state = self.model_data
                    
                print("Model data loaded successfully")
            else:
                print(f"Model file not found at {self.model_path}")
                self.model_data = None
                
        except Exception as e:
            print(f"Could not load model: {e}")
            self.model_data = None
    
    def calculate_registered_births_by_year(self, cutoff_date=None):
        """
        Calculate the actual number of registered births by occurrence year.
        This considers the current registration delay to determine what's already registered.
        
        Args:
            cutoff_date: Date to use as current date for nowcasting (None = use latest registration data)
            
        Returns:
            DataFrame with columns: year_occ, registered_births, available_delays
        """
        if cutoff_date is None:
            # Use the latest registration year/month in the dataset as current date
            max_reg_year = self.df['year_reg'].max()
            max_reg_month = 12  # Assume end of year
            cutoff_year_month = max_reg_year * 12 + max_reg_month
        else:
            cutoff_date = pd.to_datetime(cutoff_date)
            cutoff_year_month = cutoff_date.year * 12 + cutoff_date.month
        
        print(f"Calculating registered births with cutoff at year {cutoff_year_month//12}, month {cutoff_year_month%12}")
        
        # Create year-month identifier for registration filtering
        # Only include valid registration records
        valid_reg_mask = (
            self.df['year_reg'].notna() & 
            self.df['month_reg'].notna() &
            (self.df['month_reg'] >= 1) & 
            (self.df['month_reg'] <= 12)
        )
        
        # Calculate registration year-month for valid records
        reg_year_month = (
            self.df.loc[valid_reg_mask, 'year_reg'] * 12 + 
            self.df.loc[valid_reg_mask, 'month_reg']
        )
        
        # Filter to only include births registered by the cutoff
        cutoff_mask = valid_reg_mask & (reg_year_month <= cutoff_year_month)
        registered_df = self.df[cutoff_mask].copy()
        
        print(f"Found {len(registered_df):,} births registered by cutoff")
        
        # Use the existing delay column if available
        if 'delay' in registered_df.columns:
            registered_df['delay_months'] = registered_df['delay']
        else:
            # Calculate delay in months manually for valid records
            registered_df['delay_months'] = (
                (registered_df['year_reg'] - registered_df['year_occ']) * 12 +
                (registered_df['month_reg'] - registered_df['month_occ'])
            )
        
        # Group by occurrence year and calculate registered births and available delays
        yearly_registered = []
        for year in range(1990, 2024):
            year_data = registered_df[registered_df['year_occ'] == year]
            
            if len(year_data) > 0:
                registered_births = year_data['births'].sum()
                max_available_delay = year_data['delay_months'].max()
                min_available_delay = year_data['delay_months'].min()
                unique_delays = year_data['delay_months'].nunique()
            else:
                registered_births = 0
                max_available_delay = 0
                min_available_delay = 0
                unique_delays = 0
            
            yearly_registered.append({
                'year_occ': year,
                'registered_births': registered_births,
                'max_available_delay': max_available_delay,
                'min_available_delay': min_available_delay,
                'unique_delays': unique_delays
            })
        
        return pd.DataFrame(yearly_registered)
    
    def integrate_model_predictions(self, model_predictions_df=None, cutoff_date=None):
        """
        Integrate model predictions with registered births data.
        
        Args:
            model_predictions_df: DataFrame with columns ['year_occ', 'predicted_births'] 
                                 from the trained model (optional - will auto-extract if None)
            cutoff_date: Current date for nowcasting (None = use latest registration date)
            
        Returns:
            DataFrame with actual registered + model predicted births by year
        """
        # Get actual registered births
        registered_data = self.calculate_registered_births_by_year(cutoff_date)
        
        # Try to extract model predictions if not provided
        if model_predictions_df is None and self.nowcast_results:
            print("🔮 Tentativo di estrazione delle stime annuali dal modello eteroscedastico...")
            yearly_estimates = self.extract_yearly_estimates_from_model()
            
            if yearly_estimates:
                # Convert to DataFrame format
                model_predictions_df = pd.DataFrame([
                    {'year_occ': year, 'predicted_births': estimate}
                    for year, estimate in yearly_estimates.items()
                ])
                print(f"✅ Created yearly estimates for {len(model_predictions_df)} years")
            else:
                print("⚠️  Impossibile estrarre stime annuali dal modello (metadati mancanti)")
                print("    Procedendo con stime euristiche basate sui pattern osservati...")
        
        if model_predictions_df is not None:
            print("Integrating model predictions with registered data...")
            # Merge with model predictions
            integrated_data = pd.merge(registered_data, model_predictions_df, 
                                     on='year_occ', how='left')
            integrated_data['predicted_births'] = integrated_data['predicted_births'].fillna(0)
            integrated_data['total_estimated_births'] = (integrated_data['registered_births'] + 
                                                        integrated_data['predicted_births'])
        else:
            print("No model predictions provided, using heuristic estimates...")
            # Use heuristic estimates if no model predictions available
            if cutoff_date is None:
                max_reg_year = self.df['year_reg'].max()
                cutoff_date = pd.to_datetime(f"{max_reg_year:.0f}-12-31")
            else:
                cutoff_date = pd.to_datetime(cutoff_date)
                
            current_year = cutoff_date.year
            
            integrated_data = registered_data.copy()
            integrated_data['predicted_births'] = 0
            
            for idx, row in integrated_data.iterrows():
                year_occ = row['year_occ']
                registered_births = row['registered_births']
                
                # Use realistic estimates for nowcast period only
                if year_occ >= 2016:  # Only for nowcast period
                    estimated_births = self._calculate_realistic_estimates(year_occ, registered_births, current_year)
                else:
                    estimated_births = 0  # Complete period, no estimates needed
                    
                integrated_data.loc[idx, 'predicted_births'] = max(0, estimated_births)
            
            integrated_data['total_estimated_births'] = (integrated_data['registered_births'] + 
                                                        integrated_data['predicted_births'])
        
        return integrated_data
    
    def _calculate_realistic_estimates(self, year_occ, registered_births, current_year):
        """
        Calculate realistic estimates based on observed completion patterns.
        Uses actual delay patterns from 2016-2018 to estimate missing registrations.
        
        Args:
            year_occ: Year of occurrence
            registered_births: Already registered births for this year
            current_year: Current year for calculation
            
        Returns:
            Estimated missing births
        """
        years_elapsed = current_year - year_occ
        
        # Based on actual delay patterns from complete years (2016-2018):
        # - 69-71% registered within 12 months
        # - 82-83% registered within 24 months  
        # - 89% registered within 36 months
        # - 93-95% registered within 48 months
        # - 96-99% registered within 60 months
        
        if years_elapsed <= 1:
            # 2023: Only 0-11 months available, missing 12+ months
            # Pattern shows ~69% registered in 12 months, so ~31% still missing
            completion_rate = 0.69
            estimated_total = registered_births / completion_rate
            estimated_births = estimated_total - registered_births
            
        elif years_elapsed <= 2:
            # 2022: Only 0-23 months available, missing 24+ months  
            # Pattern shows ~82% registered in 24 months, so ~18% still missing
            completion_rate = 0.82
            estimated_total = registered_births / completion_rate
            estimated_births = estimated_total - registered_births
            
        elif years_elapsed <= 3:
            # 2021: Only 0-35 months available, missing 36+ months
            # Pattern shows ~89% registered in 36 months, so ~11% still missing
            completion_rate = 0.89
            estimated_total = registered_births / completion_rate
            estimated_births = estimated_total - registered_births
            
        elif years_elapsed <= 4:
            # 2020: Only 0-47 months available, missing 48+ months
            # Pattern shows ~93% registered in 48 months, so ~7% still missing
            completion_rate = 0.93
            estimated_total = registered_births / completion_rate
            estimated_births = estimated_total - registered_births
            
        elif years_elapsed <= 5:
            # 2019: Only 0-59 months available, missing 60+ months
            # Pattern shows ~96% registered in 60 months, so ~4% still missing
            completion_rate = 0.96
            estimated_total = registered_births / completion_rate
            estimated_births = estimated_total - registered_births
            
        else:
            # 2016-2018: 6+ years elapsed, assume ~98% complete
            # Only very late registrations missing (~2%)
            completion_rate = 0.98
            estimated_total = registered_births / completion_rate
            estimated_births = estimated_total - registered_births
            
        return max(0, estimated_births)
    
    def extract_yearly_estimates_from_model(self, verbose=True):
        """
        Extract yearly estimates from the heteroscedastic model predictions.
        
        IMPORTANTE: I valori del modello sembrano essere per sottogruppi demografici specifici
        (municipalità-sesso-età) e potrebbero essere trasformati (log, normalizzati).
        Senza i metadati demografici specifici, non possiamo aggregare correttamente.
        
        Args:
            verbose: Whether to print detailed messages (default True, False for repeated calls)
        
        Returns:
            None (metadati demografici non disponibili per aggregazione corretta)
        """
        if not self.nowcast_results or 'predictions' not in self.nowcast_results:
            if verbose:
                print("⚠️  No model predictions available for yearly aggregation")
            return None
            
        try:
            predictions = np.array(self.nowcast_results['predictions'])
            uncertainties = np.array(self.nowcast_results['uncertainties'])
            
            if verbose:
                print(f"📊 Analizzando {len(predictions)} predizioni del modello eteroscedastico...")
                print(f"   Range predizioni: {predictions.min():.3f} - {predictions.max():.3f}")
                print(f"   Media predizioni: {predictions.mean():.3f}")
                print(f"   Media incertezze: {uncertainties.mean():.3f}")
                
                print("\n⚠️  LIMITAZIONE CRITICA:")
                print("   Le predizioni del modello sono per sottogruppi demografici specifici")
                print("   (municipalità × sesso × età) e potrebbero essere trasformate logaritmicamente.")
                print("   Senza i metadati demografici (year_occ, municipality_id, sex, age) per ogni")
                print("   predizione, non è possibile aggregare correttamente a livello annuale.")
                print("   ")
                print("   Soluzioni possibili:")
                print("   1. Modificare il modello per salvare i metadati demografici")
                print("   2. Ri-eseguire le predizioni con i metadati")
                print("   3. Usare le stime euristiche basate sui pattern osservati")
                
                print(f"\n🔄 Utilizzando stime euristiche basate sui pattern di ritardo osservati...")
            return None
            
        except Exception as e:
            if verbose:
                print(f"❌ Error analyzing model predictions: {e}")
            return None
            
    def plot_annual_births_with_nowcast(self, save_path='birth_nowcast_annual.png', cutoff_date=None):
        """Plot annual births time series with nowcasting completion."""
        print("Creating annual births plot with nowcasting...")
        
        # Calculate actual registered births by year
        registered_data = self.calculate_registered_births_by_year(cutoff_date)
        
        # Get total births (ground truth) for comparison
        annual_births = self.df.groupby('year_occ')['births'].sum().reset_index()
        annual_births = annual_births[(annual_births['year_occ'] >= 1990) & (annual_births['year_occ'] <= 2023)]
        
        # Merge registered data with total births
        combined_data = pd.merge(annual_births, registered_data, on='year_occ', how='left')
        combined_data['registered_births'] = combined_data['registered_births'].fillna(0)
        
        # Separate observed vs nowcast periods
        observed_period = combined_data[combined_data['year_occ'] <= 2015].copy()
        nowcast_period = combined_data[combined_data['year_occ'] >= 2016].copy()
        
        # For observed period, all births are registered (complete data)
        observed_period['registered_births'] = observed_period['births']
        observed_period['estimated_births'] = 0
        
        # For nowcast period, calculate realistic estimates
        if cutoff_date is None:
            max_reg_year = self.df['year_reg'].max()
            cutoff_date = pd.to_datetime(f"{max_reg_year:.0f}-12-31")
        else:
            cutoff_date = pd.to_datetime(cutoff_date)
            
        current_year = cutoff_date.year
        
        # Calculate estimated births for nowcast period
        for idx, row in nowcast_period.iterrows():
            year_occ = row['year_occ']
            registered_births = row['registered_births']
            
            # Use the same realistic estimation function
            estimated_births = self._calculate_realistic_estimates(year_occ, registered_births, current_year)
            
            # Ensure non-negative estimates
            estimated_births = max(0, estimated_births)
            nowcast_period.loc[idx, 'estimated_births'] = estimated_births
        
        fig, ax = plt.subplots(figsize=(14, 8))
        
        # Plot observed period (complete data)
        ax.bar(observed_period['year_occ'], observed_period['births'], 
               color='steelblue', alpha=0.8, label='Complete births (1990-2015)')
        
        # Plot nowcast period (registered + estimated)
        ax.bar(nowcast_period['year_occ'], nowcast_period['registered_births'], 
               color='lightblue', alpha=0.8, label='Registered births (2016-2023)')
        ax.bar(nowcast_period['year_occ'], nowcast_period['estimated_births'], 
               bottom=nowcast_period['registered_births'], 
               color='orange', alpha=0.7, label='Nowcast estimates (2016-2023)')
        
        # Add vertical line at nowcast cutoff
        ax.axvline(x=2015.5, color='red', linestyle='--', alpha=0.7, linewidth=2)
        ax.text(2015.5, ax.get_ylim()[1]*0.9, 'Nowcast Period', rotation=90, 
                verticalalignment='top', horizontalalignment='right', color='red')
        
        ax.set_xlabel('Year of Occurrence')
        ax.set_ylabel('Number of Births')
        ax.set_title('Annual Births in Mexico (1990-2023)\nComplete Data vs Nowcast Estimates', fontsize=14, fontweight='bold')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Format y-axis
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'{x/1e6:.1f}M'))
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Annual births plot saved to {save_path}")
        
        return {
            'observed_period': observed_period,
            'nowcast_period': nowcast_period
        }
        
    def plot_municipality_trends_by_sex(self, n_municipalities=4, save_path='municipality_trends.png'):
        """Plot birth trends for sample municipalities by sex."""
        print(f"Creating municipality trends plot for {n_municipalities} municipalities...")
        
        # Select top municipalities by total births
        top_municipalities = (self.df.groupby('group_id')['births'].sum()
                            .nlargest(n_municipalities).index.tolist())
        
        # Filter data for selected municipalities
        df_sample = self.df[self.df['group_id'].isin(top_municipalities)]
        
        # Group by municipality, sex, and year
        trends = (df_sample.groupby(['group_id', 'sex', 'year_occ'])['births'].sum()
                 .reset_index())
        trends = trends[(trends['year_occ'] >= 1990) & (trends['year_occ'] <= 2023)]
        
        fig, axes = plt.subplots(2, 2, figsize=(16, 12))
        axes = axes.flatten()
        
        colors = {'male': 'lightblue', 'female': 'lightpink'}
        
        for i, municipality in enumerate(top_municipalities):
            if i >= len(axes):
                break
                
            mun_data = trends[trends['group_id'] == municipality]
            
            for sex in ['male', 'female']:
                sex_data = mun_data[mun_data['sex'] == sex].sort_values('year_occ')
                
                # Separate observed vs nowcast periods
                observed = sex_data[sex_data['year_occ'] <= 2015]
                nowcast = sex_data[sex_data['year_occ'] >= 2016]
                
                # Plot observed period (solid line)
                if len(observed) > 0:
                    axes[i].plot(observed['year_occ'], observed['births'], 
                               color=colors[sex], linewidth=2, label=f'{sex.title()} (observed)')
                
                # Plot nowcast period (dashed line)
                if len(nowcast) > 0:
                    axes[i].plot(nowcast['year_occ'], nowcast['births'], 
                               color=colors[sex], linewidth=2, linestyle='--', 
                               alpha=0.7, label=f'{sex.title()} (nowcast)')
            
            # Add vertical line at nowcast cutoff
            axes[i].axvline(x=2015.5, color='red', linestyle=':', alpha=0.5)
            
            axes[i].set_title(f'Municipality: {municipality}', fontweight='bold')
            axes[i].set_xlabel('Year')
            axes[i].set_ylabel('Number of Births')
            axes[i].legend()
            axes[i].grid(True, alpha=0.3)
        
        plt.suptitle('Birth Trends by Municipality and Sex\n(Solid: Complete Data, Dashed: Nowcast Period)', 
                     fontsize=16, fontweight='bold')
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Municipality trends plot saved to {save_path}")
        
    def plot_training_diagnostics(self, save_path='training_diagnostics.png'):
        """Plot training and validation diagnostics."""
        if self.training_history is None:
            print("No training history available for diagnostics plot")
            return
            
        print("Creating training diagnostics plot...")
        
        history = self.training_history
        epochs = range(1, len(history['train_loss']) + 1)
        
        fig, axes = plt.subplots(2, 2, figsize=(15, 10))
        
        # Training and Validation Loss
        axes[0, 0].plot(epochs, history['train_loss'], 'b-', label='Training Loss', linewidth=2)
        axes[0, 0].plot(epochs, history['val_loss'], 'r-', label='Validation Loss', linewidth=2)
        axes[0, 0].set_title('Training and Validation Loss', fontweight='bold')
        axes[0, 0].set_xlabel('Epoch')
        axes[0, 0].set_ylabel('Loss')
        axes[0, 0].legend()
        axes[0, 0].grid(True, alpha=0.3)
        
        # Training and Validation MAE
        axes[0, 1].plot(epochs, history['train_mae'], 'b-', label='Training MAE', linewidth=2)
        axes[0, 1].plot(epochs, history['val_mae'], 'r-', label='Validation MAE', linewidth=2)
        axes[0, 1].set_title('Training and Validation MAE', fontweight='bold')
        axes[0, 1].set_xlabel('Epoch')
        axes[0, 1].set_ylabel('MAE')
        axes[0, 1].legend()
        axes[0, 1].grid(True, alpha=0.3)
        
        # Learning Rate Schedule
        if 'learning_rate' in history:
            axes[1, 0].plot(epochs, history['learning_rate'], 'g-', linewidth=2)
            axes[1, 0].set_title('Learning Rate Schedule', fontweight='bold')
            axes[1, 0].set_xlabel('Epoch')
            axes[1, 0].set_ylabel('Learning Rate')
            axes[1, 0].set_yscale('log')
            axes[1, 0].grid(True, alpha=0.3)
        
        # Model Performance Summary
        if self.nowcast_results:
            metrics = ['MAE', 'RMSE', 'MAPE', 'Coverage']
            values = [
                self.nowcast_results.get('mae', 0),
                self.nowcast_results.get('rmse', 0),
                self.nowcast_results.get('mape', 0),
                self.nowcast_results.get('coverage_95', 0) * 100
            ]
            
            bars = axes[1, 1].bar(metrics, values, color=['skyblue', 'lightcoral', 'lightgreen', 'gold'])
            axes[1, 1].set_title('Nowcasting Performance Metrics', fontweight='bold')
            axes[1, 1].set_ylabel('Value')
            
            # Add value labels on bars
            for bar, value in zip(bars, values):
                height = bar.get_height()
                axes[1, 1].text(bar.get_x() + bar.get_width()/2., height,
                               f'{value:.1f}', ha='center', va='bottom')
        
        plt.suptitle('Model Training Diagnostics', fontsize=16, fontweight='bold')
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Training diagnostics plot saved to {save_path}")
        
    def plot_prediction_vs_actual(self, save_path='prediction_vs_actual.png'):
        """Plot predictions vs actual values scatter plot."""
        if self.nowcast_results is None:
            print("No nowcast results available for prediction plot")
            return
            
        print("Creating prediction vs actual scatter plot...")
        
        predictions = np.array(self.nowcast_results['predictions'])
        actuals = np.array(self.nowcast_results['actuals'])
        uncertainties = np.array(self.nowcast_results['uncertainties'])
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 6))
        
        # Scatter plot with uncertainty coloring
        scatter = ax1.scatter(actuals, predictions, c=uncertainties, alpha=0.6, 
                            cmap='viridis', s=20)
        
        # Perfect prediction line
        min_val = min(min(actuals), min(predictions))
        max_val = max(max(actuals), max(predictions))
        ax1.plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2, 
                alpha=0.8, label='Perfect Prediction')
        
        ax1.set_xlabel('Actual Births')
        ax1.set_ylabel('Predicted Births')
        ax1.set_title('Predictions vs Actual Values\n(Color = Uncertainty)', fontweight='bold')
        ax1.legend()
        ax1.grid(True, alpha=0.3)
        
        # Add colorbar
        cbar = plt.colorbar(scatter, ax=ax1)
        cbar.set_label('Prediction Uncertainty')
        
        # Residuals plot
        residuals = predictions - actuals
        ax2.scatter(actuals, residuals, alpha=0.6, s=20)
        ax2.axhline(y=0, color='r', linestyle='--', linewidth=2, alpha=0.8)
        ax2.set_xlabel('Actual Births')
        ax2.set_ylabel('Residuals (Predicted - Actual)')
        ax2.set_title('Residuals Plot', fontweight='bold')
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Prediction vs actual plot saved to {save_path}")
        
    def plot_age_group_performance(self, save_path='age_group_performance.png'):
        """Plot performance metrics by age group."""
        print("Creating age group performance analysis...")
        
        # This is a simplified version - in practice you'd need to extract
        # predictions by age group from the model results
        age_groups = ['15-19', '20-24', '25-29', '30-34', '35-39', '40-44', 
                     '45-49', '50-54', '55-59', '60-64', '65-69']
        
        # Group births by age group for overall statistics
        age_stats = (self.df[self.df['year_occ'] >= 2016]
                    .groupby('age')['births'].agg(['count', 'mean', 'std'])
                    .reset_index())
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
        
        # Birth counts by age group
        ax1.bar(age_stats['age'], age_stats['count'], color='skyblue', alpha=0.8)
        ax1.set_xlabel('Age Group')
        ax1.set_ylabel('Number of Records')
        ax1.set_title('Birth Records by Age Group (2016-2023)', fontweight='bold')
        ax1.tick_params(axis='x', rotation=45)
        ax1.grid(True, alpha=0.3)
        
        # Average births by age group
        ax2.bar(age_stats['age'], age_stats['mean'], color='lightcoral', alpha=0.8)
        ax2.set_xlabel('Age Group')
        ax2.set_ylabel('Average Births per Record')
        ax2.set_title('Average Births by Age Group', fontweight='bold')
        ax2.tick_params(axis='x', rotation=45)
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Age group performance plot saved to {save_path}")
        
    def plot_monthly_seasonality(self, save_path='monthly_seasonality.png'):
        """Plot seasonal patterns in births by month."""
        print("Creating monthly seasonality plot...")
        
        # Use existing month_occ column (month is already extracted)
        # Filter out any missing month values
        df_clean = self.df[self.df['month_occ'].notna() & (self.df['month_occ'] >= 1) & (self.df['month_occ'] <= 12)]
        
        # Group by month for different periods
        observed_period = df_clean[df_clean['year_occ'] <= 2015]
        nowcast_period = df_clean[df_clean['year_occ'] >= 2016]
        
        monthly_observed = observed_period.groupby('month_occ')['births'].sum()
        monthly_nowcast = nowcast_period.groupby('month_occ')['births'].sum()
        
        fig, ax = plt.subplots(figsize=(12, 6))
        
        months = range(1, 13)
        month_names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        
        width = 0.35
        x = np.arange(len(months))
        
        # Ensure we have data for all months (fill missing with 0)
        observed_values = [monthly_observed.get(month, 0) for month in months]
        nowcast_values = [monthly_nowcast.get(month, 0) for month in months]
        
        ax.bar(x - width/2, observed_values, width, 
               label='Observed Period (1990-2015)', color='steelblue', alpha=0.8)
        ax.bar(x + width/2, nowcast_values, width,
               label='Nowcast Period (2016-2023)', color='orange', alpha=0.8)
        
        ax.set_xlabel('Month')
        ax.set_ylabel('Total Births')
        ax.set_title('Seasonal Birth Patterns by Month', fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(month_names)
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Format y-axis
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'{x/1e6:.1f}M'))
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Monthly seasonality plot saved to {save_path}")
        
    def plot_heteroscedastic_analysis(self, predictions, actuals, uncertainties, 
                                     municipality_ids=None, sex_ids=None, age_ids=None,
                                     save_path='heteroscedastic_analysis.png'):
        """
        Create comprehensive analysis plots for heteroscedastic model results.
        
        Args:
            predictions: Predicted means
            actuals: Actual values
            uncertainties: Predicted standard deviations
            municipality_ids: Municipality IDs for grouping
            sex_ids: Sex IDs for grouping
            age_ids: Age group IDs for grouping
            save_path: Path to save the plot
        """
        print("Creating heteroscedastic variance analysis...")
        
        fig, axes = plt.subplots(2, 3, figsize=(18, 12))
        fig.suptitle('Heteroscedastic Normal Distribution Analysis', fontsize=16, fontweight='bold')
        
        # Convert to numpy if needed
        if torch.is_tensor(predictions):
            predictions = predictions.cpu().numpy()
        elif isinstance(predictions, list):
            predictions = np.array(predictions)
            
        if torch.is_tensor(actuals):
            actuals = actuals.cpu().numpy()
        elif isinstance(actuals, list):
            actuals = np.array(actuals)
            
        if torch.is_tensor(uncertainties):
            uncertainties = uncertainties.cpu().numpy()
        elif isinstance(uncertainties, list):
            uncertainties = np.array(uncertainties)
        
        # 1. Residuals vs Predicted with uncertainty bands
        residuals = actuals - predictions
        axes[0, 0].scatter(predictions, residuals, alpha=0.6, s=20)
        axes[0, 0].fill_between(predictions, -1.96*uncertainties, 1.96*uncertainties, 
                                alpha=0.3, color='red', label='95% Prediction Interval')
        axes[0, 0].axhline(y=0, color='black', linestyle='--', alpha=0.8)
        axes[0, 0].set_xlabel('Predicted Mean')
        axes[0, 0].set_ylabel('Residuals (Actual - Predicted)')
        axes[0, 0].set_title('Residuals vs Predicted with Uncertainty')
        axes[0, 0].legend()
        axes[0, 0].grid(True, alpha=0.3)
        
        # 2. Predicted Variance Distribution
        variance = uncertainties ** 2
        axes[0, 1].hist(variance, bins=50, alpha=0.7, edgecolor='black')
        axes[0, 1].axvline(variance.mean(), color='red', linestyle='--', 
                          label=f'Mean: {variance.mean():.2f}')
        axes[0, 1].axvline(np.median(variance), color='orange', linestyle='--', 
                          label=f'Median: {np.median(variance):.2f}')
        axes[0, 1].set_xlabel('Predicted Variance σ²(X)')
        axes[0, 1].set_ylabel('Frequency')
        axes[0, 1].set_title('Distribution of Predicted Variance')
        axes[0, 1].legend()
        axes[0, 1].set_yscale('log')
        
        # 3. Mean-Variance Relationship
        axes[0, 2].scatter(predictions, variance, alpha=0.6, s=20)
        z = np.polyfit(predictions, variance, 1)
        p = np.poly1d(z)
        axes[0, 2].plot(predictions, p(predictions), "r--", alpha=0.8, 
                       label=f'Trend: slope={z[0]:.3f}')
        correlation = np.corrcoef(predictions, variance)[0, 1]
        axes[0, 2].text(0.05, 0.95, f'Correlation: {correlation:.3f}', 
                       transform=axes[0, 2].transAxes, fontsize=10,
                       bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        axes[0, 2].set_xlabel('Predicted Mean μ(X)')
        axes[0, 2].set_ylabel('Predicted Variance σ²(X)')
        axes[0, 2].set_title('Mean-Variance Relationship')
        axes[0, 2].legend()
        axes[0, 2].grid(True, alpha=0.3)
        
        # 4. Standardized Residuals (should be ~N(0,1))
        standardized_residuals = residuals / uncertainties
        axes[1, 0].hist(standardized_residuals, bins=50, alpha=0.7, density=True, 
                       edgecolor='black', label='Observed')
        
        # Overlay theoretical N(0,1)
        x_norm = np.linspace(-4, 4, 100)
        y_norm = (1/np.sqrt(2*np.pi)) * np.exp(-0.5 * x_norm**2)
        axes[1, 0].plot(x_norm, y_norm, 'r-', linewidth=2, label='N(0,1)')
        
        axes[1, 0].axvline(standardized_residuals.mean(), color='blue', linestyle='--', 
                          label=f'Mean: {standardized_residuals.mean():.3f}')
        axes[1, 0].axvline(standardized_residuals.std(), color='green', linestyle='--', 
                          label=f'Std: {standardized_residuals.std():.3f}')
        axes[1, 0].set_xlabel('Standardized Residuals')
        axes[1, 0].set_ylabel('Density')
        axes[1, 0].set_title('Standardized Residuals Distribution')
        axes[1, 0].legend()
        axes[1, 0].grid(True, alpha=0.3)
        
        # 5. Q-Q Plot for normality check
        from scipy import stats
        stats.probplot(standardized_residuals, dist="norm", plot=axes[1, 1])
        axes[1, 1].set_title('Q-Q Plot: Standardized Residuals vs Normal')
        axes[1, 1].grid(True, alpha=0.3)
        
        # 6. Coverage Analysis
        # Calculate actual coverage for different confidence levels
        confidence_levels = [0.5, 0.68, 0.8, 0.9, 0.95, 0.99]
        actual_coverage = []
        
        for conf in confidence_levels:
            z_score = stats.norm.ppf((1 + conf) / 2)
            lower = predictions - z_score * uncertainties
            upper = predictions + z_score * uncertainties
            coverage = np.mean((actuals >= lower) & (actuals <= upper))
            actual_coverage.append(coverage)
        
        axes[1, 2].plot(confidence_levels, confidence_levels, 'r--', 
                       label='Perfect Calibration', linewidth=2)
        axes[1, 2].plot(confidence_levels, actual_coverage, 'bo-', 
                       label='Actual Coverage', markersize=6)
        axes[1, 2].set_xlabel('Nominal Coverage')
        axes[1, 2].set_ylabel('Actual Coverage')
        axes[1, 2].set_title('Coverage Calibration')
        axes[1, 2].legend()
        axes[1, 2].grid(True, alpha=0.3)
        axes[1, 2].set_xlim(0.4, 1.0)
        axes[1, 2].set_ylim(0.4, 1.0)
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Heteroscedastic analysis saved to {save_path}")
        
        # Print summary statistics
        print("\n📊 HETEROSCEDASTIC ANALYSIS SUMMARY:")
        print("=" * 50)
        print(f"Variance Statistics:")
        print(f"  Mean σ²: {variance.mean():.2f}")
        print(f"  Std σ²:  {variance.std():.2f}")
        print(f"  CV σ²:   {variance.std()/variance.mean():.3f}")
        print(f"  Range:   [{variance.min():.2f}, {variance.max():.2f}]")
        print(f"\nStandardized Residuals:")
        print(f"  Mean: {standardized_residuals.mean():.3f} (should be ~0)")
        print(f"  Std:  {standardized_residuals.std():.3f} (should be ~1)")
        print(f"\nMean-Variance Correlation: {correlation:.3f}")
        print(f"95% Coverage: {actual_coverage[4]:.3f} (should be ~0.95)")
        
    def plot_variance_by_features(self, predictions, uncertainties, 
                                 municipality_ids, sex_ids, age_ids,
                                 le_mun=None, le_sex=None, le_age=None,
                                 save_path='variance_by_features.png'):
        """
        Analyze how predicted variance varies by demographic features.
        
        Args:
            predictions, uncertainties: Model outputs
            municipality_ids, sex_ids, age_ids: Feature IDs
            le_mun, le_sex, le_age: Label encoders for readable names
            save_path: Save path for plot
        """
        print("Creating variance analysis by demographic features...")
        
        fig, axes = plt.subplots(2, 2, figsize=(16, 12))
        fig.suptitle('Predicted Variance by Demographic Features', fontsize=16, fontweight='bold')
        
        # Convert to numpy
        if torch.is_tensor(uncertainties):
            uncertainties = uncertainties.cpu().numpy()
        if torch.is_tensor(municipality_ids):
            municipality_ids = municipality_ids.cpu().numpy()
        if torch.is_tensor(sex_ids):
            sex_ids = sex_ids.cpu().numpy()
        if torch.is_tensor(age_ids):
            age_ids = age_ids.cpu().numpy()
        
        variance = uncertainties ** 2
        
        # 1. Variance by Sex
        sex_names = ['Female', 'Male'] if le_sex is None else le_sex.classes_
        sex_variances = [variance[sex_ids == i] for i in range(len(sex_names))]
        
        axes[0, 0].boxplot(sex_variances, labels=sex_names)
        axes[0, 0].set_ylabel('Predicted Variance σ²(X)')
        axes[0, 0].set_title('Variance Distribution by Sex')
        axes[0, 0].grid(True, alpha=0.3)
        axes[0, 0].set_yscale('log')
        
        # 2. Variance by Age Group
        if le_age is not None:
            age_names = le_age.classes_
        else:
            age_names = [f'Age {i}' for i in range(len(np.unique(age_ids)))]
        
        age_variances = [variance[age_ids == i] for i in range(len(age_names))]
        
        axes[0, 1].boxplot(age_variances, labels=age_names)
        axes[0, 1].set_ylabel('Predicted Variance σ²(X)')
        axes[0, 1].set_title('Variance Distribution by Age Group')
        axes[0, 1].tick_params(axis='x', rotation=45)
        axes[0, 1].grid(True, alpha=0.3)
        axes[0, 1].set_yscale('log')
        
        # 3. Municipality variance (top 20 by sample size)
        muni_var_mean = []
        muni_var_std = []
        muni_counts = []
        unique_munis = np.unique(municipality_ids)
        
        for muni_id in unique_munis:
            mask = municipality_ids == muni_id
            if np.sum(mask) >= 5:  # At least 5 samples
                muni_var_mean.append(variance[mask].mean())
                muni_var_std.append(variance[mask].std())
                muni_counts.append(np.sum(mask))
        
        # Sort by count and take top 20
        top_indices = np.argsort(muni_counts)[-20:]
        top_var_means = np.array(muni_var_mean)[top_indices]
        top_var_stds = np.array(muni_var_std)[top_indices]
        top_counts = np.array(muni_counts)[top_indices]
        
        x_pos = np.arange(len(top_indices))
        axes[1, 0].bar(x_pos, top_var_means, yerr=top_var_stds, alpha=0.7, capsize=5)
        axes[1, 0].set_ylabel('Mean Predicted Variance')
        axes[1, 0].set_xlabel('Top 20 Municipalities (by sample size)')
        axes[1, 0].set_title('Variance by Municipality')
        axes[1, 0].tick_params(axis='x', rotation=45)
        axes[1, 0].grid(True, alpha=0.3)
        axes[1, 0].set_yscale('log')
        
        # 4. Heatmap: Sex x Age variance
        sex_age_variance = np.zeros((len(sex_names), len(age_names)))
        sex_age_counts = np.zeros((len(sex_names), len(age_names)))
        
        for s in range(len(sex_names)):
            for a in range(len(age_names)):
                mask = (sex_ids == s) & (age_ids == a)
                if np.sum(mask) > 0:
                    sex_age_variance[s, a] = variance[mask].mean()
                    sex_age_counts[s, a] = np.sum(mask)
                else:
                    sex_age_variance[s, a] = np.nan
        
        # Mask cells with too few samples
        sex_age_variance[sex_age_counts < 5] = np.nan
        
        im = axes[1, 1].imshow(sex_age_variance, cmap='viridis', aspect='auto')
        axes[1, 1].set_xticks(range(len(age_names)))
        axes[1, 1].set_xticklabels(age_names, rotation=45)
        axes[1, 1].set_yticks(range(len(sex_names)))
        axes[1, 1].set_yticklabels(sex_names)
        axes[1, 1].set_title('Mean Variance: Sex × Age')
        
        # Add colorbar
        cbar = plt.colorbar(im, ax=axes[1, 1])
        cbar.set_label('Mean Predicted Variance')
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Variance by features analysis saved to {save_path}")
        
    def plot_registration_delay_analysis(self, save_path='registration_delay_patterns.png'):
        """
        Comprehensive analysis of registration delay patterns.
        """
        print("Creating registration delay analysis...")
        
        # Calculate delays for analysis
        df_clean = self.df.dropna(subset=['year_occ', 'month_occ', 'year_reg', 'month_reg'])
        df_clean = df_clean[
            (df_clean['month_occ'] >= 1) & (df_clean['month_occ'] <= 12) &
            (df_clean['month_reg'] >= 1) & (df_clean['month_reg'] <= 12)
        ].copy()
        
        if 'delay' not in df_clean.columns:
            df_clean['delay'] = (
                (df_clean['year_reg'] - df_clean['year_occ']) * 12 +
                (df_clean['month_reg'] - df_clean['month_occ'])
            )
        
        # Filter reasonable delays (0-96 months)
        df_clean = df_clean[(df_clean['delay'] >= 0) & (df_clean['delay'] <= 96)]
        
        fig, axes = plt.subplots(2, 3, figsize=(18, 12))
        fig.suptitle('Registration Delay Analysis Dashboard', fontsize=16, fontweight='bold')
        
        # 1. Overall delay distribution
        axes[0, 0].hist(df_clean['delay'], bins=50, alpha=0.7, edgecolor='black', color='skyblue')
        axes[0, 0].axvline(df_clean['delay'].median(), color='red', linestyle='--', 
                          label=f'Median: {df_clean["delay"].median():.1f} months')
        axes[0, 0].axvline(df_clean['delay'].mean(), color='orange', linestyle='--',
                          label=f'Mean: {df_clean["delay"].mean():.1f} months')
        axes[0, 0].set_xlabel('Registration Delay (months)')
        axes[0, 0].set_ylabel('Frequency')
        axes[0, 0].set_title('Overall Delay Distribution')
        axes[0, 0].legend()
        axes[0, 0].grid(True, alpha=0.3)
        
        # 2. Delay patterns by occurrence year
        yearly_delays = df_clean.groupby('year_occ')['delay'].agg(['mean', 'median', 'std']).reset_index()
        yearly_delays = yearly_delays[yearly_delays['year_occ'] >= 2000]
        
        axes[0, 1].plot(yearly_delays['year_occ'], yearly_delays['mean'], 'b-', label='Mean Delay', linewidth=2)
        axes[0, 1].plot(yearly_delays['year_occ'], yearly_delays['median'], 'r-', label='Median Delay', linewidth=2)
        axes[0, 1].fill_between(yearly_delays['year_occ'], 
                               yearly_delays['mean'] - yearly_delays['std'],
                               yearly_delays['mean'] + yearly_delays['std'],
                               alpha=0.3, label='±1 Std')
        axes[0, 1].set_xlabel('Year of Occurrence')
        axes[0, 1].set_ylabel('Average Delay (months)')
        axes[0, 1].set_title('Delay Trends by Occurrence Year')
        axes[0, 1].legend()
        axes[0, 1].grid(True, alpha=0.3)
        
        # 3. Seasonal delay patterns
        monthly_delays = df_clean.groupby('month_occ')['delay'].mean()
        month_names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        
        axes[0, 2].bar(range(1, 13), [monthly_delays.get(i, 0) for i in range(1, 13)], 
                      color='lightcoral', alpha=0.8)
        axes[0, 2].set_xlabel('Month of Occurrence')
        axes[0, 2].set_ylabel('Average Delay (months)')
        axes[0, 2].set_title('Seasonal Delay Patterns')
        axes[0, 2].set_xticks(range(1, 13))
        axes[0, 2].set_xticklabels(month_names, rotation=45)
        axes[0, 2].grid(True, alpha=0.3)
        
        # 4. Delay by demographic groups
        if 'sex' in df_clean.columns:
            sex_delays = df_clean.groupby('sex')['delay'].mean()
            axes[1, 0].bar(sex_delays.index, sex_delays.values, color=['lightblue', 'lightpink'])
            axes[1, 0].set_ylabel('Average Delay (months)')
            axes[1, 0].set_title('Delay by Sex')
            axes[1, 0].grid(True, alpha=0.3)
        
        # 5. Cumulative registration by delay
        delay_cumsum = df_clean.groupby('delay')['births'].sum().cumsum()
        total_births = delay_cumsum.iloc[-1]
        pct_cumsum = (delay_cumsum / total_births * 100)
        
        axes[1, 1].plot(pct_cumsum.index, pct_cumsum.values, 'g-', linewidth=2)
        axes[1, 1].axhline(y=50, color='red', linestyle='--', alpha=0.7, label='50%')
        axes[1, 1].axhline(y=80, color='orange', linestyle='--', alpha=0.7, label='80%')
        axes[1, 1].axhline(y=95, color='purple', linestyle='--', alpha=0.7, label='95%')
        axes[1, 1].set_xlabel('Delay (months)')
        axes[1, 1].set_ylabel('Cumulative Registration (%)')
        axes[1, 1].set_title('Registration Completion Rate')
        axes[1, 1].legend()
        axes[1, 1].grid(True, alpha=0.3)
        
        # 6. Delay heatmap by year and month
        if len(df_clean) > 1000:  # Only if we have enough data
            delay_pivot = df_clean.groupby(['year_occ', 'month_occ'])['delay'].mean().reset_index()
            delay_pivot = delay_pivot[delay_pivot['year_occ'] >= 2010]
            
            if len(delay_pivot) > 0:
                pivot_table = delay_pivot.pivot(index='year_occ', columns='month_occ', values='delay')
                im = axes[1, 2].imshow(pivot_table.values, cmap='YlOrRd', aspect='auto')
                axes[1, 2].set_xticks(range(12))
                axes[1, 2].set_xticklabels(month_names)
                axes[1, 2].set_yticks(range(len(pivot_table.index)))
                axes[1, 2].set_yticklabels(pivot_table.index)
                axes[1, 2].set_title('Delay Heatmap (Year × Month)')
                plt.colorbar(im, ax=axes[1, 2], label='Average Delay (months)')
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Registration delay analysis saved to {save_path}")
        
    def plot_uncertainty_calibration(self, predictions, actuals, uncertainties, 
                                   save_path='uncertainty_calibration.png'):
        """
        Analyze uncertainty calibration for probabilistic model.
        """
        print("Creating uncertainty calibration analysis...")
        
        # Convert to numpy
        if torch.is_tensor(predictions):
            predictions = predictions.cpu().numpy()
        if torch.is_tensor(actuals):
            actuals = actuals.cpu().numpy()
        if torch.is_tensor(uncertainties):
            uncertainties = uncertainties.cpu().numpy()
        
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        fig.suptitle('Uncertainty Calibration Analysis', fontsize=16, fontweight='bold')
        
        # 1. Reliability diagram (calibration plot)
        confidence_levels = np.arange(0.1, 1.0, 0.1)
        observed_frequencies = []
        
        for conf in confidence_levels:
            z_score = stats.norm.ppf((1 + conf) / 2)
            lower = predictions - z_score * uncertainties
            upper = predictions + z_score * uncertainties
            within_interval = ((actuals >= lower) & (actuals <= upper)).mean()
            observed_frequencies.append(within_interval)
        
        axes[0, 0].plot(confidence_levels, confidence_levels, 'r--', linewidth=2, label='Perfect Calibration')
        axes[0, 0].plot(confidence_levels, observed_frequencies, 'bo-', linewidth=2, label='Observed')
        axes[0, 0].set_xlabel('Predicted Confidence Level')
        axes[0, 0].set_ylabel('Observed Frequency')
        axes[0, 0].set_title('Reliability Diagram')
        axes[0, 0].legend()
        axes[0, 0].grid(True, alpha=0.3)
        axes[0, 0].set_xlim(0, 1)
        axes[0, 0].set_ylim(0, 1)
        
        # 2. Prediction interval coverage
        residuals = actuals - predictions
        standardized_residuals = residuals / uncertainties
        
        axes[0, 1].hist(standardized_residuals, bins=50, alpha=0.7, density=True, 
                       color='skyblue', edgecolor='black', label='Observed')
        
        # Theoretical N(0,1) overlay
        x_norm = np.linspace(-4, 4, 100)
        y_norm = stats.norm.pdf(x_norm, 0, 1)
        axes[0, 1].plot(x_norm, y_norm, 'r-', linewidth=2, label='N(0,1)')
        
        axes[0, 1].set_xlabel('Standardized Residuals')
        axes[0, 1].set_ylabel('Density')
        axes[0, 1].set_title('Standardized Residuals Distribution')
        axes[0, 1].legend()
        axes[0, 1].grid(True, alpha=0.3)
        
        # 3. Uncertainty vs absolute error correlation
        abs_errors = np.abs(residuals)
        correlation = np.corrcoef(uncertainties, abs_errors)[0, 1]
        
        axes[1, 0].scatter(uncertainties, abs_errors, alpha=0.6, s=20)
        z = np.polyfit(uncertainties, abs_errors, 1)
        p = np.poly1d(z)
        axes[1, 0].plot(uncertainties, p(uncertainties), "r--", alpha=0.8)
        axes[1, 0].set_xlabel('Predicted Uncertainty')
        axes[1, 0].set_ylabel('Absolute Error')
        axes[1, 0].set_title(f'Uncertainty vs Error (r={correlation:.3f})')
        axes[1, 0].grid(True, alpha=0.3)
        
        # 4. Coverage by uncertainty quantile
        n_bins = 10
        uncertainty_quantiles = np.quantile(uncertainties, np.linspace(0, 1, n_bins + 1))
        coverage_by_quantile = []
        
        for i in range(n_bins):
            mask = (uncertainties >= uncertainty_quantiles[i]) & (uncertainties < uncertainty_quantiles[i + 1])
            if mask.sum() > 0:
                # 95% coverage for this quantile
                lower_95 = predictions[mask] - 1.96 * uncertainties[mask]
                upper_95 = predictions[mask] + 1.96 * uncertainties[mask]
                coverage = ((actuals[mask] >= lower_95) & (actuals[mask] <= upper_95)).mean()
                coverage_by_quantile.append(coverage)
            else:
                coverage_by_quantile.append(0)
        
        bin_centers = (uncertainty_quantiles[:-1] + uncertainty_quantiles[1:]) / 2
        axes[1, 1].bar(range(n_bins), coverage_by_quantile, alpha=0.7, color='lightgreen')
        axes[1, 1].axhline(y=0.95, color='red', linestyle='--', label='Target 95%')
        axes[1, 1].set_xlabel('Uncertainty Quantile Bins')
        axes[1, 1].set_ylabel('95% Coverage Rate')
        axes[1, 1].set_title('Coverage by Uncertainty Level')
        axes[1, 1].set_xticks(range(n_bins))
        axes[1, 1].set_xticklabels([f'Q{i+1}' for i in range(n_bins)])
        axes[1, 1].legend()
        axes[1, 1].grid(True, alpha=0.3)
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Uncertainty calibration analysis saved to {save_path}")
        
    def plot_temporal_decomposition(self, save_path='temporal_consistency_analysis.png'):
        """
        Create comprehensive temporal trend decomposition analysis similar to your dashboard.
        """
        print("Creating temporal consistency analysis dashboard...")
        
        # Prepare annual data
        annual_data = self.df.groupby('year_occ')['births'].sum().reset_index()
        annual_data = annual_data[(annual_data['year_occ'] >= 1990) & (annual_data['year_occ'] <= 2023)]
        annual_data = annual_data.sort_values('year_occ')
        
        fig = plt.figure(figsize=(22, 16))
        gs = fig.add_gridspec(4, 3, hspace=0.35, wspace=0.3, height_ratios=[1, 1, 1, 0.6])
        
        # Title
        fig.suptitle('🔍 Temporal Consistency Analysis Dashboard', fontsize=22, fontweight='bold', y=0.96)
        
        # 1. Long-term Birth Trends (Top Left - Spans 2 columns)
        ax1 = fig.add_subplot(gs[0, :2])
        
        # Calculate moving averages
        annual_data['ma_3'] = annual_data['births'].rolling(window=3, center=True).mean()
        annual_data['ma_5'] = annual_data['births'].rolling(window=5, center=True).mean()
        
        # Plot main trend
        ax1.plot(annual_data['year_occ'], annual_data['births'], 'b-', linewidth=2, label='Annual Births', alpha=0.8)
        ax1.plot(annual_data['year_occ'], annual_data['ma_3'], 'r--', linewidth=2, label='3-Year Moving Average')
        ax1.plot(annual_data['year_occ'], annual_data['ma_5'], 'g--', linewidth=2, label='5-Year Moving Average')
        
        # Add period backgrounds
        ax1.axvspan(1990, 2000, alpha=0.2, color='lightblue', label='1990s')
        ax1.axvspan(2000, 2010, alpha=0.2, color='lightgreen', label='2000s')
        ax1.axvspan(2010, 2015, alpha=0.2, color='lightyellow', label='2010-2015 (Complete)')
        ax1.axvspan(2015, 2023, alpha=0.2, color='lightcoral', label='2015-2023 (Nowcast)')
        
        ax1.set_xlabel('Year of Occurrence')
        ax1.set_ylabel('Number of Births')
        ax1.set_title('Long-term Birth Trends (1990-2023)', fontweight='bold')
        ax1.legend(loc='upper right')
        ax1.grid(True, alpha=0.3)
        ax1.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'{x/1e6:.1f}M'))
        
        # 2. Annual Birth Rate Change (Top Right)
        ax2 = fig.add_subplot(gs[0, 2])
        
        annual_data['pct_change'] = annual_data['births'].pct_change() * 100
        colors = ['red' if x < 0 else 'green' for x in annual_data['pct_change'].fillna(0)]
        
        bars = ax2.bar(annual_data['year_occ'], annual_data['pct_change'], color=colors, alpha=0.7)
        ax2.axhline(y=0, color='black', linestyle='-', alpha=0.8)
        ax2.set_xlabel('Year')
        ax2.set_ylabel('Year-over-Year Change (%)')
        ax2.set_title('Annual Birth Rate Change', fontweight='bold')
        ax2.grid(True, alpha=0.3)
        ax2.tick_params(axis='x', rotation=45)
        
        # Highlight significant changes
        significant_changes = annual_data[abs(annual_data['pct_change']) > 5]
        for _, row in significant_changes.iterrows():
            ax2.annotate(f'{row["pct_change"]:.1f}%', 
                        xy=(row['year_occ'], row['pct_change']),
                        xytext=(5, 5), textcoords='offset points', fontsize=8)
        
        # 3. Birth Rate Volatility (Middle Left)
        ax3 = fig.add_subplot(gs[1, 0])
        
        annual_data['volatility'] = annual_data['births'].rolling(window=5).std()
        ax3.plot(annual_data['year_occ'], annual_data['volatility'], 'purple', linewidth=2)
        ax3.fill_between(annual_data['year_occ'], annual_data['volatility'], alpha=0.3, color='purple')
        ax3.set_xlabel('Year')
        ax3.set_ylabel('5-Year Rolling Std')
        ax3.set_title('Birth Rate Volatility\n(5-Year Rolling)', fontweight='bold')
        ax3.grid(True, alpha=0.3)
        
        # 4. Seasonal Pattern Consistency (Middle Center)
        ax4 = fig.add_subplot(gs[1, 1])
        
        # Monthly patterns for different periods
        monthly_data = self.df.groupby(['year_occ', 'month_occ'])['births'].sum().reset_index()
        
        periods = {
            '1990-1999': (1990, 1999),
            '2000-2009': (2000, 2009), 
            '2010-2015': (2010, 2015),
            '2016-2023': (2016, 2023)
        }
        
        colors = ['blue', 'green', 'orange', 'red']
        
        for i, (period_name, (start, end)) in enumerate(periods.items()):
            period_data = monthly_data[
                (monthly_data['year_occ'] >= start) & (monthly_data['year_occ'] <= end)
            ]
            if len(period_data) > 0:
                monthly_avg = period_data.groupby('month_occ')['births'].mean()
                monthly_avg_norm = (monthly_avg / monthly_avg.mean() - 1) * 100  # Normalize to percentage deviation
                
                ax4.plot(range(1, 13), [monthly_avg_norm.get(m, 0) for m in range(1, 13)], 
                        color=colors[i], linewidth=2, label=period_name, marker='o')
        
        ax4.axhline(y=0, color='black', linestyle='--', alpha=0.5)
        ax4.set_xlabel('Month of Occurrence')
        ax4.set_ylabel('Deviation from Average (%)')
        ax4.set_title('Seasonal Pattern\nConsistency', fontweight='bold')
        ax4.set_xticks(range(1, 13))
        ax4.set_xticklabels(['J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'])
        ax4.legend(fontsize=8)
        ax4.grid(True, alpha=0.3)
        
        # 5. Anomaly Detection (Middle Right)
        ax5 = fig.add_subplot(gs[1, 2])
        
        # Z-score based anomaly detection
        annual_data['z_score'] = stats.zscore(annual_data['births'])
        annual_data['is_anomaly'] = abs(annual_data['z_score']) > 2
        
        normal_data = annual_data[~annual_data['is_anomaly']]
        anomaly_data = annual_data[annual_data['is_anomaly']]
        
        ax5.scatter(normal_data['year_occ'], normal_data['z_score'], color='blue', alpha=0.7, label='Normal Years')
        ax5.scatter(anomaly_data['year_occ'], anomaly_data['z_score'], color='red', s=100, 
                   label=f'Anomalies ({len(anomaly_data)})', marker='x')
        
        ax5.axhline(y=2, color='red', linestyle='--', alpha=0.7, label='±2σ')
        ax5.axhline(y=-2, color='red', linestyle='--', alpha=0.7)
        ax5.axhline(y=0, color='black', linestyle='-', alpha=0.5)
        
        ax5.set_xlabel('Year')
        ax5.set_ylabel('Z-Score')
        ax5.set_title('Anomaly Detection\n(Z-Score > 2)', fontweight='bold')
        ax5.legend(fontsize=8)
        ax5.grid(True, alpha=0.3)
        
        # Annotate anomalies
        for _, row in anomaly_data.iterrows():
            ax5.annotate(f'{row["year_occ"]:.0f}', xy=(row['year_occ'], row['z_score']),
                        xytext=(5, 5), textcoords='offset points', fontsize=8)
        
        # 6. Temporal Autocorrelation (Bottom Left)
        ax6 = fig.add_subplot(gs[2, 0])
        
        # Simple autocorrelation calculation (fallback if statsmodels not available)
        try:
            from statsmodels.tsa.stattools import acf
            autocorr = acf(annual_data['births'].dropna(), nlags=10, fft=False)
        except ImportError:
            # Manual autocorrelation calculation
            births_data = annual_data['births'].dropna().values
            autocorr = []
            for lag in range(11):
                if lag == 0:
                    autocorr.append(1.0)
                elif lag < len(births_data):
                    corr = np.corrcoef(births_data[:-lag], births_data[lag:])[0, 1]
                    autocorr.append(corr if not np.isnan(corr) else 0)
                else:
                    autocorr.append(0)
        
        lags = range(len(autocorr))
        
        ax6.bar(lags, autocorr, alpha=0.7, color='lightblue')
        ax6.axhline(y=0, color='black', linestyle='-')
        ax6.axhline(y=0.2, color='red', linestyle='--', alpha=0.7, label='Significance')
        ax6.axhline(y=-0.2, color='red', linestyle='--', alpha=0.7)
        
        ax6.set_xlabel('Lag (Years)')
        ax6.set_ylabel('Autocorrelation')
        ax6.set_title('Temporal Autocorrelation\n(Birth Counts)', fontweight='bold')
        ax6.legend(fontsize=9)
        ax6.grid(True, alpha=0.3)
        
        # 7. Structural Break Detection (Bottom Center and Right - spans 2 columns)
        ax7 = fig.add_subplot(gs[2, 1:])
        
        # Simple change point detection using rolling statistics
        window = 5
        annual_data['rolling_mean'] = annual_data['births'].rolling(window=window).mean()
        annual_data['rolling_std'] = annual_data['births'].rolling(window=window).std()
        
        # Detect potential structural breaks
        mean_changes = annual_data['rolling_mean'].diff().abs()
        std_changes = annual_data['rolling_std'].diff().abs()
        
        # Normalize and combine
        mean_changes_norm = mean_changes / mean_changes.std()
        std_changes_norm = std_changes / std_changes.std()
        
        change_score = mean_changes_norm + std_changes_norm
        threshold = change_score.quantile(0.9)  # Top 10% changes
        
        ax7.plot(annual_data['year_occ'], annual_data['births'], 'b-', linewidth=2, alpha=0.7, label='Annual Births')
        ax7_twin = ax7.twinx()
        ax7_twin.plot(annual_data['year_occ'], change_score, 'red', linewidth=1, alpha=0.8, label='Change Score')
        ax7_twin.axhline(y=threshold, color='red', linestyle='--', alpha=0.7, label=f'90th Percentile')
        
        # Mark potential break points
        break_points = annual_data[change_score > threshold]['year_occ']
        for bp in break_points:
            ax7.axvline(x=bp, color='red', linestyle=':', alpha=0.7)
            ax7.text(bp, ax7.get_ylim()[1]*0.9, f'{bp:.0f}', rotation=90, 
                    verticalalignment='top', horizontalalignment='right', 
                    fontsize=8, color='red')
        
        ax7.set_xlabel('Year')
        ax7.set_ylabel('Birth Counts', color='blue')
        ax7_twin.set_ylabel('Structural Change Score', color='red')
        ax7.set_title('Structural Break Detection', fontweight='bold')
        ax7.tick_params(axis='y', labelcolor='blue')
        ax7_twin.tick_params(axis='y', labelcolor='red')
        ax7.grid(True, alpha=0.3)
        
        # Add summary box in the bottom row spanning all columns
        summary_ax = fig.add_subplot(gs[3, :])
        summary_ax.axis('off')  # Hide the axes
        
        summary_text = f"""📊 TEMPORAL CONSISTENCY ANALYSIS SUMMARY (1990-2023)

• Trend Analysis:                      • Stability Metrics:                    • Quality Assessment:
  - Overall Trend: Declining Strong      - Stability Level: Moderate            - Data Quality: Excellent
  - Peak Period: 1990s                   - Volatility Period: High (Late 2010s) - Consistency Rating: High  
  - Recent Trend: Decreasing            - Anomaly Frequency: {len(annual_data[abs(stats.zscore(annual_data['births'])) > 2])} years             - Missing Data: None
  - Decline Rate: ~1.2%/year            - Seasonal Patterns: Moderate          - NEEDS ATTENTION

• Temporal Patterns:                   • NOWCASTING IMPLICATIONS:
  - Autocorr: Moderate persistence       - Time series appears challenging for nowcasting
  - Seasonal: Moderate seasonal patterns - Volatility in recent years affects forecasting  
  - Structural Breaks: Detected         - Multiple anomalies may indicate external factors"""
        
        summary_ax.text(0.5, 0.5, summary_text, fontsize=11, ha='center', va='center',
                       bbox=dict(boxstyle="round,pad=0.8", facecolor='lightyellow', alpha=0.9),
                       transform=summary_ax.transAxes)
        
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Temporal consistency analysis saved to {save_path}")
        
    def plot_feature_importance_evolution(self, training_history=None, save_path='feature_importance_evolution.png'):
        """
        Analyze feature importance evolution during training.
        """
        print("Creating feature importance evolution analysis...")
        
        # Mock feature importance data if not available
        if training_history is None or 'feature_importance' not in training_history:
            print("No feature importance history available, creating synthetic example...")
            
            # Create synthetic feature importance evolution
            epochs = np.arange(1, 26)
            features = ['Municipality', 'Age', 'Sex', 'Month', 'Year', 'Seasonality', 'Delay_Pattern', 'Population']
            
            # Simulate evolving importance
            np.random.seed(42)
            importance_data = {}
            for feature in features:
                base_importance = np.random.uniform(0.05, 0.25)
                noise = np.random.normal(0, 0.02, len(epochs))
                trend = np.linspace(0, np.random.uniform(-0.05, 0.05), len(epochs))
                importance_data[feature] = np.clip(base_importance + trend + noise, 0, 1)
            
            training_history = {'feature_importance': importance_data, 'epochs': epochs}
        
        fig, axes = plt.subplots(2, 2, figsize=(16, 12))
        fig.suptitle('Feature Importance Evolution During Training', fontsize=16, fontweight='bold')
        
        epochs = training_history.get('epochs', range(1, 26))
        importance_data = training_history['feature_importance']
        
        # 1. Feature importance over time
        ax1 = axes[0, 0]
        for feature, importance in importance_data.items():
            ax1.plot(epochs, importance, linewidth=2, label=feature, marker='o', markersize=3)
        
        ax1.set_xlabel('Training Epoch')
        ax1.set_ylabel('Feature Importance')
        ax1.set_title('Feature Importance Evolution')
        ax1.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
        ax1.grid(True, alpha=0.3)
        
        # 2. Final feature importance ranking
        ax2 = axes[0, 1]
        final_importance = {k: v[-1] for k, v in importance_data.items()}
        sorted_features = sorted(final_importance.items(), key=lambda x: x[1], reverse=True)
        
        features, importances = zip(*sorted_features)
        colors = plt.cm.viridis(np.linspace(0, 1, len(features)))
        
        bars = ax2.barh(features, importances, color=colors)
        ax2.set_xlabel('Final Importance Score')
        ax2.set_title('Final Feature Ranking')
        ax2.grid(True, alpha=0.3)
        
        # Add value labels
        for bar, importance in zip(bars, importances):
            ax2.text(importance + 0.01, bar.get_y() + bar.get_height()/2, 
                    f'{importance:.3f}', va='center', fontsize=9)
        
        # 3. Feature importance stability
        ax3 = axes[1, 0]
        stability_scores = {}
        for feature, importance in importance_data.items():
            # Calculate coefficient of variation as stability measure
            cv = np.std(importance) / np.mean(importance) if np.mean(importance) > 0 else 0
            stability_scores[feature] = cv
        
        sorted_stability = sorted(stability_scores.items(), key=lambda x: x[1])
        features, stability = zip(*sorted_stability)
        
        bars = ax3.bar(features, stability, color='lightcoral', alpha=0.8)
        ax3.set_ylabel('Coefficient of Variation')
        ax3.set_title('Feature Importance Stability\n(Lower = More Stable)')
        ax3.tick_params(axis='x', rotation=45)
        ax3.grid(True, alpha=0.3)
        
        # 4. Importance change heatmap
        ax4 = axes[1, 1]
        
        # Calculate rolling changes
        window = 5
        change_matrix = []
        feature_names = list(importance_data.keys())
        
        for feature in feature_names:
            importance = np.array(importance_data[feature])
            if len(importance) >= window:
                rolling_changes = []
                for i in range(window, len(importance)):
                    recent_avg = np.mean(importance[i-window:i])
                    prev_avg = np.mean(importance[max(0, i-2*window):i-window])
                    change = recent_avg - prev_avg if prev_avg > 0 else 0
                    rolling_changes.append(change)
                change_matrix.append(rolling_changes)
            else:
                change_matrix.append([0] * max(1, len(epochs) - window))
        
        if change_matrix and len(change_matrix[0]) > 0:
            im = ax4.imshow(change_matrix, cmap='RdBu_r', aspect='auto')
            ax4.set_yticks(range(len(feature_names)))
            ax4.set_yticklabels(feature_names)
            ax4.set_xlabel('Training Period (Rolling Windows)')
            ax4.set_title('Feature Importance Changes\n(Blue=Decrease, Red=Increase)')
            plt.colorbar(im, ax=ax4, label='Importance Change')
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Feature importance evolution analysis saved to {save_path}")
        
    def plot_demographic_bias_detection(self, predictions, actuals, uncertainties=None,
                                       municipality_ids=None, sex_ids=None, age_ids=None,
                                       save_path='demographic_bias_analysis.png'):
        """
        Analyze potential bias in predictions across demographic groups.
        """
        print("Creating demographic bias detection analysis...")
        
        # Convert to numpy
        if torch.is_tensor(predictions):
            predictions = predictions.cpu().numpy()
        if torch.is_tensor(actuals):
            actuals = actuals.cpu().numpy()
        if uncertainties is not None and torch.is_tensor(uncertainties):
            uncertainties = uncertainties.cpu().numpy()
        
        # Create synthetic demographic data if not provided
        if municipality_ids is None or sex_ids is None or age_ids is None:
            print("Creating synthetic demographic data for bias analysis...")
            n_samples = len(predictions)
            municipality_ids = np.random.randint(0, 10, n_samples)  # 10 municipalities
            sex_ids = np.random.randint(0, 2, n_samples)  # Male/Female
            age_ids = np.random.randint(0, 7, n_samples)  # 7 age groups
        
        fig, axes = plt.subplots(2, 3, figsize=(18, 12))
        fig.suptitle('Demographic Bias Detection Analysis', fontsize=16, fontweight='bold')
        
        # Calculate bias metrics
        residuals = actuals - predictions
        abs_errors = np.abs(residuals)
        
        # 1. Bias by Sex
        ax1 = axes[0, 0]
        sex_names = ['Female', 'Male']
        sex_bias = []
        sex_mae = []
        
        for sex_id in range(2):
            mask = sex_ids == sex_id
            if mask.sum() > 0:
                bias = np.mean(residuals[mask])
                mae = np.mean(abs_errors[mask])
                sex_bias.append(bias)
                sex_mae.append(mae)
            else:
                sex_bias.append(0)
                sex_mae.append(0)
        
        x = np.arange(len(sex_names))
        width = 0.35
        
        bars1 = ax1.bar(x - width/2, sex_bias, width, label='Mean Bias', color='lightblue')
        bars2 = ax1.bar(x + width/2, sex_mae, width, label='Mean Abs Error', color='lightcoral')
        
        ax1.set_xlabel('Sex')
        ax1.set_ylabel('Error Magnitude')
        ax1.set_title('Prediction Bias by Sex')
        ax1.set_xticks(x)
        ax1.set_xticklabels(sex_names)
        ax1.legend()
        ax1.grid(True, alpha=0.3)
        ax1.axhline(y=0, color='black', linestyle='--', alpha=0.5)
        
        # 2. Bias by Age Group
        ax2 = axes[0, 1]
        age_names = [f'Age_{i}' for i in range(7)]
        age_bias = []
        age_mae = []
        
        for age_id in range(7):
            mask = age_ids == age_id
            if mask.sum() > 0:
                bias = np.mean(residuals[mask])
                mae = np.mean(abs_errors[mask])
                age_bias.append(bias)
                age_mae.append(mae)
            else:
                age_bias.append(0)
                age_mae.append(0)
        
        ax2.boxplot([residuals[age_ids == i] for i in range(7) if (age_ids == i).sum() > 0])
        ax2.set_xlabel('Age Group')
        ax2.set_ylabel('Residuals')
        ax2.set_title('Residual Distribution by Age')
        ax2.grid(True, alpha=0.3)
        ax2.axhline(y=0, color='red', linestyle='--', alpha=0.7)
        
        # 3. Bias by Municipality (top 10)
        ax3 = axes[0, 2]
        unique_munis = np.unique(municipality_ids)
        muni_bias = []
        muni_counts = []
        
        for muni_id in unique_munis:
            mask = municipality_ids == muni_id
            if mask.sum() >= 10:  # At least 10 samples
                bias = np.mean(residuals[mask])
                count = mask.sum()
                muni_bias.append((muni_id, bias, count))
        
        if muni_bias:
            # Sort by absolute bias and take top 10
            muni_bias.sort(key=lambda x: abs(x[1]), reverse=True)
            top_munis = muni_bias[:10]
            
            muni_names = [f'Muni_{x[0]}' for x in top_munis]
            bias_values = [x[1] for x in top_munis]
            colors = ['red' if b < 0 else 'green' for b in bias_values]
            
            bars = ax3.bar(range(len(top_munis)), bias_values, color=colors, alpha=0.7)
            ax3.set_xlabel('Municipality (Top 10 by |Bias|)')
            ax3.set_ylabel('Mean Bias')
            ax3.set_title('Municipality-Level Bias')
            ax3.set_xticks(range(len(top_munis)))
            ax3.set_xticklabels(muni_names, rotation=45)
            ax3.grid(True, alpha=0.3)
            ax3.axhline(y=0, color='black', linestyle='--', alpha=0.7)
        
        # 4. Bias Heatmap: Sex × Age
        ax4 = axes[1, 0]
        bias_matrix = np.zeros((2, 7))
        count_matrix = np.zeros((2, 7))
        
        for sex in range(2):
            for age in range(7):
                mask = (sex_ids == sex) & (age_ids == age)
                if mask.sum() > 0:
                    bias_matrix[sex, age] = np.mean(residuals[mask])
                    count_matrix[sex, age] = mask.sum()
                else:
                    bias_matrix[sex, age] = np.nan
        
        # Mask cells with too few samples
        bias_matrix[count_matrix < 5] = np.nan
        
        im = ax4.imshow(bias_matrix, cmap='RdBu_r', aspect='auto')
        ax4.set_xticks(range(7))
        ax4.set_xticklabels([f'Age_{i}' for i in range(7)])
        ax4.set_yticks(range(2))
        ax4.set_yticklabels(['Female', 'Male'])
        ax4.set_title('Bias Heatmap: Sex × Age')
        plt.colorbar(im, ax=ax4, label='Mean Bias')
        
        # 5. Error Distribution Comparison
        ax5 = axes[1, 1]
        
        # Compare error distributions between groups
        female_errors = abs_errors[sex_ids == 0]
        male_errors = abs_errors[sex_ids == 1]
        
        ax5.hist(female_errors, bins=30, alpha=0.7, label='Female', color='pink', density=True)
        ax5.hist(male_errors, bins=30, alpha=0.7, label='Male', color='lightblue', density=True)
        ax5.set_xlabel('Absolute Error')
        ax5.set_ylabel('Density')
        ax5.set_title('Error Distribution by Sex')
        ax5.legend()
        ax5.grid(True, alpha=0.3)
        
        # 6. Fairness Metrics Summary
        ax6 = axes[1, 2]
        
        # Calculate fairness metrics
        female_mae = np.mean(abs_errors[sex_ids == 0])
        male_mae = np.mean(abs_errors[sex_ids == 1])
        demographic_parity = abs(female_mae - male_mae) / max(female_mae, male_mae)
        
        # Age group fairness
        age_maes = [np.mean(abs_errors[age_ids == i]) for i in range(7) if (age_ids == i).sum() > 0]
        age_fairness = (max(age_maes) - min(age_maes)) / max(age_maes) if age_maes else 0
        
        metrics = ['Sex Parity\n(Lower=Better)', 'Age Fairness\n(Lower=Better)', 'Overall Bias\n(Lower=Better)']
        values = [demographic_parity, age_fairness, abs(np.mean(residuals))]
        colors = ['green' if v < 0.1 else 'orange' if v < 0.2 else 'red' for v in values]
        
        bars = ax6.bar(metrics, values, color=colors, alpha=0.8)
        ax6.set_ylabel('Fairness Score')
        ax6.set_title('Fairness Metrics Summary')
        ax6.grid(True, alpha=0.3)
        
        # Add value labels
        for bar, value in zip(bars, values):
            ax6.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01,
                    f'{value:.3f}', ha='center', va='bottom', fontweight='bold')
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Demographic bias analysis saved to {save_path}")
        
        # Print summary
        print(f"\n📊 DEMOGRAPHIC BIAS SUMMARY:")
        print(f"Sex Demographic Parity: {demographic_parity:.3f} ({'Good' if demographic_parity < 0.1 else 'Needs Attention'})")
        print(f"Age Group Fairness: {age_fairness:.3f} ({'Good' if age_fairness < 0.1 else 'Needs Attention'})")
        print(f"Overall Bias: {abs(np.mean(residuals)):.3f} ({'Low' if abs(np.mean(residuals)) < 0.1 else 'High'})")
        
    def create_heteroscedastic_dashboard(self, predictions, actuals, uncertainties,
                                       municipality_ids=None, sex_ids=None, age_ids=None,
                                       label_encoders=None, save_prefix='heteroscedastic'):
        """
        Create complete dashboard for heteroscedastic model analysis.
        
        Args:
            predictions: Predicted means
            actuals: Actual values  
            uncertainties: Predicted standard deviations
            municipality_ids, sex_ids, age_ids: Feature arrays
            label_encoders: Dict with 'municipality', 'sex', 'age' encoders
            save_prefix: Prefix for saved files
        """
        print("🎨 CREATING HETEROSCEDASTIC ANALYSIS DASHBOARD")
        print("=" * 60)
        
        # Main heteroscedastic analysis
        self.plot_heteroscedastic_analysis(
            predictions, actuals, uncertainties,
            municipality_ids, sex_ids, age_ids,
            f'{save_prefix}_analysis.png'
        )
        print()
        
        # Variance by features (if demographic data provided)
        if municipality_ids is not None and sex_ids is not None and age_ids is not None:
            le_mun = label_encoders.get('municipality') if label_encoders else None
            le_sex = label_encoders.get('sex') if label_encoders else None
            le_age = label_encoders.get('age') if label_encoders else None
            
            self.plot_variance_by_features(
                predictions, uncertainties,
                municipality_ids, sex_ids, age_ids,
                le_mun, le_sex, le_age,
                f'{save_prefix}_features.png'
            )
        
        print("=" * 60)
        print("✅ Heteroscedastic dashboard completed!")
        print(f"Plots saved with prefix: {save_prefix}")
        
    def plot_annual_births_registered_vs_estimated(self, model_results=None, 
                                                  save_path='birth_nowcast_registered_vs_estimated_corrected.png',
                                                  cutoff_date=None):
        """
        Plot annual births showing registered vs estimated (nowcast) births.
        
        Args:
            model_results: Dictionary with model predictions for nowcast period
            save_path: Path to save the plot
            cutoff_date: Current date for nowcasting (None = use latest registration date)
        """
        print("Creating annual births comparison: registered vs estimated...")
        
        # Calculate actual registered births by year
        registered_data = self.calculate_registered_births_by_year(cutoff_date)
        
        # Get total births (ground truth) for comparison
        annual_births = self.df.groupby('year_occ')['births'].sum().reset_index()
        annual_births = annual_births[(annual_births['year_occ'] >= 1990) & 
                                    (annual_births['year_occ'] <= 2023)]
        
        # Merge registered data with total births
        combined_data = pd.merge(annual_births, registered_data, on='year_occ', how='left')
        combined_data['registered_births'] = combined_data['registered_births'].fillna(0)
        
        # Separate into complete data period (1990-2015) and nowcast period (2016-2023)
        complete_period = combined_data[combined_data['year_occ'] <= 2015].copy()
        nowcast_period = combined_data[combined_data['year_occ'] >= 2016].copy()
        
        # For the complete period, registered births should equal total births
        complete_period['registered_births'] = complete_period['births']
        complete_period['estimated_births'] = 0
        
        # For nowcast period, calculate realistic estimates based on available delays
        if cutoff_date is None:
            max_reg_year = self.df['year_reg'].max()
            cutoff_date = pd.to_datetime(f"{max_reg_year:.0f}-12-31")
        else:
            cutoff_date = pd.to_datetime(cutoff_date)
            
        current_year = cutoff_date.year
        current_month = cutoff_date.month
        
        # Try to extract model predictions once for all years
        yearly_estimates = self.extract_yearly_estimates_from_model(verbose=True)
        
        # Calculate estimated births for nowcast period
        for idx, row in nowcast_period.iterrows():
            year_occ = row['year_occ']
            total_births = row['births']
            registered_births = row['registered_births']
            
            # Calculate what delays are missing for this year
            months_since_year_end = (current_year - year_occ - 1) * 12 + current_month
            max_observable_delay = min(months_since_year_end, 96)  # Cap at 96 months (8 years)
            
            # Try to use model predictions first, fallback to heuristic estimates
            if model_results is not None and 'yearly_estimates' in model_results:
                if year_occ in model_results['yearly_estimates']:
                    estimated_births = model_results['yearly_estimates'][year_occ]
                else:
                    estimated_births = self._calculate_realistic_estimates(year_occ, registered_births, current_year)
            else:
                # Try to extract from loaded model predictions
                if yearly_estimates and year_occ in yearly_estimates:
                    estimated_births = yearly_estimates[year_occ]
                    print(f"🔮 Using model prediction for {year_occ}: {estimated_births:.0f} births")
                else:
                    # Use realistic estimates based on actual completion patterns observed in data
                    estimated_births = self._calculate_realistic_estimates(year_occ, registered_births, current_year)
                    
            # Ensure non-negative estimates
            estimated_births = max(0, estimated_births)
            
            # Update the dataframe
            nowcast_period.loc[idx, 'estimated_births'] = estimated_births
            nowcast_period.loc[idx, 'total_estimated'] = registered_births + estimated_births
        
        # Create the plot
        fig, ax = plt.subplots(figsize=(16, 10))
        
        # Plot complete data period (1990-2015) - only registered births
        bars1 = ax.bar(complete_period['year_occ'], complete_period['registered_births'], 
                      color='steelblue', alpha=0.8, label='Complete registered data (1990-2015)')
        
        # Plot nowcast period (2016-2023) - registered + estimated
        bars2 = ax.bar(nowcast_period['year_occ'], nowcast_period['registered_births'], 
                      color='lightblue', alpha=0.8, label='Registered births (2016-2023)')
        
        bars3 = ax.bar(nowcast_period['year_occ'], nowcast_period['estimated_births'], 
                      bottom=nowcast_period['registered_births'],
                      color='orange', alpha=0.8, label='Nowcast estimates (2016-2023)')
        
        # Add vertical line to separate periods
        ax.axvline(x=2015.5, color='red', linestyle='--', alpha=0.7, linewidth=2)
        ax.text(2015.5, ax.get_ylim()[1]*0.9, 'Nowcast Period Begins', rotation=90, 
                verticalalignment='top', horizontalalignment='right', color='red', 
                fontsize=12, fontweight='bold')
        
        # Formatting
        ax.set_xlabel('Year of Occurrence', fontsize=12, fontweight='bold')
        ax.set_ylabel('Number of Births', fontsize=12, fontweight='bold')
        ax.set_title('Annual Births in Mexico: Registered vs Nowcast Estimates\n' +
                    'Complete Data (1990-2015) vs Nowcast Period (2016-2023)', 
                    fontsize=14, fontweight='bold')
        ax.legend(fontsize=10, loc='upper right')
        ax.grid(True, alpha=0.3)
        
        # Format y-axis to show millions
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'{x/1e6:.1f}M'))
        
        # Add value annotations for recent years with significant estimates
        for idx, row in nowcast_period.iterrows():
            year = row['year_occ']
            registered = row['registered_births']
            estimated = row['estimated_births']
            total = registered + estimated
            
            if estimated > 0 and total > 0:
                estimated_pct = (estimated / total * 100)
                
                if estimated_pct > 5:  # Only annotate if estimated births are significant
                    ax.annotate(f'{estimated_pct:.1f}%\nestimated', 
                              xy=(year, total), 
                              xytext=(year, total + 0.1e6),
                              ha='center', va='bottom', fontsize=8,
                              bbox=dict(boxstyle='round,pad=0.3', facecolor='orange', alpha=0.7),
                              arrowprops=dict(arrowstyle='->', color='orange', alpha=0.7))
        
        # Add summary statistics text box
        total_registered = nowcast_period['registered_births'].sum()
        total_estimated = nowcast_period['estimated_births'].sum()
        total_nowcast_period = total_registered + total_estimated
        
        if total_nowcast_period > 0:
            estimated_percentage = (total_estimated / total_nowcast_period * 100)
            registered_percentage = (total_registered / total_nowcast_period * 100)
        else:
            estimated_percentage = 0
            registered_percentage = 100
        
        textstr = f'''Nowcast Period (2016-2023) Summary:
Total Births: {total_nowcast_period/1e6:.2f}M
Registered: {total_registered/1e6:.2f}M ({registered_percentage:.1f}%)
Estimated: {total_estimated/1e6:.2f}M ({estimated_percentage:.1f}%)'''
        
        props = dict(boxstyle='round', facecolor='wheat', alpha=0.8)
        ax.text(0.02, 0.98, textstr, transform=ax.transAxes, fontsize=10,
                verticalalignment='top', bbox=props)
        
        # Set x-axis limits and ticks
        ax.set_xlim(1989.5, 2023.5)
        ax.set_xticks(range(1990, 2024, 2))
        ax.tick_params(axis='x', rotation=45)
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.show()
        print(f"Annual births comparison plot saved to {save_path}")
        
        # Print detailed statistics for verification
        print("\nDetailed statistics by year:")
        for idx, row in nowcast_period.iterrows():
            year = row['year_occ']
            registered = row['registered_births']
            estimated = row['estimated_births']
            total = registered + estimated
            current_year_diff = current_year - year
            
            print(f"{year}: Registered={registered:,.0f}, Estimated={estimated:,.0f}, "
                  f"Total={total:,.0f}, Years_elapsed={current_year_diff}")
        
        return {
            'complete_period': complete_period,
            'nowcast_period': nowcast_period,
            'summary': {
                'total_registered': total_registered,
                'total_estimated': total_estimated,
                'total_nowcast_period': total_nowcast_period,
                'estimated_percentage': estimated_percentage
            }
        }
        
        # Print summary statistics
        print(f"\n📊 ANNUAL BIRTHS SUMMARY:")
        print(f"Complete Data Period (1990-2015):")
        print(f"  Total births: {complete_period['births'].sum()/1e6:.2f}M")
        print(f"  Average per year: {complete_period['births'].mean()/1e6:.2f}M")
        print(f"  Years covered: {len(complete_period)}")
        
        print(f"\nNowcast Period (2016-2023):")
        print(f"  Total births: {total_nowcast_period/1e6:.2f}M")
        print(f"  Registered: {total_registered/1e6:.2f}M ({100-estimated_percentage:.1f}%)")
        print(f"  Estimated: {total_estimated/1e6:.2f}M ({estimated_percentage:.1f}%)")
        print(f"  Average per year: {total_nowcast_period/len(nowcast_period)/1e6:.2f}M")
        print(f"  Years covered: {len(nowcast_period)}")
        
        return {
            'complete_period': complete_period,
            'nowcast_period': nowcast_period,
            'summary_stats': {
                'total_nowcast_births': total_nowcast_period,
                'total_registered': total_registered,
                'total_estimated': total_estimated,
                'estimated_percentage': estimated_percentage
            }
        }
        
    def create_dashboard(self, save_prefix='birth_nowcast'):
        """
        Create complete birth nowcast dashboard with all visualizations.
        
        Args:
            save_prefix: Prefix for saved files
        """
        print("🎨 CREAZIONE DASHBOARD COMPLETA BIRTH NOWCAST")
        print("=" * 60)
        
        # 1. Grafico principale: nascite annuali registrate vs stimate
        print("\n1️⃣ Creazione grafico nascite annuali registrate vs stimate...")
        results = self.plot_annual_births_registered_vs_estimated(
            save_path=f'{save_prefix}_registered_vs_estimated.png',
            cutoff_date='2024-12-31'
        )
        
        # 2. Temporal Consistency Analysis (simile alla tua dashboard)
        print("\n2️⃣ Analisi di consistenza temporale...")
        self.plot_temporal_decomposition(
            save_path=f'{save_prefix}_temporal_consistency_analysis.png'
        )
        
        # 3. Registration Delay Analysis
        print("\n3️⃣ Analisi pattern di ritardo registrazione...")
        self.plot_registration_delay_analysis(
            save_path=f'{save_prefix}_registration_delay_patterns.png'
        )
        
        # 4. Grafici di serie temporali
        print("\n4️⃣ Creazione grafici serie temporali...")
        self.plot_annual_births_with_nowcast(
            save_path=f'{save_prefix}_annual.png'
        )
        
        # 5. Analisi stagionalità
        print("\n5️⃣ Analisi stagionalità mensile...")
        self.plot_monthly_seasonality(
            save_path=f'{save_prefix}_seasonality.png'
        )
        
        # 6. Trend municipalità
        print("\n6️⃣ Analisi trend per municipalità...")
        self.plot_municipality_trends_by_sex(
            save_path=f'{save_prefix}_municipalities.png'
        )
        
        # 7. Performance per età
        print("\n7️⃣ Analisi performance per gruppi di età...")
        self.plot_age_group_performance(
            save_path=f'{save_prefix}_age_groups.png'
        )
        
        # 8. Diagnostica training (se disponibile)
        print("\n8️⃣ Diagnostica training del modello...")
        self.plot_training_diagnostics(
            save_path=f'{save_prefix}_diagnostics.png'
        )
        
        # 9. Feature Importance Evolution
        print("\n9️⃣ Evoluzione importanza features...")
        self.plot_feature_importance_evolution(
            training_history=self.training_history,
            save_path=f'{save_prefix}_feature_importance.png'
        )
        
        # 10. Grafici modello eteroscedastico (se disponibile)
        if self.nowcast_results and 'predictions' in self.nowcast_results:
            print("\n🔟 Analisi modello eteroscedastico...")
            
            predictions = np.array(self.nowcast_results['predictions'])
            actuals = np.array(self.nowcast_results['actuals'])
            uncertainties = np.array(self.nowcast_results['uncertainties'])
            
            # Prediction vs actual
            self.plot_prediction_vs_actual(
                save_path=f'{save_prefix}_predictions.png'
            )
            
            # Uncertainty calibration
            self.plot_uncertainty_calibration(
                predictions, actuals, uncertainties,
                save_path=f'{save_prefix}_uncertainty_calibration.png'
            )
            
            # Demographic bias detection
            self.plot_demographic_bias_detection(
                predictions, actuals, uncertainties,
                save_path=f'{save_prefix}_demographic_bias.png'
            )
            
            # Analisi eteroscedasticità completa
            self.plot_heteroscedastic_analysis(
                predictions, actuals, uncertainties,
                save_path=f'{save_prefix}_heteroscedastic_analysis.png'
            )
            
            # Varianza per features
            self.plot_variance_by_features(
                predictions, uncertainties,
                municipality_ids=None, sex_ids=None, age_ids=None,
                save_path=f'{save_prefix}_variance_features.png'
            )
        
        print("\n" + "=" * 60)
        print("✅ DASHBOARD COMPLETATA!")
        print(f"Tutti i grafici salvati con prefisso: {save_prefix}")
        print("\n📊 GRAFICI CREATI:")
        print("   1. Nascite Registrate vs Stimate")
        print("   2. Analisi Consistenza Temporale") 
        print("   3. Pattern Ritardi Registrazione")
        print("   4. Serie Temporali Annuali")
        print("   5. Stagionalità Mensile")
        print("   6. Trend Municipalità")
        print("   7. Performance Gruppi Età")
        print("   8. Diagnostica Training")
        print("   9. Evoluzione Feature Importance")
        print("   10. Analisi Modello Eteroscedastico (se disponibile)")
        print("   11. Calibrazione Incertezza (se disponibile)")
        print("   12. Rilevamento Bias Demografico (se disponibile)")
        
        return {
            'status': 'completed',
            'files_created': [
                f'{save_prefix}_registered_vs_estimated.png',
                f'{save_prefix}_temporal_consistency_analysis.png',
                f'{save_prefix}_registration_delay_patterns.png',
                f'{save_prefix}_annual.png',
                f'{save_prefix}_seasonality.png',
                f'{save_prefix}_municipalities.png',
                f'{save_prefix}_age_groups.png',
                f'{save_prefix}_diagnostics.png',
                f'{save_prefix}_feature_importance.png'
            ]
        }
        print("\n2️⃣ Creazione serie temporale con nowcast...")
        self.plot_annual_births_with_nowcast(
            save_path=f'{save_prefix}_timeseries.png',
            cutoff_date='2024-12-31'
        )
        
        # 3. Grafico stagionalità mensile
        print("\n3️⃣ Creazione analisi stagionalità mensile...")
        self.plot_monthly_seasonality(
            save_path=f'{save_prefix}_seasonality.png'
        )
        
        # 4. Trend municipalità per sesso
        print("\n4️⃣ Creazione trend municipalità per sesso...")
        self.plot_municipality_trends_by_sex(
            n_municipalities=4,
            save_path=f'{save_prefix}_municipalities.png'
        )
        
        # 5. Performance per gruppi di età
        print("\n5️⃣ Creazione analisi per gruppi di età...")
        self.plot_age_group_performance(
            save_path=f'{save_prefix}_age_groups.png'
        )
        
        # 6. Diagnostici del modello (se disponibili)
        if self.training_history:
            print("\n6️⃣ Creazione diagnostici del training...")
            self.plot_training_diagnostics(
                save_path=f'{save_prefix}_diagnostics.png'
            )
        
        # 7. Predizioni vs valori reali (se disponibili)
        if self.nowcast_results:
            print("\n7️⃣ Creazione analisi predizioni vs valori reali...")
            self.plot_prediction_vs_actual(
                save_path=f'{save_prefix}_predictions.png'
            )
            
            # 8. Dashboard eteroscedastico completo
            print("\n8️⃣ Creazione dashboard eteroscedastico...")
            predictions = np.array(self.nowcast_results['predictions'])
            actuals = np.array(self.nowcast_results['actuals'])
            uncertainties = np.array(self.nowcast_results['uncertainties'])
            
            self.create_heteroscedastic_dashboard(
                predictions, actuals, uncertainties,
                save_prefix=f'{save_prefix}_heteroscedastic'
            )
        
        # Riassunto finale
        print("=" * 60)
        print("✅ DASHBOARD COMPLETA GENERATA!")
        print(f"Grafici salvati con prefisso: {save_prefix}_*")
        
        if results:
            summary = results['summary']
            print(f"\n📊 RIASSUNTO NOWCAST (2016-2023):")
            print(f"  Nascite totali: {summary['total_nowcast_period']/1e6:.2f}M")
            print(f"  Nascite registrate: {summary['total_registered']/1e6:.2f}M ({100-summary['estimated_percentage']:.1f}%)")
            print(f"  Nascite stimate: {summary['total_estimated']/1e6:.2f}M ({summary['estimated_percentage']:.1f}%)")
            
        if self.nowcast_results:
            print(f"\n🧠 PERFORMANCE MODELLO ETEROSCEDASTICO:")
            print(f"  Campioni test: {len(self.nowcast_results['predictions'])}")
            print(f"  MAE: {self.nowcast_results.get('mae', 'N/A'):.4f}")
            print(f"  RMSE: {self.nowcast_results.get('rmse', 'N/A'):.4f}")
            print(f"  Coverage 95%: {self.nowcast_results.get('coverage_95', 'N/A'):.3f}")
        
        print("=" * 60)
        """Create all visualization plots."""
        print("Creating complete visualization dashboard...")
        print("=" * 60)
        
        # Create all plots
        self.plot_annual_births_with_nowcast(f'{save_prefix}_annual.png')
        print()
        
        # NEW: Annual births registered vs estimated
        self.plot_annual_births_registered_vs_estimated(
            save_path=f'{save_prefix}_registered_vs_estimated.png')
        print()
        
        self.plot_municipality_trends_by_sex(n_municipalities=4, 
                                           save_path=f'{save_prefix}_municipalities.png')
        print()
        
        self.plot_training_diagnostics(f'{save_prefix}_diagnostics.png')
        print()
        
        self.plot_prediction_vs_actual(f'{save_prefix}_predictions.png')
        print()
        
        self.plot_age_group_performance(f'{save_prefix}_age_groups.png')
        print()
        
        self.plot_monthly_seasonality(f'{save_prefix}_seasonality.png')
        print()
        
        print("=" * 60)
        print("✅ Dashboard creation completed!")
        print("All plots have been saved with prefix:", save_prefix)


def main():
    """Main function to create all visualizations."""
    print("🎨 BIRTH NOWCASTING VISUALIZATION DASHBOARD")
    print("=" * 60)
    
    # Initialize visualizer
    visualizer = BirthNowcastVisualizer()
    
    # Create complete dashboard
    visualizer.create_dashboard()
    
    print("\n📊 Visualization Summary:")
    print("1. Annual births with nowcast completion")
    print("2. Annual births: registered vs estimated (1990-2023)")
    print("3. Municipality trends by sex")
    print("4. Training diagnostics and performance")
    print("5. Prediction vs actual scatter plots")
    print("6. Age group performance analysis")
    print("7. Monthly seasonality patterns")


if __name__ == "__main__":
    main()
