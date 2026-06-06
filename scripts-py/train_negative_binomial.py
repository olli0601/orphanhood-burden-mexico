"""
Training with CORRECTED Negative Binomial Distribution
Implementation of municipality-specific overdispersion parameters
CORRECTED formula: Var = μ + α_l·μ² where α_l is municipality-specific
"""

import os
import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader
from datetime import datetime
from typing import Dict, Any, List, Tuple
import warnings

from data_processing import BirthDataProcessor
from birth_nowcast_models import BirthNowcastPNN

class NegativeBinomialTrainer:
    """
    Trainer for CORRECTED Negative Binomial distribution with municipality-specific overdispersion.
    
    The negative binomial distribution is parameterized as:
    - μ = mean (from neural network)
    - α_l = municipality-specific overdispersion parameter
    - CORRECTED Variance = μ + α_l·μ² (quadratic growth)
    """
    
    def __init__(self, model: nn.Module, device: torch.device):
        self.model = model
        self.device = device
        self.optimizer = None
        self.history = {'train_loss': [], 'val_loss': []}
        
    def negative_binomial_loss(self, dist, targets):
        """
        Compute negative log-likelihood for Negative Binomial distribution.
        
        Args:
            dist: torch.distributions.NegativeBinomial distribution
            targets: actual birth counts (raw counts, not log-transformed)
            
        Returns:
            loss: negative log-likelihood
        """
        # Ensure targets are integers for Negative Binomial
        targets = targets.round().long()
        
        # Compute negative log-likelihood
        log_prob = dist.log_prob(targets)
        
        # Handle edge cases
        valid_mask = torch.isfinite(log_prob)
        if not valid_mask.all():
            warnings.warn(f"Found {(~valid_mask).sum()} non-finite log probabilities")
            log_prob = log_prob[valid_mask]
        
        # Return negative log-likelihood (we minimize loss)
        return -log_prob.mean()
    
    def train_epoch(self, train_loader: DataLoader) -> float:
        """Train for one epoch."""
        self.model.train()
        total_loss = 0.0
        num_batches = 0
        
        for batch in train_loader:
            # Move data to device
            reporting_triangle = batch['reporting_triangle'].to(self.device)
            municipality_ids = batch['municipality_id'].squeeze().to(self.device)
            sex_ids = batch['sex_id'].squeeze().to(self.device)
            age_group_ids = batch['age_group_id'].squeeze().to(self.device)
            population_offset = batch['population_offset'].squeeze().to(self.device)
            targets = batch['target_births'].squeeze().to(self.device)
            
            # Forward pass
            dist = self.model(reporting_triangle, municipality_ids, sex_ids, age_group_ids, population_offset)
            
            # Compute loss
            loss = self.negative_binomial_loss(dist, targets)
            
            # Backward pass
            self.optimizer.zero_grad()
            loss.backward()
            
            # Gradient clipping to prevent exploding gradients
            torch.nn.utils.clip_grad_norm_(self.model.parameters(), max_norm=5.0)
            
            self.optimizer.step()
            
            total_loss += loss.item()
            num_batches += 1
        
        return total_loss / num_batches
    
    def validate_epoch(self, val_loader: DataLoader) -> float:
        """Validate for one epoch."""
        self.model.eval()
        total_loss = 0.0
        num_batches = 0
        
        with torch.no_grad():
            for batch in val_loader:
                # Move data to device
                reporting_triangle = batch['reporting_triangle'].to(self.device)
                municipality_ids = batch['municipality_id'].squeeze().to(self.device)
                sex_ids = batch['sex_id'].squeeze().to(self.device)
                age_group_ids = batch['age_group_id'].squeeze().to(self.device)
                population_offset = batch['population_offset'].squeeze().to(self.device)
                targets = batch['target_births'].squeeze().to(self.device)
                
                # Forward pass
                dist = self.model(reporting_triangle, municipality_ids, sex_ids, age_group_ids, population_offset)
                
                # Compute loss
                loss = self.negative_binomial_loss(dist, targets)
                
                total_loss += loss.item()
                num_batches += 1
        
        return total_loss / num_batches
    
    def train(self, train_loader: DataLoader, val_loader: DataLoader, 
              num_epochs: int, learning_rate: float = 0.001, patience: int = 5) -> Dict[str, Any]:
        """
        Train the model.
        
        Args:
            train_loader: Training data loader
            val_loader: Validation data loader
            num_epochs: Number of epochs
            learning_rate: Learning rate
            patience: Early stopping patience
            
        Returns:
            training_history: Dictionary with training history
        """
        
        # Initialize optimizer
        self.optimizer = optim.Adam(self.model.parameters(), lr=learning_rate, weight_decay=1e-5)
        scheduler = optim.lr_scheduler.ReduceLROnPlateau(self.optimizer, mode='min', factor=0.5, patience=3)
        
        best_val_loss = float('inf')
        patience_counter = 0
        best_model_state = None
        
        print(f"Starting training for {num_epochs} epochs...")
        print(f"Learning rate: {learning_rate}, Patience: {patience}")
        
        for epoch in range(num_epochs):
            # Train
            train_loss = self.train_epoch(train_loader)
            
            # Validate
            val_loss = self.validate_epoch(val_loader)
            
            # Update learning rate scheduler
            scheduler.step(val_loss)
            
            # Record history
            self.history['train_loss'].append(train_loss)
            self.history['val_loss'].append(val_loss)
            
            # Print progress
            current_lr = self.optimizer.param_groups[0]['lr']
            print(f"Epoch {epoch+1:3d}/{num_epochs}: "
                  f"Train Loss: {train_loss:.4f}, "
                  f"Val Loss: {val_loss:.4f}, "
                  f"LR: {current_lr:.6f}")
            
            # Early stopping
            if val_loss < best_val_loss:
                best_val_loss = val_loss
                patience_counter = 0
                best_model_state = self.model.state_dict().copy()
                
                # Save checkpoint
                os.makedirs('./checkpoints', exist_ok=True)
                torch.save({
                    'epoch': epoch,
                    'model_state_dict': best_model_state,
                    'optimizer_state_dict': self.optimizer.state_dict(),
                    'train_loss': train_loss,
                    'val_loss': val_loss,
                    'history': self.history
                }, './checkpoints/best_model.pt')
                
            else:
                patience_counter += 1
                if patience_counter >= patience:
                    print(f"Early stopping triggered after {epoch+1} epochs")
                    break
        
        # Load best model
        if best_model_state is not None:
            self.model.load_state_dict(best_model_state)
            print(f"✓ Loaded best model (val_loss: {best_val_loss:.4f})")
        
        return {
            'best_val_loss': best_val_loss,
            'num_epochs_trained': epoch + 1,
            'history': self.history
        }
    
    def evaluate(self, test_loader: DataLoader) -> Dict[str, Any]:
        """
        Evaluate the model on test data.
        
        Returns:
            results: Dictionary with evaluation metrics
        """
        self.model.eval()
        
        all_predictions = []
        all_targets = []
        all_mean_predictions = []
        all_variances = []
        all_theta_params = []
        total_loss = 0.0
        num_batches = 0
        
        with torch.no_grad():
            for batch in test_loader:
                # Move data to device
                reporting_triangle = batch['reporting_triangle'].to(self.device)
                municipality_ids = batch['municipality_id'].squeeze().to(self.device)
                sex_ids = batch['sex_id'].squeeze().to(self.device)
                age_group_ids = batch['age_group_id'].squeeze().to(self.device)
                population_offset = batch['population_offset'].squeeze().to(self.device)
                targets = batch['target_births'].squeeze().to(self.device)
                
                # Forward pass
                dist = self.model(reporting_triangle, municipality_ids, sex_ids, age_group_ids, population_offset)
                
                # Compute loss
                loss = self.negative_binomial_loss(dist, targets)
                total_loss += loss.item()
                num_batches += 1
                
                # Collect predictions and statistics
                mean_pred = dist.mean.cpu().numpy()
                variance_pred = dist.variance.cpu().numpy()
                targets_np = targets.cpu().numpy()
                
                all_mean_predictions.extend(mean_pred)
                all_variances.extend(variance_pred)
                all_targets.extend(targets_np)
                
                # Sample from distribution for additional metrics
                samples = dist.sample().cpu().numpy()
                all_predictions.extend(samples)
                
                # Get alpha parameters for municipality analysis
                if hasattr(self.model, 'alpha_embedding'):
                    alpha_vals = torch.nn.functional.softplus(self.model.alpha_embedding(municipality_ids))
                    all_theta_params.extend(alpha_vals.cpu().numpy())
        
        # Convert to numpy arrays
        predictions = np.array(all_predictions)
        targets = np.array(all_targets)
        mean_predictions = np.array(all_mean_predictions)
        variances = np.array(all_variances)
        
        # Calculate metrics
        mae = np.mean(np.abs(mean_predictions - targets))
        mse = np.mean((mean_predictions - targets) ** 2)
        rmse = np.sqrt(mse)
        mape = np.mean(np.abs((targets - mean_predictions) / np.maximum(targets, 1e-8))) * 100
        
        # Overdispersion analysis
        mean_overdispersion = np.mean(variances / np.maximum(mean_predictions, 1e-8))
        theta_stats = {}
        if all_theta_params:
            theta_array = np.array(all_theta_params).flatten()
            theta_stats = {
                'mean': np.mean(theta_array),
                'std': np.std(theta_array),
                'min': np.min(theta_array),
                'max': np.max(theta_array),
                'median': np.median(theta_array)
            }
        
        results = {
            'test_loss': total_loss / num_batches,
            'mae': mae,
            'mse': mse,
            'rmse': rmse,
            'mape': mape,
            'mean_overdispersion': mean_overdispersion,
            'theta_statistics': theta_stats,
            'predictions': mean_predictions,
            'actuals': targets,
            'variances': variances,
            'sample_predictions': predictions  # Sampled predictions
        }
        
        return results


def main():
    """Run training with CORRECTED Negative Binomial distribution."""
    
    print("🎯 CORRECTED NEGATIVE BINOMIAL DISTRIBUTION TRAINING")
    print("=" * 80)
    print(f"Started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("Implementation: NegativeBinomial(μ, α_l) with Var = μ + α_l·μ²")
    print("where α_l is municipality-specific overdispersion parameter")
    
    # Configuration
    config = {
        'past_units': 36,
        'max_delay': 96,
        'min_births_threshold': 5,
        'batch_size': 32,
        'num_epochs': 25,  # Slightly more epochs for NB
        'learning_rate': 0.0005,  # Lower learning rate for stability
        'patience': 7,  # More patience for NB convergence
        'nowcast_cutoff_year': 2016,
        'current_date': '2023-12-31'
    }
    
    print("\nConfiguration:")
    for key, value in config.items():
        print(f"  {key}: {value}")
    
    # Step 1: Initialize data processor
    print(f"\n{'='*50}")
    print("STEP 1: DATA PROCESSING")
    print(f"{'='*50}")
    
    processor = BirthDataProcessor(
        past_units=config['past_units'],
        max_delay=config['max_delay'],
        min_births_threshold=config['min_births_threshold'],
        nowcast_cutoff_year=config['nowcast_cutoff_year'],
        current_date=pd.to_datetime(config['current_date'])
    )
    
    # Load data
    data_path = os.path.join('../../datasets', 'monthly_births.parquet')
    print(f"Loading data from: {data_path}")
    
    df = pd.read_parquet(data_path)
    print(f"Dataset shape: {df.shape}")
    
    print("Preprocessing data...")
    df_processed = processor.filter_reproductive_ages(df)

    print("Pre-threshold shape:", df_processed.shape)
    print("Nulls:", df_processed[['group_id','sex','age']].isna().sum().to_dict())
    print("Sample:", df_processed[['group_id','sex','age','births']].head().to_dict(orient='list'))

    df_processed = processor.apply_birth_threshold(df_processed)
    
    # Store data and fit encoders
    processor.data = df_processed
    processor.fit_label_encoders(df_processed)
    
    # Load population data
    processor.load_population_data()
    
    # IMPORTANT: Disable log transformation for Negative Binomial (we need raw counts)
    processor.enable_log_transform(False)
    print("✓ Log transformation disabled - using raw counts for Negative Binomial")
    
    # Calculate municipality baselines
    processor.calculate_municipality_baselines(df_processed)
    
    # Create reporting triangles (training period)
    triangles, group_ids, sex_codes, age_codes, pop_offsets = processor.create_reporting_triangles('training')
    print(f"Training samples: {len(triangles):,}")
    
    if len(triangles) == 0:
        print("❌ No training triangles created! Check data filtering.")
        return
    
    # Create data loaders
    from data_processing import create_data_loaders
    train_loader, val_loader = create_data_loaders(
        triangles=triangles,
        group_ids=group_ids, 
        sex_codes=sex_codes,
        age_codes=age_codes,
        population_offsets=pop_offsets,
        batch_size=config['batch_size'],
        use_log_transform=processor.use_log_transform  # Should be False
    )
    
    test_loader = val_loader  # Use validation as test for now
    
    print(f"Training batches: {len(train_loader)}")
    print(f"Validation batches: {len(val_loader)}")
    print(f"Test batches: {len(test_loader)}")
    
    # Get data info
    sample_batch = next(iter(train_loader))
    reporting_triangle = sample_batch['reporting_triangle']
    targets = sample_batch['target_births']
    
    print(f"\nData shapes:")
    print(f"  Reporting triangle: {reporting_triangle.shape}")
    print(f"  Raw count targets: {targets.shape}")
    print(f"  Target range: [{targets.min().item():.0f}, {targets.max().item():.0f}]")
    print(f"  Target mean: {targets.mean().item():.2f}")
    print(f"  Target data type: {targets.dtype}")
    
    # Step 2: Create Negative Binomial model
    print(f"\n{'='*50}")
    print("STEP 2: MODEL ARCHITECTURE")
    print(f"{'='*50}")
    
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")
    
    model = BirthNowcastPNN(
        past_units=config['past_units'],
        max_delay=config['max_delay'],
        n_municipalities=520,
        n_age_groups=12,
        hidden_units=[32, 16],
        embedding_dims={'municipality': 25, 'sex': 2, 'age': 8},
        dropout_probs=[0.2, 0.15]
    ).to(device)
    
    total_params = sum(p.numel() for p in model.parameters())
    trainable_params = sum(p.numel() for p in model.parameters() if p.requires_grad)
    
    print(f"Model: BirthNowcastPNN with Negative Binomial Distribution")
    print(f"Total parameters: {total_params:,}")
    print(f"Trainable parameters: {trainable_params:,}")
    print(f"Architecture:")
    print(f"  - Mean head: Predicts μ via exp(NN(X) + log(population))")
    print(f"  - Theta embedding: 520 municipality-specific θ_l parameters")
    print(f"  - Distribution: NegativeBinomial(μ, θ_l) with Var = μ(1 + θ_l)")
    
    # Step 3: Test forward pass
    print(f"\n{'='*50}")
    print("STEP 3: FORWARD PASS TEST")
    print(f"{'='*50}")
    
    # Test forward pass
    reporting_triangle = sample_batch['reporting_triangle'].to(device)
    municipality_ids = sample_batch['municipality_id'].squeeze().to(device)
    sex_ids = sample_batch['sex_id'].squeeze().to(device)
    age_group_ids = sample_batch['age_group_id'].squeeze().to(device)
    population_offset = sample_batch['population_offset'].squeeze().to(device)
    targets = sample_batch['target_births'].squeeze().to(device)
    
    print("Testing forward pass...")
    try:
        with torch.no_grad():
            dist = model(reporting_triangle, municipality_ids, sex_ids, age_group_ids, population_offset)
            
            predicted_mean = dist.mean
            predicted_variance = dist.variance
            
            print(f"✓ Forward pass successful!")
            print(f"  Distribution type: {type(dist)}")
            print(f"  Predicted mean shape: {predicted_mean.shape}")
            print(f"  Predicted mean range: [{predicted_mean.min().item():.2f}, {predicted_mean.max().item():.2f}]")
            print(f"  Predicted variance shape: {predicted_variance.shape}")
            print(f"  Predicted variance range: [{predicted_variance.min().item():.2f}, {predicted_variance.max().item():.2f}]")
            
            # Check overdispersion - CORRECTED formula: Var/μ = 1 + α_l·μ
            overdispersion = predicted_variance / predicted_mean
            print(f"  Overdispersion range: [{overdispersion.min().item():.3f}, {overdispersion.max().item():.3f}]")
            print(f"  Mean overdispersion: {overdispersion.mean().item():.3f}")
            
            # Test alpha parameters (renamed from theta)
            alpha_values = torch.nn.functional.softplus(model.alpha_embedding(municipality_ids))
            print(f"  Alpha parameters range: [{alpha_values.min().item():.6f}, {alpha_values.max().item():.6f}]")
            print(f"  (Formula: Var = μ + α_l·μ²)")
            
    except Exception as e:
        print(f"❌ Forward pass failed: {e}")
        return
    
    # Step 4: Initialize trainer
    print(f"\n{'='*50}")
    print("STEP 4: TRAINER & LOSS FUNCTION")
    print(f"{'='*50}")
    
    trainer = NegativeBinomialTrainer(model=model, device=device)
    
    print(f"✓ NegativeBinomialTrainer initialized")
    print(f"  Loss function: Negative log-likelihood of NegativeBinomial")
    print(f"  Optimizer: Adam with weight decay")
    print(f"  Learning rate scheduler: ReduceLROnPlateau")
    
    # Test loss computation
    try:
        with torch.no_grad():
            dist = model(reporting_triangle, municipality_ids, sex_ids, age_group_ids, population_offset)
            loss = trainer.negative_binomial_loss(dist, targets)
            print(f"  Test loss: {loss.item():.4f}")
            
            if torch.isfinite(loss):
                print(f"  ✓ Loss computation successful")
            else:
                print(f"  ❌ Loss is not finite: {loss.item()}")
                return
                
    except Exception as e:
        print(f"❌ Loss computation failed: {e}")
        return
    
    # Step 5: Training
    print(f"\n{'='*50}")
    print("STEP 5: TRAINING")
    print(f"{'='*50}")
    
    print("Starting training...")
    training_results = trainer.train(
        train_loader=train_loader,
        val_loader=val_loader,
        num_epochs=config['num_epochs'],
        learning_rate=config['learning_rate'],
        patience=config['patience']
    )
    
    print(f"\nTraining completed!")
    print(f"Best validation loss: {training_results['best_val_loss']:.4f}")
    print(f"Epochs trained: {training_results['num_epochs_trained']}")
    
    # Step 6: Evaluation
    print(f"\n{'='*50}")
    print("STEP 6: EVALUATION")
    print(f"{'='*50}")
    
    results = trainer.evaluate(test_loader)
    
    print("Test Results:")
    print(f"  Test Loss: {results['test_loss']:.4f}")
    print(f"  MAE: {results['mae']:.4f}")
    print(f"  RMSE: {results['rmse']:.4f}")
    print(f"  MAPE: {results['mape']:.2f}%")
    print(f"  Mean Overdispersion: {results['mean_overdispersion']:.3f}")
    
    # Theta parameter analysis
    if results['theta_statistics']:
        theta_stats = results['theta_statistics']
        print(f"\nTheta Parameter Analysis (Municipality Overdispersion):")
        print(f"  Mean: {theta_stats['mean']:.6f}")
        print(f"  Std: {theta_stats['std']:.6f}")
        print(f"  Range: [{theta_stats['min']:.6f}, {theta_stats['max']:.6f}]")
        print(f"  Median: {theta_stats['median']:.6f}")
    
    # Save complete model and results for nowcasting
    print(f"\n{'='*50}")
    print("STEP 7: SAVING MODEL & RESULTS")
    print(f"{'='*50}")
    
    # Save model with all metadata
    final_model_path = './checkpoints/negative_binomial_model_final.pt'
    torch.save({
        'model_state_dict': model.state_dict(),
        'model_config': {
            'past_units': config['past_units'],
            'max_delay': config['max_delay'],
            'n_municipalities': 520,
            'n_age_groups': 12,
            'hidden_units': [32, 16],
            'embedding_dims': {'municipality': 25, 'sex': 2, 'age': 8},
            'dropout_probs': [0.2, 0.15]
        },
        'training_config': config,
        'training_results': training_results,
        'test_results': results,
        'data_processor_state': {
            'municipality_encoder': processor.le_mun,
            'sex_encoder': processor.le_sex,
            'age_encoder': processor.le_age,
            'use_log_transform': processor.use_log_transform,
            'municipality_baselines': processor.municipality_baselines
        },
        'training_period': f"1990-{config['nowcast_cutoff_year']-1}",
        'model_type': 'negative_binomial',
        'distribution_formula': 'Var = μ(1 + θ_l)',
        'saved_at': datetime.now().isoformat()
    }, final_model_path)
    
    print(f"✅ Final model saved to: {final_model_path}")
    print(f"   📊 Training period: 1990-{config['nowcast_cutoff_year']-1}")
    print(f"   🎯 Model type: Negative Binomial with municipality-specific θ_l")
    print(f"   📈 Formula: Var = μ(1 + θ_l)")
    print(f"   📋 Test MAE: {results['mae']:.2f} births")
    print(f"   📋 Test RMSE: {results['rmse']:.2f} births")
    print(f"   📋 Mean Overdispersion: {results['mean_overdispersion']:.3f}x")
    
    # Save summary for easy access
    summary_path = './checkpoints/training_summary.txt'
    with open(summary_path, 'w') as f:
        f.write(f"NEGATIVE BINOMIAL BIRTH NOWCASTING MODEL\n")
        f.write(f"{'='*50}\n\n")
        f.write(f"Training completed: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"Training period: 1990-{config['nowcast_cutoff_year']-1}\n")
        f.write(f"Model architecture: BirthNowcastPNN with NegativeBinomial distribution\n")
        f.write(f"Municipality-specific parameters: 520 θ_l values\n")
        f.write(f"Variance formula: Var = μ(1 + θ_l)\n\n")
        f.write(f"PERFORMANCE METRICS:\n")
        f.write(f"Test Loss: {results['test_loss']:.4f}\n")
        f.write(f"MAE: {results['mae']:.2f} births\n")
        f.write(f"RMSE: {results['rmse']:.2f} births\n")
        f.write(f"MAPE: {results['mape']:.1f}%\n")
        f.write(f"Mean Overdispersion: {results['mean_overdispersion']:.3f}x\n\n")
        f.write(f"THETA PARAMETERS:\n")
        if results['theta_statistics']:
            theta_stats = results['theta_statistics']
            f.write(f"Mean: {theta_stats['mean']:.6f}\n")
            f.write(f"Std: {theta_stats['std']:.6f}\n")
            f.write(f"Range: [{theta_stats['min']:.6f}, {theta_stats['max']:.6f}]\n")
        f.write(f"\nMODEL FILES:\n")
        f.write(f"Final model: {final_model_path}\n")
        f.write(f"Best checkpoint: ./checkpoints/best_model.pt\n")
    
    print(f"✅ Training summary saved to: {summary_path}")
    print(f"\n🎯 MODEL READY FOR NOWCASTING!")
    print(f"   Use the saved model for validation and nowcast predictions")
    
    print(f"\n✅ Training completed at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    return results


if __name__ == "__main__":
    main()
