# Birth Nowcasting with Probabilistic Neural Networks

This project implements a nowcasting system for birth counts in Mexico using a modified version of the Probabilistic Neural Network (PNN) architecture. The system predicts the total number of births that have occurred but haven't yet been reported, accounting for reporting delays and various demographic factors.

## Project Overview

### Problem Statement
Birth registration in Mexico can have significant delays. This system aims to:
- Estimate the true number of births that occurred in recent months
- Account for reporting delays up to 8 years (96 months)
- Provide uncertainty quantification for predictions
- Handle multiple demographic stratifications (municipality, sex, age groups)

### Key Features
- **Probabilistic predictions**: Uses Negative Binomial distributions for uncertainty quantification
- **Multi-dimensional modeling**: Handles municipality (520 municipalities), sex, and age group variables
- **Attention mechanism**: Captures temporal patterns in birth data
- **Convolutional processing**: Extracts delay patterns from reporting triangles
- **Embedding layers**: Efficiently handles high-cardinality categorical variables
- **Pre-calculated age groups**: Works with existing age group classifications in the data

## Architecture

### Model Components

1. **Reporting Triangles**: Matrix of size [past_units, max_delay] containing birth counts by occurrence month and reporting delay
2. **Embedding Layers**: Convert categorical variables (municipality, sex, age) to dense vectors
3. **Attention Mechanism**: Multi-head attention to capture temporal dependencies
4. **Convolutional Layers**: Extract delay patterns from reporting triangles
5. **Fully Connected Layers**: Final prediction layers
6. **Negative Binomial Output**: Probabilistic output distribution

### Data Processing Pipeline

```
Raw Birth Data
     ↓
Data Cleaning & Validation
     ↓
Age Group Filtering
     ↓
Label Encoding (Municipality, Sex, Age)
     ↓
Reporting Triangle Construction
     ↓
Train/Val/Test Split
     ↓
PyTorch DataLoaders
```

## File Structure

```
├── negative_binomial.py      # Negative Binomial distribution implementation
├── birth_nowcast_models.py   # Neural network model definitions
├── data_processing.py        # Data preprocessing and triangle creation
├── training.py              # Training and evaluation utilities
├── quick_test.py             # Quick testing script for framework validation
├── full_training.py          # Complete dataset training script
├── requirements.txt          # Python package dependencies
├── Untitled-1.ipynb         # Main analysis notebook
└── README.md                # This file
```

## Model Variants

### SimpleBirthNowcastPNN
- Lightweight version for experimentation
- Fewer parameters and simpler architecture
- Good starting point for proof of concept

### BirthNowcastPNN
- Full-featured model with more complex architecture
- Additional attention mechanisms and layer normalization
- Better performance but requires more computational resources

## Usage

### 1. Data Preparation
```python
from data_processing import BirthDataProcessor

processor = BirthDataProcessor(
    past_units=36,          # 3 years of history
    max_delay=96,           # 8 years max delay
    min_births_threshold=5   # Minimum births per group
)

df_processed = processor.load_and_preprocess_data("path/to/monthly_births.parquet")
```

### 2. Model Training
```python
from birth_nowcast_models import SimpleBirthNowcastPNN
from training import BirthNowcastTrainer

model = SimpleBirthNowcastPNN(
    past_units=36,
    max_delay=96,
    n_municipalities=520,  # Mexico's municipalities
    n_age_groups=processor.n_age_groups
)

trainer = BirthNowcastTrainer(model)
history = trainer.train(train_loader, val_loader, num_epochs=50)
```

### 3. Evaluation
```python
test_metrics = trainer.evaluate(test_loader)
print(f"MAE: {test_metrics['mae']:.2f} births")
print(f"95% Coverage: {test_metrics['coverage_95']:.3f}")
```

## Key Parameters

### Data Processing
- `past_units`: Number of past months to include in triangles (default: 36)
- `max_delay`: Maximum reporting delay in months (default: 96)
- `min_births_threshold`: Minimum births to include a demographic group (default: 5)

### Model Architecture
- `embedding_dims`: Dictionary specifying embedding dimensions for categorical variables
- `hidden_units`: List of hidden layer sizes
- `conv_channels`: Convolutional layer channel sizes
- `dropout_probs`: Dropout probabilities for regularization

### Training
- `learning_rate`: Adam optimizer learning rate (default: 1e-3)
- `weight_decay`: L2 regularization strength (default: 1e-5)
- `patience`: Early stopping patience (default: 10)
- `batch_size`: Training batch size (default: 32)

## Evaluation Metrics

- **MAE**: Mean Absolute Error in birth count predictions
- **RMSE**: Root Mean Square Error
- **MAPE**: Mean Absolute Percentage Error
- **Coverage**: Proportion of actual values within 95% prediction intervals
- **Uncertainty**: Average prediction uncertainty (standard deviation)

## Output

The model produces a Negative Binomial distribution for each prediction, providing:
- **Mean prediction**: Expected number of births
- **Uncertainty**: Standard deviation of the prediction
- **Full distribution**: Can sample from the distribution for probabilistic forecasts

## Requirements

- Python 3.8+
- PyTorch 1.12+
- pandas
- numpy
- scikit-learn
- matplotlib
- seaborn
- pyarrow (for parquet file support)
- fastparquet (alternative parquet engine)

### Installation

1. Install the required packages:
```bash
pip install -r requirements.txt
```

Or install manually:
```bash
pip install torch pandas numpy scikit-learn matplotlib seaborn pyarrow fastparquet
```

## Notes

### Data Requirements
The input dataset should have columns:
- `group_id`: Municipality identifier
- `sex`: Gender ("male"/"female")
- `age`: Age group as strings (e.g., "15-19", "20-24", "25-29", etc.)
- `year_occ`, `month_occ`: Occurrence date
- `year_reg`, `month_reg`: Registration date
- `delay`: Reporting delay in months (optional, will be calculated if missing)

**Note**: Age groups are filtered to reproductive ages:
- Females: 15-44 years (age groups: "15-19" through "40-44")
- Males: 15-69 years (age groups: "15-19" through "65-69")

### Memory Considerations
- Large datasets may require batching strategies
- Consider using the SimpleBirthNowcastPNN model for initial experiments
- Adjust `batch_size` based on available GPU memory

### Reproducibility
- Set random seeds for reproducible results
- Save model checkpoints during training
- Store label encoders for consistent categorical variable mapping

## Future Enhancements

1. **Hierarchical modeling**: Share parameters across similar municipalities
2. **Seasonal patterns**: Explicit modeling of birth seasonality
3. **External features**: Incorporate socioeconomic and geographic variables
4. **Real-time updates**: Streaming inference for new data
5. **Ensemble methods**: Combine multiple models for improved predictions

## Quick Testing

To quickly test the framework with a subset of data:

```bash
python quick_test.py
```

This will run a fast validation of the entire pipeline with reduced parameters.

## Full Dataset Training

To train on the complete dataset, you have several options:

### Option 1: Using the Jupyter Notebook (Recommended)
1. Open the notebook:
```bash
jupyter notebook Untitled-1.ipynb
```

2. Run all cells sequentially, or execute them one by one to monitor progress.

### Option 2: Using Python Script
Create and run a training script:

```python
# full_training.py
from data_processing import BirthDataProcessor, create_data_loaders
from birth_nowcast_models import SimpleBirthNowcastPNN, BirthNowcastPNN
from training import BirthNowcastTrainer
import torch

# Process full dataset
processor = BirthDataProcessor(
    past_units=36,
    max_delay=96,
    min_births_threshold=10  # Higher threshold for full dataset
)

print("Loading and processing full dataset...")
df_processed = processor.load_and_preprocess_data("../../datasets/monthly_births.parquet")

# Create label encoders and triangles
processor.fit_encoders(df_processed)
df_encoded = processor.transform_encoders(df_processed)
triangles_data = processor.create_reporting_triangles(df_encoded)

# Create data loaders
train_loader, val_loader, test_loader = create_data_loaders(
    triangles_data,
    train_ratio=0.7,
    val_ratio=0.15,
    batch_size=64,  # Larger batch size for full training
    shuffle=True
)

# Initialize full model
model = BirthNowcastPNN(  # Use full model for best performance
    past_units=processor.past_units,
    max_delay=processor.max_delay,
    n_municipalities=processor.n_municipalities,
    n_age_groups=processor.n_age_groups
)

# Train model
trainer = BirthNowcastTrainer(model, device='auto')
history = trainer.train(
    train_loader=train_loader,
    val_loader=val_loader,
    num_epochs=100,
    learning_rate=1e-3,
    patience=15,
    print_every=5
)

# Evaluate
test_metrics = trainer.evaluate(test_loader)
print(f"Final Test MAE: {test_metrics['mae']:.2f}")

# Save final model
torch.save({
    'model_state_dict': model.state_dict(),
    'processor': processor,
    'test_metrics': test_metrics
}, './final_birth_nowcast_model.pt')
```

Then run:
```bash
python full_training.py
```

### Option 3: Command Line Training
For advanced users, modify parameters directly:

```bash
# Set larger memory limits if needed
export PYTORCH_CUDA_ALLOC_CONF=max_split_size_mb:512

# Run with GPU if available
python -c "
from data_processing import *
from birth_nowcast_models import *
from training import *
# ... (insert training code)
"
```

### Recommended Settings for Full Dataset

**For SimpleBirthNowcastPNN (faster):**
- `batch_size=64`
- `num_epochs=50-100`
- `learning_rate=1e-3`
- `min_births_threshold=10`

**For BirthNowcastPNN (better performance):**
- `batch_size=32`
- `num_epochs=100-200`
- `learning_rate=5e-4`
- `min_births_threshold=15`

### Expected Training Time
- **SimpleBirthNowcastPNN**: 2-4 hours (CPU), 30-60 minutes (GPU)
- **BirthNowcastPNN**: 4-8 hours (CPU), 1-2 hours (GPU)

### Memory Requirements
- **Minimum**: 8GB RAM
- **Recommended**: 16GB RAM + GPU with 4GB+ VRAM
- **Large dataset**: Consider reducing `batch_size` if you get out-of-memory errors
