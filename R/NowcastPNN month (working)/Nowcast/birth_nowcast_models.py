"""
NowcastPNN Models for Birth Count Prediction
Adapted for Mexican birth registration data with multiple categorical variables
"""

import torch
import torch.nn as nn


class BirthNowcastPNN(nn.Module):
    """
    Probabilistic Neural Network for nowcasting birth counts.
    
    This model handles multiple categorical variables:
    - Municipality (group_id)
    - Sex (male/female) 
    - Age groups (5-year intervals)
    
    Architecture:
    1. Embedding layers for categorical variables
    2. Attention mechanism for temporal patterns
    3. Convolutional layers for delay patterns
    4. Fully connected layers for final prediction
    5. Negative Binomial output distribution
    """
    
    def __init__(self, 
                 past_units=36,           # Number of past months to consider
                 max_delay=96,            # Maximum delay in months (8 years)
                 n_municipalities=520,    # Number of unique municipalities in Mexico
                 n_age_groups=12,         # Number of age groups (pre-calculated in data)
                 hidden_units=[32, 16],   # Hidden layer dimensions
                 conv_channels=[32, 16, 1], # Convolutional channels
                 embedding_dims={         # Embedding dimensions for categorical variables
                     'municipality': 25,  # ~520/20 for municipalities
                     'sex': 2,
                     'age': 8
                 },
                 dropout_probs=[0.2, 0.15],  # Dropout probabilities
                 load_embeddings=False):     # Whether to load pre-trained embeddings
        
        super().__init__()
        
        # Store dimensions
        self.past_units = past_units
        self.max_delay = max_delay
        self.final_dim = past_units
        
        # Embedding layers for categorical variables
        self.embed_municipality = nn.Embedding(n_municipalities, embedding_dims['municipality'])
        self.embed_sex = nn.Embedding(2, embedding_dims['sex'])  # 0: female, 1: male
        self.embed_age = nn.Embedding(n_age_groups, embedding_dims['age'])
        
        # Calculate total embedding dimension
        total_embed_dim = sum(embedding_dims.values())
        
        # Embedding processing layers
        self.fc_embed1 = nn.Linear(total_embed_dim, 2 * total_embed_dim)
        self.fc_embed2 = nn.Linear(2 * total_embed_dim, past_units)
        self.bnorm_embed = nn.BatchNorm1d(2 * total_embed_dim)
        
        # Attention mechanism for temporal patterns
        self.attn1 = nn.MultiheadAttention(embed_dim=self.max_delay, num_heads=4, batch_first=True)
        self.fc_attn = nn.Linear(self.past_units, self.past_units)
        
        # Convolutional layers for delay pattern extraction
        self.conv1 = nn.Conv1d(self.max_delay, conv_channels[0], kernel_size=7, padding="same")
        self.conv2 = nn.Conv1d(conv_channels[0], conv_channels[1], kernel_size=7, padding="same")
        self.conv3 = nn.Conv1d(conv_channels[1], conv_channels[2], kernel_size=7, padding="same")
        
        # Batch normalization for convolutional layers
        self.bnorm_conv1 = nn.BatchNorm1d(self.max_delay)
        self.bnorm_conv2 = nn.BatchNorm1d(conv_channels[0])
        self.bnorm_conv3 = nn.BatchNorm1d(conv_channels[1])
        
        # Fully connected layers
        self.fc1 = nn.Linear(self.final_dim, hidden_units[0])
        self.fc2 = nn.Linear(hidden_units[0], hidden_units[1])
        
        # CORRECTED NEGATIVE BINOMIAL: Mean head + municipality-specific alpha parameters
        # Formula: Var = μ + α_l·μ² (quadratic variance growth)
        self.mean_head = nn.Linear(hidden_units[-1], 1)     # log μ - mean in log-space
        self.alpha_embedding = nn.Embedding(n_municipalities, 1)  # α_l per municipality (overdispersion)
        
        # Batch normalization for FC layers
        self.bnorm_fc1 = nn.BatchNorm1d(self.final_dim)
        self.bnorm_fc2 = nn.BatchNorm1d(hidden_units[0])
        self.bnorm_final = nn.BatchNorm1d(hidden_units[-1])
        
        # Dropout layers
        self.dropout1 = nn.Dropout(dropout_probs[0])
        self.dropout2 = nn.Dropout(dropout_probs[1])
        
        # Activation functions
        self.act = nn.SiLU()  # Swish activation
        self.softplus = nn.Softplus()
        
        # Scaling constant for birth counts
        self.const = 1000.0  # Adjust based on typical birth count scale
        
        # Initialize embeddings if specified
        if load_embeddings:
            self._load_pretrained_embeddings()
        
        # Initialize alpha embeddings to small positive values for near-Poisson start
        # log(α) ≈ -3 → α ≈ 0.05 (small overdispersion, near Poisson initially)
        nn.init.constant_(self.alpha_embedding.weight, -3.0)
        
        # Initialize mean head normally
        nn.init.normal_(self.mean_head.weight, 0, 0.01)
    
    def _load_pretrained_embeddings(self):
        """Load pre-trained embeddings if available."""
        try:
            # Try to load municipality embeddings
            muni_weights = torch.load("./weights/municipality_embeddings.pt", map_location='cpu')
            self.embed_municipality.weight.data = muni_weights
            print("✓ Loaded pre-trained municipality embeddings")
        except FileNotFoundError:
            print("! Municipality embeddings not found, using random initialization")
    
    def save_embeddings(self):
        """Save trained embeddings for future use."""
        import os
        os.makedirs("./weights", exist_ok=True)
        torch.save(self.embed_municipality.weight.data, "./weights/municipality_embeddings.pt")
        print("✓ Embeddings saved to ./weights/")
    
    def forward(self, reporting_triangle, municipality_ids, sex_ids, age_group_ids, population_offset=None):
        """
        Forward pass with population offset support.
        
        Args:
            reporting_triangle: Tensor of shape [batch, past_units, max_delay]
            municipality_ids: Tensor of shape [batch] with municipality indices
            sex_ids: Tensor of shape [batch] with sex indices (0=female, 1=male)
            age_group_ids: Tensor of shape [batch] with age group indices
            population_offset: Tensor of shape [batch] with log population offsets (optional)
            
        Returns:
            Independent Negative Binomial distribution
        """
        
        # Process reporting triangle
        x = reporting_triangle.float()
        batch_size = x.size(0)
        
        # === EMBEDDING PROCESSING ===
        # Handle scalar inputs by converting to batch format
        if municipality_ids.dim() == 0:
            municipality_ids = municipality_ids.unsqueeze(0)
        if sex_ids.dim() == 0:
            sex_ids = sex_ids.unsqueeze(0)
        if age_group_ids.dim() == 0:
            age_group_ids = age_group_ids.unsqueeze(0)
        
        # Get embeddings
        embed_muni = self.embed_municipality(municipality_ids)
        embed_sex = self.embed_sex(sex_ids)
        embed_age = self.embed_age(age_group_ids)
        
        # Concatenate all embeddings
        embeddings = torch.cat([embed_muni, embed_sex, embed_age], dim=1)
        
        # Process embeddings through FC layers
        embed_processed = self.act(self.fc_embed1(embeddings))
        embed_processed = self.bnorm_embed(embed_processed)
        embed_processed = self.act(self.fc_embed2(embed_processed))
        
        # === ATTENTION MECHANISM ===
        x_residual = x.clone()
        
        # Self-attention on temporal dimension
        x_attn, _ = self.attn1(x, x, x, need_weights=False)
        
        # Process through FC layer and add residual connection
        x_attn = self.act(self.fc_attn(x_attn.permute(0, 2, 1)))
        x = x_attn.permute(0, 2, 1) + x_residual
        
        # === CONVOLUTIONAL PROCESSING ===
        # Reshape for convolution: [batch, max_delay, past_units]
        x = x.permute(0, 2, 1)
        
        # Apply convolutional layers with batch norm and activation
        x = self.act(self.conv1(self.bnorm_conv1(x)))
        x = self.act(self.conv2(self.bnorm_conv2(x)))
        x = self.act(self.conv3(self.bnorm_conv3(x)))
        
        # Remove the delay dimension (should be 1 after final conv)
        x = torch.squeeze(x, dim=1)
        
        # === ADD CATEGORICAL INFORMATION ===
        # Add processed embeddings to the main features
        x = x + embed_processed
        
        # === FULLY CONNECTED LAYERS ===
        x = self.dropout1(x)
        x = self.act(self.fc1(self.bnorm_fc1(x)))
        
        x = self.dropout2(x)
        features = self.act(self.fc2(self.bnorm_fc2(x)))  # Final feature representation
        
        # === CORRECTED NEGATIVE BINOMIAL OUTPUT ===
        # FORMULA: Var = μ + α_l·μ² (quadratic variance growth)
        
        # Mean prediction in log-space
        log_birth_rate = self.mean_head(self.bnorm_final(features)).squeeze(-1)     # log μ
        
        # Add population offset to mean if provided (GLM-style)
        if population_offset is not None:
            log_births_mean = log_birth_rate + population_offset  # log(μ) = log_rate + log(pop)
        else:
            log_births_mean = log_birth_rate
            
        # Convert to count-space mean with floor for numerical stability
        births_mean = torch.clamp(torch.exp(log_births_mean), min=1e-8)  # μ ≥ 1e-8
        
        # Get municipality-specific overdispersion parameters α_l
        log_alpha = self.alpha_embedding(municipality_ids).squeeze(-1)  # log(α_l)
        alpha_l = self.softplus(log_alpha) + 1e-6  # α_l > 0, ensure numerical stability
        
        # CORRECTED Negative Binomial variance: Var = μ + α_l·μ²
        births_variance = births_mean + alpha_l * births_mean * births_mean
        
        # CORRECTED Convert to PyTorch NB parameterization
        # Formula: Var = μ + α_l·μ²
        # NB parameterization: r = 1/α_l (independent of μ), p = r/(r + μ)
        r = 1.0 / (alpha_l + 1e-8)           # total_count
        p = births_mean / (r + births_mean)
        total_count = torch.clamp(r, 1e-6, 1e6)
        probs = torch.clamp(p, 1e-6, 1 - 1e-6)
        dist = torch.distributions.NegativeBinomial(total_count=total_count, probs=probs)


        
        # Clamp parameters for numerical stability
        total_count = torch.clamp(r, min=1e-6, max=1e6)
        probs = torch.clamp(p, min=1e-6, max=1.0 - 1e-6)
        
        # Create Negative Binomial distribution
        # This models: births ~ NB with Var = μ + α_l·μ²
        dist = torch.distributions.NegativeBinomial(total_count=total_count, probs=probs)

        # Return as independent distribution
        return torch.distributions.Independent(dist, reinterpreted_batch_ndims=1)


class SimpleBirthNowcastPNN(nn.Module):
    """
    Simplified version of BirthNowcastPNN for faster experimentation.
    
    This version has fewer parameters and simpler architecture while maintaining
    the core functionality for birth count nowcasting.
    """
    
    def __init__(self, 
                 past_units=36,
                 max_delay=96,
                 n_municipalities=520,    # Updated for Mexico's 520 municipalities
                 n_age_groups=12,
                 embedding_dims={'municipality': 64, 'sex': 2, 'age': 8},  # Increased municipality: 20->64, age: 6->8
                 hidden_units=[16, 8]):
        
        super().__init__()
        
        self.past_units = past_units
        self.max_delay = max_delay
        
        # Simplified embeddings
        self.embed_municipality = nn.Embedding(n_municipalities, embedding_dims['municipality'])
        self.embed_sex = nn.Embedding(2, embedding_dims['sex'])
        self.embed_age = nn.Embedding(n_age_groups, embedding_dims['age'])
        
        total_embed_dim = sum(embedding_dims.values())
        
        # Single embedding processing layer
        self.fc_embed = nn.Linear(total_embed_dim, past_units)
        
        # Simplified convolutional processing
        self.conv1 = nn.Conv1d(max_delay, 16, kernel_size=5, padding="same")
        self.conv2 = nn.Conv1d(16, 1, kernel_size=5, padding="same")
        
        # Simplified FC layers
        self.fc1 = nn.Linear(past_units, hidden_units[0])
        self.fc2 = nn.Linear(hidden_units[0], hidden_units[1])
        
        # CORRECTED NEGATIVE BINOMIAL: Mean head + municipality-specific alpha parameters
        self.mean_head = nn.Linear(hidden_units[1], 1)     # log μ - mean in log-space
        self.alpha_embedding = nn.Embedding(n_municipalities, 1)  # α_l per municipality (overdispersion)
        
        # Activations and regularization
        self.act = nn.ReLU()
        self.softplus = nn.Softplus()
        self.dropout = nn.Dropout(0.1)
        
        self.const = 100.0
        
        # Initialize alpha embeddings to small positive values for near-Poisson start
        nn.init.constant_(self.alpha_embedding.weight, -3.0)  # log(α) ≈ -3 → α ≈ 0.05
        nn.init.normal_(self.mean_head.weight, 0, 0.01)
    
    def forward(self, reporting_triangle, municipality_ids, sex_ids, age_group_ids, population_offset=None):
        """Simplified forward pass with population offset support."""
        
        x = reporting_triangle.float()
        
        # Process embeddings
        embed_muni = self.embed_municipality(municipality_ids)
        embed_sex = self.embed_sex(sex_ids)
        embed_age = self.embed_age(age_group_ids)
        
        embeddings = torch.cat([embed_muni, embed_sex, embed_age], dim=1)
        embed_processed = self.act(self.fc_embed(embeddings))
        
        # Convolutional processing
        x = x.permute(0, 2, 1)  # [batch, max_delay, past_units]
        x = self.act(self.conv1(x))
        x = self.act(self.conv2(x))
        x = torch.squeeze(x, dim=1)
        
        # Add embeddings
        x = x + embed_processed
        
        # FC layers
        x = self.dropout(x)
        x = self.act(self.fc1(x))
        x = self.dropout(x)
        features = self.act(self.fc2(x))  # Final feature representation
        
        # === CORRECTED NEGATIVE BINOMIAL OUTPUT ===
        # FORMULA: Var = μ + α_l·μ² (quadratic variance growth)
        
        # Mean prediction in log-space
        log_birth_rate = self.mean_head(features).squeeze(-1)     # log μ
        
        # Add population offset to mean if provided (GLM-style)
        if population_offset is not None:
            log_births_mean = log_birth_rate + population_offset  # log(μ) = log_rate + log(pop)
        else:
            log_births_mean = log_birth_rate
            
        # Convert to count-space mean with floor for numerical stability
        births_mean = torch.clamp(torch.exp(log_births_mean), min=1e-8)  # μ ≥ 1e-8
        
        # Get municipality-specific overdispersion parameters α_l
        log_alpha = self.alpha_embedding(municipality_ids).squeeze(-1)  # log(α_l)
        alpha_l = self.softplus(log_alpha) + 1e-6  # α_l > 0
        
        # CORRECTED Negative Binomial variance: Var = μ + α_l·μ²
        births_variance = births_mean + alpha_l * births_mean * births_mean
        
        # CORRECTED Convert to PyTorch NB parameterization
        r = 1.0 / (alpha_l + 1e-8)
        p = births_mean / (r + births_mean)

        # Clamp for numerical stability
        total_count = torch.clamp(r, min=1e-6, max=1e6)
        probs = torch.clamp(p, min=1e-6, max=1.0 - 1e-6)
        
        # Create Negative Binomial distribution
        dist = torch.distributions.NegativeBinomial(total_count=total_count, probs=probs)
        
        return torch.distributions.Independent(dist, reinterpreted_batch_ndims=1)
