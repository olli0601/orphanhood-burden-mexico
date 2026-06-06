# =============================================================================
# ch4_320_nowcasting.R  ·  Chapter 4 — Delay-adjusted nowcasting
# NIMBLE-based nowcasting exploration.
# Reads input-data-processed/mort.RDS.
# =============================================================================

library(dplyr)
library(tidyr)
library(keras)

deaths_df <- readRDS("input-data-processed/mort.RDS")      

delay_matrix <- deaths_df %>%
  group_by(year, delay) %>%
  summarise(deaths = sum(tot_deaths), .groups = "drop") %>%
  pivot_wider(names_from = delay, values_from = deaths, values_fill = 0) %>%
  arrange(year)

# Convert to matrix
delay_array <- as.matrix(delay_matrix[,-1])
timesteps <- nrow(delay_array)
delay_steps <- ncol(delay_array)
window_size <- 3

X <- array(0, dim = c(timesteps - window_size, window_size, delay_steps))
y <- array(0, dim = c(timesteps - window_size, delay_steps))

for (i in 1:(timesteps - window_size)) {
  X[i,,] <- delay_array[i:(i + window_size - 1), ]
  y[i,] <- delay_array[i + window_size, ]
}

model <- keras_model_sequential() %>%
  layer_lstm(units = 64, input_shape = c(window_size, delay_steps), return_sequences = FALSE) %>%
  layer_dense(units = delay_steps)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam()
)

model %>% fit(
  x = X,
  y = y,
  epochs = 100,
  batch_size = 1,
  validation_split = 0.2
)


pred <- model %>% predict(X)

