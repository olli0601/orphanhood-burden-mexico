library(dplyr)
library(ggplot2)
library(MASS)   # per glm.nb
library(broom)  # per tidy()
library(mgcv)   # per GAM
library(tidyr)  # per pivot_longer

# =============================================================================
# EVALUATION AND FIT FOR TOP-20 AND BOTTOM-20 MUNICIPALITIES
# Based on complete data model performance
# =============================================================================

cat("=== EVALUATION OF TOP-20 AND BOTTOM-20 MUNICIPALITIES ===\n")
cat("Loading data and models...\n")

# Load validation predictions (which contain observed vs predicted data)
if (!file.exists("validation_predictions_corrected.RDS")) {
  stop("Error: validation_predictions_corrected.RDS not found.")
}

validation_data <- readRDS("validation_predictions_corrected.RDS")
cat("Loaded validation predictions data:", nrow(validation_data), "rows\n")

# Also load test predictions if available
if (file.exists("test_predictions_corrected.RDS")) {
  test_data <- readRDS("test_predictions_corrected.RDS")
  cat("Loaded test predictions data:", nrow(test_data), "rows\n")
  
  # Combine validation and test data for comprehensive evaluation
  all_predictions <- bind_rows(
    validation_data %>% mutate(dataset = "validation"),
    test_data %>% mutate(dataset = "test")
  )
} else {
  all_predictions <- validation_data %>% mutate(dataset = "validation")
}

cat("Total predictions available:", nrow(all_predictions), "rows\n")

# Data is already in the format we need from validation/test predictions
# Standardize column names to match our expected format
all_predictions <- all_predictions %>%
  mutate(
    municipio = municipality,
    year = event_year,
    age_band = age_group,
    observed = events,
    predicted = predicted_corrected  # Use bias-corrected predictions
  ) %>%
  dplyr::select(municipio, year, age_band, sex, observed, predicted, dataset)

cat("Predictions available for", length(unique(all_predictions$municipio)), "municipalities\n")

# =============================================================================
# OVERALL MODEL EVALUATION METRICS
# =============================================================================

cat("\n=== OVERALL MODEL EVALUATION METRICS ===\n")

eval_metrics <- all_predictions %>%
  summarise(
    Municipalities = n_distinct(municipio),
    Total_Observations = n(),
    Correlation = cor(observed, predicted, use = "complete.obs"),
    RMSE = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(observed - predicted), na.rm = TRUE),
    Mean_Observed = mean(observed, na.rm = TRUE),
    Mean_Predicted = mean(predicted, na.rm = TRUE),
    Bias_Percentage = 100 * (mean(predicted, na.rm = TRUE) - mean(observed, na.rm = TRUE)) / mean(observed, na.rm = TRUE),
    PearsonChi2 = sum((observed - predicted)^2 / pmax(predicted, 1), na.rm = TRUE),
    .groups = "drop"
  )

print(eval_metrics)

# =============================================================================
# IDENTIFY TOP-20 AND BOTTOM-20 MUNICIPALITIES
# =============================================================================

cat("\n=== IDENTIFYING TOP-20 AND BOTTOM-20 MUNICIPALITIES ===\n")

# Calculate total births by municipality across all years
municipality_totals <- all_predictions %>%
  group_by(municipio) %>%
  summarise(
    total_observed = sum(observed, na.rm = TRUE),
    total_predicted = sum(predicted, na.rm = TRUE),
    years_available = n_distinct(year),
    observations = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_observed))

# Top-20 largest municipalities
top20_municipalities <- municipality_totals %>%
  slice_head(n = 20)

cat("Top-20 largest municipalities (by total births):\n")
print(top20_municipalities %>% dplyr::select(municipio, total_observed, years_available))

# Bottom-20 smallest municipalities
bottom20_municipalities <- municipality_totals %>%
  slice_tail(n = 20)

cat("\nBottom-20 smallest municipalities (by total births):\n")
print(bottom20_municipalities %>% dplyr::select(municipio, total_observed, years_available))

# =============================================================================
# EXTRACT DATA FOR TOP-20 AND BOTTOM-20
# =============================================================================

df_top20 <- all_predictions %>%
  semi_join(top20_municipalities, by = "municipio")

df_bottom20 <- all_predictions %>%
  semi_join(bottom20_municipalities, by = "municipio")

cat("\nTop-20 data:", nrow(df_top20), "observations\n")
cat("Bottom-20 data:", nrow(df_bottom20), "observations\n")

# =============================================================================
# EVALUATION METRICS FOR TOP-20 AND BOTTOM-20
# =============================================================================

cat("\n=== EVALUATION METRICS COMPARISON ===\n")

# Top-20 metrics
metrics_top20 <- df_top20 %>%
  summarise(
    Group = "Top-20 Largest",
    Municipalities = n_distinct(municipio),
    Observations = n(),
    Correlation = cor(observed, predicted, use = "complete.obs"),
    RMSE = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(observed - predicted), na.rm = TRUE),
    Mean_Observed = mean(observed, na.rm = TRUE),
    Mean_Predicted = mean(predicted, na.rm = TRUE),
    Bias_Percentage = 100 * (mean(predicted, na.rm = TRUE) - mean(observed, na.rm = TRUE)) / mean(observed, na.rm = TRUE)
  )

# Bottom-20 metrics
metrics_bottom20 <- df_bottom20 %>%
  summarise(
    Group = "Bottom-20 Smallest",
    Municipalities = n_distinct(municipio),
    Observations = n(),
    Correlation = cor(observed, predicted, use = "complete.obs"),
    RMSE = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
    MAE = mean(abs(observed - predicted), na.rm = TRUE),
    Mean_Observed = mean(observed, na.rm = TRUE),
    Mean_Predicted = mean(predicted, na.rm = TRUE),
    Bias_Percentage = 100 * (mean(predicted, na.rm = TRUE) - mean(observed, na.rm = TRUE)) / mean(observed, na.rm = TRUE)
  )

# Combined comparison
comparison_metrics <- bind_rows(metrics_top20, metrics_bottom20)
print(comparison_metrics)

# =============================================================================
# VISUALIZATION: OBSERVED vs PREDICTED FOR TOP-20
# =============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# Aggregate by municipality and year for cleaner visualization
df_top20_annual <- df_top20 %>%
  group_by(municipio, year) %>%
  summarise(
    observed = sum(observed, na.rm = TRUE),
    predicted = sum(predicted, na.rm = TRUE),
    .groups = "drop"
  )

# Plot observed vs predicted for Top-20
plot_top20 <- ggplot(df_top20_annual, aes(x = year)) +
  geom_point(aes(y = observed, color = "Observed"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted"), linewidth = 0.8) +
  facet_wrap(~ municipio, scales = "free_y", ncol = 4) +
  labs(
    title = "Observed vs Predicted Births - Top 20 Largest Municipalities",
    subtitle = "Complete Data 1990-2015 | Municipality-Specific GAM Models",
    x = "Year",
    y = "Annual Births",
    color = "Data Type"
  ) +
  scale_color_manual(values = c("Observed" = "#E91E63", "Predicted" = "#2196F3")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6),
    legend.position = "bottom"
  )

# Save top-20 plot
ggsave("evaluation_top20_municipalities.png", plot_top20, 
       width = 12, height = 10, dpi = 300, bg = "white")
cat("Saved: evaluation_top20_municipalities.png\n")

# =============================================================================
# VISUALIZATION: OBSERVED vs PREDICTED FOR BOTTOM-20
# =============================================================================

# Aggregate by municipality and year for bottom-20
df_bottom20_annual <- df_bottom20 %>%
  group_by(municipio, year) %>%
  summarise(
    observed = sum(observed, na.rm = TRUE),
    predicted = sum(predicted, na.rm = TRUE),
    .groups = "drop"
  )

# Plot observed vs predicted for Bottom-20
plot_bottom20 <- ggplot(df_bottom20_annual, aes(x = year)) +
  geom_point(aes(y = observed, color = "Observed"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted"), linewidth = 0.8) +
  facet_wrap(~ municipio, scales = "free_y", ncol = 4) +
  labs(
    title = "Observed vs Predicted Births - Bottom 20 Smallest Municipalities",
    subtitle = "Complete Data 1990-2015 | Municipality-Specific GAM Models",
    x = "Year",
    y = "Annual Births",
    color = "Data Type"
  ) +
  scale_color_manual(values = c("Observed" = "#E91E63", "Predicted" = "#2196F3")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6),
    legend.position = "bottom"
  )

# Save bottom-20 plot
ggsave("evaluation_bottom20_municipalities.png", plot_bottom20, 
       width = 12, height = 10, dpi = 300, bg = "white")
cat("Saved: evaluation_bottom20_municipalities.png\n")

# =============================================================================
# CORRELATION SCATTER PLOTS
# =============================================================================

# Overall correlation plot for top-20
scatter_top20 <- ggplot(df_top20, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.6, color = "#2196F3") +
  geom_abline(slope = 1, intercept = 0, color = "#E91E63", linewidth = 1) +
  labs(
    title = "Observed vs Predicted: Top 20 Largest Municipalities",
    subtitle = paste("Correlation =", round(cor(df_top20$observed, df_top20$predicted), 3)),
    x = "Observed Births",
    y = "Predicted Births"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Overall correlation plot for bottom-20
scatter_bottom20 <- ggplot(df_bottom20, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.6, color = "#2196F3") +
  geom_abline(slope = 1, intercept = 0, color = "#E91E63", linewidth = 1) +
  labs(
    title = "Observed vs Predicted: Bottom 20 Smallest Municipalities",
    subtitle = paste("Correlation =", round(cor(df_bottom20$observed, df_bottom20$predicted), 3)),
    x = "Observed Births",
    y = "Predicted Births"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save scatter plots
ggsave("evaluation_scatter_top20.png", scatter_top20, 
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("evaluation_scatter_bottom20.png", scatter_bottom20, 
       width = 8, height = 6, dpi = 300, bg = "white")

cat("Saved: evaluation_scatter_top20.png\n")
cat("Saved: evaluation_scatter_bottom20.png\n")

# =============================================================================
# DIRECT FITS vs DATA COMPARISON PLOTS
# =============================================================================

cat("\n=== CREATING FITS vs DATA COMPARISON PLOTS ===\n")

# 1. Overall Fits vs Data Scatter Plot
fits_vs_data_overall <- ggplot(all_predictions, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.5, color = "#2196F3", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "#E91E63", linewidth = 1.2, linetype = "solid") +
  geom_smooth(method = "lm", color = "#FF9800", se = TRUE, linewidth = 1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Model Fits vs Real Data - Complete Validation/Test Set",
    subtitle = paste("Correlation =", round(cor(all_predictions$observed, all_predictions$predicted), 3), 
                     "| Red line = Perfect fit | Orange line = Actual relationship"),
    x = "Observed Births (Real Complete Data)",
    y = "Model Predictions (Fits)"
  ) +
  annotation_logticks() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

ggsave("fits_vs_data_overall_log.png", fits_vs_data_overall, 
       width = 10, height = 8, dpi = 300, bg = "white")
cat("Saved: fits_vs_data_overall_log.png\n")

# 2. Linear Scale Fits vs Data
fits_vs_data_linear <- ggplot(all_predictions, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.3, color = "#2196F3", size = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "#E91E63", linewidth = 1.2) +
  geom_smooth(method = "lm", color = "#FF9800", se = TRUE, linewidth = 1) +
  labs(
    title = "Model Fits vs Real Data - Linear Scale",
    subtitle = paste("Perfect agreement would follow the red diagonal line"),
    x = "Observed Births (Real Complete Data)",
    y = "Model Predictions (Fits)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("fits_vs_data_linear.png", fits_vs_data_linear, 
       width = 10, height = 8, dpi = 300, bg = "white")
cat("Saved: fits_vs_data_linear.png\n")

# 3. Fits vs Data by Dataset (Validation vs Test)
if ("dataset" %in% names(all_predictions)) {
  fits_vs_data_by_dataset <- ggplot(all_predictions, aes(x = observed, y = predicted, color = dataset)) +
    geom_point(alpha = 0.5, size = 0.8) +
    geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(values = c("validation" = "#2196F3", "test" = "#E91E63"),
                       labels = c("validation" = "Validation Set", "test" = "Test Set")) +
    labs(
      title = "Model Fits vs Real Data by Dataset",
      subtitle = "Comparing model performance on validation vs test sets",
      x = "Observed Births (Real Complete Data)",
      y = "Model Predictions (Fits)",
      color = "Dataset"
    ) +
    annotation_logticks() +
    facet_wrap(~dataset, ncol = 2) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  ggsave("fits_vs_data_by_dataset.png", fits_vs_data_by_dataset, 
         width = 12, height = 6, dpi = 300, bg = "white")
  cat("Saved: fits_vs_data_by_dataset.png\n")
}

# 4. Fits vs Data by Age Band
fits_vs_data_age <- all_predictions %>%
  ggplot(aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.4, color = "#2196F3", size = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "#E91E63", linewidth = 1) +
  geom_smooth(method = "lm", color = "#FF9800", se = FALSE, linewidth = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~age_band, scales = "free", ncol = 5) +
  labs(
    title = "Model Fits vs Real Data by Age Band",
    subtitle = "Performance across different maternal age groups",
    x = "Observed Births (Real Complete Data)",
    y = "Model Predictions (Fits)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 9),
    axis.text = element_text(size = 7),
    panel.grid.minor = element_blank()
  )

ggsave("fits_vs_data_by_age.png", fits_vs_data_age, 
       width = 15, height = 8, dpi = 300, bg = "white")
cat("Saved: fits_vs_data_by_age.png\n")

# 5. Fits vs Data by Sex
fits_vs_data_sex <- all_predictions %>%
  ggplot(aes(x = observed, y = predicted, color = sex)) +
  geom_point(alpha = 0.5, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c("female" = "#E91E63", "male" = "#2196F3"),
                     labels = c("female" = "Female Births", "male" = "Male Births")) +
  labs(
    title = "Model Fits vs Real Data by Sex",
    subtitle = "Comparing model performance for female vs male births",
    x = "Observed Births (Real Complete Data)",
    y = "Model Predictions (Fits)",
    color = "Sex"
  ) +
  annotation_logticks() +
  facet_wrap(~sex, ncol = 2) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("fits_vs_data_by_sex.png", fits_vs_data_sex, 
       width = 12, height = 6, dpi = 300, bg = "white")
cat("Saved: fits_vs_data_by_sex.png\n")

# 6. Selected Municipalities: Detailed Fits vs Data Time Series
selected_munis <- all_predictions %>%
  group_by(municipio) %>%
  summarise(total_obs = sum(observed, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_obs)) %>%
  slice(c(1:6, (n()-5):n())) %>%  # Top 6 and bottom 6
  pull(municipio)

fits_vs_data_time_series <- all_predictions %>%
  filter(municipio %in% selected_munis) %>%
  group_by(municipio, year) %>%
  summarise(
    observed_total = sum(observed, na.rm = TRUE),
    predicted_total = sum(predicted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(observed_total, predicted_total), 
               names_to = "data_type", values_to = "births") %>%
  mutate(
    data_type = factor(data_type, 
                      levels = c("observed_total", "predicted_total"),
                      labels = c("Real Complete Data", "Model Fits"))
  ) %>%
  ggplot(aes(x = year, y = births, color = data_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Real Complete Data" = "#E91E63", "Model Fits" = "#2196F3")) +
  facet_wrap(~municipio, scales = "free_y", ncol = 3) +
  labs(
    title = "Fits vs Real Data: Time Series for Selected Municipalities",
    subtitle = "Annual totals comparing model fits with real complete data",
    x = "Year",
    y = "Total Births",
    color = "Data Source"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("fits_vs_data_time_series_selected.png", fits_vs_data_time_series, 
       width = 14, height = 10, dpi = 300, bg = "white")
cat("Saved: fits_vs_data_time_series_selected.png\n")

# =============================================================================
# FITS vs DATA QUALITY METRICS
# =============================================================================

cat("\n=== FITS vs DATA QUALITY ASSESSMENT ===\n")

# Calculate R-squared and other fit quality metrics
fit_quality_overall <- all_predictions %>%
  summarise(
    R_squared = cor(observed, predicted)^2,
    Mean_Absolute_Error = mean(abs(predicted - observed), na.rm = TRUE),
    Root_Mean_Square_Error = sqrt(mean((predicted - observed)^2, na.rm = TRUE)),
    Mean_Absolute_Percentage_Error = mean(abs(predicted - observed) / pmax(observed, 1), na.rm = TRUE) * 100,
    Coefficient_of_Determination = 1 - sum((observed - predicted)^2, na.rm = TRUE) / sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  )

cat("OVERALL FIT QUALITY METRICS:\n")
print(fit_quality_overall)

# Fit quality by age band
fit_quality_age <- all_predictions %>%
  group_by(age_band) %>%
  summarise(
    R_squared = cor(observed, predicted)^2,
    MAE = mean(abs(predicted - observed), na.rm = TRUE),
    RMSE = sqrt(mean((predicted - observed)^2, na.rm = TRUE)),
    MAPE = mean(abs(predicted - observed) / pmax(observed, 1), na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\nFIT QUALITY BY AGE BAND:\n")
print(fit_quality_age)

# Fit quality by sex
fit_quality_sex <- all_predictions %>%
  group_by(sex) %>%
  summarise(
    R_squared = cor(observed, predicted)^2,
    MAE = mean(abs(predicted - observed), na.rm = TRUE),
    RMSE = sqrt(mean((predicted - observed)^2, na.rm = TRUE)),
    MAPE = mean(abs(predicted - observed) / pmax(observed, 1), na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\nFIT QUALITY BY SEX:\n")
print(fit_quality_sex)

# =============================================================================
# DIAGNOSTIC PLOTS: RESIDUALS vs FITTED
# =============================================================================

cat("\n=== CREATING DIAGNOSTIC RESIDUALS vs FITTED PLOTS ===\n")

# Calculate residuals
all_predictions_residuals <- all_predictions %>%
  mutate(
    residual = observed - predicted,  # Important: observed - predicted
    fitted = predicted,
    standardized_residual = residual / sd(residual, na.rm = TRUE),
    relative_error = residual / pmax(observed, 1),
    abs_residual = abs(residual)
  )

# 1. Classic Residuals vs Fitted Plot
residuals_vs_fitted_classic <- ggplot(all_predictions_residuals, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.4, color = "#2196F3", size = 0.6) +
  geom_hline(yintercept = 0, color = "#E91E63", linewidth = 1.2) +
  geom_smooth(method = "loess", color = "#FF9800", se = TRUE, linewidth = 1) +
  labs(
    title = "Diagnostic: Residuals vs Fitted Values",
    subtitle = "Points should be randomly scattered around zero line (red)",
    x = "Fitted Values (Model Predictions)",
    y = "Residuals (Observed - Predicted)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("diagnostic_residuals_vs_fitted.png", residuals_vs_fitted_classic, 
       width = 10, height = 8, dpi = 300, bg = "white")
cat("Saved: diagnostic_residuals_vs_fitted.png\n")

# 2. Standardized Residuals vs Fitted
standardized_residuals_plot <- ggplot(all_predictions_residuals, aes(x = fitted, y = standardized_residual)) +
  geom_point(alpha = 0.4, color = "#2196F3", size = 0.6) +
  geom_hline(yintercept = 0, color = "#E91E63", linewidth = 1.2) +
  geom_hline(yintercept = c(-2, 2), color = "#FF9800", linewidth = 1, linetype = "dashed") +
  geom_smooth(method = "loess", color = "#FF9800", se = TRUE, linewidth = 1) +
  labs(
    title = "Diagnostic: Standardized Residuals vs Fitted Values",
    subtitle = "Points outside ±2 lines (orange) are potential outliers",
    x = "Fitted Values (Model Predictions)",
    y = "Standardized Residuals"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("diagnostic_standardized_residuals_vs_fitted.png", standardized_residuals_plot, 
       width = 10, height = 8, dpi = 300, bg = "white")
cat("Saved: diagnostic_standardized_residuals_vs_fitted.png\n")

# 3. Square Root of Absolute Residuals vs Fitted (Scale-Location plot)
sqrt_abs_residuals_plot <- ggplot(all_predictions_residuals, aes(x = fitted, y = sqrt(abs_residual))) +
  geom_point(alpha = 0.4, color = "#2196F3", size = 0.6) +
  geom_smooth(method = "loess", color = "#FF9800", se = TRUE, linewidth = 1) +
  labs(
    title = "Diagnostic: Scale-Location Plot",
    subtitle = "Checks homoscedasticity - should be roughly horizontal",
    x = "Fitted Values (Model Predictions)",
    y = "√|Residuals|"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("diagnostic_scale_location.png", sqrt_abs_residuals_plot, 
       width = 10, height = 8, dpi = 300, bg = "white")
cat("Saved: diagnostic_scale_location.png\n")

# 4. Residuals vs Fitted by Age Band
residuals_fitted_age <- ggplot(all_predictions_residuals, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.5, color = "#2196F3", size = 0.5) +
  geom_hline(yintercept = 0, color = "#E91E63", linewidth = 1) +
  geom_smooth(method = "loess", color = "#FF9800", se = FALSE, linewidth = 0.8) +
  facet_wrap(~age_band, scales = "free", ncol = 5) +
  labs(
    title = "Diagnostic: Residuals vs Fitted by Age Band",
    subtitle = "Checking for age-specific patterns in residuals",
    x = "Fitted Values",
    y = "Residuals (Observed - Predicted)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 9),
    axis.text = element_text(size = 7)
  )

ggsave("diagnostic_residuals_vs_fitted_by_age.png", residuals_fitted_age, 
       width = 15, height = 8, dpi = 300, bg = "white")
cat("Saved: diagnostic_residuals_vs_fitted_by_age.png\n")

# 5. Residuals vs Fitted by Sex
residuals_fitted_sex <- ggplot(all_predictions_residuals, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.5, color = "#2196F3", size = 0.6) +
  geom_hline(yintercept = 0, color = "#E91E63", linewidth = 1.2) +
  geom_smooth(method = "loess", color = "#FF9800", se = TRUE, linewidth = 1) +
  facet_wrap(~sex, ncol = 2) +
  labs(
    title = "Diagnostic: Residuals vs Fitted by Sex",
    subtitle = "Checking for sex-specific patterns in residuals",
    x = "Fitted Values (Model Predictions)",
    y = "Residuals (Observed - Predicted)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("diagnostic_residuals_vs_fitted_by_sex.png", residuals_fitted_sex, 
       width = 12, height = 6, dpi = 300, bg = "white")
cat("Saved: diagnostic_residuals_vs_fitted_by_sex.png\n")

# 6. Histogram of Residuals
residuals_histogram <- ggplot(all_predictions_residuals, aes(x = residual)) +
  geom_histogram(bins = 50, fill = "#2196F3", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0, color = "#E91E63", linewidth = 1.2) +
  labs(
    title = "Diagnostic: Distribution of Residuals",
    subtitle = "Should be approximately normal and centered at zero",
    x = "Residuals (Observed - Predicted)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("diagnostic_residuals_histogram.png", residuals_histogram, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: diagnostic_residuals_histogram.png\n")

# =============================================================================
# DIAGNOSTIC SUMMARY STATISTICS
# =============================================================================

cat("\n=== DIAGNOSTIC SUMMARY ===\n")

# Overall residual diagnostics
residual_diagnostics <- all_predictions_residuals %>%
  summarise(
    Mean_Residual = mean(residual, na.rm = TRUE),
    Median_Residual = median(residual, na.rm = TRUE),
    SD_Residual = sd(residual, na.rm = TRUE),
    Min_Residual = min(residual, na.rm = TRUE),
    Max_Residual = max(residual, na.rm = TRUE),
    Q25_Residual = quantile(residual, 0.25, na.rm = TRUE),
    Q75_Residual = quantile(residual, 0.75, na.rm = TRUE),
    Outliers_Beyond_2SD = sum(abs(standardized_residual) > 2, na.rm = TRUE),
    Percent_Outliers = 100 * sum(abs(standardized_residual) > 2, na.rm = TRUE) / n()
  )

cat("RESIDUAL DIAGNOSTIC STATISTICS:\n")
print(residual_diagnostics)

# Test for patterns in residuals
cat("\nPATTERN TESTS:\n")
cat("Mean residual (should be close to 0):", round(mean(all_predictions_residuals$residual, na.rm = TRUE), 3), "\n")
cat("% of residuals beyond ±2 SD:", round(100 * sum(abs(all_predictions_residuals$standardized_residual) > 2, na.rm = TRUE) / nrow(all_predictions_residuals), 2), "%\n")

# Check for systematic patterns by fitted value ranges
fitted_quartiles <- all_predictions_residuals %>%
  mutate(fitted_quartile = cut(fitted, 
                              breaks = quantile(fitted, c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE),
                              labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
                              include.lowest = TRUE)) %>%
  group_by(fitted_quartile) %>%
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    sd_residual = sd(residual, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nRESIDUALS BY FITTED VALUE QUARTILES:\n")
print(fitted_quartiles)

# =============================================================================
# NOWCASTING DEMONSTRATION: SELECTED MUNICIPALITIES
# =============================================================================

cat("\n=== CREATING NOWCASTING DEMONSTRATION PLOTS ===\n")

# Load nowcasting data for 2016-2023
if (file.exists("nowcast_predictions_2016_2023.RDS")) {
  nowcast_data <- readRDS("nowcast_predictions_2016_2023.RDS")
  cat("Loaded nowcast data for 2016-2023:", nrow(nowcast_data), "rows\n")
} else {
  cat("Warning: nowcast_predictions_2016_2023.RDS not found. Skipping nowcasting demonstration.\n")
  nowcast_data <- NULL
}

# Load stacked births data if available
if (file.exists("stacked_births_data_2016_2023.RDS")) {
  stacked_births <- readRDS("stacked_births_data_2016_2023.RDS")
  cat("Loaded stacked births data:", nrow(stacked_births), "rows\n")
} else {
  cat("Warning: stacked_births_data_2016_2023.RDS not found.\n")
  stacked_births <- NULL
}

if (!is.null(nowcast_data)) {
  
  # Select municipalities for demonstration
  demo_municipalities <- c(
    head(top20_municipalities$municipio, 3),  # Top 3 largest
    tail(bottom20_municipalities$municipio, 3)  # Bottom 3 smallest
  )
  
  cat("Selected municipalities for demonstration:", paste(demo_municipalities, collapse = ", "), "\n")
  
  # =============================================================================
  # 1. YEARLY BARPLOTS: REGISTERED + NOWCAST (STACKED)
  # =============================================================================
  
  # Prepare data for stacked barplots using nowcast data directly
  demo_stacked_data <- nowcast_data %>%
    mutate(municipio = municipality, year = event_year) %>%  # Standardize column names
    filter(municipio %in% demo_municipalities) %>%
    group_by(municipio, year) %>%
    summarise(
      births_registered = sum(population * fert_rate, na.rm = TRUE),  # Approximate registered births
      births_nowcast = sum(predicted, na.rm = TRUE),  # Nowcast total
      .groups = "drop"
    ) %>%
    mutate(
      births_nowcast_additional = pmax(0, births_nowcast - births_registered)  # Additional from nowcast
    ) %>%
    dplyr::select(municipio, year, births_registered, births_nowcast_additional) %>%
    pivot_longer(cols = c(births_registered, births_nowcast_additional),
                 names_to = "component", values_to = "births") %>%
    mutate(
      component = factor(component, 
                        levels = c("births_nowcast_additional", "births_registered"),
                        labels = c("Nowcast Additional", "Registered"))
    )
  
  # Create stacked barplot for selected municipalities
  nowcast_demo_barplot <- demo_stacked_data %>%
    ggplot(aes(x = year, y = births, fill = component)) +
    geom_col(position = "stack", alpha = 0.8) +
    scale_fill_manual(
      values = c("Registered" = "#2196F3", "Nowcast Additional" = "#E91E63"),
      guide = guide_legend(reverse = TRUE)
    ) +
    facet_wrap(~municipio, scales = "free_y", ncol = 3) +
    labs(
      title = "Nowcasting Demonstration: Registered vs Nowcast Births",
      subtitle = "Stacked bars show registered births (blue) + nowcast additional (pink) for 2016-2023",
      x = "Year",
      y = "Number of Births",
      fill = "Data Type"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 10)
    )
  
  ggsave("nowcast_demonstration_selected_municipalities.png", nowcast_demo_barplot, 
         width = 15, height = 10, dpi = 300, bg = "white")
  cat("Saved: nowcast_demonstration_selected_municipalities.png\n")
  
  # =============================================================================
  # 2. FERTILITY CURVES BY AGE AND SEX (2000-2023)
  # =============================================================================
  
  # Select 2 municipalities: 1 large, 1 small
  fertility_munis <- c(
    top20_municipalities$municipio[1],    # Largest
    bottom20_municipalities$municipio[20] # Smallest
  )
  
  cat("Creating fertility curves for municipalities:", paste(fertility_munis, collapse = ", "), "\n")
  
  # Load historical complete data if available
  historical_years <- 2000:2015
  
  # Get validation/test data for 2006-2015 (extend backwards if needed)
  historical_fertility_data <- all_predictions %>%
    filter(municipio %in% fertility_munis, year >= 2000) %>%
    mutate(
      data_source = "Complete Data",
      births_rate = observed / 1000  # Convert to rate per 1000 for visualization
    ) %>%
    dplyr::select(municipio, year, age_band, sex, births_rate, data_source)
  
  # Get nowcast data for 2016-2023
  nowcast_fertility_data <- nowcast_data %>%
    mutate(municipio = municipality, year = event_year, age_band = age_group) %>%  # Standardize names
    filter(municipio %in% fertility_munis) %>%
    group_by(municipio, year, age_band, sex) %>%
    summarise(
      births_registered = sum(population * fert_rate, na.rm = TRUE),  # Approximate registered
      births_nowcast = sum(predicted, na.rm = TRUE),  # Nowcast predictions
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(births_registered, births_nowcast),
                 names_to = "data_type", values_to = "births") %>%
    mutate(
      data_source = ifelse(data_type == "births_registered", "Registered", "Nowcast"),
      births_rate = births / 1000
    ) %>%
    dplyr::select(municipio, year, age_band, sex, births_rate, data_source)
  
  # Combine historical and nowcast data
  combined_fertility <- bind_rows(historical_fertility_data, nowcast_fertility_data) %>%
    mutate(
      age_numeric = case_when(
        age_band == "15-19" ~ 17.5,
        age_band == "20-24" ~ 22.5,
        age_band == "25-29" ~ 27.5,
        age_band == "30-34" ~ 32.5,
        age_band == "35-39" ~ 37.5,
        age_band == "40-44" ~ 42.5,
        age_band == "45-49" ~ 47.5,
        age_band == "50-54" ~ 52.5,
        age_band == "55-59" ~ 57.5,
        age_band == "60-64" ~ 62.5,
        TRUE ~ NA_real_
      ),
      period = case_when(
        year <= 2015 ~ "2000-2015 (Complete)",
        year >= 2016 ~ "2016-2023 (Nowcast)",
        TRUE ~ "Other"
      )
    ) %>%
    filter(!is.na(age_numeric))
  
  # Create fertility curves plot
  fertility_curves_plot <- combined_fertility %>%
    filter(year %in% c(2005, 2010, 2015, 2018, 2020, 2022)) %>%  # Select representative years
    ggplot(aes(x = age_numeric, y = births_rate, color = data_source, linetype = factor(year))) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_color_manual(
      values = c("Complete Data" = "#2196F3", "Registered" = "#FF9800", "Nowcast" = "#E91E63")
    ) +
    scale_linetype_manual(
      values = c("2005" = "solid", "2010" = "solid", "2015" = "solid", 
                 "2018" = "dashed", "2020" = "dashed", "2022" = "dashed")
    ) +
    facet_grid(sex ~ municipio, scales = "free_y") +
    labs(
      title = "Fertility Curves by Age: Complete Data vs Nowcast",
      subtitle = "Solid lines: Complete data (2005-2015) | Dashed lines: Registered vs Nowcast (2018-2022)",
      x = "Maternal Age",
      y = "Birth Rate (per 1000)",
      color = "Data Source",
      linetype = "Year"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      strip.text = element_text(size = 10)
    )
  
  ggsave("fertility_curves_complete_vs_nowcast.png", fertility_curves_plot, 
         width = 14, height = 10, dpi = 300, bg = "white")
  cat("Saved: fertility_curves_complete_vs_nowcast.png\n")
  
  # =============================================================================
  # 3. DETAILED TIME SERIES: REGISTERED vs NOWCAST (2016-2023)
  # =============================================================================
  
  # Create detailed time series for the 2 selected municipalities
  detailed_nowcast_series <- nowcast_data %>%
    mutate(municipio = municipality, year = event_year) %>%  # Standardize names
    filter(municipio %in% fertility_munis) %>%
    group_by(municipio, year, sex) %>%
    summarise(
      births_registered = sum(population * fert_rate, na.rm = TRUE),  # Approximate registered
      births_nowcast = sum(predicted, na.rm = TRUE),  # Nowcast total
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(births_registered, births_nowcast),
                 names_to = "data_type", values_to = "births") %>%
    mutate(
      data_type = factor(data_type, 
                        levels = c("births_registered", "births_nowcast"),
                        labels = c("Registered", "Nowcast"))
    )
  
  detailed_series_plot <- detailed_nowcast_series %>%
    ggplot(aes(x = year, y = births, color = data_type, shape = data_type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Registered" = "#FF9800", "Nowcast" = "#E91E63")) +
    scale_shape_manual(values = c("Registered" = 16, "Nowcast" = 17)) +
    facet_grid(sex ~ municipio, scales = "free_y") +
    labs(
      title = "Nowcast Time Series: Registered vs Model-Adjusted Births (2016-2023)",
      subtitle = "Comparing raw registered data with β-coefficient adjusted nowcast estimates",
      x = "Year",
      y = "Number of Births",
      color = "Data Type",
      shape = "Data Type"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      strip.text = element_text(size = 11)
    )
  
  ggsave("nowcast_time_series_detailed.png", detailed_series_plot, 
         width = 12, height = 8, dpi = 300, bg = "white")
  cat("Saved: nowcast_time_series_detailed.png\n")
  
  # =============================================================================
  # NOWCAST DEMONSTRATION SUMMARY
  # =============================================================================
  
  cat("\n=== NOWCAST DEMONSTRATION SUMMARY ===\n")
  
  # Summary statistics for demonstration municipalities
  demo_summary <- demo_stacked_data %>%
    group_by(municipio, component) %>%
    summarise(
      total_births = sum(births, na.rm = TRUE),
      avg_annual = mean(births, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = component, values_from = c(total_births, avg_annual)) %>%
    mutate(
      nowcast_percentage = 100 * `total_births_Nowcast Additional` / 
                          (`total_births_Registered` + `total_births_Nowcast Additional`)
    )
  
  cat("NOWCAST IMPACT BY MUNICIPALITY (2016-2023):\n")
  print(demo_summary)
  
} else {
  cat("Skipping nowcasting demonstration plots due to missing data files.\n")
}

# Continue with existing residual analysis from the diagnostic section above

# Calculate residuals and relative errors
all_predictions_residuals <- all_predictions %>%
  mutate(
    residual = predicted - observed,
    relative_error = (predicted - observed) / pmax(observed, 1),
    abs_residual = abs(residual),
    abs_relative_error = abs(relative_error),
    fitted_log = log(pmax(predicted, 1)),
    observed_log = log(pmax(observed, 1))
  )

# =============================================================================
# FIT vs DATA PATTERN PLOTS
# =============================================================================

# 1. Residuals vs Fitted Values (to detect patterns)
residual_fitted_plot <- ggplot(all_predictions_residuals, aes(x = predicted, y = residual)) +
  geom_point(alpha = 0.3, color = "#2196F3") +
  geom_hline(yintercept = 0, color = "#E91E63", linewidth = 1) +
  geom_smooth(method = "loess", color = "#FF9800", se = TRUE) +
  labs(
    title = "Residuals vs Fitted Values - Pattern Detection",
    subtitle = "Loess smoother shows systematic patterns (should be flat around 0)",
    x = "Predicted Births (Fitted Values)",
    y = "Residuals (Predicted - Observed)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("evaluation_residuals_vs_fitted.png", residual_fitted_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: evaluation_residuals_vs_fitted.png\n")

# 2. Relative Error vs Fitted Values (scaled analysis)
relative_error_plot <- ggplot(all_predictions_residuals, aes(x = predicted, y = relative_error)) +
  geom_point(alpha = 0.3, color = "#2196F3") +
  geom_hline(yintercept = 0, color = "#E91E63", linewidth = 1) +
  geom_smooth(method = "loess", color = "#FF9800", se = TRUE) +
  ylim(-2, 2) +  # Limit to reasonable range
  labs(
    title = "Relative Error vs Fitted Values - Scale-Independent Patterns",
    subtitle = "Relative Error = (Predicted - Observed) / Observed",
    x = "Predicted Births (Fitted Values)",
    y = "Relative Error"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("evaluation_relative_error_vs_fitted.png", relative_error_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: evaluation_relative_error_vs_fitted.png\n")

# 3. QQ Plot for Residual Distribution
qq_plot <- ggplot(all_predictions_residuals, aes(sample = residual)) +
  stat_qq(alpha = 0.5, color = "#2196F3") +
  stat_qq_line(color = "#E91E63", linewidth = 1) +
  labs(
    title = "Q-Q Plot: Residual Distribution",
    subtitle = "Assessing normality of residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles (Residuals)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("evaluation_qq_plot_residuals.png", qq_plot, 
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: evaluation_qq_plot_residuals.png\n")

# 4. Residuals by Year (temporal patterns)
residual_year_plot <- all_predictions_residuals %>%
  group_by(year) %>%
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    median_residual = median(residual, na.rm = TRUE),
    sd_residual = sd(residual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = mean_residual, color = "Mean"), linewidth = 1) +
  geom_line(aes(y = median_residual, color = "Median"), linewidth = 1) +
  geom_ribbon(aes(ymin = mean_residual - sd_residual, 
                  ymax = mean_residual + sd_residual), 
              alpha = 0.3, fill = "#2196F3") +
  geom_hline(yintercept = 0, color = "#E91E63", linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "#2196F3", "Median" = "#FF9800")) +
  labs(
    title = "Residuals by Year - Temporal Patterns",
    subtitle = "Mean/Median residuals with ±1 SD band",
    x = "Year",
    y = "Residuals",
    color = "Statistic"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

ggsave("evaluation_residuals_by_year.png", residual_year_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: evaluation_residuals_by_year.png\n")

# 5. Residuals by Age Band (demographic patterns)
residual_age_plot <- all_predictions_residuals %>%
  group_by(age_band) %>%
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    median_residual = median(residual, na.rm = TRUE),
    mean_relative_error = mean(relative_error, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = age_band)) +
  geom_col(aes(y = mean_residual, fill = "Mean Residual"), alpha = 0.7) +
  geom_point(aes(y = mean_relative_error * 100, color = "Mean Relative Error (%)"), 
             size = 3) +
  scale_y_continuous(
    name = "Mean Residual",
    sec.axis = sec_axis(~ . / 100, name = "Mean Relative Error")
  ) +
  scale_fill_manual(values = c("Mean Residual" = "#2196F3")) +
  scale_color_manual(values = c("Mean Relative Error (%)" = "#E91E63")) +
  labs(
    title = "Residuals by Age Band - Demographic Patterns",
    subtitle = "Systematic bias by maternal age",
    x = "Age Band",
    fill = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("evaluation_residuals_by_age.png", residual_age_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: evaluation_residuals_by_age.png\n")

# 6. Residuals by Sex (gender patterns)
residual_sex_plot <- all_predictions_residuals %>%
  group_by(sex) %>%
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    median_residual = median(residual, na.rm = TRUE),
    mean_relative_error = mean(relative_error, na.rm = TRUE),
    rmse = sqrt(mean(residual^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(mean_residual, median_residual, mean_relative_error, rmse),
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = sex, y = value, fill = metric)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(
    values = c("mean_residual" = "#2196F3", "median_residual" = "#FF9800", 
               "mean_relative_error" = "#E91E63", "rmse" = "#9C27B0"),
    labels = c("Mean Residual", "Median Residual", "Mean Relative Error", "RMSE")
  ) +
  labs(
    title = "Model Performance by Sex",
    subtitle = "Comparing error patterns between female and male births",
    x = "Sex",
    y = "Error Metric Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

ggsave("evaluation_residuals_by_sex.png", residual_sex_plot, 
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: evaluation_residuals_by_sex.png\n")

# =============================================================================
# PATTERN SUMMARY STATISTICS
# =============================================================================

cat("\n=== PATTERN ANALYSIS SUMMARY ===\n")

# Overall residual statistics
residual_stats <- all_predictions_residuals %>%
  summarise(
    Mean_Residual = mean(residual, na.rm = TRUE),
    Median_Residual = median(residual, na.rm = TRUE),
    SD_Residual = sd(residual, na.rm = TRUE),
    Mean_Abs_Residual = mean(abs_residual, na.rm = TRUE),
    Mean_Relative_Error = mean(relative_error, na.rm = TRUE),
    Mean_Abs_Relative_Error = mean(abs_relative_error, na.rm = TRUE),
    Q25_Residual = quantile(residual, 0.25, na.rm = TRUE),
    Q75_Residual = quantile(residual, 0.75, na.rm = TRUE)
  )

cat("RESIDUAL STATISTICS:\n")
print(residual_stats)

# Pattern detection by groups
pattern_by_year <- all_predictions_residuals %>%
  group_by(year) %>%
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    correlation_obs_pred = cor(observed, predicted, use = "complete.obs"),
    .groups = "drop"
  )

cat("\nTEMPORAL PATTERNS (by Year):\n")
print(pattern_by_year)

pattern_by_age <- all_predictions_residuals %>%
  group_by(age_band) %>%
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    mean_relative_error = mean(relative_error, na.rm = TRUE),
    correlation_obs_pred = cor(observed, predicted, use = "complete.obs"),
    .groups = "drop"
  )

cat("\nDEMOGRAPHIC PATTERNS (by Age Band):\n")
print(pattern_by_age)

pattern_by_sex <- all_predictions_residuals %>%
  group_by(sex) %>%
  summarise(
    mean_residual = mean(residual, na.rm = TRUE),
    mean_relative_error = mean(relative_error, na.rm = TRUE),
    correlation_obs_pred = cor(observed, predicted, use = "complete.obs"),
    rmse = sqrt(mean(residual^2, na.rm = TRUE)),
    .groups = "drop"
  )

cat("\nGENDER PATTERNS (by Sex):\n")
print(pattern_by_sex)

# =============================================================================
# SAVE EVALUATION RESULTS
# =============================================================================

# Save detailed results
evaluation_results <- list(
  overall_metrics = eval_metrics,
  top20_municipalities = top20_municipalities,
  bottom20_municipalities = bottom20_municipalities,
  comparison_metrics = comparison_metrics,
  top20_data = df_top20,
  bottom20_data = df_bottom20
)

saveRDS(evaluation_results, "evaluation_top_bottom_municipalities.RDS")
cat("Saved: evaluation_top_bottom_municipalities.RDS\n")

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("EVALUATION SUMMARY: TOP-20 vs BOTTOM-20 MUNICIPALITIES\n")
cat(rep("=", 80), "\n", sep="")

cat("\nOVERALL MODEL PERFORMANCE:\n")
cat("- Total municipalities evaluated:", eval_metrics$Municipalities, "\n")
cat("- Overall correlation:", round(eval_metrics$Correlation, 3), "\n")
cat("- Overall RMSE:", round(eval_metrics$RMSE, 2), "\n")
cat("- Overall bias:", round(eval_metrics$Bias_Percentage, 2), "%\n")

cat("\nTOP-20 LARGEST MUNICIPALITIES:\n")
cat("- Average births per municipality:", round(mean(top20_municipalities$total_observed), 0), "\n")
cat("- Correlation:", round(metrics_top20$Correlation, 3), "\n")
cat("- RMSE:", round(metrics_top20$RMSE, 2), "\n")
cat("- Bias:", round(metrics_top20$Bias_Percentage, 2), "%\n")

cat("\nBOTTOM-20 SMALLEST MUNICIPALITIES:\n")
cat("- Average births per municipality:", round(mean(bottom20_municipalities$total_observed), 0), "\n")
cat("- Correlation:", round(metrics_bottom20$Correlation, 3), "\n")
cat("- RMSE:", round(metrics_bottom20$RMSE, 2), "\n")
cat("- Bias:", round(metrics_bottom20$Bias_Percentage, 2), "%\n")

cat("\nFILES GENERATED:\n")
cat("✅ evaluation_top20_municipalities.png - Time series plots for largest municipalities\n")
cat("✅ evaluation_bottom20_municipalities.png - Time series plots for smallest municipalities\n")
cat("✅ evaluation_scatter_top20.png - Correlation scatter for largest municipalities\n")
cat("✅ evaluation_scatter_bottom20.png - Correlation scatter for smallest municipalities\n")
cat("✅ fits_vs_data_overall_log.png - Overall fits vs real data (log scale)\n")
cat("✅ fits_vs_data_linear.png - Overall fits vs real data (linear scale)\n")
cat("✅ fits_vs_data_by_dataset.png - Fits vs data comparison by validation/test sets\n")
cat("✅ fits_vs_data_by_age.png - Fits vs data by age band\n")
cat("✅ fits_vs_data_by_sex.png - Fits vs data by sex\n")
cat("✅ fits_vs_data_time_series_selected.png - Time series fits vs data for selected municipalities\n")
cat("✅ diagnostic_residuals_vs_fitted.png - Classic residuals vs fitted diagnostic plot\n")
cat("✅ diagnostic_standardized_residuals_vs_fitted.png - Standardized residuals vs fitted\n")
cat("✅ diagnostic_scale_location.png - Scale-location plot for homoscedasticity\n")
cat("✅ diagnostic_residuals_vs_fitted_by_age.png - Residuals vs fitted by age band\n")
cat("✅ diagnostic_residuals_vs_fitted_by_sex.png - Residuals vs fitted by sex\n")
cat("✅ diagnostic_residuals_histogram.png - Distribution of residuals\n")
cat("✅ nowcast_demonstration_selected_municipalities.png - Nowcast demo with stacked bars\n")
cat("✅ fertility_curves_complete_vs_nowcast.png - Fertility curves by age (2000-2023)\n")
cat("✅ nowcast_time_series_detailed.png - Detailed nowcast time series (2016-2023)\n")
cat("✅ evaluation_residuals_vs_fitted.png - Residuals vs fitted values pattern analysis\n")
cat("✅ evaluation_relative_error_vs_fitted.png - Relative error vs fitted values\n")
cat("✅ evaluation_qq_plot_residuals.png - Q-Q plot for residual normality\n")
cat("✅ evaluation_residuals_by_year.png - Temporal patterns in residuals\n")
cat("✅ evaluation_residuals_by_age.png - Demographic patterns by age band\n")
cat("✅ evaluation_residuals_by_sex.png - Gender-specific error patterns\n")
cat("✅ evaluation_top_bottom_municipalities.RDS - Complete evaluation results\n")

cat("\n", rep("=", 80), "\n", sep="")
