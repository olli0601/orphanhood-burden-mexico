# =============================================================================
# ch4_110_gam_nowcast_critical_fixes.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Apply critical corrections to the validated predictions/metrics and flag problematic municipalities.
# Reads input-data-processed/{validation,test}_{predictions,metrics}.RDS -> writes *_corrected.RDS, output/ch4/.
# =============================================================================

################################################################################
# NOWCASTING MODEL - CRITICAL FIXES IMPLEMENTATION
# Addressing: Bias correction, Coverage calibration, Outlier handling
################################################################################

library(dplyr)
library(tidyr)
library(mgcv)
library(ggplot2)
library(purrr)

# Keep working directory as current folder
cat("=== IMPLEMENTING CRITICAL FIXES ===\n")
cat("Working directory:", getwd(), "\n")

################################################################################
# LOAD VALIDATION RESULTS
################################################################################

validation_preds <- readRDS('input-data-processed/validation_predictions.RDS')
test_preds <- readRDS('input-data-processed/test_predictions.RDS')
validation_metrics <- readRDS('input-data-processed/validation_metrics.RDS')
test_metrics <- readRDS('input-data-processed/test_metrics.RDS')

cat("Loaded validation data\n")

################################################################################
# FIX 1: BIAS CORRECTION
################################################################################

cat("\n=== FIX 1: BIAS CORRECTION ===\n")

# Calculate overall bias
total_observed_val <- sum(validation_preds$events, na.rm = TRUE)
total_predicted_val <- sum(validation_preds$predicted, na.rm = TRUE)
bias_factor <- total_observed_val / total_predicted_val

cat("Overall bias factor:", round(bias_factor, 3), "\n")
cat("Bias percentage:", round((1 - bias_factor) * 100, 1), "%\n")

# Apply bias correction
validation_preds_corrected <- validation_preds %>%
  mutate(
    predicted_corrected = predicted * bias_factor,
    lower_95_corrected = lower_95 * bias_factor,
    upper_95_corrected = upper_95 * bias_factor
  )

test_preds_corrected <- test_preds %>%
  mutate(
    predicted_corrected = predicted * bias_factor,
    lower_95_corrected = lower_95 * bias_factor,
    upper_95_corrected = upper_95 * bias_factor
  )

# Calculate corrected metrics
corrected_val_metrics <- validation_preds_corrected %>%
  filter(!is.na(predicted_corrected), !is.na(events)) %>%
  summarise(
    correlation = cor(events, predicted_corrected, use = "complete.obs"),
    mae = mean(abs(events - predicted_corrected), na.rm = TRUE),
    rmse = sqrt(mean((events - predicted_corrected)^2, na.rm = TRUE)),
    mape = mean(abs((events - predicted_corrected) / pmax(events, 1)) * 100, na.rm = TRUE),
    bias_corrected = sum(predicted_corrected, na.rm = TRUE) - sum(events, na.rm = TRUE),
    relative_bias = bias_corrected / sum(events, na.rm = TRUE) * 100
  )

cat("\nBIAS CORRECTION RESULTS:\n")
cat("- New bias:", round(corrected_val_metrics$relative_bias, 2), "%\n")
cat("- Correlation maintained:", round(corrected_val_metrics$correlation, 3), "\n")
cat("- MAE changed:", round(validation_metrics$mae, 1), "→", round(corrected_val_metrics$mae, 1), "\n")

################################################################################
# FIX 2: COVERAGE CALIBRATION
################################################################################

cat("\n=== FIX 2: COVERAGE CALIBRATION ===\n")

# Calculate current coverage with bias-corrected predictions
current_coverage <- validation_preds_corrected %>%
  filter(!is.na(predicted_corrected), !is.na(events), !is.na(lower_95_corrected), !is.na(upper_95_corrected)) %>%
  mutate(is_covered = events >= lower_95_corrected & events <= upper_95_corrected) %>%
  summarise(coverage_rate = mean(is_covered, na.rm = TRUE)) %>%
  pull(coverage_rate)

cat("Current coverage (bias-corrected):", round(current_coverage * 100, 1), "%\n")

# Calculate calibration factor needed
target_coverage <- 0.95
# Use quantile method to find multiplier
prediction_errors <- validation_preds_corrected %>%
  filter(!is.na(predicted_corrected), !is.na(events)) %>%
  mutate(
    error = events - predicted_corrected,
    abs_error = abs(error),
    current_width = upper_95_corrected - lower_95_corrected,
    relative_error = abs_error / pmax(predicted_corrected, 1)
  )

# Find the multiplier that would give 95% coverage
error_quantile_95 <- quantile(prediction_errors$abs_error, 0.95, na.rm = TRUE)
mean_current_half_width <- mean((prediction_errors$upper_95_corrected - prediction_errors$lower_95_corrected) / 2, na.rm = TRUE)
coverage_multiplier <- error_quantile_95 / mean_current_half_width

cat("Suggested coverage multiplier:", round(coverage_multiplier, 2), "\n")

# Apply coverage calibration
validation_preds_final <- validation_preds_corrected %>%
  mutate(
    interval_center = predicted_corrected,
    current_half_width = (upper_95_corrected - lower_95_corrected) / 2,
    calibrated_half_width = current_half_width * coverage_multiplier,
    lower_95_final = interval_center - calibrated_half_width,
    upper_95_final = interval_center + calibrated_half_width
  )

test_preds_final <- test_preds_corrected %>%
  mutate(
    interval_center = predicted_corrected,
    current_half_width = (upper_95_corrected - lower_95_corrected) / 2,
    calibrated_half_width = current_half_width * coverage_multiplier,
    lower_95_final = interval_center - calibrated_half_width,
    upper_95_final = interval_center + calibrated_half_width
  )

# Test new coverage
final_coverage_val <- validation_preds_final %>%
  filter(!is.na(predicted_corrected), !is.na(events), !is.na(lower_95_final), !is.na(upper_95_final)) %>%
  mutate(is_covered = events >= lower_95_final & events <= upper_95_final) %>%
  summarise(coverage_rate = mean(is_covered, na.rm = TRUE)) %>%
  pull(coverage_rate)

final_coverage_test <- test_preds_final %>%
  filter(!is.na(predicted_corrected), !is.na(events), !is.na(lower_95_final), !is.na(upper_95_final)) %>%
  mutate(is_covered = events >= lower_95_final & events <= upper_95_final) %>%
  summarise(coverage_rate = mean(is_covered, na.rm = TRUE)) %>%
  pull(coverage_rate)

cat("NEW COVERAGE RATES:\n")
cat("- Validation:", round(final_coverage_val * 100, 1), "%\n")
cat("- Test:", round(final_coverage_test * 100, 1), "%\n")

################################################################################
# FIX 3: OUTLIER IDENTIFICATION AND FLAGGING
################################################################################

cat("\n=== FIX 3: OUTLIER IDENTIFICATION ===\n")

# Identify problematic municipalities
outlier_analysis <- validation_preds_final %>%
  filter(!is.na(predicted_corrected), !is.na(events)) %>%
  mutate(
    error_abs = abs(events - predicted_corrected),
    error_rel = error_abs / pmax(events, 1)
  ) %>%
  group_by(municipality) %>%
  summarise(
    n_obs = n(),
    mean_error_abs = mean(error_abs, na.rm = TRUE),
    max_error_abs = max(error_abs, na.rm = TRUE),
    mean_error_rel = mean(error_rel, na.rm = TRUE),
    mean_events = mean(events, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(max_error_abs))

cat("TOP 5 PROBLEMATIC MUNICIPALITIES:\n")
print(head(outlier_analysis, 5))

# Flag extreme outlier municipalities
outlier_threshold_abs <- quantile(outlier_analysis$max_error_abs, 0.95, na.rm = TRUE)
problematic_municipalities <- outlier_analysis %>%
  filter(max_error_abs > outlier_threshold_abs) %>%
  pull(municipality)

cat("\nProblematic municipalities (", length(problematic_municipalities), "):", 
    paste(head(problematic_municipalities, 5), collapse = ", "), "...\n")

# Add outlier flags
validation_preds_final <- validation_preds_final %>%
  mutate(
    is_outlier_municipality = municipality %in% problematic_municipalities,
    error_abs = abs(events - predicted_corrected),
    is_outlier_prediction = error_abs > quantile(error_abs, 0.95, na.rm = TRUE)
  )

################################################################################
# FINAL METRICS CALCULATION
################################################################################

cat("\n=== FINAL CORRECTED METRICS ===\n")

final_val_metrics <- validation_preds_final %>%
  filter(!is.na(predicted_corrected), !is.na(events)) %>%
  summarise(
    n_predictions = n(),
    correlation = cor(events, predicted_corrected, use = "complete.obs"),
    mae = mean(abs(events - predicted_corrected), na.rm = TRUE),
    rmse = sqrt(mean((events - predicted_corrected)^2, na.rm = TRUE)),
    mape = mean(abs((events - predicted_corrected) / pmax(events, 1)) * 100, na.rm = TRUE),
    coverage_95 = mean(events >= lower_95_final & events <= upper_95_final, na.rm = TRUE),
    bias_percent = (sum(predicted_corrected, na.rm = TRUE) - sum(events, na.rm = TRUE)) / sum(events, na.rm = TRUE) * 100,
    outlier_rate = mean(is_outlier_prediction, na.rm = TRUE)
  )

final_test_metrics <- test_preds_final %>%
  filter(!is.na(predicted_corrected), !is.na(events)) %>%
  mutate(error_abs = abs(events - predicted_corrected)) %>%
  summarise(
    n_predictions = n(),
    correlation = cor(events, predicted_corrected, use = "complete.obs"),
    mae = mean(abs(events - predicted_corrected), na.rm = TRUE),
    rmse = sqrt(mean((events - predicted_corrected)^2, na.rm = TRUE)),
    mape = mean(abs((events - predicted_corrected) / pmax(events, 1)) * 100, na.rm = TRUE),
    coverage_95 = mean(events >= lower_95_final & events <= upper_95_final, na.rm = TRUE),
    bias_percent = (sum(predicted_corrected, na.rm = TRUE) - sum(events, na.rm = TRUE)) / sum(events, na.rm = TRUE) * 100,
    outlier_rate = mean(error_abs > quantile(error_abs, 0.95, na.rm = TRUE), na.rm = TRUE)
  )

cat("\nVALIDATION METRICS (CORRECTED):\n")
cat("- Correlation:", round(final_val_metrics$correlation, 3), "\n")
cat("- MAE:", round(final_val_metrics$mae, 1), "\n")
cat("- RMSE:", round(final_val_metrics$rmse, 1), "\n")
cat("- MAPE:", round(final_val_metrics$mape, 1), "%\n")
cat("- Coverage 95%:", round(final_val_metrics$coverage_95 * 100, 1), "%\n")
cat("- Bias:", round(final_val_metrics$bias_percent, 1), "%\n")

cat("\nTEST METRICS (CORRECTED):\n")
cat("- Correlation:", round(final_test_metrics$correlation, 3), "\n")
cat("- MAE:", round(final_test_metrics$mae, 1), "\n")
cat("- RMSE:", round(final_test_metrics$rmse, 1), "\n")
cat("- MAPE:", round(final_test_metrics$mape, 1), "%\n")
cat("- Coverage 95%:", round(final_test_metrics$coverage_95 * 100, 1), "%\n")
cat("- Bias:", round(final_test_metrics$bias_percent, 1), "%\n")

################################################################################
# COMPARISON PLOT
################################################################################

cat("\n=== CREATING COMPARISON PLOTS ===\n")

# Set theme
theme_set(theme_bw() + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    text = element_text(size = 12)
  ))

sex_colors <- c("female" = "#E91E63", "male" = "#2196F3")

# Before vs After comparison
comparison_data <- bind_rows(
  validation_preds %>% 
    select(events, predicted, sex) %>%
    filter(!is.na(predicted), !is.na(events)) %>%
    mutate(version = "Before (Biased)"),
  validation_preds_final %>%
    select(events, predicted = predicted_corrected, sex) %>%
    filter(!is.na(predicted), !is.na(events)) %>%
    mutate(version = "After (Corrected)")
)

p_comparison <- comparison_data %>%
  ggplot(aes(x = predicted, y = events)) +
  geom_point(aes(color = sex), alpha = 0.4, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  scale_color_manual(values = sex_colors, labels = c("Femmine", "Maschi")) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~version) +
  labs(
    title = "Confronto: Prima vs Dopo Correzioni",
    subtitle = "Bias correction + Coverage calibration + Outlier flagging",
    x = "Eventi Predetti (scala log)",
    y = "Eventi Osservati (scala log)",
    color = "Sesso"
  ) +
  annotation_logticks() +
  theme(legend.position = "bottom")

ggsave("output/ch4/model_fixes_comparison.png", p_comparison, width = 12, height = 6, dpi = 300, bg = "white")
cat("Saved: model_fixes_comparison.png\n")

################################################################################
# SAVE CORRECTED RESULTS
################################################################################

cat("\n=== SAVING CORRECTED RESULTS ===\n")

saveRDS(validation_preds_final, "input-data-processed/validation_predictions_corrected.RDS")
saveRDS(test_preds_final, "input-data-processed/test_predictions_corrected.RDS")
saveRDS(final_val_metrics, "input-data-processed/validation_metrics_corrected.RDS")
saveRDS(final_test_metrics, "input-data-processed/test_metrics_corrected.RDS")
saveRDS(problematic_municipalities, "input-data-processed/problematic_municipalities.RDS")

correction_summary <- list(
  bias_factor = bias_factor,
  coverage_multiplier = coverage_multiplier,
  problematic_municipalities = problematic_municipalities,
  before_metrics = list(validation = validation_metrics, test = test_metrics),
  after_metrics = list(validation = final_val_metrics, test = final_test_metrics)
)

saveRDS(correction_summary, "input-data-processed/model_corrections_summary.RDS")

cat("\n================================================================================\n")
cat("CRITICAL FIXES APPLIED SUCCESSFULLY!\n")
cat("================================================================================\n")
cat("\nIMPROVEMENTS:\n")
cat("- Bias: ", round((1 - bias_factor) * 100, 1), "% → ", round(final_val_metrics$bias_percent, 1), "%\n")
cat("- Coverage: ", round(validation_metrics$coverage_95 * 100, 1), "% → ", round(final_val_metrics$coverage_95 * 100, 1), "%\n")
cat("- Outliers identified: ", length(problematic_municipalities), " municipalities\n")
cat("- Correlation maintained: ", round(final_val_metrics$correlation, 3), "\n")
cat("\nFIXES APPLIED:\n")
cat("✅ Bias correction factor: ", round(bias_factor, 3), "\n")
cat("✅ Coverage calibration: ", round(coverage_multiplier, 1), "x interval width\n")
cat("✅ Outlier flagging: 95th percentile threshold\n")
cat("✅ Robust metrics calculation\n")
cat("================================================================================\n")
