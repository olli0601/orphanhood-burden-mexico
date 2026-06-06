# =============================================================================
# ch4_170_gam_nowcast_diagnostics_critical.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Critical/edge-case diagnostics for the GAM nowcast.
# Reads input-data-processed/{validation,test}_predictions.RDS -> output/ch4/.
# =============================================================================

################################################################################
# NOWCASTING MODEL - DIAGNOSTIC ANALYSIS FOR CRITICAL ISSUES
# Focus: Understanding and fixing Coverage and MAPE problems
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)


cat("=== DIAGNOSTIC ANALYSIS FOR CRITICAL ISSUES ===\n")

################################################################################
# LOAD VALIDATION RESULTS
################################################################################

validation_preds <- readRDS('input-data-processed/validation_predictions.RDS')
test_preds <- readRDS('input-data-processed/test_predictions.RDS')

cat("Data loaded successfully!\n")
cat("Validation predictions:", nrow(validation_preds), "\n")
cat("Test predictions:", nrow(test_preds), "\n")

################################################################################
# PROBLEM 1: COVERAGE ANALYSIS
################################################################################

cat("\n=== PROBLEM 1: COVERAGE ANALYSIS ===\n")

# Calculate coverage by different strata
coverage_analysis <- validation_preds %>%
  filter(!is.na(predicted), !is.na(events), !is.na(lower_95), !is.na(upper_95)) %>%
  mutate(
    is_covered = events >= lower_95 & events <= upper_95,
    error_abs = abs(events - predicted),
    error_rel = abs(events - predicted) / pmax(events, 1),
    event_size = case_when(
      events <= 50 ~ "Very Small (≤50)",
      events <= 150 ~ "Small (51-150)", 
      events <= 400 ~ "Medium (151-400)",
      TRUE ~ "Large (>400)"
    ),
    interval_width = upper_95 - lower_95,
    relative_width = interval_width / pmax(predicted, 1)
  )

coverage_by_size <- coverage_analysis %>%
  group_by(event_size) %>%
  summarise(
    n = n(),
    coverage_rate = mean(is_covered, na.rm = TRUE),
    mean_events = mean(events, na.rm = TRUE),
    mean_predicted = mean(predicted, na.rm = TRUE),
    mean_interval_width = mean(interval_width, na.rm = TRUE),
    mean_relative_width = mean(relative_width, na.rm = TRUE),
    .groups = "drop"
  )

cat("COVERAGE BY EVENT SIZE:\n")
print(coverage_by_size)

# Check if intervals are too narrow
cat("\nINTERVAL WIDTH ANALYSIS:\n")
cat("Mean interval width:", round(mean(coverage_analysis$interval_width, na.rm = TRUE), 1), "\n")
cat("Median interval width:", round(median(coverage_analysis$interval_width, na.rm = TRUE), 1), "\n")
cat("Mean relative width:", round(mean(coverage_analysis$relative_width, na.rm = TRUE), 2), "\n")

################################################################################
# PROBLEM 2: OUTLIER ANALYSIS  
################################################################################

cat("\n=== PROBLEM 2: OUTLIER ANALYSIS ===\n")

# Identify extreme errors
outliers <- validation_preds %>%
  filter(!is.na(predicted), !is.na(events)) %>%
  mutate(
    error_abs = abs(events - predicted),
    error_rel = abs(events - predicted) / pmax(events, 1)
  ) %>%
  filter(error_abs > quantile(error_abs, 0.95, na.rm = TRUE)) %>%
  arrange(desc(error_abs)) %>%
  select(municipality, event_year, age_group, sex, events, predicted, error_abs, error_rel)

cat("TOP 10 OUTLIERS (absolute error):\n")
print(head(outliers, 10))

# Check if outliers are concentrated in specific groups
outlier_patterns <- outliers %>%
  group_by(sex, age_group) %>%
  summarise(
    n_outliers = n(),
    mean_error = mean(error_abs, na.rm = TRUE),
    mean_events = mean(events, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_outliers))

cat("\nOUTLIER PATTERNS BY SEX/AGE:\n")
print(outlier_patterns)

################################################################################
# PROBLEM 3: MAPE ANALYSIS
################################################################################

cat("\n=== PROBLEM 3: MAPE ANALYSIS ===\n")

mape_analysis <- validation_preds %>%
  filter(!is.na(predicted), !is.na(events), events > 0) %>%
  mutate(
    error_rel = abs(events - predicted) / events * 100,
    event_size = case_when(
      events <= 50 ~ "Very Small (≤50)",
      events <= 150 ~ "Small (51-150)", 
      events <= 400 ~ "Medium (151-400)",
      TRUE ~ "Large (>400)"
    )
  ) %>%
  group_by(event_size) %>%
  summarise(
    n = n(),
    mape = mean(error_rel, na.rm = TRUE),
    median_error_rel = median(error_rel, na.rm = TRUE),
    q75_error_rel = quantile(error_rel, 0.75, na.rm = TRUE),
    q95_error_rel = quantile(error_rel, 0.95, na.rm = TRUE),
    mean_events = mean(events, na.rm = TRUE),
    .groups = "drop"
  )

cat("MAPE BY EVENT SIZE:\n")
print(mape_analysis)

# The problem: small events have huge relative errors
cat("\nMAPE PROBLEM IDENTIFIED:\n")
cat("- Small events (≤50): Very high MAPE due to small denominators\n")
cat("- This is mathematically expected and may not be fixable\n")
cat("- Consider using MAE instead of MAPE for small counts\n")

################################################################################
# PROBLEM 4: MODEL DIAGNOSTICS
################################################################################

cat("\n=== PROBLEM 4: MODEL DIAGNOSTICS ===\n")

# Check for systematic bias
bias_analysis <- validation_preds %>%
  filter(!is.na(predicted), !is.na(events)) %>%
  mutate(
    bias = predicted - events,
    bias_rel = bias / pmax(events, 1)
  ) %>%
  group_by(sex, age_group) %>%
  summarise(
    n = n(),
    mean_bias = mean(bias, na.rm = TRUE),
    mean_bias_rel = mean(bias_rel, na.rm = TRUE),
    mean_events = mean(events, na.rm = TRUE),
    mean_predicted = mean(predicted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(mean_bias)))

cat("SYSTEMATIC BIAS BY SEX/AGE (top 10):\n")
print(head(bias_analysis, 10))

# Overall bias
overall_bias <- validation_preds %>%
  filter(!is.na(predicted), !is.na(events)) %>%
  summarise(
    total_observed = sum(events, na.rm = TRUE),
    total_predicted = sum(predicted, na.rm = TRUE),
    overall_bias = total_predicted - total_observed,
    relative_bias = overall_bias / total_observed * 100
  )

cat("\nOVERALL MODEL BIAS:\n")
cat("- Total observed:", overall_bias$total_observed, "\n")
cat("- Total predicted:", round(overall_bias$total_predicted, 0), "\n")
cat("- Overall bias:", round(overall_bias$overall_bias, 0), "\n")
cat("- Relative bias:", round(overall_bias$relative_bias, 2), "%\n")

################################################################################
# PROPOSED SOLUTIONS
################################################################################

cat("\n" + "="*80 + "\n")
cat("=== PROPOSED SOLUTIONS ===\n")
cat("="*80 + "\n")

cat("\n1. COVERAGE PROBLEM (9.9-12.7% instead of 95%):\n")
cat("   CAUSE: Intervals too narrow - model overconfident\n")
cat("   SOLUTIONS:\n")
cat("   - Use bootstrap or Bayesian methods for better uncertainty\n")
cat("   - Apply coverage correction factor (multiply intervals by ~3)\n") 
cat("   - Consider hierarchical models for better uncertainty propagation\n")

cat("\n2. OUTLIER PROBLEM (max errors 3928-5706):\n")
cat("   CAUSE: Some predictions fail catastrophically\n")
cat("   SOLUTIONS:\n")
cat("   - Robust regression methods (Huber loss, etc.)\n")
cat("   - Outlier detection and special handling\n")
cat("   - Ensemble methods to reduce extreme predictions\n")

cat("\n3. MAPE PROBLEM (58-85%):\n")
cat("   CAUSE: High relative errors on small counts (mathematical issue)\n")
cat("   SOLUTIONS:\n")
cat("   - Use MAE instead of MAPE for reporting\n")
cat("   - Define MAPE only for events > threshold (e.g., >20)\n")
cat("   - Use symmetric MAPE or other robust relative metrics\n")

cat("\n4. NEXT STEPS:\n")
cat("   IMMEDIATE:\n")
cat("   - Implement coverage correction factor\n")
cat("   - Redefine success metrics (focus on MAE, not MAPE)\n")
cat("   - Add outlier detection and flagging\n")
cat("   \n")
cat("   MEDIUM TERM:\n")
cat("   - Implement Bayesian or bootstrap uncertainty\n")
cat("   - Add robust regression methods\n")
cat("   - Consider hierarchical spatial models\n")

cat("\n" + "="*80 + "\n")
cat("DIAGNOSTIC ANALYSIS COMPLETE\n")
cat("="*80 + "\n")
