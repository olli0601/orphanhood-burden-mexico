library(dplyr)
library(ggplot2)

samples <- readRDS("results/posterior_samples.rds")
posterior_y <- do.call(rbind, samples)
y_cols <- grep("^y\\[", colnames(posterior_y))

posterior_df <- as.data.frame(posterior_y[, y_cols])
posterior_mean <- colMeans(posterior_df)

# Mapping back to index
years <- readRDS("output/years.rds")
groups <- readRDS("output/groups.rds")
index_map <- expand.grid(year = years, group = groups)

test_df <- readRDS("output/test_df.rds") |>
  mutate(group = paste(municipality, sex, age_group, sep = "_"))

# Get true values for test period
truth <- test_df |>
  group_by(year_occurrence, group) |>
  summarise(true_total = sum(n), .groups = "drop") |>
  rename(year = year_occurrence)

# Extract posterior predictions for test years
pred_df <- data.frame(index_map, pred = posterior_mean)
pred_test <- pred_df |>
  inner_join(truth, by = c("year", "group"))

# Metrics
rmse <- sqrt(mean((pred_test$pred - pred_test$true_total)^2))
coverage <- mean(pred_test$pred >= pred_test$true_total * 0.975 & pred_test$pred <= pred_test$true_total * 1.025)

cat("RMSE: ", round(rmse, 2), "\n")
cat("95% coverage (±2.5% of truth): ", round(coverage * 100, 1), "%\n")

# Plot prediction vs. truth
ggplot(pred_test, aes(x = true_total, y = pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Posterior Mean vs. True Deaths (Test Set)", x = "True", y = "Predicted")
