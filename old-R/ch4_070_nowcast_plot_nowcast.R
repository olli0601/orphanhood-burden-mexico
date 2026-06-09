# =============================================================================
# ch4_070_nowcast_plot_nowcast.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Plot nowcast predictions vs observed registrations.
# Reads input-data-processed/{posterior_samples,years,groups}.rds -> output/ch4/ figures.
# =============================================================================

library(coda)
library(dplyr)
library(ggplot2)

samples <- readRDS("input-data-processed/posterior_samples.rds")
years <- readRDS("input-data-processed/years.rds")
groups <- readRDS("input-data-processed/groups.rds")

# Extract posterior y
posterior_y <- do.call(rbind, samples)
y_cols <- grep("^y\\[", colnames(posterior_y))

posterior_df <- as.data.frame(posterior_y[, y_cols])
colnames(posterior_df) <- paste0("y", seq_len(ncol(posterior_df)))

# Summary
posterior_summary <- posterior_df |>
  summarise(across(everything(), list(mean = mean, lower = ~quantile(.x, 0.025), upper = ~quantile(.x, 0.975)))) |>
  pivot_longer(cols = everything(), names_to = c("y", "stat"), names_sep = "_") |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(index = as.integer(gsub("y", "", y)))

# Merge with mapping
index_map <- expand.grid(year = years, group = groups)
plot_df <- bind_cols(index_map, posterior_summary)

# Split group
plot_df <- plot_df |>
  separate(group, into = c("municipality", "sex", "age_group"), sep = "_")

# Aggregate and plot
agg_plot_df <- plot_df |>
  group_by(year, sex, age_group) |>
  summarise(mean = sum(mean), lower = sum(lower), upper = sum(upper), .groups = "drop")

ggplot(agg_plot_df, aes(x = year, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.5) +
  geom_line(color = "blue") +
  facet_grid(sex ~ age_group) +
  labs(title = "Nowcasted Deaths with 95% Credible Intervals", x = "Year", y = "Estimated Deaths")
