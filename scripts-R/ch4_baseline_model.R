# Load required packages
library(dplyr)
library(tidyr)

# Read your dataset
deaths <- readRDS("../datasets/deaths_new_mun.RDS")
to_exclude <- c(as.character(1:15), "80-84")

df <- deaths[!deaths$age %in% to_exclude, ]

df <- df |>
  rename(registered_year = year_reg, occurred_year = year) |>
  mutate(registered_year = as.numeric(registered_year), occurred_year = as.numeric(occurred_year))

# Step 1: Compute reporting delay
df <- df %>%
  mutate(delay = registered_year - occurred_year)

# Step 2: Filter to reasonable delays (e.g., 0 to 5 years)
MAX_DELAY <- 10
df <- df %>% filter(delay >= 0 & delay <= MAX_DELAY)

# Step 3: Create the reporting triangle (count of deaths per occurred year and delay)
triangle <- df %>%
  count(occurred_year, delay) %>%
  pivot_wider(names_from = delay, values_from = n, values_fill = 0) %>%
  arrange(occurred_year)

# Fill missing delay columns (optional but safer)
for (d in 0:MAX_DELAY) {
  col_name <- as.character(d)
  if (!(col_name %in% names(triangle))) {
    triangle[[col_name]] <- 0
  }
}
triangle <- triangle %>% select(occurred_year, as.character(0:MAX_DELAY))

# Step 4: Estimate delay distribution from complete years only
latest_complete_year <- 2021  # adjust based on what you know
complete_triangle <- triangle %>% filter(occurred_year <= latest_complete_year)

delay_totals <- colSums(complete_triangle[ , -1])
delay_probs <- delay_totals / sum(delay_totals)

# Step 5: Nowcast for incomplete year (e.g., 2023, given data available in early 2024)
current_year <- 2023
incomplete_year <- current_year - 1

# Get reported delays for the incomplete year
reported <- triangle %>% filter(occurred_year == incomplete_year)
if (nrow(reported) == 0) stop("No data for the incomplete year.")

observed_delays <- as.numeric(colnames(reported)[-1])
max_observed_delay <- current_year - incomplete_year
observed_total <- sum(reported[ , 2:(max_observed_delay + 2)])

# Cumulative delay probability up to the current delay
F_observed <- sum(delay_probs[1:(max_observed_delay + 1)])

# Nowcast total deaths
nowcasted_total <- observed_total / F_observed

cat(sprintf("Nowcasted total deaths for %d: %.0f\n", incomplete_year, nowcasted_total))


library(ggplot2)

# Step 1: Compute total reported deaths per occurred year
reported_totals <- triangle |>
  mutate(total_reported = rowSums(across(as.character(0:MAX_DELAY)))) |>
  select(occurred_year, total_reported)

# Step 2: Get partial 2023 data
partial_2023 <- reported_totals |>
  filter(occurred_year == incomplete_year) |>
  pull(total_reported)

# Step 3: Nowcast value as separate point
nowcast_df <- tibble(
  occurred_year = incomplete_year,
  nowcasted = nowcasted_total
)

# Step 4: Plot
ggplot(reported_totals, aes(x = occurred_year)) +
  # Reported deaths line (2000–2023 including partial)
  geom_line(aes(y = total_reported), color = "blue", size = 1.2) +
  
  # Partial 2023 point
  geom_point(
    data = filter(reported_totals, occurred_year == incomplete_year),
    aes(y = total_reported),
    color = "orange", size = 3
  ) +
  
  # Nowcasted 2023 point
  geom_point(
    data = nowcast_df,
    aes(y = nowcasted),
    color = "red", size = 3
  ) +
  
  # Dashed line connecting partial and nowcasted
  geom_segment(
    aes(
      x = incomplete_year, xend = incomplete_year,
      y = partial_2023, yend = nowcasted_total
    ),
    color = "red", linetype = "dashed"
  ) +
  
  labs(
    title = "Reported vs Nowcasted Deaths",
    x = "Year of Occurrence",
    y = "Number of Deaths",
    caption = "Blue line: Reported deaths\nOrange dot: Reported (partial) 2023\nRed dot: Nowcasted 2023"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, current_year - 1, 2))


