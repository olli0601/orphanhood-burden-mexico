library(dplyr)
library(tidyr)
setwd("/Users/elsafarinella/Desktop/Orphanhood Mexico/R code")
# Ensure your dataset is loaded as deaths_df
# Columns needed: year_occurrence, year_register, municipality, sex, age_group, n

# Compute delay and group id
deaths_df <- readRDS("../datasets/mort.RDS") |>
  filter(year>=1990)

set.seed(123)
sample_munis <- sample(unique(deaths_df$group_id), 5)
deaths_df <- deaths_df |>
  filter(group_id %in% sample_munis, year >= 2005, year <= 2015)

# Define cutoff year
cutoff_year <- max(deaths_df$year) - 3

# Split into train/test based on year_occurrence
train_df <- deaths_df |>
  filter(year <= cutoff_year)

test_df <- deaths_df |>
  filter(year > cutoff_year)

# Get dimensions
years <- sort(unique(train_df$year))
groups <- sort(unique(train_df$group_id))
delays <- 0:max(deaths_df$delay)

# Aggregate counts
df_agg <- train_df |>
  group_by(year, group_id, delay) |>
  summarise(count = sum(tot_deaths), .groups = "drop")

# Complete cube
df_complete <- df_agg |>
  complete(year = years, group_id = groups, delay = delays, fill = list(count = 0))

# Build 3D array
T <- length(years)
S <- length(groups)
D <- length(delays)

z_tsd <- array(0, dim = c(T, S, D),
               dimnames = list(year = years, group = groups, delay = delays))

for (i in seq_along(years)) {
  for (j in seq_along(groups)) {
    tmp <- df_complete |> filter(year == years[i], group_id == groups[j])
    z_tsd[i, j, as.character(tmp$delay)] <- tmp$count
  }
}

# Save for model
dir.create("output", showWarnings = FALSE)
saveRDS(z_tsd, "output/z_tsd.rds")
saveRDS(years, "output/years.rds")
saveRDS(groups, "output/groups.rds")
saveRDS(delays, "output/delays.rds")
saveRDS(test_df, "output/test_df.rds")

