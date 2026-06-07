# old-R/ch2_mpi_variance_plots.R
# Within-group marginalization-variance diagnostic plots, moved out of
# ch2_010_group_mun.R (cosmetic; overlaps ch1_060 MPI; was breaking the run).
# Not sourced by the pipeline; kept for reference.
# ============================================================================

if (!file.exists("input-data-processed/mpi_mun.RDS")) {
  message("ch2: skipping marginalization-variance plots — mpi_mun.RDS not built yet (produced by ch1_060_prepare_datasets).")
} else {
mpi_new_mun <- readRDS("input-data-processed/mpi_new_mun.RDS")
mpi_mun <- readRDS("input-data-processed/mpi_mun.RDS")

mpi_mun <- mpi_mun %>%
  select(mun, year, IMN)
  complete(mun, year = 2010:2020) %>%
  group_by(mun) %>%
  arrange(year, .by_group = TRUE) %>%
  group_modify(~ {
    non_na_vals <- .x %>% filter(!is.na(IMN))
    
    if (nrow(non_na_vals) >= 2) {
      .x$IMN <- approx(
        x = non_na_vals$year,
        y = non_na_vals$IMN,
        xout = .x$year
      )$y
    } else if (nrow(non_na_vals) == 1) {
      .x$IMN <- rep(non_na_vals$IMN, nrow(.x))
    } else {
      # all values are NA — leave them as NA
      .x$IMN <- NA_real_
    }
    
    return(.x)
  }) %>%
  ungroup()

mpi <- mpi_mun |>
  left_join(grouped_municipality_50000, by="mun") 


within_group_variance <- mpi |>
  filter(group_id != mun) |>
  group_by(group_id, year) |>
  summarise(
    mean_mpi = mean(IMN, na.rm = TRUE),
    var_mpi = var(IMN, na.rm = TRUE),
    .groups = "drop"
  )
small_variance_groups <- within_group_variance |>
  filter(var_mpi <= 1e-2)

nrow(small_variance_groups)  # total number of group-year combos with low variance


library(ggplot2)

ggplot(within_group_variance, aes(x = factor(year), y = var_mpi)) +
  geom_boxplot() +
  labs(
    title = "Within-Group Variance of IMN (MPI proxy)",
    x = "Year",
    y = "Variance of IMN"
  )


ggplot(within_group_variance, aes(x = year, y = var_mpi, group = group_id)) +
  geom_line(alpha = 0.3) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linewidth = 1) +
  labs(
    title = "Trends in Within-Group Variance of IMN",
    x = "Year",
    y = "Variance of IMN"
  )


################################################################################
#--------------------------- SENSITIVITY ANALYSIS ------------------------------
################################################################################
}


