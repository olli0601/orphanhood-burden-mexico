# =============================================================================
# ch3_050_delay_analysis_births_and_deaths.R  ·  Chapter 3 — Delay vs rates (births & deaths)
# Relate BIRTH and DEATH registration delays to mortality/fertility rates and
# marginalization across municipalities.
#
# Reads : input-data-processed/{deaths, births, grouped_municipality_50000,
#         marg_index}.RDS
# Writes: output/ch3/ (figures)
# Run after: ch2_060, ch2_010
# =============================================================================

library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(readxl)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(foreign)
library(Polychrome)
library(sf)
source("R/grouped_mun.R")

source("R/marginalization.R")
fig_dir <- marg_fig_dir("output/ch3")  # output/ch3_fw when ORPHAN_MARG=fw

################################################################################

#-------------------------------- Mortality rate -------------------------------

################################################################################
# DATASET CONTAINING THE DEATHS PER YEAR, SEX, AGE AND MUNICIPALITY
# The mortality panels hold parental ages only (15-19 ... 80-84, plus unknown
# age), so every delay figure here is about adult deaths. (A former
# filter(!age %in% as.character(0:15)) matched no band label and did nothing.)
deaths <- readRDS("input-data-processed/deaths.RDS")
# GROUP BY THE GROUPED MUNICIPALITIES (R/grouped_mun.R)
deaths_grouped_mun <- aggregate_to_grouped_mun(deaths, "deaths")

# CALCOLATE THE DELAY
delay_df_mort <- deaths_grouped_mun |>
  mutate(delay = as.numeric(year_reg)- as.numeric(year))


# Thesis Fig 6. Counts EVENTS: this was summarise(n()), which counted panel
# rows, so a cell with 1 death weighed the same as one with 5,000. Long delays
# are spread over many tiny cells, which is why the row-count version showed
# d >= 5 dominating, contradicting the CDFs of Fig 7.
deaths_summary <- delay_df_mort %>%
  group_by(year, year_reg) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop") %>%
  mutate(
    delay = as.numeric(year_reg) - as.numeric(year),
    delay = ifelse(delay > 5, "5+", as.character(delay)),
    delay = factor(delay, levels = c("0", "1", "2", "3", "4", "5", "5+"))
  )


deaths_prop_reg <- deaths_summary %>%
  group_by(year_reg) %>%
  mutate(
    prop_deaths = total_deaths / sum(total_deaths)
  ) %>%
  ungroup()

deaths_prop_occ <- deaths_summary %>%
  group_by(year) %>%
  mutate(
    prop_deaths = total_deaths / sum(total_deaths)
  ) %>%
  ungroup()


delay_levels <- levels(factor(deaths_summary$delay))

# Generate colors and assign names
library(Polychrome)
seed_colors <- c("#FF0000", "#0000FF", "#00FF00")
# Polychrome::createPalette is stochastic: without a seed the delay-figure colours
# change on every run, so these figures were never reproducible. Verified by
# running an unmodified script twice and diffing the output.
set.seed(20250815)
my_colors <- createPalette(length(delay_levels), seedcolors = seed_colors)
names(my_colors) <- delay_levels

#---------- BARPLOT ------------
## Proportion -- DELAY IN THE FUTURE
p_deaths_delay_by_occurrence <- ggplot(deaths_prop_occ, aes(x = factor(year),
                            y = prop_deaths,
                            fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of occurrence",
    y = "Proportion of deaths",
    fill = "Delay (year)",
    title = "Reporting Delays for deaths by Year of Occurrence",
    subtitle = "Each bar shows the proportion of deaths by how many years later they were registered"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3)
  )
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_occurrence.pdf"), p_deaths_delay_by_occurrence, width = 8, height = 6)



## Proportion -- DELAY IN THE PAST
p_deaths_delay_by_registration <- ggplot(deaths_prop_reg, aes(x = factor(year_reg),
                            y = prop_deaths,
                            fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year of registration",
    y = "Proportion of deaths",
    fill = "Delay (year)",
    title = "Reporting Delays for Deaths by Year of Registration",
    subtitle = "Each bar shows the proportion of registered deaths by how many years earlier the death occurred"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80")
  )
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_registration.pdf"), p_deaths_delay_by_registration, width = 8, height = 6)



# JOIN WITH THE MPI
marg_index <- readRDS(marg_file("index_grouped_mun"))

marg_index <- marg_index |>
  mutate(
    # IMN is INVERTED in this dataset: cross-checking against CONAPO's own GM
    # labels gives mean IMN 0.72 for "Muy alto" (very high) marginalization and
    # 0.92 for "Muy bajo" (very low), i.e. HIGHER IMN = LESS marginalized.
    # ntile() assigns 1 to the smallest values, so ntile(IMN, 5) == 1 is the
    # lowest IMN = the MOST marginalized fifth. The previous code negated IMN
    # before ntile() and still labelled quintile 1 "Very High", which inverted
    # every category.
    marginalization_quintile = ntile(as.numeric(IMN), 5),
    mpi_label = case_when(
      marginalization_quintile == 1 ~ "Very High",
      marginalization_quintile == 2 ~ "High",
      marginalization_quintile == 3 ~ "Medium",
      marginalization_quintile == 4 ~ "Low",
      marginalization_quintile == 5 ~ "Very Low"
    ),
    mpi_label = factor(mpi_label, levels = c("Very Low", "Low", "Medium", "High", "Very High"))
  ) |>
  select(-c(GM, marginalization_quintile))

delay_df_mort <- delay_df_mort |>
  filter(year >=2010 & year <=2020) |>
  left_join(marg_index, by = c("group_id", "year"))|>
  filter(!(sex == "male" & is.na(age)))


# INTERACTION WITH SEX
p_deaths_delay_by_sex <- ggplot(delay_df_mort, aes(x = delay, fill = sex, weight = deaths)) +
  geom_bar(position = "dodge") +
  labs(title = "Delay Distribution by Sex", x = "Delay (years)", y = "Deaths") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_sex.pdf"), p_deaths_delay_by_sex, width = 8, height = 6)

# INTERACTION WITH AGE
p_deaths_delay_by_age <- ggplot(delay_df_mort, aes(x = delay, weight = deaths)) +
  geom_bar(fill = "#69b3a2") +
  facet_wrap(~ age) +
  labs(title = "Delay Distribution by Age Group", x = "Delay (years)", y = "Deaths") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_age.pdf"), p_deaths_delay_by_age, width = 8, height = 6)

# INTERACTION WITH MPI
p_deaths_delay_vs_imn <- ggplot(delay_df_mort, aes(x = as.numeric(IMN), y = delay)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Delay vs Marginalization Index", x = "MPI", y = "Delay (years)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_vs_imn.pdf"), p_deaths_delay_vs_imn, width = 8, height = 6)

p_deaths_delay_by_mpi_category <- ggplot(delay_df_mort, aes(x = mpi_label, y = delay, weight = deaths)) +
  geom_boxplot() +
  labs(title = "Delay by GM Category", x = "GM", y = "Delay (years)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_deaths_delay_by_mpi_category.pdf"), p_deaths_delay_by_mpi_category, width = 8, height = 6)

################################################################################

#-------------------------------- Fertility rate -------------------------------

################################################################################


# DATASET CONTAINING THE DEATHS PER YEAR, SEX, AGE AND MUNICIPALITY
births <- readRDS("input-data-processed/births.RDS")

# GROUP BY THE GROUPED MUNICIPALITIES (R/grouped_mun.R)
births_grouped_mun <- aggregate_to_grouped_mun(births, "births")

# CALCOLATE THE DELAY
delay_df <- births_grouped_mun |>
  mutate(delay = as.numeric(year_reg)- as.numeric(year))

# Thesis Fig 4. Counts EVENTS (was n(), counting panel rows; see Fig 6 above),
# on MOTHER rows only: every birth is on both a mother's and a father's row.
births_summary <- delay_df %>%
  filter(sex == "female") %>%
  group_by(year, year_reg) %>%
  summarise(total_births = sum(births), .groups = "drop") %>%
  mutate(
    delay = as.numeric(year_reg) - as.numeric(year),
    delay = ifelse(delay > 5, "5+", as.character(delay)),
    delay = factor(delay, levels = c("0", "1", "2", "3", "4", "5", "5+"))
  )


births_prop_reg <- births_summary %>%
  group_by(year_reg) %>%
  mutate(
    prop_births = total_births / sum(total_births)
  ) %>%
  ungroup()

births_prop_occ <- births_summary %>%
  group_by(year) %>%
  mutate(
    prop_births = total_births / sum(total_births)
  ) %>%
  ungroup()


delay_levels <- levels(factor(births_summary$delay))

# Generate colors and assign names
library(Polychrome)
seed_colors <- c("#FF0000", "#0000FF", "#00FF00")
# Polychrome::createPalette is stochastic: without a seed the delay-figure colours
# change on every run, so these figures were never reproducible. Verified by
# running an unmodified script twice and diffing the output.
set.seed(20250815)
my_colors <- createPalette(length(delay_levels), seedcolors = seed_colors)
names(my_colors) <- delay_levels

#---------- BARPLOT ------------
## Proportion -- DELAY IN THE FUTURE
p_births_delay_by_occurrence <- ggplot(births_prop_occ, aes(x = factor(year),
                            y = prop_births,
                            fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of occurrence",
    y = "Proportion of births",
    fill = "Delay (year)",
    title = "Reporting Delays for births by Year of Occurrence",
    subtitle = "Each bar shows the proportion of births by how many years later they were registered"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3)
  )
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_occurrence.pdf"), p_births_delay_by_occurrence, width = 8, height = 6)



## Proportion -- DELAY IN THE PAST
p_births_delay_by_registration <- ggplot(births_prop_reg, aes(x = factor(year_reg),
                            y = prop_births,
                            fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year of registration",
    y = "Proportion of births",
    fill = "Delay (year)",
    title = "Reporting Delays for births by Year of Registration",
    subtitle = "Each bar shows the proportion of registered births by how many years earlier the death occurred"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80")
  )
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_registration.pdf"), p_births_delay_by_registration, width = 8, height = 6)


# JOIN WITH THE MPI
marg_index <- readRDS(marg_file("index_grouped_mun"))

marg_index <- marg_index |>
  mutate(
    # IMN is INVERTED in this dataset: cross-checking against CONAPO's own GM
    # labels gives mean IMN 0.72 for "Muy alto" (very high) marginalization and
    # 0.92 for "Muy bajo" (very low), i.e. HIGHER IMN = LESS marginalized.
    # ntile() assigns 1 to the smallest values, so ntile(IMN, 5) == 1 is the
    # lowest IMN = the MOST marginalized fifth. The previous code negated IMN
    # before ntile() and still labelled quintile 1 "Very High", which inverted
    # every category.
    marginalization_quintile = ntile(as.numeric(IMN), 5),
    mpi_label = case_when(
      marginalization_quintile == 1 ~ "Very High",
      marginalization_quintile == 2 ~ "High",
      marginalization_quintile == 3 ~ "Medium",
      marginalization_quintile == 4 ~ "Low",
      marginalization_quintile == 5 ~ "Very Low"
    ),
    mpi_label = factor(mpi_label, levels = c("Very Low", "Low", "Medium", "High", "Very High"))
  ) |>
  select(-c(GM, marginalization_quintile))

delay_df <- delay_df |>
  filter(year >=2010 & year <=2020) |>
  left_join(marg_index, by = c("group_id", "year"))|>
  filter(!(sex == "male" & is.na(age)))


# INTERACTION WITH SEX
p_births_delay_by_sex <- ggplot(delay_df, aes(x = delay, fill = sex, weight = births)) +
  geom_bar(position = "dodge") +
  labs(title = "Delay Distribution by Sex", x = "Delay (years)", y = "Births") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_sex.pdf"), p_births_delay_by_sex, width = 8, height = 6)

# INTERACTION WITH AGE
p_births_delay_by_age <- ggplot(delay_df, aes(x = delay, weight = births)) +
  geom_bar(fill = "#69b3a2") +
  facet_wrap(~ age) +
  labs(title = "Delay Distribution by Age Group", x = "Delay (years)", y = "Births") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_age.pdf"), p_births_delay_by_age, width = 8, height = 6)

# INTERACTION WITH MPI
p_births_delay_vs_imn <- ggplot(delay_df, aes(x = as.numeric(IMN), y = delay, size = births)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Delay vs Marginalization Index", x = "MPI", y = "Delay (years)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_births_delay_vs_imn.pdf"), p_births_delay_vs_imn, width = 8, height = 6)

p_births_delay_by_mpi_category <- ggplot(delay_df, aes(x = mpi_label, y = delay, weight = births)) +
  geom_boxplot() +
  labs(title = "Delay by GM Category", x = "GM", y = "Delay (years)") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_050_births_delay_by_mpi_category.pdf"), p_births_delay_by_mpi_category, width = 8, height = 6)


################################################################################

#----------- Thesis §3.3: cumulative registration (Figs 5, 7), -----------------
#----------- registration completeness, mean delay by GM (Figs 8, 9) -----------
#----------- and by area type (Figs 10, 11) ------------------------------------

################################################################################
# Counts are EVENTS throughout. Births use mother rows only.
D_MAX <- 7   # thesis: common maximum observable delay

events <- dplyr::bind_rows(
  deaths_grouped_mun |> transmute(group_id, year, year_reg, n = deaths, event = "Deaths"),
  births_grouped_mun |> filter(sex == "female") |>
    transmute(group_id, year, year_reg, n = births, event = "Births")
) |>
  mutate(year = as.integer(year), year_reg = as.integer(year_reg), delay = year_reg - year) |>
  filter(year >= 1990, year <= 2023)
stopifnot(all(events$delay >= 0))

last_reg <- max(events$year_reg)
last_full_year <- last_reg - D_MAX   # last occurrence year with all d <= D_MAX observed
message(sprintf("ch3_050: registrations observed to %d; d <= %d fully observed only for occurrence years <= %d.",
                last_reg, D_MAX, last_full_year))

# --- Figs 5 / 7: cohort CDF of the registration delay, d = 0..D_MAX ----------
# Cohorts as in the thesis text. CDF(t) = events registered within t years /
# events registered within D_MAX years. The thesis says it uses "a common
# exposure window across cohorts" but does not define it; here every cohort is
# truncated at D_MAX only. Cohorts reaching past last_full_year are therefore
# RIGHT-CENSORED (their long delays are not yet observable) and their CDFs are
# biased upward - flagged in the legend. ch3_050_registration_cdf.csv also
# gives each cohort restricted to fully observed occurrence years for comparison.
cohort_of <- function(y) cut(y, c(1990, 2000, 2010, 2016, 2024), right = FALSE,
                             labels = c("1990-1999", "2000-2009", "2010-2015", "2016-2023"))
reg_cdf <- function(ev) ev |>
  filter(delay <= D_MAX) |>
  mutate(cohort = cohort_of(year)) |>
  group_by(event, cohort, delay) |>
  summarise(n = sum(n), .groups = "drop") |>
  tidyr::complete(tidyr::nesting(event, cohort), delay = 0:D_MAX, fill = list(n = 0)) |>
  arrange(event, cohort, delay) |>
  group_by(event, cohort) |>
  mutate(cdf = cumsum(n) / sum(n)) |>
  ungroup()

cdf_all  <- reg_cdf(events)
cdf_full <- reg_cdf(filter(events, year <= last_full_year))
censored <- events |> mutate(cohort = cohort_of(year)) |> group_by(cohort) |>
  summarise(censored = max(year) > last_full_year, .groups = "drop")
cdf_all <- cdf_all |> left_join(censored, by = "cohort") |>
  mutate(cohort_lab = ifelse(censored, paste0(cohort, " (right-censored)"), as.character(cohort)))

readr::write_csv(
  dplyr::full_join(select(cdf_all, event, cohort, delay, cdf_as_plotted = cdf),
                   select(cdf_full, event, cohort, delay, cdf_fully_observed_years = cdf),
                   by = c("event", "cohort", "delay")),
  file.path(fig_dir, "ch3_050_registration_cdf.csv"))

d99 <- cdf_all |> filter(cdf >= 0.99) |> group_by(event, cohort_lab) |>
  summarise(d99 = min(delay), .groups = "drop")

plot_cdf <- function(ev, title) {
  ggplot(filter(cdf_all, event == ev), aes(delay, cdf, colour = cohort_lab)) +
    geom_hline(yintercept = c(0.80, 0.95), colour = "grey60", linewidth = 0.3) +
    geom_vline(data = filter(d99, event == ev), aes(xintercept = d99, colour = cohort_lab),
               linetype = "dashed", linewidth = 0.4, show.legend = FALSE) +
    geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
    scale_x_continuous(breaks = 0:D_MAX) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Registration delay d (years)", y = sprintf("Share registered within d (of d <= %d)", D_MAX),
         colour = "Occurrence cohort", title = title,
         subtitle = "Grey lines: 80% and 95%. Dashed: smallest d with CDF >= 99%.") +
    theme_minimal()
}
ggsave(file.path(fig_dir, "ch3_050_births_registration_cdf.pdf"),
       plot_cdf("Births", "Cumulative registration by occurrence cohort: births"), width = 8, height = 5.5)
ggsave(file.path(fig_dir, "ch3_050_deaths_registration_cdf.pdf"),
       plot_cdf("Deaths", "Cumulative registration by occurrence cohort: deaths"), width = 8, height = 5.5)

# --- Registration completeness by occurrence year ----------------------------
# The quantity behind §3.3.5's "mortality does not require nowcasting": the
# share of ALL registrations observed so far that came in the occurrence year
# (d = 0) and within one year (d <= 1). Exact for years <= last_full_year (7+
# years of follow-up; anything later is negligible); an upper bound after that.
completeness <- events |>
  group_by(event, year) |>
  summarise(total = sum(n), share_d0 = sum(n[delay == 0]) / total,
            share_d01 = sum(n[delay <= 1]) / total,
            share_gt1 = 1 - share_d01, .groups = "drop") |>
  mutate(fully_observed = year <= last_full_year)
readr::write_csv(completeness, file.path(fig_dir, "ch3_050_registration_completeness.csv"))
completeness |> filter(fully_observed) |> group_by(event) |>
  summarise(years = paste(range(year), collapse = "-"),
            share_d0_min = min(share_d0), share_d0_max = max(share_d0),
            share_d01_min = min(share_d01), share_d01_max = max(share_d01), .groups = "drop") |>
  as.data.frame() |> print()

# --- Figs 8-11: unit-level mean delay by GM and by area type ------------------
# One value per grouped unit x occurrence year, 2010-2020 (the years the
# marginalization index covers): the EVENT-WEIGHTED mean delay. ch3_040 used an
# unweighted mean over panel cells. GM is the categorical grade carried by
# index_grouped_mun (not an ntile() of IMN). Later years have less follow-up
# (2020: d <= 4 observed), so their means are slightly truncated.
gm_index <- readRDS(marg_file("index_grouped_mun")) |>
  transmute(group_id, year = as.integer(year),
            GM = factor(GM, levels = c("Very Low", "Low", "Medium", "High", "Very High")))
area <- readRDS("input-data-processed/rural_urban_area.RDS") |> select(group_id, area_type)

unit_delay <- events |>
  filter(year >= 2010, year <= 2020) |>
  group_by(event, group_id, year) |>
  summarise(mean_delay = weighted.mean(delay, n), n = sum(n), .groups = "drop") |>
  filter(n > 0) |>
  left_join(gm_index, by = c("group_id", "year")) |>
  left_join(area, by = "group_id")
stopifnot(!anyNA(unit_delay$GM), !anyNA(unit_delay$area_type))

plot_box <- function(ev, x, xlab, title) {
  ggplot(filter(unit_delay, event == ev), aes(.data[[x]], mean_delay)) +
    geom_boxplot(fill = "#69b3a2", outlier.size = 0.6) +
    labs(x = xlab, y = "Mean registration delay (years)", title = title,
         subtitle = "One point per grouped unit and occurrence year, 2010-2020; event-weighted") +
    theme_minimal()
}
ggsave(file.path(fig_dir, "ch3_050_births_mean_delay_by_gm.pdf"),
       plot_box("Births", "GM", "Degree of marginalization (GM)", "Births: mean delay by GM"), width = 8, height = 5.5)
ggsave(file.path(fig_dir, "ch3_050_deaths_mean_delay_by_gm.pdf"),
       plot_box("Deaths", "GM", "Degree of marginalization (GM)", "Deaths: mean delay by GM"), width = 8, height = 5.5)
ggsave(file.path(fig_dir, "ch3_050_births_mean_delay_by_area.pdf"),
       plot_box("Births", "area_type", "Area type", "Births: mean delay by area type"), width = 8, height = 5.5)
ggsave(file.path(fig_dir, "ch3_050_deaths_mean_delay_by_area.pdf"),
       plot_box("Deaths", "area_type", "Area type", "Deaths: mean delay by area type"), width = 8, height = 5.5)

unit_delay |> group_by(event, GM) |>
  summarise(median = median(mean_delay), q75 = quantile(mean_delay, 0.75), max = max(mean_delay), .groups = "drop") |>
  as.data.frame() |> print()
unit_delay |> group_by(event, area_type) |>
  summarise(median = median(mean_delay), q75 = quantile(mean_delay, 0.75), max = max(mean_delay), .groups = "drop") |>
  as.data.frame() |> print()
