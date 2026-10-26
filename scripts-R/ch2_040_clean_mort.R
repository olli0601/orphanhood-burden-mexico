# =============================================================================
# ch2_040_clean_mort.R  ·  Chapter 2 — Mortality cleaning (original municipalities)
# Clean and harmonise the per-year INEGI registered-death files into death
# counts by municipality x sex x 5-year age group x year; build the population
# and geographic-lookup tables used downstream. Works at ORIGINAL-municipality
# level and writes mort.RDS. The grouped-municipality counterpart is
# ch3_010_clean_mortality_by_grouped_mun.R, which writes mort_by_grouped_mun.RDS.
#
# Loads the supplied per-year mortality (input-data-processed/mortality datasets/),
# joins population (R/load_population) + IMM_2020 + the ch2 grouping.
# Reads : input-data-processed/mortality datasets/*.RDS, grouped_municipality_50000.RDS (ch2),
#         input-data-raw/population/...1_Grupo_Quinq..., input-data-raw/marginalization/IMM_2020.xls
# Writes: input-data-processed/mort.RDS
#         (population.RDS / geo_info.RDS are NOT written here — see the notes
#          at those objects; ch1_015 and ch2_060 own them)
# Run after: ch1_015, ch2_010
# =============================================================================

#############################################

#Data pre-processing

#############################################

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
source("R/preprocess_mortality.R"); source("R/rates.R"); source("R/plots.R")
source("R/load_population.R")
source("R/marginalization.R")
source("R/load_year_panels.R")
source("R/grouped_mun.R")

fig_dir <- "output/ch2"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Load the SUPPLIED per-year mortality datasets into mort_YYYY objects. These
# are the output of preprocess_mortality() on the raw INEGI files; loading them
# here lets the pipeline start from input-data-processed/ without the raw files.
# (To regenerate from raw instead, loop the INEGI files through
# preprocess_mortality() as in ch1_015 / the git history.)
# Run ch1_015_bootstrap_from_processed.R first.
mort_panel <- load_year_panels("mortality datasets")


## BARPLOT 
# Get all objects whose names start with "mort_"
all_mort <- mort_panel

# Summarize data: count occurrences of each 'year' within each 'year_reg'
data_summary <- all_mort %>%
  filter(year >= as.numeric(year_reg) - 20) %>%  # Keep only deaths in the last 20 years before registration
  group_by(year_reg, year) %>%
  # sum(deaths), not n(): n() counts PANEL ROWS, so a mun-sex-age cell holding
  # 1 death weighed the same as one holding 5,000. Long-delay registrations are
  # spread over many tiny cells, so counting rows inflated their apparent share
  # and inverted the delay distribution these figures exist to show.
  summarise(count = sum(deaths), .groups = "drop") %>%
  group_by(year_reg) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ungroup()


library(RColorBrewer)
length(unique(data_summary$year))
color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(data_summary$year)))  # Generate 101 colors

# Plot the data with the custom color palette
p <- ggplot(data_summary, aes(x = factor(year_reg), y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3) +
  labs(x = "Registration Year",
       y = "Total Deaths Count",
       fill = "Death Year",
       title = "Registered Deaths by Year and Actual Death Year") +
  scale_fill_manual(values = color_palette) +  # Apply the generated color palette
  theme_minimal() +
  facet_wrap(~year_reg, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.box.spacing = unit(0.5, "cm"))

ggsave(file.path(fig_dir, "ch2_040_reg_deaths_by_year.pdf"), p, width = 12, height = 8)

# Convert the data to a proportion for pie charts
data_summary_pie <- data_summary %>%
  group_by(year_reg) %>%
  mutate(perc = round(count / sum(count) * 100, 2))

data_summary_pie <- data_summary_pie %>%
  group_by(year_reg) %>%
  mutate(is_max = perc == max(perc)) %>%
  ungroup()

p_pie <- ggplot(data_summary_pie, aes(x = "", y = perc, fill = factor(year))) +
  geom_bar(stat = "identity", width = 0.5, color = "white", size = 0.1) +
  geom_text(data = subset(data_summary_pie, is_max), 
            aes(label = paste0(perc, "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~year_reg) +
  theme_void() + 
  labs(title = "Death Year Distribution per Registration Year") +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  scale_fill_manual(values = rainbow(length(unique(data_summary_pie$year))))

ggsave(file.path(fig_dir, "ch2_040_death_year_dist_pie.pdf"), p_pie, width = 12, height = 8)

# Combine the cleaned datasets together and aggregate counts in case of duplicate groups
deaths <- mort_panel |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(deaths = sum(deaths), .groups = "drop")

deaths <- deaths |> filter(year >= 1990 & year < 2024)
# Attach group_id (+ names) from the ch2 grouping; keeps original-mun rows (mort.RDS
# stays mun-level). attach_group_id drops sf geometry. See R/grouped_mun.R.
deaths <- attach_group_id(deaths, cols = c("state_name", "mun_name", "mun", "group_id"))

# --------- GROUP BY THE NEW MUNICIPALITIES ---------------
deaths <- deaths |>
  group_by(group_id, year, sex, age) |>
  mutate(tot_deaths = sum(deaths))

# 1. Aggregate total deaths by occurrence year and reporting year
deaths_summary <- deaths %>%
  group_by(year, year_reg) %>%
  # sum(deaths), not n() — see the note on data_summary above.
  summarise(total_deaths = sum(deaths), .groups = "drop") %>%
  add_delay(max_delay = 5)


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
## Total deaths
p_total_deaths_delay <- ggplot(deaths_summary, aes(x = factor(year_reg), y = log(total_deaths), fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of Registration",
    y = "Total Deaths",
    fill = "Delay (Years)",
    title = "Total Deaths by Occurrence Year and Reporting Year"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch2_040_total_deaths_delay.pdf"), p_total_deaths_delay, width = 8, height = 6)

## Proportion -- DELAY IN THE FUTURE
p_delay_by_occurrence <- ggplot(deaths_prop_occ, aes(x = factor(year), y = (prop_deaths), fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023))+
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of occurrence",
    y = "Proportion of deaths",
    fill = "Delay (year)",
    title = "Reporting Delays for Deaths by Year of Occurrence",
    subtitle = "Each bar shows the proportion of deaths by how many years later they were registered"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch2_040_delay_by_occurrence.pdf"), p_delay_by_occurrence, width = 8, height = 6)


## Proportion -- DELAY IN THE PAST
p_delay_by_registration <- ggplot(deaths_prop_reg, aes(x = factor(year_reg), y = prop_deaths, fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = as.character(1990:2023)) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of registration",
    y = "Proportion of deaths",
    fill = "Delay (year)",
    title = "Reporting Delays for Deaths by Year of Registration",
    subtitle = "Each bar shows the proportion of registered deaths by how many years earlier the death occurred"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch2_040_delay_by_registration.pdf"), p_delay_by_registration, width = 8, height = 6)
#------

p_delay_pct_by_reg <- ggplot(deaths_prop_reg, aes(x = factor(year_reg), y = prop_deaths, fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year of Occurrence",
    y = "Percentage of Deaths",
    fill = "Year Reported",
    title = "Proportion of Deaths by Occurrence Year and Reporting Delay"
  ) +
  theme_minimal()
ggsave(file.path(fig_dir, "ch2_040_delay_pct_by_reg.pdf"), p_delay_pct_by_reg, width = 8, height = 6)




##############################################

# Population
population <- load_population(keep_child = FALSE)

# NOT written to input-data-processed/population.RDS. ch1_015 already wrote a
# byte-identical object from the same load_population(keep_child = FALSE) call,
# and ch2_060 overwrites it again with a DIFFERENT one (keep_child = TRUE, i.e.
# with the 0-4/5-9 bands Chapter 5 needs). Three writers meant the file's meaning
# depended on which script ran last, with nothing on disk recording that — so
# re-running ch2_040 alone silently reverted population.RDS to the age-restricted
# vintage. This write was pure duplication and pure hazard.
mort <- full_join(deaths |> select(mun, group_id, year, sex, age, year_reg, deaths, tot_deaths), population, by = c("mun", "year", "sex", "age"))

mort$tot_deaths[is.na(mort$tot_deaths)] <- 0
mort$deaths[is.na(mort$deaths)] <- 0

#marginality index

index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)

index <- index[-1,]

#IM - marginalization index
#IMN - normalized marginalization index
# GM- degree of marginalization

im.tmp <- index |> dplyr:: select(mun, IM, IMN, illiterate_pct, no_basic_edu_pct, 
                                  no_drainage_pct, no_electricity_pct, no_piped_water_pct, 
                                  dirt_floors_pct, overcrowding_pct, small_towns_pct, low_income_pct)
im.tmp$mun <- as.factor(im.tmp$mun)
im.tmp$IM <- as.numeric(im.tmp$IM)
im.tmp$IMN <- as.numeric(im.tmp$IMN)

mort <- mort |> full_join(im.tmp, by = c("mun"))

##some municipalities don't have IM information (??)
##some municipalities have population of 0

mort$mort_rate <- mort$deaths / mort$population

# ⚠️ drop_na() here removes a LARGE and previously unreported share of deaths.
# Measured on the current panel: 989,725 deaths, 8.85% of the 11.7M total.
#
#   926,155 (8.28%)  age band 80-84 — present in the deaths panel but absent
#                    from the population table, because load_population() is
#                    called above with keep_old = FALSE (which drops 80-84 and
#                    85-mm). These deaths have no denominator, so every
#                    age-standardised mortality rate in this script excludes
#                    them. ONE-WORD FIX, but it changes every ch2_040 rate and
#                    figure, so it is left for the researchers:
#                    load_population(keep_child = FALSE, keep_old = TRUE).
#    63,570 (0.57%)  age unknown (INEGI EDAD 4998/4999). These are deliberately
#                    kept upstream as age = NA and genuinely cannot be
#                    age-standardised — dropping them is correct, reporting
#                    them is not optional.
#
# The full_join above is NOT the problem: it completes the grid with
# population cells that have no deaths, and those are zero-filled a few lines
# up, which is right for a rate panel.
n_before <- nrow(mort); d_before <- sum(mort$deaths, na.rm = TRUE)
mort <- mort |> drop_na()
message(sprintf("ch2_040: drop_na() removed %s rows and %s deaths (%.2f%% of all deaths).",
                format(n_before - nrow(mort), big.mark = ","),
                format(round(d_before - sum(mort$deaths)), big.mark = ","),
                100 * (d_before - sum(mort$deaths)) / d_before))


saveRDS(mort, file = "input-data-processed/mort.RDS")



geo_info <- build_geo_info(index)

# Also not written, for the same reason: ch1_015 writes an identical object
# (same load_imm -> [-1, ] -> build_geo_info path), and ch2_060 later writes a
# different one joined to group_id and filtered. ch2_050 reads geo_info.RDS
# between here and there and now gets ch1_015's version, which is identical.

#----------------------- Standardized mortality rate ---------------------------
# Computed ONCE. This used to be recomputed on every one of the 34 loop
# iterations below, plus twice more here and at the multi-year figure: measured
# at 8.4 s per call, that was ~283 s of a ~300 s loop for a result that does not
# depend on the loop variable. The only per-year part is filter(year == yr).
# Aggregate over year_reg FIRST. `mort` carries one row per registration year
# (1.11 rows per mun-year-sex-age), so standardising it directly would add each
# age band's contribution once per year_reg.
mort_mun <- mort %>%
  group_by(mun, year, sex, age) %>%
  summarise(deaths = sum(deaths), population = dplyr::first(population), .groups = "drop") %>%
  mutate(mort_rate = deaths / population)

# compute_std_mort_rate() rather than the retired compute_std_rate(): the latter
# was an unverified reconstruction that skipped .drop_unstandardisable() (10 of
# 82,212 rates came back Inf from population == 0) and built its reference
# population with sum() over a frame still multiplied by year_reg. This helper is
# the one ch2_020 and Chapter 3 use, and it reports what it drops.
#
# Both-sex standardised death rate (by_sex = FALSE: one weight total over both
# sexes). For FERTILITY a both-sex rate would double-count every birth — see ch2_050.
std_all <- compute_std_mort_rate(mort_mun, unit_col = "mun", by_sex = FALSE) %>%
  dplyr::select(-sex) %>%
  left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>%
  left_join(y = index, by = "mun") %>%
  mutate(capital = factor(capital)) %>%
  rename(mpi = IMN) %>%
  mutate(mpi = as.numeric(mpi))

std_raw <- std_all %>% filter(year == 2023) %>% dplyr::select(-year)
# (removed) min_max_norm(overcrowding_pct) — plot_std_rate() reads only `mpi`
# and `capital`, so this rescaled a column that never reaches a figure. It was
# the sole live call to an unverified reconstruction; retired to old-R/.

# ---------------------Plot mortality rate against mpi -------------------------
p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (mortality)")
ggsave(file.path(fig_dir, "ch2_040_std_rate_2023.pdf"), p_raw_pts, width = 8, height = 6)

#------------------------ Iterate over the years -------------------------------
years <- seq(1990, 2023, 1)

for (yr in years) {
  std_yr <- std_all %>% filter(year == yr) %>% dplyr::select(-year)
  p_raw_pts <- plot_std_rate(data = std_yr, tt = paste("Raw data (mortality) - Year", yr))
  ggsave(file.path(fig_dir, paste0("ch2_040_std_rate_", yr, ".pdf")), p_raw_pts, width = 8, height = 6)
}
# (removed) grid.arrange(grobs = plot_list, ...) — plot_list was initialised empty
# and never appended to, so this emitted a blank page to the default device.

std_raw_all <- std_all %>% filter(year %in% 2005:2023)

# Title said "2012 to 2023"; the filter one line above is 2005:2023.
p_raw_pts <- plot_std_rate(data = std_raw_all, tt = "Raw data (mortality) - Years 2005 to 2023") +
  facet_wrap(~ year)

ggsave(file.path(fig_dir, "ch2_040_std_rate_multiyear.pdf"), p_raw_pts, width = 12, height = 8)
#------------------------------------------------------------------------------
######
#First: group by year, sex and age
# collapse_year_reg(): `mort` has one row per registration year with the cell's
# population repeated, so summing population directly counted it once per row.
mort_national <- mort |> collapse_year_reg("deaths", keys = c("mun", "year", "sex", "age")) |>
  group_by(year, age, sex)|> summarise(deaths = sum(deaths), population = sum(population), .groups = "drop")
mort_national <- mort_national |> 
  # (removed) filter(!(sex == "female" & age %in% c("65-69","70-74","75-79")))
  # That line is copy-pasted from ch2_050, where dropping female 65-79 is a
  # reproductive-age cap. Here it deleted REAL female deaths at 65-79, leaving
  # the female mortality curve truncated at 64 while the male curve ran to 79 —
  # in a figure whose whole point is the male/female comparison.
  identity()
mort_national$mort_rate <- mort_national$deaths / mort_national$population

mort_national <- mort_national |> drop_na()


# Second: calculate the standardized deaths rate
std_age_sex <- compute_national_std_rate_age_gender(mort_national, std_ref_population(population))
std_age_sex <- std_age_sex %>% filter(year == 2023)


#Plot
p_std_rate_age_2023 <- ggplot(std_age_sex, aes(x = age, y = std_rate, color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_point()+
  facet_wrap(~ sex, nrow = 1) +
  labs(title = "Standardized Mortality Rate by Age Group",
       x = "Age Group",
       y = "Standardized Mortality Rate") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch2_040_std_rate_age_2023.pdf"), p_std_rate_age_2023, width = 12, height = 8)


std_age_sex <- std_age_sex %>% arrange(year, sex, age)

# 2. Plot with ggplot
p <- ggplot(std_age_sex, aes(x = age, y = std_rate, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ year) + 
  labs(
    title = "Standardized Mortality Rate by Age and Sex",
    x = "Age Group",
    y = "Standardized Mortality Rate"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_text(size = 8)
  )


ggsave(file.path(fig_dir, "ch2_040_std_rate_age_sex.pdf"), p, width = 12, height = 8)



