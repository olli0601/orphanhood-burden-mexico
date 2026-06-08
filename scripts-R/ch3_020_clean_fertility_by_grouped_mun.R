# =============================================================================
# ch3_020_clean_fertility_by_grouped_mun.R  ·  Chapter 3 — Fertility by grouped municipality
# Re-express births on the AGGREGATED (grouped) municipalities and merge with the
# grouped population, geography and marginalization to produce the analysis-ready
# fertility table at grouped-municipality level. Grouped counterpart of
# ch2_050_clean_fert.R (original-municipality level); the two write different files
# (fert_by_grouped_mun.RDS vs fert.RDS).
#
# Reads : input-data-processed/{births, grouped_municipality_*, geo_info*,
#         population*, index_marg}.RDS
# Writes: input-data-processed/fert_by_grouped_mun.RDS
#         output/ch3/ch3_020_by_grouped_mun_*.pdf
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
source("R/rates.R"); source("R/plots.R"); source("R/load_year_panels.R"); source("R/grouped_mun.R")
fig_dir <- "output/ch3"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
################################################################################

#---------------------------- SHOW DELAY REPORTING ----------------------------

################################################################################
births <- readRDS("input-data-processed/births.RDS")

# 1. Aggregate total deaths by occurrence year and reporting year
births_summary <- births %>%
  group_by(year, year_reg) %>%
  summarise(total_births = n(), .groups = "drop") %>%
  add_delay(max_delay = 8)


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
my_colors <- createPalette(length(delay_levels), seedcolors = seed_colors)
names(my_colors) <- delay_levels

#---------- BARPLOT ------------
## Total deaths
p_births_total_by_reg_year <- ggplot(births_summary, aes(x = factor(year_reg), y = (total_births), fill = factor(delay))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Year of Registration",
    y = "Total Births",
    fill = "Delay (Years)",
    title = "Total Births by Occurrence Year and Reporting Year"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3)
  )
ggsave(file.path(fig_dir, "ch3_020_by_grouped_mun_births_total_by_reg_year.pdf"), p_births_total_by_reg_year, width = 8, height = 6)

## Proportion -- DELAY IN THE FUTURE
p_births_prop_by_occ_year <- ggplot(births_prop_occ, aes(x = factor(year),
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
ggsave(file.path(fig_dir, "ch3_020_by_grouped_mun_births_prop_by_occ_year.pdf"), p_births_prop_by_occ_year, width = 8, height = 6)



## Proportion -- DELAY IN THE PAST
p_births_prop_by_reg_year <- ggplot(births_prop_reg, aes(x = factor(year_reg),
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
ggsave(file.path(fig_dir, "ch3_020_by_grouped_mun_births_prop_by_reg_year.pdf"), p_births_prop_by_reg_year, width = 8, height = 6)


################################################################################

#----------------------- GROUP BY THE NEW MUNICIPALITIES -----------------------

################################################################################
# --------- GROUP BY THE GROUPED MUNICIPALITIES (R/grouped_mun.R) ---------------
births_grouped_mun <- aggregate_to_grouped_mun(births, "births", out_col = "tot_births") |>
  filter(year >= 1990)

################################################################################

#---------------------------------- GEO INFO ----------------------------------

################################################################################
geo_info <- readRDS("input-data-processed/geo_info.RDS")
geo_info_grouped_mun <- readRDS("input-data-processed/geo_info_grouped_mun.RDS")


################################################################################

#--------------------------------- POPULATION ----------------------------------

################################################################################
population_grouped_mun <- readRDS("input-data-processed/population_grouped_mun.RDS")
fert <- left_join(births_grouped_mun %>% dplyr::select(group_id, year, sex, age, year_reg, tot_births), 
                  population_grouped_mun, 
                  by = c("group_id", "year", "sex", "age"))

fert$tot_births[is.na(fert$tot_births)] <- 0
fert$fert_rate <- fert$tot_births / fert$population

fert <- fert |> 
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79")))
fert <- fert |>
  filter(!(sex == "male" & age == "80-84"))



fert %>% 
  count(group_id, year, sex, age) %>% 
  filter(n > 1)


fert<- fert %>%
  add_delay(bucket = FALSE)

saveRDS(fert, file = "input-data-processed/fert_by_grouped_mun.RDS")


################################################################################

#------------------------------------- MPI -------------------------------------

################################################################################
index_marg <- readRDS("input-data-processed/index_marg.RDS")

df_2023_fert <- fert |> filter(year==2023) |> full_join(index_marg, by = c("group_id")) 
df_2023_fert$fert_rate <- df_2023_fert$tot_births / df_2023_fert$population
df_2023_fert <- df_2023_fert |> drop_na()

################################################################################

#----------------------- Standardized mortality rate ---------------------------

################################################################################
std_raw <- compute_std_fert_rate(df_2023_fert);
std_raw <- std_raw %>% left_join(y = geo_info_grouped_mun |> dplyr::select(group_id, capital), by = "group_id") %>% left_join(y = index_marg, by = "group_id") %>% mutate(capital = factor(capital))
std_raw <- std_raw %>% rename("mpi"="marg_index_weighted")
std_raw$mpi <- as.numeric(std_raw$mpi)

# ---------------------Plot mortality rate against mpi -------------------------
p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (fertility)")
print(p_raw_pts)
ggsave(file.path(fig_dir, "ch3_020_by_grouped_mun_std_fert_rate_vs_mpi.pdf"), p_raw_pts, width = 8, height = 6)


################################################################################

#------------------------- National mortality rate -----------------------------

################################################################################
fert <- readRDS("input-data-processed/fert_by_grouped_mun.RDS")
fert$year <- as.character(fert$year)
fert <- fert |>
  tidyr::unnest(cols = c(tot_births, population))

fert_national <- fert |>
  group_by(year, age, sex) |>
  summarise(
    births = sum(tot_births, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

fert_national <- fert_national |> 
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79")))

fert_national <- fert_national |>
  filter(!(sex == "male" & age == "80-84"))

fert_national$fert_rate <- fert_national$births / fert_national$population

fert_national <- fert_national |> drop_na()

# Second: calculate the standardized deaths rate
std_age_sex <- compute_national_std_rate_age_gender(fert_national)
std_age_sex <- std_age_sex %>% arrange(year, sex, age)

# 2. Plot with ggplot
p <- ggplot(std_age_sex, aes(x = age, y = std_rate, color = sex, group = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ year) + 
  labs(
    title = "Standardized Fertility Rate by Age and Sex",
    x = "Age Group",
    y = "Standardized Fertility Rate"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_text(size = 8)
  )


print(p)
ggsave(file.path(fig_dir, "ch3_020_by_grouped_mun_std_fert_rate_by_age_sex_year.pdf"), p, width = 12, height = 8)


std_age_sex <- std_age_sex %>% filter(year == 2023)
#Plot
p_std_fert_2023_by_sex <- ggplot(std_age_sex, aes(x = age, y = std_rate, color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_point()+
  facet_wrap(~ sex, nrow = 1) +
  labs(title = "Standardized Mortality Rate by Age Group",
       x = "Age Group",
       y = "Standardized Mortality Rate") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch3_020_by_grouped_mun_std_fert_2023_by_sex.pdf"), p_std_fert_2023_by_sex, width = 8, height = 6)


#-------------------------------------------------------------------------------
# Ensure year is numeric
index_marg <- readRDS("input-data-processed/marg_index.RDS")  # group_id x year IMN/GM
index_marg <- index_marg |>
  mutate(year = as.integer(year))

# Get 2010 values for all group_ids
imn_2010 <- index_marg |>
  filter(year == 2010) |>
  mutate(year = list(1990:2009)) |>
  unnest(year)

# Get 2020 values for all group_ids
imn_2020 <- index_marg |>
  filter(year == 2020) |>
  mutate(year = list(2021:2023)) |>
  unnest(year)

# Combine everything
index_marg <- bind_rows(imn_2010, index_marg, imn_2020) |>
  arrange(group_id, year)
df_fert <- left_join(fert, dplyr::mutate(index_marg, year = as.character(year)), by=c("group_id", "year"))

df_fert <- df_fert |>
  filter(!age %in% as.character(0:15)) |>
  filter(year >= 1990 & year <= 2023)

df_fert <- df_fert |>
  mutate(
    poverty_quintile = ntile(IMN, 5)  # 5 = number of quantile groups
  )

df_fert <- df_fert |>
  mutate(
    fert_rate = (tot_births/population) 
  )

library(dplyr)
library(ggplot2)

# Summarize: average mortality rate per year-sex-quintile
df_summary <- df_fert |>
  filter(!is.na(poverty_quintile)) |>
  group_by(year, sex, poverty_quintile) |>
  summarise(
    fertility_rate = mean(fert_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Plot trends
p_fert_trends_by_quintile <- ggplot(df_summary, aes(x = year, y = fertility_rate, color = as.factor(poverty_quintile), group = poverty_quintile)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ sex) +
  theme_minimal() +
  labs(
    title = "Fertility Trends Over Time",
    x = "Year",
    y = "Average Fertility Rate",
    color = "Poverty Quintile"
  )
ggsave(file.path(fig_dir, "ch3_020_by_grouped_mun_fert_trends_by_quintile.pdf"), p_fert_trends_by_quintile, width = 8, height = 6)

df_fert_std <- compute_std_fert_rate(df_fert)
df_fert_std <- left_join(df_fert_std, dplyr::mutate(index_marg, year = as.character(year)), by=c("group_id", "year"))
df_fert_std <- df_fert_std |>
  mutate(
    poverty_quintile = ntile(IMN, 5)  # 5 = number of quantile groups
  )

df_fert_std <- df_fert_std|>
  filter(!is.na(poverty_quintile)) |>
  group_by(year, sex, poverty_quintile) |>
  summarise(
    std_fert_rate = mean(std_fert_rate, na.rm = TRUE),
    .groups = "drop"
  )


p_std_fert_trends_by_quintile <- ggplot(df_fert_std, aes(x = year, y = std_fert_rate, color = as.factor(poverty_quintile), group = poverty_quintile)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ sex) +
  theme_minimal() +
  labs(
    title = "Fertility Trends Over Time",
    x = "Year",
    y = "Average Standardized Fertility Rate",
    color = "Poverty Quintile"
  )
ggsave(file.path(fig_dir, "ch3_020_by_grouped_mun_std_fert_trends_by_quintile.pdf"), p_std_fert_trends_by_quintile, width = 8, height = 6)



