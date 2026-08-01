# =============================================================================
# ch2_050_clean_fert.R  ·  Chapter 2 — Fertility cleaning (original municipalities)
# Clean and harmonise the per-year INEGI registered-birth files into birth
# counts by municipality x parent-sex x 5-year age group x year. Works at
# ORIGINAL-municipality level and writes fert.RDS. The grouped-municipality
# counterpart is ch3_020_clean_fertility_by_grouped_mun.R (fert_by_grouped_mun.RDS).
#
# Loads the supplied per-year fertility (input-data-processed/fertility datasets/),
# joins population + IMM_2020 + geo_info.
# Reads : input-data-processed/fertility datasets/*.RDS, geo_info_mun.RDS (ch1_015),
#         input-data-raw/population/...1_Grupo_Quinq..., input-data-raw/marginalization/IMM_2020.xls
# Writes: input-data-processed/fert.RDS (municipality level)
# Run after: ch2_040
# =============================================================================

###########################################
# Data pre-processing 
#############################################

library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(foreign)
source("R/preprocess_fertility.R"); source("R/preprocess_mortality.R"); source("R/rates.R"); source("R/plots.R")
source("R/marginalization.R")
source("R/load_year_panels.R")

fig_dir <- "output/ch2"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Load the SUPPLIED per-year fertility datasets into fert_YYYY objects (the
# output of preprocess_fertility() on the raw INEGI files). Lets the pipeline
# start from input-data-processed/ without the raw files. (To regenerate from
# raw instead, loop the INEGI files through preprocess_fertility().)
# Run ch1_015_bootstrap_from_processed.R first.
fert_panel <- load_year_panels("fertility datasets")

## BARPLOT 
# Get all objects whose names start with "mort_"
all_fert <- fert_panel

# Summarize data: count occurrences of each 'year' within each 'year_reg'
data_summary_fert <- all_fert %>%
  filter(year >= as.numeric(year_reg) - 20) %>%  # Keep only deaths in the last 20 years before registration
  group_by(year_reg, year) %>%
  # sum(births), not n(): n() counted panel rows, giving a cell with 1 birth the
  # same weight as one with 5,000 and distorting the delay distribution.
  summarise(count = sum(births), .groups = "drop") %>%
  group_by(year_reg) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ungroup()

# Convert the data to a proportion for pie charts
data_summary_pie <- data_summary_fert %>%
  group_by(year_reg) %>%
  mutate(perc = round(count / sum(count) * 100, 2))

data_summary_pie <- data_summary_pie %>%
  group_by(year_reg) %>%
  arrange(desc(perc)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

p_pie <- ggplot(data_summary_pie, aes(x = "", y = perc, fill = factor(year))) +
  geom_bar(stat = "identity", width = 0.5, color = "white", size = 0.1) +
  # Only add labels for the top three highest percentage slices in each group
  geom_text(data = subset(data_summary_pie, rank <= 5),
            aes(label = paste0(round(perc, 1), "%")),
            position = position_stack(vjust = 0.5), 
            size=2.5) +
  coord_polar(theta = "y") +
  facet_wrap(~year_reg) +
  theme_void() + 
  labs(title = "Birth Year Distribution per Registration Year") +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  scale_fill_manual(values = rainbow(length(unique(data_summary_pie$year))))

ggsave(file.path(fig_dir, "ch2_050_pie_birth_year_dist.pdf"), p_pie, width = 12, height = 8)

# Combine and aggregate all available years (1985-2024).
births <- fert_panel |>
  group_by(year, sex, age, mun) |>
  summarise(births = sum(births), .groups = "drop") |>
  filter(year >= 1985, year <= 2024)


##########################################

#population

X1_Grupo_Quinq_00_RM <- read_excel("input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx")

population <- X1_Grupo_Quinq_00_RM |> rename(
  mun = CLAVE,
  state = CLAVE_ENT,
  mun_name = NOM_MUN,
  state_name = NOM_ENT,
  sex = SEXO,
  year = AÑO
) |> pivot_longer(
  cols = starts_with("POB_"), values_to = "population", names_to = "age"
) |> mutate(
  sex = case_when(
    sex == "HOMBRES" ~ "male",
    sex == "MUJERES" ~ "female"
  )
)

population$mun <- str_pad(population$mun, 5, pad = "0")

population <- population |> filter(year >= 1990 & year < 2024)

population <- population |>
  mutate(
    age = str_replace(age, "POB_", "")
  ) |>
  mutate(
    age = str_replace(age, "_", "-")
  )

age_cat_f <- births |>
  filter(sex == "female") |>
  distinct(age) |>
  pull(age) |>
  sort()

age_cat_m <- births |>
  filter(sex == "male") |>
  distinct(age) |>
  pull(age) |>
  sort()


population.f <- population |> filter(sex == "female") |>
  filter(age %in% age_cat_f)

population.m <- population |> filter(sex == "male") |>
  filter(age %in% age_cat_m)

population <- rbind(population.f, population.m)

population$sex <- as.factor(population$sex)
population$age <- as.factor(population$age)
population$mun <- as.factor(population$mun)

fert <- full_join(births, population, by = c("mun", "year", "sex", "age"))

fert$births[is.na(fert$births)] <- 0

#marginality index

index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)

index <- index[-1,]

#IM - marginalization index
#IMN - normalized marginalization index
# GM- degree of marginalization

im.tmp <- index |> dplyr:: select(mun, IM, IMN)
im.tmp$mun <- as.factor(im.tmp$mun)
im.tmp$IM <- as.numeric(im.tmp$IM)
im.tmp$IMN <- as.numeric(im.tmp$IMN)

fert <- fert |> full_join(im.tmp, by = c("mun")) |> dplyr::select(
  mun, year, sex, age, births, population, IM, IMN
)

##some municipalities don't have IM information (??)
##some municipalities have population of 0

fert$fert_rate <- fert$births / fert$population

# Same reporting as ch2_040: births with no matching population cell are removed
# here, silently until now. See the note there — for fertility the age universe
# is derived from the births panel itself a few lines up, so the loss should be
# much smaller than mortality's 8.85%, but it was never checked.
n_before <- nrow(fert); b_before <- sum(fert$births, na.rm = TRUE)
fert <- fert |> drop_na()
message(sprintf("ch2_050: drop_na() removed %s rows and %s births (%.2f%% of all births).",
                format(n_before - nrow(fert), big.mark = ","),
                format(round(b_before - sum(fert$births)), big.mark = ","),
                100 * (b_before - sum(fert$births)) / b_before))


saveRDS(fert, file = "input-data-processed/fert.RDS")

geo_info <- readRDS("input-data-processed/geo_info_mun.RDS")
#-----------------------. Standardized fertility rate --------------------------
# MOTHER ROWS ONLY. `sex` here is the PARENT's sex and every birth appears twice,
# once on the mother's row and once on the father's. The retired
# compute_std_rate() grouped by (mun, year) only, collapsing sex, so it summed
# both rows: every published municipal fertility rate in this script was a
# literal double count. Restricting to mothers also makes the standardisation
# weights sum to 1 over the female bands, i.e. births per woman — the same thing
# ch2_020 does, which is the reference implementation.
fert_mothers <- fert %>% filter(sex == "female")

std_all_fert <- compute_std_fert_rate(fert_mothers, unit_col = "mun") %>%
  dplyr::select(mun, year, std_fert_rate) %>%
  left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>%
  left_join(y = index, by = "mun") %>%
  mutate(capital = factor(capital)) %>%
  rename(mpi = IMN) %>%
  mutate(mpi = as.numeric(mpi))

std_raw <- std_all_fert %>% filter(year == 2020) %>% dplyr::select(-year)

#--------------------- Plot std fertility rate against mpi --------------------
y_lim <- c(0, 0.15)
p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (fertility)", y_lim = y_lim)
ggsave(file.path(fig_dir, "ch2_050_std_fert_rate_2020.pdf"), p_raw_pts, width = 8, height = 6)

#--------------- Iterate over all the years considered ---------------------
std_raw_all_fert <- std_all_fert %>% filter(year %in% 2005:2023)


# Title said "2017 to 2023"; the filter two lines above is 2005:2023.
p_raw_pts <- plot_std_rate(data = std_raw_all_fert, tt = "Raw data (fertility) - Years 2005 to 2023") +
  facet_wrap(~ year)

ggsave(file.path(fig_dir, "ch2_050_std_fert_rate_all_years.pdf"), p_raw_pts, width = 12, height = 8)
# (removed) a `for (year in years)` loop that produced nothing.
# The loop variable shadowed the `year` COLUMN, so `filter(year == year)` was
# TRUE for every row and the filter was a no-op: each iteration recomputed the
# full 34-year compute_std_rate(fert) and plotted every year at once, under a
# title naming a single year. Nothing was saved — the plots went to the default
# device (Rplots.pdf), which had grown to 136 MB. The per-year fertility figures
# that ARE saved come from the loop further down.



library(dplyr)
library(ggplot2)
library(gridExtra)


# Get the unique years sorted (from 2000 to 2023)
# (removed) a second print-only per-year loop. Its comment said "Store the plot
# in the list", but the line under it was `print(p)` — plot_list stayed empty, so
# the grid.arrange() that followed rendered a blank page. Nothing reached disk.


######
#First: group by year, sex and age
fert_national <- fert |> group_by(year, age, sex)|> summarise(births = sum(births), population = sum(population), .groups = "drop")
fert_national <- fert_national |> 
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79")))
fert_national$fert_rate <- fert_national$births / fert_national$population

fert_national <- fert_national |> drop_na()


# Second: calculate the standardized deaths rate
std_age_sex <- compute_national_std_rate_age_gender(fert_national)
std_age_sex <- std_age_sex %>% filter(year == 2023)

# (removed) a duplicate of the fertility plot below, saved as
# ch2_050_std_mort_rate_age.pdf with a "Standardized Mortality Rate" title and
# axis label. This is the FERTILITY script and std_age_sex holds fertility
# rates, so that file was the fertility series mislabelled as mortality. The
# correctly labelled version is immediately below; the real mortality-by-age
# figure is ch2_040_std_rate_age_2023.pdf.

#Plot
p_std_fert_age <- ggplot(std_age_sex, aes(x = age, y = std_rate, color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_point()+
  facet_wrap(~ sex, nrow = 1) +
  labs(title = "Standardized Fertility Rate by Age Group",
       x = "Age Group",
       y = "Standardized Fertility Rate") +
  theme_minimal()
ggsave(file.path(fig_dir, "ch2_050_std_fert_rate_age.pdf"), p_std_fert_age, width = 8, height = 6)


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


ggsave(file.path(fig_dir, "ch2_050_std_fert_rate_age_sex.pdf"), p, width = 8, height = 6)

# (removed) a third print-only per-year loop (2017-2023), plus the entire
# poverty-index (MPI) block that followed it.
#
# The MPI block was guarded on an optional CONAPO file that ch1_010 never
# downloads, so it had never run — and nothing inside it was used even when it
# did: `mpi`/`mpi_norm` were built and never referenced, its loop was an exact
# copy of the one above it (also print-only), and `grupos_poblacionales_2020`
# was read and never used. Deleted rather than repaired.
#
# The marginalization actually used by the pipeline comes from ch2_060
# (index_grouped_mun.RDS), not from this file.


