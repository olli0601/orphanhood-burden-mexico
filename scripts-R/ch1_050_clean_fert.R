# =============================================================================
# ch1_050_clean_fert.R  ·  Chapter 1 — Fertility cleaning
# Clean and harmonise the per-year INEGI registered-birth files into birth
# counts by municipality x parent-sex x 5-year age group x year.
#
# Loads the supplied per-year fertility (input-data-processed/fertility datasets/),
# joins population + IMM_2020 + geo_info.
# Reads : input-data-processed/fertility datasets/*.RDS, geo_info.RDS (ch1_040),
#         input-data-raw/population/...1_Grupo_Quinq..., input-data-raw/marginalization/IMM_2020.xls
# Writes: input-data-processed/fert.RDS (municipality level)
# Run after: ch1_040
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

# Load the SUPPLIED per-year fertility datasets into fert_YYYY objects (the
# output of preprocess_fertility() on the raw INEGI files). Lets the pipeline
# start from input-data-processed/ without the raw files. (To regenerate from
# raw instead, loop the INEGI files through preprocess_fertility().)
# Run ch1_005_bootstrap_from_processed.R first.
fert_panel <- load_year_panels("fertility datasets")

## BARPLOT 
# Get all objects whose names start with "mort_"
all_fert <- fert_panel

# Summarize data: count occurrences of each 'year' within each 'year_reg'
data_summary_fert <- all_fert %>%
  filter(year >= as.numeric(year_reg) - 20) %>%  # Keep only deaths in the last 20 years before registration
  group_by(year_reg, year) %>%
  summarise(count = n(), .groups = "drop") %>%
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

print(p_pie)


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

fert <- fert |> drop_na()


saveRDS(fert, file = "input-data-processed/fert.RDS")

geo_info <- readRDS("input-data-processed/geo_info.RDS")
#-----------------------. Standardized fertility rate --------------------------
std_raw <- compute_std_rate(fert); std_raw <- std_raw %>% filter(year == 2020) %>% dplyr::select(-year)
std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = index, by = "mun") %>% mutate(capital = factor(capital))
std_raw <- std_raw %>% rename("mpi"="IMN")
std_raw$mpi <- as.numeric(std_raw$mpi)

#--------------------- Plot std fertility rate against mpi --------------------
y_lim <- c(0, 0.15)
p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (fertility)", y_lim = y_lim)
print(p_raw_pts)

#--------------- Iterate over all the years considered ---------------------
std_raw_all_fert <- compute_std_rate(fert) %>%
  filter(year %in% 2005:2023) %>% 
  left_join(geo_info[, c("mun", "capital")], by = "mun") %>% 
  left_join(index, by = "mun") %>% 
  mutate(capital = factor(capital)) %>%
  rename(mpi = IMN) %>% 
  mutate(mpi = as.numeric(mpi))


p_raw_pts <- plot_std_rate(data = std_raw_all_fert, tt = "Raw data (fertility) - Years 2017 to 2023") +
  facet_wrap(~ year)

print(p_raw_pts)
# Get the unique years from the 'fert' dataset
years <- unique(fert$year)

# Loop over each year
for (year in years) {
  # Filter the dataset for the current year
  std_raw <- compute_std_rate(fert) 
  std_raw <- std_raw %>% 
    filter(year == year) %>% 
    dplyr::select(-year)
  
  # Merge with geo_info and index data, and convert 'capital' to a factor
  std_raw <- std_raw %>% 
    left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% 
    left_join(y = index, by = "mun") %>% 
    mutate(capital = factor(capital))
  
  # Rename and convert 'mpi' to numeric
  std_raw <- std_raw %>% rename("mpi" = "IMN")
  std_raw$mpi <- as.numeric(std_raw$mpi)
  
  # Set y-axis limits
  y_lim <- c(0, 0.15)
  
  # Plot the data for the current year
  p_raw_pts <- plot_std_rate(data = std_raw, tt = paste("Raw data (fertility) - Year", year), y_lim = y_lim)
  
  # Print the plot
  print(p_raw_pts)
}



library(dplyr)
library(ggplot2)
library(gridExtra)


# Get the unique years sorted (from 2000 to 2023)
years <- sort(unique(fert$year))

# Create an empty list to store the plots
plot_list <- list()

# Loop over each year using a loop variable 'yr'
for (yr in years) {
  # Filter the fertility data for the current year
  fert_year <- fert %>% filter(year == yr)
  
  # Compute the standardized rate for the current year's data
  std_raw <- compute_std_rate(fert_year)
  
  # Remove the year column and merge with geo_info and index
  std_raw <- std_raw %>% 
    dplyr::select(-year) %>% 
    left_join(geo_info[, c("mun", "capital")], by = "mun") %>% 
    left_join(index, by = "mun") %>% 
    mutate(capital = factor(capital))
  
  # Rename the MPI column and convert it to numeric
  std_raw <- std_raw %>% rename(mpi = IMN)
  std_raw$mpi <- as.numeric(std_raw$mpi)
  
  
  # Create the plot for the current year
  p <- plot_std_rate(data = std_raw, tt = paste("Raw data (fertility) - Year", yr), y_lim = y_lim)
  
  # Store the plot in the list
  print(p)
}

# Arrange all the plots in a grid with 4 rows and 6 columns
grid.arrange(grobs = plot_list, nrow = 4, ncol = 6)


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

ggplot(std_age_sex, aes(x = age, y = std_rate, color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_point()+
  facet_wrap(~ sex, nrow = 1) +
  labs(title = "Standardized Mortality Rate by Age Group",
       x = "Age Group",
       y = "Standardized Mortality Rate") +
  theme_minimal() 

#Plot 
ggplot(std_age_sex, aes(x = age, y = std_rate, color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_point()+
  facet_wrap(~ sex, nrow = 1) +
  labs(title = "Standardized Fertility Rate by Age Group",
       x = "Age Group",
       y = "Standardized Fertility Rate") +
  theme_minimal() 


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

years <- seq(2017, 2023, 1)


# Loop over each year
for (yr in years) {
  std_raw <- compute_std_rate(fert); std_raw <- std_raw %>% filter(year == yr) %>% dplyr::select(-year)
  std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = index, by = "mun") %>% mutate(capital = factor(capital))
  #std_raw$overcrowding_pct <- min_max_norm(std_raw$overcrowding_pct)
  std_raw <- std_raw %>% rename("mpi"="IMN")
  std_raw$mpi <- as.numeric(std_raw$mpi)
  
  p_raw_pts <- plot_std_rate(data = std_raw, tt= paste("Raw data (fertility) - Year", yr))
  print(p_raw_pts)
}


library(readr)

if (!file.exists("input-data-raw/marginalization/Indicadores_municipales_sabana_DA.csv")) {
  message("ch1_050: skipping poverty-index (MPI) section — Indicadores_municipales_sabana_DA.csv not in input-data-raw/ (optional CONAPO file).")
} else {
Indicadores_municipales_sabana_DA <- read_csv("input-data-raw/marginalization/Indicadores_municipales_sabana_DA.csv")
mpi <- Indicadores_municipales_sabana_DA %>% select(ent, nom_ent, mun, nom_mun, pobreza) %>% rename(mpi=pobreza)
mpi <- mpi %>% mutate(mun= paste0(ent, mun))
mpi <- mpi %>% mutate(mpi_norm =min_max_norm(mpi))


for (yr in years) {
  std_raw <- compute_std_rate(fert); std_raw <- std_raw %>% filter(year == yr) %>% dplyr::select(-year)
  std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = index, by = "mun") %>% mutate(capital = factor(capital))
  #std_raw$overcrowding_pct <- min_max_norm(std_raw$overcrowding_pct)
  std_raw <- std_raw %>% rename("mpi"="IMN")
  std_raw$mpi <- as.numeric(std_raw$mpi)
  
  p_raw_pts <- plot_std_rate(data = std_raw, tt= paste("Raw data (fertility) - Year", yr))
  print(p_raw_pts)
}

library(readr)
grupos_poblacionales_2020 <- read_csv("input-data-raw/grupos_poblacionales_2020.csv")
}


