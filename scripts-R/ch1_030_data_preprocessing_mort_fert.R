# =============================================================================
# ch1_030_data_preprocessing_mort_fert.R  ·  Chapter 1 — Rate standardisation
# Compute and compare standardised age-specific mortality/fertility rates
# (raw vs fitted) and export the comparison figures.
#
# Reads : input-data-raw/{deaths,births}/*_mexico.csv, marginalization/IMM_2020.xls,
#         input-data-processed/data_pop.rds
# Writes: output/ch1/std_mortality_comparison_*.jpeg
# Run after: ch1_020
# =============================================================================

library(readr)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(writexl)
library(stringr)
dir.create("output/ch1", recursive = TRUE, showWarnings = FALSE)
#-------------------------------------------------------------------------------
###------------------------------ MORTALITY ---------------------------------###
#-------------------------------------------------------------------------------
# Read the data
deaths_data <- read_csv("input-data-raw/deaths/mortality_mexico.csv")

# Rename the columns in english and retain only the columns sex, age, municipality and year of the death
deaths_data <- deaths_data %>% rename("sex" = "sexo", "age"="edad", "state_code"="ent_resid", "municipality_code"= "mun_resid", "year"="anio_ocur")
deaths_df <- deaths_data %>% select("sex", "age", "state_code", "municipality_code", "year")

deaths <- deaths |> filter(deaths$age > 4009 & deaths$age < 4080)

# The age is displayed with a 40 in front of the number. Eliminate the 40
deaths_df <- deaths_df %>% 
  mutate(age = age %% 100)

# Filter the dataset based on the age of the deceased. The age ranges coincide with the one of the paper of orphanhood in the US
# Mothers: 15-64 years old; Fathers: 15-84
deaths_df <- deaths_df %>% 
  filter(
    (sex == 2 & age >= 15 & age <= 64) |
      (sex == 1 & age >= 15 & age <= 84)
  )
### NOTE: In this way the people classificated without a gender are eliminated


deaths_df <- deaths_df %>% 
  mutate(gender = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female"
  ))


deaths_df <- deaths_df %>%
  mutate(age_group = case_when(
    gender == "Female" & age >= 15 & age <= 64 ~ paste0(15 + 5 * floor((age - 15) / 5), "-", 19 + 5 * floor((age - 15) / 5)),
    gender == "Male" & age >= 15 & age <= 84 ~ paste0(15 + 5 * floor((age - 15) / 5), "-", 19 + 5 * floor((age - 15) / 5))
  ))


# Merge the dataset between the municipality and the population
deaths_df$municipality_code <- as.numeric(deaths_df$municipality_code)
deaths_df$state_code <- as.numeric(deaths_df$state_code)

deaths_df <- deaths_df %>% 
  filter(
    deaths_df$state_code <= 32 & deaths_df$municipality_code != 999
  )

# Count the number of deaths for each year, age group, municipality and gender
deaths_df <- deaths_df %>%
  group_by(year, age_group, municipality_code, gender) %>%
  mutate(death_count = n()) %>%
  ungroup()

# Filter deaths_df to match the years considered in df_interp
deaths_df_filtered <- deaths_df %>%
  filter(year >= 2000 & year <= 2023)

# Clean key columns (there is extra spaces in the age_group column)
deaths_df_filtered <- deaths_df_filtered %>%
  mutate(
    municipality_code = tolower(trimws(as.character(municipality_code))),
    gender = tolower(trimws(as.character(gender))),
    age_group = tolower(trimws(as.character(age_group))),
    year = as.numeric(year)
  )

#----------------
## Uploade population data in this R file 
#----------------
data_pop <- readRDS("input-data-processed/data_pop.rds")

# Extract the municipality code
data_pop <- data_pop %>%
  mutate(municipality_code = str_sub(municipality_complete_code, 
                                     start = if_else(nchar(municipality_complete_code) == 5, 3, 2), 
                                     end = nchar(municipality_complete_code)),
         municipality_code = as.numeric(municipality_code)) 

data_pop$municipality_code <- as.numeric(data_pop$municipality_code)
data_pop$state_code <- as.numeric(data_pop$state_code)

deaths_df_filtered$municipality_code <- as.numeric(deaths_df_filtered$municipality_code)
deaths_df_filtered$state_code <- as.numeric(deaths_df_filtered$state_code)

# Perform the join on all four keys
merged_df <- left_join(deaths_df_filtered,
                       data_pop %>% select(state_code, state_name, municipality_code, municipality_name, year, gender, age_group, population),
                       by = c("state_code", "municipality_code", "year", "gender", "age_group"))

# Calculate the mortality rate as the number of deaths over the population
merged_df <- merged_df %>%
  mutate(mortality_rate = death_count / population)

merged_df <- merged_df %>%
  mutate(municipality_complete_code = paste0(state_code, 
                                             if_else(municipality_code < 10, "00", 
                                                     if_else(municipality_code < 100, "0", "")), 
                                             municipality_code))

#-------------------------------------------------------------------------------
###------------------------------ FERTILITY ---------------------------------###
#-------------------------------------------------------------------------------

# Read the data
fertility_data <- read_csv("input-data-raw/births/fertility_mexico.csv")

# Rename the columns in english and retain only the columns sex, age, municipality and year of the birth
fertility_data <- fertility_data %>% rename("sex" = "sexo", "moms_age"="edad_madn", "dads_age"="edad_padn", "state_code"="ent_resid", "municipality_code"= "mun_resid", "year"="ano_reg")
fertility_df_women <- fertility_data %>% select("sex", "moms_age", "state_code", "municipality_code", "year")
fertility_df_men <- fertility_data %>% select("sex", "dads_age", "state_code", "municipality_code", "year")

# Filter the dataset based on the age.
# Mothers: 15-64 years old; Fathers: 15-84
fertility_df_women <- fertility_df_women %>% 
  filter(
    (moms_age >= 15 & moms_age <=64) 
  )
fertility_df_men <- fertility_df_men %>% 
  filter(
    (dads_age >= 15 & dads_age <= 84)
  )

### NOTE: In this way the people classificated without a gender are eliminated

# Create the column "gender" for the gender of the newborn
# WOMEN
fertility_df_women <- fertility_df_women %>% 
  mutate(gender = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female"
  ))

#MEN
fertility_df_men <- fertility_df_men %>% 
  mutate(gender = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female"
  ))


## Create age groups
# WOMEN
fertility_df_women <- fertility_df_women %>%
  mutate(
    age_group = paste0(15 + 5 * floor((moms_age - 15) / 5), "-", 19 + 5 * floor((moms_age - 15) / 5)),
  )

#MEN
fertility_df_men <- fertility_df_men %>%
  mutate(
    age_group = paste0(15 + 5 * floor((dads_age - 15) / 5), "-", 19 + 5 * floor((dads_age - 15) / 5))
  )


# Merge the dataset between the municipality and the population
fertility_df_women$municipality_code <- as.numeric(fertility_df_women$municipality_code)
fertility_df_men$municipality_code <- as.numeric(fertility_df_men$municipality_code)

fertility_df_women$state_code <- as.numeric(fertility_df_women$state_code)
fertility_df_men$state_code <- as.numeric(fertility_df_men$state_code)


# Filter the State and Municipality
#WOMEN
fertility_df_women <- fertility_df_women%>% 
  filter(
    fertility_df_women$state_code <= 32 & fertility_df_women$municipality_code != 999
  )

#MEN
fertility_df_men <- fertility_df_men%>% 
  filter(
    fertility_df_men$state_code <= 32 & fertility_df_men$municipality_code != 999
  )

# Count the number of births for each year, age group, municipality and gender
#WOMEN
fertility_df_women <- fertility_df_women %>%
  group_by(year, age_group, municipality_code, gender) %>%
  mutate(births_count = n()) %>%
  ungroup()

#MEN
fertility_df_men <- fertility_df_men %>%
  group_by(year, age_group, municipality_code, gender) %>%
  mutate(births_count = n()) %>%
  ungroup()


# Filter fertility_df 
#WOMEN
fertility_df_filtered_women <- fertility_df_women %>%
  filter(year >= 2000 & year <= 2023)

#MEN
fertility_df_filtered_men <- fertility_df_men %>%
  filter(year >= 2000 & year <= 2023)

# Clean key columns (there is extra spaces in the age_group column)
#WOMEN
fertility_df_filtered_women<- fertility_df_filtered_women %>%
  mutate(
    municipality_code = as.numeric(municipality_code),
    gender = tolower(trimws(as.character(gender))),
    age_group = tolower(trimws(as.character(age_group))),
    year = as.numeric(year)
  )

#MEN
fertility_df_filtered_men<- fertility_df_filtered_men %>%
  mutate(
    municipality_code = as.numeric(municipality_code),
    gender = tolower(trimws(as.character(gender))),
    age_group = tolower(trimws(as.character(age_group))),
    year = as.numeric(year)
  )

# Perform the join on all four keys
#WOMEN
merged_df_fertility_women <- left_join(
  fertility_df_filtered_women,
  data_pop %>% select(state_code, state_name, municipality_code, municipality_name, year, gender, age_group, population),
  by = c("state_code", "municipality_code", "year", "gender", "age_group")
)

#MEN
merged_df_fertility_men <- left_join(
  fertility_df_filtered_men,
  data_pop %>% select(state_code, state_name, municipality_code, municipality_name, year, gender, age_group, population),
  by = c("state_code", "municipality_code", "year", "gender", "age_group")
)

# Calculate the mortality rate as the number of deaths over the population
#WOMEN
merged_df_fertility_women <- merged_df_fertility_women %>%
  mutate(fertility_rate_moms = births_count / population)

#MEN
merged_df_fertility_men <- merged_df_fertility_men %>%
  mutate(fertility_rate_dads = births_count / population)

#-------------------------------------------------------------------------------
#------------------------------ STANDARDIZED RATES -----------------------------
#-------------------------------------------------------------------------------
compute_std_rate <- function (data, dataset_type, ...) {
  cns <- colnames(data)
  
  # Identify the category (deaths or births) and rate based on dataset type
  ctg <- if (dataset_type == "mortality") {
    "death_count"
  } else {
    "births_count"
  }
  
  rte <- if (dataset_type == "mortality") {
    "mortality_rate"
  } else if (dataset_type == "fertility_moms") {
    "fertility_rate_moms"
  } else {
    "fertility_rate_dads"
  }
  
  # Get the population reference for year 2018
  pop_ref  <- data %>% 
    filter(year %in% 2020) %>% 
    dplyr::select(municipality_complete_code, gender, age_group, year, population) %>% 
    group_by(municipality_complete_code, gender, age_group) %>% 
    summarise(population = mean(population)) %>% 
    ungroup()
  
  # Calculate the total population for each gender and age group
  p_nat    <- pop_ref %>% 
    group_by(gender, age_group) %>% 
    summarise(p_nat = sum(population)) %>% 
    ungroup()
  
  p_nat_total <- sum(pop_ref$population)
  
  # Select relevant columns
  my_clmns <- c("municipality_complete_code", "year", "age_group", "gender", ctg, "population", rte)
  std_rate <- data %>% 
    dplyr::select(all_of(my_clmns))
  
  # Merge with population reference
  std_rate <- std_rate %>% 
    left_join(y = p_nat, by = c("age_group", "gender"))
  
  # Add the total population
  std_rate <- std_rate %>% 
    mutate(p_nat_total = p_nat_total)
  
  # Compute the standardized rate
  std_rate <- std_rate %>% 
    mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  
  # Group by municipality and year, then summarize the standardized rate
  std_rate <- std_rate %>% 
    group_by(municipality_complete_code, year) %>% 
    summarise(std_rate = sum(std_rate)) %>% 
    ungroup()
  
  return(std_rate)
}

std_rates_mortality <- compute_std_rate(merged_df, "mortality")
std_rates_fertility_women <- compute_std_rate(merged_df_fertility_women, "fertility_moms")
std_rates_fertility_men <- compute_std_rate(merged_df_fertility_men, "fertility_dads")

##------------------------------- Poverty rate -------------------------------
mpi <- read_excel("input-data-raw/marginalization/IMM_2020.xls", sheet = "IMM_2020")
mpi <- mpi %>%
  mutate(municipality_complete_code = if_else(str_sub(CVE_MUN, 1, 1) == "0", 
                                              str_sub(CVE_MUN, 2, nchar(CVE_MUN)), 
                                              CVE_MUN),
         municipality_complete_code = as.numeric(municipality_complete_code))


mortality_2020 <- std_rates_mortality %>% filter(year ==2023)
mortality_2020$municipality_complete_code <- as.numeric(mortality_2020$municipality_complete_code)
df_plot_mortality <- left_join(mortality_2020,
                               mpi %>% select("municipality_complete_code", "IMN_2020"), 
                               by="municipality_complete_code")
library(ggplot2)

ggplot(df_plot_mortality, aes(x = IMN_2020, y = std_rate)) +
  geom_point(color = "red") +                # Red points
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Add regression line
  labs(x = "Poverty Rate (IMN 2020)", y = "Standardized Mortality Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


# PLOT STANDARDIZED RATES
for (y in 2000:2023) {
  # y <- 2021
  yy <- y - 2000 + 1
  y_lim <- c(0, 0.02)
  
  std_raw <- compute_std_rate(std_rates_mortality); std_raw <- std_raw %>% filter(year == y) %>% dplyr::select(-year)
  std_raw <- std_raw %>% left_join(y = geo_info[, c("municipality_complete_code", "state_code")], by = "municipality_complete_code") %>% left_join(y = mpi_info, by = "municipality_complete_code") %>% mutate(capital = factor(state_code))
  
  p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (mortality)", y_lim = y_lim)
  
  alpha_0 <- c(draws[, paste("alpha_0[", yy, "]", sep = "")])
  alpha_1 <- c(draws[, paste("alpha_1[", yy, "]", sep = "")])
  mpi_mun <- seq(0, 1, by = 0.01)
  std_mun <- alpha_0 + outer(alpha_1, mpi_mun)
  mpi_obs <- fit_d$mpi_municip
  std_obs <- alpha_0 + outer(alpha_1, mpi_obs) 
  
  # Calculate the quantiles (2.5%, 50%, 97.5%) for each x value (column-wise)
  quantiles_lin <- apply(std_mun, 2, quantile, probs = c(0.025, 0.5, 0.975))
  quantiles_lin <- data.frame(x = mpi_mun, ll = quantiles_lin[1, ], mm = quantiles_lin[2, ], uu = quantiles_lin[3, ])
  
  quantiles_obs <- apply(std_obs, 2, quantile, probs = c(0.025, 0.5, 0.975))
  quantiles_obs <- data.frame(x = mpi_obs, ll = quantiles_obs[1, ], mm = quantiles_obs[2, ], uu = quantiles_obs[3, ])
  
  std_fit <- compute_std_rate(data_fit); std_fit <- std_fit %>% filter(year == y) %>% dplyr::select(-year)
  std_fit <- std_fit %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital), mpi = mpi / 100)
  
  p_fit_pts <- plot_fit_std_rate(data_lin = quantiles_lin, data_fit = std_fit, tt = "Fitted data (mortality)", y_lim = y_lim)
  
  (p_tot_pts <- p_raw_pts + p_fit_pts)
  ggsave(filename = paste("output/ch1/std_mortality_comparison_", y ,".jpeg" , sep = ""), plot = p_tot_pts , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")
  
  if (y == 2021) {
    p_raw_pts <- plot_std_rate(data = std_raw, tt = "Mortality", y_lim = c(0, 0.015))
    p_fit_pts <- plot_fit_std_rate(data_lin = quantiles_lin, data = std_fit, tt = "Fitted Mortality", y_lim = c(0, 0.015))
    ggsave(filename = paste("output/ch1/std_mortality_comparison_raw.jpeg" , sep = ""), plot = p_raw_pts , width = 1500, height = 1500, units = c("px"), dpi = 300, bg = "white")
    ggsave(filename = paste("output/ch1/std_mortality_comparison_fit.jpeg" , sep = ""), plot = p_fit_pts , width = 1500, height = 1500, units = c("px"), dpi = 300, bg = "white")
  }
}

levels(as.factor(mpi$GM_2020))
#
mpi_very_high <- mpi %>% filter(GM_2020 == "Muy alto")
mpi_medium <- mpi %>% filter(GM_2020 == "Medio")
mpi_very_low <- mpi %>% filter(GM_2020 == "Muy bajo")


plot_std_rate <- function (data, tt = "", y_lim = NULL, ...) {
  y_ran <- range(data$std_rate)
  ggplot(data = data) +
    geom_point(mapping = aes(x = mpi, y = std_rate, color = capital, size = capital)) + 
    scale_color_manual(name = "", values = c("#FF000099", "#0000FF99"), labels = c("Non-capital", "Department capital")) +
    scale_size_manual(name = "", values = c(1, 3), labels = c("Non-capital", "Department capital")) +
    geom_smooth(data = data[data$capital == 1, ], mapping = aes(x = mpi, y = std_rate), method = "lm", formula = y ~ x, se = TRUE, color = "blue", linewidth = 0.5, linetype = "solid") + 
    # geom_smooth(data = data[data$capital == 0, ], mapping = aes(x = mpi, y = std_rate), method = "lm", formula = y ~ x, se = TRUE, color = "red",  linewidth = 0.5, linetype = "solid") +  
    scale_x_continuous(breaks = seq(0, 100, 25), labels = seq(0, 100, 25), limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = y_lim, expand = c(0, 0)) +
    labs(title = tt, x = "MPI", y = "Standardised rate") +
    # scale_x_log10() +
    # { if (!is.na(y_lim[1])) ylim(y_lim) } + 
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10")) 
}




