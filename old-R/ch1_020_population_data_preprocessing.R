# =============================================================================
# ch1_020_population_data_preprocessing.R  ·  Chapter 1 — Population
# Filter and reshape the CONAPO municipal population workbook to the analytical
# resolution (municipality x sex x 5-year age group x year, 1990-2023).
#
# Reads : input-data-raw/population/ (CONAPO quinquennial workbook, CNGMD)
# Writes: input-data-processed/data_pop.rds
# Run after: ch1_010
# =============================================================================

library(readr)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(writexl)
####-------------------------- POPULATION DATA --------------------------------
# Geo information
geo_info <- read_excel("input-data-raw/population/cngmd2023_ayunt_alcald.xlsx", sheet = "1", skip = 6)
geo_info <- geo_info %>% select(1:4)
geo_info <- geo_info %>% rename("state_code"= "00", "state_name" ="Estados Unidos Mexicanos", "municipality_code" = "000", "municipality_name"="...4")
geo_info <- geo_info %>% filter(!is.na(municipality_name))
geo_info$state_code <- as.numeric(geo_info$state_code)
geo_info <- geo_info %>% mutate(municipality_complete_code = paste0(state_code, municipality_code))

# Population
data_pop <- read_excel("input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx")

#eliminate the columns with population size of the age groups 0-4, 5-9, 10-14
data_pop <- data_pop %>%
  select(-"POB_00_04", -"POB_05_09", -"POB_10_14")

#rename the columns in english
data_pop <- data_pop %>% rename("municipality_complete_code"="CLAVE", "state_code" = "CLAVE_ENT", "state_name" ="NOM_ENT", "municipality_name"="NOM_MUN", "year"="AÑO", "gender"="SEXO")

#Filter the years just retaining the time interval 2000-2023
data_pop <- data_pop %>% filter(year >= 2000 & year <= 2023)

# Assuming 'data_pop' is your dataset
data_pop_long <- data_pop %>%
  pivot_longer(cols = starts_with("POB"),  # Select all columns starting with "POB"
               names_to = "age_group",     # Name the new column for age groups
               values_to = "population")   # Name the new column for population size

# Clean up the age_group column to extract the age range
data_pop_long <- data_pop_long %>%
  mutate(age_group = gsub("POB_", "", age_group),  # Remove "POB_" from the column names
         age_group = gsub("_", "-", age_group))    # Replace underscores with hyphens

data_pop_long <- data_pop_long %>% filter(age_group != "TOTAL" & age_group!= "85-mm")

data_pop_filtered <- data_pop_long %>%
  filter(!(gender == "MUJERES" & age_group >= "65-69"))


data_pop_filtered <- data_pop_filtered %>% 
  mutate(gender = case_when(
    gender == "HOMBRES" ~ "male",
    gender == "MUJERES" ~ "female"
  ))
## SAVE DATASET
saveRDS(data_pop_filtered, "input-data-processed/data_pop.rds")
