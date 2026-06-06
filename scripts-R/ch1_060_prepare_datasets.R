# =============================================================================
# ch1_060_prepare_datasets.R  ·  Chapter 1 — Analytical panel builder
# Master assembly script: re-aggregate births/deaths/population/marginalization
# to the new (merged) municipalities, build the long birth/death panels, the
# monthly-birth nowcasting input, and the area-type lookups.
#
# Reads : input-data-raw/ (per-year births/deaths, IMM 2010/2015/2020, type_of_mun),
#         input-data-processed/{grouped_municipality_50000, aggregated_muni_50000,
#         population_new_mun, index_new_mun}.RDS (ch2)
# Writes: input-data-processed/{deaths, births, *_new_mun, geo_info*, population*,
#         mpi_*, marg_index, month_*, monthly_births.parquet, rural_urban_area.parquet}
# Run after: ch1_050, ch2_010
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
source("../R/utils.R")
library(arrow)
write_parquet(mpi_new_mun, "input-data-processed/mpi_new_mun.parquet")
#############################################

# MORTALITY

#############################################
# Get a list of CSV files in the "mortality" folder
csv_files <- list.files("input-data-raw/deaths/mortality", 
                        pattern = "mortality_mexico_[0-9]{4}\\.csv", 
                        full.names = TRUE)

# Get a list of DBF files in the "mortality_1990_2011" folder
dbf_files <- list.files("input-data-raw/deaths/mortality_1990_2011", 
                        pattern = "mortality_mexico_[0-9]{4}\\.dbf", 
                        full.names = TRUE)


file_list <- c(dbf_files, csv_files)

# Loop through each file, extract the year, preprocess, and assign it to an object
for(file in file_list) {
  # Extract the year from the filename
  year_reg <- str_extract(basename(file), "[0-9]{4}")
  
  # Process the file using the preprocess_mortality function
  df_processed <- preprocess_mortality(file)
  
  # Add the year to the processed dataframe
  df_processed$year_reg <- year_reg
  
  # Dynamically create a variable name for each year's dataset
  assign(paste0("mort_", year_reg), df_processed)
  #saveRDS(df_processed, file = paste0("input-data-processed/mort_", year_reg, ".RDS"))
}

mort_names <- c("mort_1997", "mort_1996", "mort_1995", "mort_1994", 
                "mort_1993", "mort_1992", "mort_1991", "mort_1990")


for(name in mort_names){
  df <- get(name)       
  df <- correct_year(df) 
  assign(name, df)      
}


# Combine the cleaned datasets together and aggregate counts in case of duplicate groups
deaths <- rbind(mort_2023, mort_2022, mort_2021, mort_2020, mort_2019, 
                mort_2018, mort_2017, mort_2016, mort_2015, mort_2014, 
                mort_2013, mort_2012, mort_2011, mort_2010, mort_2009, 
                mort_2008, mort_2007, mort_2006, mort_2005, mort_2004,
                mort_2003, mort_2002, mort_2001, mort_2000, mort_1999,
                mort_1998, mort_1997, mort_1996, mort_1995, mort_1994,
                mort_1993, mort_1992, mort_1991, mort_1990) |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(deaths = sum(deaths), .groups = "drop")

#deaths <- deaths |> filter(year >= 1990 & year < 2024)
saveRDS(deaths, file = "input-data-processed/deaths.RDS")


#---------------------------------------
# Loop through each file, extract the year, preprocess, and assign it to an object
for(file in file_list) {
  # Extract the year from the filename
  year_reg <- str_extract(basename(file), "[0-9]{4}")
  
  # Process the file using the preprocess_mortality function
  df_processed <- preprocess_orphans(file)
  
  # Add the year to the processed dataframe
  df_processed$year_reg <- year_reg
  
  # Dynamically create a variable name for each year's dataset
  assign(paste0("mort_", year_reg), df_processed)
  #saveRDS(df_processed, file = paste0("input-data-processed/mort_", year_reg, ".RDS"))
}

mort_names <- c("mort_1997", "mort_1996", "mort_1995", "mort_1994", 
                "mort_1993", "mort_1992", "mort_1991", "mort_1990")


for(name in mort_names){
  df <- get(name)       
  df <- correct_year(df) 
  assign(name, df)      
}


# Combine the cleaned datasets together and aggregate counts in case of duplicate groups
mort_df <- rbind(mort_2023, mort_2022, mort_2021, mort_2020, mort_2019, 
                 mort_2018, mort_2017, mort_2016, mort_2015, mort_2014, 
                 mort_2013, mort_2012, mort_2011, mort_2010, mort_2009, 
                 mort_2008, mort_2007, mort_2006, mort_2005, mort_2004,
                 mort_2003, mort_2002, mort_2001, mort_2000, mort_1999,
                 mort_1998, mort_1997, mort_1996, mort_1995, mort_1994,
                 mort_1993, mort_1992, mort_1991, mort_1990) |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  filter(year >=1990)

saveRDS(mort_df, "input-data-processed/mort_df.RDS")

################################################################################

#------------------------------ DEATHS SINGLE AGE ------------------------------

################################################################################
# Loop through each file, extract the year, preprocess, and assign it to an object
for(file in file_list) {
  # Extract the year from the filename
  year_reg <- str_extract(basename(file), "[0-9]{4}")
  
  # Process the file using the preprocess_mortality function
  df_processed <- preprocess_mortality_long(file)
  
  # Add the year to the processed dataframe
  df_processed$year_reg <- year_reg
  
  # Dynamically create a variable name for each year's dataset
  assign(paste0("mort_", year_reg), df_processed)
  #saveRDS(df_processed, file = paste0("input-data-processed/mort_", year_reg, ".RDS"))
}

mort_names <- c("mort_1997", "mort_1996", "mort_1995", "mort_1994", 
                "mort_1993", "mort_1992", "mort_1991", "mort_1990")


for(name in mort_names){
  df <- get(name)       
  df <- correct_year(df) 
  assign(name, df)      
}


# Combine the cleaned datasets together and aggregate counts in case of duplicate groups
deaths_df_long <- rbind(mort_2023, mort_2022, mort_2021, mort_2020, mort_2019, 
                 mort_2018, mort_2017, mort_2016, mort_2015, mort_2014, 
                 mort_2013, mort_2012, mort_2011, mort_2010, mort_2009, 
                 mort_2008, mort_2007, mort_2006, mort_2005, mort_2004,
                 mort_2003, mort_2002, mort_2001, mort_2000, mort_1999,
                 mort_1998, mort_1997, mort_1996, mort_1995, mort_1994,
                 mort_1993, mort_1992, mort_1991, mort_1990) |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  filter(year >=1990)

saveRDS(deaths_df_long, "input-data-processed/deaths_df_long.RDS")


grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
aggregated_muni_50000 <- readRDS("input-data-processed/aggregated_muni_50000.RDS")
################################################################################

#---------------------------------- GEO INFO ----------------------------------

################################################################################

IMM_2020 <- read_excel("input-data-raw/marginalization/IMM_2020.xls", skip = 5)

index <- IMM_2020 |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    illiterate_pct = ANALF, 
    no_basic_edu_pct = SBASC, 
    no_drainage_pct = OVSDE, 
    no_electricity_pct = OVSEE, 
    no_piped_water_pct = OVSAE, 
    dirt_floors_pct = OVPT, 
    overcrowding_pct = VHAC, 
    small_towns_pct = PL.5000, 
    low_income_pct = PO2SM, 
    IM = IM_2020,
    IMN = IMN_2020,
    GM = GM_2020
  ) |> drop_na(IM)

index <- index[-1,]

geo_info <- index |> dplyr::select(state, state_name, mun, mun_name)
colSums(is.na(geo_info))
capitals <- c("Aguascalientes", "Mexicali", "La Paz", "Campeche", "Tuxtla Gutiérrez", 
              "Chihuahua", "Saltillo", "Colima", "Durango", "Guanajuato", 
              "Chilpancingo de los Bravo", "Pachuca de Soto", "Guadalajara",
              "Toluca", "Morelia", "Cuernavaca", "Tepic", "Monterrey", "Oaxaca de Juárez", 
              "Puebla", "Querétaro", "Othón P. Blanco", "San Luis Potosí", 
              "Culiacán", "Hermosillo", "Centro", "Victoria", 
              "Tlaxcala", "Xalapa", "Mérida", "Zacatecas")

geo_info <- geo_info |> mutate(
  capital = if_else(mun_name %in% capitals, 1, 0)
)

geo_info$capital[geo_info$state_name == "Guanajuato" & geo_info$mun_name == "Victoria"] <- 0

geo_info <- geo_info %>%
  left_join(grouped_municipality_50000 |>
              select(mun, group_id) |>
              sf::st_drop_geometry(), by = "mun") 

geo_info <- geo_info |>
  filter(!is.na(group_id))

geo_info_new_mun <- geo_info %>%
  filter(!is.na(group_id)) %>%
  group_by(group_id) %>%
  summarise(
    geometry = st_union(geometry),                     
    state_name = first(state_name),                    
    state = first(state),                    
    capital = first(capital),                          
    .groups = "drop"
  )

geo_info_new_mun$capital[geo_info_new_mun$group_id %in% c("06002", "30087")] <- 1


saveRDS(geo_info, file = "input-data-processed/geo_info.RDS")
saveRDS(geo_info_new_mun, file = "input-data-processed/geo_info_new_mun.RDS")

################################################################################

#--------------------------------- POPULATION ----------------------------------

################################################################################
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
  ) |> filter(age != "TOTAL" & age!= "85-mm" & age != "80-84")

population$sex <- as.factor(population$sex)
population$age <- as.factor(population$age)
population$mun <- as.factor(population$mun)

population <- population |>
  left_join(geo_info |>
              select(mun, group_id) |>
              sf::st_drop_geometry(), by = "mun")

population_new_mun <- population |> 
  group_by(group_id, year, sex, age) |>
  summarise(population = sum(population), .groups = "drop")

saveRDS(population, file = "input-data-processed/population.RDS")
saveRDS(population_new_mun, file = "input-data-processed/population_new_mun.RDS")


################################################################################

#------------------------------------- MPI -------------------------------------

################################################################################

IMM_2020 <- read_excel("input-data-raw/marginalization/IMM_2020.xls", skip = 5)
IMM_2015 <- read_excel("input-data-raw/marginalization/IMM_DP2_2015.xlsx", sheet = "IMM_2015")
IMM_2010 <- read_excel("input-data-raw/marginalization/IMM_DP2_2010.xlsx", sheet = "IMM_2010")

index_2020 <- IMM_2020 |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    illiterate_pct = ANALF, 
    no_basic_edu_pct = SBASC, 
    no_drainage_pct = OVSDE, 
    no_electricity_pct = OVSEE, 
    no_piped_water_pct = OVSAE, 
    dirt_floors_pct = OVPT, 
    overcrowding_pct = VHAC, 
    small_towns_pct = PL.5000, 
    low_income_pct = PO2SM, 
    IM = IM_2020,
    IMN = IMN_2020,
    GM = GM_2020
  ) |> drop_na(IM)

index_2015 <- IMM_2015 |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    illiterate_pct = ANALF, 
    no_basic_edu_pct = SBASC, 
    no_drainage_pct = OVSDE, 
    no_electricity_pct = OVSEE, 
    no_piped_water_pct = OVSAE, 
    dirt_floors_pct = OVPT, 
    overcrowding_pct = VHAC, 
    small_towns_pct = PL.5000, 
    low_income_pct = PO2SM, 
    IM = IM_2015,
    IMN = IMN_2015,
    GM = GM_2015
  ) |> drop_na(IM)

index_2010 <- IMM_2010 |>
  rename(
    mun = CVE_MUN,
    state = CVE_ENT,
    mun_name = NOM_MUN,
    state_name = NOM_ENT,
    population = POB_TOT,
    illiterate_pct = ANALF, 
    no_basic_edu_pct = SBASC, 
    no_drainage_pct = OVSDE, 
    no_electricity_pct = OVSEE, 
    no_piped_water_pct = OVSAE, 
    dirt_floors_pct = OVPT, 
    overcrowding_pct = VHAC, 
    small_towns_pct = PL.5000, 
    low_income_pct = PO2SM, 
    IM = IM_2010,
    IMN = IMN_2010,
    GM = GM_2010
  ) |> drop_na(IM)

index_2020 <- index_2020[-1,]

index_2020 <- index_2020 %>%
  mutate(IMN = as.numeric(IMN), year=2020)

index_2015 <- index_2015 %>%
  mutate(IMN = as.numeric(IMN), year=2015)

index_2010 <- index_2010 %>%
  mutate(IMN = as.numeric(IMN), year=2010)

combined_index <- bind_rows(
  index_2020 %>% dplyr:: select(GM, IMN, mun, year, population),
  index_2015 %>% dplyr:: select(GM, IMN, mun, year, population),
  index_2010 %>% dplyr:: select(GM, IMN, mun, year, population)
)

saveRDS(combined_index, "input-data-processed/mpi_mun.RDS")
grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
grouped_municipality_50000 <- grouped_municipality_50000 |>
  st_drop_geometry() |>
  select(mun, group_id)

# Perform the join
combined_index_new_mun <- left_join(
  combined_index,
  grouped_municipality_50000,
  by = "mun"
)

mpi_new_mun <- combined_index_new_mun |>
  group_by(group_id, year) |>
  summarise(mpi = weighted.mean(IMN, w = population), .groups = "drop")

thresholds <- combined_index_new_mun |>
  group_by(GM) |>
  summarise(
    min_value = min(IMN, na.rm = TRUE),
    max_value = max(IMN, na.rm = TRUE)
  ) |>
  arrange(min_value)

classified_data <- combined_index_new_mun %>%
  group_by(group_id, year) %>%
  summarise(IMN = mean(IMN, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    GM = case_when(
      IMN >= thresholds$min_value[thresholds$GM == "Muy alto"] &
        IMN <= thresholds$max_value[thresholds$GM == "Muy alto"] ~ "Very High",
      IMN >= thresholds$min_value[thresholds$GM == "Alto"] &
        IMN <= thresholds$max_value[thresholds$GM == "Alto"] ~ "High",
      IMN >= thresholds$min_value[thresholds$GM == "Medio"] &
        IMN <= thresholds$max_value[thresholds$GM == "Medio"] ~ "Medium",
      IMN >= thresholds$min_value[thresholds$GM == "Bajo"] &
        IMN <= thresholds$max_value[thresholds$GM == "Bajo"] ~ "Low",
      IMN >= thresholds$min_value[thresholds$GM == "Muy bajo"] &
        IMN <= thresholds$max_value[thresholds$GM == "Muy bajo"] ~ "Very Low",
      TRUE ~ NA_character_
    )
  )

saveRDS(classified_data, file = "input-data-processed/mpi_classified.RDS")

#------------------------------------------------------------------------------
index <- mpi_new_mun %>%
  complete(group_id, year = 2010:2020) %>%
  group_by(group_id) %>%
  arrange(year, .by_group = TRUE) %>%
  group_modify(~ {
    non_na_vals <- .x %>% filter(!is.na(mpi))
    
    if (nrow(non_na_vals) >= 2) {
      .x$mpi <- approx(
        x = non_na_vals$year,
        y = non_na_vals$mpi,
        xout = .x$year
      )$y
    } else if (nrow(non_na_vals) == 1) {
      .x$mpi <- rep(non_na_vals$mpi, nrow(.x))
    } else {
      # all values are NA — leave them as NA
      .x$mpi <- NA_real_
    }
    
    return(.x)
  }) %>%
  ungroup()

saveRDS(index, "input-data-processed/mpi_new_mun.RDS")

index <- index %>%
  mutate(
  GM = case_when(
    IMN >= thresholds$min_value[thresholds$GM == "Muy alto"] &
      IMN <= thresholds$max_value[thresholds$GM == "Muy alto"] ~ "Very High",
    IMN >= thresholds$min_value[thresholds$GM == "Alto"] &
      IMN <= thresholds$max_value[thresholds$GM == "Alto"] ~ "High",
    IMN >= thresholds$min_value[thresholds$GM == "Medio"] &
      IMN <= thresholds$max_value[thresholds$GM == "Medio"] ~ "Medium",
    IMN >= thresholds$min_value[thresholds$GM == "Bajo"] &
      IMN <= thresholds$max_value[thresholds$GM == "Bajo"] ~ "Low",
    IMN >= thresholds$min_value[thresholds$GM == "Muy bajo"] &
      IMN <= thresholds$max_value[thresholds$GM == "Muy bajo"] ~ "Very Low",
    TRUE ~ NA_character_
  )
)

saveRDS(index, file = "input-data-processed/marg_index.RDS")

grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
index_new_mun <- left_join(index,
                           grouped_municipality_50000 %>% dplyr::select(mun, group_id), 
                           by="mun")

thresholds <- range_by_category

index_new_mun <- index_new_mun %>%
  group_by(group_id, year) %>%
  summarise(IMN = mean(IMN, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    GM = case_when(
      IMN >= thresholds$min_value[thresholds$GM == "Muy alto"] &
        IMN <= thresholds$max_value[thresholds$GM == "Muy alto"] ~ "Very High",
      IMN >= thresholds$min_value[thresholds$GM == "Alto"] &
        IMN <= thresholds$max_value[thresholds$GM == "Alto"] ~ "High",
      IMN >= thresholds$min_value[thresholds$GM == "Medio"] &
        IMN <= thresholds$max_value[thresholds$GM == "Medio"] ~ "Medium",
      IMN >= thresholds$min_value[thresholds$GM == "Bajo"] &
        IMN <= thresholds$max_value[thresholds$GM == "Bajo"] ~ "Low",
      IMN >= thresholds$min_value[thresholds$GM == "Muy bajo"] &
        IMN <= thresholds$max_value[thresholds$GM == "Muy bajo"] ~ "Very Low",
      TRUE ~ NA_character_
    )
  )

saveRDS(index_new_mun, file = "input-data-processed/index_new_mun.RDS")


# Tag original vs interpolated values
plot_data <- index %>%
  left_join(
    combined_index %>% mutate(source = "original"),
    by = c("mun", "year", "IMN")
  ) %>%
  mutate(source = ifelse(is.na(source), "interpolated", source))

# Plot a sample of municipalities (or all if it's not too many)
plot_data %>%
  filter(mun %in% sample(unique(mun), 6)) %>%
  ggplot(aes(x = year, y = IMN, color = source)) +
  geom_line(aes(group = mun), linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ mun) +
  scale_color_manual(values = c("original" = "black", "interpolated" = "skyblue")) +
  theme_minimal() +
  labs(
    title = "Interpolated IMN per Municipality (Sample)",
    y = "IMN",
    color = "Value Type"
  )



#IM - marginalization index
#IMN - normalized marginalization index
# GM- degree of marginalization

im.tmp <- index |> dplyr:: select(mun, IM, IMN, illiterate_pct, no_basic_edu_pct, 
                                  no_drainage_pct, no_electricity_pct, no_piped_water_pct, 
                                  dirt_floors_pct, overcrowding_pct, small_towns_pct, low_income_pct)

# Calculate the mpi normalized for the new municipalities
pop <- population_new_mun |>
  group_by(group_id, year) |>
  summarise(population = sum(tot_population))

index_marg <- index |>
  left_join(geo_info |> dplyr:: select(mun, group_id), by = "mun") |>
  left_join(pop, by = c("group_id", "year")) |>
  mutate(
    IMN = as.numeric(IMN),
    population = as.numeric(population)
  ) |>
  group_by(group_id, year) |>
  summarise(
    marg_index_weighted = sum(IMN * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    .groups = "drop"
  ) 

saveRDS(im.tmp, file = "input-data-processed/im.tmp.RDS")
saveRDS(index_marg, file = "input-data-processed/index_marg.RDS")
mpi <- readRDS("input-data-processed/index_new_mun.RDS")

################################################################################

#---------------------------------- FERTILITY ----------------------------------

################################################################################

# Get a list of CSV files in the "fertility" folder
csv_files_fert <- list.files("input-data-raw/births/fertility", 
                             pattern = "fertility_mexico_[0-9]{4}\\.csv", 
                             full.names = TRUE)

# Get a list of DBF files in the "fertility_1990_2011" folder
dbf_files_fert <- list.files("input-data-raw/births/fertility_1990_2016", 
                             pattern = "fertility_mexico_[0-9]{4}\\.dbf", 
                             full.names = TRUE)

file_list_fert <- c(dbf_files_fert, csv_files_fert)

# Loop through each file, extract the year, preprocess, and assign it to an object
for(file in file_list_fert) {
  # Extract the year from the filename
  year_reg <- str_extract(basename(file), "[0-9]{4}")
  print(file)
  # Process the file using the preprocess_mortality function
  df_processed <- preprocess_fertility(file)
  
  # Add the year to the processed dataframe
  df_processed$year_reg <- year_reg
  
  # Dynamically create a variable name for each year's dataset
  assign(paste0("fert_", year_reg), df_processed)
  saveRDS(df_processed, file = paste0("input-data-processed/fert_", year_reg, ".RDS"))
  print("File preprocessed")
}

fert_list <- c("fert_1997", "fert_1996", "fert_1995", "fert_1994", 
               "fert_1993", "fert_1992", "fert_1991", "fert_1990")


for(name in fert_list){
  df <- get(name)       
  df <- correct_year(df) 
  assign(name, df)      
}


# Combine the cleaned datasets together and aggregate counts in case of duplicate groups
# Combine and aggregate all years
births <- rbind(
  fert_2023, fert_2022, fert_2021, fert_2020,
  fert_2019, fert_2018, fert_2017, fert_2016, 
  fert_2015, fert_2014, fert_2013, fert_2012, 
  fert_2011, fert_2010, fert_2009, fert_2008,
  fert_2007, fert_2006, fert_2005, fert_2004, 
  fert_2003, fert_2002, fert_2001, fert_2000, 
  fert_1999, fert_1998, fert_1997, fert_1996, 
  fert_1995, fert_1994, fert_1993, fert_1992, 
  fert_1991, fert_1990
) |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(births = sum(births), .groups = "drop")

births <- births |>
  filter(year >= 1990, year < 2024)

saveRDS(births, file = "input-data-processed/births.RDS")
grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
grouped_municipality_50000 <-
  grouped_municipality_50000 |>
  sf::st_drop_geometry()
births <- births |>
  left_join(grouped_municipality_50000, by = "mun")

births <- births |>
  group_by(group_id, year, sex, age, year_reg) |>
  summarise(births = sum(births), .groups = "drop")
saveRDS(births, file = "input-data-processed/births_new_mun.RDS")

#-------------------------------------------------------------------------------
csv_files_fert <- list.files("input-data-raw/births/fertility", 
                             pattern = "fertility_mexico_[0-9]{4}\\.csv", 
                             full.names = TRUE)

# Get a list of DBF files in the "fertility_1990_2011" folder
dbf_files_fert <- list.files("input-data-raw/births/fertility_1990_2016", 
                             pattern = "fertility_mexico_[0-9]{4}\\.dbf", 
                             full.names = TRUE)

file_list_fert <- c(dbf_files_fert, csv_files_fert)

# Loop through each file, extract the year, preprocess, and assign it to an object
for(file in file_list_fert) {
  # Extract the year from the filename
  year_reg <- str_extract(basename(file), "[0-9]{4}")
  print(file)
  # Process the file using the preprocess_mortality function
  df_processed <- preprocess_fertility_month(file)
  
  # Add the year to the processed dataframe
  df_processed$year_reg <- year_reg
  
  # Dynamically create a variable name for each year's dataset
  assign(paste0("month_fert_", year_reg), df_processed)
  saveRDS(df_processed, file = paste0("input-data-processed/month_fert_", year_reg, ".RDS"))
  print("File preprocessed")
}

# Combine the cleaned datasets together and aggregate counts in case of duplicate groups
# Combine and aggregate all years
library(data.table)          # C-speed joins & in-place ops

## 1 ─────────────────── Bind the 1990-2023 tables --------------------------------
fert_names   <- ls(pattern = "^month_fert_\\d{4}$")   # all objects already in memory
month_births <- rbindlist(mget(fert_names), use.names = TRUE)  # one pass, no copies

## (optional) free the individual tables to reclaim RAM
rm(list = fert_names); gc()

## 2 ─────────────────── Create date columns in-place ------------------------------
month_births <- month_births |>
  dplyr::mutate(date_occ = lubridate::make_date(year_occ, month_occ, 1),
                date_reg = lubridate::make_date(year_reg, month_reg, 1)) |>
  dplyr::select(-year_occ, -month_occ, -year_reg, -month_reg)

## --- 1.1  Convert to data.table ---------------------------------------------
library(data.table)
library(sf)



library(data.table)   # rbindlist() for one-pass binding
library(stringr)      # str_extract() if you want the year later

## 1 ── build full paths & keep only the ones that exist ------------------------
yrs         <- 1990:2023
paths       <- sprintf("input-data-processed/month_fert_%d.RDS", yrs)
paths_exist <- paths[file.exists(paths)]

if (length(paths_exist) == 0) stop("No month_fert_*.RDS files found in ../datasets")

## 2 ── read them all, name each element by its year ----------------------------
fert_list <- setNames(
  lapply(paths_exist, readRDS),
  str_extract(paths_exist, "\\d{4}")           # gives "1990", "1991", …
)

## 3 ── bind into one big table (keeps column names in correct order) ----------
month_births <- rbindlist(fert_list, use.names = TRUE, fill = TRUE)





## 1 ─────────────────── drop geometry ------------------------------------------
grouped_municipality_50000 <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
group_map <- grouped_municipality_50000 |>
  sf::st_drop_geometry()

## 2 ─────────────────── convert + keep the two columns -------------------------
setDT(group_map)                       # in-place, returns invisibly
group_map <- group_map[ , .(mun, group_id)]


## --- 1.2  Key both tables on 'mun' for a lightning-fast join -----------------
setDT(month_births)
setkey(month_births, mun)
setkey(group_map,     mun)

result <- group_map[month_births]      # left-join  
result <- result[ ,
                   .(births = sum(births)),                 # collapse rows
                   by = .(group_id, sex, age, year_occ, year_reg, month_occ, month_reg)
]

write_parquet(result, "input-data-processed/result.parquet")
month_births <- read_parquet("input-data-processed/monthly_births.parquet")
result[ ,
         delay := 12 * (as.numeric(year_reg)  - as.numeric(year_occ)) +
           (month_reg - month_occ)      # integer months
]

pop <- readRDS("input-data-processed/population_new_mun.RDS")

month_births <- result

saveRDS(month_births, file = "input-data-processed/month_births.RDS")
install.packages("arrow")      # only once
library(arrow)
write_parquet(month_births, "input-data-processed/monthly_births.parquet")
#-------------------------------------------------------------------------------
# Get a list of CSV files in the "fertility" folder
csv_files_fert <- list.files("input-data-raw/births/fertility", 
                             pattern = "fertility_mexico_[0-9]{4}\\.csv", 
                             full.names = TRUE)

# Get a list of DBF files in the "fertility_1990_2011" folder
dbf_files_fert <- list.files("input-data-raw/births/fertility_1990_2016", 
                             pattern = "fertility_mexico_[0-9]{4}\\.dbf", 
                             full.names = TRUE)


file_list_fert <- c(dbf_files_fert, csv_files_fert)

# Loop through each file, extract the year, preprocess, and assign it to an object
for(file in file_list_fert) {
  print(file)
  # Extract the year from the filename
  year_reg <- str_extract(basename(file), "[0-9]{4}")
  
  # Process the file using the preprocess_mortality function
  df_processed <- preprocess_fertility_long(file)
  
  # Add the year to the processed dataframe
  df_processed$year_reg <- year_reg
  
  # Dynamically create a variable name for each year's dataset
  assign(paste0("fert_", year_reg), df_processed)
  saveRDS(df_processed, file = paste0("input-data-processed/fert_", year_reg, ".RDS"))
  print("file save and pre processed")
}

fert_list <- c("fert_1997", "fert_1996", "fert_1995", "fert_1994", 
               "fert_1993", "fert_1992", "fert_1991", "fert_1990")


for(name in fert_list){
  df <- get(name)       
  df <- correct_year(df) 
  assign(name, df)      
}


# Combine the cleaned datasets together and aggregate counts in case of duplicate groups
# Combine and aggregate all years
births_long <- rbind(
  fert_2023, fert_2022, fert_2021, fert_2020,
  fert_2019, fert_2018, fert_2017, fert_2016, 
  fert_2015, fert_2014, fert_2013, fert_2012, 
  fert_2011, fert_2010, fert_2009, fert_2008,
  fert_2007, fert_2006, fert_2005, fert_2004, 
  fert_2003, fert_2002, fert_2001, fert_2000, 
  fert_1999, fert_1998, fert_1997, fert_1996, 
  fert_1995, fert_1994, fert_1993, fert_1992, 
  fert_1991, fert_1990
) |>
  group_by(year, sex, age, mun, year_reg) |>
  summarise(births = sum(births), .groups = "drop")

births_long <- births_long |>
  filter(year >= 1990, year < 2024)

saveRDS(births_long, file = "input-data-processed/births_long.RDS")


library(dplyr)
library(readr)

type_of_mun <- read_csv("input-data-raw/type_of_mun.csv") |>
  select(
    CVE_ENT, CVE_MUN, AMBITO, POB_TOTAL   # keep only what you need
  ) |>
  mutate(
    mun  = paste0(as.character(CVE_ENT), as.character(CVE_MUN)),
    type = recode(AMBITO,
                  "U" = "Urban",
                  "R" = "Rural",
                  .default = NA_character_)     # flag any unexpected code
  ) |>
  rename(tot_pop = POB_TOTAL) |>
  group_by(mun) |>
  summarise(
    type     = first(type),        # or use `unique(type)` if you worry about clashes
    tot_pop  = sum(as.numeric(tot_pop), na.rm = T),       # keep the population while we're here
    .groups  = "drop"              # ungroups the result
  )


library(dplyr)
library(sf)

cutoff <- 30   # change this once and everything else stays in sync

rural_urban_area <- readRDS("input-data-processed/grouped_municipality_50000.RDS") |>
  # 1 ── make every source polygon valid ---------------------------------
mutate(geometry = sf::st_make_valid(geometry)) |>
  
  # 2 ── dissolve municipalities inside each group -----------------------
group_by(group_id) |>
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    geometry  = sf::st_union(geometry),    # dissolve into one multipart geom
    .groups   = "drop"
  ) |>
  
  # 3 ── (re-)validate the dissolved geometry just in case --------------
mutate(geometry = sf::st_make_valid(geometry)) |>
  
  # 4 ── compute area, density, tag urban / rural ------------------------
mutate(
  km2       = as.numeric(sf::st_area(geometry)) / 1e6,     # m² → km²
  density   = total_pop / km2,
  area_type = if_else(
    density >= cutoff,
    "Urban",
    "Rural")
  )


table(rural_urban_area$area_type)

rural_urban_area <- rural_urban_area |>
  st_drop_geometry() |>
  select(group_id, area_type)
library(arrow)
write_parquet(rural_urban_area, "input-data-processed/rural_urban_area.parquet")
