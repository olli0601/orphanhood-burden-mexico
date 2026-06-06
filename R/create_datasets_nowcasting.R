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
source("utils.R")

df <- foreign::read.dbf("/Users/elsafarinella/Desktop/Orphanhood Mexico/datasets/fertility_1990_2016/fertility_mexico_2014.dbf", as.is = FALSE)
df <- readr::read_delim("/Users/elsafarinella/Desktop/Orphanhood Mexico/datasets/fertility/fertility_mexico_2017.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Get a list of CSV files in the "fertility" folder
year_regex <- "(199[8-9]|20[0-9]{2})"

csv_files_fert <- list.files(
  "../datasets/fertility",
  pattern = str_c("fertility_mexico_", year_regex, "\\.csv$"),
  full.names = TRUE
)

dbf_files_fert <- list.files(
  "../datasets/fertility_1990_2016",
  pattern = str_c("fertility_mexico_", year_regex, "\\.dbf$"),
  full.names = TRUE
)

file_list_fert <- c(dbf_files_fert, csv_files_fert)

# Loop through each file, extract the year, preprocess, and assign it to an object
for(file in file_list_fert) {
  print("Reading")
  # Extract the year from the filename
  year_reg <- str_extract(basename(file), "[0-9]{4}")
  
  # Process the file using the preprocess_mortality function
  df_processed <- preprocess_fertility_day(file)
  
  # Add the year to the processed dataframe
  df_processed$year_reg <- year_reg
  
  # Dynamically create a variable name for each year's dataset
  assign(paste0("fert_", year_reg), df_processed)
  saveRDS(df_processed, file = paste0("../datasets/fert_", year_reg, ".RDS"))
  print("Preprocessed completed")
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
  fert_1999, fert_1998
) |>
  group_by(date_occ, sex, age, mun, date_reg) |>
  summarise(births = sum(births), .groups = "drop")


# ---- 1. parameters you might tweak -----------------------------------------
years      <- 1998:2023                   # the suffixes on your objects
csv_dir    <- "fertility_csv"             # folder to keep the CSV files
zip_name   <- "fertility_data.zip"        # final zip file name
add_readme <- TRUE                        # set FALSE if you don't want it
# ----------------------------------------------------------------------------

# create a clean sub-folder to avoid scooping up unrelated files
if (dir.exists(csv_dir)) unlink(csv_dir, recursive = TRUE)
dir.create(csv_dir)

# ---- 2. write each fert_<year> to CSV --------------------------------------
for (yr in years) {
  obj_name <- paste0("fert_", yr)               # e.g. "fert_1998"
  if (!exists(obj_name, envir = .GlobalEnv)) {
    warning(obj_name, " was not found; skipping.")
    next
  }
  df        <- get(obj_name, envir = .GlobalEnv)
  out_file  <- file.path(csv_dir, paste0(obj_name, ".csv"))
  utils::write.csv(df, out_file, row.names = FALSE)
}



