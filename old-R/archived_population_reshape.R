# old-R/archived_population_reshape.R
# Inline population read+reshape blocks removed from ch1_005/ch1_040/ch1_060
# and replaced by the shared R/load_population() helper. Kept for reference.
# ============================================================================


# --- from ch1_060_prepare_datasets.R (keep_child = TRUE) ---
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

# --- from ch1_040_clean_mort.R (keep_child = FALSE) ---
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
  ) |> filter(age != "TOTAL" & age != "00-04" & age != "05-09" & age!= "85-mm" & age != "80-84")

population$sex <- as.factor(population$sex)
population$age <- as.factor(population$age)
population$mun <- as.factor(population$mun)

# --- from ch1_005_bootstrap_from_processed.R (keep_child = FALSE) ---
X1_Grupo_Quinq_00_RM <- read_excel(
  "input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx")
population <- X1_Grupo_Quinq_00_RM |>
  rename(mun = CLAVE, state = CLAVE_ENT, mun_name = NOM_MUN,
         state_name = NOM_ENT, sex = SEXO, year = AÑO) |>
  pivot_longer(cols = starts_with("POB_"), values_to = "population", names_to = "age") |>
  mutate(sex = case_when(sex == "HOMBRES" ~ "male", sex == "MUJERES" ~ "female")) |>
  mutate(mun = str_pad(mun, 5, pad = "0")) |>
  filter(year >= 1990 & year < 2024) |>
  mutate(age = str_replace(str_replace(age, "POB_", ""), "_", "-")) |>
  filter(!age %in% c("TOTAL", "00-04", "05-09", "85-mm", "80-84")) |>
  mutate(sex = as.factor(sex), age = as.factor(age), mun = as.factor(mun))
