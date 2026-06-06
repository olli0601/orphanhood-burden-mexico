library(sf)
library(spdep)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(viridis)
source("utils.R")

setwd("/Users/elsafarinella/Desktop/Orphanhood Mexico/R code")
# Replace with your actual GeoJSON file path
mex_muni <- st_read("/Users/elsafarinella/Desktop/gadm41_MEX_2.json")

# Make sure the municipality ID column is character
names(mex_muni) <- tolower(names(mex_muni))  # Optional cleanup

mex_muni <- mex_muni |> 
  select(name_1, name_2, geometry) |>
  rename(
    state_name = name_1, 
    mun_name = name_2
  )

population <- readRDS("../datasets/population.RDS")
geo_info <- readRDS("../datasets/geo_info.RDS")
# Remove all spaces from your municipality names in the main dataset
mex_muni <- mex_muni %>%
  mutate(mun_name = str_remove_all(mun_name, "\\s+"),
         mun_name = stri_trans_general(mun_name, "Latin-ASCII"))

mex_muni <-  mex_muni %>%
  mutate(state_name = recode(state_name,
                            "Coahuila" = "CoahuiladeZaragoza",
                            "DistritoFederal" = "CiudaddeMéxico",
                            "Michoacán" = "MichoacándeOcampo",
                            "Veracruz" = "VeracruzdeIgnaciodelaLlave"), 
         mun_name = recode(mun_name, 
                           "SantiagoTulantepecdeLugoGuer" = "SantiagoTulantepecdeLugoGuerrero", 
                           "Batopilas" = "BatopilasdeManuelGomezMorin", 
                           "DoloresHidalgoCunadelaIndep" = "DoloresHidalgoCunadelaIndependenciaNacional", 
                           "CoahuayutladeJoseMariaIzazag" = "CoahuayutladeJoseMariaIzazaga", 
                           "LaUniondeIsidoroMontesdeOc" = "LaUniondeIsidoroMontesdeOca", 
                           "Jonacatepec" = "JonacatepecdeLeandroValle", 
                           "Zacualpan" = "ZacualpandeAmilpas",
                           "HeroicaCiudaddeEjutladeCres" = "HeroicaCiudaddeEjutladeCrespo", 
                           "HeroicaCiudaddeHuajuapandeL" = "HeroicaCiudaddeHuajuapandeLeon",
                           "HeroicaCiudaddeJuchitandeZa" = "HeroicaCiudaddeJuchitandeZaragoza",
                           "HeroicaVillaTezoatlandeSegur" = "HeroicaVillaTezoatlandeSegurayLuna,CunadelaIndependenciadeOaxaca", 
                           "MagdalenaYodoconodePorfirioD" = "MagdalenaYodoconodePorfirioDiaz", 
                           "SanJuanBautistaTlacoatzintepe" = "SanJuanBautistaTlacoatzintepec", 
                           "SanJuanMixtepec-Dto.08-" = "SanJuanMixtepec-Dto.08", 
                           "SanJuanMixtepec-Dto.26-" = "SanJuanMixtepec-Dto.26", 
                           "SanMateoYucutindo" = "SanMateoYucutindoo", 
                           "SanPedroMixtepec-Dto.22-" = "SanPedroMixtepec-Dto.22", 
                           "SanPedroMixtepec-Dto.26-" = "SanPedroMixtepec-Dto.26", 
                           "SanPedroySanPabloTeposcolul" = "SanPedroySanPabloTeposcolula", 
                           "SanPedroySanPabloTequixtepe" = "SanPedroySanPabloTequixtepec", 
                           "VilladeTamazulapamdelProgres" = "VilladeTamazulapamdelProgreso", 
                           "VilladeTututepecdeMelchorOc" = "VilladeTututepecdeMelchorOcampo", 
                           "MazatecochcodeJoseMariaMorel" = "MazatecochcodeJoseMariaMorelos", 
                           "ZiltlaltepecdeTrinidadSanchez" = "ZiltlaltepecdeTrinidadSanchezSantos", 
                           "Medellin" = "MedellindeBravo", 
                           "NanchitaldeLazaroCardenasdel" = "NanchitaldeLazaroCardenasdelRio"
                           ))

# Remove all spaces from the reference names in geo_info
geo_info <- geo_info %>%
  mutate(mun_name = str_remove_all(mun_name, "\\s+"), 
         state_name = str_remove_all(state_name, "\\s+"), 
         mun_name = stri_trans_general(mun_name, "Latin-ASCII"))

mex_muni <- mex_muni %>%
  mutate(
    mun_name = case_when(
      state_name == "Morelos" & mun_name == "ZacualpandeAmilpas" ~ "ZacualpandeAmilpas",
      mun_name == "ZacualpandeAmilpas" ~ "Zacualpan",
      TRUE ~ mun_name
    )
  )

mex_muni <- left_join(mex_muni, geo_info |>
                        select(mun_name, mun, state_name), by = c("mun_name", "state_name"))

pop_2023 <- population %>%
  filter(year == 2023) %>%
  group_by(mun) %>%
  summarise(total_pop = sum(population, na.rm = TRUE), .groups = "drop")

mex_muni <- left_join(mex_muni, pop_2023, by= "mun")

# Plot using ggplot2
ggplot(data = mex_muni) +
  geom_sf(aes(fill = log(total_pop))) +
  scale_fill_viridis_c(option = "viridis", name = "Population") +
  theme_minimal() +
  labs(title = "Map Colored by Population")

saveRDS(mex_muni, file = "../datasets/mex_muni.RDS")

## --------------------------- GROUPING -----------------------------
#aggregated_muni <- group_small_municipalities(mex_muni, threshold = 100000)
aggregated <- group_until_threshold(mex_muni, threshold = 30000, max_iter = 50)

aggregated_muni_30000 <- aggregated$aggregated
saveRDS(aggregated_muni_30000, file = "../datasets/aggregated_muni_30000.RDS")

new_mun <- aggregated$muni_ref_with_groups
saveRDS(new_mun, file = "../datasets/grouped_municipality_30000.RDS")

#--------------------------------------------
aggregated_50000 <- group_until_threshold(mex_muni, threshold = 50000, max_iter = 50)
aggregated_muni_50000 <- aggregated_50000$aggregated
saveRDS(aggregated_muni_50000, file = "../datasets/aggregated_muni_50000.RDS")
aggregated_muni_50000 <- readRDS("../datasets/aggregated_muni_50000.RDS")

new_mun_50000 <- aggregated_50000$muni_ref_with_groups
saveRDS(new_mun_50000, file = "../datasets/grouped_municipality_50000.RDS")
grouped_municipality_50000 <- readRDS("../datasets/grouped_municipality_50000.RDS")

sum(aggregated_muni_30000$total_pop < 30000)

ggplot(data = aggregated_muni) +
  geom_sf(aes(fill = log(total_pop))) +
  scale_fill_viridis_c(option = "viridis", name = "Log Population") +
  theme_minimal() +
  labs(
    title = "Grouped Municipalities by Population",
    subtitle = "Each group meets the minimum threshold of 15,000 people",
    fill = "Log(Pop)"
  )


library(viridis)  # for scale_fill_viridis_d
aggregated_muni_50000 <- readRDS(file = "../datasets/aggregated_muni_50000.RDS")
new_mun_50000 <- readRDS(file = "../datasets/grouped_municipality_50000.RDS")


ggplot() +
  geom_sf(data = new_mun_50000, aes(fill = as.factor(group_id)), color = "white", size = 0.1) +
  #geom_sf_text(data = new_mun, aes(label = mun), size = 1.8, color = "black") +
  scale_fill_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(
    title = "Original Municipalities Colored by Grouped Municipality",
    subtitle = "Each color = a group (group_id); Labels = original municipality ID (mun)",
    fill = "Group ID"
  ) +
  theme(legend.position = "none")

new_mun_50000 <- sf::st_make_valid(new_mun_50000)
aggregated_muni_50000 <- sf::st_make_valid(aggregated_muni_50000)


ggplot() +
  geom_sf(data = new_mun_50000, fill = NA, color = "grey70", size = 0.1) +
  geom_sf(data = aggregated_muni_50000, fill = NA, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Group Boundaries Overlaid on Original Municipalities")

ggplot() +
  #geom_sf(data = new_mun_50000, fill = NA, color = "grey70", size = 0.1) +
  #geom_sf(data = aggregated_muni, fill = NA, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Group Boundaries Overlaid on Original Municipalities")


#------------ RELATIONSHIP WITH MPI ---------------
mpi_new_mun <- readRDS("../datasets/mpi_new_mun.RDS")
mpi_mun <- readRDS("../datasets/mpi_mun.RDS")

mpi_mun <- mpi_mun %>%
  select(mun, year, IMN)
  complete(mun, year = 2010:2020) %>%
  group_by(mun) %>%
  arrange(year, .by_group = TRUE) %>%
  group_modify(~ {
    non_na_vals <- .x %>% filter(!is.na(IMN))
    
    if (nrow(non_na_vals) >= 2) {
      .x$IMN <- approx(
        x = non_na_vals$year,
        y = non_na_vals$IMN,
        xout = .x$year
      )$y
    } else if (nrow(non_na_vals) == 1) {
      .x$IMN <- rep(non_na_vals$IMN, nrow(.x))
    } else {
      # all values are NA — leave them as NA
      .x$IMN <- NA_real_
    }
    
    return(.x)
  }) %>%
  ungroup()

mpi <- mpi_mun |>
  left_join(grouped_municipality_50000, by="mun") 


within_group_variance <- mpi |>
  filter(group_id != mun) |>
  group_by(group_id, year) |>
  summarise(
    mean_mpi = mean(IMN, na.rm = TRUE),
    var_mpi = var(IMN, na.rm = TRUE),
    .groups = "drop"
  )
small_variance_groups <- within_group_variance |>
  filter(var_mpi <= 1e-2)

nrow(small_variance_groups)  # total number of group-year combos with low variance


library(ggplot2)

ggplot(within_group_variance, aes(x = factor(year), y = var_mpi)) +
  geom_boxplot() +
  labs(
    title = "Within-Group Variance of IMN (MPI proxy)",
    x = "Year",
    y = "Variance of IMN"
  )


ggplot(within_group_variance, aes(x = year, y = var_mpi, group = group_id)) +
  geom_line(alpha = 0.3) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linewidth = 1) +
  labs(
    title = "Trends in Within-Group Variance of IMN",
    x = "Year",
    y = "Variance of IMN"
  )


################################################################################
#--------------------------- SENSITIVITY ANALYSIS ------------------------------
################################################################################
