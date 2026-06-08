# =============================================================================
# ch2_010_group_mun.R  ·  Chapter 2 — Municipality aggregation
# Iteratively merge low-population municipalities with neighbours until every
# unit exceeds a population threshold (50,000), reducing 2,457 municipalities to
# ~519 analytical units. Uses the helper R/municipality_grouping.R.
#
# Reads : input-data-raw/geometries/gadm41_MEX_2.json,
#         input-data-processed/{population, geo_info}.RDS (ch1)
# Writes: input-data-processed/{aggregated_muni_50000, grouped_municipality_50000,
#         aggregated_muni_30000, grouped_municipality_30000, mex_muni}.RDS
# Run after: ch1_040 (population, geo_info); feeds ch1_060 and ch3+
# =============================================================================

library(sf)
library(spdep)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(viridis)
source("R/municipality_grouping.R")

# Figures -> output/ch2/ch2_010_<name>.pdf
fig_dir <- "output/ch2"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Replace with your actual GeoJSON file path
mex_muni <- st_read("input-data-raw/geometries/gadm41_MEX_2.json")

# Make sure the municipality ID column is character
names(mex_muni) <- tolower(names(mex_muni))  # Optional cleanup

mex_muni <- mex_muni |> 
  select(name_1, name_2, geometry) |>
  rename(
    state_name = name_1, 
    mun_name = name_2
  )

population <- readRDS("input-data-processed/population.RDS")
geo_info <- readRDS("input-data-processed/geo_info.RDS")
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
p_map_population <- ggplot(data = mex_muni) +
  geom_sf(aes(fill = log(total_pop))) +
  scale_fill_viridis_c(option = "viridis", name = "Population") +
  theme_minimal() +
  labs(title = "Map Colored by Population")
ggsave(file.path(fig_dir, "ch2_010_map_population.pdf"), p_map_population, width = 8, height = 8)

saveRDS(mex_muni, file = "input-data-processed/mex_muni.RDS")

## --------------------------- GROUPING -----------------------------
#aggregated_muni <- group_small_municipalities(mex_muni, threshold = 100000)
aggregated <- group_until_threshold(mex_muni, threshold = 30000, max_iter = 50)

aggregated_muni_30000 <- aggregated$aggregated
saveRDS(aggregated_muni_30000, file = "input-data-processed/aggregated_muni_30000.RDS")

grouped_mun <- aggregated$muni_ref_with_groups
saveRDS(grouped_mun, file = "input-data-processed/grouped_municipality_30000.RDS")

#--------------------------------------------
aggregated_50000 <- group_until_threshold(mex_muni, threshold = 50000, max_iter = 50)
aggregated_muni_50000 <- aggregated_50000$aggregated
saveRDS(aggregated_muni_50000, file = "input-data-processed/aggregated_muni_50000.RDS")

grouped_mun_50000 <- aggregated_50000$muni_ref_with_groups
saveRDS(grouped_mun_50000, file = "input-data-processed/grouped_municipality_50000.RDS")

sum(aggregated_muni_30000$total_pop < 30000)

p_map_grouped_30000 <- ggplot(data = aggregated_muni_30000) +
  geom_sf(aes(fill = log(total_pop))) +
  scale_fill_viridis_c(option = "viridis", name = "Log Population") +
  theme_minimal() +
  labs(
    title = "Grouped Municipalities by Population",
    subtitle = "Each group meets the minimum threshold of 15,000 people",
    fill = "Log(Pop)"
  )
ggsave(file.path(fig_dir, "ch2_010_map_grouped_30000.pdf"), p_map_grouped_30000, width = 8, height = 8)


library(viridis)  # for scale_fill_viridis_d
aggregated_muni_50000 <- readRDS(file = "input-data-processed/aggregated_muni_50000.RDS")
grouped_mun_50000 <- readRDS(file = "input-data-processed/grouped_municipality_50000.RDS")


p_map_groups_50000 <- ggplot() +
  geom_sf(data = grouped_mun_50000, aes(fill = as.factor(group_id)), color = "white", size = 0.1) +
  #geom_sf_text(data = grouped_mun, aes(label = mun), size = 1.8, color = "black") +
  scale_fill_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(
    title = "Original Municipalities Colored by Grouped Municipality",
    subtitle = "Each color = a group (group_id); Labels = original municipality ID (mun)",
    fill = "Group ID"
  ) +
  theme(legend.position = "none")
ggsave(file.path(fig_dir, "ch2_010_map_groups_50000.pdf"), p_map_groups_50000, width = 8, height = 8)

grouped_mun_50000 <- sf::st_make_valid(grouped_mun_50000)
aggregated_muni_50000 <- sf::st_make_valid(aggregated_muni_50000)


p_map_group_boundaries <- ggplot() +
  geom_sf(data = grouped_mun_50000, fill = NA, color = "grey70", size = 0.1) +
  geom_sf(data = aggregated_muni_50000, fill = NA, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Group Boundaries Overlaid on Original Municipalities")
ggsave(file.path(fig_dir, "ch2_010_map_group_boundaries.pdf"), p_map_group_boundaries, width = 8, height = 8)


#------------ RELATIONSHIP WITH MPI ---------------
