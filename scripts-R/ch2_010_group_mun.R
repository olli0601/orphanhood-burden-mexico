# =============================================================================
# ch2_010_group_mun.R  ·  Chapter 2 — Municipality aggregation
# Iteratively merge low-population municipalities with neighbours until every
# unit exceeds a population threshold (50,000). Uses R/municipality_grouping.R.
# The unit count is printed at runtime — read it from the run, not from prose.
#
# It does NOT reproduce the 519 units of thesis Section 2.5, and it is not meant
# to. Those figures are internally inconsistent: 353 municipalities exceed 66,300
# inhabitants on their own and an above-threshold unit is never a merge
# candidate, so a post-merge median of 66,300 requires at least 706 units. See
# docs/ch2_grouping_diagnostics.md.
#
# The threshold itself is not settled either. Thesis Section 2.4 states 50,000
# was chosen to minimise the coefficient of variation of age-standardised death
# rates over a 10k-100k sweep; that sweep was never implemented. It now is, in
# ch2_020_grouping_diagnostics.R. Revisit the thresholds below once it is read.
#
# Reads : input-data-raw/geometries/gadm41_MEX_2.json,
#         input-data-raw/population/.../1_Grupo_Quinq_00_RM.xlsx (via load_population),
#         input-data-processed/geo_info_mun.RDS (ch1_015)
# Writes: input-data-processed/{aggregated_muni_50000, grouped_municipality_50000,
#         aggregated_muni_30000, grouped_municipality_30000, mex_muni}.RDS
# Run after: ch1_015 (geo_info); feeds ch2_040, ch2_060 and ch3+
# =============================================================================

library(sf)
library(spdep)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(viridis)
source("R/municipality_grouping.R")
source("R/load_population.R")

# Population vintage the 50,000-inhabitant threshold is evaluated against. The
# thesis (Section 2.2) states 2020, so the unit count here is on the same footing
# as the one it reports.
#
# Read straight from the CONAPO workbook rather than input-data-processed/population.RDS:
# that file is rewritten by ch1_015 -> ch2_040 -> ch2_060 with different age coverage,
# and this script used to read whichever version happened to be on disk. It picked up a
# table missing ages 0-09 and 80+, i.e. 107.5M instead of 127.8M nationally, which pushed
# hundreds of municipalities below a threshold they clear, and produced 477 units.
GROUPING_POP_YEAR <- 2020

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

geo_info <- readRDS("input-data-processed/geo_info_mun.RDS")
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

# Total population, all ages: the threshold counts inhabitants.
pop_grouping <- load_population(keep_child = TRUE, keep_old = TRUE) %>%
  filter(year == GROUPING_POP_YEAR) %>%
  group_by(mun) %>%
  summarise(total_pop = sum(population, na.rm = TRUE), .groups = "drop")

# Assert on the payload, not on the file existing: a truncated age range is exactly
# the defect this replaced, and it is invisible unless the total is checked.
stopifnot(nrow(pop_grouping) > 2400, sum(pop_grouping$total_pop) > 1.2e8)
message(sprintf("ch2_010: %d municipalities, %s inhabitants (%d, all ages)",
                nrow(pop_grouping), format(sum(pop_grouping$total_pop), big.mark = ","),
                GROUPING_POP_YEAR))

mex_muni <- left_join(mex_muni, pop_grouping, by = "mun")
message(sprintf("ch2_010: %d of %d polygons matched a population",
                sum(!is.na(mex_muni$total_pop)), nrow(mex_muni)))

# Plot using ggplot2
p_map_population <- ggplot(data = mex_muni) +
  geom_sf(aes(fill = log(total_pop))) +
  scale_fill_viridis_c(option = "viridis", name = "Population") +
  theme_minimal() +
  labs(title = "Map Colored by Population")
ggsave(file.path(fig_dir, "ch2_010_map_population.pdf"), p_map_population, width = 8, height = 8)

saveRDS(mex_muni, file = "input-data-processed/mex_muni.RDS")

## --------------------------- GROUPING -----------------------------
# Build the contiguity graph once and share it: it is the only expensive sf step
# here, and both thresholds run on the same polygons.
adj <- build_contiguity(mex_muni)

aggregated <- group_until_threshold(mex_muni, threshold = 30000, nb = adj)

aggregated_muni_30000 <- aggregated$aggregated
saveRDS(aggregated_muni_30000, file = "input-data-processed/aggregated_muni_30000.RDS")

grouped_mun <- aggregated$muni_ref_with_groups
saveRDS(grouped_mun, file = "input-data-processed/grouped_municipality_30000.RDS")

#--------------------------------------------
aggregated_50000 <- group_until_threshold(mex_muni, threshold = 50000, nb = adj)
aggregated_muni_50000 <- aggregated_50000$aggregated
saveRDS(aggregated_muni_50000, file = "input-data-processed/aggregated_muni_50000.RDS")

grouped_mun_50000 <- aggregated_50000$muni_ref_with_groups
saveRDS(grouped_mun_50000, file = "input-data-processed/grouped_municipality_50000.RDS")

# This was a bare expression whose value Rscript discards — a check that checked
# nothing. group_until_threshold() now asserts it internally; keep it here too,
# because it is the one property the whole script exists to deliver.
stopifnot(sum(aggregated_muni_30000$total_pop < 30000) == 0,
          sum(aggregated_muni_50000$total_pop < 50000) == 0)

p_map_grouped_30000 <- ggplot(data = aggregated_muni_30000) +
  geom_sf(aes(fill = log(total_pop))) +
  scale_fill_viridis_c(option = "viridis", name = "Log Population") +
  theme_minimal() +
  labs(
    title = "Grouped Municipalities by Population",
    subtitle = "Each group meets the minimum threshold of 30,000 people",
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
