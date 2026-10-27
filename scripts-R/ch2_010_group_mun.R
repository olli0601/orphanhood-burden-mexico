# =============================================================================
# ch2_010_group_mun.R  ·  Chapter 2 — Municipality aggregation
# Merge low-population municipalities with contiguous neighbours until every
# unit reaches the threshold (30,000 and 50,000; 50,000 is the analysis default),
# using 2020 total population. The unit count is printed at runtime.
# Thesis §2.5's 519 units are not reproducible (internally inconsistent; see
# docs/ch2_grouping_diagnostics.md); ch2_020 sweeps the threshold.
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
library(gridExtra)
source("R/municipality_grouping.R")
source("R/load_population.R")

GROUPING_POP_YEAR <- 2020   # thesis §2.2
fig_dir <- "output/ch2"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# GADM v4.1 municipalities -> CONAPO/INEGI names, matched on names with spaces
# and accents removed. GADM truncates long names; these are the mismatches.
gadm_state_names <- c("Coahuila" = "CoahuiladeZaragoza", "DistritoFederal" = "CiudaddeMéxico",
                      "Michoacán" = "MichoacándeOcampo", "Veracruz" = "VeracruzdeIgnaciodelaLlave")
gadm_mun_names <- c(
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
  "NanchitaldeLazaroCardenasdel" = "NanchitaldeLazaroCardenasdelRio")
squash <- function(x) stringi::stri_trans_general(str_remove_all(x, "\\s+"), "Latin-ASCII")

mex_muni <- st_read("input-data-raw/geometries/gadm41_MEX_2.json")
names(mex_muni) <- tolower(names(mex_muni))
mex_muni <- mex_muni |>
  select(state_name = name_1, mun_name = name_2, geometry) |>
  mutate(mun_name = squash(mun_name),
         state_name = recode(state_name, !!!gadm_state_names),
         mun_name = recode(mun_name, !!!gadm_mun_names))

geo_info <- readRDS("input-data-processed/geo_info_mun.RDS") |>
  mutate(mun_name = squash(mun_name), state_name = str_remove_all(state_name, "\\s+"))

# "Zacualpan" exists in Morelos (as Zacualpan de Amilpas) and in Mexico state.
mex_muni <- mex_muni |>
  mutate(mun_name = case_when(
    state_name == "Morelos" & mun_name == "ZacualpandeAmilpas" ~ "ZacualpandeAmilpas",
    mun_name == "ZacualpandeAmilpas" ~ "Zacualpan",
    TRUE ~ mun_name)) |>
  left_join(geo_info |> select(mun_name, mun, state_name), by = c("mun_name", "state_name"))

# Total population, all ages: the threshold counts inhabitants.
pop_grouping <- load_population(keep_child = TRUE, keep_old = TRUE) |>
  filter(year == GROUPING_POP_YEAR) |>
  group_by(mun) |>
  summarise(total_pop = sum(population, na.rm = TRUE), .groups = "drop")

# A truncated age range would leave the national total well short of ~127.8M.
stopifnot(nrow(pop_grouping) > 2400, sum(pop_grouping$total_pop) > 1.2e8)
message(sprintf("ch2_010: %d municipalities, %s inhabitants (%d, all ages)",
                nrow(pop_grouping), format(sum(pop_grouping$total_pop), big.mark = ","),
                GROUPING_POP_YEAR))

mex_muni <- left_join(mex_muni, pop_grouping, by = "mun")
message(sprintf("ch2_010: %d of %d polygons matched a population",
                sum(!is.na(mex_muni$total_pop)), nrow(mex_muni)))

p_map_population <- ggplot(data = mex_muni) +
  geom_sf(aes(fill = log(total_pop))) +
  scale_fill_viridis_c(option = "viridis", name = "Population") +
  theme_minimal() +
  labs(title = "Map Colored by Population")
ggsave(file.path(fig_dir, "ch2_010_map_population.pdf"), p_map_population, width = 8, height = 8)

saveRDS(mex_muni, file = "input-data-processed/mex_muni.RDS")

# --- Grouping (contiguity graph built once, shared by both thresholds) --------
adj <- build_contiguity(mex_muni)
for (th in c(30000, 50000)) {
  g <- group_until_threshold(mex_muni, threshold = th, nb = adj)
  saveRDS(g$aggregated, file = sprintf("input-data-processed/aggregated_muni_%d.RDS", th))
  saveRDS(g$muni_ref_with_groups, file = sprintf("input-data-processed/grouped_municipality_%d.RDS", th))
}
aggregated_muni_30000 <- readRDS("input-data-processed/aggregated_muni_30000.RDS")
aggregated_muni_50000 <- readRDS("input-data-processed/aggregated_muni_50000.RDS")
grouped_mun_50000     <- readRDS("input-data-processed/grouped_municipality_50000.RDS")
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

p_map_groups_50000 <- ggplot() +
  geom_sf(data = grouped_mun_50000, aes(fill = as.factor(group_id)), color = "white", size = 0.1) +
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

# --- Before/after map: each unit shaded by how many municipalities it merges ---
merge_counts <- grouped_mun_50000 |>
  sf::st_drop_geometry() |>
  dplyr::count(group_id, name = "n_muns")

units_map <- aggregated_muni_50000 |>
  dplyr::left_join(merge_counts, by = "group_id") |>
  dplyr::mutate(n_band = factor(dplyr::case_when(n_muns == 1 ~ "1 (unchanged)",
                                                 n_muns == 2 ~ "2",
                                                 n_muns == 3 ~ "3",
                                                 n_muns == 4 ~ "4",
                                                 TRUE        ~ "5+"),
                                levels = c("1 (unchanged)", "2", "3", "4", "5+")))
stopifnot(!anyNA(units_map$n_muns))

# One hue, light-to-dark; the lightest step is not near-white because
# singletons are about half the units.
seq_blues <- c("1 (unchanged)" = "#dbe6f2", "2" = "#a8c4de", "3" = "#6f9cc6",
               "4" = "#3d73a8", "5+" = "#1b4a75")

# Bottom legends on both panels keep the two map areas the same width.
map_theme <- theme_minimal(base_size = 11) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"), legend.position = "bottom",
        plot.margin = margin(4, 4, 4, 4))

p_before <- ggplot(mex_muni) +
  geom_sf(aes(fill = "Municipality"), colour = "grey55", linewidth = 0.06) +
  scale_fill_manual(values = c(Municipality = "grey92"), name = NULL) +
  map_theme +
  labs(title = sprintf("Before: %s municipalities", format(nrow(mex_muni), big.mark = ",")),
       subtitle = "INEGI municipalities as published")

p_after <- ggplot(units_map) +
  geom_sf(aes(fill = n_band), colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = seq_blues, name = "Municipalities per unit", drop = FALSE) +
  guides(fill = guide_legend(nrow = 1, title.position = "top")) +
  map_theme +
  labs(title = sprintf("After: %s analytical units", format(nrow(units_map), big.mark = ",")),
       subtitle = sprintf("%s merged, %s already above 50,000",
                          format(sum(merge_counts$n_muns > 1), big.mark = ","),
                          format(sum(merge_counts$n_muns == 1), big.mark = ",")))

png(file.path(fig_dir, "ch2_010_map_units_50000.png"),
    width = 12, height = 5.5, units = "in", res = 150)
grid.arrange(p_before, p_after, ncol = 2)
invisible(dev.off())
message(sprintf("ch2_010: wrote ch2_010_map_units_50000.png (%d units, max %d muns/unit)",
                nrow(units_map), max(merge_counts$n_muns)))
