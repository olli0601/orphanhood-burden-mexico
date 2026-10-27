# Auto-downloaded raw inputs that later chapters require (ch1_010 fetches them,
# ch1_020 verifies them). Paths relative to the repository root.
# =============================================================================
REQUIRED_RAW_FILES <- c(
  "CONAPO quinquennial population" =
    "input-data-raw/population/00_Republica_mexicana/1_Grupo_Quinq_00_RM.xlsx",
  "IMM 2020"         = "input-data-raw/marginalization/IMM_2020.xls",
  "IMM 2015"         = "input-data-raw/marginalization/IMM_DP2_2015.xlsx",
  "IMM 2010"         = "input-data-raw/marginalization/IMM_DP2_2010.xlsx",
  "IMM 1990-2015"    = "input-data-raw/marginalization/Base_Indice_de_marginacion_municipal_90-15.csv",
  "census ITER 2020" = "input-data-raw/census/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv",
  "census ITER 2010" = "input-data-raw/census/iter_00_cpv2010/conjunto_de_datos/iter_00_cpv2010.csv",
  "GADM ADM2 geometries" = "input-data-raw/geometries/gadm41_MEX_2.json")

# A file this small is an HTML error page from a dead link, not data.
MIN_RAW_BYTES <- 10000
