# Common setup for the chapter 2-3 scripts: core packages and the shared helpers.
# =============================================================================
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
for (f in c("load_population", "load_year_panels", "grouped_mun", "marginalization",
            "rates", "delay", "plots")) {
  source(file.path("R", paste0(f, ".R")))
}
rm(f)
