# =============================================================================
# ch2_050_clean_fert.R  ·  Chapter 2 — Fertility at municipality level
# Registered births by municipality x parent sex x age band x year (unknown
# parental age redistributed), joined to population and IMM 2020; occurrence-year
# and standardised-rate figures. Municipal counterpart of ch3_020.
#
# Reads : input-data-processed/{fertility datasets/*.RDS, geo_info_mun.RDS},
#         input-data-raw/population/.../1_Grupo_Quinq_00_RM.xlsx, input-data-raw/marginalization/IMM_2020.xls
# Writes: input-data-processed/fert.RDS
#         output/ch2/ch2_050_*.pdf
# Run after: ch1_015
# =============================================================================

source("R/setup.R")
fig_dir <- "output/ch2"; dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

fert_panel <- load_birth_panels()

# --- Occurrence year within each registration year (last 20 years) -----------
# Sum births, not rows: panel cells range from 1 birth to thousands.
data_summary_pie <- fert_panel |>
  filter(year >= as.numeric(year_reg) - 20) |>
  group_by(year_reg, year) |>
  summarise(count = sum(births), .groups = "drop") |>
  mutate(perc = round(count / sum(count) * 100, 2), .by = year_reg) |>
  arrange(desc(perc)) |>
  mutate(rank = row_number(), .by = year_reg)

p_pie <- ggplot(data_summary_pie, aes(x = "", y = perc, fill = factor(year))) +
  geom_bar(stat = "identity", width = 0.5, color = "white", size = 0.1) +
  geom_text(data = subset(data_summary_pie, rank <= 5), aes(label = paste0(round(perc, 1), "%")),
            position = position_stack(vjust = 0.5), size = 2.5) +
  coord_polar(theta = "y") +
  facet_wrap(~year_reg) +
  theme_void() +
  labs(title = "Birth Year Distribution per Registration Year") +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  scale_fill_manual(values = rainbow(length(unique(data_summary_pie$year))))
save_fig(p_pie, "ch2_050_pie_birth_year_dist.pdf", 12, 8)

# --- Rates panel --------------------------------------------------------------------
births <- fert_panel |>
  group_by(year, sex, age, mun) |>
  summarise(births = sum(births), .groups = "drop") |>
  filter(year >= 1985, year <= 2024)

# Population restricted, per parent sex, to the age bands present in the births.
ages_of <- function(s) sort(unique(births$age[births$sex == s]))
population <- load_population(keep_child = TRUE, keep_old = TRUE)
population <- bind_rows(filter(population, sex == "female", age %in% ages_of("female")),
                        filter(population, sex == "male", age %in% ages_of("male"))) |>
  mutate(across(c(sex, age, mun), ~ factor(as.character(.x))))

fert <- full_join(births, population, by = c("mun", "year", "sex", "age"))
fert$births[is.na(fert$births)] <- 0

index <- load_imm("input-data-raw/marginalization/IMM_2020.xls", 2020, skip = 5)
im.tmp <- index |>
  select(mun, IM, IMN) |>
  mutate(mun = as.factor(mun), IM = as.numeric(IM), IMN = as.numeric(IMN))
fert <- fert |>
  full_join(im.tmp, by = c("mun")) |>
  select(mun, year, sex, age, births, population, IM, IMN)
fert$fert_rate <- fert$births / fert$population

n_before <- nrow(fert); b_before <- sum(fert$births, na.rm = TRUE)
fert <- fert |> drop_na()
message(sprintf("ch2_050: drop_na() removed %s rows and %s births (%.2f%% of all births).",
                format(n_before - nrow(fert), big.mark = ","),
                format(round(b_before - sum(fert$births)), big.mark = ","),
                100 * (b_before - sum(fert$births)) / b_before))
saveRDS(fert, file = "input-data-processed/fert.RDS")

# --- Standardised fertility rate vs IMN ------------------------------------------
# Mother rows only: every birth sits on both a mother's and a father's row, and
# with female weights summing to one the rate is births per woman.
geo_info <- readRDS("input-data-processed/geo_info_mun.RDS")
std_all_fert <- compute_std_fert_rate(filter(fert, sex == "female"), unit_col = "mun") |>
  select(mun, year, std_fert_rate) |>
  left_join(y = geo_info[, c("mun", "capital")], by = "mun") |>
  left_join(y = index, by = "mun") |>
  mutate(capital = factor(capital)) |>
  rename(mpi = IMN) |>
  mutate(mpi = as.numeric(mpi))

save_fig(plot_std_rate(data = std_all_fert |> filter(year == 2020) |> select(-year),
                       tt = "Raw data (fertility)", y_lim = c(0, 0.15)),
         "ch2_050_std_fert_rate_2020.pdf")
save_fig(plot_std_rate(data = std_all_fert |> filter(year %in% 2005:2023),
                       tt = "Raw data (fertility) - Years 2005 to 2023") + facet_wrap(~ year),
         "ch2_050_std_fert_rate_all_years.pdf", 12, 8)

# --- National age profile, 2023 ----------------------------------------------------
fert_national <- fert |>
  filter(!(sex == "female" & age %in% c("65-69", "70-74", "75-79"))) |>
  national_rates("births", "births", "fert_rate", na_rm = FALSE)
std_age_sex <- compute_national_std_rate_age_gender(fert_national, std_ref_population(population)) |>
  filter(year == 2023)
save_fig(plot_age_profile(std_age_sex, "Fertility", by = "sex"), "ch2_050_std_fert_rate_age.pdf")
save_fig(plot_age_profile(arrange(std_age_sex, year, sex, age), "Fertility"), "ch2_050_std_fert_rate_age_sex.pdf")
