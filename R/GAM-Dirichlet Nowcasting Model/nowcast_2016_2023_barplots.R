################################################################################
# NOWCASTING 2016-2023 WITH STACKED BAR PLOTS
# Complete data (1990-2015) vs Nowcasted data (2016-2023)
################################################################################

library(dplyr)
library(tidyr)
library(mgcv)
library(ggplot2)
library(purrr)
library(patchwork)

# Set working directory - Keep in current folder
cat("=== NOWCASTING 2016-2023 WITH BARPLOT VISUALIZATION ===\n")
cat("Creating stacked barplots: Complete data (1990-2015) + Nowcast (2016-2023)\n")
cat("Working directory:", getwd(), "\n\n")

################################################################################
# LOAD DATA AND MODELS
################################################################################

cat("=== LOADING DATA AND MODELS ===\n")

# Load trained models
municipality_models <- readRDS("../municipality_models_proper_validation.RDS")
successful_models <- municipality_models[map_lgl(municipality_models, ~.x$success)]

# Load raw data
births <- readRDS("../../datasets/births_new_mun.RDS") %>%
  rename(event_year = year, reg_year = year_reg, municipality = group_id, 
         age_group = age, n = births) %>%
  mutate(reg_year = as.numeric(reg_year), delay = reg_year - event_year)

pop_tbl <- readRDS("../../datasets/population_new_mun.RDS") %>%
  rename(event_year = year, municipality = group_id, age_group = age) %>%
  filter(!age_group %in% c("00-04", "05-09", "10-14"))

cat("Loaded", length(successful_models), "successful municipality models\n")
cat("Loaded births data:", nrow(births), "rows\n")
cat("Loaded population data:", nrow(pop_tbl), "rows\n")

################################################################################
# PREPARE COMPLETE DATA (1990-2015) - OBSERVED BIRTHS
################################################################################

cat("\n=== PREPARING COMPLETE DATA (1990-2015) ===\n")

# Calculate total registered births by year (complete data)
complete_births_yearly <- births %>%
  filter(event_year >= 1990 & event_year <= 2015) %>%
  filter(delay == 0) %>%  # Only complete registrations
  filter(
    (sex == "female" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) |
    (sex == "male" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"))
  ) %>%
  group_by(event_year, sex) %>%
  summarise(total_births_complete = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(data_type = "complete")

cat("Complete births by year calculated (1990-2015)\n")

################################################################################
# PREPARE NATIONAL FERTILITY CURVES FOR NOWCASTING
################################################################################

cat("\n=== PREPARING NATIONAL FERTILITY CURVES ===\n")

# Calculate national fertility curves from training period (1990-2005)
train_births <- births %>% 
  filter(event_year >= 1990 & event_year <= 2005) %>%
  filter(delay == 0) %>%
  filter(
    (sex == "female" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) |
    (sex == "male" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"))
  )

train_pop <- pop_tbl %>%
  filter(event_year >= 1990 & event_year <= 2005) %>%
  filter(
    (sex == "female" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) |
    (sex == "male" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"))
  )

# National fertility curves
births_national_train <- train_births %>%
  group_by(event_year, age_group, sex) %>%
  summarise(births = sum(n, na.rm = TRUE), .groups = "drop")

pop_national_train <- train_pop %>%
  group_by(event_year, age_group, sex) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

fert_national_train <- births_national_train %>%
  left_join(pop_national_train, by = c("event_year", "age_group", "sex")) %>%
  filter(!is.na(population), population > 0, !is.na(births)) %>%
  mutate(fert_rate = births / population)

# Extend fertility curves to 2016-2023 (use mean from training period)
fert_national_extended <- fert_national_train %>%
  group_by(age_group, sex) %>%
  summarise(fert_rate = mean(fert_rate, na.rm = TRUE), .groups = "drop") %>%
  crossing(event_year = 2016:2023)

cat("National fertility curves extended to 2016-2023\n")

################################################################################
# NOWCAST 2016-2023
################################################################################

cat("\n=== PERFORMING NOWCASTING 2016-2023 ===\n")

# Get population data for nowcasting period
nowcast_pop <- pop_tbl %>%
  filter(event_year >= 2016 & event_year <= 2023) %>%
  filter(
    (sex == "female" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) |
    (sex == "male" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"))
  )

# Function to make nowcast predictions for a municipality
nowcast_municipality <- function(mun_result, nowcast_data, fert_nat) {
  
  if(!mun_result$success) return(NULL)
  
  mun_id <- mun_result$municipality
  
  # Prepare nowcast data for this municipality
  mun_nowcast_data <- nowcast_data %>%
    filter(municipality == mun_id) %>%
    left_join(
      fert_nat %>% select(event_year, age_group, sex, fert_rate),
      by = c("event_year", "age_group", "sex")
    ) %>%
    filter(!is.na(population), !is.na(fert_rate), population > 0, fert_rate > 0) %>%
    mutate(
      expected = population * fert_rate,
      log_expected = log(expected),
      age = factor(age_group, levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")),
      parent_sex = factor(sex, levels = c("female", "male"))
    )
  
  if(nrow(mun_nowcast_data) == 0) return(NULL)
  
  # Make predictions
  tryCatch({
    predictions <- predict(mun_result$model, newdata = mun_nowcast_data, type = "response", se.fit = TRUE)
    
    mun_nowcast_data$predicted <- as.numeric(predictions$fit)
    mun_nowcast_data$se <- as.numeric(predictions$se.fit)
    mun_nowcast_data$lower_95 <- pmax(0, mun_nowcast_data$predicted - 1.96 * mun_nowcast_data$se)
    mun_nowcast_data$upper_95 <- mun_nowcast_data$predicted + 1.96 * mun_nowcast_data$se
    
    return(mun_nowcast_data)
    
  }, error = function(e) {
    return(NULL)
  })
}

# Make nowcast predictions for all municipalities
cat("Making nowcast predictions for", length(successful_models), "municipalities...\n")

nowcast_predictions <- map_dfr(successful_models, function(mun_result) {
  nowcast_municipality(mun_result, nowcast_pop, fert_national_extended)
})

if(nrow(nowcast_predictions) > 0) {
  cat("Nowcast predictions generated:", nrow(nowcast_predictions), "rows\n")
  
  # Aggregate by year and sex
  # Calcolo media aggregata e se aggregato (varianza della somma = somma delle varianze)
  nowcast_births_yearly <- nowcast_predictions %>%
    group_by(event_year, sex) %>%
    summarise(
      mu = sum(predicted, na.rm = TRUE),
      se = sqrt(sum(se^2, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      lower_95 = pmax(0, mu - 1.96 * se),
      upper_95 = mu + 1.96 * se,
      data_type = "nowcast"
    )
  
  cat("Nowcast aggregated by year (con CI)\n")
} else {
  stop("No nowcast predictions generated!")
}

################################################################################
# PREPARE REGISTERED BIRTHS FOR 2016-2023 (INCOMPLETE)
################################################################################

cat("\n=== PREPARING REGISTERED BIRTHS 2016-2023 ===\n")

# Get registered births for 2016-2023 (these are incomplete due to delays)
registered_births_2016_2023 <- births %>%
  filter(event_year >= 2016 & event_year <= 2023) %>%
  filter(delay == 0) %>%  # Only immediate registrations (incomplete)
  filter(
    (sex == "female" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) |
    (sex == "male" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"))
  ) %>%
  group_by(event_year, sex) %>%
  summarise(total_births_registered = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(data_type = "registered_incomplete")

cat("Registered births 2016-2023 calculated (incomplete data)\n")

################################################################################
# COMBINE ALL DATA FOR PLOTTING
################################################################################

cat("\n=== COMBINING DATA FOR PLOTTING ===\n")

# Combine all data
plot_data_complete <- complete_births_yearly %>%
  select(event_year, sex, total_births = total_births_complete, data_type)


# Usa mu come total_births per la parte nowcast
plot_data_nowcast <- nowcast_births_yearly %>%
  select(event_year, sex, total_births = mu, data_type)

plot_data_registered <- registered_births_2016_2023 %>%
  select(event_year, sex, total_births = total_births_registered, data_type)


# Create stacked data for 2016-2023
# Usa mu come stima nowcast
stacked_data_2016_2023 <- registered_births_2016_2023 %>%
  left_join(nowcast_births_yearly, by = c("event_year", "sex")) %>%
  mutate(
    births_registered = total_births_registered,
    births_nowcast_additional = pmax(0, mu - total_births_registered),
    total_estimated = births_registered + births_nowcast_additional
  ) %>%
  select(event_year, sex, births_registered, births_nowcast_additional, total_estimated)


print(stacked_data_2016_2023)
cat("Data combined for plotting\n")

################################################################################
# CREATE BARPLOTS
################################################################################

cat("\n=== CREATING BARPLOTS ===\n")

# Set theme
theme_set(theme_bw() + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ))

sex_colors <- c("female" = "#E91E63", "male" = "#2196F3")

# Plot 1: Complete time series (1990-2015 complete + 2016-2023 stacked)

plot_data_combined <- bind_rows(
  complete_births_yearly %>%
    select(event_year, sex, births = total_births_complete) %>%
    mutate(type = "Complete"),
  registered_births_2016_2023 %>%
    select(event_year, sex, births = total_births_registered) %>%
    mutate(type = "Registered"),
  stacked_data_2016_2023 %>%
    select(event_year, sex, births = births_nowcast_additional) %>%
    mutate(type = "Nowcast Additional")
) %>%
  mutate(
    period = ifelse(event_year <= 2015, "Complete (1990-2015)", "Nowcast (2016-2023)"),
    type = factor(type, levels = c("Complete", "Nowcast Additional", "Registered")),
    births_thousands = births / 1000
  )

print(plot_data_combined)

# Prepara dati CI per ribbon/linea
nowcast_ci_by_sex <- nowcast_births_yearly %>%
  mutate(
    mu_thousands = mu / 1000,
    lower_thousands = lower_95 / 1000,
    upper_thousands = upper_95 / 1000
  )

nowcast_ci_total <- nowcast_births_yearly %>%
  group_by(event_year) %>%
  summarise(
    mu_thousands = sum(mu, na.rm = TRUE) / 1000,
    lower_thousands = sum(lower_95, na.rm = TRUE) / 1000,
    upper_thousands = sum(upper_95, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

# PLOT BY SEX

p_female <- plot_data_combined %>%
  filter(sex == "female") %>%
  ggplot(aes(x = event_year, y = births_thousands, fill = type)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("Complete" = "#E91E63", "Registered" = "#F8BBD9", "Nowcast Additional" = "#AD1457"),
    labels = c("Complete Births", "Registered Births (Base)", "Nowcast Estimates (Additional)")
  ) +
  geom_ribbon(
    data = nowcast_ci_by_sex %>% dplyr::filter(sex == "female"),
    aes(x = event_year, ymin = lower_thousands, ymax = upper_thousands),
    inherit.aes = FALSE, fill = "#81C784", alpha = 0.25
  ) +
  geom_line(
    data = nowcast_ci_by_sex %>% dplyr::filter(sex == "female"),
    aes(x = event_year, y = mu_thousands),
    inherit.aes = FALSE, color = "#2E7D32", linewidth = 1.3
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, 3)) +
  labs(
    title = "Female Births: Complete Data (1990-2015) vs Nowcast (2016-2023)",
    subtitle = "Complete bars for 1990-2015, stacked bars (registered + nowcast on top) for 2016-2023",
    x = "Year",
    y = "Births (thousands)",
    fill = "Type"
  ) +
  geom_vline(xintercept = 2015.5, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = 2002, y = max(plot_data_combined$births_thousands[plot_data_combined$sex == "female"], na.rm = TRUE) * 0.9, 
           label = "Complete Data", color = "darkred", size = 3) +
  annotate("text", x = 2019, y = max(plot_data_combined$births_thousands[plot_data_combined$sex == "female"], na.rm = TRUE) * 0.9, 
           label = "Nowcast", color = "darkred", size = 3) +
  theme(legend.position = "bottom")

p_male <- plot_data_combined %>%
  filter(sex == "male") %>%
  ggplot(aes(x = event_year, y = births_thousands, fill = type)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("Complete" = "#2196F3", "Registered" = "#BBDEFB", "Nowcast Additional" = "#1565C0"),
    labels = c("Complete Births", "Registered Births (Base)", "Nowcast Estimates (Additional)")
  ) +
  geom_ribbon(
    data = nowcast_ci_by_sex %>% dplyr::filter(sex == "male"),
    aes(x = event_year, ymin = lower_thousands, ymax = upper_thousands),
    inherit.aes = FALSE, fill = "#81C784", alpha = 0.25
  ) +
  geom_line(
    data = nowcast_ci_by_sex %>% dplyr::filter(sex == "male"),
    aes(x = event_year, y = mu_thousands),
    inherit.aes = FALSE, color = "#2E7D32", linewidth = 1.3
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, 3)) +
  labs(
    title = "Male Births: Complete Data (1990-2015) vs Nowcast (2016-2023)",
    subtitle = "Complete bars for 1990-2015, stacked bars (registered + nowcast on top) for 2016-2023",
    x = "Year", 
    y = "Births (thousands)",
    fill = "Type"
  ) +
  geom_vline(xintercept = 2015.5, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = 2002, y = max(plot_data_combined$births_thousands[plot_data_combined$sex == "male"], na.rm = TRUE) * 0.9, 
           label = "Complete Data", color = "darkred", size = 3) +
  annotate("text", x = 2019, y = max(plot_data_combined$births_thousands[plot_data_combined$sex == "male"], na.rm = TRUE) * 0.9, 
           label = "Nowcast", color = "darkred", size = 3) +
  theme(legend.position = "bottom")

# Combined plot
p_combined <- p_female / p_male

ggsave("nowcast_barplots_by_sex.png", p_combined, width = 14, height = 10, dpi = 300, bg = "white")

# PLOT TOTAL (BOTH SEXES)

plot_data_total <- plot_data_combined %>%
  group_by(event_year, type, period) %>%
  summarise(births_thousands = sum(births_thousands, na.rm = TRUE), .groups = "drop")

p_total <- plot_data_total %>%
  ggplot(aes(x = event_year, y = births_thousands, fill = type)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("Complete" = "#9C27B0", "Registered" = "#E1BEE7", "Nowcast Additional" = "#6A1B9A"),
    labels = c("Complete Births", "Registered Births (Base)", "Nowcast Estimates (Additional)")
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, 2)) +
  labs(
    title = "Total Births: Complete Data (1990-2015) vs Nowcast (2016-2023)",
    subtitle = "Complete bars for 1990-2015, stacked bars (registered base + nowcast on top) for 2016-2023",
    x = "Year",
    y = "Total Births (thousands)",
    fill = "Data Type",
    caption = "Red dashed line: transition from complete data to nowcast"
  ) +
  geom_vline(xintercept = 2015.5, linetype = "dashed", color = "red", alpha = 0.8, size = 1) +
  annotate("text", x = 2002, y = max(plot_data_total$births_thousands, na.rm = TRUE) * 0.9, 
           label = "COMPLETE DATA", color = "darkred", size = 4, fontface = "bold") +
  annotate("text", x = 2019, y = max(plot_data_total$births_thousands, na.rm = TRUE) * 0.9, 
           label = "NOWCAST", color = "darkred", size = 4, fontface = "bold") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p_total)


ggsave("nowcast_barplots_total.png", p_total, width = 16, height = 8, dpi = 300, bg = "white")

cat("Saved: nowcast_barplots_by_sex.png\n")
cat("Saved: nowcast_barplots_total.png\n")

p_total <- plot_data_total %>%
  ggplot(aes(x = event_year, y = births_thousands, fill = type)) +
  geom_col(position = "stack", alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "Complete" = "steelblue3",
      "Registered" = "steelblue3",         # dark gray
      "Nowcast Additional" = "#0D47A1"  # dark blue
    ),
    labels = c(
      "Complete data",
      "Nowcast Estimates (Additional)",
      "Registered Births (Base)"
    )
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, 2)) +
  scale_y_continuous(limits = c(0, 3500)) +
  labs(
    title = "Total Births: Complete Data (1990–2015) vs Nowcast (2016–2023)",
    subtitle = "Complete bars for 1990–2015, stacked bars (registered base + nowcast on top) for 2016–2023",
    x = "Year",
    y = "Total Births (thousands)",
    fill = "Data Type",
    caption = "Red dashed line: transition from complete data to nowcast"
  ) +
  geom_vline(xintercept = 2015.5, linetype = "dashed", color = "red", alpha = 0.8, size = 1) +
  annotate(
    "text",
    x = 2002,
    y = 3300,  # Higher to avoid bar overlap
    label = "COMPLETE DATA",
    color = "darkred",
    size = 4.5,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2019,
    y = 3300,  # Higher to avoid bar overlap
    label = "NOWCAST",
    color = "darkred",
    size = 4.5,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p_total)
################################################################################
# DETAILED ANNUAL PLOTS (2016-2023)
################################################################################

cat("\n=== CREATING DETAILED ANNUAL PLOTS (2016-2023) ===\n")

# Create individual plots for each year 2016-2023
annual_plots <- map(2016:2023, function(year) {
  
  year_data <- stacked_data_2016_2023 %>%
    filter(event_year == year) %>%
    pivot_longer(
      cols = c(births_registered, births_nowcast_additional),
      names_to = "component",
      values_to = "births"
    ) %>%
    mutate(
      # IMPORTANT: Order for stacking - Nowcast Additional FIRST (top), then Registered (bottom)
      component = factor(component, 
                        levels = c("births_nowcast_additional", "births_registered"),
                        labels = c("Nowcast (Top)", "Registered (Base)")),
      births_thousands = births / 1000
    )
  
  ggplot(year_data, aes(x = sex, y = births_thousands, fill = component)) +
    geom_col(position = "stack", alpha = 0.8, width = 0.6) +
    scale_fill_manual(
      values = c("Nowcast (Sopra)" = "#FF9800", "Registrate (Base)" = "#FFC107"),
      labels = c("Stima Nowcast (Sopra)", "Nascite Registrate (Base)")
    ) +
    scale_x_discrete(labels = c("female" = "Femmine", "male" = "Maschi")) +
    labs(
      title = paste("Anno", year),
      x = "Sesso",
      y = "Nascite (migliaia)",
      fill = "Componente"
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    ) +
    geom_text(aes(label = round(births_thousands, 1)), 
              position = position_stack(vjust = 0.5), 
              size = 3, color = "black")
})

# Arrange annual plots in grid
p_annual_grid <- wrap_plots(annual_plots, ncol = 4, nrow = 2)

ggsave("nowcast_annual_details_2016_2023.png", p_annual_grid, width = 16, height = 8, dpi = 300, bg = "white")

cat("Saved: nowcast_annual_details_2016_2023.png\n")

################################################################################
# SAVE RESULTS
################################################################################

cat("\n=== SAVING NOWCAST RESULTS ===\n")

saveRDS(nowcast_predictions, "nowcast_predictions_2016_2023.RDS")
saveRDS(nowcast_births_yearly, "nowcast_births_yearly.RDS")
saveRDS(stacked_data_2016_2023, "stacked_births_data_2016_2023.RDS")

# Summary statistics
nowcast_summary <- list(
  total_municipalities = length(successful_models),
  predictions_generated = nrow(nowcast_predictions),
  years_nowcast = 2016:2023,
  total_births_nowcast = sum(nowcast_births_yearly$total_births_nowcast, na.rm = TRUE),
  total_births_registered = sum(registered_births_2016_2023$total_births_registered, na.rm = TRUE),
  additional_births_estimated = sum(stacked_data_2016_2023$births_nowcast_additional, na.rm = TRUE)
)

saveRDS(nowcast_summary, "nowcast_summary_2016_2023.RDS")

cat("\n================================================================================\n")
cat("NOWCASTING 2016-2023 COMPLETED SUCCESSFULLY!\n")
cat("================================================================================\n")
cat("\nSUMMARY:\n")
cat("- Municipalities with predictions:", nowcast_summary$total_municipalities, "\n")
cat("- Total nowcast predictions:", nowcast_summary$predictions_generated, "\n")
cat("- Total births nowcast (2016-2023):", format(round(nowcast_summary$total_births_nowcast), big.mark = ","), "\n")
cat("- Total births registered (2016-2023):", format(round(nowcast_summary$total_births_registered), big.mark = ","), "\n")
cat("- Additional births estimated:", format(round(nowcast_summary$additional_births_estimated), big.mark = ","), "\n")
cat("- Percentage increase:", round(nowcast_summary$additional_births_estimated / nowcast_summary$total_births_registered * 100, 1), "%\n")

cat("\nFILES GENERATED:\n")
cat("✅ nowcast_barplots_by_sex.png - Barplots by sex (female/male)\n")
cat("✅ nowcast_barplots_total.png - Total barplots (both sexes)\n")
cat("✅ nowcast_annual_details_2016_2023.png - Detailed annual breakdowns\n")
cat("✅ nowcast_predictions_2016_2023.RDS - Raw predictions data\n")
cat("✅ nowcast_summary_2016_2023.RDS - Summary statistics\n")
cat("================================================================================\n")

library(dplyr)
setwd("/Users/elsafarinella/Desktop/Orphanhood Mexico/R code/GAM-Dirichlet Nowcasting Model ")
# Calcola il massimo delay osservabile per ogni anno evento
births <- readRDS("../../datasets/births_new_mun.RDS") %>%
  rename(event_year = year, reg_year = year_reg, municipality = group_id, 
         age_group = age, n = births) %>%
  mutate(reg_year = as.numeric(reg_year), delay = reg_year - event_year)

# Considera solo anni evento 2016-2023 e delay possibili
registered_cells <- births %>%
  filter(event_year >= 2016 & event_year <= 2023) %>%
  filter(delay >= 0, reg_year <= 2023) %>%
  mutate(max_delay = 2023 - event_year) %>%
  filter(delay <= max_delay) %>%
  group_by(municipality, event_year, age_group, sex) %>%
  summarise(registered_n = sum(n, na.rm = TRUE), .groups = "drop")

# Unisci alle previsioni
colnames(nowcast_cell_predictions_full)
nowcast_cell_predictions_full <- nowcast_cell_predictions %>%
  left_join(registered_cells, by = c("municipality", "event_year", "age_group", "sex")) %>%
  mutate(registered_n = ifelse(is.na(registered_n), 0, registered_n))

birth_data_all <- nowcast_cell_predictions_full |>
  rename(year = event_year, 
         group_id = municipality) |>
  mutate(tot_births = predicted + registered_n) |>
  select(-c(log_expected, age_group, parent_sex, expected, log_expected, parent_sex, predicted, se, lower_95, upper_95, mu, theta, model_family, registered_n)) |>
  relocate(group_id, year, sex, age, tot_births, population, fert_rate)

saveRDS(birth_data_all, "../datasets/birth_data_all.RDS")
birth_data_all <- readRDS("../datasets/birth_data_all.RDS")


#############################################################################
##-------------------------- FERTILITY LONG ---------------------------------
#############################################################################
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# --- 1) Parse age band bounds -------------------------------------------------
births5 <- birth_data_all |>
  mutate(
    age_start = as.integer(str_extract(age, "^\\d+")),
    age_end   = as.integer(str_extract(age, "\\d+$"))
  )

# --- 2) Graduate population to single-year ages (per group_id-year-sex) -------
graduate_pop <- function(df) {
  df <- arrange(df, age_start)
  ages_out <- seq(min(df$age_start), max(df$age_end))
  fit <- ungroup::pclm(
    x        = df$age_start,     # lower bounds of 5y bands
    y        = df$population,    # 5y population (sex-specific column in your data)
    nlast    = 0,                # last group is closed (e.g., 50–54)
    out.step = 1                 # want single-year ages
  )
  tibble(age = ages_out, population_single = as.numeric(fit$fitted))
}

pop_single <- births5 |>
  group_by(group_id, year, sex) |>
  group_modify(~ graduate_pop(.x)) |>
  ungroup()

# --- 3) Expand each 5y birth band to single years and weight by exposures -----
births_expanded <- births5 |>
  select(group_id, year, sex, age_start, age_end, tot_births) |>
  mutate(age = map2(age_start, age_end, seq)) |>
  unnest(age)

births_1yr <- births_expanded |>
  left_join(pop_single, by = c("group_id", "year", "sex", "age")) |>
  group_by(group_id, year, sex, age_start, age_end) |>
  mutate(
    w = population_single / sum(population_single, na.rm = TRUE),
    w = if_else(is.finite(w) & !is.na(w),
                w,
                1 / (age_end - age_start + 1))  # uniform fallback
  ) |>
  ungroup() |>
  mutate(
    births_single     = tot_births * w,
    fert_rate_single  = if_else(population_single > 0,
                                births_single / population_single,
                                NA_real_)
  ) |>
  select(group_id, year, sex, age, population_single, births_single, fert_rate_single) |>
  arrange(group_id, year, sex, age)

# Optional check: band totals preserved exactly
# births_1yr |>
#   left_join(births5, by = c("group_id","year","sex")) |>
#   filter(age >= age_start & age <= age_end) |>
#   group_by(group_id, year, sex, age_start, age_end) |>
#   summarise(orig = first(tot_births), split = sum(births_single), .groups = "drop") |>
#   summarise(max_abs_diff = max(abs(orig - split)))

