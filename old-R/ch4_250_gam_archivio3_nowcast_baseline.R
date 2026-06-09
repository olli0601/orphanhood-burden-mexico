# =============================================================================
# ch4_250_gam_archivio3_nowcast_baseline.R  ·  Chapter 4 — Delay-adjusted nowcasting
# Archived patched baseline (with line-based credible intervals); kept for reproducibility.
# Reads input-data-processed/{births_grouped_mun,population_grouped_mun}.RDS -> models/predictions, output/ch4/.
# =============================================================================
source("R/rates.R")

################################################################################
# NOWCASTING REGISTERED BIRTHS - BASELINE MODEL
# Spec: Municipality-level β shifts national fertility curve
################################################################################

library(dplyr)
library(tidyr)
library(mgcv)
library(ggplot2)

################################################################################
# STEP 1: SETUP AND DATA LOADING
################################################################################

# Set working directory and load data
births <- readRDS("input-data-processed/births_grouped_mun.RDS") |>
  rename(event_year   = year,
         reg_year     = year_reg,
         municipality = group_id,
         age_group    = age,
         n            = births) |>
  mutate(reg_year = as.numeric(reg_year),
         delay = reg_year - event_year)
# Load population data
pop_tbl <- readRDS("input-data-processed/population_grouped_mun.RDS") |>
  rename(event_year   = year,
         municipality = group_id,
         age_group    = age) |>
  filter(!age_group %in% c("00-04", "05-09", "10-14"))

# Explore data structure
cat("=== BIRTHS DATA STRUCTURE ===\n")
cat("Dimensions:", dim(births), "\n")
cat("Columns:", colnames(births), "\n")
cat("Event years available:", range(births$event_year, na.rm = TRUE), "\n")
cat("Registration years available:", range(births$reg_year, na.rm = TRUE), "\n")
cat("Unique municipalities:", length(unique(births$municipality)), "\n")
cat("Unique age groups:", sort(unique(births$age_group)), "\n")
cat("Delay range:", range(births$delay, na.rm = TRUE), "\n")

cat("\n=== POPULATION DATA STRUCTURE ===\n")
cat("Dimensions:", dim(pop_tbl), "\n")
cat("Columns:", colnames(pop_tbl), "\n")
cat("Years available:", range(pop_tbl$event_year, na.rm = TRUE), "\n")
cat("Age groups:", sort(unique(pop_tbl$age_group)), "\n")

# Check for parent sex variable
sex_vars <- grep("sex", colnames(births), value = TRUE, ignore.case = TRUE)
cat("Sex-related variables:", sex_vars, "\n")

# Filter to training years (1990-2015) and appropriate age ranges by sex
births_train <- births %>% 
  filter(event_year >= 1990 & event_year <= 2015) %>%
  filter(
    (sex == "female" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) |
    (sex == "male" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"))
  )

pop_train <- pop_tbl %>%
  filter(event_year >= 1990 & event_year <= 2015) %>%
  filter(
    (sex == "female" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")) |
    (sex == "male" & age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"))
  )

cat("Training births dimensions:", dim(births_train), "\n")
cat("Training births years:", range(births_train$event_year, na.rm = TRUE), "\n")
cat("Training population dimensions:", dim(pop_train), "\n")
cat("Training population years:", range(pop_train$event_year, na.rm = TRUE), "\n")

################################################################################
# STEP 2: NATIONAL FERTILITY CURVE ANALYSIS
################################################################################

cat("\n=== PREPARING NATIONAL FERTILITY DATA ===\n")

# Aggregate births and population at national level by age and sex
# First, we need to collapse delays and sum births by event year
births_national <- births_train %>%
  group_by(event_year, age_group, sex) %>%
  summarise(births = sum(n, na.rm = TRUE), .groups = "drop")

# Aggregate population at national level  
pop_national <- pop_train %>%
  group_by(event_year, age_group, sex) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# Combine births and population to calculate fertility rates
fert_national <- births_national %>%
  left_join(pop_national, by = c("event_year", "age_group", "sex")) %>%
  filter(!is.na(population), population > 0, !is.na(births)) %>%
  mutate(fert_rate = births / population)

cat("National fertility data dimensions:", dim(fert_national), "\n")
cat("Sample of national fertility data:\n")
print(head(fert_national))

# Check sex distribution in data
cat("\n=== SEX DISTRIBUTION ANALYSIS ===\n")
if("sex" %in% colnames(fert_national)) {
  cat("Unique sex values:", unique(fert_national$sex), "\n")
  
  # Analyze fertility rate variation by sex
  sex_analysis <- fert_national %>%
    group_by(age_group, sex) %>%
    summarise(
      mean_fert_rate = mean(fert_rate, na.rm = TRUE),
      median_fert_rate = median(fert_rate, na.rm = TRUE),
      sd_fert_rate = sd(fert_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age_group, sex)
  
  cat("Fertility rates by age and sex:\n")
  print(sex_analysis)
  
  # Calculate ratio between female and male fertility rates
  sex_ratio <- sex_analysis %>%
    select(age_group, sex, mean_fert_rate) %>%
    pivot_wider(names_from = sex, values_from = mean_fert_rate) %>%
    mutate(female_to_male_ratio = ifelse(!is.na(female) & !is.na(male) & male > 0, 
                                        female / male, NA))
  
  cat("\nFemale to male fertility ratio by age:\n")
  print(sex_ratio)
  
  # Statistical test for sex differences
  cat("\n=== STATISTICAL SIGNIFICANCE OF SEX DIFFERENCES ===\n")
  
  # Test if fertility rates differ significantly by sex within each age group
  sex_test_results <- fert_national %>%
    filter(!is.na(fert_rate), fert_rate > 0) %>%
    group_by(age_group) %>%
    do({
      if(length(unique(.$sex)) > 1) {
        test_result <- try(wilcox.test(fert_rate ~ sex, data = .), silent = TRUE)
        if(!inherits(test_result, "try-error")) {
          data.frame(p_value = test_result$p.value, 
                    significant = test_result$p.value < 0.05)
        } else {
          data.frame(p_value = NA, significant = NA)
        }
      } else {
        data.frame(p_value = NA, significant = NA)
      }
    }) %>%
    ungroup()
  
  cat("Wilcoxon test results for sex differences by age group:\n")
  print(sex_test_results)
  
  # Overall conclusion
  significant_ages <- sum(sex_test_results$significant, na.rm = TRUE)
  total_ages <- sum(!is.na(sex_test_results$significant))
  
  cat("\nCONCLUSION:\n")
  cat("Age groups with significant sex differences:", significant_ages, "out of", total_ages, "\n")
  
  if(significant_ages > total_ages * 0.5) {
    cat("RECOMMENDATION: Include β_sex in the model (fertility varies significantly by parent sex)\n")
    use_sex_beta <- TRUE
  } else {
    cat("RECOMMENDATION: Do not include β_sex in the model (no strong evidence of sex differences)\n")
    use_sex_beta <- FALSE
  }
  
} else {
  cat("No sex variable found in data\n")
  use_sex_beta <- FALSE
}

# Function to compute national standardized rate (modified for births)
compute_national_std_rate_age_gender <- function(data) {
  
  # Use 2015 as reference year for training data
  pop_ref <- data %>% 
    filter(event_year %in% 2015) %>%  
    select(sex, age_group, event_year, population) %>% 
    group_by(sex, age_group) %>% 
    summarise(population = mean(population, na.rm = TRUE), .groups = "drop")
  
  p_nat <- pop_ref %>% 
    group_by(sex, age_group) %>% 
    summarise(p_nat = sum(population, na.rm = TRUE), .groups = "drop")
  
  p_nat_total <- sum(pop_ref$population, na.rm = TRUE)
  
  # Prepare standardized rates
  std_rate <- data %>% 
    select(event_year, age_group, sex, births, population, fert_rate) %>%
    left_join(p_nat, by = c("age_group", "sex")) %>%
    mutate(p_nat_total = p_nat_total) %>%
    mutate(std_rate = (p_nat / p_nat_total) * fert_rate) %>%
    group_by(age_group, sex, event_year) %>% 
    summarise(std_rate = sum(std_rate, na.rm = TRUE), .groups = "drop")
  
  return(std_rate)
}

# Calculate standardized national fertility curve
cat("\n=== CALCULATING STANDARDIZED NATIONAL FERTILITY CURVE ===\n")
std_fert_national <- compute_national_std_rate_age_gender(fert_national)

cat("Standardized fertility data dimensions:", dim(std_fert_national), "\n")
cat("Sample of standardized fertility data:\n")
print(head(std_fert_national))

# Save the decision about sex beta for later use
cat("\nSaving model specification:\n")
cat("Include β_sex in model:", use_sex_beta, "\n")

################################################################################
# STEP 3: DELAY TRIANGLE CONSTRUCTION
################################################################################

cat("\n=== CONSTRUCTING DELAY TRIANGLE ===\n")

# The delay triangle is already partially constructed in births_train
# We need to create the full triangle structure with columns:
# year (event_year), delay, age, parent_sex, n

# Check current delay distribution
delay_summary <- births_train %>%
  group_by(delay) %>%
  summarise(
    n_registrations = sum(n, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  ) %>%
  arrange(delay)

cat("Delay distribution summary:\n")
print(head(delay_summary, 15))

# Set maximum delay D for modeling (e.g., 10 years)
D <- 8
cat("\nUsing maximum delay D =", D, "years\n")

# Filter data to reasonable delays and create triangle
tri_delays <- births_train %>%
  filter(delay >= 0 & delay <= D) %>%
  select(event_year, delay, age_group, sex, municipality, n) %>%
  rename(year = event_year, age = age_group, parent_sex = sex) %>%
  filter(!is.na(n), n > 0)

cat("Triangle data dimensions:", dim(tri_delays), "\n")
cat("Delay range in triangle:", range(tri_delays$delay), "\n")
cat("Years in triangle:", range(tri_delays$year), "\n")

# Check triangle completeness
triangle_check <- tri_delays %>%
  group_by(year, delay) %>%
  summarise(
    total_events = sum(n, na.rm = TRUE),
    n_municipalities = n_distinct(municipality),
    .groups = "drop"
  ) %>%
  arrange(year, delay)

cat("Sample of triangle by year-delay:\n")
print(head(triangle_check, 15))

################################################################################
# STEP 4: GLOBAL DELAY DISTRIBUTION π (Dirichlet-smoothed)
################################################################################

cat("\n=== CALCULATING GLOBAL DELAY DISTRIBUTION ===\n")

# Aggregate delays across all municipalities, ages, and sexes
delay_counts <- tri_delays %>%
  group_by(delay) %>%
  summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(delay)

cat("Raw delay counts:\n")
print(delay_counts)

# Calculate raw proportions
delay_counts$raw_prop <- delay_counts$total_n / sum(delay_counts$total_n)

# Dirichlet smoothing approach
# Add small constant (alpha) to each delay count for smoothing
alpha <- 1  # Dirichlet parameter
delay_counts$smoothed_count <- delay_counts$total_n + alpha
delay_counts$pi_d <- delay_counts$smoothed_count / sum(delay_counts$smoothed_count)

cat("\nDirichlet-smoothed delay distribution:\n")
print(delay_counts)

# Create pi vector for use in modeling
pi_vec <- delay_counts$pi_d
names(pi_vec) <- paste0("delay_", delay_counts$delay)

cat("\nπ vector (π_0, π_1, ..., π_D):\n")
print(round(pi_vec, 4))

# Verify π sums to 1
cat("Sum of π:", round(sum(pi_vec), 6), "\n")

# Calculate cumulative reporting probabilities
delay_counts$cum_pi <- cumsum(delay_counts$pi_d)
cat("\nCumulative reporting probabilities:\n")
print(delay_counts[c("delay", "pi_d", "cum_pi")])

# Plot delay distribution
cat("\n=== DELAY DISTRIBUTION VISUALIZATION ===\n")

# Create simple visualization data
delay_plot_data <- data.frame(
  delay = delay_counts$delay,
  probability = delay_counts$pi_d,
  cumulative = delay_counts$cum_pi
)
cat("Delay distribution for plotting:\n")
print(delay_plot_data)


# Visualizzazione della curva cumulata dei ritardi
if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")
}
library(scales) # per percent_format
ggplot(delay_plot_data, aes(x = delay, y = cumulative)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_point(color = "#D55E00", size = 2) +
  scale_x_continuous(breaks = delay_plot_data$delay) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Curva cumulata dei ritardi di registrazione",
    x = "Ritardo (anni)",
    y = "Frazione cumulata dei casi registrati"
  ) +
  theme_minimal(base_size = 14)+
  geom_hline(yintercept = 0.99, linetype = "dashed", color = "red")

ggsave("output/ch4/delay_cumulative_curve.png", width = 7, height = 5)
cat("\nSalvata la curva cumulata dei ritardi come 'delay_cumulative_curve.png'\n")

# Save key parameters for modeling
T_now <- 2015  # Current time for training (last complete year)
cat("\nSaving parameters for modeling:\n")
cat("T_now (current time):", T_now, "\n")
cat("D (maximum delay):", D, "\n")
cat("α (Dirichlet parameter):", alpha, "\n")

################################################################################
# STEP 5: SINGLE MUNICIPALITY MODEL (PROTOTYPE)
################################################################################

cat("\n=== SINGLE MUNICIPALITY MODEL ===\n")

# Function to fit model for a single municipality
fit_municipality_model <- function(mun_id, tri_data, pop_data, nat_fert_curve, pi_vector, use_sex_effects = TRUE) {
  
  cat("Fitting model for municipality:", mun_id, "\n")
  
  # 1) Collapse delays to incidence for fitting (sum over delays)
  inc_m <- tri_data %>%
    filter(municipality == mun_id) %>%
    group_by(year, age, parent_sex) %>%
    summarise(events = sum(n, na.rm = TRUE), .groups = "drop")
  
  # 2) Add population data
  pop_m <- pop_data %>%
    filter(municipality == mun_id) %>%
    select(event_year, age_group, sex, population) %>%
    rename(year = event_year, age = age_group, parent_sex = sex)
  
  inc_m <- inc_m %>%
    left_join(pop_m, by = c("year", "age", "parent_sex"))
  
  # 3) Add national fertility curve - CORRECTED: calculate expected events first
  nat_curve <- nat_fert_curve %>%
    select(event_year, age_group, sex, std_rate) %>%
    rename(year = event_year, age = age_group, parent_sex = sex, f_ts = std_rate)
  
  inc_m <- inc_m %>%
    left_join(nat_curve, by = c("year", "age", "parent_sex"))
  
  # 4) Filter and prepare for modeling - CORRECTED: calculate expected events
  inc_m <- inc_m %>%
    filter(
      !is.na(events), !is.na(population), !is.na(f_ts),
      population > 0, f_ts > 0, events >= 0
    ) %>%
    mutate(
      # Calculate expected events from national curve
      expected_events = population * f_ts,
      log_pop = log(population),
      # Use expected events as offset (this is what β will multiply)
      log_expected = log(pmax(expected_events, 0.1)), # avoid log(0)
      age = factor(age),
      parent_sex = factor(parent_sex)
    )
  
  # Check if we have enough data
  if(nrow(inc_m) < 20) {
    cat("Warning: Municipality", mun_id, "has insufficient data (", nrow(inc_m), "observations)\n")
    return(list(
      municipality = mun_id,
      model = NULL,
      family = NULL,
      n_obs = nrow(inc_m),
      dispersion = NA,
      success = FALSE
    ))
  }
  
  cat("Municipality", mun_id, "- observations:", nrow(inc_m), "\n")
  
  # 5) Set sum-to-zero contrasts for age effects
  options(contrasts = c("contr.sum", "contr.poly"))
  
  # 6) Fit Poisson GLM/GAM - CORRECTED: use expected events as offset
  if(use_sex_effects && length(unique(inc_m$parent_sex)) > 1) {
    # Include sex effects
    formula_str <- "events ~ 1 + age + parent_sex + offset(log_expected)"
  } else {
    # Age effects only
    formula_str <- "events ~ 1 + age + offset(log_expected)"
  }
  
  cat("Using formula:", formula_str, "\n")
  cat("Expected events range:", round(range(inc_m$expected_events), 2), "\n")
  cat("Observed vs Expected ratio range:", round(range(inc_m$events / inc_m$expected_events), 2), "\n")
  
  fit_pois <- try(
    bam(
      as.formula(formula_str),
      family = poisson(),
      method = "fREML",
      discrete = TRUE,
      data = inc_m
    ),
    silent = TRUE
  )
  
  if(inherits(fit_pois, "try-error")) {
    cat("Error fitting Poisson model for municipality", mun_id, "\n")
    return(list(
      municipality = mun_id,
      model = NULL,
      family = NULL,
      n_obs = nrow(inc_m),
      dispersion = NA,
      success = FALSE
    ))
  }
  
  # 7) Check dispersion and upgrade to NB if needed
  disp <- sum(residuals(fit_pois, type = "pearson")^2) / fit_pois$df.residual
  cat("Dispersion:", round(disp, 3), "\n")
  
  if(!is.na(disp) && disp > 1.2) {
    cat("Upgrading to Negative Binomial due to overdispersion\n")
    
    fit_nb <- try(
      bam(
        as.formula(formula_str),
        family = nb(),
        method = "fREML", 
        discrete = TRUE,
        data = inc_m
      ),
      silent = TRUE
    )
    
    if(!inherits(fit_nb, "try-error")) {
      final_fit <- fit_nb
      final_family <- "nb"
    } else {
      cat("NB model failed, using Poisson\n")
      final_fit <- fit_pois
      final_family <- "poisson"
    }
  } else {
    final_fit <- fit_pois
    final_family <- "poisson"
  }
  
  # 8) Extract coefficients (β parameters) and calculate shifted curves
  beta_coefs <- coef(final_fit)
  
  # Calculate shifted fertility curve for this municipality
  shifted_curve <- inc_m %>%
    select(year, age, parent_sex, f_ts, expected_events) %>%
    distinct() %>%
    mutate(
      # Apply beta effects to shift the national curve
      beta_intercept = beta_coefs["(Intercept)"],
      beta_age = 0, # Initialize
      beta_sex = 0  # Initialize
    )
  
  # Add age effects (sum-to-zero contrasts)
  for(age_level in levels(inc_m$age)) {
    age_coef_name <- paste0("age", age_level)
    if(age_coef_name %in% names(beta_coefs)) {
      shifted_curve$beta_age[shifted_curve$age == age_level] <- beta_coefs[age_coef_name]
    }
  }
  
  # Add sex effects if included
  if(use_sex_effects && "parent_sex1" %in% names(beta_coefs)) {
    # Sum-to-zero contrasts: parent_sex1 = female - male, parent_sex2 = -(female - male)
    shifted_curve$beta_sex[shifted_curve$parent_sex == "female"] <- beta_coefs["parent_sex1"]
    shifted_curve$beta_sex[shifted_curve$parent_sex == "male"] <- -beta_coefs["parent_sex1"]
  }
  
  # Calculate the shifted fertility rate
  shifted_curve <- shifted_curve %>%
    mutate(
      # Total beta effect
      total_beta = beta_intercept + beta_age + beta_sex,
      # Shifted fertility rate (exp(beta) multiplies the national rate)
      fertility_multiplier = exp(total_beta),
      fertility_rate_shifted = f_ts * fertility_multiplier,
      expected_events_shifted = expected_events * fertility_multiplier
    )
  
  cat("Municipality", mun_id, "fitted successfully with", final_family, "family\n")
  cat("Beta coefficients:\n")
  print(round(beta_coefs, 4))
  cat("Fertility multiplier range:", round(range(shifted_curve$fertility_multiplier), 3), "\n")
  
  # Calcola predizioni cella-per-cella (mu) e salva theta se NB
  pred_mu <- as.numeric(predict(final_fit, type = "response"))
  pred_cells <- inc_m %>%
    mutate(
      mu = pred_mu,
      theta = if (final_family == "nb") final_fit$family$getTheta(TRUE) else NA,
      model_family = final_family,
      municipality = mun_id
    )

  return(list(
    municipality = mun_id,
    model = final_fit,
    family = final_family,
    n_obs = nrow(inc_m),
    dispersion = disp,
    beta_coefs = beta_coefs,
    data = inc_m,
    shifted_curve = shifted_curve,
    pred_cells = pred_cells, # <-- aggiunto: predizioni cella-per-cella
    success = TRUE
  ))
}

# Test the function on multiple municipalities
cat("\n=== TESTING ON MULTIPLE MUNICIPALITIES ===\n")

# Get municipalities with different data volumes
mun_counts <- tri_delays %>%
  group_by(municipality) %>%
  summarise(
    total_events = sum(n, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_events))

cat("Total municipalities:", nrow(mun_counts), "\n")
cat("Event count range:", range(mun_counts$total_events), "\n")

# Select top 20 largest and bottom 20 smallest municipalities
largest_muns <- head(mun_counts, 20)
smallest_muns <- tail(mun_counts, 20)

cat("\n=== TOP 20 LARGEST MUNICIPALITIES ===\n")
print(largest_muns)

cat("\n=== BOTTOM 20 SMALLEST MUNICIPALITIES ===\n")
print(smallest_muns)

# Function to test multiple municipalities
test_multiple_municipalities <- function(mun_list, label) {
  cat("\n=== TESTING", label, "===\n")
  
  results <- list()
  
  for(i in 1:nrow(mun_list)) {
    mun_id <- mun_list$municipality[i]
    cat("\n--- Municipality", i, "of", nrow(mun_list), ":", mun_id, "---\n")
    cat("Total events:", mun_list$total_events[i], "\n")
    
    result <- fit_municipality_model(
      mun_id = mun_id,
      tri_data = tri_delays,
      pop_data = pop_train,
      nat_fert_curve = std_fert_national,
      pi_vector = pi_vec,
      use_sex_effects = use_sex_beta
    )
    
    results[[i]] <- result
    
    if(result$success) {
      cat("✓ Model fitted successfully\n")
      cat("  Family:", result$family, "\n")
      cat("  Observations:", result$n_obs, "\n")
      cat("  Dispersion:", round(result$dispersion, 3), "\n")
      
      # Show key beta coefficients
      key_betas <- result$beta_coefs[c("(Intercept)", "parent_sex1")]
      if(!is.na(key_betas["parent_sex1"])) {
        cat("  β₀ (intercept):", round(key_betas["(Intercept)"], 3), "\n")
        cat("  β_sex:", round(key_betas["parent_sex1"], 3), "\n")
      } else {
        cat("  β₀ (intercept):", round(key_betas["(Intercept)"], 3), "\n")
        cat("  β_sex: not included\n")
      }
    } else {
      cat("✗ Model fitting failed\n")
    }
  }
  
  return(results)
}

# Test largest municipalities
largest_results <- test_multiple_municipalities(largest_muns, "LARGEST MUNICIPALITIES")

# Test smallest municipalities  
smallest_results <- test_multiple_municipalities(smallest_muns, "SMALLEST MUNICIPALITIES")

# Summary of results
cat("\n=== SUMMARY OF RESULTS ===\n")

# Analyze largest municipalities
largest_success <- sapply(largest_results, function(x) x$success)
largest_families <- sapply(largest_results[largest_success], function(x) x$family)
largest_dispersions <- sapply(largest_results[largest_success], function(x) x$dispersion)

cat("LARGEST MUNICIPALITIES:\n")
cat("  Success rate:", sum(largest_success), "/", length(largest_success), 
    "(", round(100*sum(largest_success)/length(largest_success), 1), "%)\n")
if(sum(largest_success) > 0) {
  cat("  Families used:", table(largest_families), "\n")
  cat("  Dispersion range:", round(range(largest_dispersions, na.rm=TRUE), 2), "\n")
  cat("  Mean dispersion:", round(mean(largest_dispersions, na.rm=TRUE), 2), "\n")
}

# Analyze smallest municipalities
smallest_success <- sapply(smallest_results, function(x) x$success)
smallest_families <- sapply(smallest_results[smallest_success], function(x) x$family)
smallest_dispersions <- sapply(smallest_results[smallest_success], function(x) x$dispersion)

cat("\nSMALLEST MUNICIPALITIES:\n")
cat("  Success rate:", sum(smallest_success), "/", length(smallest_success), 
    "(", round(100*sum(smallest_success)/length(smallest_success), 1), "%)\n")
if(sum(smallest_success) > 0) {
  cat("  Families used:", table(smallest_families), "\n")
  cat("  Dispersion range:", round(range(smallest_dispersions, na.rm=TRUE), 2), "\n")
  cat("  Mean dispersion:", round(mean(smallest_dispersions, na.rm=TRUE), 2), "\n")
}

# Compare beta coefficients between large and small municipalities
cat("\n=== BETA COEFFICIENT COMPARISON ===\n")

extract_beta_intercept <- function(results) {
  sapply(results, function(x) {
    if(x$success && !is.null(x$beta_coefs)) {
      return(x$beta_coefs["(Intercept)"])
    } else {
      return(NA)
    }
  })
}

extract_beta_sex <- function(results) {
  sapply(results, function(x) {
    if(x$success && !is.null(x$beta_coefs) && "parent_sex1" %in% names(x$beta_coefs)) {
      return(x$beta_coefs["parent_sex1"])
    } else {
      return(NA)
    }
  })
}

largest_beta0 <- extract_beta_intercept(largest_results)
largest_beta_sex <- extract_beta_sex(largest_results)
smallest_beta0 <- extract_beta_intercept(smallest_results)
smallest_beta_sex <- extract_beta_sex(smallest_results)

cat("β₀ (Intercept) comparison:\n")
cat("  Largest municipalities - mean:", round(mean(largest_beta0, na.rm=TRUE), 3), 
    "sd:", round(sd(largest_beta0, na.rm=TRUE), 3), "\n")
cat("  Smallest municipalities - mean:", round(mean(smallest_beta0, na.rm=TRUE), 3), 
    "sd:", round(sd(smallest_beta0, na.rm=TRUE), 3), "\n")

cat("β_sex comparison:\n")
cat("  Largest municipalities - mean:", round(mean(largest_beta_sex, na.rm=TRUE), 3), 
    "sd:", round(sd(largest_beta_sex, na.rm=TRUE), 3), "\n")
cat("  Smallest municipalities - mean:", round(mean(smallest_beta_sex, na.rm=TRUE), 3), 
    "sd:", round(sd(smallest_beta_sex, na.rm=TRUE), 3), "\n")

################################################################################
# STEP 6: FIT MODELS FOR ALL MUNICIPALITIES
################################################################################

cat("\n=== FITTING MODELS FOR ALL MUNICIPALITIES ===\n")

# Function to fit models for all municipalities with progress tracking
fit_all_municipalities <- function(mun_counts, tri_data, pop_data, nat_fert_curve, pi_vector, use_sex_effects = TRUE) {
  
  n_total <- nrow(mun_counts)
  results <- vector("list", n_total)
  names(results) <- mun_counts$municipality
  
  # Track progress and statistics
  success_count <- 0
  failure_count <- 0
  
  cat("Starting to fit models for", n_total, "municipalities...\n")
  
  for(i in 1:n_total) {
    mun_id <- mun_counts$municipality[i]
    
    # Progress indicator every 50 municipalities
    if(i %% 50 == 0 || i == 1 || i == n_total) {
      cat("Progress:", i, "/", n_total, "(", round(100*i/n_total, 1), "%) -", 
          "Success:", success_count, "Failures:", failure_count, "\n")
    }
    
    # Fit model for this municipality
    result <- try(
      fit_municipality_model(
        mun_id = mun_id,
        tri_data = tri_data,
        pop_data = pop_data,
        nat_fert_curve = nat_fert_curve,
        pi_vector = pi_vector,
        use_sex_effects = use_sex_effects
      ),
      silent = TRUE
    )
    
    # Handle errors gracefully
    if(inherits(result, "try-error")) {
      result <- list(
        municipality = mun_id,
        model = NULL,
        family = NULL,
        n_obs = 0,
        dispersion = NA,
        success = FALSE,
        error = as.character(result)
      )
    }
    
    results[[i]] <- result
    
    # Update counters
    if(result$success) {
      success_count <- success_count + 1
    } else {
      failure_count <- failure_count + 1
    }
  }
  
  cat("\nFinal results: Success:", success_count, "Failures:", failure_count, 
      "Success rate:", round(100*success_count/n_total, 1), "%\n")
  
  return(results)
}

# Run the full fitting process
cat("This will fit models for all", nrow(mun_counts), "municipalities.\n")
cat("This may take several minutes...\n")

# Fit all municipalities
all_results <- fit_all_municipalities(
  mun_counts = mun_counts,
  tri_data = tri_delays,
  pop_data = pop_train,
  nat_fert_curve = std_fert_national,
  pi_vector = pi_vec,
  use_sex_effects = use_sex_beta
)

# Analyze overall results
cat("\n=== OVERALL RESULTS ANALYSIS ===\n")

# Extract success/failure statistics
all_success <- sapply(all_results, function(x) x$success)
success_rate <- sum(all_success) / length(all_success) * 100

cat("Overall success rate:", round(success_rate, 1), "% (", sum(all_success), "/", length(all_success), ")\n")

# Analyze successful models
successful_results <- all_results[all_success]
if(length(successful_results) > 0) {
  # Family distribution
  families <- sapply(successful_results, function(x) x$family)
  cat("Family distribution:\n")
  print(table(families))
  # Dispersion statistics
  dispersions <- sapply(successful_results, function(x) x$dispersion)
  cat("Dispersion statistics:\n")
  cat("  Range:", round(range(dispersions, na.rm=TRUE), 2), "\n")
  cat("  Mean:", round(mean(dispersions, na.rm=TRUE), 2), "\n")
  cat("  Median:", round(median(dispersions, na.rm=TRUE), 2), "\n")
  # Extract beta coefficients for analysis
  beta_intercepts <- sapply(successful_results, function(x) {
    if(!is.null(x$beta_coefs)) x$beta_coefs["(Intercept)"] else NA
  })
  beta_sex_effects <- sapply(successful_results, function(x) {
    if(!is.null(x$beta_coefs) && "parent_sex1" %in% names(x$beta_coefs)) {
      x$beta_coefs["parent_sex1"]
    } else {
      NA
    }
  })
  cat("Beta coefficient summary:\n")
  cat("  β₀ (intercept) - mean:", round(mean(beta_intercepts, na.rm=TRUE), 3), 
      "sd:", round(sd(beta_intercepts, na.rm=TRUE), 3), "\n")
  cat("  β_sex - mean:", round(mean(beta_sex_effects, na.rm=TRUE), 3), 
      "sd:", round(sd(beta_sex_effects, na.rm=TRUE), 3), "\n")
  # Create summary data frame
  results_summary <- data.frame(
    municipality = sapply(successful_results, function(x) x$municipality),
    family = sapply(successful_results, function(x) x$family),
    n_obs = sapply(successful_results, function(x) x$n_obs),
    dispersion = sapply(successful_results, function(x) x$dispersion),
    beta_intercept = beta_intercepts,
    beta_sex = beta_sex_effects,
    stringsAsFactors = FALSE
  )
  cat("Summary data frame created with", nrow(results_summary), "successful municipalities\n")
  cat("First few rows:\n")
  print(head(results_summary))
  # --- AGGIUNTA: salva tutte le predizioni cella-per-cella ---
  all_pred_cells <- do.call(rbind, lapply(successful_results, function(x) x$pred_cells))
  saveRDS(all_pred_cells, "input-data-processed/nowcast_cell_predictions.RDS")
  cat("Salvate tutte le predizioni cella-per-cella in 'input-data-processed/nowcast_cell_predictions.RDS'\n")
} else {
  cat("No successful model fits found\n")
}

# Save results for later use
cat("\nSaving all results...\n")
saveRDS(all_results, "input-data-processed/municipality_models_all.RDS")
if(exists("results_summary")) {
  saveRDS(results_summary, "input-data-processed/municipality_models_summary.RDS")
}
cat("Results saved successfully!\n")
