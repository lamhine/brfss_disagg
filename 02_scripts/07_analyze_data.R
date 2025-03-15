# 07_analyze_data.R
# Purpose: Compute unadjusted and age-sex standardized prevalence estimates.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(survey)
library(tidycensus)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD IMPUTED SURVEY DESIGN
# ---------------------- #

# Load the imputed survey design object
survey_designs <- readRDS(file.path(processed_data_dir, "05_survey_designs.rds"))

# Use the first imputed dataset
ca_dsn <- survey_designs[[1]]

# ---------------------- #
# LOAD REFERENCE POPULATION (ACS 2020)
# ---------------------- #

# Retrieve ACS estimates for California residents aged 18+
pop_denoms <- get_acs(
  geography = "state",
  state = "CA",
  variables = c(
    paste0("B01001_", sprintf("%03d", 7:25)),  # Male age groups 18+
    paste0("B01001_", sprintf("%03d", 31:49))  # Female age groups 18+
  ),
  year = 2020,
  survey = "acs5"
) %>%
  mutate(
    sex = if_else(str_detect(variable, "B01001_0(0[7-9]|1[0-9]|2[0-5])"), "Male", "Female"),
    sex = factor(sex, levels = c("Male", "Female")),  
    age_grp = case_when(
      variable %in% c("B01001_007", "B01001_031", "B01001_008", "B01001_032", "B01001_009", "B01001_033") ~ "15 to 29 years",
      variable %in% c("B01001_010", "B01001_034", "B01001_011", "B01001_035", "B01001_012", "B01001_036") ~ "30 to 44 years",
      variable %in% c("B01001_013", "B01001_037", "B01001_014", "B01001_038", "B01001_015", "B01001_039") ~ "45 to 59 years",
      variable %in% c("B01001_016", "B01001_040", "B01001_017", "B01001_041", "B01001_018", "B01001_042") ~ "60 years and over",
      TRUE ~ NA_character_
    ),
    age_grp = factor(age_grp, 
                     levels = c("15 to 29 years", "30 to 44 years", 
                                "45 to 59 years", "60 years and over"), 
                     ordered = TRUE)
  ) %>%
  filter(!is.na(age_grp)) %>%
  group_by(sex, age_grp) %>%
  summarise(estimate = sum(estimate), .groups = "drop")

# ---------------------- #
# COMPUTE AGE-SEX DISTRIBUTION IN BRFSS DATA
# ---------------------- #

age_sex_distribution <- ca_dsn %>%
  group_by(re_text, age_grp, sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(re_text) %>%
  mutate(
    total_count = sum(count),
    age_sex_dist = count / total_count,
    age_grp = factor(age_grp, 
                     levels = c("15 to 29 years", "30 to 44 years", 
                                "45 to 59 years", "60 years and over"), 
                     ordered = TRUE)
  ) %>%
  select(-count, -total_count)

# ---------------------- #
# PREVALENCE ESTIMATION (UNADJUSTED & STANDARDIZED)
# ---------------------- #

# Initialize a list to store results
all_results <- list()

# Loop through each outcome variable
for (outcome in outcome_vars) {
  
  # Compute unadjusted prevalence (survey-weighted) stratified by age, sex, and re_text
  unadj_df <- svyby(
    formula = as.formula(paste0("~", outcome)),
    by = as.formula("~re_text + age_grp + sex"),
    design = ca_dsn,
    FUN = svymean,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(prevalence = !!sym(outcome), se_prevalence = se) %>%  # Correct renaming
    mutate(
      adjustment_type = "Unadjusted",
      RSE = case_when(prevalence > 0 ~ (se_prevalence / prevalence) * 100, TRUE ~ NA_real_)
    )
  
  # Merge age-sex distributions with ACS denominators
  merged_dist <- left_join(age_sex_distribution, pop_denoms, by = c("age_grp", "sex")) %>%
    mutate(weight = estimate / sum(estimate, na.rm = TRUE))
  
  # Merge with unadjusted prevalence
  merged_dist <- left_join(merged_dist, unadj_df, by = c("re_text", "age_grp", "sex")) %>%
    mutate(expected_cases = prevalence * weight * sum(pop_denoms$estimate, na.rm = TRUE))
  
  # Compute standardized prevalence using Taylor Series Approximation
  std_df <- merged_dist %>%
    group_by(re_text) %>%
    summarise(
      prevalence = sum(expected_cases, na.rm = TRUE) / sum(pop_denoms$estimate, na.rm = TRUE),
      se_prevalence = sqrt(sum((weight^2) * (se_prevalence^2), na.rm = TRUE)),  
      RSE = case_when(prevalence > 0 ~ (se_prevalence / prevalence) * 100, TRUE ~ NA_real_),
      .groups = "drop"
    ) %>% 
    mutate(adjustment_type = "Standardized")
  
  # Combine unadjusted and standardized results
  combined_df <- bind_rows(unadj_df, std_df) %>%
    mutate(outcome = outcome)
  
  # Store results
  all_results[[outcome]] <- combined_df
}

# Combine results into a single dataframe
final_combined_df <- bind_rows(all_results)

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save output
saveRDS(final_combined_df, file.path(processed_data_dir, "07_prevalence_estimates.rds"))
message("Saved prevalence estimates: ", file.path(processed_data_dir, "07_prevalence_estimates.rds"))

# Print summary
glimpse(final_combined_df)