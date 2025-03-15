# 05_summarize_data.R
# Purpose: Create tables summarizing study sample.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(gt)
library(gtsummary)
library(survey)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD IMPUTED DATA AND CONVERT OUTCOMES TO NUMERIC FOR SURVEY DESIGN
# ---------------------- #

# Load imputed survey designs
survey_designs <- readRDS(file.path(processed_data_dir, "05_survey_designs.rds"))

# Extract first imputed dataset for Table 1
imputed_design <- survey_designs[[1]]

# Convert table_vars back to factors and apply level labels to outcome_vars
imputed_design$variables <- imputed_design$variables %>%
  mutate(across(all_of(outcome_vars), ~ factor(.x, levels = c(1, 0), labels = c("Yes", "No")))) %>%
  mutate(across(all_of(setdiff(table_vars, outcome_vars)), as.factor))  

# Apply variable labels for gtsummary
var_label(imputed_design$variables) <- as.list(variable_labels)

# ---------------------- #
# CREATE TABLE 1
# ---------------------- #

# Generate Table 1A: Stratified by Sex
table1_sex <- tbl_svysummary(
  data = imputed_design,
  by = "sex",
  include = all_of(setdiff(table_vars, "age_grp")),
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
  digits = all_categorical() ~ c(0, 1),
  missing = "no"
) %>% 
  add_overall() %>% 
  modify_header(label = "Characteristic", 
                all_stat_cols() ~ 
                  "{level}, N = {n_unweighted} ({style_percent(p)}%)") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "Sex")

# Generate Table 1B: Stratified by Age Group
table1_age <- tbl_svysummary(
  data = imputed_design,
  by = "age_grp",
  include = all_of(setdiff(table_vars, "sex")),
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
  digits = all_categorical() ~ c(0, 1),
  missing = "no"
) %>% 
  modify_header(label = "Characteristic", 
                all_stat_cols() ~ 
                  "{level}, N = {n_unweighted} ({style_percent(p)}%)") %>%
  modify_spanning_header(all_stat_cols() ~ "Age group")

# Merge tables by characteristic 
table1 <- as_tibble(table1_sex) %>%
  full_join(as_tibble(table1_age), by = "Characteristic") %>% 
  gt() %>% 
  tab_spanner(label = "Sex", columns = names(as_tibble(table1_sex))[-c(1,2)]) %>%
  tab_spanner(label = "Age group", columns = names(as_tibble(table1_age))[-1])

# Print table
table1

# ---------------------- #
# SAVE FILES TO RESULTS DIRECTORY
# ---------------------- #

# Save table as an RDS object
saveRDS(table1, file.path(results_dir, "06_table1_summary.rds"))

# Save as a Word document
gtsave(table1, file.path(results_dir, "06_table1_summary.docx"))

# End of script
message("06_summarize_data.R completed successfully.")