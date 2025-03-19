# 05_summarize_data.R
# Purpose: Create tables summarizing study sample.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(labelled)
library(gt)
library(gtsummary)
library(survey)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD IMPUTED DATA AND CONVERT OUTCOMES TO NUMERIC FOR SURVEY DESIGN
# ---------------------- #

# Load cleaned dataset and imputed survey designs
ca_df <- readRDS(file.path(processed_data_dir, "03_ca_race_cleaned.rds"))
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

# Generate Table: Summary of participant characteristics
table1 <- tbl_svysummary(
  data = imputed_design, 
  include = c("sex", "age_grp", "re_text"),
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
  digits = all_categorical() ~ c(0, 1),
  missing = "no")

# ---------------------- #
# CREATE APPENDIX TABLES
# ---------------------- #

# Generate Appendix A1: Stratified by Sex
appendix_A1_sex <- tbl_svysummary(
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

# Generate Appendix A2: Stratified by Age Group
appendix_A1_age <- tbl_svysummary(
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
appendix_A <- as_tibble(appendix_A1_sex) %>%
  full_join(as_tibble(appendix_A1_age), by = "Characteristic") %>% 
  gt() %>% 
  tab_spanner(label = "Sex", columns = names(as_tibble(appendix_A1_sex))[-c(1,2)]) %>%
  tab_spanner(label = "Age group", columns = names(as_tibble(appendix_A1_age))[-1])

# Print table
appendix_A

# Generate Appendix B: Missingness frequencies
ca_df <- ca_df %>%
  mutate(
    re_groups = case_when(
      re_text %in% hispanic_groups ~ "Hispanic",
      re_text %in% names(re_groups_lookup) ~ re_groups_lookup[re_text],
      TRUE ~ "Multiracial"
    ),
    re_groups = factor(re_groups, levels = re_groups_levels) # Ensure correct ordering
  )

# Compute missingness for each variable overall
missing_overall <- ca_df %>%
  summarise(across(all_of(table_vars), ~ mean(is.na(.)) * 100, .names = "Overall_{.col}")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Overall") %>%
  mutate(Variable = str_remove(Variable, "Overall_"))

# Compute missingness by re_groups
missing_by_group <- ca_df %>%
  group_by(re_groups) %>%
  summarise(across(all_of(table_vars), ~ mean(is.na(.)) * 100, .names = "{.col}")) %>%
  pivot_longer(cols = -re_groups, names_to = "Variable", values_to = "Missing (%)") %>%
  pivot_wider(names_from = re_groups, values_from = `Missing (%)`)

# Merge tables
missing_table <- missing_overall %>%
  left_join(missing_by_group, by = "Variable") %>%
  left_join(tibble(Variable = names(variable_labels), Label = variable_labels), by = "Variable") %>%
  select(Label, Overall, all_of(re_groups_levels)) %>%
  arrange(match(Label, variable_labels))

# Convert to gt table for better formatting
missing_gt <- missing_table %>%
  gt() %>%
  fmt_number(
    columns = everything(),
    decimals = 1
  ) %>%
  cols_label(Label = "Variable", Overall = "Overall") 


# ---------------------- #
# SAVE FILES TO RESULTS DIRECTORY
# ---------------------- #

# Save table as an RDS object (un-commment if needed)
# saveRDS(table1, file.path(results_dir, "06_table1_summary.rds"))

# Save as a Word document
gtsave(as_gt(table1), file.path(results_dir, "06_table1_summary.docx"))
gtsave(appendix_A, file.path(results_dir, "06_appendix_A.docx"))
gtsave(missing_gt, file.path(results_dir, "06_appendix_B.docx"))

# End of script
message("06_summarize_data.R completed successfully.")