# 04_multiple_imputation.R [IMPUTE MISSING DATA]
# Purpose: Handle missing data via multiple imputation.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)   
library(mice) 
library(future)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD CLEANED DATA WITH CODED RACE/ETHNICITY AND PREP FOR IMPUTATION
# ---------------------- #

# Load cleaned dataset with detailed race categorized from previous step
ca_df <- readRDS(file.path(processed_data_dir, "ca_race_cleaned.rds"))

# Define variables to exclude from imputation (survey design + race/ethnicity)
exclude_vars <- c("YEAR", "PSU", "STSTR", "LLCPWT", "age", "race", "hisp", "re_text")

# Define imputation variables (everything except excluded vars)
impute_vars <- setdiff(names(ca_df), exclude_vars)

# Subset data for imputation
df_subset <- ca_df %>% select(all_of(impute_vars))

# Check missing data summary
missing_summary <- df_subset %>% summarise(across(everything(), ~ mean(is.na(.)) * 100))
print(missing_summary)

# Define imputation methods based on variable types
methods <- map_chr(df_subset, ~ case_when(
  is.numeric(.) && length(unique(.)) > 10 ~ "pmm",   # Predictive Mean Matching for continuous variables
  is.factor(.) && nlevels(.) == 2 ~ "logreg",        # Logistic regression for binary factors
  is.factor(.) && nlevels(.) > 2 ~ "polyreg",        # Polytomous regression for multi-level factors
  TRUE ~ "pmm"                                       # Default to PMM
))

# Generate predictor matrix
pred_matrix <- quickpred(df_subset)

# Enable parallel processing
plan(multisession)

# Perform multiple imputation
imp <- mice(
  ca_df, 
  method = methods, 
  m = 20, 
  maxit = 10, 
  seed = 123, 
  predictorMatrix = pred_matrix
)

# Extract completed datasets from the imputation object
imputed_data <- complete(imp, action = "all") 

# Add back in survey variables
imputed_data <- lapply(imputed_data, function(df_imp) {
  bind_cols(df %>% select(all_of(survey_vars)), df_imp)
})

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save the imputed dataset
saveRDS(ca_imp, file.path(processed_data_dir, "ca_imputed.rds"))
message("Saved imputed dataset: ", file.path(processed_data_dir, "ca_imputed.rds"))

# End of script
message("03_multiple_imputation.R completed successfully.")