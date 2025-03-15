# 04_multiple_imputation.R
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
ca_df <- readRDS(file.path(processed_data_dir, "03_ca_race_cleaned.rds"))

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
  df_subset, 
  method = methods, 
  m = 5, 
  maxit = 5, 
  seed = 123, 
  predictorMatrix = pred_matrix
)

# Extract completed datasets from the imputation object
ca_imp <- complete(imp, action = "all") 

# Add back in survey variables
ca_imp <- lapply(ca_imp, function(df_imp) {
  bind_cols(ca_df %>% select(all_of(exclude_vars)), df_imp)
})

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save the imputed dataset
saveRDS(imp, file.path(processed_data_dir, "04A_ca_imputed.rds"))
saveRDS(ca_imp, file.path(processed_data_dir, "04B_ca_completed.rds"))
message("Saved imputed dataset: ", file.path(processed_data_dir, "04A_ca_imputed.rds"))
message("Saved completed imputations: ", file.path(processed_data_dir, "04B_ca_completed.rds"))

# End of script
message("04_multiple_imputation.R completed successfully.")