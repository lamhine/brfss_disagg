# =====================================================================
# 01_load_data.R [LOAD BRFSS DATA]
# Purpose: Load and merge California BRFSS microdata (2014-2022)
# Author: Tracy Lam-Hine
# Created: 2025-03-04
# =====================================================================

### 1. LOAD PACKAGES & CONFIGURATION ####################################

# Load required packages
library(haven)       # Read .sas7bdat files
library(data.table)  # Efficient data handling
library(tidyverse)   # Core tidyverse packages
library(rstudioapi)  # Ensure RProj environment detection

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() ||
    is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration file
source("config.R")

# Confirm raw data directory is correctly set
if (!dir.exists(raw_data_dir)) {
  stop(
    "ERROR: The data directory does not exist. Please update 'config.R' with the correct path."
  )
}

# Ensure processed data directory exists before saving
if (!dir.exists(processed_data_dir)) {
  dir.create(processed_data_dir, recursive = TRUE)
  message("Created missing processed data directory: ",
          processed_data_dir)
}

### 2. LOAD & MERGE 2014-2022 CA BRFSS DATA #############################

# Get list of all `.sas7bdat` files in `raw_data_dir`
brfss_files <- list.files(path = raw_data_dir,
                          pattern = "\\.sas7bdat$",
                          full.names = TRUE)

if (length(brfss_files) == 0) {
  stop(
    "ERROR: No .sas7bdat files found in ",
    raw_data_dir,
    ". Ensure BRFSS data is correctly stored."
  )
}

# Read in BRFSS `.sas7bdat` files into a list
ca_brfss <- lapply(brfss_files, read_sas)

# Convert DATE column to date format for datasets where it is inconsistent
date_fix_indices <- c(1, 2, 5, 6, 7, 8)  # Specify datasets with mismatched DATE formats
for (i in date_fix_indices) {
  if ("DATE" %in% names(ca_brfss[[i]])) {
    ca_brfss[[i]]$DATE <- mdy(ca_brfss[[i]]$DATE)
  }
}

# Bind all list elements into one data frame
ca_bound <- rbindlist(ca_brfss, fill = TRUE) %>% as.data.frame()

# Ensure processed data directory exists before saving
if (!dir.exists(processed_data_dir)) {
  dir.create(processed_data_dir, recursive = TRUE)
  message("Created missing processed data directory: ",
          processed_data_dir)
}

# Save raw dataset
saveRDS(ca_bound, file = file.path(processed_data_dir, "ca_bound.rds"))
message("Saved raw BRFSS dataset: ",
        file.path(processed_data_dir, "ca_bound.rds"))

### 3. VALIDATION CHECKS #######################################################

# Quick check of loaded data
glimpse(ca_bound)
summary(ca_bound$YEAR)  # Confirm years range from 2014-2022

# End of script
message("01_load_data.R completed successfully.")