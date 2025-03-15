# config.R (Example)
# Purpose: Define user-specific settings for file paths and directories

# Set paths to your local data directories

# Path to raw SAS files (update this to your local location)
raw_data_dir <- "/path/to/your/files/CA-BRFSS-2014-2022"

# Path to processed files (inside your cloned GitHub repo)
processed_data_dir <- "/path/to/your/cloned/repo/01_data"

# Path to result files 
results_dir <- "/path/to/your/cloned/repo/03_results"

# Ensure raw data directory exists
if (!dir.exists(raw_data_dir)) {
  stop("ERROR: The raw data directory does not exist. Update 'config.R' with the correct path.")
}

# Ensure processed data directory exists (create if missing)
if (!dir.exists(processed_data_dir)) {
  message("Creating processed data directory: ", processed_data_dir)
  dir.create(processed_data_dir, showWarnings = FALSE, recursive = TRUE)
}

# Ensure results directory exists (create if missing)
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  message("Created missing results directory: ", results_dir)
}

# Confirm directory setup
message("Using raw data directory: ", raw_data_dir)
message("Using processed data directory: ", processed_data_dir)
message("Using results directory", results_dir)

# Load some commonly used packages (update or comment out if not preferred)
library(rstudioapi)
library(tidyverse)
library(tidycensus)
library(summarytools)
library(janitor)

# Suppress scientific notation for cleaner output
options(scipen = 999)

# Set survey design settings
options(survey.lonely.psu = "adjust")

# Set API key (replace "YOUR_CENSUS_API_KEY" with actual key in `.Renviron`)
# The key should be stored in your system's `.Renviron` file 
# Run the following command in your R console:

# usethis::edit_r_environ()

# This will open the `.Renviron` file in RStudio. 
# Add the following line (without quotes), replacing YOUR_CENSUS_API_KEY:

# census_api_key=YOUR_CENSUS_API_KEY

# Save the `.Renviron` file and restart R for the changes to take effect.
# Retrieve API key from environment
api_key <- Sys.getenv("census_api_key")

# Set the API key only if it's missing or empty
if (nzchar(api_key)) {
  message("Census API key already set.")
} else {
  census_api_key(api_key, install = TRUE)
  message("Census API key set successfully.")
}