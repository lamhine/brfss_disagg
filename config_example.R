# config.R (Example)
# Purpose: Define user-specific settings for file paths and directories

# Set paths to your local data directories

# Path to raw SAS files (update this to your local location)
raw_data_dir <- "/path/to/your/files/CA-BRFSS-2014-2022"

# Path to processed files (inside your cloned GitHub repo)
processed_data_dir <- "/path/to/your/cloned/repo/01_data"

# Ensure raw data directory exists
if (!dir.exists(raw_data_dir)) {
  stop("ERROR: The raw data directory does not exist. Update 'config.R' with the correct path.")
}

# Ensure processed data directory exists (create if missing)
if (!dir.exists(processed_data_dir)) {
  message("Creating processed data directory: ", processed_data_dir)
  dir.create(processed_data_dir, showWarnings = FALSE, recursive = TRUE)
}

# Confirm directory setup
message("Using raw data directory: ", raw_data_dir)
message("Using processed data directory: ", processed_data_dir)

# Load some commonly used packages (update or comment out if not preferred)
library(tidyverse)
library(summarytools)
library(janitor)

# Suppress scientific notation for cleaner output
options(scipen = 999)