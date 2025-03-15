# 05_create_survey_designs.R
# Purpose: Create survey design objects to account for complex sampling design.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse) 
library(survey)
library(srvyr)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD IMPUTED DATA AND CONVERT OUTCOMES TO NUMERIC FOR SURVEY DESIGN
# ---------------------- #

# Load imputed dataset
imp <- readRDS(file.path(processed_data_dir, "04A_ca_imputed.rds"))
ca_imp <- readRDS(file.path(processed_data_dir, "04B_ca_completed.rds"))

# Convert binary factor variables back to numeric (0,1) for survey design
ca_imp <- lapply(ca_imp, function(data) {
  data %>% mutate(across(all_of(outcome_vars), ~ as.numeric(. == "Yes")))
})

# ---------------------- #
# FETCH CALIFORNIA 18+ POPULATION FROM TIDYCENSUS FOR WEIGHT ADJUSTMENT
# ---------------------- #

# Set API key to avoid tidycensus warnings
census_api_key(Sys.getenv("census_api_key"), overwrite = TRUE)

# Retrieve ACS population estimates for California residents aged 18+
pop_denoms <- get_acs(
  geography = "state",
  state = "CA",
  variables = c(
    paste0("B01001_", sprintf("%03d", 7:25)),  # Male age groups 18+
    paste0("B01001_", sprintf("%03d", 31:49))  # Female age groups 18+
  ),
  year = 2020,
  survey = "acs5"
)

# Sum population estimates across all 18+ age groups
total_population <- sum(pop_denoms$estimate, na.rm = TRUE)

# Print the total CA adult population
print(paste("Total CA population (18+):", total_population))

# ---------------------- #
# ADJUST WEIGHTS FOR MULTI-YEAR BRFSS DATA
# ---------------------- #

# Compute sample size proportions per year
samp_size <- lapply(ca_imp, function(data) {
  data %>%
    group_by(YEAR) %>%
    summarize(samp_size = n() / nrow(data), .groups = "drop")
})

# Apply sample size proportions and normalize weights
ca_imp <- mapply(function(data, samp) {
  data <- left_join(data, samp, by = "YEAR")
  
  data <- data %>%
    mutate(final_wt = LLCPWT / length(unique(data$YEAR)),  # Adjust for number of years
           final_wt = final_wt / sum(final_wt, na.rm = TRUE) * total_population)  # Normalize to CA pop
  
  data %>% mutate(one = 1)  # Add constant "one" for total population check
}, ca_imp, samp_size, SIMPLIFY = FALSE)

# ---------------------- #
# SPECIFY SURVEY DESIGNS
# ---------------------- #

# Create survey design objects for each imputed dataset
survey_designs <- lapply(ca_imp, function(data) {
  svydesign(
    id = ~1, 
    strata = ~STSTR, 
    weights = ~final_wt,  # Use adjusted weights
    data = data
  ) %>%
    as_survey_design()  # Convert to srvyr format for easier manipulation
})

# ---------------------- #
# VALIDATION CHECKS
# ---------------------- #

# Check final weights
print(summary(ca_imp[[1]]$final_wt))

# Check total weighted sample size (should match ~30.4M)
svytotal(~one, design = survey_designs[[1]])

# Check weighted prevalence of smoking (should sum to ~1)
svymean(~smoke, design = survey_designs[[1]], na.rm = TRUE)

# Verify total weighted count for health insurance (should be reasonable)
svytotal(~ins_no, design = survey_designs[[1]])

# Check structure of survey design
summary(survey_designs[[1]])

# Check factor levels after collapsing
table(ca_imp[[1]]$smoke)
levels(as.factor(ca_imp[[1]]$smoke))  # Ensure correct factor levels


# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

saveRDS(survey_designs, file.path(processed_data_dir, "05_survey_designs.rds"))
message("Saved survey design objects: ", file.path(processed_data_dir, "05_survey_designs.rds"))

# End of script
message("05_create_survey_designs.R completed successfully.")

