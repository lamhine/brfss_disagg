# setup.R
# Purpose: Standardized project setup for all scripts

# ---------------------- #
# SET DIRECTORIES
# ---------------------- #

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration (defines `raw_data_dir`, `processed_data_dir`, `results_dir`)
source("config.R")

# Function to ensure directories exist, creating them if missing
dir_check_create <- function(dir_path, desc) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created missing ", desc, ": ", dir_path)
  }
}

# Ensure all required directories exist
dir_check_create(raw_data_dir, "raw data directory")
unzipped_dir <- file.path(raw_data_dir, "unzipped")  # Unzipped files inside raw data directory
dir_check_create(unzipped_dir, "unzipped data directory")
dir_check_create(processed_data_dir, "processed data directory")
dir_check_create(results_dir, "results directory")

# ---------------------- #
# DEFINE VARIABLE GROUPS
# ---------------------- #

# Final list of all relevant variables
vars <- c("LLCPWT", "PSU", "STSTR", "YEAR", "sex", "age", "age_grp", "hisp", 
          "race", "re_text", "edu", "employ", "income", "ownhome", "marital", 
          "children", "smoke", "drink_any", "drink_binge", "exer_no", 
          "hivtest_no", "laiv_no", "pcv_no", "chk2_no", "pcp_no", "ins_no",
          "apcvd", "mi", "dm", "ckd", "ast_lt", "ast_now", "copd", "bcc", 
          "ovwob", "mdd", "fphs", "mphd_poor", "mmhd_poor", "vip", "dis_walk",  
          "dis_conc", "dis_err", "dis_dress")

# Variables to exclude from imputation
exclude_vars <- c("YEAR", "PSU", "STSTR", "LLCPWT", "age", "hisp", "race", "re_text")

# Variables included in imputation
impute_vars <- setdiff(vars, exclude_vars)

# Variables used as outcomes in analysis (binary numeric during survey design)
outcome_vars <- setdiff(
  impute_vars, c("sex", "age_grp", "edu", "employ", "income", "ownhome", 
                 "marital", "children"))

# Variables for passing to gtsummary to create tables
table_vars <- c("sex", "age_grp", outcome_vars)

# Label variables
variable_labels <- c(
  # Demographics
  sex = "Male sex",
  age_grp = "Age categories",
  re_text = "Race/Ethnicity",
  
  # Lifestyle factors
  smoke = "Ever smoked",
  drink_any = "Any alcohol use",
  drink_binge = "Binge drinking",
  exer_no = "No exercise in last 30 days",
  
  # Preventive healthcare
  hivtest_no = "Never had HIV test",
  laiv_no = "Never had flu vaccine",
  pcv_no = "Never had pneumonia vaccine",
  chk2_no = "No routine checkup in last 2 years",
  pcp_no = "No primary care provider",
  ins_no = "No health insurance",
  
  # Chronic conditions
  apcvd = "Angina or coronary heart disease",
  mi = "Heart attack",
  dm = "Diabetes diagnosis",
  ckd = "Chronic kidney disease",
  ast_lt = "Lifetime asthma",
  ast_now = "Current asthma",
  copd = "COPD diagnosis",
  bcc = "Skin cancer diagnosis",
  ovwob = "Overweight or obese",
  mdd = "Major depression diagnosis",
  
  # General health status
  fphs = "Fair or poor health status",
  mphd_poor = "Frequent poor physical health",
  mmhd_poor = "Frequent poor mental health",
  
  # Disabilities
  vip = "Vision impairment",
  dis_walk = "Difficulty walking",
  dis_conc = "Cognitive difficulty",
  dis_err = "Difficulty running errands",
  dis_dress = "Difficulty dressing"
)

# Define category labels
outcome_cats <- c(
  # Lifestyle factors
  "smoke" = "Lifestyle factors",
  "exer_no" = "Lifestyle factors",
  "drink_any" = "Lifestyle factors",
  "drink_binge" = "Lifestyle factors",
  
  # Preventive healthcare
  "hivtest_no" = "Preventive healthcare",
  "laiv_no" = "Preventive healthcare",
  "pcv_no" = "Preventive healthcare",
  "ins_no" = "Preventive healthcare",
  "chk2_no" = "Preventive healthcare",
  "pcp_no" = "Preventive healthcare",
  
  # Chronic conditions
  "apcvd" = "Chronic conditions",
  "mi" = "Chronic conditions",
  "dm" = "Chronic conditions",
  "ckd" = "Chronic conditions",
  "ast_lt" = "Chronic conditions",
  "ast_now" = "Chronic conditions",
  "copd" = "Chronic conditions",
  "bcc" = "Chronic conditions",
  "mdd" = "Chronic conditions",
  "ovwob" = "Chronic conditions",
  
  # General health status
  "fphs" = "General health status",
  "mphd_poor" = "General health status",
  "mmhd_poor" = "General health status",
  
  # Disabilities
  "vip" = "Disabilities",
  "dis_walk" = "Disabilities",
  "dis_conc" = "Disabilities",
  "dis_err" = "Disabilities",
  "dis_dress" = "Disabilities"
)

# ---------------------- #
# CONFIRM SETUP
# ---------------------- #

# Message confirming setup
message("Project setup complete. 
  Raw Data: ", raw_data_dir, 
        " | Unzipped Data: ", unzipped_dir, 
        " | Processed Data: ", processed_data_dir, 
        " | Results: ", results_dir)