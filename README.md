# California BRFSS (2014-2022) – Disaggregated Health Analysis

## Introduction
This repository contains code for analyzing the California Behavioral Risk Factor Surveillance System (BRFSS) microdata (2014-2022) with a focus on disaggregating health indicators by detailed race/ethnicity groups. The analysis applies survey weighting, multiple imputation for missing data, age standardization, and predictive marginal standardization to ensure robust estimates.

Key health indicators include:
- **Health behaviors** (smoking, alcohol use, physical activity, preventive care)
- **Chronic conditions** (diabetes, asthma, cardiovascular disease)
- **General health status**
- **Disabilities** (vision impairment, cognitive difficulty, mobility limitations)

## Data Availability and Access
The California BRFSS microdata used in this project **are not publicly available** and must be requested directly from the California Department of Public Health (CDPH).

### How to Request BRFSS Data:
1. Visit the [CDPH BRFSS Data Request Page](https://www.cdph.ca.gov/Programs/CCDPHP/DCDIC/CDSRB/Pages/BRFSS.aspx)
2. Complete a Data Use Agreement (DUA) with CDPH.
3. Once approved, download the **SAS-format (.sas7bdat) files**.
4. After obtaining the data, place them in the **raw data directory** specified in `config.R`.

## Setup Instructions
### Step 1: Download this repository
```
git clone https://github.com/lamhine/brfss_ca.git
cd brfss_ca
```

Or download the ZIP manually from GitHub and extract it.

### Step 2: Configure file paths
- Copy `config_example.R` to `config.R`:
```
cp config_example.R config.R
```
- Open `config.R` in **RStudio** and **update the paths** for:
  - `raw_data_dir`: The folder containing `.sas7bdat` files from CDPH.
  - `processed_data_dir`: The location where cleaned files will be stored.

### Step 3: Open the RStudio Project (`brfss_disagg.Rproj`).

### Step 4: Run the analysis pipeline in R
```
source(“01_load_data.R”)    # Loads raw BRFSS data
source(“02_clean_data.R”)   # Cleans and preprocesses data
source("03_define_race_categories.R")   # Creates detailed race/ethnicity categories
source(“04_multiple_imputation.R”)  # Handles missing data
source(“05_analyze_data.R”) # Runs analysis and estimates prevalence
source(“06_visualize_results.R”) # Generates tables and plots
```

## Outputs and File Structure
| File/Folder | Description |
|-------------|------------|
| `01_load_data.R` | Reads raw `.sas7bdat` files and merges 2014-2022 BRFSS data. |
| `02_clean_data.R` | Cleans variables, handles missing values, and applies factor recoding. |
| `03_define_race_categories.R` | Cleans and creates detailed race/ethnicity categories. |
| `04_multiple_imputation.R` | Uses multiple imputation for missing data (if applicable). |
| `05_analyze_data.R` | Computes prevalence estimates and standardization. |
| `06_visualize_results.R` | Generates tables and visualizations. |
| `01_data/` | Stores intermediate and final processed datasets. |
| `figures/` | Stores generated plots and tables. |
| `config_example.R` | Template for setting up file paths. |
| `README.md` | Project documentation. |

## Troubleshooting
- **Data loading errors?** Ensure `.sas7bdat` files are placed in the correct `raw_data_dir`.
- **`config.R` errors?** Double-check that paths are set correctly.
- **RStudio errors?** Always open the `.Rproj` file before running scripts.
- **Missing packages?** Install them using:
```
install.packages(c(“tidyverse”, “haven”, “survey”, “janitor”, “summarytools”))
```

For questions or issues, open a GitHub issue.

## License
This repository contains code only, as the California BRFSS dataset is subject to CDPH data use policies.

## Acknowledgments
This analysis was conducted using California BRFSS microdata provided by the CDPH. The author acknowledges the contributions of the CDC BRFSS program in survey design and implementation and would like to thank Vanessa Miguelino-Keasling, MPH and Nicholas Cuvelier, MPH for their assistance in accessing the data. 
