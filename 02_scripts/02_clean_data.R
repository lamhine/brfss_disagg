# 02_clean_data.R [CLEAN AND PREP BRFSS DATA]
# Purpose: Process raw BRFSS CA data, clean variables, handle missing values, and categorize variables.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(labelled)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD AND CLEAN DATA
# ---------------------- #

# Load raw dataset from previous step
ca_df <- readRDS(file.path(processed_data_dir, "ca_bound.rds"))

# Select and rename variables; subset to those needed 
ca_df <- ca_df %>%
  dplyr::select(
    `_LLCPWT`,
    `_PSU`,
    `_STSTR`,
    YEAR,
    # Sampling vars
    AGE,
    `_EDUCAG`,
    EMPLOY2,
    EMPLOY1,
    INCOM02,
    INCOM03,
    INCOME3,
    # Demographics
    MARITAL,
    `_CHLDCNT`,
    OWNHOME,
    RENTHOM1,
    SEX,
    SEX1,
    SEX2,
    BIRTHSEX,
    MRACASC1,
    MRACASC2,
    HISPANC3,
    # Keep original race/ethnicity variables
    DRNKANY5,
    DRNKANY6,
    `_RFBING5`,
    `_RFBING6`,
    # Alcohol use
    ASTHEVE3,
    ASTHMA3,
    ASTHNOW,
    ANGINA,
    CVDCRHD4,
    HEART2,
    CVDINFR4,
    # Health conditions
    COPDEVER,
    CHCCOPD3,
    DEPRESS1,
    ADDEPEV3,
    DIABCOR3,
    DIABETE4,
    KIDNEY,
    CHCKDNY2,
    SKCANC,
    CHCSCNC1,
    HAVEPLN3,
    `_HLTHPLN`,
    CHECKUP2,
    CHECKUP1,
    PERSDOC,
    PERSDOC1,
    PERSDOC3,
    GENHLTH,
    PHYSHLTH,
    MENTHLTH,
    # Health status
    AIDSTST8,
    AIDSTST9,
    HIVTST7,
    FLUSHOT6,
    FLUSHOT7,
    PNEUMVC3,
    PNEUMVC4,
    # Vaccines
    `_RFBMI5`,
    EXERANY1,
    EXERANY2,
    `_SMOKER3`,
    BLIND,
    REMEM2,
    DECIDE,
    DIFFERND,
    DIFFALON,
    DIFDRES2,
    DIFFDRES,
    DIFFWALK  # Disability & risk factors
  )

# Harmonize variables and rename to represent single constructs
ca_df <- ca_df %>%
  mutate(
    sex = coalesce(SEX, SEX1, SEX2, BIRTHSEX),
    race = coalesce(MRACASC1, MRACASC2),
    employ = coalesce(EMPLOY1, EMPLOY2),
    income = coalesce(INCOM02, INCOM03, INCOME3),
    ownhome = coalesce(OWNHOME, RENTHOM1),
    ast_lt = coalesce(ASTHEVE3, ASTHMA3),
    apcvd = coalesce(ANGINA, CVDCRHD4),
    mi = coalesce(HEART2, CVDINFR4),
    copd = coalesce(COPDEVER, CHCCOPD3),
    mdd = coalesce(DEPRESS1, ADDEPEV3),
    dm = coalesce(DIABCOR3, DIABETE4),
    ckd = coalesce(KIDNEY, CHCKDNY2),
    bcc = coalesce(SKCANC, CHCSCNC1),
    exer_no = coalesce(EXERANY1, EXERANY2),
    drink_any = coalesce(DRNKANY5, DRNKANY6),
    drink_binge = coalesce(`_RFBING5`, `_RFBING6`),
    hivtest_no = coalesce(AIDSTST8, AIDSTST9, HIVTST7),
    laiv_no = coalesce(FLUSHOT6, FLUSHOT7),
    pcv_no = coalesce(PNEUMVC3, PNEUMVC4),
    dis_conc = coalesce(REMEM2, DECIDE),
    dis_err = coalesce(DIFFERND, DIFFALON),
    dis_dress = coalesce(DIFDRES2, DIFFDRES),
    ins_no = coalesce(HAVEPLN3, `_HLTHPLN`),
    chk2_no = coalesce(CHECKUP2, CHECKUP1),
    pcp_no = coalesce(PERSDOC, PERSDOC3)
  ) %>%
  rename(
    LLCPWT = `_LLCPWT`,
    PSU = `_PSU`,
    STSTR = `_STSTR`,
    age = AGE,
    hisp = HISPANC3,
    edu = `_EDUCAG`,
    marital = MARITAL,
    children = `_CHLDCNT`,
    fphs = GENHLTH,
    mphd_poor = PHYSHLTH,
    mmhd_poor = MENTHLTH,
    ast_now = ASTHNOW,
    ovwob = `_RFBMI5`,
    smoke = `_SMOKER3`,
    vip = BLIND,
    dis_walk = DIFFWALK
  ) %>%
  dplyr::select(
    -c(
      SEX,
      SEX1,
      SEX2,
      BIRTHSEX,
      MRACASC1,
      MRACASC2,
      EMPLOY1,
      EMPLOY2,
      INCOM02,
      INCOM03,
      INCOME3,
      OWNHOME,
      RENTHOM1,
      ASTHEVE3,
      ASTHMA3,
      ANGINA,
      CVDCRHD4,
      HEART2,
      CVDINFR4,
      COPDEVER,
      CHCCOPD3,
      DEPRESS1,
      ADDEPEV3,
      DIABCOR3,
      DIABETE4,
      KIDNEY,
      CHCKDNY2,
      SKCANC,
      CHCSCNC1,
      EXERANY1,
      EXERANY2,
      DRNKANY5,
      DRNKANY6,
      `_RFBING5`,
      `_RFBING6`,
      AIDSTST8,
      AIDSTST9,
      HIVTST7,
      FLUSHOT6,
      FLUSHOT7,
      PNEUMVC3,
      PNEUMVC4,
      REMEM2,
      DECIDE,
      DIFFERND,
      DIFFALON,
      DIFDRES2,
      DIFFDRES,
      HAVEPLN3,
      `_HLTHPLN`,
      CHECKUP2,
      CHECKUP1,
      PERSDOC,
      PERSDOC1,
      PERSDOC3
    )
  )

# Handle missing value codes
ca_df <- ca_df %>%
  mutate(
    age = na_if(age, 7) %>% na_if(9),
    # Age: 7, 9 → NA; 77, 99 are valid
    children = na_if(children, 9) %>% na_if(77) %>% na_if(99),
    # Children: 7 is valid; 9, 77, 99 → NA
    employ = na_if(employ, 9) %>% na_if(77) %>% na_if(99),
    # Employ: 7 is valid; 9, 77, 99 → NA
    income = na_if(income, 77) %>% na_if(99),
    # Income: 7, 9 are valid; 77, 99 → NA
    mphd_poor = na_if(mphd_poor, 77) %>% na_if(99),
    # Health status: 7, 9 are valid; 77, 99 → NA
    mmhd_poor = na_if(mmhd_poor, 77) %>% na_if(99),
    ast_now = if_else(is.na(ast_now), 2, ast_now)              # NA → no
  ) %>%
  mutate(across(
    -c(
      LLCPWT,
      PSU,
      STSTR,
      YEAR,
      hisp,
      race,
      age,
      children,
      employ,
      income,
      mphd_poor,
      mmhd_poor
    ),
    ~ if_else(. %in% c(7, 77, 9, 99), NA_real_, .)  # Use if_else() to handle multiple NA values
  ))

ca_df <- ca_df %>%
  mutate(
    age = na_if(age, 7) %>% na_if(9),
    children = na_if(children, 9) %>% na_if(77) %>% na_if(99),
    employ = na_if(employ, 9) %>% na_if(77) %>% na_if(99),
    income = na_if(income, 77) %>% na_if(99),
    mphd_poor = na_if(mphd_poor, 77) %>% na_if(99),
    mmhd_poor = na_if(mmhd_poor, 77) %>% na_if(99),
    ast_now = case_when(is.na(ast_now) ~ 2, TRUE ~ ast_now)
  ) %>%
  mutate(across(
    -c(
      LLCPWT,
      PSU,
      STSTR,
      YEAR,
      hisp,
      race,
      # Exclude ID vars
      age,
      children,
      employ,
      income,
      mphd_poor,
      mmhd_poor
    ),
    # Exclude special cases
    ~ if_else(. %in% c(7, 77, 9, 99), NA_real_, .)
  ))

# Recategorize multi-category factor variables 
ca_df <- ca_df %>%
  mutate(across(
    -c(LLCPWT, PSU, STSTR, YEAR, age, mphd_poor, mmhd_poor, hisp, race),
    ~ factor(.)
  )) %>%
  mutate(
    # Ordered age category
    age_grp = factor(cut(
      age,
      breaks = c(14, 29, 44, 59, Inf),
      labels = c("15 to 29 years", "30 to 44 years", "45 to 59 years", "60 years and over"),
      right = TRUE
    ), ordered = TRUE),
    
    # Multi-category demographic variables
    sex = fct_recode(sex, "Male" = "1", "Female" = "2"),
    edu = fct_recode(
      edu,
      "Did not graduate high school" = "1",
      "Graduated high school" = "2",
      "Attended college or technical school" = "3",
      "Graduated from college or technical school" = "4"
    ),
    employ = fct_collapse(
      employ,
      "Employed" = c("1", "2"),
      "Unemployed" = c("3", "4"),
      "Not in labor force" = c("5", "6", "7", "8")
    ),
    income = fct_collapse(
      income,
      "Less than $25,000" = c("1", "2", "3", "4"),
      "$25,000-49,999" = c("5", "6"),
      "$50,000-99,999" = c("7", "8"),
      "$100,000 or more" = c("9", "10", "11")
    ),
    marital = fct_collapse(
      marital,
      "Married" = "1",
      "Divorced, widowed, or separated" = c("2", "3", "4"),
      "Never married" = c("5", "6")
    ),
    children = fct_collapse(
      children,
      "None" = "1",
      "One" = "2",
      "Two" = "3",
      "Three or more" = c("4", "5", "6")
    ),
    ownhome = fct_collapse(ownhome, "Yes" = "1", "No" = c("2", "3")),
    
    # Collapse levels in multi-level health variables
    smoke = fct_collapse(smoke, "Yes" = c("1", "2", "3"), "No" = "4"),
    chk2_no = fct_collapse(
      chk2_no,
      "Yes" = c("3", "4", "8", "88"),
      "No" = c("1", "2")
    ),
    pcp_no = fct_collapse(pcp_no, "Yes" = "3", "No" = c("1", "2")),
    fphs = fct_collapse(
      fphs,
      "Yes" = c("4", "5"),
      "No" = c("1", "2", "3")
    ),
    mphd_poor = factor(
      case_when(
        mphd_poor == 88 ~ "No",
        mphd_poor >= 15 ~ "Yes",
        mphd_poor < 15 ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("Yes", "No")
    ),
    mmhd_poor = factor(
      case_when(
        mmhd_poor == 88 ~ "No",
        mmhd_poor >= 15 ~ "Yes",
        mmhd_poor < 15 ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("Yes", "No")
    ),
    dm = fct_collapse(dm, "Yes" = "1", "No" = c("2", "3", "4"))
  )

# Recategorize binary variables
ca_df <- ca_df %>%
  mutate(# Standard yes/no binary variables (forcing Yes to be level 1)
    across(
      c(
        smoke,
        drink_any,
        chk2_no,
        pcp_no,
        fphs,
        mphd_poor,
        mmhd_poor,
        ast_lt,
        ast_now,
        apcvd,
        mi,
        copd,
        mdd,
        dm,
        ckd,
        bcc,
        vip,
        dis_conc,
        dis_err,
        dis_dress,
        dis_walk,
        hivtest_no,
        ins_no
      ),
      ~ factor(
        case_when(
          .x %in% c("Yes", "No") ~ .x,
          # Keep as is if already "Yes"/"No"
          .x == "1" ~ "Yes",
          .x == "2" ~ "No",
          TRUE ~ NA_character_  # Preserve NAs
        ),
        levels = c("Yes", "No")
      )
    ), # Reverse-coded yes/no binary variables (forcing Yes to be level 1)
    across(
      c(ovwob, drink_binge, exer_no, laiv_no, pcv_no),
      ~ factor(
        case_when(
          .x %in% c("Yes", "No") ~ .x,
          # Keep as is if already "Yes"/"No"
          .x == "2" ~ "Yes",
          .x == "1" ~ "No",
          TRUE ~ NA_character_  # Preserve NAs
        ),
        levels = c("Yes", "No")
      )
    ))


# Assign labels for gtsummary()
var_label(ca_df) <- list(
  sex = "Male sex",
  age_grp = "Age categories",
  edu = "Education level",
  income = "Income level",
  marital = "Marital status",
  children = "Number of children",
  employ = "Employment status",
  ownhome = "Homeowner",
  smoke = "Ever Smoked",
  drink_any = "Any alcohol use",
  drink_binge = "Binge drinking",
  exer_no = "No exercise in last 30 days",
  hivtest_no = "Never had HIV test",
  laiv_no = "Never had flu vaccine",
  pcv_no = "Never had pneumonia vaccine",
  chk2_no = "No routine checkup in last 2 years",
  pcp_no = "No primary care provider",
  fphs = "Fair or poor health status",
  mphd_poor = "Frequent poor physical health",
  mmhd_poor = "Frequent poor mental health",
  ovwob = "Overweight or obese",
  dm = "Diabetes diagnosis",
  copd = "COPD diagnosis",
  mdd = "Major depression diagnosis",
  ckd = "Chronic kidney disease",
  bcc = "Skin cancer diagnosis",
  mi = "Heart dttack",
  apcvd = "Angina or coronary heart disease",
  ast_lt = "Lifetime asthma",
  ast_now = "Current asthma",
  vip = "Vision impairment",
  dis_conc = "Cognitive difficulty",
  dis_err = "Difficulty running errands",
  dis_dress = "Difficulty dressing",
  dis_walk = "Difficulty walking"
)

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save cleaned dataset
saveRDS(ca_df, file.path(processed_data_dir, "ca_cleaned.rds"))
message("Saved cleaned BRFSS dataset: ",
        file.path(processed_data_dir, "ca_cleaned.rds"))

# End of script
message("02_clean_data.R completed successfully.")