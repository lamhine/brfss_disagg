### 1. SET WORKING DIRECTORY, LOAD PACKAGES ####################################
setwd("/Users/lamhine/Documents/GitHub/brfss_disagg/")
options(scipen = 999)

# Load required packages
library(haven)
library(foreign)
library(survey)
library(srvyr)
library(labelled)
library(janitor)
library(summarytools)
library(data.table)
library(tidyverse)
library(tidycensus)
library(mice)  

### 2. LOAD AND MERGE 2014-2022 BRFSS MICRODATA ################################
# Initialize storage vector and get list of names of DTA files
ca_brfss <- list()
lf <- list.files(path="/Users/lamhine/Library/CloudStorage/Box-Box/Tracy Lam-Hine's Files/brfss_disagg/CA-BRFSS/2014-2022", 
                 pattern=NULL, all.files=FALSE, full.names=TRUE)

# Read in BRFSS DTA files into ca_brfss list
for (i in 1:9) {
  ca_brfss[i] <- list(read_sas(lf[i]))
}

# Fix mismatched element types for date
for (i in c(1, 2, 5, 6, 7, 8)) {
  ca_brfss[[i]]$DATE <- mdy(ca_brfss[[i]]$DATE)
}

# Bind all list elements into one df
ca_bind <- rbindlist(ca_brfss, fill = TRUE) %>% as.data.frame()

### 3. SELECT AND RENAME VARIABLES #############################################
# Subset to variables needed
ca_df <- ca_bind %>% 
  dplyr::select(
    `_LLCPWT`, `_PSU`, `_STSTR`, YEAR,  # Sampling vars
    AGE, `_EDUCAG`, EMPLOY2, EMPLOY1, INCOM02, INCOM03, INCOME3,  # Demographics
    MARITAL, `_CHLDCNT`, OWNHOME, RENTHOM1, SEX, SEX1, SEX2, BIRTHSEX,
    MRACASC1, MRACASC2, HISPANC3,  # Keep original race/ethnicity variables
    DRNKANY5, DRNKANY6, `_RFBING5`, `_RFBING6`,  # Alcohol use
    ASTHEVE3, ASTHMA3, ASTHNOW, ANGINA, CVDCRHD4, HEART2, CVDINFR4,  # Health conditions
    COPDEVER, CHCCOPD3, DEPRESS1, ADDEPEV3, DIABCOR3, DIABETE4, KIDNEY, CHCKDNY2,
    SKCANC, CHCSCNC1, HAVEPLN3, `_HLTHPLN`, CHECKUP2, CHECKUP1,
    PERSDOC, PERSDOC1, PERSDOC3, GENHLTH, PHYSHLTH, MENTHLTH,  # Health status
    AIDSTST8, AIDSTST9, HIVTST7, FLUSHOT6, FLUSHOT7, PNEUMVC3, PNEUMVC4,  # Vaccines
    `_RFBMI5`, EXERANY1, EXERANY2, `_SMOKER3`, BLIND, REMEM2, DECIDE,
    DIFFERND, DIFFALON, DIFDRES2, DIFFDRES, DIFFWALK  # Disability & risk factors
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
    laiv_no = coalesce(FLUSHOT6,FLUSHOT7), 
    pcv_no = coalesce(PNEUMVC3,PNEUMVC4),
    dis_conc = coalesce(REMEM2, DECIDE),
    dis_err = coalesce(DIFFERND, DIFFALON),
    dis_dress = coalesce(DIFDRES2, DIFFDRES),
    ins_no = coalesce(HAVEPLN3, `_HLTHPLN`),
    chk2_no = coalesce(CHECKUP2, CHECKUP1),
    pcp_no = coalesce(PERSDOC, PERSDOC3)
  ) %>% 
  rename(
    LLCPWT = `_LLCPWT`, PSU = `_PSU`, STSTR = `_STSTR`, age = AGE, 
    hisp = HISPANC3, edu = `_EDUCAG`, marital = MARITAL, children = `_CHLDCNT`,
    fphs = GENHLTH, mphd_poor = PHYSHLTH, mmhd_poor = MENTHLTH, 
    ast_now = ASTHNOW, ovwob = `_RFBMI5`, smoke = `_SMOKER3`, 
    vip = BLIND, dis_walk = DIFFWALK
  ) %>% 
  dplyr::select(-c(
    SEX, SEX1, SEX2, BIRTHSEX, MRACASC1, MRACASC2, EMPLOY1, EMPLOY2,
    INCOM02, INCOM03, INCOME3, OWNHOME, RENTHOM1, ASTHEVE3, ASTHMA3, ANGINA, 
    CVDCRHD4, HEART2, CVDINFR4, COPDEVER, CHCCOPD3, DEPRESS1, ADDEPEV3, 
    DIABCOR3, DIABETE4, KIDNEY, CHCKDNY2, SKCANC, CHCSCNC1, EXERANY1, EXERANY2,
    DRNKANY5, DRNKANY6, `_RFBING5`, `_RFBING6`, AIDSTST8, AIDSTST9, HIVTST7,
    FLUSHOT6, FLUSHOT7, PNEUMVC3, PNEUMVC4, REMEM2, DECIDE, DIFFERND, DIFFALON,
    DIFDRES2, DIFFDRES, HAVEPLN3, `_HLTHPLN`, CHECKUP2, 
    CHECKUP1, PERSDOC, PERSDOC1, PERSDOC3))


# Get an overview of the data frame
#dfSummary(ca_df)


### 4. HANDLE MISSING VALUES WITH MULTIPLE IMPUTATION ##########################
# Replace 7, 77, 9, 99 with NA except special case variables
ca_df <- ca_df %>%
  mutate(
    age = na_if(age, 7) %>% na_if(9),                          # Age: 7, 9 → NA; 77, 99 are valid
    children = na_if(children, 9) %>% na_if(77) %>% na_if(99), # Children: 7 is valid; 9, 77, 99 → NA
    employ = na_if(employ, 9) %>% na_if(77) %>% na_if(99),     # Employ: 7 is valid; 9, 77, 99 → NA
    income = na_if(income, 77) %>% na_if(99),                  # Income: 7, 9 are valid; 77, 99 → NA
    mphd_poor = na_if(mphd_poor, 77) %>% na_if(99),            # Health status: 7, 9 are valid; 77, 99 → NA 
    mmhd_poor = na_if(mmhd_poor, 77) %>% na_if(99),
    ast_now = case_when(is.na(ast_now) ~ 2, TRUE ~ ast_now)    # NA's are no for ast_now
  ) %>%
  mutate(across(
    -c(LLCPWT, PSU, STSTR, YEAR, hisp, race, # Exclude ID vars
       age, children, employ, income, mphd_poor, mmhd_poor),  # Exclude special cases
    ~ if_else(. %in% c(7, 77, 9, 99), NA_real_, .)
  ))


# Copy dataset and define imputation variables
ca_imp <- ca_df  

# Run multiple imputation; LLCPWT is a predictor but not imputed
imp <- mice(
  ca_imp, 
  method = "pmm", 
  m = 10, 
  maxit = 10, 
  seed = 123, 
  predictorMatrix = {
    pred_matrix <- quickpred(ca_imp, exclude = c("PSU", "STSTR", "YEAR", "race", "hisp"))
    pred_matrix["LLCPWT", ] <- 0  # Do not impute LLCPWT
    pred_matrix[, "LLCPWT"] <- 1  # Use LLCPWT as a predictor
    pred_matrix  # Directly return the matrix
  }
)

# Replace missing values with imputed data
ca_imp <- complete(imp)


### 4. CONVERT VARIABLES TO FACTOR AND PROCESS LEVELS ##########################
# Convert all variables to factor except special case variables
ca_imp <- ca_imp %>% 
  mutate(across(
    -c(LLCPWT, PSU, STSTR, YEAR, age, mphd_poor, mmhd_poor, hisp, race), 
    ~ factor(.)
  ))

# Collapse, reorder, and rename levels of multi-level categorical variables
ca_imp <- ca_imp %>% 
  mutate(
    sex = fct_recode(sex,
      "Male" = "1",
      "Female" = "2"
    ),
    edu = fct_recode(edu,
      "Did not graduate high school" = "1",
      "Graduated high school" = "2",
      "Attended college or technical school" = "3",
      "Graduated from college or technical school" = "4"
      ),
    employ = fct_collapse(employ,
                          "Employed" = c("1","2"),
                          "Unemployed" = c("3","4"),
                          "Not in labor force" = c("5","6","7","8")
                          ),
    income = fct_collapse(income,
                          "Less than $25,000" = c("1","2","3","4"),
                          "$25,000-49,999" = c("5","6"),
                          "$50,000-99,999" = c("7","8"),
                          "$100,000 or more" = c("9","10","11")
                          ),
    marital = fct_collapse(marital,
                           "Married" = "1",
                           "Divorced, widowed, or separated" = c("2","3","4"),
                           "Never married" = c("5","6")
                           ),
    children = fct_collapse(children,
                            "None" = "1", 
                            "One" = "2", 
                            "Two" = "3", 
                            "Three or more" = c("4", "5", "6")),
    ownhome = fct_collapse(ownhome,
                           "Owns home" = "1", 
                           "Does not own home" = c("2", "3"))
    )

# Dichotomize variables for prevalence estimates
ca_imp <- ca_imp %>% 
  mutate(
    ovwob = fct_recode(
      fct_relevel(ovwob, "1", "2"),
      "Yes" = "2",  # BMI > 25.0
      "No" = "1"    # BMI ≤ 25.0
    ),
    smoke = fct_recode(
      fct_collapse(smoke, "1" = c("1","2","3"), "2" = "4"),
      "Yes" = "1",  # Ever smoked
      "No" = "2"    # Never smoked
    ),
    dm = fct_recode(
      fct_collapse(dm, "1" = "1", "2" = c("2", "3", "4")),
      "Yes" = "1",  # Diabetes 
      "No" = "2"    # No diabetes
    ),
    drink_binge = fct_recode(
      fct_relevel(drink_binge, "1", "2"),
      "Yes" = "2",  # Binge drinking
      "No" = "1"    # No binge drinking
    ),
    ins_no = fct_recode(
      fct_relevel(ins_no, "1", "2"),
      "Yes" = "2",  # Uninsured 
      "No" = "1"    # Insured
    ),
    chk2_no = fct_recode(
      fct_collapse(chk2_no, "1" = c("3", "4", "8", "88"), "2" = c("1", "2")),
      "Yes" = "1",  # No checkup in past two years           
      "No" = "2"    # Checkup in past two years
    ),
    pcp_no = fct_recode(
      fct_collapse(pcp_no, "1" = "3", "2" = c("1", "2")),
      "Yes" = "1",  # No primary care provider
      "No" = "2"    # Has primary care provider
    ),
    fphs = fct_recode(
      fct_collapse(fphs, "1" = c("4", "5"), "2" = c("1", "2", "3")),
      "Yes" = "1",  # Fair or poor health
      "No" = "2"    # Good, very good, or excellent health
    ),
    mphd_poor = factor(
      case_when(
        mphd_poor == 88 ~ "No",        # 88 means 0 days of poor health
        mphd_poor >= 15 ~ "Yes",       # 15+ days = poor health
        mphd_poor < 15 ~ "No",         # 0-14 days = good health
        TRUE ~ as.character(mphd_poor)
      ),
      levels = c("Yes", "No")
      ),
    mmhd_poor = factor(
      case_when(
        mmhd_poor == 88 ~ "No",
        mmhd_poor >= 15 ~ "Yes",
        mmhd_poor < 15 ~ "No",
        TRUE ~ as.character(mmhd_poor)
      ),
      levels = c("Yes", "No")
      ),
    # Rename levels in remaining binary variables
    ast_lt = fct_recode(ast_lt, "Yes" = "1", "No" = "2"),             
    ast_now = fct_recode(ast_now, "Yes" = "1", "No" = "2"),           
    apcvd = fct_recode(apcvd, "Yes" = "1", "No" = "2"),               
    mi = fct_recode(mi, "Yes" = "1", "No" = "2"),                     
    copd = fct_recode(copd, "Yes" = "1", "No" = "2"),                 
    mdd = fct_recode(mdd, "Yes" = "1", "No" = "2"),                     
    ckd = fct_recode(ckd, "Yes" = "1", "No" = "2"),                   
    bcc = fct_recode(bcc, "Yes" = "1", "No" = "2"),                   
    exer_no = fct_recode(exer_no, "Yes" = "2", "No" = "1"),           
    drink_any = fct_recode(drink_any, "Yes" = "1", "No" = "2"),       
    hivtest_no = fct_recode(hivtest_no, "Yes" = "2", "No" = "1"),     
    laiv_no = fct_recode(laiv_no, "Yes" = "2", "No" = "1"),           
    pcv_no = fct_recode(pcv_no, "Yes" = "2", "No" = "1"),             
    vip = fct_recode(vip, "Yes" = "1", "No" = "2"),                   
    dis_conc = fct_recode(dis_conc, "Yes" = "1", "No" = "2"),         
    dis_err = fct_recode(dis_err, "Yes" = "1", "No" = "2"),           
    dis_dress = fct_recode(dis_dress, "Yes" = "1", "No" = "2"),       
    dis_walk = fct_recode(dis_walk, "Yes" = "1", "No" = "2")
  )

# Ensure binary variables are correctly ordered (1 = Yes, 2 = No)
ca_imp <- ca_imp %>% 
  mutate(across(
    c(ast_now, fphs, mphd_poor, mmhd_poor, ovwob, smoke, vip, dis_walk, ast_lt, 
      apcvd, mi, copd, mdd, dm, ckd, bcc, exer_no, drink_any, drink_binge, 
      mphd_poor, mmhd_poor, hivtest_no, laiv_no, pcv_no, dis_conc, dis_err, 
      dis_dress, ins_no, chk2_no, pcp_no),  
    ~ fct_relevel(.x, "Yes", "No")
  ))

# create age category variable
ca_imp <- ca_imp %>%
  mutate(age_grp = case_when(
    age >= 15 & age <= 29 ~ "15 to 29 years",
    age >= 30 & age <= 44 ~ "30 to 44 years",
    age >= 45 & age <= 59 ~ "45 to 59 years",
    age >= 60 ~ "60 years and over",
    TRUE ~ NA_character_
  )) %>%
  mutate(age_grp = factor(age_grp, levels = c("15 to 29 years", "30 to 44 years", "45 to 59 years", "60 years and over")))


# Get an overview of the data frame
dfSummary(ca_imp)

## RECODE RACE AND ETHNICITY VARIABLES ## 

# write function to reorder the digits of a number (from ChatGPT)
reorder_digits <- function(num) {
  if (num < 10) {
    num  # If the number has only one digit, return it
  } else {
    digits <- as.numeric(strsplit(as.character(num), '')[[1]])  # Split the number into digits
    sorted_digits <- sort(digits)  # Sort the digits
    as.numeric(paste(sorted_digits, collapse = ''))  # Concatenate the sorted digits
  }
}

# recode hispanic variable to group together digit permutations
ca_imp <- ca_imp %>% 
  mutate(
    hisp = as.numeric(hisp),
    hisp = sapply(hisp, reorder_digits)
  )

# Coerce MRACASC1 and HISPANC3 to character
ca_imp <- ca_imp %>% 
  mutate(
    race = as.character(race),
    hisp = as.character(hisp)
  )

# recode data entry inconsistencies ("44" when in combination and "5050")
ca_imp <- ca_imp %>% 
  mutate(
    race = case_when(
      race == "1044" ~ "1040",
      race == "4044" ~ "40",
      race == "4460" ~ "4060",
      race == "5050" ~ "50",
      TRUE ~ race)
  )

# categories for number of races 
ca_imp <- ca_imp %>% 
  mutate(
    race_ct = case_when(
      race %in% c("60", "77", "88", "99") ~ 0,
      is.na(race) ~ 0,
      str_length(race) > 2 ~ 2, 
      TRUE ~ 1)
  )

# categories for non-, single- and multiple-group hispanic participants
ca_imp <- ca_imp %>% 
  mutate(
    hisp_ct = case_when(
      hisp %in% c("5","7","77","9","99") ~ 0,
      hisp %in% c("1","2","3","4") ~ 1,
      TRUE ~ 2)
  )

# Create race_text variable 
ca_imp <- ca_imp %>% 
  mutate(
    race_text = race,
    race_text = str_replace_all(race_text, "10", "White-"),
    race_text = str_replace_all(race_text, "20", "Black-"),
    race_text = str_replace_all(race_text, "30", "AIAN-"),
    race_text = str_replace_all(race_text, "40", "Asian-"),
    race_text = str_replace_all(race_text, "41", "Asian Indian-"),
    race_text = str_replace_all(race_text, "42", "Chinese-"),
    race_text = str_replace_all(race_text, "43", "Filipino-"),
    race_text = str_replace_all(race_text, "44", "Japanese-"),
    race_text = str_replace_all(race_text, "45", "Korean-"),
    race_text = str_replace_all(race_text, "46", "Vietnamese-"),
    race_text = str_replace_all(race_text, "47", "Other Asian-"),
    race_text = str_replace_all(race_text, "50", "Pacific Islander-"),
    race_text = str_replace_all(race_text, "51", "Native Hawaiian-"),
    race_text = str_replace_all(race_text, "52", "Guamanian-"),
    race_text = str_replace_all(race_text, "53", "Samoan-"),
    race_text = str_replace_all(race_text, "54", "Other Pacific Islander-"),
    race_text = str_replace_all(race_text, "60", "Other Race-"),
    race_text = str_replace_all(race_text, "77|88|99", "DK/R-"),
    race_text = str_replace_na(race_text, "DK/R-"),
    race_text = case_when(
      str_detect(race_text, "Other Race-|DK/R-") & hisp_ct %in% c(1,2) ~ str_remove(race_text, "Other Race-|DK/R-"),
      TRUE ~ race_text
    )
  ) 

# ca_imp %>% group_by(race,race_text,race_ct) %>% summarize(n=n()) %>% View()

# Create hisp_text variable for detailed Hispanic ethnicity
ca_imp <- ca_imp %>% 
  mutate(
    hisp_text = hisp,
    hisp_text = str_replace_all(hisp_text, "1", "Mexican-"),
    hisp_text = str_replace_all(hisp_text, "2", "Puerto Rican-"),
    hisp_text = str_replace_all(hisp_text, "3", "Cuban-"),
    hisp_text = str_replace_all(hisp_text, "4", "Other Hispanic-"),
    hisp_text = str_replace_all(hisp_text, "5|7|77|9|99", "")
  )

# ca_imp %>% group_by(hisp,hisp_text,hisp_ct) %>% summarize(n=n()) %>% View()


# Create combined race-ethnicity variable, collapsing to highest group level 
# for multiracial and multiple Hispanic group participants 

#ca_imp %>% group_by(race,race_text,race_ct,hisp,hisp_text,hisp_ct) %>% summarize(n=n()) %>% View()

ca_imp <- ca_imp %>% 
  mutate(
    re_text_full = paste0(race_text, hisp_text),
    re_text = case_when(
      race_ct == 0 & hisp_ct == 0 ~ paste0(race_text, hisp_text),
      race_ct == 0 & hisp_ct == 1 ~ paste0(race_text, hisp_text),
      race_ct == 0 & hisp_ct == 2 ~ "Multiple Hispanic-",
      race_ct == 1 & hisp_ct == 0 ~ paste0(race_text, hisp_text),
      race_ct == 1 & race_text == "White-" & hisp_ct %in% c(1,2) ~ paste0("Hispanic-", "White-"),
      race_ct == 1 & race_text == "Black-" & hisp_ct %in% c(1,2) ~ paste0("Black-", "Hispanic-"),
      race_ct == 1 & race_text == "AIAN-" & hisp_ct %in% c(1,2) ~ paste0("AIAN-", "Hispanic-"),
      race_ct == 1 & race_text %in% c("Asian-", "Asian Indian-", "Chinese-", "Filipino-", "Japanese-", "Korean-", "Vietnamese-", "Other Asian-") & hisp_ct %in% c(1,2) ~ paste0("Asian-", "Hispanic-"),
      race_ct == 1 & race_text %in% c("Pacific Islander-", "Native Hawaiian-", "Guamanian-", "Samoan-", "Other Pacific Islander-") & hisp_ct %in% c(1,2) ~ paste0("Hispanic-", "Pacific Islander-"),
      race_ct == 2 & hisp_ct == 0 ~ paste0(race_text, hisp_text),
      race_ct == 2 & hisp_ct %in% c(1,2) ~ paste0(race_text, "Hispanic-")
    )
  )


# ca_imp %>% group_by(re_text) %>% summarize(n=n()) %>% View()

# Get dataframe of group counts and identify groups with n < 50
lt50_df <- ca_imp %>% 
  group_by(re_text) %>% 
  summarize (n = n()) %>% 
  mutate(
    lt50 = 
      case_when(
        n < 50 ~ 1,
        TRUE ~ 0)) %>% 
  ungroup()


# CODING FOR AI/AN AND NHPI FIRST
lt50_df <- lt50_df %>% 
  mutate(
    lt50_ra = 
      case_when(
        re_text == "Native Hawaiian-" ~ "Pacific Islander-",
        re_text == "Guamanian-" ~ "Pacific Islander-",
        re_text == "Cuban-" ~ "Other Hispanic-",
        lt50 == 1 & str_detect(re_text, "AIAN") ~ "AIAN-Multiple-",
        lt50 == 1 & str_detect(re_text, "Pacific Islander") ~ "Pacific Islander-Multiple-", 
        lt50 == 1 & str_detect(re_text, "Black") ~ "Black-Multiple-",
        lt50 == 1 & str_detect(re_text, "Asian") ~ "Asian-Multiple",
        #lt50 == 1 & str_detect(re_text, "Hispanic") ~ "Hispanic-Multiple-",
        TRUE ~ re_text)
  ) %>% 
  select(-c("n", "lt50"))

# join in new lt50_ra2 variable
ca_imp <- left_join(ca_imp, lt50_df, by = "re_text") 

# remove trailing "-" characters
ca_imp <- ca_imp %>% 
  mutate(
    lt50_ra = case_when(
      str_sub(lt50_ra, -1) == "-" ~ str_sub(lt50_ra, 1, -2),
      TRUE ~ lt50_ra)
  )

# reorder group names in Multiracial groups to be alphabetical
ca_imp <- ca_imp %>% 
  mutate(
    lt50_ra = case_when(
      lt50_ra == "Asian" ~ "Unspecified Asian",
      lt50_ra == "White-AIAN-Hispanic" ~ "AIAN-Hispanic-White",
      lt50_ra == "White-Black-Hispanic" ~ "Black-Hispanic-White",
      lt50_ra == "White-Hispanic" ~ "Hispanic-White",
      lt50_ra == "Black-AIAN" ~ "AIAN-Black",
      lt50_ra == "Black-Asian" ~ "Asian-Black",
      lt50_ra == "White-AIAN" ~ "AIAN-White",
      lt50_ra == "White-Asian" ~ "Asian-White",
      lt50_ra == "White-Black" ~ "Black-White",
      lt50_ra == "White-Black-AIAN" ~ "AIAN-Black-White",
      lt50_ra == "White-Other Race" ~ "Other Race-White",
      lt50_ra == "White-Pacific Islander" ~ "Pacific Islander-White",
      lt50_ra == "White-Black-Hispanic" ~ "Black-Hispanic-White",
      lt50_ra == "Other Race" ~ "Unknown Race",
      lt50_ra == "DK/R" ~ "Unknown Race",
      TRUE ~ lt50_ra)
  )

# check group counts
ca_imp %>% 
  group_by(lt50_ra) %>% 
  summarize (n = n()) %>% View()

## Create summary group color labels
ca_imp <- ca_imp %>% 
  mutate(
    re_col_lab = case_when(
      str_detect(lt50_ra, "Mexican|Puerto Rican|Cuban|Hispanic") ~ "Hispanic",
      lt50_ra == "White" ~ "White",
      lt50_ra == "Black" ~ "Black",
      lt50_ra == "AIAN" ~ "AIAN",
      lt50_ra %in% c("Unspecified Asian", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian") ~ "Asian", 
      lt50_ra %in% c("Pacific Islander", "Native Hawaiian", "Guamanian", "Samoan", "Other Pacific Islander") ~ "NHPI",
      lt50_ra == "Unknown Race" ~ "Unknown Race",
      TRUE ~ "Multiracial"
    )
  )


ca_imp %>% 
  group_by(re_col_lab, lt50_ra) %>% 
  summarize (n = n()) %>% View()

### 8. CREATE COMPLEX SURVEY DESIGN #############################################

# Define total California population ages 18+ (2020)
total_population <- 23455020  

# Compute sample size proportions per year
samp_size <- ca_imp %>%
  group_by(YEAR) %>%
  summarize(samp_size = n() / nrow(ca_imp))

# Join sample size proportions to dataset
ca_imp <- left_join(ca_imp, samp_size, by = "YEAR")

# Normalize final weights
ca_imp <- ca_imp %>%
  mutate(final_wt = LLCPWT / length(unique(ca_imp$YEAR))) %>%  # Adjust for number of years
  mutate(final_wt = final_wt / sum(final_wt, na.rm = TRUE) * total_population)  # Normalize to CA population

# Add a constant "one" variable for weighted total check
ca_imp <- ca_imp %>%
  mutate(one = 1)

# Convert `lt50_ra` to factor for survey functions
ca_imp <- ca_imp %>% mutate(lt50_ra = as.factor(lt50_ra))

# Create survey design object (BRFSS uses stratified design)
options(survey.lonely.psu = "adjust")  # Adjust single PSU strata
ca_dsn <- svydesign(
  id = ~1,            # No PSU clustering in BRFSS (use id = ~1)
  strata = ~STSTR,    # Use survey strata
  weights = ~final_wt, # Corrected weights
  data = ca_imp
)

# Convert to `srvyr` format for easier manipulation
ca_dsn <- as_survey_design(ca_dsn)


### 9. VALIDATION CHECKS #######################################################

# **Confirm final weights exist and are correctly scaled**
summary(ca_imp$final_wt)

# **Check total weighted sample size (should match CA population 18+ ~23.5M)**
svytotal(~one, design = ca_dsn)  

# **Check weighted prevalence of smoking (should sum to ~1)**
svymean(~smoke, design = ca_dsn, na.rm = TRUE)

# **Verify total weighted count for health insurance (should be reasonable)**
svytotal(~ins_no, design = ca_dsn)

# **Check full survey design object**
summary(ca_dsn)

# **Check factor levels after collapsing**
table(ca_imp$smoke)
levels(ca_imp$smoke)


