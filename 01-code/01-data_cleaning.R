
### 1. SET WORKING DIRECTORY, LOAD PACKAGES ###
setwd("/Users/lamhine/Documents/GitHub/brfss_disagg/")

options(scipen = 999)
library(haven)
library(foreign)
library(survey)
library(srvyr)
library(labelled)
library(janitor)
library(data.table)
library(tidyverse)


### 2. LOAD AND MERGE 2014-2022 BRFSS MICRODATA ###
# initialize storage vector and get list of names of DTA files
ca_brfss <- list()
lf = list.files(path="/Users/lamhine/Library/CloudStorage/Box-Box/Tracy Lam-Hine's Files/brfss_disagg/ca_brfss_2000-22/2014-2022", 
                pattern=NULL, all.files=FALSE,
                full.names=TRUE)

# read in 2010-2017 and 2019, 2020 BRFSS DTA files into ca_brfss list
for (i in 1:9) {
  ca_brfss[i] <- list(read_sas(lf[i]))
  }

# fix mismatched element types for date to enable rbind
ca_brfss[[1]]$DATE <- mdy(ca_brfss[[1]]$DATE)
ca_brfss[[2]]$DATE <- mdy(ca_brfss[[2]]$DATE)
ca_brfss[[5]]$DATE <- mdy(ca_brfss[[5]]$DATE)
ca_brfss[[6]]$DATE <- mdy(ca_brfss[[6]]$DATE)
ca_brfss[[7]]$DATE <- mdy(ca_brfss[[7]]$DATE)
ca_brfss[[8]]$DATE <- mdy(ca_brfss[[8]]$DATE)

# bind all list elements into one df 
ca_bind <- rbindlist(ca_brfss, fill = T)


### 3. DATA CLEANING ###
# subset to variables needed
ca_df <- ca_bind %>% 
  dplyr::select(
    `_LLCPWT`, `_PSU`, `_STSTR`, YEAR,  #sampling vars
    AGE, EDUCA, # age, education
    EMPLOY2, EMPLOY1, # employment
    INCOM02, INCOM03, INCOME3, # income
    MARITAL, `_CHLDCNT`, # marital status, number of children
    OWNHOME, RENTHOM1, # own/rent home
    SEX, SEX1, SEX2, BIRTHSEX, # sex
    MRACASC1, MRACASC2, HISPANC3, # race/ethnicity
    DRNKANY5, DRNKANY6, # any drinks in last 30 days
    `_RFBING5`, `_RFBING6`, # binge drinking calculated
    ASTHEVE3, ASTHMA3, ASTHNOW, # lifetime/current asthma
    ANGINA, CVDCRHD4, # ever diagnosed with angina or CVD
    HEART2, CVDINFR4, # ever had heart attack
    COPDEVER, CHCCOPD3, # diagnosed with COPD/emphysema/chronic bronchitis
    DEPRESS1, ADDEPEV3, # diagnosed with depressive disorder
    DIABCOR3, DIABETE4, # ever diagnosed with diabetes or pre-diabetes
    KIDNEY, CHCKDNY2, # ever had trouble with kidney or told had kidney disease
    SKCANC, CHCSCNC1, # ever diagnosed with skin cancer
    HAVEPLN3, `_HLTHPLN`, # have any health insurance
    `_HCVU651`, `_HCVU652`, # under 65 with health insurance      
    CHECKUP2, CHECKUP1, # time since last routine checkup
    PERSDOC, PERSDOC3, # have primary care provider
    GENHLTH, PHYSHLTH, MENTHLTH, # overall health status
    AIDSTST8, AIDSTST9, HIVTST7, # HIV test history
    FLUSHOT6,FLUSHOT7, # influenza shot history
    PNEUMVC3,PNEUMVC4, # pneumonia vaccine history
    `_RFBMI5`, # overweight or obese calculated variable
    EXERANY1, EXERANY2, # physical activity in last 30 days other than job
    `_SMOKER3`, USENOW3, # smoker/current use of smokeless tobacco
    BLIND, # blind or difficulty seeing
    REMEM2, DECIDE, # difficulty concentrating or remembering
    DIFFERND, DIFFALON, # difficulty doing errands alone
    DIFDRES2, DIFFDRES, # difficulty dressing or bathing
    DIFFWALK # difficulty walking or climbing stairs
  )

# coalesce variables from the 2014-2021 and 2022 data sets and rename
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
    exercise = coalesce(EXERANY1, EXERANY2),
    drink_any = coalesce(DRNKANY5, DRNKANY6),
    drink_binge = coalesce(`_RFBING5`, `_RFBING6`),
    hiv_test = coalesce(AIDSTST8, AIDSTST9, HIVTST7),
    laiv = coalesce(FLUSHOT6,FLUSHOT7), 
    pcv = coalesce(PNEUMVC3,PNEUMVC4),
    dis_conc = coalesce(REMEM2, DECIDE),
    dis_err = coalesce(DIFFERND, DIFFALON),
    dis_dress = coalesce(DIFDRES2, DIFFDRES),
    ins_any = coalesce(HAVEPLN3, `_HLTHPLN`),
    ins_u65 = coalesce(`_HCVU651`, `_HCVU652`),
    pe_last = coalesce(CHECKUP2, CHECKUP1),
    pcp = coalesce(PERSDOC, PERSDOC3)) %>% 
  dplyr::select(-c(
    SEX, SEX1, SEX2, BIRTHSEX, MRACASC1, MRACASC2, EMPLOY1, EMPLOY2,
    INCOM02, INCOM03, INCOME3, OWNHOME, RENTHOM1, ASTHEVE3, ASTHMA3, ANGINA, 
    CVDCRHD4, HEART2, CVDINFR4, COPDEVER, CHCCOPD3, DEPRESS1, ADDEPEV3, 
    DIABCOR3, DIABETE4, KIDNEY, CHCKDNY2, SKCANC, CHCSCNC1, EXERANY1, EXERANY2,
    DRNKANY5, DRNKANY6, `_RFBING5`, `_RFBING6`, AIDSTST8, AIDSTST9, HIVTST7,
    FLUSHOT6, FLUSHOT7, PNEUMVC3, PNEUMVC4, REMEM2, DECIDE, DIFFERND, DIFFALON,
    DIFDRES2, DIFFDRES, HAVEPLN3, `_HLTHPLN`, `_HCVU651`, `_HCVU652`, CHECKUP2, 
    CHECKUP1, PERSDOC, PERSDOC3)) %>% 
  rename(
    LLCPWT = `_LLCPWT`,
    PSU = `_PSU`,
    STSTR = `_STSTR`,
    age = AGE,
    hisp = HISPANC3,
    edu = EDUCA,
    marital = MARITAL,
    children = `_CHLDCNT`,
    hlth_gen = GENHLTH,
    hlth_phys = PHYSHLTH,
    hlth_ment = MENTHLTH,
    ast_now = ASTHNOW,
    bmi_cat = `_RFBMI5`,
    smoke = `_SMOKER3`,
    smokeless = USENOW3,
    vip = BLIND,
    dis_walk = DIFFWALK
    ) 




### 4. RECODE VARIABLES ### 



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
ca_df <- ca_df %>% 
  mutate(
    hisp = as.numeric(hisp),
    hisp = sapply(hisp, reorder_digits)
  )

# Coerce MRACASC1 and HISPANC3 to character
ca_df <- ca_df %>% 
  mutate(
    race = as.character(race),
    hisp = as.character(hisp)
  )

# recode data entry inconsistencies ("44" when in combination and "5050")
ca_df <- ca_df %>% 
  mutate(
    race = case_when(
      race == "1044" ~ "1040",
      race == "4044" ~ "40",
      race == "4460" ~ "4060",
      race == "5050" ~ "50",
      TRUE ~ race)
  )

# categories for number of races 
ca_df <- ca_df %>% 
  mutate(
    race_ct = case_when(
      race %in% c("60", "77", "88", "99") ~ 0,
      is.na(race) ~ 0,
      str_length(race) > 2 ~ 2, 
      TRUE ~ 1)
    )

# categories for non-, single- and multiple-group hispanic participants
ca_df <- ca_df %>% 
  mutate(
    hisp_ct = case_when(
      hisp %in% c("5","7","77","9","99") ~ 0,
      hisp %in% c("1","2","3","4") ~ 1,
      TRUE ~ 2)
    )

# Create race_text variable 
ca_df <- ca_df %>% 
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

ca_df %>% group_by(race,race_text,race_ct) %>% summarize(n=n()) %>% View()

# Create hisp_text variable for detailed Hispanic ethnicity
ca_df <- ca_df %>% 
  mutate(
    hisp_text = hisp,
    hisp_text = str_replace_all(hisp_text, "1", "Mexican-"),
    hisp_text = str_replace_all(hisp_text, "2", "Puerto Rican-"),
    hisp_text = str_replace_all(hisp_text, "3", "Cuban-"),
    hisp_text = str_replace_all(hisp_text, "4", "Other Hispanic-"),
    hisp_text = str_replace_all(hisp_text, "5|7|77|9|99", "")
  )

ca_df %>% group_by(hisp,hisp_text,hisp_ct) %>% summarize(n=n()) %>% View()


# Create combined race-ethnicity variable, collapsing to highest group level 
# for multiracial and multiple Hispanic group participants 

ca_df %>% group_by(race,race_text,race_ct,hisp,hisp_text,hisp_ct) %>% summarize(n=n()) %>% View()

ca_df <- ca_df %>% 
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


ca_df %>% group_by(re_text) %>% summarize(n=n()) %>% View()

# Get dataframe of group counts and identify groups with n < 50
lt50_df <- ca_df %>% 
  group_by(re_text) %>% 
  summarize (n = n()) %>% 
  mutate(
    lt50 = 
      case_when(
        n < 50 ~ 1,
        TRUE ~ 0)) %>% 
  ungroup()


# ALTERNATE CODING FOR AI/AN AND NHPI FIRST
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
ca_df <- left_join(ca_df, lt50_df, by = "re_text") 

# remove trailing "-" characters
ca_df <- ca_df %>% 
  mutate(
    lt50_ra = case_when(
      str_sub(lt50_ra, -1) == "-" ~ str_sub(lt50_ra, 1, -2),
      TRUE ~ lt50_ra)
    )

# reorder group names in Multiracial groups to be alphabetical
ca_df <- ca_df %>% 
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
ca_df %>% 
  group_by(lt50_ra) %>% 
  summarize (n = n()) %>% View()

## Create summary group color labels
ca_df <- ca_df %>% 
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


ca_df %>% 
  group_by(re_col_lab, lt50_ra) %>% 
  summarize (n = n()) %>% View()

## RECODE OTHER VARIABLES ##

# Replace NAs in ast_now with "No" for those never diagnosed with asthma
ca_df <- ca_df %>% 
  mutate(
    ast_now = case_when(
      ast_lt == 2 ~ 2, 
      TRUE ~ ast_now
    ))

# Mutate 7, 77, 9, 99 in all relevant variables to NA
ca_df <- ca_df %>% 
  mutate(
    across(
      c(sex, marital, ownhome, hlth_gen, ast_lt, 
        ast_now, apcvd, mi, copd, 
        mdd, dm, ckd, bcc, bmi_cat, exercise, smoke, smokeless, 
        drink_any, drink_binge, hiv_test, laiv, pcv, vip, 
        dis_conc, dis_err, dis_dress, dis_walk, ins_any, ins_u65, 
        pe_last, pcp),
      ~if_else(. %in% c(7, 77, 9, 99), NA_real_, .)
      )
  )

ca_df <- ca_df %>% 
  mutate(edu = na_if(edu, 77),
         edu = na_if(edu, 9),
         edu = na_if(edu, 99),
         employ = na_if(employ, 77),
         employ = na_if(employ, 9),
         employ = na_if(employ, 99),
         income = na_if(income, 77), 
         income = na_if(income, 99),
         children = na_if(children, 9),
         across(c(hlth_ment, hlth_phys), ~if_else(. %in% c(77,88,99), NA_real_,.)))

# Mutate all relevant variables to factors
ca_df <- ca_df %>% 
  mutate(
    across(
      c(sex, edu, lt50_ra, employ, income, marital, 
        children, ownhome, hlth_gen, ast_lt, ast_now, apcvd, mi, copd, 
        mdd, dm, ckd, bcc, bmi_cat, exercise, smoke, smokeless, 
        drink_any, drink_binge, hiv_test, laiv, pcv, vip, 
        dis_conc, dis_err, dis_dress, dis_walk, ins_any, ins_u65, 
        pe_last, pcp),
      as_factor)
  ) 


# Collapse variable levels where needed
ca_df <- ca_df %>% 
  mutate(
    edu = fct_collapse(edu,
      "1" = c("1","2","4","88"),      # no HS diploma
      "2" = c("3","5"),               # high or technical school grad, GED
      "3" = "6",                      # some college
      "4" = "7",                      # college graduate
      "5" = "8"),                     # postgraduate
    employ = fct_collapse(employ,
      "1" = c("1","2"),               # employed
      "2" = c("3","4"),               # unemployed
      "3" = c("5","6","7","8")),      # not in labor force
    income = fct_collapse(income,
      "1" = c("1","2","3","4"),       # less than $25,000
      "2" = c("5","6"),               # less than $50,000
      "3" = c("7","8"),               # less than $100,000
      "4" = c("9","10","11")),        # over $100,000
    marital = fct_collapse(marital,
      "2" = c("2","3","4"),           # divorced, widowed, or separated
      "3" = c("5","6")),              # never married
    children = fct_collapse(children,
      "4" = c("4","5","6")),          # 3 or more children
    hlth_gen = fct_collapse(hlth_gen,
      "1" = c("4","5"),               # fair or poor health
      "2" = c("1","2","3")),          # excellent, very good, or good health
    dm = fct_collapse(dm,
      "1" = c("1","4"),               # yes
      "2" = c("2","3")),              # no
    smoke = fct_collapse(smoke,
      "1" = c("1","2","3"),           # smoker
      "2" = "4"),                     # never smoker
    smokeless = fct_collapse(smokeless,
      "1" = c("1","2"),               # yes
      "2" = "3"),                     # no
    pe_last = fct_collapse(pe_last,
      "1" = c("1","2"),               # yes 
      "2" = c("3","4","5","8","88")), # no
    pcp = fct_collapse(pcp,
      "1" = c("1","2"),               # yes
      "2" = "3")                      # no
    )

# Name levels of factors
ca_df <- ca_df %>% 
  mutate(
    sex = fct_recode(sex, "Male" = "1", "Female" = "2"),
    edu = fct_recode(edu, "No HS Diploma" = "1", "HS Diploma or GED" = "2",
                     "Some college" = "3", "College graduate" = "4", 
                     "Postgraduate" = "5"),
    employ = fct_recode(employ, "Employed" = "1", "Unemployed" = "2", 
                        "Not in labor force" = "3"),
    income = fct_recode(income, "Less than $25,000" = "1", 
                        "$25,000-49,999" = "2", "$50,000-99,999" = "3",
                        "$100,000 or more" = "4"),
    marital = fct_recode(marital, "Married" = "1", 
                         "Divorced, widowed, or separated" = "2", 
                         "Never married" = "3"),
    children = fct_recode(children, "None" = "1", "One" = "2", "Two" = "3", 
                          "Three or more" = "4"), 
    ownhome = fct_recode(ownhome, "Own" = "1", "Rent" = "2", "Other" = "3"),
    hlth_gen = fct_relevel(hlth_gen, "1"),
    hlth_gen = fct_recode(hlth_gen, "Fair or poor health" = "1", 
                          "Excellent, very good, or good health" = "2"),
    ast_lt = fct_recode(ast_lt, "Yes" = "1", "No" = "2"),
    ast_now = fct_recode(ast_now, "Yes" = "1", "No" = "2"),
    apcvd = fct_recode(apcvd, "Yes" = "1", "No" = "2"),
    mi = fct_recode(mi, "Yes" = "1", "No" = "2"),
    copd = fct_recode(copd, "Yes" = "1", "No" = "2"),
    mdd = fct_recode(mdd, "Yes" = "1", "No" = "2"),
    dm = fct_recode(dm, "Yes" = "1", "No" = "2"),
    ckd = fct_recode(ckd, "Yes" = "1", "No" = "2"),
    bcc = fct_recode(bcc, "Yes" = "1", "No" = "2"),
    bmi_cat = fct_relevel(bmi_cat, "2"),
    bmi_cat = fct_recode(bmi_cat, "Yes" = "2", "No" = "1"),
    exercise = fct_recode(exercise, "Yes" = "1", "No" = "2"),
    smoke = fct_recode(smoke, "Yes" = "1", "No" = "2"),
    smokeless = fct_recode(smokeless, "Yes" = "1", "No" = "2"),
    drink_any = fct_recode(drink_any, "Yes" = "1", "No" = "2"),
    drink_binge = fct_relevel(drink_binge, "2"),
    drink_binge = fct_recode(drink_binge, "Yes" = "2", "No" = "1"),
    hiv_test = fct_recode(hiv_test, "Yes" = "1", "No" = "2"),
    laiv = fct_recode(laiv, "Yes" = "1", "No" = "2"),
    pcv = fct_recode(pcv, "Yes" = "1", "No" = "2"),
    vip = fct_recode(vip, "Yes" = "1", "No" = "2"),
    dis_conc = fct_recode(dis_conc, "Yes" = "1", "No" = "2"),
    dis_err = fct_recode(dis_err, "Yes" = "1", "No" = "2"),
    dis_dress = fct_recode(dis_dress, "Yes" = "1", "No" = "2"),
    dis_walk = fct_recode(dis_walk, "Yes" = "1", "No" = "2"),
    ins_any = fct_recode(ins_any, "Yes" = "1", "No" = "2"),
    ins_u65 = fct_recode(ins_u65, "Yes" = "1", "No" = "2"),
    pe_last = fct_recode(pe_last, "Yes" = "1", "No" = "2"),
    pcp = fct_recode(pcp, "Yes" = "1", "No" = "2")
    )



















