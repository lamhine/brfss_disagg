### 1. SET WORKING DIRECTORY, LOAD PACKAGES ###
setwd("/Users/lamhine/Documents/GitHub/brfss_disagg/")
library(tidyverse)
library(haven)
library(gtsummary)
library(labelled)
library(boxr)
library(data.table)
library(foreign)
library(survey)
library(srvyr)
library(ggrepel)


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

# create table 1 
table1 <- ca_df %>% 
  tbl_summary(
    include = c(
      age, sex, race, hisp, 
      edu, employ, income, marital, children, ownhome, 
      hlth_gen, hlth_phys, hlth_ment, 
      ast_lt, ast_now, apcvd, mi, copd, mdd, dm, ckd, bcc, 
      bmi_cat, exercise, smoke, smokeless, drink_any, drink_binge, 
      hiv_test, laiv, pcv, vip, 
      dis_conc, dis_err, dis_dress, dis_walk, 
      ins_any, ins_u65, pe_last, pcp),  
    by = YEAR,
    missing = "ifany") 

table1




### 4. RECODE VARIABLES ### 






## RECODE DEMOGRAPHIC VARIABLES ## 
 
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
    MRACASC1 = as.character(MRACASC1),
    HISPANIC = as.character(HISPANIC)
  )

# recode "44" as "40" when in combination with other races
ca_df <- ca_df %>% 
  mutate(
    MRACASC1 = case_when(
      MRACASC1 == "1044" ~ "1040",
      MRACASC1 == "4044" ~ "40",
      MRACASC1 == "4460" ~ "4060",
      TRUE ~ MRACASC1)
  )

# Create race text variable  
ca_df <- ca_df %>% 
  mutate(
    race_text = case_when(
      MRACASC1 == "10" ~ "White-",
      MRACASC1 == "20" ~ "Black-",
      MRACASC1 == "30" ~ "AIAN-",
      MRACASC1 == "40" ~ "Asian-",
      MRACASC1 == "41" ~ "Asian Indian-",
      MRACASC1 == "42" ~ "Chinese-",
      MRACASC1 == "43" ~ "Filipino-",
      MRACASC1 == "44" ~ "Japanese-",
      MRACASC1 == "45" ~ "Korean-",
      MRACASC1 == "46" ~ "Vietnamese-",
      MRACASC1 == "47" ~ "Other Asian-",
      MRACASC1 == "50" ~ "Pacific Islander-",
      MRACASC1 == "51" ~ "Native Hawaiian-",
      MRACASC1 == "52" ~ "Guamanian-",
      MRACASC1 == "53" ~ "Samoan-",
      MRACASC1 == "54" ~ "Other Pacific Islander-",
      MRACASC1 == "60" ~ "OtherRace-",
      is.na(MRACASC1) ~ "DK/O/R-",
      MRACASC1 %in% c("77", "88", "99") ~ "DK/O/R-",
      TRUE ~ MRACASC1),
    race_text = str_replace_all(race_text, "10", "White-"),
    race_text = str_replace_all(race_text, "20", "Black-"),
    race_text = str_replace_all(race_text, "30", "AIAN-"),
    race_text = str_replace_all(race_text, "40", "Asian-"),
    race_text = str_replace_all(race_text, "50", "Pacific Islander-"),
    race_text = str_replace_all(race_text, "54", "Other Pacific Islander-"),
    race_text = str_replace_all(race_text, "60", "OtherRace-")
  )

ca_df <- ca_df %>% 
  mutate(
    hisp_text = case_when(
      HISPANIC %in% c("5", "7", "77", "9", "99") ~ "",
      TRUE ~ HISPANIC),
    hisp_text = str_replace_all(hisp_text, "1", "Mexican-"),
    hisp_text = str_replace_all(hisp_text, "2", "Puerto Rican-"),
    hisp_text = str_replace_all(hisp_text, "3", "Cuban-"),
    hisp_text = str_replace_all(hisp_text, "4", "Other Hispanic-"), 
    hisp_text = str_replace_all(hisp_text, "5", ""),
    hisp_text = str_replace_all(hisp_text, "4", "Other Hispanic-")
    )

ca_df <- ca_df %>% 
  mutate(race_eth_text = paste0(race_text,hisp_text)) %>% 
  dplyr::select(-c("race_text", "hisp_text"))


# Remove trailing "-" characters
ca_df <- ca_df %>% 
  mutate(
    race_eth_text = case_when(
      str_sub(race_eth_text, -1) == "-" ~ str_sub(race_eth_text, 1, -2),
      TRUE ~ race_eth_text
    )
  )

table(ca_df$race_eth_text, ca_df$YEAR, exclude = NULL)

## Create summary group color labels
ca_df <- ca_df %>% 
  mutate(
    race_eth_col_lab = case_when(
      !HISPANIC %in% c("5", "7", "77", "9", "99") ~ "Hispanic",
      MRACASC1 == "10" ~ "White",
      MRACASC1 == "20" ~ "Black",
      MRACASC1 == "30" ~ "AIAN",
      MRACASC1 %in% c("40", "41", "42", "43", "44", "45", "46", "47") ~ "Asian", 
      MRACASC1 %in% c("50", "51", "52", "53", "54") ~ "NHPI",
      is.na(MRACASC1) ~ "DK/O/R",
      MRACASC1 %in% c("60", "77", "88", "99") ~ "DK/O/R",
      TRUE ~ "Multiracial"
    )
  )


## 4. CALCULATE WEIGHTED PREVALENCE BY GROUP ##  

# new column with each year's proportional contribution to total sample size 
samp_size <- ca_df %>% group_by(YEAR) %>% summarize(samp_size = n()/nrow(ca_df))
ca_df <- left_join(ca_df, samp_size, by = "YEAR")

# divide LLCPWT by sample size to calculate final weight variable 
ca_df <- ca_df %>% mutate(final_wt = LLCPWT/samp_size) %>% select(-samp_size)

# Create complex survey design 
ca_dsn <- ca_df %>% 
  as_survey_design(id = 1, strata = STSTR, weights = final_wt)


# calculate prevalence by group, remove Other/DK/Refused & groups with n<30  

#brfss_dsn %>% 
#  group_by(race_eth_text, race_eth_col_lab) %>% 
#  summarize(asthma = survey_mean(ATHEVE3, na.rm = T, vartype = "ci", level = 0.95, proportion = T, prop_method = "logit", df = T)*100)


asthma_prevs <- ca_dsn_ast %>% 
  group_by(race_eth_text, race_eth_col_lab) %>% 
  summarize(asthma = survey_mean(ASTHEVE3, na.rm = T, vartype = "ci", level = 0.95, proportion = T, prop_method = "logit", df = T)*100, 
            n = n()) %>% 
  #filter(n >= 50, 
  #       race_eth_col_lab != "DK/Other/Refused") %>% 
  select(-n) 

ggplot(asthma_prevs, aes(x=race_eth_col_lab, y=asthma))+
  geom_pointrange(aes(ymin=asthma_low, ymax=asthma_upp, color = hisp_hypo_text),
                  position=position_dodge(width = 0.5)) + 
  geom_text_repel(
    aes(label = hisp_hypo_text),
    color = "black") +
  xlab("") +
  ylab("Prevalence") +
  theme_bw() + 
  scale_color_discrete(name="Race")


## 5. PLOT PREVALENCES ## 
# If using only asthma: 
plot_asthma <- ggplot(
  data = prevs,
  aes(x = race_eth_col_lab,
      y = asthma)) + 
  geom_point(
    aes(size = n, color = race_eth_col_lab), 
    alpha = 0.6) +
  scale_size_area(max_size = 30) + 
  geom_text_repel(
    aes(label = hisp_hypo_text),
    color = "black") +
  xlab("") +
  ylab("Prevalence") +
  theme_bw() + 
  scale_color_discrete(name="Race")


















## EXTRAS
## Create collapsed race text categories
#brfss1720 <- brfss1720 %>% 
#  mutate()

## Create race-ethnicity combined variable
#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_eth_text = case_when(
#      `_HISPANC` == 1 ~ paste0(race_eth_text, "-Hispanic"),
#      TRUE ~ race_eth_text
#    )
#  )

#+
#scale_color_manual(
#  values = c("#fdd0a2", "#d9f0a3", "#ffffbf", "#d9d9d9", "#fcc5c0", "#cb181d", "#c6dbef", "#dadaeb")
#) 

#prevs <- brfss1720 %>% 
#  group_by(hisp_hypo_text, 
#           race_eth_col_lab) %>% 
#  filter(n() >= 30) %>% 
#  summarize(asthma = mean(asthma, na.rm = T)*100,
#            n = n()) 

#binge = mean(binge, na.rm = T)*100,
#poor_health = mean(poor_health, na.rm = T)*100,
#mh_15days = mean(mh_15days, na.rm = T)*100,
#depression = mean(depression, na.rm = T)*100,
#heart_att = mean(heart_att, na.rm = T)*100,

# If using multiple outcomes, pivot longer first
# prevs_long <- prevs %>% 
#  pivot_longer(cols = c("asthma", 
#                        "binge", 
#                        "poor_health", 
#                        "mh_15days", 
#                        "depression", 
#                        "heart_att"),
#               names_to = "condition")

#plot_mult <- ggplot(
#  data = prevs_long,
#  aes(x = race_col_lab,
#      y = value)) + 
#  ylab("Prevalence") +
#  theme_bw() + 
#  geom_point(
#    aes(
#      size = n,
#      color = race_col_lab
#    )
#  ) +
#  scale_size_area(max_size = 30) + 
#  geom_text_repel(
#    aes(
#      label = race_eth_text,
#      color = race_col_lab
#    )
#  ) +
#  scale_color_manual(
#    values = c("#fdd0a2", "#d9f0a3", "#ffffbf", "#d9d9d9", "#fcc5c0", "#cb181d", "#c6dbef", "#dadaeb")
#  ) + 
#  facet_wrap(vars(race_col_lab)) + 
#  facet_wrap(vars(condition)) 

## Create detailed race text variable
#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_det = as.character(MRACASC1),
#    race_det = str_replace_all(race_det, "10", "White-"),
#    race_det = str_replace_all(race_det, "20", "Black-"),
#    race_det = str_replace_all(race_det, "30", "AIAN-"),
#    race_det = str_replace_all(race_det, "40", "Asian-"),
#    race_det = str_replace_all(race_det, "41", "Asian Indian-"),
#    race_det = str_replace_all(race_det, "42", "Chinese-"),
#    race_det = str_replace_all(race_det, "43", "Filipino-"),
#    race_det = str_replace_all(race_det, "44", "Japanese-"),
#    race_det = str_replace_all(race_det, "45", "Korean-"),
#    race_det = str_replace_all(race_det, "46", "Vietnamese-"),
#    race_det = str_replace_all(race_det, "47", "Other Asian-"),
#    race_det = str_replace_all(race_det, "50", "Pacific Islander-"),
#    race_det = str_replace_all(race_det, "51", "Native Hawaiian-"),
#    race_det = str_replace_all(race_det, "52", "Guamanian-"),
#    race_det = str_replace_all(race_det, "53", "Samoan-"),
#    race_det = str_replace_all(race_det, "54", "Other Pacific Islander-"),
#    race_det = str_replace_all(race_det, "60", "Other-"),
#    race_det = str_replace_all(race_det, "77", ""),
#    race_det = str_replace_all(race_det, "99", "")
#  )

## Create detailed ethnicity text variable
#brfss1720 <- brfss1720 %>% 
#  mutate(
#    eth_det = as.character(HISPANC3),
#    eth_det = str_replace_all(eth_det, "1", "Mexican-"),
#    eth_det = str_replace_all(eth_det, "2", "Puerto Rican-"),
#    eth_det = str_replace_all(eth_det, "3", "Cuban-"),
#    eth_det = str_replace_all(eth_det, "4", "Other Hispanic-"),
#    eth_det = str_replace_all(eth_det, "5", ""),
#    eth_det = str_replace_all(eth_det, "7", ""),
#    eth_det = str_replace_all(eth_det, "9", ""),
#  )

## Combine detailed race and ethnicity variables (and text variables)
#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_eth = paste0(as.character(MRACASC1),as.character(HISPANC3)),
#    race_eth_det = paste0(race_det, eth_det)
#  )

## Parse text labels to make more readable
#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_eth_det = case_when(
#      is.na(race_eth_det) ~ "NA"),
#    race_eth_det = case_when(
      
#    )
#  )

## Create detailed race and ethnicity variable preserving all responses 
#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_eth_col = paste0(as.character(MRACASC1), "_", as.character(HISPANC3))
#  )



#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_eth_col = case_when(
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "41") ~ str_replace(race_eth_col, "41", "40"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "43") ~ str_replace(race_eth_col, "43", "40"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "44") ~ str_replace(race_eth_col, "44", "40"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "45") ~ str_replace(race_eth_col, "45", "40"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "46") ~ str_replace(race_eth_col, "46", "40"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "47") ~ str_replace(race_eth_col, "47", "40"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "51") ~ str_replace(race_eth_col, "51", "50"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "52") ~ str_replace(race_eth_col, "52", "50"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "53") ~ str_replace(race_eth_col, "53", "50"),
#      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "54") ~ str_replace(race_eth_col, "54", "50"),
#      .default = race_eth_col
#    )
#  )

#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_eth_col = case_when(
#      race_eth_col == "4040" ~ "40",
#      .default = race_eth_col
#    )
#  )


## Create new detailed race text variable
#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_eth_text = race_eth_col,
#    race_eth_text = str_replace(race_eth_text, "H", "Hispanic-"),
#    race_eth_text = str_replace(race_eth_text, "10", "White-"),
#    race_eth_text = str_replace(race_eth_text, "20", "Black-"),
#    race_eth_text = str_replace(race_eth_text, "30", "AIAN-"),
#    race_eth_text = str_replace(race_eth_text, "40", "Asian-"),
#    race_eth_text = str_replace(race_eth_text, "41", "Asian Indian-"),
#    race_eth_text = str_replace(race_eth_text, "42", "Chinese-"),
#    race_eth_text = str_replace(race_eth_text, "43", "Filipino-"),
#    race_eth_text = str_replace(race_eth_text, "44", "Japanese-"),
#    race_eth_text = str_replace(race_eth_text, "45", "Korean-"),
#    race_eth_text = str_replace(race_eth_text, "46", "Vietnamese-"),
#    race_eth_text = str_replace(race_eth_text, "47", "Other Asian-"),
#    race_eth_text = str_replace(race_eth_text, "50", "Pacific Islander-"),
#    race_eth_text = str_replace(race_eth_text, "51", "Native Hawaiian-"),
#    race_eth_text = str_replace(race_eth_text, "52", "Guamanian or Chamorro-"),
#    race_eth_text = str_replace(race_eth_text, "53", "Samoan-"),
#    race_eth_text = str_replace(race_eth_text, "54", "Other Pacific Islander-"),
#    race_eth_text = str_replace(race_eth_text, "60", "Other-"),
#    race_eth_text = str_replace(race_eth_text, "77", "DK-"),
#    race_eth_text = str_replace(race_eth_text, "99", "Refused-"),
#    race_eth_text = str_sub(race_eth_text, end = -2)
#  ) 

## Collapse "DK-Hisp", "Other-Hisp" and "Refused-Hisp" into "Hispanic"
#brfss1720 <- brfss1720 %>% 
#  mutate(
#    race_eth_text = str_replace(race_eth_text, "DK-Hispanic", "Hispanic"),
#    race_eth_text = str_replace(race_eth_text, "Other-Hispanic", "Hispanic"),
#    race_eth_text = str_replace(race_eth_text, "Refused-Hispanic", "Hispanic"),
#  )





# create summary asthma prevalence table
crude_tab <- ca_df %>% 
  select(YEAR, ASTHEVE3, ORACE4, ORACE4A) %>% 
  mutate(
    ASTHEVE3 = case_when(
      ASTHEVE3 %in% c(7, 9, 77, 99) ~ NA_real_, 
      TRUE ~ ASTHEVE3), 
    ORACE4 = case_when(
      ORACE4 == 3 ~ 4, 
      ORACE4 == 4 ~ 5, 
      ORACE4 == 5 ~ 3),
    race = coalesce(ORACE4, ORACE4A)
  ) %>% 
  group_by(YEAR, ASTHEVE3) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = ASTHEVE3, values_from = n) %>% 
  mutate(prop = 100 * `1` / sum(`1` + `2`))

## 1. PREP DATAFRAME ## 
# Coerce DATE, _MSACODE, and SEQNO to numeric in all datasets 
brfss17 <- brfss17 %>% mutate(DATE = as.numeric(DATE))
brfss18 <- brfss18 %>% mutate(DATE = as.numeric(DATE))
brfss19 <- brfss19 %>% mutate(DATE = as.numeric(DATE))
brfss19 <- brfss19 %>% mutate(`_MSACODE` = as.numeric(`_MSACODE`))
brfss20 <- brfss20 %>% mutate(`_MSACODE` = as.numeric(`_MSACODE`))
brfss19 <- brfss19 %>% mutate(`SEQNO` = as.numeric(`SEQNO`))

# Create YEAR variable
brfss17$YEAR <- 2017
brfss18$YEAR <- 2018
brfss19$YEAR <- 2019
brfss20$YEAR <- 2020

# Create new weights for pooled analyses
brfss17$pool_wt <- brfss17$`_LLCPWT` / nrow(brfss17)
brfss18$pool_wt <- brfss18$`_LLCPWT` / nrow(brfss18)
brfss19$pool_wt <- brfss19$`_LLCPWT` / nrow(brfss19)
brfss20$pool_wt <- brfss20$`_LLCPWT` / nrow(brfss20)

# Bind all rows
brfss1720 <- bind_rows(brfss17, brfss18, brfss19, brfss20)

brfss1720 <- brfss1720 %>% 
  mutate(
    asthma = case_when(
      ASTHEVE3 == 1 ~ 1,
      ASTHEVE3 == 2 ~ 0, 
      ASTHEVE3 %in% c(77,99) ~ NA_real_),
    binge = case_when(
      `_RFBING5` == 1 ~ 0,
      `_RFBING5` == 2 ~ 1,
      `_RFBING5` == 99 ~ NA_real_),
    poor_health = case_when(
      `_RFHLTH` == 1 ~ 0,
      `_RFHLTH` == 2 ~ 1,
      `_RFHLTH` == 99 ~ NA_real_),
    mh_15days = case_when(
      MENTHLTH %in% c(77,88,99) ~ NA_real_,
      MENTHLTH >= 15 ~ 1,
      MENTHLTH < 15 ~ 0),
    depression = case_when(
      DEPRESS1 == 1 ~ 1,
      DEPRESS1 == 2 ~ 0, 
      DEPRESS1 %in% c(77,99) ~ NA_real_),
    heart_att = case_when(
      HEART2 == 1 ~ 1,
      HEART2 == 2 ~ 0, 
      HEART2 %in% c(77,99) ~ NA_real_)
  )

# Create "Hispanic Hypodescent" plot (Hispanic ethnicity groups override race)
# If DK/Refused Hispanic ethnicity, use race instead
ca_df <- ca_df %>% 
  mutate(
    hisp_hypo = case_when(
      HISPANIC %in% c("1", "2", "3", "4", "12", "13", "14", "23", "24", "34", 
                      "123", "124", "134", "234", "1234") ~ HISPANIC,
      TRUE ~ MRACASC1
    )
  )

# Create text variable for hisp_hypo 
ca_df <- ca_df %>% 
  mutate(
    hisp_hypo_text = case_when(
      hisp_hypo == "1" ~ "Mexican",
      hisp_hypo == "2" ~ "Puerto Rican",
      hisp_hypo == "3" ~ "Cuban",
      hisp_hypo == "4" ~ "Other Hispanic",
      hisp_hypo == "10" ~ "White",
      hisp_hypo == "20" ~ "Black",
      hisp_hypo == "30" ~ "AIAN",
      hisp_hypo == "40" ~ "Asian",
      hisp_hypo == "41" ~ "Asian Indian",
      hisp_hypo == "42" ~ "Chinese",
      hisp_hypo == "43" ~ "Filipino",
      hisp_hypo == "44" ~ "Japanese",
      hisp_hypo == "45" ~ "Korean",
      hisp_hypo == "46" ~ "Vietnamese",
      hisp_hypo == "47" ~ "Other Asian",
      hisp_hypo == "50" ~ "Pacific Islander",
      hisp_hypo == "51" ~ "Native Hawaiian",
      hisp_hypo == "52" ~ "Guamanian",
      hisp_hypo == "53" ~ "Samoan",
      hisp_hypo == "54" ~ "Other Pacific Islander",
      hisp_hypo == "60" ~ "Other",
      #is.na(hisp_hypo) ~ "DK/O/R",
      hisp_hypo %in% c("77", "88", "99") ~ "DK/O/R",
      TRUE ~ hisp_hypo),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "10", "White-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "20", "Black-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "30", "AIAN-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "40", "Asian-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "50", "Pacific Islander-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "54", "Other Pacific Islander-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "60", "Other-"), 
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "1", "Mexican-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "2", "Puerto Rican-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "3", "Cuban-"),
    hisp_hypo_text = str_replace_all(hisp_hypo_text, "4", "Other Hispanic-")
  )

# Remove trailing "-" characters
ca_df <- ca_df %>% 
  mutate(
    hisp_hypo_text = case_when(
      str_sub(hisp_hypo_text, -1) == "-" ~ str_sub(hisp_hypo_text, 1, -2),
      TRUE ~ hisp_hypo_text
    )
  )

# Reorder racial category names
ca_df <- ca_df %>% 
  mutate(
    hisp_hypo_text = case_when(
      hisp_hypo_text == "Black-AIAN" ~ "AIAN-Black", 
      hisp_hypo_text == "White-AIAN" ~ "AIAN-White", 
      hisp_hypo_text == "White-Asian" ~ "Asian-White", 
      hisp_hypo_text == "White-Black" ~ "Black-White", 
      hisp_hypo_text == "White-Black-AIAN" ~ "AIAN-Black-White", 
      hisp_hypo_text == "White-Other" ~ "Other-White", 
      TRUE ~ hisp_hypo_text
    )
  )

table(ca_df$hisp_hypo_text, ca_df$MRACASC1, exclude = NULL)










# 2022 was sent as CSV; pull in separately and fix variable names
ca_22 <- read_csv("/Users/lamhine/Library/CloudStorage/Box-Box/Tracy Lam-Hine's Files/brfss_disagg/ca_brfss_2000-22/2014-2022/brfs_22core.csv")
ca_22 <- ca_22 %>% 
  mutate(
    ASTHEVE3 = case_when(
      ASTHMA3 == "Yes" ~ 1, 
      ASTHMA3 == "No" ~ 2, 
      ASTHMA3 == "(VOL) Don't know / Not sure" ~ 7,
      ASTHMA3 == "(VOL) Refused" ~ 9, 
      .default = NA_real_)
  )
ca_22 <- ca_22 %>% rename(MRACASC1 = MRACASC2)
ca_brfss <- append(ca_brfss, list(ca_22), after = 8)
