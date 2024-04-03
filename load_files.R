## Set working directory
setwd("/Users/lamhine/Documents/GitHub/cabrfss_multiracial/")

## LOAD PACKAGES AND DATA ##
library(tidyverse)
library(boxr)
library(data.table)
library(foreign)
library(survey)
library(srvyr)
library(ggrepel)


"/Users/lamhine/Library/CloudStorage/Box-Box/Tracy Lam-Hine's Files/BRFSS MRACASC Analysis/"

# initialize storage vector and get list of names of DTA files
ca_brfss <- list()
lf = list.files(path="/Users/lamhine/My Drive/My papers, presentations, and abstracts/Active/BRFSS MRACASC1/state_brfss_data/California/CABRFSS/DTA FILES_T.Lam/", 
                pattern=NULL, all.files=FALSE,
                full.names=TRUE)

# read in 2010-2017 and 2019, 2020 BRFSS DTA files into ca_brfss list
for (i in 1:length(lf)) {
  ca_brfss[i] <- list(read_dta(lf[i]))
  }

# 2018 was not included; pull in separately and append into ca_brfss
ca18 <- read_sas("/Users/lamhine/Library/CloudStorage/Box-Box/Data Core/1B_Consortia_Partnerships/San Joaquin Valley PH/Disparities Report/BRFSS/Request#1422-Lam-Hine/2017-2020 Datasets with SJVPHC County Recode/brfss18sjvphc.sas7bdat")
ca_brfss <- append(ca_brfss, list(ca18), after = 8)

# fix mismatched element types for date 
ca_brfss[[5]]$DATE <- mdy(ca_brfss[[5]]$DATE)
ca_brfss[[6]]$DATE <- mdy(ca_brfss[[6]]$DATE)
ca_brfss[[9]]$DATE <- mdy(ca_brfss[[9]]$DATE)
ca_brfss[[10]]$DATE <- mdy(ca_brfss[[10]]$DATE)
ca_brfss[[11]]$DATE <- mdy(ca_brfss[[11]]$DATE)

# bind all list elements into one df 
ca_df <- rbindlist(ca_brfss, fill = T)

# diagnostics
table(ca_df$YEAR, exclude = NULL)
table(ca_df$ASTHEVE3, ca_df$YEAR, exclude=NULL)
table(ca_df$MRACASC1, exclude=NULL)
table(ca_df$MRACASC1, ca_df$YEAR, exclude=NULL)
table(ca_df$MRACE1, ca_df$YEAR, exclude=NULL)
table(ca_df$ORACE4, ca_df$YEAR, exclude=NULL)
table(ca_df$ORACE4A, ca_df$YEAR, exclude=NULL)
table(ca_df$ORACE3_A, ca_df$YEAR, exclude=NULL)

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

## 2. RECODE HEALTH VARIABLES ## 
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

## 3. PREP RACE/ETHNICITY DATA ## 
# Coerce MRACASC1 and HISPANC3 to character
brfss1720 <- brfss1720 %>% 
  mutate(
    MRACASC1 = as.character(MRACASC1),
    HISPANC3 = as.character(HISPANC3)
  )

# Correct data entry errors
brfss1720 <- brfss1720 %>% 
  mutate(
    MRACASC1 = case_when(
      MRACASC1 == "1044" ~ "1040",
      MRACASC1 == "4044" ~ "40",
      MRACASC1 == "4460" ~ "4060",
      TRUE ~ MRACASC1),
    HISPANC3 = case_when(
      HISPANC3 == "41" ~ "14",
      TRUE ~ HISPANC3)
  )

## Create summary group color labels
brfss1720 <- brfss1720 %>% 
  mutate(
    race_eth_col_lab = case_when(
      HISP4 == 1 ~ "Hispanic",
      MRACASC1 == "10" ~ "White",
      MRACASC1 == "20" ~ "Black",
      MRACASC1 == "30" ~ "AIAN",
      MRACASC1 %in% c("40", "41", "42", "43", "44", "45", "46", "47") ~ "Asian", 
      MRACASC1 %in% c("50", "51", "52", "53", "54") ~ "NHPI",
      MRACASC1 %in% c(60, 77, 99) ~ "DK/Other/Refused",
      TRUE ~ "Multiracial"
    )
  )

# Create "Hispanic Hypodescent" plot (Hispanic ethnicity groups override race)
# If DK/Refused Hispanic ethnicity, use race instead
brfss1720 <- brfss1720 %>% 
  mutate(
    hisp_hypo = case_when(
      HISPANC3 %in% c("1", "2", "3", "4", "12", "13", "14", "23", "24", "34", 
                      "123", "124", "134", "1234") ~ HISPANC3,
      TRUE ~ MRACASC1
      )
  )

# Create text variable for hisp_hypo 
brfss1720 <- brfss1720 %>% 
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
      hisp_hypo %in% c("77", "99") ~ "DK/Refused",
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
brfss1720 <- brfss1720 %>% 
  mutate(
    hisp_hypo_text = case_when(
      str_sub(hisp_hypo_text, -1) == "-" ~ str_sub(hisp_hypo_text, 1, -2),
      TRUE ~ hisp_hypo_text
      )
  )

# Reorder racial category names
brfss1720 <- brfss1720 %>% 
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

## 4. CALCULATE WEIGHTED PREVALENCE BY GROUP ##  
# Create complex survey design 
brfsslim <- brfss1720 %>% 
  rename(PSU = `_PSU`,
         STSTR = `_STSTR`) %>% 
  select(PSU, STSTR, pool_wt, race_eth_col_lab, hisp_hypo_text, asthma, 
         `_IMPAGE`, `_IMPSEX`)

brfss_dsn <- svydesign(
  id = ~ PSU, 
  strata = ~ STSTR, 
  weights = ~ pool_wt, 
  data = brfsslim)

brfss_dsn <- brfsslim %>%
  as_survey_design(id = PSU, strata = STSTR, weights = pool_wt)

# calculate prevalence by group, remove Other/DK/Refused & groups with n<30  

brfss_dsn %>% 
  group_by(hisp_hypo_text, race_eth_col_lab) %>% 
  summarize(asthma = survey_mean(asthma, na.rm = T, vartype = "ci", level = 0.95, proportion = T, prop_method = "logit", df = T)*100)


asthma_prevs <- brfss_dsn %>% 
  group_by(hisp_hypo_text, race_eth_col_lab) %>% 
  summarize(asthma = survey_mean(asthma, na.rm = T, vartype = "ci", level = 0.95, proportion = T, prop_method = "logit", df = T)*100, 
            n = n()) %>% 
  filter(n >= 50, 
         race_eth_col_lab != "DK/Other/Refused") %>% 
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








