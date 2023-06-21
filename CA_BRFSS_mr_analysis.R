## Set working directory
setwd("/Users/lamhine/Documents/GitHub/cabrfss_multiracial/")

## Load packages
library(tidyverse)
library(foreign)
library(haven)
library(survey)
library(srvyr)
library(ggrepel)

## Load BRFSS
brfss17 <- read_sas("/Users/lamhine/Library/CloudStorage/Box-Box/Data Core/1B_Consortia_Partnerships/San Joaquin Valley PH/Disparities Report/BRFSS/Request#1422-Lam-Hine/2017-2020 Datasets with SJVPHC County Recode/brfss17sjvphc.sas7bdat")
brfss18 <- read_sas("/Users/lamhine/Library/CloudStorage/Box-Box/Data Core/1B_Consortia_Partnerships/San Joaquin Valley PH/Disparities Report/BRFSS/Request#1422-Lam-Hine/2017-2020 Datasets with SJVPHC County Recode/brfss18sjvphc.sas7bdat")
brfss19 <- read_sas("/Users/lamhine/Library/CloudStorage/Box-Box/Data Core/1B_Consortia_Partnerships/San Joaquin Valley PH/Disparities Report/BRFSS/Request#1422-Lam-Hine/2017-2020 Datasets with SJVPHC County Recode/brfss19sjvphc.sas7bdat")
brfss20 <- read_sas("/Users/lamhine/Library/CloudStorage/Box-Box/Data Core/1B_Consortia_Partnerships/San Joaquin Valley PH/Disparities Report/BRFSS/Request#1422-Lam-Hine/2017-2020 Datasets with SJVPHC County Recode/brfss20sjvphc.sas7bdat")

## Coerce DATE, _MSACODE, and SEQNO to numeric in all datasets 
brfss17 <- brfss17 %>% mutate(DATE = as.numeric(DATE))
brfss18 <- brfss18 %>% mutate(DATE = as.numeric(DATE))
brfss19 <- brfss19 %>% mutate(DATE = as.numeric(DATE))
brfss19 <- brfss19 %>% mutate(`_MSACODE` = as.numeric(`_MSACODE`))
brfss20 <- brfss20 %>% mutate(`_MSACODE` = as.numeric(`_MSACODE`))
brfss19 <- brfss19 %>% mutate(`SEQNO` = as.numeric(`SEQNO`))

## Create YEAR variable
brfss17$YEAR <- 2017
brfss18$YEAR <- 2018
brfss19$YEAR <- 2019
brfss20$YEAR <- 2020

## Create new weights for pooled analyses
brfss17$pool_wt <- brfss17$`_LLCPWT` / nrow(brfss17)
brfss18$pool_wt <- brfss18$`_LLCPWT` / nrow(brfss18)
brfss19$pool_wt <- brfss19$`_LLCPWT` / nrow(brfss19)
brfss20$pool_wt <- brfss20$`_LLCPWT` / nrow(brfss20)

## Bind all rows
brfss1720 <- bind_rows(brfss17, brfss18, brfss19, brfss20)

## Tabulate different Multiracial combinations
table(brfss1720$MRACASC1)

## Tabulate different Multiracial combinations by Hispanic/Latino ethnicity
table(brfss1720$MRACASC1, brfss1720$`_HISPANC`)

## Create detailed race variable with ethnicity collapsed 
brfss1720 <- brfss1720 %>% 
  mutate(
    race_eth_col = as.character(MRACASC1),
    race_eth_col = case_when(
      `_HISPANC` == 1 ~ paste0(race_eth_col, "H"),
      .default = race_eth_col
    )
  )

brfss1720 <- brfss1720 %>% 
  mutate(
    race_eth_col = case_when(
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "41") ~ str_replace(race_eth_col, "41", "40"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "42") ~ str_replace(race_eth_col, "42", "40"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "43") ~ str_replace(race_eth_col, "43", "40"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "44") ~ str_replace(race_eth_col, "44", "40"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "45") ~ str_replace(race_eth_col, "45", "40"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "46") ~ str_replace(race_eth_col, "46", "40"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "47") ~ str_replace(race_eth_col, "47", "40"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "51") ~ str_replace(race_eth_col, "51", "50"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "52") ~ str_replace(race_eth_col, "52", "50"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "53") ~ str_replace(race_eth_col, "53", "50"),
      nchar(race_eth_col) > 2 & str_detect(race_eth_col, "54") ~ str_replace(race_eth_col, "54", "50"),
      .default = race_eth_col
    )
  )

brfss1720 <- brfss1720 %>% 
  mutate(
    race_eth_col = case_when(
      race_eth_col == "4040" ~ "40",
      .default = race_eth_col
    )
  )


## Create new detailed race text variable
brfss1720 <- brfss1720 %>% 
  mutate(
    race_eth_text = race_eth_col,
    race_eth_text = str_replace(race_eth_text, "H", "Hispanic-"),
    race_eth_text = str_replace(race_eth_text, "10", "White-"),
    race_eth_text = str_replace(race_eth_text, "20", "Black-"),
    race_eth_text = str_replace(race_eth_text, "30", "AIAN-"),
    race_eth_text = str_replace(race_eth_text, "40", "Asian-"),
    race_eth_text = str_replace(race_eth_text, "41", "Asian Indian-"),
    race_eth_text = str_replace(race_eth_text, "42", "Chinese-"),
    race_eth_text = str_replace(race_eth_text, "43", "Filipino-"),
    race_eth_text = str_replace(race_eth_text, "44", "Japanese-"),
    race_eth_text = str_replace(race_eth_text, "45", "Korean-"),
    race_eth_text = str_replace(race_eth_text, "46", "Vietnamese-"),
    race_eth_text = str_replace(race_eth_text, "47", "Other Asian-"),
    race_eth_text = str_replace(race_eth_text, "50", "Pacific Islander-"),
    race_eth_text = str_replace(race_eth_text, "51", "Native Hawaiian-"),
    race_eth_text = str_replace(race_eth_text, "52", "Guamanian or Chamorro-"),
    race_eth_text = str_replace(race_eth_text, "53", "Samoan-"),
    race_eth_text = str_replace(race_eth_text, "54", "Other Pacific Islander-"),
    race_eth_text = str_replace(race_eth_text, "60", "Other-"),
    race_eth_text = str_replace(race_eth_text, "77", "DK-"),
    race_eth_text = str_replace(race_eth_text, "99", "Refused-"),
    race_eth_text = str_sub(race_eth_text, end = -2)
    ) 

## Collapse "DK-Hisp", "Other-Hisp" and "Refused-Hisp" into "Hispanic"
brfss1720 <- brfss1720 %>% 
  mutate(
    race_eth_text = str_replace(race_eth_text, "DK-Hispanic", "Hispanic"),
    race_eth_text = str_replace(race_eth_text, "Other-Hispanic", "Hispanic"),
    race_eth_text = str_replace(race_eth_text, "Refused-Hispanic", "Hispanic"),
  )


## Create summary group color labels
brfss1720 <- brfss1720 %>% 
  mutate(
    race_col_lab = case_when(
      str_detect(race_eth_text, "Hispanic") ~ "Hispanic",
      MRACASC1 == 10 ~ "White",
      MRACASC1 == 20 ~ "Black",
      MRACASC1 == 30 ~ "AIAN",
      MRACASC1 %in% c(40:49) ~ "Asian", 
      MRACASC1 %in% c(50:59) ~ "NHPI",
      MRACASC1 %in% c(60, 77, 99) ~ "DK/Other/Refused",
      .default = "Multiracial"
    )
  )

## Recode asthma and binge drinking variable
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



## Calculate unweighted prevalences of asthma and binge drinking 
prevs <- brfss1720 %>% 
  group_by(race_eth_text, 
           race_col_lab) %>% 
  filter(n() >= 30) %>% 
  summarize(asthma = mean(asthma, na.rm = T)*100,
            binge = mean(binge, na.rm = T)*100,
            poor_health = mean(poor_health, na.rm = T)*100,
            mh_15days = mean(mh_15days, na.rm = T)*100,
            depression = mean(depression, na.rm = T)*100,
            heart_att = mean(heart_att, na.rm = T)*100,
            n = n()) 

## Scatterplot prevalences (go to long format first)
prevs_long <- prevs %>% 
  pivot_longer(cols = c("asthma", "binge", "poor_health", "mh_15days", 
                        "depression", "heart_att"),
               names_to = "condition")

plot <- ggplot(
  data = prevs_long,
  aes(x = race_col_lab,
      y = value)) + 
  ylab("Prevalence") +
  theme_bw()

plot + 
  geom_point(
    aes(
      size = n,
      color = race_col_lab
      )
    ) +
  scale_size_area(max_size = 30) + 
  geom_text_repel(
    aes(
      label = race_eth_text,
      color = race_col_lab
      )
  ) +
  scale_color_manual(
    values = c("#fdd0a2", "#d9f0a3", "#ffffbf", "#d9d9d9", "#fcc5c0", "#cb181d", "#c6dbef", "#dadaeb")
  ) + 
  facet_wrap(vars(race_col_lab)) + 
  facet_wrap(vars(condition)) 



## Create complex survey design 
brfss_dsn <- svydesign(
  id = ~`_PSU`, 
  strata = ~`_STSTR`, 
  weights = ~pool_wt, 
  data = brfss1720)

brfss_dsn <- brfss1720 %>%
  as_survey_design(id = `_PSU`, strata = `_STSTR`, weights = pool_wt)





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
