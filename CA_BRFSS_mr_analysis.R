## Set working directory
setwd("/Users/lamhine/Documents/GitHub/cabrfss_multiracial/")

## Load packages
library(tidyverse)
library(foreign)
library(haven)
library(survey)
library(naniar)

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

## Subset dataframe to just Multiracial people
brfss_mr <- brfss1720 %>% filter(nchar(MRACASC1) > 3)


