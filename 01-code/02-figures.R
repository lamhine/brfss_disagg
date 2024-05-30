### 1. SET WORKING DIRECTORY, LOAD PACKAGES ###
setwd("/Users/lamhine/Documents/GitHub/brfss_disagg/")
options(scipen = 999)
library(tidyverse)
library(haven)
library(gtsummary)
library(labelled)
library(UpSetR)
library(data.table)
library(foreign)
library(survey)
library(srvyr)
library(ggrepel)




# create unweighted table 1 
table1_unw <- ca_df %>% 
  mutate(across(where(is.factor), ~fct_na_value_to_level(.x, level="Missing"))) %>% 
  tbl_summary(
    include = c(
      age, sex, lt50_ra,  
      edu, employ, income, marital, children, ownhome, 
      hlth_gen, hlth_phys, hlth_ment, 
      ast_lt, ast_now, apcvd, mi, copd, mdd, dm, ckd, bcc, 
      bmi_cat, exercise, smoke, smokeless, drink_any, drink_binge, 
      hiv_test, laiv, pcv, vip, 
      dis_conc, dis_err, dis_dress, dis_walk, 
      ins_any, ins_u65, pe_last, pcp),  
    label = list(age ~ "Age", sex ~ "Male sex", lt50_ra ~ "Race-Ethnicity", 
                 edu ~ "Highest educational attainment", 
                 employ ~ "Employment status", income ~ "Household income",
                 marital ~ "Marital status", children ~ "Number of children",
                 ownhome ~ "Housing status", hlth_gen ~ "Fair or poor health",
                 hlth_phys ~ "Poor physical health days per month",
                 hlth_ment ~ "Poor physical health days per month",
                 ast_lt ~ "Ever diagnosed with asthma", 
                 ast_now ~ "Currently has asthma", 
                 apcvd ~ "Ever diagnosed with angina or CVD",
                 mi ~ "Ever diagnosed with heart attack",
                 copd ~ "Ever diagnosed with COPD",
                 mdd ~ "Ever diagnosed with depressive disorder",
                 dm ~ "Ever diagnosed with diabetes or prediabetes",
                 ckd ~ "Ever diagnosed with kidney disease",
                 bcc ~ "Ever diagnosed with skin cancer",
                 bmi_cat ~ "Overweight or obese BMI",
                 exercise ~ "Any physical activity in last month",
                 smoke ~ "Ever used cigarettes",
                 smokeless ~ "Ever used smokeless tobacco",
                 drink_any ~ "Alcohol use in the last month",
                 drink_binge ~ "Binge drinking status",
                 hiv_test ~ "Ever tested for HIV",
                 laiv ~ "Received flu vaccine in the last year",
                 pcv ~ "Received pneumonia vaccination",
                 vip ~ "Blind or serious vision impairment",
                 dis_conc ~ "Difficulty concentrating, remembering, or making decisions",
                 dis_err ~ "Difficulty doing errands because of physical, mental, or emotional condition",
                 dis_dress ~ "Difficulty dressing or bathing",
                 dis_walk ~ "Serious difficulty walking or climbing stairs",
                 ins_any ~ "Has any insurance coverage",
                 ins_u65 ~ "Under 65 and has insurance coverage",
                 pe_last ~ "Had a routine checkup within the last 2 years",
                 pcp ~ "Has a primary care provider"),
    value = list(sex ~ "Male", hlth_gen ~ "Fair or poor health", ast_lt ~ "Yes", 
                 ast_now ~ "Yes", apcvd ~ "Yes", mi ~ "Yes", copd ~ "Yes", 
                 mdd ~ "Yes", dm ~ "Yes", ckd ~ "Yes", bcc ~ "Yes", 
                 bmi_cat ~ "Yes", exercise ~ "Yes", smoke ~ "Yes", 
                 smokeless ~ "Yes", drink_any ~ "Yes", drink_binge ~ "Yes", 
                 hiv_test ~ "Yes", laiv ~ "Yes", pcv ~ "Yes", vip ~ "Yes", 
                 dis_conc ~ "Yes", dis_err ~ "Yes", dis_dress ~ "Yes", 
                 dis_walk ~ "Yes", ins_any ~ "Yes", ins_u65 ~ "Yes", 
                 pe_last ~ "Yes", pcp ~ "Yes")
  )

table1_unw

## 4. CREATE COMPLEX SURVEY DESIGN ##  

# new column with each year's proportional contribution to total sample size 
samp_size <- ca_df %>% group_by(YEAR) %>% summarize(samp_size = n()/nrow(ca_df))
ca_df <- left_join(ca_df, samp_size, by = "YEAR")

# divide LLCPWT by sample size to calculate final weight variable 
ca_df <- ca_df %>% mutate(final_wt = LLCPWT/samp_size) %>% select(-samp_size)

# Create complex survey design 
ca_dsn <- ca_df %>% 
  as_survey_design(id = 1, strata = STSTR, weights = final_wt)



## 5. CALCULATE WEIGHTED PREVALENCE BY GROUP ## 

# calculate prevalence by group, remove Other/DK/Refused 
prevs <- ca_dsn %>% 
  filter(!re_col_lab %in% c("Other Race", "DK/R")) %>% 
  group_by(lt50_ra, re_col_lab) %>% 
  summarize(n = n(),
    mean = survey_mean(-as.integer(pcp)+2, na.rm = T, vartype = "ci", level = 0.95, proportion = T, prop_method = "logit", df = T)*100)

# BEST WORKING EXAMPLE
plot <- ggplot(prevs, 
       aes(x=reorder(lt50_ra,mean),
           y=mean)) +
  geom_pointrange(
    fatten = 0.5,
    aes(ymin=mean_low, 
        ymax=mean_upp, 
        color=re_col_lab)) + 
  geom_point(
    aes(size = n, color = re_col_lab), 
    alpha = 0.6) +
  scale_size_area(max_size = 5) + 
  coord_cartesian(clip = "off") +
  geom_text_repel(
    aes(label = lt50_ra),
    color = "black",
    #max.overlaps = Inf,
    size = 4) +
  ylab("Prevalence") +
  scale_x_discrete(breaks = NULL) +
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_grid(~re_col_lab, scales = "free_x", space = "free_x")

plot



