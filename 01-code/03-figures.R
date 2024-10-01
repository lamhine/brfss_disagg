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


# BEST WORKING PLOTS
ast_lt_plot <- ast_lt_prevs %>% 
  filter(
    lt50_ra != "",
    lt50_ra != "Unknown Race"
  ) %>% 
  ggplot(aes(x=reorder(lt50_ra,mean_lt),
             y=mean_lt)) +
  geom_pointrange(
    fatten = 0.5,
    aes(ymin=mean_lt_low, 
        ymax=mean_lt_upp, 
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

ast_lt_plot




ast_now_plot <- ast_now_prevs %>% 
  filter(
    lt50_ra != "",
    lt50_ra != "Unknown Race"
  ) %>% 
  ggplot(aes(x=reorder(lt50_ra,mean_now),
             y=mean_now)) +
  geom_pointrange(
    fatten = 0.5,
    aes(ymin=mean_now_low, 
        ymax=mean_now_upp, 
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

ast_now_plot









# MODEL RESULTS PLOT -- DO NOT RUN

ast_reg_plot <- mod_full_plot %>% 
  filter(!term %in% c("DK/R", "Other Race")) %>% 
  ggplot(aes(x=reorder(term,estimate),
             y=estimate)) +
  geom_pointrange(
    fatten = 0.5,
    aes(ymin=conf.low, 
        ymax=conf.high, 
        color=re_col_lab)) + 
  geom_point(
    aes(color = re_col_lab), 
    alpha = 0.6) +
  geom_hline(yintercept = 1, color = "gray", linewidth = 1.5) + 
  scale_size_area(max_size = 5) + 
  coord_cartesian(clip = "off") +
  geom_text_repel(
    aes(label = term),
    color = "black",
    max.overlaps = 12,
    size = 4) +
  ylab("Risk ratio") +
  scale_x_discrete(breaks = NULL) +
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_grid(~re_col_lab, scales = "free_x", space = "free_x")

ast_reg_plot





# create weighted table 1 for all outcomes
all_tab1 <- ca_dsn %>% 
  mutate(across(where(is.factor), ~fct_na_value_to_level(.x, level="Missing"))) %>% 
  tbl_svysummary(
    include = c(
      age, sex, re_col_lab,  
      edu, employ, income, marital, children, ownhome, 
      hlth_gen, hlth_phys, hlth_ment, 
      ast_lt, ast_now, apcvd, mi, copd, mdd, dm, ckd, bcc, 
      bmi_cat, exercise, smoke, smokeless, drink_any, drink_binge, 
      hiv_test, laiv, pcv, vip, 
      dis_conc, dis_err, dis_dress, dis_walk, 
      ins_any, ins_u65, pe_last, pcp),  
    label = list(age ~ "Age", sex ~ "Male sex", re_col_lab ~ "Race and Ethnicity", 
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
                 pe_last ~ "Yes", pcp ~ "Yes"),
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  ) 

all_tab1_colap <- ca_dsn %>% 
  mutate(across(where(is.factor), ~fct_na_value_to_level(.x, level="Missing"))) %>% 
  tbl_svysummary(
    include = c(
      age, sex,   
      edu, employ, income, marital, children, ownhome, 
      hlth_gen, hlth_phys, hlth_ment, 
      ast_lt, ast_now, apcvd, mi, copd, mdd, dm, ckd, bcc, 
      bmi_cat, exercise, smoke, smokeless, drink_any, drink_binge, 
      hiv_test, laiv, pcv, vip, 
      dis_conc, dis_err, dis_dress, dis_walk, 
      ins_any, ins_u65, pe_last, pcp),  
    by = re_col_lab,
    label = list(age ~ "Age", sex ~ "Male sex",  
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
                 pe_last ~ "Yes", pcp ~ "Yes"),
    statistic = list(all_categorical() ~ "{p}%")
  ) 

all_tab1_strat <- ca_dsn %>% 
  mutate(across(where(is.factor), ~fct_na_value_to_level(.x, level="Missing"))) %>% 
  tbl_svysummary(
    include = c(
      age, sex,   
      edu, employ, income, marital, children, ownhome, 
      hlth_gen, hlth_phys, hlth_ment, 
      ast_lt, ast_now, apcvd, mi, copd, mdd, dm, ckd, bcc, 
      bmi_cat, exercise, smoke, smokeless, drink_any, drink_binge, 
      hiv_test, laiv, pcv, vip, 
      dis_conc, dis_err, dis_dress, dis_walk, 
      ins_any, ins_u65, pe_last, pcp),  
    by = lt50_ra,
    label = list(age ~ "Age", sex ~ "Male sex",  
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
                 pe_last ~ "Yes", pcp ~ "Yes"),
    statistic = list(all_categorical() ~ "{p}%")
  ) 

all_tab1_strat %>% 
  as_gt() %>% 
  gt::gtsave(filename = "all_tab1_strat.docx",
             path = "/Users/lamhine/Documents/GitHub/brfss_disagg/02-output/multi-outcome")









