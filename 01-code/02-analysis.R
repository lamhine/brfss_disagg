### 1. SET WORKING DIRECTORY, LOAD PACKAGES ###
setwd("/Users/lamhine/Documents/GitHub/brfss_disagg/")
options(scipen = 999)
library(tidyverse)
library(janitor)
library(haven)
library(gtsummary)
library(labelled)
library(UpSetR)
library(data.table)
library(foreign)
library(survey)
library(srvyr)
library(ggrepel)



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

# xwalk to top-level racial groups
re_xwalk <- ca_df %>% group_by(re_col_lab, lt50_ra) %>% summarize(n=n())

# calculate overall lifetime prevalence 
ast_lt_overall_prevs <- ca_dsn %>%
  summarize(
    n = n(),
    mean_lt = 
      survey_mean(-as.integer(ast_lt)+2, 
                  na.rm = T, vartype = c("ci", "se"), 
                  level = 0.95, proportion = T, 
                  prop_method = "logit", df = T)*100) %>% 
  mutate(
    re_col_lab = "Overall",
    lt50_ra = "")

# calculate group lifetime prevalence 
ast_lt_group_prevs <- ca_dsn %>%
  group_by(re_col_lab) %>% 
  summarize(
    n = n(),
    mean_lt = 
      survey_mean(-as.integer(ast_lt)+2, 
                  na.rm = T, vartype = c("ci", "se"), 
                  level = 0.95, proportion = T, 
                  prop_method = "logit", df = T)*100) %>% 
  mutate(lt50_ra = "")

# calculate subgroup lifetime prevalence 
ast_lt_subgroup_prevs <- ca_dsn %>%
  group_by(re_col_lab, lt50_ra) %>% 
  summarize(
    n = n(),
    mean_lt = 
      survey_mean(-as.integer(ast_lt)+2, 
                  na.rm = T, vartype = c("ci", "se"), 
                  level = 0.95, proportion = T, 
                  prop_method = "logit", df = T)*100) 

# calculate overall current prevalence 
ast_now_overall_prevs <- ca_dsn %>%
  summarize(
    n = n(),
    mean_now = 
      survey_mean(-as.integer(ast_now)+2, 
                  na.rm = T, vartype = c("ci", "se"), 
                  level = 0.95, proportion = T, 
                  prop_method = "logit", df = T)*100) %>% 
  mutate(
    re_col_lab = "Overall",
    lt50_ra = "")

# calculate group current prevalence 
ast_now_group_prevs <- ca_dsn %>%
  group_by(re_col_lab) %>% 
  summarize(
    n = n(),
    mean_now = 
      survey_mean(-as.integer(ast_now)+2, 
                  na.rm = T, vartype = c("ci", "se"), 
                  level = 0.95, proportion = T, 
                  prop_method = "logit", df = T)*100) %>% 
  mutate(lt50_ra = "")

# calculate subgroup current prevalence 
ast_now_subgroup_prevs <- ca_dsn %>%
  group_by(re_col_lab, lt50_ra) %>% 
  summarize(
    n = n(),
    mean_now = 
      survey_mean(-as.integer(ast_now)+2, 
                  na.rm = T, vartype = c("ci", "se"), 
                  level = 0.95, proportion = T, 
                  prop_method = "logit", df = T)*100) 

# format prevalence table 
ast_lt_prevs <- bind_rows(
  ast_lt_subgroup_prevs, 
  ast_lt_group_prevs, 
  ast_lt_overall_prevs) %>% 
  arrange(lt50_ra) %>% 
  arrange(re_col_lab) %>% 
  mutate(
    rse_lt = format(round((mean_lt_se/mean_lt)*100, 1), nsmall=1),
    prev_lt = 
      paste0(format(round(mean_lt, 1), nsmall=1),
             " (", 
             format(round(mean_lt_low, 1), nsmall=1), 
             ", ", 
             format(round(mean_lt_upp, 1), nsmall=1),
             ")"
      )
  ) 

ast_now_prevs <- bind_rows(
  ast_now_subgroup_prevs, 
  ast_now_group_prevs, 
  ast_now_overall_prevs) %>% 
  arrange(lt50_ra) %>% 
  arrange(re_col_lab) %>% 
  mutate(
    rse_now = format(round((mean_now_se/mean_now)*100, 1), nsmall=1),
    prev_now = 
      paste0(format(round(mean_now, 1), nsmall=1),
             " (", 
             format(round(mean_now_low, 1), nsmall=1), 
             ", ", 
             format(round(mean_now_upp, 1), nsmall=1),
             ")"
             )
         ) 

# merge tables
prevs_table <- left_join(
  select(ast_lt_prevs, -c("mean_lt", "mean_lt_low", "mean_lt_upp", "mean_lt_se")),
  select(ast_now_prevs, -c("mean_now", "mean_now_low", "mean_now_upp", "mean_now_se")),
  by = c("re_col_lab", "lt50_ra", "n")
  ) %>%
  filter(!(re_col_lab == "AIAN" & lt50_ra == "AIAN"),
         !(re_col_lab == "Black" & lt50_ra == "Black"),
         !(re_col_lab == "Other Race" & lt50_ra == "Other Race"),
         !(re_col_lab == "White" & lt50_ra == "White"))
  



# ast_now_prevs <- ca_dsn %>% 
  filter(lt50_ra != "DK/R",
         lt50_ra != "Other Race") %>% 
  group_by(re_col_lab, lt50_ra) %>% 
  summarize(
    n = n(), 
    mean = survey_mean(-as.integer(ast_now)+2, 
                       na.rm = T, vartype = "se", 
                       level = 0.95, proportion = T, 
                       prop_method = "logit", df = T)*100) 

ast_now_prev <- ast_now_prevs %>% 
  mutate(prev_ast_now = 
           paste0(
             signif(mean, 2), 
             " (", 
             signif(mean_low, 2), 
             ", ", 
             signif(mean_upp, 2),
             ")"
           )
  ) %>% 
  select(-c("mean", "mean_low", "mean_upp"))


# create table 2
ast_tab2 <- left_join(re_xwalk, ast_lt_prevs) %>% 
  left_join(ast_now_prevs, by = "lt50_ra")


# get full sample prop of ast_lt and ast_now
ca_dsn %>% 
  summarize(
    mean = survey_mean(-as.integer(ast_lt)+2, 
                       na.rm = T, vartype = "ci", 
                       level = 0.95, proportion = T, 
                       prop_method = "logit", df = T)*100) %>% 
  mutate(prev_ast_lt = 
           paste0(
             signif(mean, 2), 
             " (", 
             signif(mean_low, 2), 
             ", ", 
             signif(mean_upp, 2),
             ")"
           )
  ) %>% 
  select(-c("mean", "mean_low", "mean_upp"))

ca_dsn %>% 
  summarize(
    mean = survey_mean(-as.integer(ast_now)+2, 
                       na.rm = T, vartype = "ci", 
                       level = 0.95, proportion = T, 
                       prop_method = "logit", df = T)*100) %>% 
  mutate(prev_ast_now = 
           paste0(
             signif(mean, 2), 
             " (", 
             signif(mean_low, 2), 
             ", ", 
             signif(mean_upp, 2),
             ")"
           )
  ) %>% 
  select(-c("mean", "mean_low", "mean_upp"))






## MODELS - DO NOT RUN
mod_full <- svyglm(-as.integer(ast_lt)+2 ~ age + sex + 
                     fct_relevel(lt50_ra, "White") + edu + employ + income + 
                     ownhome + bmi_cat + exercise + smoke + ins_any + pe_last + pcp, 
                   family=quasipoisson("log"),
                   ca_dsn)

ast_lt_mod_full <- svyglm(-as.integer(ast_lt)+2 ~ age + sex + 
                            fct_relevel(lt50_ra, "White") + edu + employ + income + 
                            ownhome + bmi_cat + exercise + smoke + ins_any + pe_last + pcp, 
                          family=quasipoisson("log"),
                          ca_dsn)

ast_now_mod_full <- svyglm(-as.integer(ast_now)+2 ~ age + sex + 
                             fct_relevel(lt50_ra, "White") + edu + employ + income + 
                             ownhome + bmi_cat + exercise + smoke + ins_any + pe_last + pcp, 
                           family=quasipoisson("log"),
                           ca_dsn)

# tidy results
mod_full_res <- broom::tidy(mod_full, conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
  filter(str_detect(term, "lt50_ra")) %>% 
  mutate(term = substr(term,30,nchar(term)))
mod_full_res <- left_join(mod_full_res, re_xwalk, by = c("term" = "lt50_ra"))

ast_lt_res <- broom::tidy(ast_lt_mod_full, conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
  filter(str_detect(term, "lt50_ra")) %>% 
  mutate(lt50_ra = substr(term,30,nchar(term)),
         rr_ast_lt = 
           paste0(
             signif(estimate, 2), 
             " (", 
             signif(conf.low, 2), 
             ", ", 
             signif(conf.high, 2),
             ")"
           )) %>% 
  select(lt50_ra, rr_ast_lt)


ast_now_res <- broom::tidy(ast_now_mod_full, conf.int = T, conf.level = 0.95, exponentiate = T) %>% 
  filter(str_detect(term, "lt50_ra")) %>% 
  mutate(lt50_ra = substr(term,30,nchar(term)),
         rr_ast_now = 
           paste0(
             signif(estimate, 2), 
             " (", 
             signif(conf.low, 2), 
             ", ", 
             signif(conf.high, 2),
             ")"
           )) %>% 
  select(lt50_ra, rr_ast_now)

