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
library(splines)
library(ggrepel)
library(tidycensus)
library(patchwork)

########viridis############################## MULTI-OUTCOME ANALYSIS ##########################

# define outcomes to caluclate prevalence
outcomes <- c("ast_now", "fphs", "mphd_poor", "mmhd_poor", "ovwob", "smoke", 
              "vip", "dis_walk", "ast_lt", "apcvd", "mi", "copd", "mdd", "dm", 
              "ckd", "bcc", "exer_no", "drink_any", "drink_binge", "mphd_poor", 
              "mmhd_poor", "hivtest_no", "laiv_no", "pcv_no", "dis_conc", 
              "dis_err", "dis_dress", "ins_no", "chk2_no", "pcp_no")


# Direct adjustment to CA population ##
census_api_key(Sys.getenv("census_api_key"))


# Retrieve ACS population estimates for 18 and over and process
pop_denoms <- get_acs(
  geography = "state",
  state = "CA",
  variables = c(
    paste0("B01001_", sprintf("%03d", 7:25)),  # Male age groups 18 years and older
    paste0("B01001_", sprintf("%03d", 31:49))  # Female age groups 18 years and older
  ),
  year = 2020,
  survey = "acs5"
) %>%
  mutate(
    # Determine sex based on the variable code
    sex = if_else(str_detect(variable, "B01001_0(0[7-9]|1[0-9]|2[0-5])"), "Male", "Female"),
    sex = factor(sex, levels = c("Male", "Female")),  # Ensure it matches ca_dsn$variables
    
    # Map the variables to the detailed age groups
    age_grp = case_when(
      variable %in% c("B01001_007", "B01001_031", "B01001_008", "B01001_032", "B01001_009", "B01001_033") ~ "15 to 29 years",
      variable %in% c("B01001_010", "B01001_034", "B01001_011", "B01001_035", "B01001_012", "B01001_036") ~ "30 to 44 years",
      variable %in% c("B01001_013", "B01001_037", "B01001_014", "B01001_038", "B01001_015", "B01001_039") ~ "45 to 59 years",
      variable %in% c("B01001_016", "B01001_040", "B01001_017", "B01001_041", "B01001_018", "B01001_042") ~ "60 years and over",
      TRUE ~ NA_character_  # Catch-all for variables that may not fit into the groups
    ),
    
    # Ensure the factor levels match the desired groups
    age_grp = factor(age_grp, levels = c("15 to 29 years", "30 to 44 years", "45 to 59 years", "60 years and over"))
  ) %>%
  filter(!is.na(age_grp)) %>%  # Remove NAs
  select(sex, age_grp, estimate) %>%  # Keep only the necessary columns
  
  # Summarize to combine rows for each sex-age group combination
  group_by(sex, age_grp) %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup()  # Remove grouping for further processing


# Create age and sex distribution by lt50_ra (the race/ethnicity categories in BRFSS)
# Recreate age_sex_distribution with the correct count
age_sex_distribution <- ca_dsn %>%
  group_by(lt50_ra, age_grp, sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(lt50_ra) %>%
  mutate(total_count = sum(count),
         age_sex_dist = count / total_count) %>%
  select(-count, -total_count)



# Initialize an empty list to store results
all_results <- list()

# Loop through each outcome and calculate unadjusted and standardized prevalence
for (outcome in outcomes) {
  
  # Calculate unadjusted prevalence (survey-weighted) stratified by age, sex, and lt50_ra
  unadj_df <- 
    svyby(
      formula = as.formula(paste0("~", outcome)),
      by = as.formula("~lt50_ra + age_grp + sex"),  
      design = ca_dsn,
      FUN = svymean,
      na.rm = TRUE
    ) %>%
    tibble::as_tibble() %>%
    remove_rownames() %>%
    select(
      lt50_ra, age_grp, sex, 
      prevalence = all_of(paste0(outcome, "Yes")), 
      se_prevalence = all_of(paste0("se.", outcome, "Yes"))  
    ) %>%
    mutate(
      adjustment_type = "Unadjusted", 
      RSE = case_when(
        prevalence > 0 ~ (se_prevalence / prevalence) * 100,  
        TRUE ~ NA_real_
      )
    )
  
  # Merge population distribution with pop_denoms to get 'estimate'
  merged_dist <- left_join(age_sex_distribution, pop_denoms, by = c("age_grp", "sex")) 
  
  # Ensure 'estimate' exists before proceeding
  if (!"estimate" %in% colnames(merged_dist)) {
    stop("Error: 'estimate' column is missing after merging with pop_denoms.")
  }
  
  # Compute weights using 'estimate'
  merged_dist <- merged_dist %>%
    mutate(weight = estimate / sum(estimate, na.rm = TRUE))
  
  # Merge unadjusted prevalence
  merged_dist <- left_join(merged_dist, unadj_df, by = c("lt50_ra", "age_grp", "sex"))
  
  # Ensure weight is still present
  if (!"weight" %in% colnames(merged_dist)) {
    stop("Error: 'weight' column is missing after merging with unadj_df.")
  }
  
  # Compute expected cases
  merged_dist <- merged_dist %>%
    mutate(expected_cases = prevalence * weight * sum(pop_denoms$estimate, na.rm = TRUE))
  
  # Unadjusted summary  
  unadj_summary <- merged_dist %>%
    group_by(lt50_ra) %>%
    summarise(
      prevalence = mean(prevalence, na.rm = TRUE), 
      se_prevalence = mean(se_prevalence, na.rm = TRUE),  
      RSE = case_when(
        prevalence > 0 ~ (se_prevalence / prevalence) * 100,  
        TRUE ~ NA_real_
      ),
      .groups = 'drop'
    ) %>%
    mutate(adjustment_type = "Unadjusted")
  
  # Standardized prevalence calculation using Taylor Series Approximation
  std_df <- merged_dist %>%
    group_by(lt50_ra) %>%
    summarise(
      prevalence = sum(expected_cases, na.rm = TRUE) / sum(pop_denoms$estimate, na.rm = TRUE),
      
      # Taylor Series Approximation for SE
      se_prevalence = sqrt(
        sum((weight^2) * (se_prevalence^2), na.rm = TRUE)  # Correct variance propagation
      ),
      
      RSE = case_when(
        prevalence > 0 ~ (se_prevalence / prevalence) * 100,
        TRUE ~ NA_real_
      ),
      
      .groups = 'drop'
    ) %>% 
    mutate(adjustment_type = "Standardized")
  
  # Combine unadjusted and standardized results
  combined_df <- bind_rows(unadj_summary, std_df)
  
  # Store results for each outcome
  all_results[[outcome]] <- combined_df
}

# Combine all results into one dataframe
final_combined_df <- bind_rows(all_results, .id = "outcome")

# View final results
View(final_combined_df)
glimpse(final_combined_df)








## HEATMAP FOR UNADJUSTED PREVALENCE ###########################################

# Prepare heatmap data for unadjusted prevalence
unadj_heatmap_data <- final_combined_df %>%
  filter(adjustment_type == "Unadjusted") %>%
  left_join(
    distinct(ca_imp, lt50_ra, re_col_lab),  
    by = "lt50_ra"
  ) %>%
  mutate(
    lt50_ra = factor(
      lt50_ra,
      levels = ca_imp %>%
        arrange(re_col_lab, desc(lt50_ra)) %>%
        pull(lt50_ra) %>%
        unique()
    )
  ) %>%
  # Normalize the prevalence values within each outcome
  group_by(outcome) %>%
  mutate(
    Normalized_Values = (prevalence - min(prevalence, na.rm = TRUE)) /
      (max(prevalence, na.rm = TRUE) - min(prevalence, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  
  # Define opacity based on certainty (1 - RSE)
  mutate(
    alpha_value = scales::rescale(1 - RSE, to = c(0.3, 1)),  
    text_color = ifelse(Normalized_Values > 0.5, "white", "black")  # Adjust font color dynamically
  )

# Create heatmap with "heat" color scale
unadj_heatmap <- ggplot(unadj_heatmap_data2, aes(x = outcome, y = lt50_ra, fill = Normalized_Values, alpha = alpha_value)) + 
  geom_tile(color = NA) +  
  
  # **"Heat" Palette for Strong Contrast (Yellow → Red)**
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,  # Ensures Yellow (High) → Blue (Low)
    name = "Prevalence (Normalized)"
  ) +
  
  # **Opacity for certainty (1 - RSE)**
  scale_alpha_continuous(name = "Certainty (1 - RSE)", range = c(0.3, 1)) +
  
  # **Text labels (NOT bold, dynamically colored for contrast)**
  geom_text(aes(label = sprintf("%.1f", prevalence * 100), color = text_color), 
            size = 3, 
            alpha = 1) +  
  
  # **Manual color scale for text**
  scale_color_manual(values = c("black" = "black", "white" = "white"), guide = "none") +
  
  labs(
    title = "Heatmap of Unadjusted Prevalence by Race and Outcome\n(Opacity Represents Certainty)",
    x = "Outcome",
    y = NULL  
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    axis.text.y.left = element_text(angle = 0),
    
    # **Remove unwanted gridlines**
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  facet_grid(re_col_lab ~ ., scales = "free_y", space = "free_y") +
  coord_cartesian(clip = "off")  

# Display the heatmap
unadj_heatmap    


## HEATMAP FOR STANDARDIZED PREVALENCE #########################################


# Prepare data
std_heatmap_data <- final_combined_df %>%
  filter(adjustment_type == "Standardized") %>%
  left_join(distinct(ca_imp, lt50_ra, re_col_lab), by = "lt50_ra") %>%
  mutate(
    lt50_ra = factor(
      lt50_ra,
      levels = ca_imp %>%
        arrange(re_col_lab, desc(lt50_ra)) %>%
        pull(lt50_ra) %>%
        unique()
    )
  ) %>%
  group_by(outcome) %>%
  mutate(
    Normalized_Values = (prevalence - min(prevalence, na.rm = TRUE)) /
      (max(prevalence, na.rm = TRUE) - min(prevalence, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  
  # **Dynamic text color for legibility**
  mutate(
    text_color = ifelse(Normalized_Values > 0.5, "white", "black")
  )

# **Prevalence Heatmap (Left, Larger, No `re_col_lab` Labels)**
prevalence_plot <- ggplot(std_heatmap_data, aes(x = outcome, y = lt50_ra, fill = Normalized_Values)) +
  geom_tile(color = "white", size = 0.25) +  
  geom_text(aes(label = sprintf("%.1f", prevalence * 100), color = text_color), size = 3) +  
  scale_fill_viridis_c(option = "YlGnBu", direction = -1, name = "Prevalence (Normalized)") +
  scale_color_manual(values = c("black" = "black", "white" = "white"), guide = "none") +
  labs(title = "Standardized Prevalence by Race and Outcome", x = "Outcome", y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text.y = element_blank()  # **Hide re_col_lab labels**
  ) +
  facet_grid(re_col_lab ~ ., scales = "free_y", space = "free_y")

# **Uncertainty Heatmap (Right, Smaller, No Labels, No `re_col_lab`)**
uncertainty_plot <- ggplot(std_heatmap_data, aes(x = outcome, y = lt50_ra, fill = RSE)) +
  geom_tile(color = "white", size = 0.25) +  
  scale_fill_gradient(low = "white", high = "black", name = "Uncertainty (RSE)") +
  labs(title = "Uncertainty by Race and Outcome", x = "Outcome", y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),  # **Remove y-axis labels to avoid redundancy**
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text.y = element_blank()  # **Hide re_col_lab labels**
  ) +
  facet_grid(re_col_lab ~ ., scales = "free_y", space = "free_y")

# **Combine Plots (Left = 2x Width of Right)**
final_plot <- prevalence_plot + uncertainty_plot + plot_layout(ncol = 2, widths = c(2, 1))

# **Display**
final_plot



# Prepare data
std_heatmap_data <- final_combined_df %>%
  filter(adjustment_type == "Standardized") %>%
  left_join(distinct(ca_imp, lt50_ra, re_col_lab), by = "lt50_ra") %>%
  mutate(
    lt50_ra = factor(
      lt50_ra,
      levels = ca_imp %>%
        arrange(re_col_lab, desc(lt50_ra)) %>%
        pull(lt50_ra) %>%
        unique()
    )
  ) %>%
  group_by(outcome) %>%
  mutate(
    Normalized_Values = (prevalence - min(prevalence, na.rm = TRUE)) /
      (max(prevalence, na.rm = TRUE) - min(prevalence, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  
  # Define opacity (uncertainty) and dynamic text color
  mutate(
    alpha_value = scales::rescale(1 - RSE, to = c(0.3, 1)),  # More transparent for higher RSE
    text_color = ifelse(Normalized_Values > 0.5, "white", "black")
  )

# Create overlaid heatmap
std_heatmap <- ggplot(std_heatmap_data, aes(x = outcome, y = lt50_ra, fill = Normalized_Values, alpha = alpha_value)) +
  geom_tile(color = NA) +  # No tile borders for smoother blending
  
  # **Color Scale for Prevalence**
  scale_fill_distiller(
    palette = "YlGnBu",  # Your selected color palette
    direction = 1,  # Ensures correct color scaling
    name = "Prevalence (Normalized)"
  ) +
  
  # **Opacity for Uncertainty (RSE)**
  scale_alpha_continuous(
    name = "Certainty (1 - RSE)",
    range = c(0.3, 1)  # Ensuring transparency varies effectively
  ) +
  
  # **Text labels for prevalence**
  geom_text(aes(label = sprintf("%.1f", prevalence * 100), color = text_color), size = 3, alpha = 1) +  
  
  # **Dynamic text color for contrast**
  scale_color_manual(values = c("black" = "black", "white" = "white"), guide = "none") +
  
  # **Plot labels**
  labs(
    title = "Standardized Prevalence by Race and Outcome\n(Opacity Represents Uncertainty)",
    x = "Outcome",
    y = NULL
  ) +
  
  # **Clean, readable theme**
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",  # Legend at the bottom for a clean layout
    strip.text.y = element_blank()  # **Remove re_col_lab labels**
  ) +
  
  facet_grid(re_col_lab ~ ., scales = "free_y", space = "free_y")

# Display the heatmap
std_heatmap
