# 08_visualize_data.R
# Purpose: Generate heatmap visualizations for standardized prevalence.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(ggpattern)
library(gt)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD STANDARDIZED PREVALENCE ESTIMATES
# ---------------------- #

# Load survey design and prevalence estimates
final_combined_df <- readRDS(file.path(processed_data_dir, "07_prevalence_estimates.rds"))

# ---------------------- #
# CREATE RACE/ETHNICITY GROUPS (re_groups)
# ---------------------- #

# Manually specify the correct category order
category_order <- c("Lifestyle factors", "Preventive healthcare", "Chronic conditions", "General health status", "Disabilities")

# Assign summary group labels and order categories
std_heatmap_data <- final_combined_df %>%
  filter(adjustment_type == "Standardized") %>%
  
  # Assign re_groups 
  mutate(
    re_groups = case_when(
      re_text %in% hispanic_groups ~ "Hispanic",
      re_text %in% names(re_groups_lookup) ~ re_groups_lookup[re_text],
      TRUE ~ "Multiracial"
    ),
    
    # Define the correct order of re_groups
    re_groups = factor(re_groups, levels = re_groups_levels),
    
    # Ensure correct order for category labels
    category_labels = factor(outcome_cats[outcome], levels = category_order),
    
    # Replace outcome variable names with labels
    outcome = variable_labels[outcome]
  ) %>% 
  select(-c(adjustment_type, se_prevalence, age_grp, sex))

# ---------------------- #
# CREATE CENSUS-STYLE HEATMAP GROUPS (heatmap_groups)
# ---------------------- #

std_heatmap_data <- std_heatmap_data %>%
  mutate(
    heatmap_groups = map(re_text, function(group) {
      groupings <- c()
      
      # Assign usual groupings
      if (str_detect(group, "Hispanic|Mexican|Puerto Rican|Cuban|Other Hispanic")) groupings <- c(groupings, "Hispanic")
      if (str_detect(group, "AIAN")) groupings <- c(groupings, "AIAN")
      if (str_detect(group, "Asian|Chinese|Filipino|Japanese|Vietnamese|Korean|Asian Indian|Other Asian|Unspecified Asian")) groupings <- c(groupings, "Asian")
      if (str_detect(group, "Black")) groupings <- c(groupings, "Black")
      if (str_detect(group, "White")) groupings <- c(groupings, "White")
      if (str_detect(group, "Pacific Islander|NHPI|Samoan|Guamanian|Hawaiian|Native Hawaiian|Other Pacific Islander|Unspecified NHPI")) {
        groupings <- c(groupings, "NHPI")
      }
      if (str_detect(group, "Other Race")) groupings <- c(groupings, "Other Race")
      if (str_detect(group, "DK/R")) groupings <- c(groupings, "DK/R")
      
      # Count racial mentions (excluding Hispanic-related words)
      race_count <- str_count(group, paste(c(
        "AIAN", "Asian", "Black", "White", "NHPI", 
        "Pacific Islander", "Native Hawaiian", "Samoan", "Guamanian", "Multiple"
      ), collapse = "|"))
      
      # Add Multiracial if: more than one race mentioned (regardless of Hispanic status)
      if (race_count > 1) groupings <- c(groupings, "Multiracial")
      
      # Fallback if still empty
      if (length(groupings) == 0) groupings <- "Multiracial"
      
      return(unique(groupings))
    })
  ) %>%
  unnest(heatmap_groups) %>%
  mutate(
    heatmap_groups = factor(heatmap_groups, levels = re_groups_levels)
  )

# Order re_text correctly within re_groups
ordered_re_text <- std_heatmap_data %>%
  mutate(
    sort_order = case_when(
      re_groups == "Asian" & re_text %in% c("Other Asian", "Unspecified Asian") ~ 2,  # Move to bottom of Asian
      re_groups == "NHPI" & re_text %in% c("Other Pacific Islander", "Unspecified NHPI") ~ 2,  # Move to bottom of NHPI
      TRUE ~ 1  # Default alphabetical order
    )
  ) %>%
  arrange(re_groups, sort_order, re_text) %>%  # First sort by group, then order, then alphabetize
  pull(re_text) %>%
  unique()

# Apply correct factor levels to re_text
std_heatmap_data <- std_heatmap_data %>%
  mutate(re_text = factor(re_text, levels = ordered_re_text))

# ---------------------- #
# INSERT VERTICAL GAP COLUMNS BETWEEN OUTCOME CATEGORIES
# ---------------------- #

# Build ordered outcome list with gap markers
outcome_order_df <- std_heatmap_data %>%
  select(outcome, category_labels) %>%
  distinct() %>%
  mutate(category_labels = factor(category_labels, levels = category_order)) %>%
  arrange(category_labels) %>%
  mutate(outcome = as.character(outcome))

# Create gap outcome labels (e.g. "gap_1", "gap_2", ...)
gap_outcomes <- outcome_order_df %>%
  group_by(category_labels) %>%
  summarise(max_outcome = last(outcome), .groups = "drop") %>%
  mutate(gap_outcome = paste0("gap_", row_number())) %>%
  pull(gap_outcome)

# Interleave gap outcomes after each category
outcome_order_with_gaps <- outcome_order_df %>%
  group_by(category_labels) %>%
  summarise(outcomes = list(outcome), .groups = "drop") %>%
  mutate(outcomes = map2(outcomes, row_number(), function(outs, i) {
    if (i == length(category_order)) {
      outs
    } else {
      c(outs, paste0("gap_", i))
    }
  })) %>%
  pull(outcomes) %>%
  flatten_chr()

# Create dummy rows for each gap × re_text × heatmap_group
row_structure <- std_heatmap_data %>%
  distinct(re_text, heatmap_groups)

gap_data <- expand.grid(
  outcome = outcome_order_with_gaps[outcome_order_with_gaps %in% paste0("gap_", seq_len(10))],
  re_text = unique(std_heatmap_data$re_text),
  stringsAsFactors = FALSE
) %>%
  left_join(row_structure, by = "re_text") %>%
  mutate(
    prevalence = NA,
    RSE = NA,
    label = "",
    label_cell = FALSE,
    Normalized_Values = NA,
    RSE_category = "Low",
    category_labels = NA  # optional
  )

# Bind in gap rows and refactor outcome
std_heatmap_data <- bind_rows(std_heatmap_data, gap_data) %>%
  mutate(outcome = factor(outcome, levels = outcome_order_with_gaps))

# Make each gap label unique but visually blank
gap_levels <- grep("^gap_", levels(std_heatmap_data$outcome), value = TRUE)
replacement_labels <- strrep(" ", seq_along(gap_levels))  # " ", "  ", "   ", ...

levels(std_heatmap_data$outcome) <- levels(std_heatmap_data$outcome) %>%
  replace(match(gap_levels, .), replacement_labels)


# ---------------------- #
# PROCESS OUTCOME AND CALCULATE MIN/MAX NORMALIZED PREVALENCE 
# ---------------------- #

# Normalize values within each outcome and convert prevalence to character
std_heatmap_data <- std_heatmap_data %>%
  group_by(outcome) %>%
  mutate(
    Normalized_Values = (prevalence - min(prevalence, na.rm = TRUE)) /
      (max(prevalence, na.rm = TRUE) - min(prevalence, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    text_color = ifelse(Normalized_Values > 0.5, "white", "black")
  ) %>% 
  mutate(
    prevalence = as.character(round(std_heatmap_data$prevalence*100,1))
  )

# ---------------------- #
# CATEGORIZE RSE AS HIGH OR LOW
# ---------------------- #

# Create the RSE_category based on the RSE values
std_heatmap_data <- std_heatmap_data %>%
  mutate(
    RSE_category = case_when(
      RSE < 30 ~ "Low",  # Low RSE: transparent (no pattern)
      RSE >= 30 ~ "High",  # Medium RSE: crosshatch
      TRUE ~ "Low"  # For 0% prevalence outcomes
    )
  )

# ---------------------- #
# ID HIGHEST AND LOWEST PREVALENCE GROUPS WITHIN EACH OUTCOME
# ---------------------- #

# Identify rows to label: highest and lowest prevalence per outcome
label_flags <- std_heatmap_data %>%
  filter(!is.na(outcome)) %>%
  group_by(outcome) %>%
  mutate(
    max_flag = prevalence == max(as.numeric(prevalence), na.rm = TRUE),
    min_flag = prevalence == min(as.numeric(prevalence), na.rm = TRUE),
    label_cell = max_flag | min_flag
  ) %>%
  ungroup() %>%
  select(outcome, re_text, heatmap_groups, label_cell)

# Get only distinct rows
label_flags <- label_flags %>%
  distinct(outcome, re_text, heatmap_groups, .keep_all = TRUE)

# Join back using all keys including heatmap_groups
std_heatmap_data <- std_heatmap_data %>%
  left_join(label_flags, by = c("outcome", "re_text", "heatmap_groups")) 

# Clean up duplicate columns from join
std_heatmap_data <- std_heatmap_data %>%
  select(-label_cell.x) %>%                         # remove old version
  rename(label_cell = label_cell.y) %>%             # keep the joined version
  mutate(label_cell = ifelse(is.na(label_cell), FALSE, label_cell))  # fill NAs as FALSE

# Create label column to pass to aes in ggplot
std_heatmap_data <- std_heatmap_data %>%
  mutate(
    label = case_when(
      RSE >= 30 & label_cell ~ paste0(prevalence, "*"),
      RSE >= 30 & !label_cell ~ "*",
      RSE < 30 & label_cell ~ prevalence,
      TRUE ~ ""  # For all other cells
    )
  )

# ---------------------- #
# CREATE HEATMAP LABELING ONLY MIN/MAX GROUP PER OUTCOME 
# ---------------------- #

# Modified heatmap plot with default reversed blue color scale for normalized prevalence and gray crosshatch visibility
heatmap_minmax <- ggplot(std_heatmap_data, aes(x = outcome, y = fct_rev(re_text),
                                             pattern = RSE_category, fill = Normalized_Values)) +  
  
  geom_tile(aes(fill = Normalized_Values), color = NA)+
  
  # Set the pattern types for RSE categories
  scale_pattern_manual(name = "RSE Level",
                       values = c(
                         Low = "none",       # No pattern for Low RSE
                         High = "stripe",    # Stripe pattern for High RSE
                         Unknown = "none"    # No pattern for Unknown (NA) RSE category
                       )) +
  
  # Default ggplot blue color gradient for normalized prevalence (reversed)
  scale_fill_gradient(name = "Normalized prevalence", 
                      trans = "reverse",  # Reverse the default ggplot blue-to-white gradient
                      na.value = "white") +  # Handle NAs with white
  
  # Add text labels with manual color assignment
  geom_text(data = subset(std_heatmap_data, label != ""),
            aes(label = label), color = "white", size = 3, hjust = 0.5) +
  
  labs(title = NULL, x = NULL, y = NULL) +
  
  facet_grid(rows = vars(heatmap_groups), scales = "free_y", space = "free_y") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    
    strip.text.y = element_text(angle = 0, hjust = 0.5),
    strip.text.x = element_text(size = 10),
    panel.spacing.x = unit(0.5, "lines")  # Adds padding between facets
  ) +
  
  # Adjust guides for the legends for normalized prevalence and RSE
  guides(
    fill = guide_colorbar(title = "Normalized prevalence"),  # For normalized prevalence
    pattern = guide_legend(title = "RSE Level")  # For RSE category patterns
  )

# Plot
heatmap_minmax

# ---------------------- #
# CREATE FULL HEATMAP WITH ALL LABEL VALUES USING ggpattern
# ---------------------- #

# Modified heatmap plot with default reversed blue color scale for normalized prevalence and gray crosshatch visibility
heatmap_plot <- ggplot(std_heatmap_data, aes(x = outcome, y = fct_rev(re_text),
                                             pattern = RSE_category, fill = Normalized_Values)) +  
  geom_tile_pattern(
    aes(pattern = RSE_category),  # Apply patterns based on RSE_category
    pattern_color = NA,  # Use medium gray for line color
    pattern_fill = "gray",  # Red pattern fill color
    pattern_angle = 45,  # Adjust pattern angle for crosshatch
    pattern_density = 0.2,  # Use adjusted pattern density
    pattern_spacing = 0.01,  # Use adjusted pattern spacing
    pattern_key_scale_factor = 1,  # Control the size of the pattern key (legend)
    show.legend = TRUE  # Do not display legend for patterns here, handled separately
  ) +
  
  # Set the pattern types for RSE categories
  scale_pattern_manual(name = "RSE Level",
  values = c(
    Low = "none",       # No pattern for Low RSE
    High = "stripe",    # Stripe pattern for High RSE
    Unknown = "none"    # No pattern for Unknown (NA) RSE category
  )) +
  
  # Default ggplot blue color gradient for normalized prevalence (reversed)
  scale_fill_gradient(name = "Normalized prevalence", 
                      trans = "reverse",  # Reverse the default ggplot blue-to-white gradient
                      na.value = "white") +  # Handle NAs with white
  
  # Add text labels with manual color assignment
  geom_text(aes(label = prevalence, color = text_color, hjust = 0.5), size = 3, na.rm = FALSE) +  
  scale_color_manual(values = c("black" = "black", "white" = "white"),
                     guide = "none") +  # Manually set black and white colors for text
  
  labs(title = NULL, x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    
    strip.text.y = element_text(angle = 0, hjust = 0.5),
    strip.text.x = element_text(size = 10),
    panel.spacing.x = unit(0.5, "lines")  # Adds padding between facets
  ) +
  
  # Adjust guides for the legends for normalized prevalence and RSE
  guides(
    fill = guide_colorbar(title = "Normalized prevalence"),  # For normalized prevalence
    pattern = guide_legend(title = "RSE Level")  # For RSE category patterns
  )

# Plot
heatmap_plot

# ---------------------- #
# CREATE SUMMARY TABLE CHARACTERIZING MIN AND MAX FOR EACH OUTCOME
# ---------------------- #

# Summarize data into a min-max table
min_max_prevalence <- final_combined_df %>%
  filter(
    adjustment_type == "Standardized",
    RSE <= 30, 
    re_text != "DK/R") %>%  # Exclude high RSE and "DK/R"
  group_by(outcome) %>%
  summarise(
    min_group = re_text[which.min(prevalence)], 
    min_prevalence = min(prevalence, na.rm = TRUE) * 100,
    min_CI_lower = (min(prevalence, na.rm = TRUE) - 1.96 * min(se_prevalence, na.rm = TRUE)) * 100,
    min_CI_upper = (min(prevalence, na.rm = TRUE) + 1.96 * min(se_prevalence, na.rm = TRUE)) * 100,
    
    max_group = re_text[which.max(prevalence)], 
    max_prevalence = max(prevalence, na.rm = TRUE) * 100,
    max_CI_lower = (max(prevalence, na.rm = TRUE) - 1.96 * max(se_prevalence, na.rm = TRUE)) * 100,
    max_CI_upper = (max(prevalence, na.rm = TRUE) + 1.96 * max(se_prevalence, na.rm = TRUE)) * 100,
    
    prevalence_diff = round(max_prevalence - min_prevalence, 1),  # Calculate & round difference
    prevalence_ratio = round(max_prevalence / min_prevalence, 1)  # Calculate & round ratio
  ) %>%
  mutate(
    outcome_label = variable_labels[outcome],  # Use human-readable labels
    min_prevalence_CI = sprintf("%.1f (%.1f - %.1f)", min_prevalence, min_CI_lower, min_CI_upper),
    max_prevalence_CI = sprintf("%.1f (%.1f - %.1f)", max_prevalence, max_CI_lower, max_CI_upper),
    outcome_category = outcome_cats[outcome]  # Assign category label
  ) %>%
  select(outcome_category, outcome_label, min_group, min_prevalence_CI, 
         max_group, max_prevalence_CI, prevalence_diff, prevalence_ratio) %>%  # Include new columns
  arrange(factor(outcome_category, levels = unique(outcome_cats)), 
          factor(outcome_label, levels = variable_labels))

# Put into a gt table format
min_max_gt <-  min_max_prevalence %>%
  gt(groupname_col = "outcome_category") %>%  # Group by health indicator category
  cols_label(
    outcome_label = "Health Indicator",
    min_group = "Lowest Prevalence Group",
    min_prevalence_CI = "Lowest Prevalence (95% CI)",
    max_group = "Highest Prevalence Group",
    max_prevalence_CI = "Highest Prevalence (95% CI)",
    prevalence_diff = "Prevalence Difference",
    prevalence_ratio = "Prevalence Ratio"
  ) %>%
  fmt_number(
    columns = c(prevalence_diff, prevalence_ratio),
    decimals = 1
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center",
    row_group.font.weight = "bold",  # Bold sub-header rows
    row_group.border.top.width = px(2),  # Add top border for each category
    row_group.border.top.color = "black"
  )

print(min_max_gt)

# ---------------------- #
# SAVE FILES TO RESULTS DIRECTORY
# ---------------------- #

# Save the heatmap plot as a PNG file
ggsave(file.path(results_dir, "08_heatmap.png"), plot = heatmap_plot, 
       width = 14, height = 10, units = "in", dpi = 300)

# Save the heatmap plot as a PDF file
ggsave(file.path(results_dir, "08_heatmap.pdf"), plot = heatmap_plot, 
       width = 14, height = 10, units = "in", dpi = 300)

# Save the min-max table as a DOCX file
gtsave(min_max_gt, file.path(results_dir, "09_min_max_prevalence_table.docx"))

# End of script
message("08_visualize_data.R completed successfully.")