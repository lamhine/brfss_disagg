# 08_visualize_data.R
# Purpose: Generate heatmap visualizations for standardized prevalence.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(ggpattern)  # For ggpattern to add patterns

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
      str_detect(re_text, "Mexican|Puerto Rican|Cuban|Multiple Hispanic") ~ "Hispanic",
      re_text == "White" ~ "White",
      re_text == "Black" ~ "Black",
      re_text == "AIAN" ~ "AIAN",
      re_text %in% c("Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Unspecified Asian") ~ "Asian", 
      re_text %in% c("Pacific Islander", "Native Hawaiian", "Guamanian", "Samoan", "Other Pacific Islander", "Unspecified NHPI") ~ "NHPI",
      re_text %in% c("Other Race", "DK/R") ~ "Other Race",
      TRUE ~ "Multiracial"
    ),
    
    # Define the correct order of re_groups
    re_groups = factor(re_groups, levels = c("AIAN", "Asian", "Black", "Hispanic", "Multiracial", "NHPI", "White", "Other Race")),
    
    # Ensure correct order for category labels
    category_labels = factor(outcome_cats[outcome], levels = category_order),
    
    # Replace outcome variable names with labels
    outcome = variable_labels[outcome]
  ) %>% 
  select(-c(adjustment_type, se_prevalence, age_grp, sex))

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
# PROCESS OUTCOME AND CALCULATE MIN/MAX NORMALIZED PREVALENCE 
# ---------------------- #

# Step 1: Ensure outcome is a factor in the correct order
std_heatmap_data <- std_heatmap_data %>%
  mutate(outcome = factor(outcome, levels = variable_labels[names(outcome_cats)]))

# Step 2: Replace variable names with human-readable labels
levels(std_heatmap_data$outcome) <- variable_labels[names(outcome_cats)]

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
      TRUE ~ "Unknown"  # Handle any missing or unexpected RSE values
    )
  )


# ---------------------- #
# CREATE HEATMAP WITH ggpattern
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
# SAVE FILES TO RESULTS DIRECTORY
# ---------------------- #

# Save the plot as a PNG file
ggsave(file.path(results_dir, "08_heatmap.png"), plot = heatmap_plot, 
       width = 14, height = 10, units = "in", dpi = 300)

# Save the plot as a PDF file
ggsave(file.path(results_dir, "08_heatmap.pdf"), plot = heatmap_plot, 
       width = 14, height = 10, units = "in", dpi = 300)

# End of script
message("08_visualize_data.R completed successfully.")