# 08_visualize_data.R
# Purpose: Generate heatmap visualizations for standardized prevalence.

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #
library(tidyverse)
library(ggpattern)
library(gt)

source("config.R")
source("setup.R")

# ---------------------- #
# LOAD DATA AND SETUP LABELS
# ---------------------- #
final_combined_df <- readRDS(file.path(processed_data_dir, "07_prevalence_estimates.rds"))

category_order <- c("Lifestyle factors", "Preventive healthcare", "Chronic conditions", "General health status", "Disabilities")

plot_var_labs <- c(
  smoke = "Ever\nsmoked", drink_any = "Any\nalcohol", drink_binge = "Binge\ndrinking", exer_no = "No\nexercise",
  hivtest_no = "Never\nHIV test", laiv_no = "No\nflu shot", pcv_no = "No\npneum. vax", ins_no = "No\ninsurance",
  chk2_no = "No\ncheckup\n(2y)", pcp_no = "No\nPCP",
  apcvd = "Angina or\nCHD", mi = "Heart\nattack", dm = "Diabetes", ckd = "Kidney\ndisease",
  ast_lt = "Lifetime\nasthma", ast_now = "Current\nasthma", copd = "COPD", bcc = "Skin\ncancer", mdd = "Depressive\ndisorder",
  ovwob = "Ovwt.\nor obese",
  fphs = "Poor/fair\nhealth", mphd_poor = "Freq. poor\nphys hlth", mmhd_poor = "Freq. poor\nment. hlth",
  vip = "Vision\nimpair", dis_walk = "Diff.\nwalking", dis_conc = "Cognitive\ndiff.",
  dis_err = "Diff.\nerrands", dis_dress = "Diff.\ndressing"
)

long_labels <- variable_labels[names(plot_var_labs)]
short_labels <- plot_var_labs[names(long_labels)]
names(short_labels) <- unname(long_labels)

# ---------------------- #
# INITIAL CLEANING AND LABELING
# ---------------------- #
std_heatmap_data <- final_combined_df %>%
  filter(adjustment_type == "Standardized") %>%
  mutate(
    re_groups = case_when(
      re_text %in% hispanic_groups ~ "Hispanic",
      re_text %in% names(re_groups_lookup) ~ re_groups_lookup[re_text],
      TRUE ~ "Multiracial"
    ),
    re_groups = factor(re_groups, levels = re_groups_levels),
    category_labels = factor(outcome_cats[outcome], levels = category_order),
    outcome = variable_labels[outcome],
    outcome = if_else(outcome %in% names(short_labels), short_labels[outcome], outcome)
  ) %>%
  select(-c(adjustment_type, se_prevalence, age_grp, sex))

# ---------------------- #
# ADD HEATMAP GROUPS
# ---------------------- #

# Define function for "alone or in combination" groupings
assign_groupings <- function(df, re_groups_levels) {
  df %>%
    mutate(
      re_groups = case_when(
        re_text %in% hispanic_groups ~ "Hispanic",
        re_text %in% names(re_groups_lookup) ~ re_groups_lookup[re_text],
        TRUE ~ "Multiracial"
      ),
      re_groups = factor(re_groups, levels = re_groups_levels),
      heatmap_groups = map(re_text, function(group) {
        groupings <- c()
        if (str_detect(group, "Hispanic|Mexican|Puerto Rican|Cuban|Other Hispanic")) groupings <- c(groupings, "Hispanic")
        if (str_detect(group, "AIAN")) groupings <- c(groupings, "AIAN")
        if (str_detect(group, "Asian|Chinese|Filipino|Japanese|Vietnamese|Korean|Asian Indian|Other Asian|Unspecified Asian")) groupings <- c(groupings, "Asian")
        if (str_detect(group, "Black")) groupings <- c(groupings, "Black")
        if (str_detect(group, "White")) groupings <- c(groupings, "White")
        if (str_detect(group, "Pacific Islander|NHPI|Samoan|Guamanian|Hawaiian|Native Hawaiian|Other Pacific Islander|Unspecified NHPI")) groupings <- c(groupings, "NHPI")
        if (str_detect(group, "Other Race")) groupings <- c(groupings, "Other Race")
        if (str_detect(group, "DK/R")) groupings <- c(groupings, "DK/R")
        race_count <- str_count(group, paste(c("AIAN", "Asian", "Black", "White", "NHPI", "Pacific Islander", "Native Hawaiian", "Samoan", "Guamanian", "Multiple"), collapse = "|"))
        if (race_count > 1) groupings <- c(groupings, "Multiracial")
        if (length(groupings) == 0) groupings <- "Multiracial"
        return(unique(groupings))
      })
    ) %>%
    unnest(heatmap_groups) %>%
    mutate(heatmap_groups = factor(heatmap_groups, levels = re_groups_levels))
}

# Apply to std_heatmap_data
std_heatmap_data <- assign_groupings(std_heatmap_data, re_groups_levels)

# ---------------------- #
# INSERT GAPS
# ---------------------- #
outcome_order_df <- std_heatmap_data %>%
  select(outcome, category_labels) %>%
  distinct() %>%
  arrange(category_labels) %>%
  mutate(outcome = as.character(outcome))

outcome_order_with_gaps <- outcome_order_df %>%
  group_by(category_labels) %>%
  summarise(outcomes = list(outcome), .groups = "drop") %>%
  mutate(outcomes = map2(outcomes, row_number(), function(outs, i) {
    if (i == length(category_order)) outs else c(outs, paste0("gap_", i))
  })) %>%
  pull(outcomes) %>%
  flatten_chr()

row_structure <- std_heatmap_data %>% distinct(re_text, heatmap_groups)

gap_map <- outcome_order_df %>%
  group_by(category_labels) %>%
  summarise(max_outcome = last(outcome), .groups = "drop") %>%
  mutate(gap_outcome = paste0("gap_", row_number())) %>%
  select(category_labels, outcome = gap_outcome)

gap_data <- expand.grid(
  outcome = gap_map$outcome,
  re_text = unique(std_heatmap_data$re_text),
  stringsAsFactors = FALSE
) %>%
  left_join(row_structure, by = "re_text") %>%
  left_join(gap_map, by = "outcome") %>%
  mutate(
    prevalence = NA, RSE = NA, label = "", label_cell = FALSE,
    Normalized_Values = NA, RSE_category = "Low"
  )

# Recreate gap levels based on your earlier logic (gap_1 to gap_4)
gap_levels <- paste0("gap_", seq_len(length(category_order) - 1))  # e.g., "gap_1", "gap_2", ...
replacement_labels <- strrep(" ", seq_along(gap_levels))

# Replace in the level list
outcome_order_with_gaps_labeled <- outcome_order_with_gaps
gap_matches <- match(gap_levels, outcome_order_with_gaps_labeled)

if (any(is.na(gap_matches))) {
  stop("Some gap labels not found in outcome_order_with_gaps_labeled. Match failed.")
}

outcome_order_with_gaps_labeled[gap_matches] <- replacement_labels

# Apply updated factor levels (with gaps as whitespace)
std_heatmap_data <- std_heatmap_data %>%
  mutate(outcome = factor(outcome, levels = outcome_order_with_gaps_labeled))

# Apply factor levels to outcome, using the relabeled version
std_heatmap_data <- std_heatmap_data %>%
  mutate(outcome = factor(outcome, levels = outcome_order_with_gaps_labeled))

# ---------------------- #
# RELABEL OUTCOMES WITH SHORT LABELS
# ---------------------- #
old_levels <- levels(std_heatmap_data$outcome)
new_levels <- old_levels
matched <- old_levels %in% names(short_labels)
new_levels[matched] <- short_labels[old_levels[matched]]
levels(std_heatmap_data$outcome) <- new_levels


# ---------------------- #
# FINAL PROCESSING
# ---------------------- #

std_heatmap_data <- std_heatmap_data %>%
  group_by(outcome) %>%
  mutate(Normalized_Values = (prevalence - min(prevalence, na.rm = TRUE)) /
           (max(prevalence, na.rm = TRUE) - min(prevalence, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(
    prevalence = as.character(round(prevalence * 100, 1)),
    text_color = ifelse(Normalized_Values > 0.5, "white", "black"),
    RSE_category = case_when(
      RSE < 30 ~ "Low",
      RSE >= 30 ~ "High",
      TRUE ~ "Low"
    )
  )

label_flags <- std_heatmap_data %>%
  mutate(outcome_chr = as.character(outcome), prevalence_num = as.numeric(prevalence)) %>%
  filter(!grepl("^\\s*$", outcome_chr)) %>%
  group_by(outcome_chr) %>%
  mutate(
    max_flag = prevalence_num == max(prevalence_num, na.rm = TRUE),
    min_flag = prevalence_num == min(prevalence_num, na.rm = TRUE),
    label_cell = max_flag | min_flag
  ) %>%
  ungroup() %>%
  select(outcome = outcome_chr, re_text, heatmap_groups, label_cell, max_flag, min_flag) %>%
  distinct()

std_heatmap_data <- std_heatmap_data %>%
  left_join(label_flags, by = c("outcome", "re_text", "heatmap_groups")) %>%
  mutate(
    label_cell = ifelse(is.na(label_cell), FALSE, label_cell),
    label = case_when(
      RSE >= 30 & label_cell ~ paste0(prevalence, "*"),
      RSE >= 30 & !label_cell ~ "*",
      RSE < 30 & label_cell ~ prevalence,
      TRUE ~ ""
    )
  )

std_heatmap_data <- std_heatmap_data %>%
  mutate(outcome = factor(outcome, levels = outcome_order_with_gaps_labeled))

# ---------------------- #
# CREATE FIGURE 1 
# ---------------------- #

# Create min-max heatmap in ggplot
heatmap_minmax <- ggplot(std_heatmap_data, aes(x = outcome, y = fct_rev(re_text),
                                               pattern = RSE_category, fill = Normalized_Values)) +  
  geom_tile(aes(fill = Normalized_Values), color = NA) +
  scale_pattern_manual(name = "RSE Level", values = c(Low = "none", High = "stripe", Unknown = "none")) +
  scale_fill_gradient(name = "Normalized prevalence", trans = "reverse", na.value = "white") +
  geom_text(data = subset(std_heatmap_data, label != ""), aes(label = label),
            color = "white", size = 3, hjust = 0.5) +
  scale_x_discrete(drop = FALSE) +   # <- **This line is new and key**
  labs(title = NULL, x = NULL, y = NULL) +
  facet_grid(rows = vars(heatmap_groups), scales = "free_y", space = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)),
    axis.text.y = element_text(size = rel(0.9)),
    strip.text.y = element_text(size = rel(1.0)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(0.5, "lines"),
    legend.position = "bottom",
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.margin = margin(t = -5, unit = "pt"),  # reduces top margin of legend
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
  ) +
  guides(fill = guide_colorbar(title = "Normalized prevalence"),
         pattern = guide_legend(title = "RSE Level"))

# Plot
heatmap_minmax

# ---------------------- #
# CREATE TABLE 2
# ---------------------- #

# Apply alone or in combination function to final_combined_df
final_combined_df <- assign_groupings(final_combined_df, re_groups_levels)

# Filter for Multiracial subgruops and summarize data into a min-max table
min_max_tab <- final_combined_df %>%
  filter(
    heatmap_groups == "Multiracial",
    adjustment_type == "Standardized",
    re_text != "DK/R") %>%
  group_by(outcome) %>%
  summarise(
    min_group = re_text[which.min(prevalence)], 
    min_prevalence = min(prevalence, na.rm = TRUE) * 100,
    min_CI_lower = pmax(0, (min(prevalence, na.rm = TRUE) - 1.96 * min(se_prevalence, na.rm = TRUE)) * 100),
    min_CI_upper = (min(prevalence, na.rm = TRUE) + 1.96 * min(se_prevalence, na.rm = TRUE)) * 100,
    
    max_group = re_text[which.max(prevalence)], 
    max_prevalence = max(prevalence, na.rm = TRUE) * 100,
    max_CI_lower = pmax(0, (max(prevalence, na.rm = TRUE) - 1.96 * max(se_prevalence, na.rm = TRUE)) * 100),
    max_CI_upper = (max(prevalence, na.rm = TRUE) + 1.96 * max(se_prevalence, na.rm = TRUE)) * 100,
    
    prevalence_diff = round(max_prevalence - min_prevalence, 1),
    prevalence_ratio = round(max_prevalence / min_prevalence, 1)
  ) %>%
  mutate(
    outcome_label = variable_labels[outcome],
    min_prevalence_CI = sprintf("%.1f (%.1f - %.1f)", min_prevalence, min_CI_lower, min_CI_upper),
    max_prevalence_CI = sprintf("%.1f (%.1f - %.1f)", max_prevalence, max_CI_lower, max_CI_upper),
    outcome_category = outcome_cats[outcome]
  ) %>%
  select(outcome_category, outcome_label, min_group, min_prevalence_CI, 
         max_group, max_prevalence_CI, prevalence_diff, prevalence_ratio) %>%
  arrange(factor(outcome_category, levels = unique(outcome_cats)), 
          factor(outcome_label, levels = variable_labels))

# Put into a gt table format
min_max_gt <-  min_max_tab %>%
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
# CREATE APPENDIX TABLE OF ALL PREVALENCE AND CI ESTIMATES
# ---------------------- #
wide_appendix_tab <- final_combined_df %>%
  filter(adjustment_type == "Standardized") %>%
  mutate(
    re_groups = case_when(
      re_text %in% hispanic_groups ~ "Hispanic",
      re_text %in% names(re_groups_lookup) ~ re_groups_lookup[re_text],
      TRUE ~ "Multiracial"
    )
  ) %>%
  group_by(re_text, re_groups, outcome) %>%
  summarise(
    prevalence = mean(prevalence, na.rm = TRUE),
    se = mean(se_prevalence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prevalence = prevalence * 100,
    lower = pmax(0, prevalence - 1.96 * se * 100),
    upper = pmin(100, prevalence + 1.96 * se * 100),
    estimate_ci = sprintf("%.1f (%.1f–%.1f)", prevalence, lower, upper),
    short_label = plot_var_labs[outcome]
  ) %>%
  select(re_text, short_label, estimate_ci) %>%
  pivot_wider(
    names_from = short_label,
    values_from = estimate_ci
  ) %>%
  arrange(re_text)

wide_appendix_gt <- wide_appendix_tab %>%
  gt(rowname_col = "re_text") %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center",
    row_group.font.weight = "bold",
    row_group.border.top.width = px(2),
    row_group.border.top.color = "black"
  )

print(wide_appendix_gt)


# ---------------------- #
# SAVE FILES TO RESULTS DIRECTORY
# ---------------------- #

# Save the min-max heatmap plot as a PNG file
ggsave(file.path(results_dir, "08_heatmap_minmax.pdf"), plot = heatmap_minmax, 
       width = 14, height = 10, units = "in")

# Save the min-max table as a DOCX file
gtsave(min_max_gt, file.path(results_dir, "09_table_minmax.docx"))

# Save the min-max table as a DOCX file
gtsave(wide_appendix_gt, file.path(results_dir, "10_table_appendix.docx"))

# End of script
message("08_visualize_data.R completed successfully.")