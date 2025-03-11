# 03_define_race_categories.R 
# Purpose: Recode and define detailed race/ethnicity categories

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD CLEANED DATA AND PREP RACE CATEGORIES
# ---------------------- #

# Load cleaned dataset from previous step
ca_df <- readRDS(file.path(processed_data_dir, "ca_cleaned.rds"))

# Function to reorder digits in a number (preserves categorical meaning)
reorder_digits <- function(num) {
  if (num < 10) {
    num  # If the number has only one digit, return it
  } else {
    digits <- as.numeric(strsplit(as.character(num), '')[[1]])
    sorted_digits <- sort(digits)
    as.numeric(paste(sorted_digits, collapse = ''))
  }
}

# Apply function to Hispanic variable
ca_df <- ca_df %>%
  mutate(hisp = as.numeric(hisp), hisp = sapply(hisp, reorder_digits))

# Standardize race and Hispanic variables
ca_df <- ca_df %>%
  mutate(race = as.character(race), hisp = as.character(hisp))

# Fix inconsistent entries
ca_df <- ca_df %>%
  mutate(
    race = case_when(
      race == "1044" ~ "1040",
      race == "4044" ~ "40",
      race == "4460" ~ "4060",
      race == "5050" ~ "50",
      TRUE ~ race
    )
  )

# Create race and ethnicity lookup codes
race_lookup <- c(
  "10" = "White",
  "20" = "Black",
  "30" = "AIAN",
  "40" = "Asian",
  "41" = "Asian Indian",
  "42" = "Chinese",
  "43" = "Filipino",
  "44" = "Japanese",
  "45" = "Korean",
  "46" = "Vietnamese",
  "47" = "Other Asian",
  "50" = "NHPI",
  "51" = "Native Hawaiian",
  "52" = "Guamanian",
  "53" = "Samoan",
  "54" = "Other Pacific Islander",
  "60" = "Other Race",
  "77" = "DK/R",
  "88" = "DK/R",
  "99" = "DK/R"
)

hisp_lookup <- c(
  "1" = "Mexican",
  "2" = "Puerto Rican",
  "3" = "Cuban",
  "4" = "Other Hispanic",
  "5" = "",
  "7" = "",
  "77" = "",
  "9" = "",
  "99" = ""
)

# Convert numeric codes to text based on lookups
ca_df <- ca_df %>%
  mutate(
    # Ensure multi-digit race and Hispanic codes are properly separated BEFORE replacement
    race_separated = str_replace_all(race, "(\\d{2})(?=\\d)", "\\1-"),
    hisp_separated = case_when(
      hisp %in% c("77", "99") ~ hisp,
      # Keep "77" and "99" unchanged
      TRUE ~ str_replace_all(hisp, "(\\d)(?=\\d)", "\\1-")  # Apply hyphenation to other cases
    ),
    
    # Replace numeric codes with text labels
    race_text = str_replace_all(race_separated, race_lookup),
    hisp_text = str_replace_all(hisp_separated, hisp_lookup),
    
    # Drop "Other Race" and "DK/R" if ANY Hispanic category is present
    race_text = case_when(
      str_detect(race_text, "Other Race|DK/R") & hisp_text != "" ~ "",
      TRUE ~ race_text
    ),
    
    # Split race and Hispanic into lists for sorting
    race_components = str_split(race_text, "-"),
    hisp_components = str_split(hisp_text, "-"),
    
    # Sort race components alphabetically, but keep Hispanic first
    sorted_race = map_chr(race_components, ~ paste(sort(unique(
      .x
    )), collapse = "-")),
    sorted_hisp = map_chr(hisp_components, ~ paste(sort(unique(
      .x
    )), collapse = "-")),
    
    # Combine Hispanic and Race (Hispanic first if present)
    re_text = case_when(
      sorted_hisp != "" &
        sorted_race != "" ~ paste(sorted_hisp, sorted_race, sep = "-"),
      sorted_hisp != "" &
        sorted_race == "" ~ sorted_hisp,
      # Only Hispanic
      TRUE ~ sorted_race
    )  # Only race (no Hispanic)
  ) %>%
  select(
    -race_separated,
    -hisp_separated,
    -race_components,
    -hisp_components,
    -sorted_race,
    -sorted_hisp
  )  # Remove temp columns

# Count group sizes
group_counts <- ca_df %>%
  count(re_text, name = "n")

# Identify small groups (n < 50)
small_groups <- group_counts %>%
  filter(n < 50) %>%
  pull(re_text)

# Phase 1: Apply subgroup reclassification
ca_df <- ca_df %>%
  mutate(
    re_text = ifelse(
      re_text %in% small_groups,
      re_text %>%
        str_replace_all("\\b(Native Hawaiian|Guamanian|Samoan|Other Pacific Islander|Pacific Islander)\\b", "NHPI") %>%
        str_replace_all("\\b(Chinese|Filipino|Japanese|Korean|Vietnamese|Asian Indian|Other Asian)\\b", "Asian") %>%
        str_replace_all("\\b(Mexican|Puerto Rican|Cuban|Other Hispanic)\\b", "Hispanic") %>%
        str_replace_all("\\b(Hispanic-)+Hispanic\\b", "Hispanic"),  # Prevents "Hispanic-Hispanic"
      re_text
    )
  ) %>%
  # Remove repeated occurrences of "Hispanic-" (e.g., "Hispanic-Hispanic" → "Hispanic")
  mutate(re_text = str_replace_all(re_text, "\\b(Hispanic-)+Hispanic\\b", "Hispanic"))


# If "Other Hispanic" is the only Hispanic category, replace it with "Hispanic"
ca_df <- ca_df %>%
  mutate(
    re_text = case_when(
      str_detect(re_text, "\\bOther Hispanic\\b") & !str_detect(re_text, "Mexican|Puerto Rican|Cuban") ~ 
        str_replace(re_text, "Other Hispanic", "Hispanic"),
      TRUE ~ re_text
    )
  )

# Phase 2: Aggregate small groups using priority order
group_counts <- ca_df %>%
  count(re_text, name = "n")

small_groups <- group_counts %>%
  filter(n < 50) %>%
  pull(re_text)

ca_df <- ca_df %>%
  mutate(re_text = ifelse(
    re_text %in% small_groups,
    case_when(
      str_detect(re_text, "NHPI") ~ "NHPI-Multiple",
      str_detect(re_text, "AIAN") ~ "AIAN-Multiple",
      str_detect(re_text, "Black") ~ "Black-Multiple",
      str_detect(re_text, "Asian") ~ "Asian-Multiple",
      str_detect(re_text, "Hispanic") ~ "Hispanic-Multiple",
      TRUE ~ "DK/R"
    ),
    re_text
  ))

# Remove unneeded columns
ca_df <- ca_df %>% 
  select(-c("race_text", "hisp_text"))

# Final group counts
final_group_counts <- ca_df %>%
  count(re_text, name = "final_n")

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save cleaned dataset
saveRDS(ca_df, file.path(processed_data_dir, "ca_race_cleaned.rds"))
message(
  "Saved cleaned BRFSS dataset: ",
  file.path(processed_data_dir, "ca_race_cleaned.rds")
)

# End of script
message("03_define_race_categories.R completed successfully.")