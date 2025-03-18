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
ca_df <- readRDS(file.path(processed_data_dir, "02_ca_cleaned.rds"))

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
      is.na(race) & hisp == "5" ~ "DK/R",
      TRUE ~ race
      )
  )

# Create race, Hispanic ethnicity, and subgroup lookup codes
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

subgroup_lookup <- c(
  "Mexican" = "Hispanic",
  "Puerto Rican" = "Hispanic",
  "Cuban" = "Hispanic", 
  "Other Hispanic" = "Hispanic",
  "Asian Indian" = "Asian", 
  "Chinese" = "Asian", 
  "Filipino" = "Asian", 
  "Japanese" = "Asian", 
  "Korean" = "Asian", 
  "Vietnamese" = "Asian", 
  "Other Asian" = "Asian", 
  "Native Hawaiian" = "NHPI", 
  "Guamanian" = "NHPI", 
  "Samoan" = "NHPI", 
  "Other Pacific Islander" = "NHPI"
  )


# Convert numeric codes to text based on lookups and process text
ca_df <- ca_df %>%
  mutate(
    # Collapse multiple Hispanic groups into "Multiple Hispanic"
    hisp_collapsed = case_when(
      hisp %in% names(hisp_lookup) ~ hisp, 
      TRUE ~ "Multiple Hispanic"),
    
    # Ensure multi-digit race codes are properly separated 
    race_separated = str_replace_all(race, "(\\d{2})(?=\\d)", "\\1-"),
    
    # Replace numeric codes with text labels
    race_text = str_replace_all(race_separated, race_lookup),
    hisp_text = str_replace_all(hisp_collapsed, hisp_lookup),
    
    # Simplify race_text when Hispanic groups are present
    race_text = case_when(
      # Drop "Other Race" and "DK/R" if any Hispanic group is present
      str_detect(race_text, "Other Race|DK/R") & hisp_text != "" ~ "",
      
      # Keep aggregated race groups if any Hispanic group is present
      race_text %in% setdiff(race_lookup, names(subgroup_lookup)) & 
                   hisp_text != "" ~ race_text,
      
      # Aggregate subgroups if any Hispanic category is present
      hisp_text != "" ~ str_replace_all(race_text, subgroup_lookup),
      TRUE ~ race_text
    ),
    
    # Reaggregate Hispanic subgroups when in combination with another category
    hisp_text = case_when(
      !str_detect(race_text, "Other Race|DK/R") & 
        str_detect(race_text, "\\w") & 
        hisp_text != "" ~ "Hispanic",
      TRUE ~ hisp_text
    ),
    
    # Split race into lists and sort components alphabetically
    race_components = str_split(race_text, "-"),
    sorted_race = map_chr(race_components, ~ paste(sort(unique(
      .x
    )), collapse = "-")),

    # Combine Hispanic and Race (Hispanic first if present)
    re_text = case_when(
      hisp_text != "" &
        sorted_race != "" ~ paste(hisp_text, sorted_race, sep = "-"),
      
      # Only Hispanic
      hisp_text != "" &
        sorted_race == "" ~ hisp_text,
      
      # Only race (no Hispanic)
      TRUE ~ sorted_race
    )  
  ) %>%
  select(
    -hisp_collapsed,
    -race_separated,
    -race_components,
    -sorted_race
  )  # Remove temp columns


# Phase 1: Identify small groups and apply subgroup reclassification
group_counts <- ca_df %>%
  count(re_text, name = "n")

# Identify small groups (n < 50)
small_groups <- group_counts %>%
  filter(n < 50) %>%
  pull(re_text)

# Apply subgroup reclassification using and subgroup lookup table
ca_df <- ca_df %>%
  mutate(
    re_text = case_when(
      # If the group is in small_groups AND is a Hispanic category, reclassify as "Other Hispanic"
      re_text %in% small_groups & str_detect(re_text, "Mexican|Puerto Rican|Cuban|Other Hispanic") ~ "Other Hispanic",
      
      # Otherwise, apply subgroup lookup replacements
      re_text %in% small_groups ~ str_replace_all(re_text, subgroup_lookup),
      
      # Keep original values if no match
      TRUE ~ re_text
    )
  )

# Phase 2: Re-calculate small groups and re-aggregate using priority order
group_counts <- ca_df %>%
  count(re_text, name = "n")

small_groups <- group_counts %>%
  filter(n < 50) %>%
  pull(re_text)

# Priority to avoid data genocide: NHPI > AIAN > Black > Asian > Hispanic 
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

# Rename "Asian" and "NHPI" as "Unspecified" 
ca_df <- ca_df %>% 
  mutate(
    re_text = case_when(
      re_text == "Asian" ~ "Unspecified Asian",
      re_text == "NHPI" ~ "Unspecified NHPI", 
      TRUE ~ re_text
    )
  )

# Make sure no groups are under 50
group_counts <- ca_df %>%
  count(re_text, name = "n")

small_groups <- group_counts %>%
  filter(n < 50) %>%
  pull(re_text)

print(small_groups)

# Remove unneeded columns
ca_df <- ca_df %>% 
  select(-c("race_text", "hisp_text"))

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save cleaned dataset
saveRDS(ca_df, file.path(processed_data_dir, "03_ca_race_cleaned.rds"))
message(
  "Saved cleaned BRFSS dataset: ",
  file.path(processed_data_dir, "03_ca_race_cleaned.rds")
)

# End of script
message("03_define_race_categories.R completed successfully.")