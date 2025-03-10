library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(gtsummary)

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "infants_subgroup"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
}

patients_df <- read_feather(
  here::here("output", "flow_chart", paste0(cohort, "_", year(study_start_date), 
             "_", year(study_end_date), "_flow_chart", ".arrow")))

if (study_start_date == as.Date("2020-09-01") &
    cohort != "infants" & cohort != "infants_subgroup") {
  
  df_household <- read_feather(
    here::here("output", "data", paste0("input_household_processed_", 
               year(study_start_date), "_", year(study_end_date), ".arrow")))
  
  household_comp_vars <- tibble(
    "patient_id" = df_household$patient_id,
    "num_generations"= df_household$num_generations,
    "composition_category" = df_household$composition_category
  )
  
  patients_df <- merge(patients_df, household_comp_vars, by = "patient_id")
  
}

patients_df <- patients_df %>%
  mutate(
    has_imd = if_else(is.na(patients_df$imd_rounded), F, T),
    is_female_or_male = if_else(patients_df$sex == "female" |
                                  patients_df$sex == "male", T, F)
  ) 

# Define counts based on inclusion and exclusion criteria
total <- nrow(patients_df)
registered_count <- sum(patients_df$registered, na.rm = TRUE)
non_registered_count <- total - registered_count
if (cohort == "infants_subgroup") {
  mother_linkage_available_count <- sum(patients_df$mother_id_present,
                                        na.rm = TRUE)
  mother_registered_spanning_count <- sum(patients_df$mother_registered,
                                          na.rm = TRUE)
}
age_count <- if (cohort == "infants") {
  sum(patients_df$is_appropriate_age, na.rm = TRUE)
} else if (cohort == "infants_subgroup") {
  sum(patients_df$is_appropriate_age &
      patients_df$mother_registered, na.rm = TRUE)
} else {
  sum(patients_df$is_appropriate_age & patients_df$registered, na.rm = TRUE)
}
not_age_count <- if (cohort == "infants") {
  total - age_count
} else if (cohort == "infants_subgroup") {
  mother_registered_spanning_count - age_count
} else {
  registered_count - age_count
}

# Define the base population for exclusion: Only consider registered and appropriate age patients
if (cohort == "infants") { 
  eligible_for_exclusion <- patients_df %>%
    filter(is_appropriate_age == TRUE)
} else if (cohort == "infants_subgroup") {
  eligible_for_exclusion <- patients_df %>%
    filter(is_appropriate_age == TRUE & mother_registered == TRUE)
} else {
  eligible_for_exclusion <- patients_df %>%
    filter(registered == TRUE & is_appropriate_age == TRUE)
}

if (cohort == "infants") {
  excluded_count <- sum(
    !eligible_for_exclusion$is_female_or_male |
    !eligible_for_exclusion$has_imd |
    eligible_for_exclusion$risk_group_infants |
    eligible_for_exclusion$care_home |
    eligible_for_exclusion$severe_immunodeficiency,
    na.rm = TRUE
  )
} else {
  excluded_count <- sum(
    !eligible_for_exclusion$is_female_or_male |
    !eligible_for_exclusion$has_imd |
    eligible_for_exclusion$care_home,
    na.rm = TRUE
  )
}

included_count <- age_count - excluded_count


## create output directories ----
fs::dir_create(here::here("output", "flow_chart"))

#export flow chart numbers 
table <- cbind(total, non_registered_count, registered_count,  
               not_age_count, age_count, excluded_count, included_count)

if (cohort == "infants_subgroup") {
  table <- cbind(table, mother_linkage_available_count,
                 mother_registered_spanning_count)
}

table <- table %>%
  as.data.frame() %>%
  write_csv(path = paste0(here::here("output", "flow_chart"), "/", 
            "flow_chart_processed_", cohort, "_", year(study_start_date), "_", 
            year(study_end_date), ".csv"))
