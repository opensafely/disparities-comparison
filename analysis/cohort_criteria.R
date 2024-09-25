library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(gtsummary)

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "infants"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
}

patients_df <- read_csv(
  here::here("output", "flow_chart", paste0(cohort, "_", year(study_start_date), 
             "_", year(study_end_date), "_flow_chart", ".csv")))

patients_df <- patients_df %>%
  mutate(
    has_imd = if_else(is.na(patients_df$imd_rounded), F, T),
    is_female_or_male = if_else(patients_df$sex == "female" | patients_df$sex == "male", T, F)
  ) 


if (cohort == "infants" | cohort == "infants_subgroup") {
  is_appropriate_age = if_else(patients_df$age >= 0 & patients_df$age <= 23, T, F)
} else if (cohort == "children_and_adolescents") {
  is_appropriate_age = if_else(patients_df$age >= 2 & patients_df$age <= 17, T, F)
} else if (cohort == "adults") {
  is_appropriate_age = if_else(patients_df$age >= 18 & patients_df$age <= 64, T, F)
} else {
  is_appropriate_age = if_else(patients_df$age >= 65, T, F)
}

patients_df <- patients_df %>%
  mutate(is_appropriate_age = is_appropriate_age)

# Define counts based on inclusion and exclusion criteria
total <- nrow(patients_df)
registered_count <- sum(patients_df$registered, na.rm = TRUE)
non_registered_count <- total - registered_count
age_count <- if (cohort == "infants" | cohort == "infants_subgroup") {
  sum(patients_df$is_appropriate_age, na.rm = TRUE)
} else {
  sum(patients_df$is_appropriate_age & patients_df$registered, na.rm = TRUE)
}
not_age_count <- if (cohort == "infants" | cohort == "infants_subgroup") {
  total - age_count
} else {
  registered_count - age_count
}

if (cohort == "infants" | cohort == "infants_subgroup") {
  included_count <- sum(!patients_df$severe_immunodeficiency
                        & patients_df$is_appropriate_age & patients_df$has_imd 
                        & patients_df$is_female_or_male & !patients_df$care_home
                        & !patients_df$risk_group_infants, na.rm = TRUE)
  excluded_count <- sum(!patients_df$is_female_or_male|!patients_df$is_appropriate_age
                        |!patients_df$has_imd|patients_df$risk_group_infants
                        |patients_df$care_home |patients_df$severe_immunodeficiency,
                        na.rm = TRUE) 
} else {
  included_count <- sum(patients_df$registered & patients_df$is_female_or_male 
                        & patients_df$is_appropriate_age & patients_df$has_imd 
                        & !patients_df$care_home, na.rm = TRUE)
  excluded_count <- sum(!patients_df$is_female_or_male|!patients_df$is_appropriate_age
                        |!patients_df$has_imd|patients_df$care_home, 
                        na.rm = TRUE)
}

## create output directories ----
fs::dir_create(here("output", "flow_chart"))

#export flow chart numbers 
table <- cbind(total, non_registered_count, registered_count,  
               not_age_count, age_count, excluded_count, included_count)
table <- table %>%
  as.data.frame() %>%
  write_csv(path = paste0(here::here("output", "flow_chart"), "/", 
            "flow_chart_processed_", cohort, "_", year(study_start_date), "_", 
            year(study_end_date), ".csv"))
