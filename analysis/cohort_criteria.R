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

source(here::here("analysis", "functions", "redaction.R"))

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

# Define the base population for exclusion: Only consider registered and appropriate age patients
if (cohort == "infants" | cohort == "infants_subgroup") {
  population <- patients_df %>%
    mutate(
      stage0 = 1,
      stage1 = registered,
      stage2a = registered & is_female_or_male,
      stage2b = registered & has_imd,
      stage2c = registered &
        !(care_home | risk_group_infants | severe_immunodeficiency),
      stage2 = registered & is_female_or_male & has_imd &
        !(care_home | risk_group_infants | severe_immunodeficiency) 
    )
} else {
  population <- patients_df %>%
    mutate(
      stage0 = 1,
      stage1 = registered,
      stage2a = registered & is_female_or_male,
      stage2b = registered & has_imd,
      stage2c = registered & !care_home,
      stage2 = registered & is_female_or_male & has_imd & !care_home
    )
}

population_summary <- population %>%
  summarise(
    n0 = roundmid_any(n()),
    n1 = roundmid_any(sum(stage1)),
    n2a = roundmid_any(sum(stage2a)),
    n2b = roundmid_any(sum(stage2b)),
    n2c = roundmid_any(sum(stage2c)),
    n2 = roundmid_any(sum(stage2)),
    
    pct1 = n1 / n0 * 100,
    pct2a = n2a / n1 * 100,
    pct2b = n2b / n1 * 100,
    pct2c = n2c / n1 * 100,
    pct2 = n2 / n1 * 100
  )


## create output directories ----
fs::dir_create(here::here("output", "flow_chart"))

write_csv(population_summary, path = paste0(
  here::here("output", "flow_chart"), "/", "flow_chart_processed_", cohort,
  "_", year(study_start_date), "_", year(study_end_date), ".csv"))
