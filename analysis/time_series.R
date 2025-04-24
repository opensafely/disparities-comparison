library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(purrr)

## create output directories ----
fs::dir_create(here::here("analysis"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2019-09-01")
  study_end_date <- as.Date("2020-08-31")
  cohort <- "older_adults"
  codelist_type <- "specific"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}

covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min <- as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min <- as.Date("2021-09-01", "%Y-%m-%d")

source(here::here("analysis", "functions", "redaction.R"))

columns_needed <- c("patient_id", "patient_index_date", "patient_end_date",
                    "age_band", "sex", "latest_ethnicity_group",
                    "imd_quintile", "rurality_classification")

if (study_start_date == as.Date("2020-09-01")) {
  columns_needed <- c(columns_needed, "composition_category")
}

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_",
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type, ".arrow")),
  col_select = c(all_of(columns_needed), ((ends_with("_date")) & (contains(c(
    "primary", "secondary", "mortality"))) & (!contains(c(
      "_second_", "_inf_", "patient_")))))
)

#create function to format data
reformat_frame <- function(df, cols_needed, characteristic) {
  
  df_long <- df %>%
    pivot_longer(
      cols = -c(all_of(cols_needed)),
      names_to = "outcome_type", values_to = "date"
    ) %>%
    group_by(outcome_type, date, !!sym(characteristic)) %>%
    summarise(count = n())
  
}

# #change from row per patient to row per date
# df_long_sex <- reformat_frame(df_input, columns_needed, "sex")
# df_long_age <- reformat_frame(df_input, columns_needed, "age_band")
# df_long_imd <- reformat_frame(df_input, columns_needed, "imd_quintile")
# df_long_ethnicity <- reformat_frame(
#   df_input, columns_needed, "latest_ethnicity_group")
# df_long_rurality <- reformat_frame(
#   df_input, columns_needed, "rurality_classification")
# if (study_start_date == as.Date("2020-09-01")) {
#   df_long_composition <- reformat_frame(
#     df_input, columns_needed, "composition_category")
# }
# 
# #combine frames
# frames <- list(df_long_sex, df_long_age, df_long_imd, df_long_ethnicity,
#                df_long_rurality)
# if (study_start_date == as.Date("2020-09-01")) {
#   list <- list(frames, df_long_composition)
# }
# df_long <- reduce(.x = frames, merge, by = c("outcome_type", "date"),
#                   all = TRUE)

#rename columns

