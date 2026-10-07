library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here::here("analysis", "exploratory_analyses"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2017-09-01"
  study_end_date <- "2018-08-31"
  cohort <- "adults"
} else {
  cohort <- args[[1]]
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
}
covid_season_min <- as.Date("2019-09-01")

#import redaction function
source(here::here("analysis", "functions", "redaction.R"))

#import the data 
df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             "specific", "_", "primary",".arrow")))

# infants are month-expanded; collapse to one row per patient for LoS checks.
# LoS is patient-level (hours), so take the first non-missing value per patient;
# infection flags / dates follow the usual any() / min() collapse.
if (cohort %in% c("infants", "infants_subgroup")) {
  inf_cols <- names(df_input)[endsWith(names(df_input), "_inf")]
  date_cols <- names(df_input)[endsWith(names(df_input), "_date")]
  los_cols <- names(df_input)[grepl("_los", names(df_input))]
  other_cols <- setdiff(names(df_input), c("patient_id", inf_cols, date_cols, los_cols))
  df_input <- df_input %>%
    arrange(patient_id, date) %>%
    group_by(patient_id) %>%
    summarise(
      across(all_of(inf_cols), ~ as.integer(any(.x == 1, na.rm = TRUE))),
      across(all_of(date_cols), ~ if (all(is.na(.x))) as.Date(NA) else min(.x, na.rm = TRUE)),
      across(all_of(los_cols), ~ {
        x <- .x[!is.na(.x)]
        if (length(x) == 0) NA_real_ else x[[1]]
      }),
      across(all_of(other_cols), first),
      .groups = "drop"
    )
}

#get summary of length of stay, and proportion in hosp for at least a week
if (study_start_date >= covid_season_min) {
  los_summary <- df_input %>%
    summarise(
      rsv_los = mean(if_else(rsv_secondary_inf == 1, rsv_los, NA_real_), na.rm = TRUE),
      rsv_los_days = mean(if_else(rsv_secondary_inf == 1, rsv_los / 24, NA_real_), na.rm = TRUE),
      rsv_n_midpoint10 = roundmid_any(sum(rsv_secondary_inf == 1, na.rm = TRUE)),
      rsv_long_n_midpoint10 = roundmid_any(
        sum(rsv_secondary_inf == 1 & rsv_los / 24 > 6, na.rm = TRUE)
      ),
      rsv_long_prop = roundmid_any(
        sum(rsv_secondary_inf == 1 & rsv_los / 24 > 6, na.rm = TRUE)
      ) / roundmid_any(sum(rsv_secondary_inf == 1, na.rm = TRUE)) * 100,
      flu_los = mean(if_else(flu_secondary_inf == 1, flu_los, NA_real_), na.rm = TRUE),
      flu_los_days = mean(if_else(flu_secondary_inf == 1, flu_los / 24, NA_real_), na.rm = TRUE),
      flu_n_midpoint10 = roundmid_any(sum(flu_secondary_inf == 1, na.rm = TRUE)),
      flu_long_n_midpoint10 = roundmid_any(
        sum(flu_secondary_inf == 1 & flu_los / 24 > 6, na.rm = TRUE)
      ),
      flu_long_prop = roundmid_any(
        sum(flu_secondary_inf == 1 & flu_los / 24 > 6, na.rm = TRUE)
      ) / roundmid_any(sum(flu_secondary_inf == 1, na.rm = TRUE)) * 100,
      covid_los = mean(if_else(covid_secondary_inf == 1, covid_los, NA_real_), na.rm = TRUE),
      covid_los_days = mean(if_else(covid_secondary_inf == 1, covid_los / 24, NA_real_), na.rm = TRUE),
      covid_n_midpoint10 = roundmid_any(sum(covid_secondary_inf == 1, na.rm = TRUE)),
      covid_long_n_midpoint10 = roundmid_any(
        sum(covid_secondary_inf == 1 & covid_los / 24 > 6, na.rm = TRUE)
      ),
      covid_long_prop = roundmid_any(
        sum(covid_secondary_inf == 1 & covid_los / 24 > 6, na.rm = TRUE)
      ) / roundmid_any(sum(covid_secondary_inf == 1, na.rm = TRUE)) * 100
    )
} else {
  los_summary <- df_input %>%
    summarise(
      rsv_los = mean(if_else(rsv_secondary_inf == 1, rsv_los, NA_real_), na.rm = TRUE),
      rsv_los_days = mean(if_else(rsv_secondary_inf == 1, rsv_los / 24, NA_real_), na.rm = TRUE),
      rsv_n_midpoint10 = roundmid_any(sum(rsv_secondary_inf == 1, na.rm = TRUE)),
      rsv_long_n_midpoint10 = roundmid_any(
        sum(rsv_secondary_inf == 1 & rsv_los / 24 > 6, na.rm = TRUE)
      ),
      rsv_long_prop = roundmid_any(
        sum(rsv_secondary_inf == 1 & rsv_los / 24 > 6, na.rm = TRUE)
      ) / roundmid_any(sum(rsv_secondary_inf == 1, na.rm = TRUE)) * 100,
      flu_los = mean(if_else(flu_secondary_inf == 1, flu_los, NA_real_), na.rm = TRUE),
      flu_los_days = mean(if_else(flu_secondary_inf == 1, flu_los / 24, NA_real_), na.rm = TRUE),
      flu_n_midpoint10 = roundmid_any(sum(flu_secondary_inf == 1, na.rm = TRUE)),
      flu_long_n_midpoint10 = roundmid_any(
        sum(flu_secondary_inf == 1 & flu_los / 24 > 6, na.rm = TRUE)
      ),
      flu_long_prop = roundmid_any(
        sum(flu_secondary_inf == 1 & flu_los / 24 > 6, na.rm = TRUE)
      ) / roundmid_any(sum(flu_secondary_inf == 1, na.rm = TRUE)) * 100
    )
}

#create output directory
fs::dir_create(here::here("output", "exploratory"))

#save results
write_csv(los_summary, here::here("output", "exploratory", paste0(
  "los_summary_", cohort, "_", year(study_start_date), "_",
  year(study_end_date), ".csv")))
