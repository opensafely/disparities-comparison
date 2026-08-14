library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here::here("output", "collated", "descriptive"))

source(here::here("analysis", "design", "design.R"))
source(here::here("analysis", "functions", "redaction.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "adults"
} else {
  cohort <- args[[1]]
}

seasons <- tibble(
  season = paste0(2016:2023, "_", 17:24),
  start_key = paste0("season", 1:8, "_start_date"),
  end_key = paste0("season", 1:8, "_end_date")
) %>%
  mutate(
    study_start_date = as.Date(map_chr(start_key, ~ as.character(study_dates[[.x]]))),
    study_end_date = as.Date(map_chr(end_key, ~ as.character(study_dates[[.x]])))
  )

age_stats <- function(age, timepoint, age_unit, cohort, season,
                      study_start_date, study_end_date) {
  tibble(
    n = roundmid_any(sum(!is.na(age))),
    median = median(age, na.rm = TRUE),
    q25 = quantile(age, 0.25, na.rm = TRUE, names = FALSE),
    q75 = quantile(age, 0.75, na.rm = TRUE, names = FALSE)
  ) %>%
    mutate(
      cohort = cohort,
      season = season,
      year_start = year(study_start_date),
      year_end = year(study_end_date),
      timepoint = timepoint,
      age_unit = age_unit,
      iqr = q75 - q25,
      median_iqr = paste0(round(median, 1), " (", round(q25, 1),
                          "-", round(q75, 1), ")")
    ) %>%
    select(cohort, season, year_start, year_end, timepoint, age_unit,
           n, median, q25, q75, iqr, median_iqr)
}

summarise_age <- function(cohort, study_start_date, study_end_date, season) {

  input_path <- here::here(
    "output", "data",
    paste0("input_processed_", cohort, "_",
           year(study_start_date), "_", year(study_end_date), "_",
           "specific", "_", "primary", ".arrow")
  )

  if (!file.exists(input_path)) {
    message("Skipping missing file: ", basename(input_path))
    return(NULL)
  }

  df_input <- read_feather(input_path)
  is_infant <- cohort %in% c("infants", "infants_subgroup")
  age_unit <- if (is_infant) "months" else "years"

  # infants are expanded to monthly rows; take age at first month as index age
  if (is_infant && "date" %in% names(df_input)) {
    df_input <- df_input %>%
      group_by(patient_id) %>%
      slice_min(date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        age_index = age,
        # date is the unshifted index date used to update monthly age
        age_end = age + as.numeric((patient_end_date - date) / 30.44)
      )
  } else {
    df_input <- df_input %>%
      distinct(patient_id, .keep_all = TRUE) %>%
      mutate(
        age_index = age,
        # patient_index_date is shifted back 1 day after monthly age is derived
        age_end = if (is_infant) {
          age + as.numeric((patient_end_date - (patient_index_date + days(1))) / 30.44)
        } else {
          NA_real_
        }
      )
  }

  out <- age_stats(
    df_input$age_index, "index", age_unit, cohort, season,
    study_start_date, study_end_date
  )

  if (is_infant) {
    out <- bind_rows(
      out,
      age_stats(
        df_input$age_end, "follow_up_end", age_unit, cohort, season,
        study_start_date, study_end_date
      )
    )
  }

  out

}

age_summary <- pmap(
  list(
    cohort = cohort,
    study_start_date = seasons$study_start_date,
    study_end_date = seasons$study_end_date,
    season = seasons$season
  ),
  summarise_age
) %>%
  bind_rows()

write_csv(
  age_summary,
  here::here("output", "collated", "descriptive", paste0("age_summary_", cohort, "_collated.csv"))
)
