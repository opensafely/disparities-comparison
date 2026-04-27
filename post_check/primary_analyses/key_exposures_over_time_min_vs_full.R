library(tidyverse)
library(here)
library(arrow)

# Plotting + model helper functions.
source(here::here("post_check", "functions", "forest.R"))

# Ensure white background for all saved PNGs.
ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

# `forest()` and helpers rely on these globals in this project.
investigation_type <- "primary"

load_collated_outputs_further <- function(cohort, pathogen) {
  read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
}

load_collated_outputs_base <- function(cohort, pathogen) {
  read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
}

load_dummy_inputs <- function(cohort, pathogen) {
  if (identical(pathogen, "covid")) {
    df_dummy <- read_feather(
      here::here("output", "data", paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow"))
    ) %>%
      mutate(
        covid_vaccination_immunity_date = if (!"covid_vaccination_immunity_date" %in% names(.)) NA else covid_vaccination_immunity_date,
        time_since_last_covid_vaccination = if (!"time_since_last_covid_vaccination" %in% names(.)) NA_character_ else time_since_last_covid_vaccination
      ) %>%
      mutate(
        subset = "2021_22",
        time_since_last_covid_vaccination = if_else(
          is.na(covid_vaccination_immunity_date),
          "6-12m",
          time_since_last_covid_vaccination
        )
      )
  } else {
    df_dummy <- read_feather(
      here::here("output", "data", paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow"))
    )
  }

  df_dummy
}

slug <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

run_one_key_exposures_over_time <- function(df_min, df_full, df_dummy, cohort, pathogen, phenotype, outcome_type, out_dir) {
  # `forest()` uses global `cohort` in multiple places.
  cohort <<- cohort
  
  # Infants do not have household composition in the current pipelines.
  is_infant <- cohort %in% c("infants", "infants_subgroup")

  p_min <- forest_key_exposures_three_column_plot(
    df_min = df_min,
    df_full = df_full,
    df_dummy = df_dummy,
    pathogen = pathogen,
    outcome_type = outcome_type,
    adjustment = "base",
    show_ci = TRUE
  )
  p_full <- forest_key_exposures_three_column_plot(
    df_min = df_min,
    df_full = df_full,
    df_dummy = df_dummy,
    pathogen = pathogen,
    outcome_type = outcome_type,
    adjustment = "further",
    show_ci = TRUE
  )

  fname_min <- paste(cohort, pathogen, phenotype, outcome_type, "key_exposures_base_over_time", sep = "_")
  fname_full <- paste(cohort, pathogen, phenotype, outcome_type, "key_exposures_further_over_time", sep = "_")

  ggsave(here::here(out_dir, paste0(slug(fname_min), ".png")), p_min, width = 11.69, height = 8.27)
  ggsave(here::here(out_dir, paste0(slug(fname_full), ".png")), p_full, width = 11.69, height = 8.27)
}

run_all_key_exposures_over_time <- function() {
  out_root <- here::here("post_check", "plots", "primary_analyses", "forest_models_by_virus")
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  cohorts <- c("older_adults", "adults", "children_and_adolescents", "infants")
  pathogens <- c("rsv", "flu", "covid")
  outcomes <- c("Mild", "Severe")
  phenotypes <- c("specific", "sensitive")

  for (pathogen in pathogens) {
    for (phenotype in phenotypes) {
      out_dir <- here::here(out_root, pathogen, phenotype)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      for (cohort in cohorts) {
        message("Running key-exposure over-time: ", cohort, " / ", pathogen, " / ", phenotype)
        df_min <- load_collated_outputs_base(cohort, pathogen) %>%
          filter(tolower(codelist_type) %in% c("reference", tolower(phenotype)))
        df_full <- load_collated_outputs_further(cohort, pathogen) %>%
          filter(tolower(codelist_type) %in% c("reference", tolower(phenotype)))
        df_dummy <- load_dummy_inputs(cohort, pathogen)

        for (outcome_type in outcomes) {
          tryCatch(
            run_one_key_exposures_over_time(df_min, df_full, df_dummy, cohort, pathogen, phenotype, outcome_type, out_dir),
            error = function(e) {
              message(
                "Failed key-exposure over-time plot: cohort=", cohort,
                " pathogen=", pathogen,
                " phenotype=", phenotype,
                " outcome=", outcome_type,
                " :: ", conditionMessage(e)
              )
              NULL
            }
          )
        }
      }
    }
  }
}

run_all_key_exposures_over_time()
