## Export virus+severity test plots with legends.
## Each output includes all seasons and all groups in one panel.

# Core data + plotting dependencies.
library(tidyverse)
library(here)
library(arrow)

# Reuse existing forest data/plot functions.
source(here::here("post_check", "functions", "forest.R"))
# Keep white background in saved PNGs.
ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

# Script-level analysis mode expected by existing helpers.
investigation_type <- "primary"

# Canonical group order (kept for consistency with main workflow).
preferred_group_order <- c(
  "Sex",
  "Age Group",
  "Ethnicity",
  "IMD Quintile",
  "Rurality",
  "Prior Vaccination",
  "Current Vaccination"
)

# Safe file-name helper.
slugify <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

# Load model outputs + matching dummy data for one cohort/pathogen.
load_inputs <- function(cohort, pathogen) {
  # Collated model results.
  df_input_raw <- read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")
    )
  )

  # Used by forest_year_further_mult() via global lookup.
  df_few <<- df_input_raw %>% filter(term == "too few events")
  df_input <- df_input_raw %>% filter(term != "too few events")

  # Dummy inputs differ for COVID vs non-COVID in this project.
  if (identical(pathogen, "covid")) {
    df_dummy <- read_feather(
      here::here("output", "data", paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow"))
    ) %>%
      # Backfill columns if absent to avoid downstream errors.
      mutate(
        covid_vaccination_immunity_date = if (!"covid_vaccination_immunity_date" %in% names(.)) NA else covid_vaccination_immunity_date,
        time_since_last_covid_vaccination = if (!"time_since_last_covid_vaccination" %in% names(.)) NA_character_ else time_since_last_covid_vaccination
      ) %>%
      # Align with legacy expectation for COVID subset.
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

  list(df_input = df_input, df_dummy = df_dummy)
}

# Build one combined dataset containing both severity outcomes.
build_plot_data <- function(df_input, df_dummy, pathogen, model_type, codelist_name) {
  mild_dat <- forest_year_further_mult(
    df_input, df_dummy, pathogen, model_type, "Mild", return_data = TRUE
  )
  severe_dat <- forest_year_further_mult(
    df_input, df_dummy, pathogen, model_type, "Severe", return_data = TRUE
  )

  bind_rows(mild_dat, severe_dat) %>%
    filter(codelist_type %in% c("reference", codelist_name))
}

# Export one combined Mild+Severe plot for a single cohort/pathogen/codelist.
export_virus_mild_severe_plot <- function(cohort, pathogen, model_type, codelist_name, out_dir) {
  inputs <- load_inputs(cohort, pathogen)
  dat <- build_plot_data(inputs$df_input, inputs$df_dummy, pathogen, model_type, codelist_name)

  if (is.null(dat) || nrow(dat) == 0) return(invisible(NULL))

  # Match facet row height to the BASE test-model plots.
  # Base exports are saved at height = 7.2 and typically show 4 facet rows
  # (Sex, Age Group, Ethnicity, IMD Quintile).
  base_height <- 7.2
  base_rows <- 4L
  facet_rows <- dplyr::n_distinct(dat$labels)
  out_height <- base_height * (facet_rows / base_rows)

  # Mild/Severe in one plot (columns), with a single legend.
  p <- forest_over_time_plot(
    dat,
    pathogen = pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    label_levels = FALSE,
    fixed_axes = TRUE
  )
  # For test exports, keep COVID facet columns tight (no extra gap).
  if (identical(pathogen, "covid")) {
    p <- p + theme(panel.spacing.x = unit(0.18, "lines"))
  }

  # File naming: cohort.model.pathogen.codelist.mild_vs_severe.png
  fname <- paste(
    cohort,
    model_type,
    pathogen,
    codelist_name,
    "mild_vs_severe",
    "png",
    sep = "."
  )

  # A4-ish width; taller because it includes both severity columns.
  ggsave(
    filename = here::here(out_dir, fname),
    plot = p,
    width = 8.27,
    height = out_height
  )
}

# Driver: iterate all cohorts/pathogens/codelists and export test plots.
run_test_models <- function(model_type = "ethnicity_ses") {
  out_dir <- here::here("post_check", "plots", "primary_analyses", "condensed_models", "test_models")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  cohorts <- c("older_adults", "adults", "children_and_adolescents", "infants")
  pathogens <- c("rsv", "flu", "covid")
  codelists <- c("specific", "sensitive")

  purrr::walk(cohorts, function(cohort_name) {
    # Existing forest helper expects global `cohort`.
    cohort <<- cohort_name
    purrr::walk(pathogens, function(pathogen_name) {
      purrr::walk(codelists, function(codelist_name) {
        export_virus_mild_severe_plot(
          cohort = cohort_name,
          pathogen = pathogen_name,
          model_type = model_type,
          codelist_name = codelist_name,
          out_dir = out_dir
        )
      })
    })
  })
}

# Execute on script run.
run_test_models()
