# Ethnicity_ses base vs further on selected seasons (Mild + Severe faceted).
# Per-virus PNGs -> forest_models_by_virus/{specific,sensitive}/.
# Condensed all-virus figure assembled here from the same panels (no separate script).
# Three-column separate base/further PNGs: key_exposures_over_time_three_column.R
library(tidyverse)
library(here)
library(arrow)
library(cowplot)

source(here::here("post_check", "functions", "forest.R"))

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

investigation_type <- "primary"
df_few <- tibble()

KEY_EXPOSURE_SEASONS <- c("2017_18", "2018_19", "2020_21")
SEASON_SLUG <- paste(gsub("_", "", KEY_EXPOSURE_SEASONS), collapse = "_")

load_collated_outputs_further <- function(cohort, pathogen) {
  raw <- read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
  df_few <<- bind_rows(df_few, raw %>% filter(term == "too few events"))
  raw %>% filter(term != "too few events")
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
    read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow")
      )
    ) %>%
      mutate(
        covid_vaccination_immunity_date = if (
          !"covid_vaccination_immunity_date" %in% names(.)
        ) NA else covid_vaccination_immunity_date,
        time_since_last_covid_vaccination = if (
          !"time_since_last_covid_vaccination" %in% names(.)
        ) NA_character_ else time_since_last_covid_vaccination
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
    read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow")
      )
    )
  }
}

slug <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

pathogen_plot_title <- function(pathogen) {
  dplyr::case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19",
    TRUE ~ pathogen
  )
}

save_per_virus_plot <- function(panel, cohort, pathogen, phenotype, out_dir) {
  season_label <- paste(normalize_season_label(KEY_EXPOSURE_SEASONS), collapse = ", ")
  p <- panel +
    labs(
      title = paste0(
        "Ethnicity and IMD — ", pathogen_plot_title(pathogen),
        " (Mild and Severe): base vs further (", season_label, ")"
      )
    )

  fname <- paste(
    cohort, pathogen, phenotype,
    "ethnicity_ses_base_vs_further_selected_seasons",
    sep = "_"
  )
  plot_width <- if (cohort %in% c("infants", "infants_subgroup")) 14 else 20
  plot_height <- if (cohort %in% c("infants", "infants_subgroup")) 8.27 else 11.69
  ggsave(
    here::here(out_dir, paste0(slug(fname), ".png")),
    p,
    width = plot_width,
    height = plot_height
  )
}

save_condensed_plot <- function(panels, cohort, phenotype, df_min_covid, df_full_covid, df_dummy_covid, out_root) {
  df_min_f <- df_min_covid %>%
    filter(tolower(codelist_type) %in% c("reference", tolower(phenotype)))
  df_full_f <- df_full_covid %>%
    filter(tolower(codelist_type) %in% c("reference", tolower(phenotype)))

  legend_dat <- collect_ethnicity_ses_base_vs_further_data(
    df_min_f,
    df_full_f,
    df_dummy_covid,
    "covid",
    c("Mild", "Severe"),
    KEY_EXPOSURE_SEASONS
  )
  shared_legends <- build_shared_legends_base_vs_further(
    legend_dat, "covid", KEY_EXPOSURE_SEASONS
  )

  condensed <- assemble_condensed_base_vs_further(
    panels$rsv,
    panels$flu,
    panels$covid,
    shared_legends$left,
    shared_legends$mid,
    shared_legends$adj
  )

  is_infant <- cohort %in% c("infants", "infants_subgroup")
  plot_width <- if (is_infant) 14 else 16.54
  plot_height <- if (is_infant) 11 else 14

  ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_ethnicity_ses_base_vs_further_condensed_",
        SEASON_SLUG, "_", phenotype, "_mild_vs_severe.png"
      )
    ),
    condensed,
    width = plot_width,
    height = plot_height
  )
}

run_all_key_exposures_over_time <- function() {
  out_root <- here::here("post_check", "plots", "primary_analyses", "forest_models_by_virus")
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  cohorts <- c(
    "older_adults", "adults", "children_and_adolescents",
    "infants", "infants_subgroup"
  )
  pathogens <- c("rsv", "flu", "covid")
  phenotypes <- c("specific", "sensitive")

  for (cohort in cohorts) {
    cohort <<- cohort
    message("Running ethnicity_ses base vs further: ", cohort)

    for (phenotype in phenotypes) {
      out_dir <- here::here(out_root, phenotype)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      panels <- list()
      df_min_covid <- NULL
      df_full_covid <- NULL
      df_dummy_covid <- NULL

      for (pathogen in pathogens) {
        message(
          "  ", pathogen, " / ", phenotype,
          " (seasons ", paste(gsub("_", "-", KEY_EXPOSURE_SEASONS), collapse = ", "), ")"
        )

        tryCatch(
          {
            df_min <- load_collated_outputs_base(cohort, pathogen)
            df_full <- load_collated_outputs_further(cohort, pathogen)
            df_dummy <- load_dummy_inputs(cohort, pathogen)

            df_min_f <- df_min %>%
              filter(tolower(codelist_type) %in% c("reference", tolower(phenotype)))
            df_full_f <- df_full %>%
              filter(tolower(codelist_type) %in% c("reference", tolower(phenotype)))

            panel <- forest_ethnicity_ses_base_vs_further_panel(
              df_min = df_min_f,
              df_full = df_full_f,
              df_dummy = df_dummy,
              pathogen = pathogen,
              outcome_types = c("Mild", "Severe"),
              seasons = KEY_EXPOSURE_SEASONS,
              show_ci = TRUE,
              show_disruption_legend = FALSE
            )

            save_per_virus_plot(panel, cohort, pathogen, phenotype, out_dir)
            panels[[pathogen]] <- panel

            if (identical(pathogen, "covid")) {
              df_min_covid <- df_min
              df_full_covid <- df_full
              df_dummy_covid <- df_dummy
            }
          },
          error = function(e) {
            message(
              "  Failed: pathogen=", pathogen, " phenotype=", phenotype,
              " :: ", conditionMessage(e)
            )
            NULL
          }
        )
      }

      if (length(panels) == length(pathogens) && !is.null(df_min_covid)) {
        tryCatch(
          save_condensed_plot(
            panels, cohort, phenotype,
            df_min_covid, df_full_covid, df_dummy_covid,
            out_root
          ),
          error = function(e) {
            message(
              "  Failed condensed: phenotype=", phenotype,
              " :: ", conditionMessage(e)
            )
            NULL
          }
        )
      } else {
        message(
          "  Skipping condensed (", phenotype, "): not all virus panels succeeded"
        )
      }
    }
  }
}

run_all_key_exposures_over_time()
