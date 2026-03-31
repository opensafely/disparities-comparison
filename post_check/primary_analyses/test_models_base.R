## Create "test_models" style plots for BASE models (further = "no").
## - Mild and Severe in one plot (2 columns)
## - One legend per plot
## - Saved into: post_check/plots/primary_analyses/condensed_models_base/test_models/

library(tidyverse)
library(here)
library(arrow)

source(here::here("post_check", "functions", "forest.R"))
ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

investigation_type <- "primary"

load_inputs_base <- function(cohort, pathogen) {
  df_input <- read_csv(
    here::here("post_check", "output", "collated", "analytic", paste0(cohort, "_", pathogen, "_model_outputs_collated.csv")),
    show_col_types = FALSE
  )

  df_dummy <- if (identical(pathogen, "covid")) {
    read_feather(here::here("output", "data", paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow"))) %>%
      mutate(
        covid_vaccination_immunity_date = if (!"covid_vaccination_immunity_date" %in% names(.)) NA else covid_vaccination_immunity_date,
        time_since_last_covid_vaccination = if (!"time_since_last_covid_vaccination" %in% names(.)) NA_character_ else time_since_last_covid_vaccination
      ) %>%
      mutate(
        subset = "2021_22",
        time_since_last_covid_vaccination = if_else(is.na(covid_vaccination_immunity_date), "6-12m", time_since_last_covid_vaccination)
      )
  } else {
    read_feather(here::here("output", "data", paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow")))
  }

  list(df_input = df_input, df_dummy = df_dummy)
}

run_test_models_base <- function(model_type = "ethnicity_ses") {
  out_dir <- here::here("post_check", "plots", "primary_analyses", "condensed_models_base", "test_models")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  cohorts <- c("older_adults", "adults", "children_and_adolescents", "infants")
  pathogens <- c("rsv", "flu", "covid")
  phenotypes <- c("specific", "sensitive")

  for (cohort_name in cohorts) {
    cohort <<- cohort_name
    for (pathogen_name in pathogens) {
      inputs <- load_inputs_base(cohort_name, pathogen_name)
      for (phenotype in phenotypes) {
        df_use <- inputs$df_input %>%
          filter(tolower(codelist_type) %in% c("reference", tolower(phenotype)))

        # Build once, then derive legend/no-legend variants to reduce memory churn.
        mild_full <- forest(
          df_use, inputs$df_dummy, pathogen_name, model_type, "Mild",
          further = "no",
          fixed_axes = TRUE
        )
        severe_full <- forest(
          df_use, inputs$df_dummy, pathogen_name, model_type, "Severe",
          further = "no",
          fixed_axes = TRUE
        )

        leg <- cowplot::get_legend(mild_full + theme(legend.position = "right"))
        mild <- mild_full + theme(legend.position = "none")
        severe <- severe_full + theme(legend.position = "none")

        combined <- cowplot::plot_grid(mild, severe, ncol = 2, align = "h", axis = "tb")
        final <- cowplot::plot_grid(combined, leg, ncol = 2, rel_widths = c(5.1, 0.9), align = "h", axis = "tb")

        ggsave(
          filename = here::here(out_dir, paste0(cohort_name, "_", pathogen_name, "_", model_type, "_base_test_models_", phenotype, ".png")),
          plot = final,
          width = 8.27,
          height = 7.2
        )

        rm(mild_full, severe_full, mild, severe, leg, combined, final, df_use)
        graphics.off()
        invisible(gc())
      }

      rm(inputs)
      invisible(gc())
    }
  }
}

run_test_models_base()

