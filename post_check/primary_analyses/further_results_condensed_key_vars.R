library(tidyverse)
library(here)
library(cowplot)

source(here::here("post_check", "functions", "forest.R"))
source(here::here("post_check", "functions", "condensed_figures.R"))

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)
investigation_type <- "primary"
df_few <- tibble()
model_type <- "ethnicity_ses"

make_facet_outcome_plots_key_vars <- function(df_input, df_dummy, pathogen, model_type) {
  mild_dat <- forest_year_further_mult_key_vars(
    df_input, df_dummy, pathogen, model_type, "Mild"
  )
  severe_dat <- forest_year_further_mult_key_vars(
    df_input, df_dummy, pathogen, model_type, "Severe"
  )
  both_dat <- bind_rows(mild_dat, severe_dat)

  list(
    specific = forest_over_time_plot(
      both_dat %>% filter(codelist_type %in% c("reference", "specific")),
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      show_disruption_legend = FALSE
    ) + theme(legend.position = "none"),
    sensitive = forest_over_time_plot(
      both_dat %>% filter(codelist_type %in% c("reference", "sensitive")),
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      show_disruption_legend = FALSE
    ) + theme(legend.position = "none")
  )
}

run_cohort_condensed_key_vars <- function(cohort) {
  assign("cohort", cohort, envir = .GlobalEnv)

  pathogen <- "rsv"
  df_input_rsv <- load_collated_further(cohort, pathogen)
  df_dummy_rsv <- load_dummy_inputs(cohort, pathogen)
  rsv_plots <- make_facet_outcome_plots_key_vars(
    df_input_rsv, df_dummy_rsv, pathogen, model_type
  )

  pathogen <- "flu"
  df_input_flu <- load_collated_further(cohort, pathogen)
  df_dummy_flu <- load_dummy_inputs(cohort, pathogen)
  flu_plots <- make_facet_outcome_plots_key_vars(
    df_input_flu, df_dummy_flu, pathogen, model_type
  )

  pathogen <- "covid"
  df_input_covid <- load_collated_further(cohort, pathogen)
  df_dummy_covid <- load_dummy_inputs(cohort, pathogen)
  covid_plots <- make_facet_outcome_plots_key_vars(
    df_input_covid, df_dummy_covid, pathogen, model_type
  )

  legend_dat <- bind_rows(
    forest_year_further_mult_key_vars(
      df_input_covid, df_dummy_covid, "covid", model_type, "Mild"
    ),
    forest_year_further_mult_key_vars(
      df_input_covid, df_dummy_covid, "covid", model_type, "Severe"
    )
  ) %>%
    filter(codelist_type %in% c("reference", "specific"))

  shared_legends <- build_shared_legends_key_vars(legend_dat, model_type)

  specific_condensed <- assemble_condensed_figure(
    rsv_plots$specific,
    flu_plots$specific,
    covid_plots$specific,
    shared_legends$left,
    shared_legends$mid
  )
  sensitive_condensed <- assemble_condensed_figure(
    rsv_plots$sensitive,
    flu_plots$sensitive,
    covid_plots$sensitive,
    shared_legends$left,
    shared_legends$mid
  )

  out_dir <- here::here(
    "post_check", "plots", "primary_analyses", "condensed_models_key_vars"
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    here::here(
      out_dir,
      paste0(cohort, "_", model_type, "_further_specific_mild_vs_severe_key_vars.png")
    ),
    specific_condensed,
    height = 11.69,
    width = 8.27
  )
  ggsave(
    here::here(
      out_dir,
      paste0(cohort, "_", model_type, "_further_sensitive_mild_vs_severe_key_vars.png")
    ),
    sensitive_condensed,
    height = 11.69,
    width = 8.27
  )
}

cohorts <- c(
  "older_adults", "adults", "children_and_adolescents", "infants", "infants_subgroup"
)

for (cohort in cohorts) {
  message("Running key-vars condensed (ethnicity_ses): ", cohort)
  tryCatch(
    run_cohort_condensed_key_vars(cohort),
    error = function(e) {
      message(
        "Failed: cohort=", cohort,
        " model_type=", model_type,
        " :: ", conditionMessage(e)
      )
      NULL
    }
  )
}
