# Fully adjusted full models (ethnicity, IMD, household composition): condensed
# key-variable figures for the 2020-21 season (RSV / flu / COVID stacked).
# Output: post_check/plots/supplemental/condensed_models_key_vars/
library(tidyverse)
library(here)
library(arrow)
library(cowplot)

source(here::here("post_check", "functions", "forest.R"))
source(here::here("post_check", "functions", "condensed_figures.R"))

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

investigation_type <- "primary"
df_few <- tibble()
seasons <- c("2020_21")

# Full models (incl. household composition) were not run for infant cohorts;
# use ethnicity_ses there (age, ethnicity, IMD only).
cohort_specs <- tibble::tribble(
  ~cohort,                   ~model_type,
  "older_adults",            "full",
  "adults",                  "full",
  "children_and_adolescents", "full"
)

for (i in seq_len(nrow(cohort_specs))) {
  cohort <- cohort_specs$cohort[[i]]
  model_type <- cohort_specs$model_type[[i]]
  message(
    "Running key-vars condensed (2020-21, further adjusted, ",
    model_type, "): ", cohort
  )
  tryCatch(
    run_cohort_condensed_key_vars_seasons_further(
      cohort = cohort,
      seasons = seasons,
      model_type = model_type
    ),
    error = function(e) {
      message(
        "Failed: cohort=", cohort,
        " model_type=", model_type,
        " seasons=", paste(seasons, collapse = ", "),
        " :: ", conditionMessage(e)
      )
      NULL
    }
  )
}
