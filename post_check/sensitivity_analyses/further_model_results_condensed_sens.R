# Ethnicity_ses further-model condensed figures (sensitivity phenotypes).
# Output: post_check/plots/sensitivity_analyses/condensed_models_key_vars/
library(tidyverse)
library(here)
library(arrow)
library(cowplot)

source(here::here("post_check", "functions", "forest.R"))
source(here::here("post_check", "functions", "condensed_figures.R"))

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

investigation_type <- "sensitivity"
df_few <- tibble()
model_type <- "ethnicity_ses"

cohorts <- c(
  "older_adults", "adults", "children_and_adolescents",
  "infants", "infants_subgroup"
)

for (cohort in cohorts) {
  message("Running sensitivity key-vars condensed (ethnicity_ses): ", cohort)
  tryCatch(
    run_cohort_condensed_key_vars_sensitivity(
      cohort = cohort,
      model_type = model_type
    ),
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
