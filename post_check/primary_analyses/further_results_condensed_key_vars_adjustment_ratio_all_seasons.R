# Ethnicity_ses adjustment-ratio condensed figures (all seasons per virus).
# Output: post_check/plots/primary_analyses/condensed_model_ratios_key_vars/
library(tidyverse)
library(here)
library(arrow)
library(cowplot)

source(here::here("post_check", "functions", "forest.R"))
source(here::here("post_check", "functions", "condensed_figures.R"))

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

investigation_type <- "primary"
df_few <- tibble()
model_type <- "ethnicity_ses"

cohorts <- c(
  "older_adults", "adults", "children_and_adolescents",
  "infants", "infants_subgroup"
)

for (cohort in cohorts) {
  message("Running key-vars condensed (adjustment ratio, all seasons): ", cohort)
  tryCatch(
    run_cohort_condensed_key_vars_adjustment_ratio(
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
