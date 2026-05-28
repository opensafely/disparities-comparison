# Ethnicity_ses adjustment-ratio condensed figures (further / minimal RR).
# Selected seasons: 2017-18, 2018-19, 2020-21.
# Output: post_check/plots/primary_analyses/forest_model_ratios_by_virus/
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

KEY_EXPOSURE_SEASONS <- c("2017_18", "2018_19", "2020_21")

cohorts <- c(
  "older_adults", "adults", "children_and_adolescents",
  "infants", "infants_subgroup"
)

for (cohort in cohorts) {
  message(
    "Running key-vars condensed (adjustment ratio, seasons ",
    paste(gsub("_", "-", KEY_EXPOSURE_SEASONS), collapse = ", "),
    "): ", cohort
  )
  tryCatch(
    run_cohort_condensed_key_vars_seasons_adjustment_ratio(
      cohort = cohort,
      seasons = KEY_EXPOSURE_SEASONS,
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
