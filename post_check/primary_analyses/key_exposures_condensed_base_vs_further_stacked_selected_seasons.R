# Ethnicity_ses base vs further condensed figures (RSV / flu / COVID stacked).
# Seasons: 2017-18, 2018-19, 2020-21.
# Minimally vs fully adjusted estimates are connected by a line per level (same season column);
# points only (no confidence intervals).
# Output: post_check/plots/primary_analyses/forest_models_by_virus/
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
    "Running key-vars condensed (base vs further stacked, seasons ",
    paste(gsub("_", "-", KEY_EXPOSURE_SEASONS), collapse = ", "),
    "): ", cohort
  )
  tryCatch(
    run_cohort_condensed_key_vars_seasons_base_vs_further_stacked(
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
