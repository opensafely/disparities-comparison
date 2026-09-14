# Further ethnicity_ses condensed key-vars with covariate-row layout,
# level colours, and ethnicity Unknown excluded.
# Prefer the full pipeline:
#   Rscript post_check/recreate_all_outputs.R
# or condensed only:
#   Rscript post_check/recreate_all_outputs.R --only=condensed

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

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohorts <- c(
    "older_adults", "adults", "children_and_adolescents",
    "infants", "infants_subgroup"
  )
} else {
  cohorts <- args
}

for (cohort in cohorts) {
  message("Running key-vars covariate-row condensed (ethnicity_ses): ", cohort)
  tryCatch(
    run_cohort_condensed_key_vars_by_covariate(
      cohort = cohort,
      model_type = model_type,
      drop_unknown_ethnicity = TRUE
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
