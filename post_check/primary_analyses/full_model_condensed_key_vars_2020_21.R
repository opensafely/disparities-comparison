# Fully adjusted full models (ethnicity, IMD, household composition): key-variable
# figures with covariate-row layout for 2020-21 season only.
# COVID panels show 2020-21 only. Infants subgroup uses ethnicity_ses (no
# household composition full model); figure heights are shorter than the
# all-seasons covariate-row defaults.
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
  ~cohort,                    ~model_type,
  "children_and_adolescents", "full"
)

out_root <- here::here(
  "post_check", "plots", "supplemental", "condensed_models_key_vars"
)

for (i in seq_len(nrow(cohort_specs))) {
  cohort <- cohort_specs$cohort[[i]]
  model_type <- cohort_specs$model_type[[i]]
  message(
    "Running covariate-row key-vars (2020-21, further adjusted, ",
    model_type, "): ", cohort
  )
  tryCatch(
    run_cohort_condensed_key_vars_by_covariate(
      cohort = cohort,
      model_type = model_type,
      drop_unknown_ethnicity = TRUE,
      seasons = seasons,
      out_root = out_root
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
