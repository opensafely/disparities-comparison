# Minimally adjusted (base) ethnicity_ses condensed key-vars for infants vs
# maternally linked infants. Same Age / IMD / Ethnicity covariate-row layout as
# further_results_condensed_key_vars.R, but columns are Infants | Maternally
# linked infants, with separate Mild and Severe figures.
# Output: post_check/plots/primary_analyses/condensed_models_key_vars/infants_vs_maternal/
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

message("Running infants vs maternally linked (minimally adjusted) condensed key-vars")
tryCatch(
  run_infants_vs_maternal_base_condensed_key_vars(
    model_type = model_type,
    drop_unknown_ethnicity = TRUE
  ),
  error = function(e) {
    message(
      "Failed: infants vs maternal base condensed",
      " model_type=", model_type,
      " :: ", conditionMessage(e)
    )
    NULL
  }
)
