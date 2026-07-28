# Fully adjusted full models (ethnicity, IMD, household composition): key-variable
# figures for children and adolescents, 2020-21 season only.
# Saves separate plots per virus (RSV / flu / COVID), with mild vs severe on each plot.
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

cohort <- "children_and_adolescents"
model_type <- "full"

message(
  "Running per-virus key-vars (2020-21, further adjusted, ",
  model_type, "): ", cohort
)
tryCatch(
  run_cohort_per_virus_key_vars_seasons_further(
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
