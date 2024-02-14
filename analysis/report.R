library("tidyverse")
library("here")
library("arrow")
library("ggplot2")
library("data.table")

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
study_start_date <- study_dates[[args[[2]]]]
study_end_date <- study_dates[[args[[3]]]]
cohort <- args[[1]]

df_input <- read_feather(
  here::here("output", paste0("input_processed_", cohort, "_", year(study_start_date),
                              "_", year(study_end_date), ".arrow")))

lab <- ifelse(cohort == "infants", "Age (Months)", 
       ifelse(cohort == "infants_subgroup", "Age (Months)", "Age (Years)"))

plot_age <- ggplot(data = df_input, aes(age, frequency(age))) + geom_col(width = 0.9) +
  xlab(lab) + ylab("Frequency")

ggsave(
  plot = plot_age,
  filename = paste0("descriptive_", cohort, "_", year(study_start_date),
    "_", year(study_end_date), ".png"), path = here::here("output"),
)


