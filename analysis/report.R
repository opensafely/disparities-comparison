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
study_start_date <- as.Date(as.numeric(study_dates[args[2]]), format = "%Y-%m-%d", origin = "1970-01-01")
study_end_date <- as.Date(as.numeric(study_dates[args[3]]), format = "%Y-%m-%d", origin = "1970-01-01")

df_input <- read_feather(
  here::here("output", paste0("input_processed_", args[[1]], "_", 
    year(study_start_date), "_", year(study_end_date), ".arrow")))

lab <- ifelse(args[1] == "infants", "Age (Months)", 
       ifelse(args[1] == "infants_subgroup", "Age (Months)", "Age (Years)"))

plot_age <- ggplot(data = df_input, aes(age, frequency(age))) + geom_col(width = 0.9) +
  xlab(lab) + ylab("Frequency")

ggsave(
  plot = plot_age,
  filename = paste0("descriptive_", args[[1]], "_", year(study_start_date),
    "_", year(study_end_date), ".png"), path = here::here("output"),
)
