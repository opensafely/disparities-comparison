library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "overall_analyses", "outcome_flu"))

#define cohort
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cohort <- "infants"
  } else {
    cohort <- args[[1]]
  }
}

df_input <- read_feather(
  here::here("output", "data", "overall_analyses",
             paste0("input_processed_combined_", cohort,
                    "_specific_primary.arrow")))

df_input <- df_input %>% 
  filter(!is.na(imd_quintile), !is.na(age_band), !is.na(sex))

#import event counting function
source(here::here("analysis", "functions", "event_count.R"))

#calculate events per group
events <- group_specific_events(
  df_input, c("imd_quintile"), "flu_primary_inf", "flu_secondary_inf")

#check if there are too few events
too_few_events_mild <- any(events$enough_events_mild == FALSE)
too_few_events_severe <- any(events$enough_events_severe == FALSE)

#show the event counts if there are too few events
if (too_few_events_mild | too_few_events_severe) print(events)

#import model function
source(here::here("analysis", "functions", "model.R"))

#run mild model
if (too_few_events_mild) {
  
  #create data frame with same columns as model output creates
  flu_mild_ses_output <- data.frame(
    term = "too few events", estimate = NA, std.error = NA,
    statistic = NA, p.value = NA, conf.low = NA, conf.high = NA)
  
} else {
  
  #flu by socioeconomic status
  flu_mild_ses_output <- glm_poisson(
    df_input, "imd_quintile", "flu_primary_inf", "time_flu_primary")
  
}

#run severe model
if (too_few_events_severe) {
  
  #create data frame with same columns as model output creates
  flu_severe_ses_output <- data.frame(
    term = "too few events", estimate = NA, std.error = NA,
    statistic = NA, p.value = NA, conf.low = NA, conf.high = NA)
  
} else {
  
  #flu by socioeconomic status
  flu_severe_ses_output <- glm_poisson(
    df_input, "imd_quintile", "flu_secondary_inf", "time_flu_secondary")
  
}

#define a vector of names for the model outputs
model_names <- c("Mild Influenza by IMD Quintile",
                 "Severe Influenza by IMD Quintile")


#create the model outputs list
model_outputs_list <- list(flu_mild_ses_output, flu_severe_ses_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here::here("output", "results", "models", "flu_overall"))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            "flu_overall"), "/", "flu_ses_model_outputs_",
                            cohort, "_specific_primary.csv"))
  
}  else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            "flu_overall"), "/", "flu_ses_model_outputs_",
                            cohort, "_specific_primary.csv"))
  
}
