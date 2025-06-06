library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_overall_and_all_cause"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    study_start_date <- "2016-09-01"
    study_end_date <- "2017-08-31"
    cohort <- "infants"
    codelist_type <- "sensitive"
    investigation_type <- "primary"
  } else {
    study_start_date <- study_dates[[args[[2]]]]
    study_end_date <- study_dates[[args[[3]]]]
    cohort <- args[[1]]
    codelist_type <- args[[4]]
    investigation_type <- args[[5]]
  }
}

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#remove rows with missing values in any of the variables used in models
#outcome will never be NA (as part of processing pipeline) so does not need to be filtered
df_input <- df_input %>%
  filter(!is.na(latest_ethnicity_group), !is.na(imd_quintile),
         !is.na(age_band), !is.na(sex))

#import event counting function
source(here::here("analysis", "functions", "event_count.R"))

#calculate events per group
events <- group_specific_events(
  df_input, c("latest_ethnicity_group", "imd_quintile"),
  "overall_resp_primary_inf", "overall_resp_secondary_inf")

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
  overall_resp_mild_ethnicity_ses_output <- data.frame(
    term = "too few events", estimate = NA, std.error = NA,
    statistic = NA, p.value = NA, conf.low = NA, conf.high = NA)
  
} else {
  
  #overall primary by ethnicity and socioeconomic status
  overall_resp_mild_ethnicity_ses_output <- glm_poisson(
    df_input, c("latest_ethnicity_group", "imd_quintile"),
    "overall_resp_primary_inf", "time_overall_resp_primary")
  
}

#run severe model
if (too_few_events_severe) {
  
  #create data frame with same columns as model output creates
  overall_resp_severe_ethnicity_ses_output <- data.frame(
    term = "too few events", estimate = NA, std.error = NA,
    statistic = NA, p.value = NA, conf.low = NA, conf.high = NA)
  
} else {
  
  #overall primary by ethnicity and socioeconomic status
  overall_resp_severe_ethnicity_ses_output <- glm_poisson(
    df_input, c("latest_ethnicity_group", "imd_quintile"),
    "overall_resp_secondary_inf", "time_overall_resp_secondary")
  
}

#define a vector of names for the model outputs
model_names <- c("Mild Overall Respiratory Virus by Ethnicity and IMD Quintile",
                 "Severe Overall Respiratory Virus by Ethnicity and IMD Quintile")

#create the model outputs list
model_outputs_list <- list(overall_resp_mild_ethnicity_ses_output,
                           overall_resp_severe_ethnicity_ses_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          paste0("overall_and_all_cause_", investigation_type)))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("overall_and_all_cause_",
                            investigation_type)), "/", 
                            "overalL_and_all_cause_ethnicity_ses_model_outputs_",
                            cohort, "_", year(study_start_date), "_",
                            year(study_end_date), "_", codelist_type,
                            "_", investigation_type, ".csv"))
  
} else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("overall_and_all_cause_",
                            investigation_type)), "/", 
                            "overall_and_all_cause_ethnicity_ses_model_outputs_",
                            cohort, "_", year(study_start_date), "_",
                            year(study_end_date), "_", codelist_type,
                            "_", investigation_type, ".csv"))
  
}
