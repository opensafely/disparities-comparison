library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_rsv"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2017-09-01"
  study_end_date <- "2018-08-31"
  cohort <- "older_adults"
  codelist_type <- "specific"
  investigation_type <- "secondary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#remove rows with missing values in any of the variables used in models
#outcome will never be NA (as part of processing pipeline) so does not need to be filtered
if (cohort == "infants_subgroup") {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(composition_category),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification),
           !is.na(maternal_age), !is.na(maternal_smoking_status),
           !is.na(maternal_drinking), !is.na(maternal_drug_usage),
           !is.na(maternal_flu_vaccination),
           !is.na(maternal_pertussis_vaccination))
  
} else {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(composition_category),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification))
  
}

#import event counting function
source(here::here("analysis", "functions", "event_count.R"))

#calculate events per group
events <- group_specific_events_further(
  df_input, c("latest_ethnicity_group", "composition_category"),
  "rsv_primary_inf", "rsv_secondary_inf")

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
  rsv_mild_ethnicity_hh_comp_further_output <- data.frame(
    term = "too few events", estimate = NA, std.error = NA,
    statistic = NA, p.value = NA, conf.low = NA, conf.high = NA)
  
} else {
  
  #rsv primary by ethnicity and household composition
  rsv_mild_ethnicity_hh_comp_further_output <- glm_poisson_further(
    df_input, c("latest_ethnicity_group", "composition_category"),
    "rsv_primary_inf", offset_var = "time_rsv_primary")
  
}

#run severe model
if (too_few_events_severe) {
  
  #create data frame with same columns as model output creates
  rsv_severe_ethnicity_hh_comp_further_output <- data.frame(
    term = "too few events", estimate = NA, std.error = NA,
    statistic = NA, p.value = NA, conf.low = NA, conf.high = NA)
  
} else {
  
  #rsv primary by ethnicity and household composition
  rsv_severe_ethnicity_hh_comp_further_output <- glm_poisson_further(
    df_input, c("latest_ethnicity_group", "composition_category"),
    "rsv_secondary_inf", offset_var = "time_rsv_secondary")
  
}

#define a vector of names for the model outputs
model_names <- c("Mild RSV by Ethnicity and Household Composition",
                 "Severe RSV by Ethnicity and Household Composition")

#create the model outputs list
model_outputs_list <- list(rsv_mild_ethnicity_hh_comp_further_output, 
                           rsv_severe_ethnicity_hh_comp_further_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          paste0("rsv_", investigation_type)))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("rsv_", investigation_type)), "/", 
                            "rsv_ethnicity_hh_comp_further_model_outputs_",
                            cohort, "_", year(study_start_date), "_",
                            year(study_end_date), "_", codelist_type, "_",
                            investigation_type, ".csv"))
  
} else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("rsv_", investigation_type)), "/", 
                            "further_rsv_ethnicity_hh_comp_model_outputs_",
                            cohort, "_", year(study_start_date), "_",
                            year(study_end_date), "_", codelist_type, "_",
                            investigation_type, ".csv"))
  
}
