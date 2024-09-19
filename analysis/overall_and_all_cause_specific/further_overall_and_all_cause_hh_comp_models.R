library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "adults"
  codelist_type <- "specific"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
                                      year(study_start_date), "_", year(study_end_date), "_", 
                                      codelist_type, "_", investigation_type,".arrow"))) 

# #add models for infants subgroup
# if (cohort == "infants_subgroup") {

# } else {

if (codelist_type == "sensitive") {
  #overall_resp primary by household composition
  overall_resp_mild_hh_comp_further <- glm(overall_resp_primary_inf ~ composition_category + 
                                             age_band + sex + 
                                             rurality_classification + 
                                             prior_flu_vaccination +
                                             flu_vaccination +
                                             offset(log(time_overall_resp_primary)),
                                           data = df_input, family = poisson)
  overall_resp_mild_hh_comp_further_output <- tidy(overall_resp_mild_hh_comp_further)
  
  #overall_resp secondary by household composition
  overall_resp_severe_hh_comp_further <- glm(overall_resp_secondary_inf ~ composition_category + 
                                               age_band + sex + 
                                               rurality_classification + 
                                               prior_flu_vaccination +
                                               flu_vaccination +
                                               offset(log(time_overall_resp_secondary)),
                                             data = df_input, family = poisson)
  overall_resp_severe_hh_comp_further_output <- tidy(overall_resp_severe_hh_comp_further)
  
  #overall_resp mortality by household composition
  overall_resp_mortality_hh_comp_further <- glm(overall_resp_mortality ~ composition_category + 
                                                  age_band + sex + 
                                                  rurality_classification + 
                                                  prior_flu_vaccination +
                                                  flu_vaccination +
                                                  offset(log(time_overall_resp_mortality)),
                                                data = df_input, family = poisson)
  overall_resp_mortality_hh_comp_further_output <- tidy(overall_resp_mortality_hh_comp_further)
}

#all cause mortality by household composition
all_cause_mortality_hh_comp_further <- glm(all_cause_mortality ~ composition_category + 
                                             age_band + sex + 
                                             rurality_classification + 
                                             prior_flu_vaccination +
                                             flu_vaccination +
                                             offset(log(time_all_cause_mortality)),
                                           data = df_input, family = poisson)
all_cause_mortality_hh_comp_further_output <- tidy(all_cause_mortality_hh_comp_further)

if (study_start_date >= covid_season_min) {
  if (codelist_type == "sensitive") {
    #overall_resp primary by household composition
    overall_resp_mild_hh_comp_further <- glm(overall_resp_primary_inf ~ composition_category + 
                                               age_band + sex + 
                                               rurality_classification +
                                               prior_flu_vaccination +
                                               flu_vaccination +
                                               time_since_last_covid_vaccination +
                                               covid_vaccination +
                                               offset(log(time_overall_resp_primary)),
                                             data = df_input, family = poisson)
    overall_resp_mild_hh_comp_further_output <- tidy(overall_resp_mild_hh_comp_further)
    
    #overall_resp secondary by household composition
    overall_resp_severe_hh_comp_further <- glm(overall_resp_secondary_inf ~ composition_category + 
                                                 age_band + sex + 
                                                 rurality_classification + 
                                                 prior_flu_vaccination +
                                                 flu_vaccination +
                                                 time_since_last_covid_vaccination +
                                                 covid_vaccination +
                                                 offset(log(time_overall_resp_secondary)),
                                               data = df_input, family = poisson)
    overall_resp_severe_hh_comp_further_output <- tidy(overall_resp_severe_hh_comp_further)
    
    #overall_resp mortality by household composition
    overall_resp_mortality_hh_comp_further <- glm(overall_resp_mortality ~ composition_category + 
                                                    age_band + sex + 
                                                    rurality_classification + 
                                                    prior_flu_vaccination +
                                                    flu_vaccination +
                                                    time_since_last_covid_vaccination +
                                                    covid_vaccination +
                                                    offset(log(time_overall_resp_mortality)),
                                                  data = df_input, family = poisson)
    overall_resp_mortality_hh_comp_further_output <- tidy(overall_resp_mortality_hh_comp_further)
  }
  
  #all cause mortality by household composition
  all_cause_mortality_hh_comp_further <- glm(all_cause_mortality ~ composition_category + 
                                               age_band + sex + 
                                               rurality_classification + 
                                               prior_flu_vaccination +
                                               flu_vaccination +
                                               time_since_last_covid_vaccination +
                                               covid_vaccination +
                                               offset(log(time_all_cause_mortality)),
                                             data = df_input, family = poisson)
  all_cause_mortality_hh_comp_further_output <- tidy(all_cause_mortality_hh_comp_further)
}
# }

#define a vector of names for the model outputs
if (study_start_date < covid_season_min) {
  model_names <- c("All Cause Mortality by Household Composition")
} else if (codelist_type == "sensitive") {
  model_names <- c("Mild Overall Respiratory Virus by Household Composition",
                   "Severe Overall Respiratory Virus by Household Composition",
                   "Overall Respiratory Virus Mortality by Household Composition",
                   "All Cause Mortality by Household Composition")
} else if (study_start_date >= covid_season_min) {
  model_names <- c("All Cause Mortality by Household Composition")
} else {
  model_names <- c("Mild Overall Respiratory Virus by Household Composition",
                   "Severe Overall Respiratory Virus by Household Composition",
                   "Overall Respiratory Virus Mortality by Household Composition",
                   "All Cause Mortality by Household Composition")
}

#define a vector of names for the model outputs
if (study_start_date < covid_season_min) {
  model_names <- c("All Cause Mortality by Household Composition")
} else if (codelist_type == "sensitive") {
  model_names <- c("Mild Overall Respiratory Virus by Household Composition",
                   "Severe Overall Respiratory Virus by Household Composition",
                   "Overall Respiratory Virus Mortality by Household Composition",
                   "All Cause Mortality by Household Composition")
} else if (study_start_date >= covid_season_min) {
  model_names <- c("All Cause Mortality by Household Composition")
} else {
  model_names <- c("Mild Overall Respiratory Virus by Household Composition",
                   "Severe Overall Respiratory Virus by Household Composition",
                   "Overall Respiratory Virus Mortality by Household Composition",
                   "All Cause Mortality by Household Composition")
}

#create the model outputs list
model_outputs_list <- list(all_cause_mortality_hh_comp_further_output)

#adjust the model outputs list based on the conditions
if (codelist_type == "sensitive") {
  model_outputs_list <- c(model_outputs_list, list(overall_resp_mild_hh_comp_further_output,
                                                   overall_resp_severe_hh_comp_further_output,
                                                   overall_resp_mortality_hh_comp_further_output))
}

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here("output", "results", "models"))

#save model output 
if (length(args) == 0) {
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models"), "/", 
                            "further_overall_and_all_cause_hh_comp_model_outputs_", 
                            cohort, "_", year(study_start_date), 
                            "_", year(study_end_date), "_", codelist_type, 
                            "_", investigation_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models"), "/", 
                            "further_overall_and_all_cause_hh_comp_model_outputs_", 
                            cohort, "_", year(study_start_date),
                            "_", year(study_end_date), "_", codelist_type, 
                            "_", investigation_type, ".csv"))
}
