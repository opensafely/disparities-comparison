library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis", "outcome_covid"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    study_start_date <- "2021-09-01"
    study_end_date <- "2022-08-31"
    cohort <- "older_adults"
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
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
                                      year(study_start_date), "_", year(study_end_date), "_", 
                                      codelist_type, "_", investigation_type,".arrow"))) 

if (study_start_date >= covid_prior_vacc_min) {
  
  #covid primary by ethnicity
  covid_mild_ethnicity_further <- glm(covid_primary_inf ~ latest_ethnicity_group +
                                        age_band + sex + rurality_classification +
                                        time_since_last_covid_vaccination +
                                        covid_vaccination_mild +
                                        offset(log(time_covid_primary)),
                                      data = df_input, family = poisson)
  covid_mild_ethnicity_further_output <- tidy(covid_mild_ethnicity_further)
  
  #covid secondary by ethnicity
  covid_severe_ethnicity_further <- glm(covid_secondary_inf ~ latest_ethnicity_group + 
                                          age_band + sex + rurality_classification + 
                                          time_since_last_covid_vaccination +
                                          covid_vaccination_severe +
                                          offset(log(time_covid_secondary)),
                                        data = df_input, family = poisson)
  covid_severe_ethnicity_further_output <- tidy(covid_severe_ethnicity_further)
  
  #covid mortality by ethnicity
  covid_mortality_ethnicity_further <- glm(covid_mortality ~ latest_ethnicity_group + 
                                             age_band + sex + rurality_classification + 
                                             time_since_last_covid_vaccination +
                                             covid_vaccination +
                                             offset(log(time_covid_mortality)),
                                           data = df_input, family = poisson)
  covid_mortality_ethnicity_further_output <- tidy(covid_mortality_ethnicity_further)

} else if (study_start_date == covid_current_vacc_min) {
  
  #covid primary by ethnicity
  covid_mild_ethnicity_further <- glm(covid_primary_inf ~ latest_ethnicity_group +
                                        age_band + sex + rurality_classification +
                                        covid_vaccination_mild +
                                        offset(log(time_covid_primary)),
                                      data = df_input, family = poisson)
  covid_mild_ethnicity_further_output <- tidy(covid_mild_ethnicity_further)
  
  #covid secondary by ethnicity
  covid_severe_ethnicity_further <- glm(covid_secondary_inf ~ latest_ethnicity_group + 
                                          age_band + sex + rurality_classification + 
                                          covid_vaccination_severe +
                                          offset(log(time_covid_secondary)),
                                        data = df_input, family = poisson)
  covid_severe_ethnicity_further_output <- tidy(covid_severe_ethnicity_further)
  
  #covid mortality by ethnicity
  covid_mortality_ethnicity_further <- glm(covid_mortality ~ latest_ethnicity_group + 
                                             age_band + sex + rurality_classification + 
                                             covid_vaccination +
                                             offset(log(time_covid_mortality)),
                                           data = df_input, family = poisson)
  covid_mortality_ethnicity_further_output <- tidy(covid_mortality_ethnicity_further)

}

#define a vector of names for the model outputs
model_names <- c("Mild COVID-19 by Ethnicity", "Severe COVID-19 by Ethnicity",
                 "COVID-19 Mortality by Ethnicity")

#create the model outputs list
model_outputs_list <- list(covid_mild_ethnicity_further_output, 
                           covid_severe_ethnicity_further_output,
                           covid_mortality_ethnicity_further_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here("output", "results", "models", paste0("covid_", investigation_type)))

#save model output 
if (length(args) == 0) {
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models", 
                            paste0("covid_", investigation_type)), "/", 
                            "further_covid_ethnicity_model_outputs_", cohort, "_", year(study_start_date), 
                            "_", year(study_end_date), "_", codelist_type,
                            "_", investigation_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("covid_", investigation_type)), "/", 
                            "further_covid_ethnicity_model_outputs_", cohort, "_", year(study_start_date),
                            "_", year(study_end_date), "_", codelist_type, 
                            "_", investigation_type, ".csv"))
}
