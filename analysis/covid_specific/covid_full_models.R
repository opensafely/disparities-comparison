library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis", "covid_specific"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2021-09-01"
  study_end_date <- "2022-08-31"
  cohort <- "infants"
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

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
                                      year(study_start_date), "_", year(study_end_date), "_", 
                                      codelist_type, "_", investigation_type,".arrow"))) 

if (cohort == "infants") {
  if (study_start_date >= covid_season_min) {
    #covid primary by ethnicity and socioeconomic status
    covid_mild_full <- glm(covid_primary_inf ~ latest_ethnicity_group + 
                             imd_quintile + composition_category +
                             age + sex + rurality_classification + 
                             offset(log(time_covid_primary)),
                           data = df_input, family = poisson)
    covid_mild_full_output <- tidy(covid_mild_full)
    
    #covid secondary by ethnicity and socioeconomic status
    covid_severe_full <- glm(covid_secondary_inf ~ latest_ethnicity_group + 
                               imd_quintile + composition_category + 
                               age + sex + rurality_classification + 
                               offset(log(time_covid_secondary)),
                             data = df_input, family = poisson)
    covid_severe_full_output <- tidy(covid_severe_full)
    
    #covid mortality by ethnicity and socioeconomic status
    covid_mortality_full <- glm(covid_mortality ~ latest_ethnicity_group + 
                                  imd_quintile + composition_category + 
                                  age + sex + rurality_classification + 
                                  offset(log(time_covid_mortality)),
                                data = df_input, family = poisson)
    covid_mortality_full_output <- tidy(covid_mortality_full)
  }
  
  #add models for infants subgroup
  #} else if (cohort == "infants_subgroup") {
  
} else {
  if (study_start_date >= covid_season_min) {
    #covid primary by ethnicity and socioeconomic status
    covid_mild_full <- glm(covid_primary_inf ~ latest_ethnicity_group + 
                             imd_quintile + composition_category +
                             age_band + sex + rurality_classification +
                             offset(log(time_covid_primary)),
                           data = df_input, family = poisson)
    covid_mild_full_output <- tidy(covid_mild_full)
    
    #covid secondary by ethnicity and socioeconomic status
    covid_severe_full <- glm(covid_secondary_inf ~ latest_ethnicity_group + 
                               imd_quintile + composition_category +
                               age_band + sex + rurality_classification + 
                               offset(log(time_covid_secondary)),
                             data = df_input, family = poisson)
    covid_severe_full_output <- tidy(covid_severe_full)
    
    #covid mortality by ethnicity and socioeconomic status
    covid_mortality_full <- glm(covid_mortality ~ latest_ethnicity_group + 
                                           imd_quintile + age_band + sex + 
                                           rurality_classification + 
                                           offset(log(time_covid_mortality)),
                                         data = df_input, family = poisson)
    covid_mortality_full_output <- tidy(covid_mortality_full)
  }
}

#define a vector of names for the model outputs
model_names <- c("Mild COVID-19 by Ethnicity, IMD Quintile and Household Composition", 
                 "Severe COVID-19 by Ethnicity, IMD Quintile and Household Composition",
                 "COVID-19 Mortality by Ethnicity, IMD Quintile and Household Composition")
  
#create the model outputs list
model_outputs_list <- list(covid_mild_full_output, 
                           covid_severe_full_output,
                           covid_mortality_full_output)
  
#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))
  
## create output directories ----
fs::dir_create(here("output", "results", "models"))
  
#save model output 
if (length(args) == 0) {
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("covid_", investigation_type)), "/", 
                            "covid_full_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date),
                            "_", codelist_type, "_", investigation_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("covid_", investigation_type)), "/", 
                            "covid_full_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
}
