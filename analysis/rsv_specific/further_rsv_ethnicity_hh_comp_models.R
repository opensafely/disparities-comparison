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
covid_season_min <- as.Date("2019-09-01")

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
                                      year(study_start_date), "_", year(study_end_date), "_", 
                                      codelist_type, "_", investigation_type,".arrow"))) 
  
  #add models for infants subgroup
  #} else if (cohort == "infants_subgroup") {
  
#} else {
  #rsv primary by ethnicity and household composition
  rsv_mild_ethnicity_hh_comp_further <- glm(rsv_primary_inf ~ latest_ethnicity_group +
                                              composition_category + age_band + sex + 
                                              rurality_classification + 
                                              prior_flu_vaccination +
                                              flu_vaccination +
                                              offset(log(time_rsv_primary)), 
                                            data = df_input, family = poisson)
  rsv_mild_ethnicity_hh_comp_further_output <- tidy(rsv_mild_ethnicity_hh_comp_further)
  
  #rsv secondary by ethnicity and household composition
  rsv_severe_ethnicity_hh_comp_further <- glm(rsv_secondary_inf ~ latest_ethnicity_group +
                                                composition_category + age_band + sex + 
                                                rurality_classification + 
                                                prior_flu_vaccination +
                                                flu_vaccination +
                                                offset(log(time_rsv_secondary)),
                                              data = df_input, family = poisson)
  rsv_severe_ethnicity_hh_comp_further_output <- tidy(rsv_severe_ethnicity_hh_comp_further)
  
  #rsv mortality by ethnicity and household composition
  rsv_mortality_ethnicity_hh_comp_further <- glm(rsv_mortality ~ latest_ethnicity_group + 
                                                   composition_category + age_band + sex + 
                                                   rurality_classification + 
                                                   prior_flu_vaccination +
                                                   flu_vaccination +
                                                   offset(log(time_rsv_mortality)),
                                                 data = df_input, family = poisson)
  rsv_mortality_ethnicity_hh_comp_further_output <- tidy(rsv_mortality_ethnicity_hh_comp_further)
  
  if (study_start_date >= covid_season_min) {
    #rsv primary by ethnicity and household composition
    rsv_mild_ethnicity_hh_comp_further <- glm(rsv_primary_inf ~ latest_ethnicity_group +
                                                composition_category + age_band + sex + 
                                                rurality_classification + 
                                                prior_flu_vaccination +
                                                flu_vaccination +
                                                time_since_last_covid_vaccination +
                                                covid_vaccination +
                                                offset(log(time_rsv_primary)), 
                                              data = df_input, family = poisson)
    rsv_mild_ethnicity_hh_comp_further_output <- tidy(rsv_mild_ethnicity_hh_comp_further)
    
    #rsv secondary by ethnicity and household composition
    rsv_severe_ethnicity_hh_comp_further <- glm(rsv_secondary_inf ~ latest_ethnicity_group +
                                                  composition_category + age_band + sex + 
                                                  rurality_classification + 
                                                  prior_flu_vaccination +
                                                  flu_vaccination +
                                                  time_since_last_covid_vaccination +
                                                  covid_vaccination +
                                                  offset(log(time_rsv_secondary)),
                                                data = df_input, family = poisson)
    rsv_severe_ethnicity_hh_comp_further_output <- tidy(rsv_severe_ethnicity_hh_comp_further)
    
    #rsv mortality by ethnicity and household composition
    rsv_mortality_ethnicity_hh_comp_further <- glm(rsv_mortality ~ latest_ethnicity_group +
                                                     composition_category + age_band + sex + 
                                                     rurality_classification + 
                                                     prior_flu_vaccination +
                                                     flu_vaccination +
                                                     time_since_last_covid_vaccination +
                                                     covid_vaccination +
                                                     offset(log(time_rsv_mortality)),
                                                   data = df_input, family = poisson)
    rsv_mortality_ethnicity_hh_comp_further_output <- tidy(rsv_mortality_ethnicity_hh_comp_further)
  }
#}

#define a vector of names for the model outputs
if (study_start_date < covid_season_min) {
  model_names <- c("Mild RSV by Ethnicity and Household Composition", 
                   "Severe RSV by Ethnicity and Household Composition",
                   "RSV Mortality By Ethnicity and Household Composition")
} else if (codelist_type == "sensitive") {
  model_names <- c("Mild RSV by Ethnicity and Household Composition", 
                   "Severe RSV by Ethnicity and Household Composition",
                   "RSV Mortality By Ethnicity and Household Composition")
} else if (study_start_date >= covid_season_min) {
  model_names <- c("Mild RSV by Ethnicity and Household Composition", 
                   "Severe RSV by Ethnicity and Household Composition",
                   "RSV Mortality By Ethnicity and Household Composition")
} else {
  model_names <- c("Mild RSV by Ethnicity and Household Composition",
                   "Severe RSV by Ethnicity and Household Composition",
                   "RSV Mortality By Ethnicity and Household Composition")
}

#create the model outputs list
model_outputs_list <- list(rsv_mild_ethnicity_hh_comp_further_output, 
                           rsv_severe_ethnicity_hh_comp_further_output,
                           rsv_mortality_ethnicity_hh_comp_further_output)

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
                            "further_rsv_ethnicity_hh_comp_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models"), "/", 
                            "further_rsv_ethnicity_hh_comp_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
}
