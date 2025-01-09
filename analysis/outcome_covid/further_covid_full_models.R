library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_covid"))

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
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#remove rows with missing values in any of the variables used in models
#outcome will never be NA (as part of processing pipeline) so does not need to be filtered
#vaccination will also never be NA as part of processing pipeline
  
df_input <- df_input %>% 
  filter(!is.na(latest_ethnicity_group), !is.na(imd_quintile),
         !is.na(composition_category), !is.na(age_band), !is.na(sex),
         !is.na(rurality_classification))
 
  
#covid primary by ethnicity and socioeconomic status
covid_mild_full_further <- glm(covid_primary_inf ~ latest_ethnicity_group +
                                 imd_quintile + composition_category +
                                 age_band + sex + rurality_classification +
                                 covid_vaccination_mild +
                                 offset(log(time_covid_primary*1000)),
                               data = df_input, family = poisson)
covid_mild_full_further_output <- tidy(covid_mild_full_further, confint = TRUE)

#covid secondary by ethnicity and socioeconomic status
covid_severe_full_further <- glm(covid_secondary_inf ~ latest_ethnicity_group +
                                   imd_quintile + composition_category +
                                   age_band + sex + rurality_classification +
                                   covid_vaccination_severe +
                                   offset(log(time_covid_secondary*1000)),
                                 data = df_input, family = poisson)
covid_severe_full_further_output <- tidy(covid_severe_full_further, confint = TRUE)

# #covid mortality by ethnicity and socioeconomic status
# covid_mortality_full_further <- glm(covid_mortality_inf ~ latest_ethnicity_group +
#                                       imd_quintile + age_band + sex +
#                                       rurality_classification +
#                                       covid_vaccination +
#                                       offset(log(time_covid_mortality*1000)),
#                                     data = df_input, family = poisson)
# covid_mortality_full_further_output <- tidy(covid_mortality_full_further, confint = TRUE)


#define a vector of names for the model outputs
model_names <- c("Mild COVID-19 by Ethnicity, IMD Quintile and Household Composition", 
                 "Severe COVID-19 by Ethnicity, IMD Quintile and Household Composition")#,
                 # "COVID-19 Mortality by Ethnicity, IMD Quintile and Household Composition")
  
#create the model outputs list
model_outputs_list <- list(covid_mild_full_further_output, covid_severe_full_further_output)#,
                           # covid_mortality_full_further_output)
  
#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))
  
## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          paste0("covid_", investigation_type)))
  
#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models", 
                            paste0("covid_", investigation_type)), "/", 
                            "further_covid_full_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date),
                            "_", codelist_type, "_", investigation_type, ".csv"))
  
} else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models", 
                            paste0("covid_", investigation_type)), "/", 
                            "further_covid_full_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
  
}
