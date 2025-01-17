library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "overall_analyses", "outcome_covid"))

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

#remove rows with missing values in any of the variables used in models
#outcome will never be NA (as part of processing pipeline) so does not need to be filtered
if (cohort == "infants_subgroup") {
  
  df_input <- df_input %>% 
    filter(!is.na(imd_quintile), !is.na(age_band), !is.na(sex),
           !is.na(rurality_classification), !is.na(maternal_age),
           !is.na(maternal_smoking_status), !is.na(maternal_drinking),
           !is.na(maternal_drug_usage), !is.na(maternal_flu_vaccination),
           !is.na(maternal_pertussis_vaccination))
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {
  
  df_input <- df_input %>% 
    filter(!is.na(imd_quintile), !is.na(age_band), !is.na(sex),
           !is.na(rurality_classification), !is.na(has_asthma),
           !is.na(has_copd), !is.na(has_cystic_fibrosis),
           !is.na(has_other_resp), !is.na(has_diabetes), !is.na(has_addisons),
           !is.na(severe_obesity), !is.na(has_chd), !is.na(has_ckd),
           !is.na(has_cld), !is.na(has_cnd), !is.na(has_cancer),
           !is.na(immunosuppressed), !is.na(has_sickle_cell),
           !is.na(smoking_status), !is.na(hazardous_drinking),
           !is.na(drug_usage))
  
} else {
  
  df_input <- df_input %>% 
    filter(!is.na(imd_quintile), !is.na(age_band),
           !is.na(sex), !is.na(rurality_classification))
  
}

#import event counting function
source(here::here("analysis", "functions", "event_count.R"))

#calculate events per group
events <- group_specific_events(df_input, c("imd_quintile"), "covid_primary_inf",
                                "covid_secondary_inf")

#check if there are too few events
too_few_events_mild <- any(events$enough_events_mild == FALSE)
too_few_events_severe <- any(events$enough_events_severe == FALSE)

#show the event counts if there are too few events
if (too_few_events_mild | too_few_events_severe) print(events)

if (cohort == "infants_subgroup") {
  
  if (too_few_events_mild) {
  
    #create data frame with the same columns as model outputs
    covid_mild_ses_output <- data.frame(term = "too few events",
                                        estimate = NA, std.error = NA,
                                        statistic = NA, p.value = NA,
                                        conf.low = NA, conf.high = NA)
  
  } else {
  
    #covid primary by ses
    covid_mild_ses <- glm(covid_primary_inf ~ imd_quintile +
                            age_band + sex + rurality_classification +
                            maternal_age + maternal_smoking_status +
                            maternal_drinking + maternal_drug_usage + 
                            maternal_flu_vaccination + 
                            maternal_pertussis_vaccination +
                            offset(log(time_covid_primary*1000)),
                          data = df_input, family = poisson)
    covid_mild_ses_output <- tidy(covid_mild_ses, conf.int = TRUE)
  
  }
  
  if (too_few_events_severe) {
  
    #create data frame with the same columns as model outputs
    covid_severe_ses_output <- data.frame(term = "too few events",
                                          estimate = NA, std.error = NA,
                                          statistic = NA, p.value = NA,
                                          conf.low = NA, conf.high = NA)
 
  } else {
  
    #covid secondary by ses
    covid_severe_ses <- glm(covid_secondary_inf ~ imd_quintile +
                              age_band + sex + rurality_classification +
                              maternal_age + maternal_smoking_status +
                              maternal_drinking + maternal_drug_usage + 
                              maternal_flu_vaccination + 
                              maternal_pertussis_vaccination +
                              offset(log(time_covid_secondary*1000)),
                            data = df_input, family = poisson)
    covid_severe_ses_output <- tidy(covid_severe_ses, conf.int = TRUE)
  
  }
  
  # #covid mortality by ses
  # covid_mortality_ses <- glm(covid_mortality_inf ~ imd_quintile +
  #                              age_band + sex + rurality_classification +
  #                              maternal_age + maternal_smoking_status +
  #                              maternal_drinking + maternal_drug_usage + 
  #                              maternal_flu_vaccination + 
  #                              maternal_pertussis_vaccination +
  #                              offset(log(time_covid_mortality*1000)),
  #                            data = df_input, family = poisson)
  # covid_mortality_ses_output <- tidy(covid_mortality_ses, conf.int = TRUE)
 
} else {
  
  if (too_few_events_mild) {
  
    #create data frame with the same columns as model outputs
    covid_mild_ses_output <- data.frame(term = "too few events",
                                        estimate = NA, std.error = NA,
                                        statistic = NA, p.value = NA,
                                        conf.low = NA, conf.high = NA)
  
  } else {
  
    #covid primary by ses
    covid_mild_ses <- glm(covid_primary_inf ~ imd_quintile +
                            age_band + sex + rurality_classification +
                            offset(log(time_covid_primary*1000)),
                          data = df_input, family = poisson)
    covid_mild_ses_output <- tidy(covid_mild_ses, conf.int = TRUE)
 
  }
  
  if (too_few_events_severe) {
  
    #create data frame with the same columns as model outputs
    covid_severe_ses_output <- data.frame(term = "too few events",
                                          estimate = NA, std.error = NA,
                                          statistic = NA, p.value = NA,
                                          conf.low = NA, conf.high = NA)
 
  } else {
  
  #covid secondary by ses
  covid_severe_ses <- glm(covid_secondary_inf ~ imd_quintile +
                            age_band + sex + rurality_classification +
                            offset(log(time_covid_secondary*1000)),
                          data = df_input, family = poisson)
  covid_severe_ses_output <- tidy(covid_severe_ses, conf.int = TRUE)
 
  }
  
  # #covid mortality by ses
  # covid_mortality_ses <- glm(covid_mortality_inf ~ imd_quintile +
  #                              age_band + sex + rurality_classification +
  #                              offset(log(time_covid_mortality*1000)),
  #                            data = df_input, family = poisson)
  # covid_mortality_ses_output <- tidy(covid_mortality_ses, conf.int = TRUE)

}

#define a vector of names for the model outputs
model_names <- c("Mild COVID-19 by IMD Quintile",
                 "Severe COVID-19 by IMD Quintile")#,
                 # "COVID-19 Mortality by IMD Quintile")
  
#create the model outputs list
model_outputs_list <- list(covid_mild_ses_output, covid_severe_ses_output)#,
                           # covid_mortality_ses_output)
  
#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))
  
## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          "covid_overall"))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results",
                            "models", "covid_overall"), "/",
                            "covid_ses_model_outputs_",
                            cohort, "_specific_primary.csv"))
  
}  else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results",
                            "models", "covid_overall"), "/",
                            "covid_ses_model_outputs_",
                            cohort, "_specific_primary.csv"))
  
}
