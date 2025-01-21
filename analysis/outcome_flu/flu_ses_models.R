library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "outcome_flu"))

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

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow"))) 

#remove rows with missing values in any of the variables using in models
#outcome will never be NA (as part of processing pipeline) so does not need to be filtered
# if (cohort == "infants_subgroup") {
#   
#   df_input <- df_input %>% 
#     filter(!is.na(latest_ethnicity_group), !is.na(age_band), !is.na(sex),
#            #!is.na(rurality_classification),
#            !is.na(maternal_age), !is.na(maternal_smoking_status),
#            !is.na(maternal_drinking), !is.na(maternal_drug_usage),
#            !is.na(maternal_flu_vaccination),
#            !is.na(maternal_pertussis_vaccination))
#   
# } else 

if (cohort == "older_adults" & investigation_type == "secondary") {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(age_band), !is.na(sex),
           #!is.na(rurality_classification),
           !is.na(has_asthma), !is.na(has_copd), !is.na(has_cystic_fibrosis),
           !is.na(has_other_resp), !is.na(has_diabetes), !is.na(has_addisons),
           !is.na(severe_obesity), !is.na(has_chd), !is.na(has_ckd),
           !is.na(has_cld), !is.na(has_cnd), !is.na(has_cancer),
           !is.na(immunosuppressed), !is.na(has_sickle_cell),
           !is.na(smoking_status), !is.na(hazardous_drinking),
           !is.na(drug_usage))
  
} else {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(age_band),
           !is.na(sex))#, !is.na(rurality_classification))
  
}

#import event counting function
source(here::here("analysis", "functions", "event_count.R"))

#calculate events per group
events <- group_specific_events(df_input, c("imd_quintile"),
                                "flu_primary_inf", "flu_secondary_inf")

#check if there are too few events
too_few_events_mild <- any(events$enough_events_mild == FALSE)
too_few_events_severe <- any(events$enough_events_severe == FALSE)

#show the event counts if there are too few events
if (too_few_events_mild | too_few_events_severe) print(events)

# if (cohort == "infants_subgroup") {
#   
#   if (too_few_events_mild) {
#  
#     #create data frame with same columns as model output creates
#     flu_mild_ses_output <- data.frame(term = "too few events", estimate = NA,
#                                       std.error = NA, statistic = NA,
#                                       p.value = NA, conf.low = NA,
#                                       conf.high = NA)
#   
#   } else {
#   
#     #flu primary by ses
#     flu_mild_ses <- glm(flu_primary_inf ~ imd_quintile + 
#                           age_band + sex + #rurality_classification + 
#                           maternal_age + maternal_smoking_status +
#                           maternal_drinking + maternal_drug_usage + 
#                           maternal_flu_vaccination + 
#                           maternal_pertussis_vaccination +
#                           offset(log(time_flu_primary*1000)),
#                         data = df_input, family = poisson)
#     flu_mild_ses_output <- tidy(flu_mild_ses, conf.int = TRUE)
#   
#   }
#   
#   if (too_few_events_severe) {
#  
#     #create data frame with same columns as model output creates
#     flu_severe_ses_output <- data.frame(term = "too few events", estimate = NA,
#                                         std.error = NA, statistic = NA,
#                                         p.value = NA, conf.low = NA,
#                                         conf.high = NA)
#   
#   } else {
#   
#     #flu secondary by ses
#     flu_severe_ses <- glm(flu_secondary_inf ~ imd_quintile + 
#                             age_band + sex + #rurality_classification + 
#                             maternal_age + maternal_smoking_status +
#                             maternal_drinking + maternal_drug_usage + 
#                             maternal_flu_vaccination + 
#                             maternal_pertussis_vaccination +
#                             offset(log(time_flu_secondary*1000)),
#                           data = df_input, family = poisson)
#     flu_severe_ses_output <- tidy(flu_severe_ses, conf.int = TRUE)
#   
#   }
#   
#   # #flu mortality by ses
#   # flu_mortality_ses <- glm(flu_mortality_inf ~ imd_quintile + 
#   #                            age_band + sex + #rurality_classification + 
#   #                            maternal_age + maternal_smoking_status +
#   #                            maternal_drinking + maternal_drug_usage + 
#   #                            maternal_flu_vaccination + 
#   #                            maternal_pertussis_vaccination +
#   #                            offset(log(time_flu_mortality*1000)),
#   #                          data = df_input, family = poisson)
#   # flu_mortality_ses_output <- tidy(flu_mortality_ses, conf.int = TRUE)
#   
# } else 

if (cohort == "older_adults" & investigation_type == "secondary") {
  
  if (too_few_events_mild) {
 
    #create data frame with same columns as model output creates
    flu_mild_ses_output <- data.frame(term = "too few events", estimate = NA,
                                      std.error = NA, statistic = NA,
                                      p.value = NA, conf.low = NA,
                                      conf.high = NA)
  
  } else {
  
    #flu primary by ses
    flu_mild_ses <- glm(flu_primary_inf ~ imd_quintile + 
                          age_band + sex + #rurality_classification +
                          has_asthma + has_copd + has_cystic_fibrosis +
                          has_other_resp + has_diabetes + has_addisons +
                          severe_obesity + has_chd + has_ckd + has_cld +
                          has_cnd + has_cancer + immunosuppressed +
                          has_sickle_cell + smoking_status +
                          hazardous_drinking + drug_usage +
                          offset(log(time_flu_primary*1000)),
                        data = df_input, family = poisson)
    flu_mild_ses_output <- tidy(flu_mild_ses, conf.int = TRUE)
  
  }
  
  if (too_few_events_severe) {
 
    #create data frame with same columns as model output creates
    flu_severe_ses_output <- data.frame(term = "too few events", estimate = NA,
                                        std.error = NA, statistic = NA,
                                        p.value = NA, conf.low = NA,
                                        conf.high = NA)
  
  } else {
  
    #flu secondary by ses
    flu_severe_ses <- glm(flu_secondary_inf ~ imd_quintile + 
                            age_band + sex + #rurality_classification +
                            has_asthma + has_copd + has_cystic_fibrosis +
                            has_other_resp + has_diabetes + has_addisons +
                            severe_obesity + has_chd + has_ckd + has_cld +
                            has_cnd + has_cancer + immunosuppressed +
                            has_sickle_cell + smoking_status +
                            hazardous_drinking + drug_usage +
                            offset(log(time_flu_secondary*1000)),
                          data = df_input, family = poisson)
    flu_severe_ses_output <- tidy(flu_severe_ses, conf.int = TRUE)
  
  }
  
  # #flu mortality by ses
  # flu_mortality_ses <- glm(flu_mortality_inf ~ imd_quintile + 
  #                            age_band + sex + #rurality_classification +
  #                            has_asthma + has_copd + has_cystic_fibrosis +
  #                            has_other_resp + has_diabetes + has_addisons +
  #                            severe_obesity + has_chd + has_ckd + has_cld +
  #                            has_cnd + has_cancer + immunosuppressed +
  #                            has_sickle_cell + smoking_status +
  #                            hazardous_drinking + drug_usage +
  #                            offset(log(time_flu_mortality*1000)),
  #                          data = df_input, family = poisson)
  # flu_mortality_ses_output <- tidy(flu_mortality_ses, conf.int = TRUE)
  
} else {
  
  if (too_few_events_mild) {
 
    #create data frame with same columns as model output creates
    flu_mild_ses_output <- data.frame(term = "too few events", estimate = NA,
                                      std.error = NA, statistic = NA,
                                      p.value = NA, conf.low = NA,
                                      conf.high = NA)
  
  } else {
  
    #flu primary by ses
    flu_mild_ses <- glm(flu_primary_inf ~ imd_quintile + 
                          age_band + sex + #rurality_classification + 
                          offset(log(time_flu_primary*1000)),
                        data = df_input, family = poisson)
    flu_mild_ses_output <- tidy(flu_mild_ses, conf.int = TRUE)
  
  }
  
  if (too_few_events_severe) {
 
    #create data frame with same columns as model output creates
    flu_severe_ses_output <- data.frame(term = "too few events", estimate = NA,
                                        std.error = NA, statistic = NA,
                                        p.value = NA, conf.low = NA,
                                        conf.high = NA)
  
  } else {
  
    #flu secondary by ses
    flu_severe_ses <- glm(flu_secondary_inf ~ imd_quintile + 
                            age_band + sex + #rurality_classification + 
                            offset(log(time_flu_secondary*1000)),
                          data = df_input, family = poisson)
    flu_severe_ses_output <- tidy(flu_severe_ses, conf.int = TRUE)
  
  }
  
  # #flu mortality by ses
  # flu_mortality_ses <- glm(flu_mortality_inf ~ imd_quintile + 
  #                            age_band + sex + #rurality_classification + 
  #                            offset(log(time_flu_mortality*1000)),
  #                          data = df_input, family = poisson)
  # flu_mortality_ses_output <- tidy(flu_mortality_ses, conf.int = TRUE)

}

#define a vector of names for the model outputs
model_names <- c("Mild Influenza by IMD Quintile",
                 "Severe Influenza by IMD Quintile")#,
                 # "Influenza Mortality by IMD Quintile")


#create the model outputs list
model_outputs_list <- list(flu_mild_ses_output,
                           flu_severe_ses_output)#, 
                           # flu_mortality_ses_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          paste0("flu_", investigation_type)))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            paste0("flu_", investigation_type)), "/", 
                            "flu_ses_model_outputs_", cohort, "_",
                            year(study_start_date), "_", year(study_end_date),
                            "_", codelist_type, "_", investigation_type, ".csv"))
  
} else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("flu_", investigation_type)), "/", 
                            "flu_ses_model_outputs_", cohort, "_",
                            year(study_start_date), "_", year(study_end_date),
                            "_", codelist_type, "_", investigation_type, ".csv"))
  
}
