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

#remove rows with missing values in any of the variables used in models
#outcome will never be NA (as part of processing pipeline) so does not need to be filtered
if (cohort == "infants_subgroup") {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(imd_quintile),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification),
           !is.na(maternal_age), !is.na(maternal_smoking_status),
           !is.na(maternal_drinking), !is.na(maternal_drug_usage),
           !is.na(maternal_flu_vaccination), !is.na(maternal_pertussis_vaccination))
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {
 
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(imd_quintile),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification),
           !is.na(has_asthma), !is.na(has_copd), !is.na(has_cystic_fibrosis),
           !is.na(has_other_resp), !is.na(has_diabetes), !is.na(has_addisons),
           !is.na(severe_obesity), !is.na(has_chd), !is.na(has_ckd),
           !is.na(has_cld), !is.na(has_cnd), !is.na(has_cancer),
           !is.na(immunosuppressed), !is.na(has_sickle_cell),
           !is.na(smoking_status), !is.na(hazardous_drinking),
           !is.na(drug_usage))
  
} else {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(imd_quintile),
           !is.na(age_band), !is.na(sex), !is.na(rurality_classification))
  
}

#check there are enough outcomes to model
too_few_events_mild = if_else(sum(df_input$rsv_primary_inf, na.rm = TRUE) < 20,
                              TRUE, FALSE)
too_few_events_severe = if_else(sum(df_input$rsv_secondary_inf, na.rm = TRUE) < 20,
                                TRUE, FALSE)

if (cohort == "infants_subgroup") {
  
  if (too_few_events_mild) {

    #create data frame with same columns as model output creates
    rsv_mild_ethnicity_ses_output <- data.frame(term = "too few events",
                                                estimate = NA, std.error = NA,
                                                statistic = NA, p.value = NA,
                                                conf.low = NA, conf.high = NA)
 
  } else {
  
    #rsv primary by ethnicity and socioeconomic status
    rsv_mild_ethnicity_ses <- glm(rsv_primary_inf ~ latest_ethnicity_group + 
                                    imd_quintile + age_band + sex + 
                                    rurality_classification + 
                                    maternal_age + maternal_smoking_status +
                                    maternal_drinking + maternal_drug_usage + 
                                    maternal_flu_vaccination + 
                                    maternal_pertussis_vaccination +
                                    offset(log(time_rsv_primary)), 
                                  data = df_input, family = poisson)
    rsv_mild_ethnicity_ses_output <- tidy(rsv_mild_ethnicity_ses, confint = TRUE)
  
  }
  
  if (too_few_events_severe) {

    #create data frame with same columns as model output creates
    rsv_severe_ethnicity_ses_output <- data.frame(term = "too few events",
                                                  estimate = NA, std.error = NA,
                                                  statistic = NA, p.value = NA,
                                                  conf.low = NA, conf.high = NA)

  } else {

    #rsv secondary by ethnicity and socioeconomic status
    rsv_severe_ethnicity_ses <- glm(rsv_secondary_inf ~ latest_ethnicity_group +
                                      imd_quintile + age_band + sex + 
                                      rurality_classification + 
                                      maternal_age + maternal_smoking_status +
                                      maternal_drinking + maternal_drug_usage + 
                                      maternal_flu_vaccination + 
                                      maternal_pertussis_vaccination +
                                      offset(log(time_rsv_secondary)),
                                    data = df_input, family = poisson)
    rsv_severe_ethnicity_ses_output <- tidy(rsv_severe_ethnicity_ses, confint = TRUE)
  
  }
  
  # #rsv mortality by ethnicity and socioeconomic status
  # rsv_mortality_ethnicity_ses <- glm(rsv_mortality_inf ~ latest_ethnicity_group + 
  #                                      imd_quintile + age_band + sex + 
  #                                      rurality_classification + 
  #                                      maternal_age + maternal_smoking_status +
  #                                      maternal_drinking + maternal_drug_usage + 
  #                                      maternal_flu_vaccination + 
  #                                      maternal_pertussis_vaccination +
  #                                      offset(log(time_rsv_mortality)),
  #                                    data = df_input, family = poisson)
  # rsv_mortality_ethnicity_ses_output <- tidy(rsv_mortality_ethnicity_ses, confint = TRUE)
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {
  
  if (too_few_events_mild) {
 
    #create data frame with same columns as model output creates
    rsv_mild_ethnicity_ses_output <- data.frame(term = "too few events",
                                                estimate = NA, std.error = NA,
                                                statistic = NA, p.value = NA,
                                                conf.low = NA, conf.high = NA)
 
  } else {
  
    #rsv primary by ethnicity and socioeconomic status
    rsv_mild_ethnicity_ses <- glm(rsv_primary_inf ~ latest_ethnicity_group +
                                    imd_quintile + age_band + sex + 
                                    rurality_classification + has_asthma +
                                    has_copd + has_cystic_fibrosis +
                                    has_other_resp + has_diabetes +
                                    has_addisons + severe_obesity +
                                    has_chd + has_ckd + has_cld + has_cnd +
                                    has_cancer + immunosuppressed +
                                    has_sickle_cell + smoking_status +
                                    hazardous_drinking + drug_usage + 
                                    offset(log(time_rsv_primary*1000)), 
                                  data = df_input, family = poisson)
    rsv_mild_ethnicity_ses_output <- tidy(rsv_mild_ethnicity_ses, confint = TRUE)
  
  }
  
  if (too_few_events_severe) {
 
    #create data frame with same columns as model output creates
    rsv_severe_ethnicity_ses_output <- data.frame(term = "too few events",
                                                  estimate = NA, std.error = NA,
                                                  statistic = NA, p.value = NA,
                                                  conf.low = NA, conf.high = NA)

  } else {
  
    #rsv secondary by ethnicity and socioeconomic status
    rsv_severe_ethnicity_ses <- glm(rsv_secondary_inf ~ latest_ethnicity_group +
                                      imd_quintile + age_band + sex + 
                                      rurality_classification + has_asthma +
                                      has_copd + has_cystic_fibrosis +
                                      has_other_resp + has_diabetes +
                                      has_addisons + severe_obesity +
                                      has_chd + has_ckd + has_cld + has_cnd +
                                      has_cancer + immunosuppressed +
                                      has_sickle_cell + smoking_status +
                                      hazardous_drinking + drug_usage +
                                      offset(log(time_rsv_secondary*1000)),
                                    data = df_input, family = poisson)
    rsv_severe_ethnicity_ses_output <- tidy(rsv_severe_ethnicity_ses, confint = TRUE)
  
  }
  
  # #rsv mortality by ethnicity and socioeconomic status
  # rsv_mortality_ethnicity_ses <- glm(rsv_mortality_inf ~ latest_ethnicity_group + 
  #                                      imd_quintile + age_band + sex + 
  #                                      rurality_classification + has_asthma +
  #                                      has_copd + has_cystic_fibrosis +
  #                                      has_other_resp + has_diabetes +
  #                                      has_addisons + severe_obesity +
  #                                      has_chd + has_ckd + has_cld + has_cnd +
  #                                      has_cancer + immunosuppressed +
  #                                      has_sickle_cell + smoking_status +
  #                                      hazardous_drinking + drug_usage +
  #                                      offset(log(time_rsv_mortality*1000)),
  #                                    data = df_input, family = poisson)
  # rsv_mortality_ethnicity_ses_output <- tidy(rsv_mortality_ethnicity_ses, confint = TRUE)
  
} else {
  
  if (too_few_events_mild) {
 
    #create data frame with same columns as model output creates
    rsv_mild_ethnicity_ses_output <- data.frame(term = "too few events",
                                                estimate = NA, std.error = NA,
                                                statistic = NA, p.value = NA,
                                                conf.low = NA, conf.high = NA)
 
  } else {
  
    #rsv primary by ethnicity and socioeconomic status
    rsv_mild_ethnicity_ses <- glm(rsv_primary_inf ~ latest_ethnicity_group +
                                    imd_quintile + age_band + sex + 
                                  rurality_classification + 
                                    offset(log(time_rsv_primary*1000)), 
                                  data = df_input, family = poisson)
    rsv_mild_ethnicity_ses_output <- tidy(rsv_mild_ethnicity_ses, confint = TRUE)
  
  }
  
  if (too_few_events_severe) {
 
    #create data frame with same columns as model output creates
    rsv_severe_ethnicity_ses_output <- data.frame(term = "too few events",
                                                  estimate = NA, std.error = NA,
                                                  statistic = NA, p.value = NA,
                                                  conf.low = NA, conf.high = NA)
 
  } else {
  
    #rsv secondary by ethnicity and socioeconomic status
    rsv_severe_ethnicity_ses <- glm(rsv_secondary_inf ~ latest_ethnicity_group +
                                      imd_quintile + age_band + sex + 
                                      rurality_classification + 
                                      offset(log(time_rsv_secondary*1000)),
                                    data = df_input, family = poisson)
    rsv_severe_ethnicity_ses_output <- tidy(rsv_severe_ethnicity_ses, confint = TRUE)
  
  }
  
  # #rsv mortality by ethnicity and socioeconomic status
  # rsv_mortality_ethnicity_ses <- glm(rsv_mortality_inf ~ latest_ethnicity_group + 
  #                                      imd_quintile + age_band + sex + 
  #                                      rurality_classification + 
  #                                      offset(log(time_rsv_mortality*1000)),
  #                                    data = df_input, family = poisson)
  # rsv_mortality_ethnicity_ses_output <- tidy(rsv_mortality_ethnicity_ses, confint = TRUE)

}

#define a vector of names for the model outputs
model_names <- c("Mild RSV by Ethnicity and IMD Quintile", 
                 "Severe RSV by Ethnicity and IMD Quintile")#,
                 # "RSV Mortality By Ethnicity and IMD Quintile")

#create the model outputs list
model_outputs_list <- list(rsv_mild_ethnicity_ses_output, 
                           rsv_severe_ethnicity_ses_output)#,
                           # rsv_mortality_ethnicity_ses_output)

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
                            "rsv_ethnicity_ses_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
  
} else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            paste0("rsv_", investigation_type)), "/", 
                            "rsv_ethnicity_ses_model_outputs_", cohort, "_", 
                            year(study_start_date), "_", year(study_end_date), 
                            "_", codelist_type, "_", investigation_type, ".csv"))
  
}
