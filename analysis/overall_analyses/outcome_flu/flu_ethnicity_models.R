library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis", "overall_analyses", "outcome_flu"))

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
    filter(!is.na(latest_ethnicity_group), !is.na(age_band), !is.na(sex),
           !is.na(rurality_classification), !is.na(maternal_age),
           !is.na(maternal_smoking_status), !is.na(maternal_drinking),
           !is.na(maternal_drug_usage), !is.na(maternal_flu_vaccination),
           !is.na(maternal_pertussis_vaccination))
  
} else if (cohort == "older_adults" & investigation_type == "secondary") {
  
  df_input <- df_input %>% 
    filter(!is.na(latest_ethnicity_group), !is.na(age_band), !is.na(sex),
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
    filter(!is.na(latest_ethnicity_group), !is.na(age_band),
           !is.na(sex), !is.na(rurality_classification))
  
}

#import event counting function
source(here::here("analysis", "functions", "event_count.R"))

#calculate events per group
events <- group_specific_events(df_input, c("latest_ethnicity_group"),
                                "flu_primary_inf", "flu_secondary_inf")

#check if there are too few events
too_few_events_mild <- any(events$enough_events_mild == FALSE)
too_few_events_severe <- any(events$enough_events_severe == FALSE)

if (cohort == "infants_subgroup") {
  
  if (too_few_events_mild) {
  
    #create data frame with the same columns as model outputs
    flu_mild_ethnicity_output <- data.frame(term = "too few events",
                                            estimate = NA, std.error = NA,
                                            statistic = NA, p.value = NA,
                                            conf.low = NA, conf.high = NA)
  
  } else {

    #flu primary by ethnicity
    flu_mild_ethnicity <- glm(flu_primary_inf ~ latest_ethnicity_group + 
                                age_band + sex + rurality_classification + 
                                maternal_age + maternal_smoking_status +
                                maternal_drinking + maternal_drug_usage + 
                                maternal_flu_vaccination + 
                                maternal_pertussis_vaccination +
                                offset(log(time_flu_primary*1000)),
                              data = df_input, family = poisson)
    flu_mild_ethnicity_output <- tidy(flu_mild_ethnicity, conf.int = TRUE)
  
  }
  
  if (too_few_events_severe) {
  
    #create data frame with the same columns as model outputs
    flu_severe_ethnicity_outputs <- data.frame(term = "too few events",
                                              estimate = NA, std.error = NA,
                                              statistic = NA, p.value = NA,
                                              conf.low = NA, conf.high = NA)
  
  } else {
  
    #flu secondary by ethnicity
    flu_severe_ethnicity <- glm(flu_secondary_inf ~ latest_ethnicity_group + 
                                  age_band + sex + rurality_classification +
                                  maternal_age + maternal_smoking_status +
                                  maternal_drinking + maternal_drug_usage + 
                                  maternal_flu_vaccination + 
                                  maternal_pertussis_vaccination +
                                  offset(log(time_flu_secondary*1000)),
                                data = df_input, family = poisson)
    flu_severe_ethnicity_output <- tidy(flu_severe_ethnicity, conf.int = TRUE)
  
  }
  
  # #flu mortality by ethnicity
  # flu_mortality_ethnicity <- glm(flu_mortality_inf ~ latest_ethnicity_group + 
  #                                  age_band + sex + rurality_classification + 
  #                                  maternal_age + maternal_smoking_status +
  #                                  maternal_drinking + maternal_drug_usage + 
  #                                  maternal_flu_vaccination + 
  #                                  maternal_pertussis_vaccination +
  #                                  offset(log(time_flu_mortality*1000)),
  #                                data = df_input, family = poisson)
  # flu_mortality_ethnicity_output <- tidy(flu_mortality_ethnicity, conf.int = TRUE)
  
} else {

  if (too_few_events_mild) {
  
    #create data frame with the same columns as model outputs
    flu_mild_ethnicity_output <- data.frame(term = "too few events",
                                            estimate = NA, std.error = NA,
                                            statistic = NA, p.value = NA,
                                            conf.low = NA, conf.high = NA)
  
  } else {
  
    #flu primary by ethnicity
    flu_mild_ethnicity <- glm(flu_primary_inf ~ latest_ethnicity_group + 
                                age_band + sex + rurality_classification + 
                                offset(log(time_flu_primary*1000)),
                              data = df_input, family = poisson)
    flu_mild_ethnicity_output <- tidy(flu_mild_ethnicity, conf.int = TRUE)
  
  }
  
  if (too_few_events_severe) {
  
    #create data frame with the same columns as model outputs
    flu_severe_ethnicity_output <- data.frame(term = "too few events",
                                              estimate = NA, std.error = NA,
                                              statistic = NA, p.value = NA,
                                              conf.low = NA, conf.high = NA)
  
  } else {
  
    #flu secondary by ethnicity
    flu_severe_ethnicity <- glm(flu_secondary_inf ~ latest_ethnicity_group + 
                                  age_band + sex + rurality_classification + 
                                  offset(log(time_flu_secondary*1000)),
                                data = df_input, family = poisson)
    flu_severe_ethnicity_output <- tidy(flu_severe_ethnicity, conf.int = TRUE)
  
  }
  
  # #flu mortality by ethnicity
  # flu_mortality_ethnicity <- glm(flu_mortality_inf ~ latest_ethnicity_group + 
  #                                  age_band + sex + rurality_classification + 
  #                                  offset(log(time_flu_mortality*1000)),
  #                                data = df_input, family = poisson)
  # flu_mortality_ethnicity_output <- tidy(flu_mortality_ethnicity, conf.int = TRUE)
  
}

#define a vector of names for the model outputs
model_names <- c("Mild Influenza by Ethnicity",
                 "Severe Influenza by Ethnicity")#, 
                 # "Influenza Mortality by Ethnicity")


#create the model outputs list
model_outputs_list <- list(flu_mild_ethnicity_output,
                           flu_severe_ethnicity_output)#, 
                           # flu_mortality_ethnicity_output)

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here::here("output", "results", "models",
                          "flu_overall"))

#save model output 
if (length(args) == 0) {
  
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models",
                            "flu_overall"), "/", "flu_ethnicity_model_outputs_",
                            cohort, "_specific_primary.csv"))
  
}  else {
  
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models",
                            "flu_overall"), "/", "flu_ethnicity_model_outputs_",
                            cohort, "_specific_primary.csv"))
  
}
