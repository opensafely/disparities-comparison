library(here)
library(broom)
library(rlang)
library(purrr)

## create output directories ----
fs::dir_create(here::here("analysis", "functions"))

#create function for poisson regression
glm_poisson <- function(df, x, y, offset_var) {
  
  #define the base predictors
  predictors <- c(x, "age_band", "sex")
  
  #update predictors based on cohort and investigation type
  if (cohort == "older_adults" & investigation_type == "secondary") {
    
    additional_predictors <- c("has_asthma", "has_copd",
                               "has_cystic_fibrosis", "has_other_resp",
                               "has_diabetes", "has_addisons",
                               "severe_obesity", "has_chd", "has_ckd",
                               "has_cld", "has_cnd", "has_cancer", 
                               "immunosuppressed", "has_sickle_cell", 
                               "smoking_status", "hazardous_drinking",
                               "drug_usage")
    predictors <- c(predictors, additional_predictors)
    
  }
  
  #add offset to the formula
  offset_term <- paste0("offset(log(", offset_var, " * 1000))")
  
  #construct the formula as a string
  formula_string <- paste(
    y, "~", paste(c(predictors, offset_term), collapse = " + ")
  )
  
  #convert to a formula object
  formula <- as.formula(formula_string)
  
  #fit the model
  model <- glm(formula, data = df, family = poisson)
  
  #tidy model output
  tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  #return output
  return(tidy_model)
  
}

#create function for poisson regression with further adjustment
glm_poisson_further <- function(df, x, y, prior_vacc, vacc_mild,
                                vacc_severe, offset_var) {
  
  #define minimum dates for covid seasons
  covid_season_min <- as.Date("2019-09-01")
  covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
  covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")
  
  #combine predictors
  predictors <- c(x, "age_band", "sex", "rurality_classification")
  
  #update predictors based on the outcome, cohort, and study start date
  if (cohort == "infants_subgroup") {
    
    maternal <- c("maternal_age", "maternal_smoking_status",
                  "maternal_drinking", "maternal_drug_usage",
                  "maternal_flu_vaccination",
                  "maternal_pertussis_vaccination")
    predictors <- c(predictors, maternal)
    
  } else if (cohort == "infants") {
    
    predictors <- predictors
    
  } else {
    
    if (y == "flu_primary_inf") {
      
      predictors <- c(predictors, prior_vacc, vacc_mild)
      
    } else if (y == "flu_secondary_inf") {
      
      predictors <- c(predictors, prior_vacc, vacc_severe)
      
    } else if (y == "covid_primary_inf") {
      
      if (study_start_date >= as.Date("2020-09-01") &
          study_start_date < as.Date("2021-09-01")) {
        
        predictors <- c(predictors, vacc_mild)
        
      } else if (study_start_date >= as.Date("2021-09-01")) {
        
        predictors <- c(predictors, prior_vacc, vacc_mild)
        
      }
      
    } else if (y == "covid_secondary_inf") {
      
      if (study_start_date == as.Date("2020-09-01")) {
        
        predictors <- c(predictors, vacc_severe)
        
      } else if (study_start_date >= as.Date("2021-09-01")) {
        
        predictors <- c(predictors, prior_vacc, vacc_severe)
        
      }
      
    }
    
  }
  
  #add offset to the formula
  offset_term <- paste0("offset(log(", offset_var, " * 1000))")
  
  #construct the formula as a string
  formula_string <- paste(
    y, "~", paste(c(predictors, offset_term), collapse = " + ")
  )
  
  #convert to a formula object
  formula <- as.formula(formula_string)
  
  #fit the model
  model <- glm(formula, data = df, family = poisson)
  
  #tidy model output
  tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  #return output
  return(tidy_model)
  
}
