library(here)
library(broom)
library(rlang)
library(purrr)

## create output directories ----
fs::dir_create(here::here("post_check", "functions"))

#create function for poisson regression
glm_poisson <- function(df, x, y, offset_var) {
  
  #filter out NA survival times
  df <- df %>%
    filter(!is.na(offset_var))
  
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
  
  #return output
  return(model)
  
}

#create function for poisson regression with further adjustment
glm_poisson_further <- function(df, x, y, prior_vacc, vacc_mild,
                                vacc_severe, offset_var) {
  
  #filter out NA survival times
  df <- df %>%
    filter(!is.na(offset_var))
  
  #source tmerge alt
  source(here::here("post_check", "functions", "expand_with_tmerge.R"))
  
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
      
      df <- expand_with_tmerge(df, "flu_primary")
      
      offset_var <- "persontime_years"
      
      predictors <- c(predictors, prior_vacc, "vax_status")
      
    } else if (y == "flu_secondary_inf") {
      
      df <- expand_with_tmerge(df, "flu_secondary")
      
      offset_var <- "persontime_years"
      
      predictors <- c(predictors, prior_vacc, "vax_status")
      
    } else if (y == "covid_primary_inf") {
      
      if (unique(df$subset) %in% c("2020_21", "2021_22", "2022_23",
                                   "2023_24")) {
        
        df <- expand_with_tmerge(df, "covid_primary")
        
        offset_var <- "persontime_years"
        
      }
      
      if (unique(df$subset) == "2020_21") {
        
        predictors <- c(predictors, "vax_status")
        
      } else if (unique(df$subset) %in% c("2021_22", "2022_23", "2023_24")) {
        
        predictors <- c(predictors, prior_vacc, "vax_status")
        
      }
      
    } else if (y == "covid_secondary_inf") {
      
      if (unique(df$subset) %in% c("2020_21", "2021_22", "2022_23",
                                   "2023_24")) {
        
        df <- expand_with_tmerge(df, "covid_secondary")
        
        offset_var <- "persontime_years"
        
      }
      
      if (unique(df$subset) == "2020_21") {
        
        predictors <- c(predictors, "vax_status")
        
      } else if (unique(df$subset) %in% c("2021_22", "2022_23", "2023_24")) {
        
        predictors <- c(predictors, prior_vacc, "vax_status")
        
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
  
  #return output
  return(model)
  
}
