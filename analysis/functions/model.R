library(here)
library(broom)
library(rlang)
library(purrr)

## create output directories ----
fs::dir_create(here::here("analysis", "functions"))

#create function for poisson regression
glm_poisson <- function(df, x, y, offset_var) {
  
  #reformat variables
  x <- syms(x)
  x_combined <- paste0(map_chr(x, as_string), collapse = " + ")
  
  #dynamically construct the formula
  formula <- as.formula(paste0(y, " ~ ", x_combined,
                               " + age_band + sex + offset(log(",
                               offset_var, " * 1000))"))
  
  
  #create model
  if (cohort == "older_adults" & investigation_type == "secondary") {
    
    #update the formula with relevant additional variables
    formula <- update(formula, . ~ . + has_asthma + has_copd +
                        has_cystic_fibrosis + has_other_resp + has_diabetes +
                        has_addisons + severe_obesity + has_chd + has_ckd +
                        has_cld + has_cnd + has_cancer + immunosuppressed + 
                        has_sickle_cell + smoking_status + hazardous_drinking +
                        drug_usage)
    
    #run model
    model <- glm(formula, data = df, family = poisson)
    
  } else {
    
    #run model
    model <- glm(formula, data = df, family = poisson)
    
  }
  
  #tidy
  tidy_model <- tidy(model, conf.int = TRUE)
  
  #return output
  return(tidy_model)
  
}

#create function for poisson regression with further adjustment
glm_poisson_further <- function(df, x, y, prior_vacc, vacc_mild,
                                vacc_severe, offset) {
  
  #define minimum dates for covid seasons
  covid_season_min <- as.Date("2019-09-01")
  covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
  covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")
  
  #reformat variables
  x <- syms(x)
  x_combined <- paste0(map_chr(x, as_string), collapse = " + ")
  
  #dynamically construct the formula
  formula <- as.formula(paste0(y, " ~ ", x_combined,
                               " + age_band + sex + rurality_classification",
                               " + offset(log(", offset_var, " * 1000))"))
  
  #create model
  if (cohort == "infants_subgroup") {
    
    #update the formula with relevant additional variables
    formula <- update(formula, . ~ . + maternal_age + maternal_smoking_status +
                        maternal_drinking + maternal_drug_usage +
                        maternal_flu_vaccination +
                        maternal_pertussis_vaccination)
    
    #run model
    model <- glm(formula, data = df, family = poisson)
    
  } else if (cohort == "infants") {
    
    #run model
    model <- glm(formula, data = df, family = poisson)
    
  } else {
    
    if (grepl("flu_primary", y)) {
      
      #update the formula with relevant additional variables
      formula <- update(formula, . ~ . + prior_vacc + vacc_mild)
      
      #run model
      model <- glm(formula, data = df, family = poisson)
      
    } else if (grep("flu_secondary", y)) {
      
      #update the formula with relevant additional variables
      formula <- update(formula, . ~ . + prior_vacc + vacc_severe)
      
      #run model
      model <- glm(formula, data = df, family = poisson)
      
    } else if (grepl("covid_primary", y)) {
      
      if (study_start_date < covid_current_vacc_min) {
        
        #run model
        model <- glm(formula, data = df, family = poisson)
        
      } else if (study_start_date == covid_current_vacc_min) {
        
        #update the formula with relevant additional variables
        formula <- update(formula, . ~ . + vacc_mild)
        
        #run model
        model <- glm(formula, data = df, family = poisson)
        
      } else {
        
        #update the formula with relevant additional variables
        formula <- update(formula, . ~ . + prior_vacc + vacc_mild)
        
        #run model
        model <- glm(formula, data = df, family = poisson)
        
      }
      
    } else if (grepl("covid_secondary", y)) {
      
      if (study_start_date < covid_current_vacc_min) {
        
        #run model
        model <- glm(formula, data = df, family = poisson)
        
      } else if (study_start_date == covid_current_vacc_min) {
        
        #update the formula with relevant additional variables
        formula <- update(formula, . ~ . + vacc_severe)
        
        #run model
        model <- glm(formula, data = df, family = poisson)
        
      } else {
        
        #update the formula with relevant additional variables
        formula <- update(formula, . ~ . + prior_vacc + vacc_severe)
        
        #run model
        model <- glm(formula, data = df, family = poisson)
        
      }
      
    }
    
  }
  
  #tidy
  tidy_model <- tidy(model, conf.int = TRUE)
  
  #return output
  return(tidy_model)
  
}
