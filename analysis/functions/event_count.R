library(dplyr)
library(here)
library(rlang)

## create output directories ----
fs::dir_create(here::here("analysis", "functions"))

#define function to calculate events per group
group_specific_events <- function(df, add_characteristics,
                                  outcome_mild, outcome_severe) {
  
  #define additional characteristics 
  if (cohort == "older_adults" & investigation_type == "secondary") {
    
    additional_characteristics <- c("has_asthma", "has_copd",
                                    "has_cystic_fibrosis", "has_other_resp",
                                    "has_diabetes", "has_addisons",
                                    "severe_obesity", "has_chd", "has_ckd",
                                    "has_cld", "has_cnd", "has_cancer",
                                    "immunosuppressed", "has_sickle_cell",
                                    "smoking_status", "hazardous_drinking",
                                    "drug_usage")
    
  } else {
    
    additional_characteristics <- character(0)
    
  }
  
  #define the characteristics that are always included
  characteristics <- c("age_band", "sex")
  
  #add characteristics which are model specific
  characteristics <- c(characteristics, add_characteristics,
                       additional_characteristics)
  
  #define outcomes for summarising
  outcome_mild <- ensym(outcome_mild)
  outcome_severe <- ensym(outcome_severe)
  
  #create empty dataframe to store results
  results <- data.frame()
  
  for (i in seq_along(characteristics)) {
    
    #calculate events per group
    events_per_group <- df %>%
      group_by(group = !!sym(characteristics[i])) %>%
      summarise(events_mild = sum(!!outcome_mild, na.rm = TRUE),
                events_severe = sum(!!outcome_severe, na.rm = TRUE))
    
    #add column with whether there are enough events
    events_per_group <- events_per_group %>%
      mutate(
        enough_events_mild = all(events_per_group$events_mild > 0),
        enough_events_severe = all(events_per_group$events_severe > 0)
      )
    
    #store results for this characteristic
    events_per_group <- cbind(characteristic = characteristics[i],
                              events_per_group)
    results <- rbind(results, events_per_group) 
    
  }
  
  return(as_tibble(results))
  
}

#define function to calculate events per group for models which include vaccination
group_specific_events_further <- function(df, add_characteristics,
                                          outcome_mild, outcome_severe,
                                          vaccination_prior = NULL,
                                          vaccination_current = NULL) {
  
  #define the characteristics that are always included
  characteristics <- c("age_band", "sex", "rurality_classification")
  
  #define additional characteristics 
  if (cohort == "infants_subgroup") {
    
    additional_characteristics <- c("maternal_smoking_status",
                                    "maternal_drinking",
                                    "maternal_drug_usage",
                                    "maternal_flu_vaccination",
                                    "maternal_pertussis_vaccination")
    
  } else {
    
    additional_characteristics <- character(0)
    
  }
  
  #add characteristics which are model specific
  characteristics <- c(characteristics, add_characteristics,
                       additional_characteristics)
  
  if (cohort != "infants" & cohort != "infants_subgroup") {
    
    #define vaccination categories for summaries
    current_mild <- paste0(vaccination_current, "_mild")
    current_severe <- paste0(vaccination_current, "_severe")
    
    #add vaccination characteristics
    characteristics <- c(characteristics, vaccination_prior, current_mild,
                         current_severe)
    
  } else {
    
    current_mild <- "none"
    current_severe <- "none"
    
  }
  
  #define outcomes for summarising
  outcome_mild <- ensym(outcome_mild)
  outcome_severe <- ensym(outcome_severe)
  
  #create empty dataframe to store results
  results <- data.frame()
  
  for (i in seq_along(characteristics)) {
    
    if (!(characteristics[i] %in% names(df))) {
      
      next
      
    } else if (characteristics[i] == current_mild) {
      
      #calculate events per group
      events_per_group <- df %>%
        group_by(group = !!sym(current_mild)) %>%
        summarise(events_mild = sum(!!outcome_mild, na.rm = TRUE))
      
      #add column with whether there are enough events
      events_per_group <- events_per_group %>%
        mutate(enough_events_mild = all(events_per_group$events_mild > 0))
      
      #store results for this characteristic
      events_per_group <- cbind(characteristic = "current vaccination",
                                events_per_group)
      results_mild <- events_per_group
     
    } else if (characteristics[i] == current_severe) {  
     
      #calculate events per group
      events_per_group <- df %>%
        group_by(group = !!sym(current_severe)) %>%
        summarise(events_severe = sum(!!outcome_severe, na.rm = TRUE))
      
      #add column with whether there are enough events
      events_per_group <- events_per_group %>%
        mutate(enough_events_mild = all(events_per_group$events_mild > 0))
      
      #store results for this characteristic
      events_per_group <- cbind(characteristic = "current vaccination",
                                events_per_group)
      results_severe <- events_per_group
      
    } else {
    
      #calculate events per group
      events_per_group <- df %>%
        group_by(group = !!sym(characteristics[i])) %>%
        summarise(events_mild = sum(!!outcome_mild, na.rm = TRUE),
                  events_severe = sum(!!outcome_severe, na.rm = TRUE))
      
      #add column with whether there are enough events
      events_per_group <- events_per_group %>%
        mutate(        
          enough_events_mild = all(events_per_group$events_mild > 0),
          enough_events_severe = all(events_per_group$events_severe > 0)
          )
      
      #store results for this characteristic
      events_per_group <- cbind(characteristic = characteristics[i],
                                events_per_group)
      results <- rbind(results, events_per_group) 
      
    }
    
  }
  
  if (current_mild %in% names(df) & current_severe %in% names(df)) {
    
    #format vaccination groups
    current_vacc <- merge(results_mild, results_severe,
                          by = c("characteristic", "group"))
    
    #combine with results
    results <- rbind(results, current_vacc)
    
  }
  
  return(as_tibble(results))
  
}
