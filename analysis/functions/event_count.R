library(dplyr)

#define function to calculate events per group
group_specific_events <- function(df, add_characteristics,
                                  outcome_mild, outcome_severe) {
  
  #define additional characteristics 
  if (cohort == "infants_subgroup") {
    
    additional_characteristics <- c("maternal_age", "maternal_smoking_status",
                                    "maternal_drinking", "maternal_drug_usage",
                                    "maternal_flu_vaccination",
                                    "maternal_pertussis_vaccination")
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
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
  characteristics <- c("age_band", "sex", "rurality_classification")
  
  #add characteristics which are model specific
  characteristics <- c(characteristics, add_characteristics,
                       additional_characteristics)
  
  outcome_mild <- ensym(outcome_mild)
  outcome_severe <- ensym(outcome_severe)
  
  results <- data.frame()
  
  for (i in seq_along(characteristics)) {
    
    #calculate events per group
    events_per_group <- df %>%
      group_by(group = !!sym(characteristics[i])) %>%
      summarise(events_mild = sum(!!outcome_mild, na.rm = TRUE),
                events_severe = sum(!!outcome_severe, na.rm = TRUE))
    
    enough_events_mild <- if_else(sum(events_per_group$events_mild > 0) > 1,
                                  TRUE, FALSE)
    enough_events_severe <- if_else(sum(events_per_group$events_severe > 0) > 1,
                                    TRUE, FALSE)
    
    #add column with whether there are enough events
    events_per_group <- events_per_group %>%
      mutate(enough_events_mild = enough_events_mild,
             enough_events_severe = enough_events_severe)
    
    # Store results for this characteristic
    results <- rbind(results, events_per_group) 
    
  }
  
  return(results)
  
}
