library(dplyr)

#define function to calculate events per group
group_specific_events <- function(df, add_characteristics,
                                  outcome_mild, outcome_severe) {
  
  characteristics <- c("age_band", "sex", "rurality_classification")
  characteristics <- c(characteristics, add_characteristics)
  
  outcome_mild <- ensym(outcome_mild)
  outcome_severe <- ensym(outcome_severe)
  
  results <- data.frame()
  
  for (i in seq_along(characteristics)) {
    
    #calculate events per group
    events_per_group <- df %>%
      group_by(group = !!sym(characteristics[i])) %>%
      summarise(events_mild = sum(!!outcome_mild, na.rm = TRUE),
                events_severe = sum(!!outcome_severe, na.rm = TRUE))
    
    #add column with whether there are enough events
    events_per_group <- events_per_group %>%
      mutate(enough_events_mild = if_else(events_mild > 0, TRUE, FALSE),
             enough_events_severe = if_else(events_severe > 0, TRUE, FALSE))
    
    # Store results for this characteristic
    results <- rbind(results, events_per_group) 
    
  }
  
  return(results)
  
}
