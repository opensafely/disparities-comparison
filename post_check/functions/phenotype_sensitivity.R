library(tidyverse)
library(ggplot2)
library(scales)
library(stringr)
library(ggeasy)
library(forcats)

#define function to create a stacked bar chart of multiple infection outcomes
bar_plot <- function(input, seasons) {
  
  filtered_input_all <- input %>%
    filter(subset %in% seasons) %>%
    mutate(n = `n (midpoint 10 rounded)`)
  
  # Helper function to safely extract count or return 0 if not found
  safe_pull <- function(df, codelist, combo_spec) {
    
    count <- df %>% 
      filter(codelist_type == codelist) %>%
      filter(combo == !!combo_spec) %>% select(n) %>% pull()
    if (length(count) == 0) return(0) else return(as.numeric(count))
    if (is.na(count)) return(0) else return(as.numeric(count))
    
  }
  
  # Define all possible combinations
  full_combos <- c("RSV", "Flu", "COVID", "RSV & Flu", "RSV & COVID",
                   "Flu & COVID", "RSV & Flu & COVID")
  
  # Define all codelist types
  codelist_types <- c("Specific", "Sensitive")
  
  # Initialize empty list to collect results
  plot_list <- list()
  
  # Loop over each season and outcome
  for (season in seasons) {
    
    # Filter input data by season
    filtered_input <- filtered_input_all %>% filter(subset == season)
    
    if (season %in% c("2019_20", "2020_21", "2021_22", "2022_23", 
                      "2023_24")) {
      
      rsv_spec <- safe_pull(
        filtered_input, "specific", "RSV_Mild_0_0")
      flu_spec <- safe_pull(
        filtered_input, "specific", "0_Flu_Mild_0")
      covid_spec <- safe_pull(
        filtered_input, "specific", "0_0_COVID_Mild")
      rsv_flu_spec <- safe_pull(
        filtered_input, "specific", "RSV_Mild_Flu_Mild_0")
      rsv_covid_spec <- safe_pull(
        filtered_input, "specific", "RSV_Mild_0_COVID_Mild")
      flu_covid_spec <- safe_pull(
        filtered_input, "specific", "0_Flu_Mild_COVID_Mild")
      rsv_flu_covid_spec <- safe_pull(
        filtered_input, "specific", "RSV_Mild_Flu_Mild_COVID_Mild")
      
      rsv_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_0_0")
      flu_sens <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Mild_0")
      covid_sens <- safe_pull(
        filtered_input, "sensitive", "0_0_COVID_Mild")
      rsv_flu_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_Flu_Mild_0")
      rsv_covid_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_0_COVID_Mild")
      flu_covid_sens <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Mild_COVID_Mild")
      rsv_flu_covid_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_Flu_Mild_COVID_Mild")
      
      df_plot_mild <- tibble(
        combo = factor(rep(full_combos, times = 2), levels = full_combos),
        n = c(rsv_spec, flu_spec, covid_spec, rsv_flu_spec, rsv_covid_spec,
              flu_covid_spec, rsv_flu_covid_spec, rsv_sens, flu_sens,
              covid_sens, rsv_flu_sens, rsv_covid_sens, flu_covid_sens,
              rsv_flu_covid_sens),
        codelist_type = fct_rev(
          factor(rep(c("Specific", "Sensitive"), each = 7),
                 levels = codelist_types, ordered = TRUE)),
        outcome_type = "Mild",
        subset = season
      )
      
      rsv_spec <- safe_pull(
        filtered_input, "specific", "RSV_Severe_0_0")
      flu_spec <- safe_pull(
        filtered_input, "specific", "0_Flu_Severe_0")
      covid_spec <- safe_pull(
        filtered_input, "specific", "0_0_COVID_Severe")
      rsv_flu_spec <- safe_pull(
        filtered_input, "specific", "RSV_Severe_Flu_Severe_0")
      rsv_covid_spec <- safe_pull(
        filtered_input, "specific", "RSV_Severe_0_COVID_Severe")
      flu_covid_spec <- safe_pull(
        filtered_input, "specific", "0_Flu_Severe_COVID_Severe")
      rsv_flu_covid_spec <- safe_pull(
        filtered_input, "specific", "RSV_Severe_Flu_Severe_COVID_Severe")
      
      rsv_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_0_0")
      flu_sens <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Severe_0")
      covid_sens <- safe_pull(
        filtered_input, "sensitive", "0_0_COVID_Severe")
      rsv_flu_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_Flu_Severe_0")
      rsv_covid_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_0_COVID_Severe")
      flu_covid_sens <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Severe_COVID_Severe")
      rsv_flu_covid_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_Flu_Severe_COVID_Severe")
      
      df_plot_severe <- tibble(
        combo = factor(rep(full_combos, times = 2), levels = full_combos),
        n = c(rsv_spec, flu_spec, covid_spec, rsv_flu_spec, rsv_covid_spec,
              flu_covid_spec, rsv_flu_covid_spec, rsv_sens, flu_sens,
              covid_sens, rsv_flu_sens, rsv_covid_sens, flu_covid_sens,
              rsv_flu_covid_sens),
        codelist_type = fct_rev(
          factor(rep(c("Specific", "Sensitive"), each = 7),
                 levels = codelist_types, ordered = TRUE)),
        outcome_type = "Severe",
        subset = season
      )
      
    } else {
      
      rsv_spec <- safe_pull(
        filtered_input, "specific", "RSV_Mild_0")
      flu_spec <- safe_pull(
        filtered_input, "specific", "0_Flu_Mild")
      rsv_flu_spec <- safe_pull(
        filtered_input, "specific", "RSV_Mild_Flu_Mild")
      
      rsv_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_0")
      flu_sens <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Mild")
      rsv_flu_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_Flu_Mild")
      
      df_plot_mild <- tibble(
        combo = factor(rep(full_combos, times = 2), levels = full_combos),
        n = c(rsv_spec, flu_spec, 0, rsv_flu_spec, 0, 0, 0,
              rsv_sens, flu_sens, 0, rsv_flu_sens, 0, 0, 0),
        codelist_type = fct_rev(
          factor(rep(c("Specific", "Sensitive"), each = 7),
                 levels = codelist_types, ordered = TRUE)),
        outcome_type = "Mild",
        subset = season
      )
      
      rsv_spec <- safe_pull(
        filtered_input, "specific", "RSV_Severe_0")
      flu_spec <- safe_pull(
        filtered_input, "specific", "0_Flu_Severe")
      rsv_flu_spec <- safe_pull(
        filtered_input, "specific", "RSV_Severe_Flu_Severe")
      
      rsv_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_0")
      flu_sens <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Severe")
      rsv_flu_sens <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_Flu_Severe")
      
      df_plot_severe <- tibble(
        combo = factor(rep(full_combos, times = 2), levels = full_combos),
        n = c(rsv_spec, flu_spec, 0, rsv_flu_spec, 0, 0, 0,
              rsv_sens, flu_sens, 0, rsv_flu_sens, 0, 0, 0),
        codelist_type = fct_rev(
          factor(rep(c("Specific", "Sensitive"), each = 7),
                 levels = codelist_types, ordered = TRUE)),
        outcome_type = "Severe",
        subset = season
      )
      
    }
    
    df_plot <- rbind(df_plot_mild, df_plot_severe)
    
    # Add to list
    plot_list[[season]] <- df_plot
    
  }
  
  # Combine all
  final_df <- bind_rows(plot_list)
  
  df_plot <- final_df %>%
    mutate(
      n = if_else(is.na(n), 0, n),
           codelist_type = factor(
             codelist_type, levels = c("Specific", "Sensitive")),
      subset = gsub("_", "-", subset),
    )
  
  #stacked bar plot
  diag <- ggplot(df_plot, aes(x = combo, y = n, fill = codelist_type)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_continuous(labels = scales::label_comma()) +
    theme_bw() + scale_fill_manual(
      name = "Phenotype", values = c("#1E88E5", "#D81B60")) +
    facet_grid(subset ~ outcome_type, scales = "free") +
    theme(legend.position = "bottom", plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.background = element_rect(fill = "white", colour = "white")) +
      labs(x = "Outcomes Within One Episode", y = "Cases Identified", 
           title = paste0("Outcomes Identified by Phenotype in ",
                          str_to_title(gsub("_", " ", cohort))))
  
  return(diag)
  
}
