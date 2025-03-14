library(tidyverse)
library(here)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(stringr)
library(ggeasy)
library(forcats)

#define function to create a stacked bar chart of multiple infection outcomes
bar_stack <- function(input, season, outcome) {
  
  filtered_input <- input %>%
    filter(subset == season, outcome_type == outcome) %>%
    mutate(n = `n (midpoint 10 rounded)`)
  
  df_diff <- filtered_input %>%
    group_by(combo) %>%
    filter(codelist_type %in% c("sensitive", "specific")) %>%
    arrange(combo, codelist_type) %>%
    reframe(n_diff = n[codelist_type == "sensitive"] - n[codelist_type == "specific"]) %>%
    ungroup() 
  
  df_comb <- filtered_input %>%
    full_join(df_diff, by = "combo") %>%
    filter(codelist_type %in% c("sensitive", "specific")) %>%
    rowwise() %>%
    mutate(
      n = if_else(codelist_type == "sensitive", n_diff, n)
    ) %>%
    subset(select = -c(n_diff))
  
  # Helper function to safely extract count or return 0 if not found
  safe_pull <- function(df, codelist, combo_spec) {
    
    count <- df %>% 
      filter(codelist_type == codelist) %>%
      filter(combo == !!combo_spec) %>% select(n) %>% pull()
    if (length(count) == 0) return(0) else return(as.numeric(count))
    if (is.na(count)) return(0) else return(as.numeric(count))
    
  }
  
  #sets 
  if (season %in% c("2019_20", "2020_21", "2021_22", "2022_23", "2023_24")) {
    
    if (outcome == "mild") {
      
      rsv_total_spec <- safe_pull(df_comb, "specific", "RSV_Mild_Total")
      flu_total_spec <- safe_pull(df_comb, "specific", "Flu_Mild_Total")
      covid_total_spec <- safe_pull(df_comb, "specific", "COVID_Mild_Total")
      
      rsv_total_sens <- safe_pull(df_comb, "sensitive", "RSV_Mild_Total")
      flu_total_sens <- safe_pull(df_comb, "sensitive", "Flu_Mild_Total")
      covid_total_sens <- safe_pull(df_comb, "sensitive", "COVID_Mild_Total")
      
      rsv_spec <- safe_pull(df_comb, "specific", "RSV_Mild_0_0")
      flu_spec <- safe_pull(df_comb, "specific", "0_Flu_Mild_0")
      covid_spec <- safe_pull(df_comb, "specific", "0_0_COVID_Mild")
      rsv_flu_spec <- safe_pull(df_comb, "specific", "RSV_Mild_Flu_Mild_0")
      rsv_covid_spec <- safe_pull(df_comb, "specific", "RSV_Mild_0_COVID_Mild")
      flu_covid_spec <- safe_pull(df_comb, "specific", "0_Flu_Mild_COVID_Mild")
      rsv_flu_covid_spec <- safe_pull(df_comb, "specific", "RSV_Mild_Flu_Mild_COVID_Mild")
      
      rsv_sens <- safe_pull(df_comb, "sensitive", "RSV_Mild_0_0")
      flu_sens <- safe_pull(df_comb, "sensitive", "0_Flu_Mild_0")
      covid_sens <- safe_pull(df_comb, "sensitive", "0_0_COVID_Mild")
      rsv_flu_sens <- safe_pull(df_comb, "sensitive", "RSV_Mild_Flu_Mild_0")
      rsv_covid_sens <- safe_pull(df_comb, "sensitive", "RSV_Mild_0_COVID_Mild")
      flu_covid_sens <- safe_pull(df_comb, "sensitive", "0_Flu_Mild_COVID_Mild")
      rsv_flu_covid_sens <- safe_pull(df_comb, "sensitive", "RSV_Mild_Flu_Mild_COVID_Mild")
      
      #combine as tibble
      df_plot <- tibble(
        combo = factor(rep(c("RSV", "Flu", "COVID", "RSV & Flu", "RSV & COVID", 
                             "Flu & COVID", "RSV & Flu & COVID"), 2),
                       levels = c("RSV", "Flu", "COVID", "RSV & Flu", "RSV & COVID", 
                                  "Flu & COVID", "RSV & Flu & COVID")),
        n = c(rsv_spec, flu_spec, covid_spec, rsv_flu_spec, 
              rsv_covid_spec, flu_covid_spec, rsv_flu_covid_spec, 
              rsv_sens, flu_sens, covid_sens, rsv_flu_sens, 
              rsv_covid_sens, flu_covid_sens, rsv_flu_covid_sens),
        codelist_type = fct_rev(factor(c(rep("Specific", 7), rep("Sensitive", 7)),
                        levels = c("Specific", "Sensitive"), ordered = TRUE))
      )
      
    } else {
      
      rsv_total_spec <- safe_pull(df_comb, "specific", "RSV_Severe_Total")
      flu_total_spec <- safe_pull(df_comb, "specific", "Flu_Severe_Total")
      covid_total_spec <- safe_pull(df_comb, "specific", "COVID_Severe_Total")
      
      rsv_total_sens <- safe_pull(df_comb, "sensitive", "RSV_Severe_Total")
      flu_total_sens <- safe_pull(df_comb, "sensitive", "Flu_Severe_Total")
      covid_total_sens <- safe_pull(df_comb, "sensitive", "COVID_Severe_Total")
      
      rsv_spec <- safe_pull(df_comb, "specific", "RSV_Severe_0_0")
      flu_spec <- safe_pull(df_comb, "specific", "0_Flu_Severe_0")
      covid_spec <- safe_pull(df_comb, "specific", "0_0_COVID_Severe")
      rsv_flu_spec <- safe_pull(df_comb, "specific", "RSV_Severe_Flu_Severe_0")
      rsv_covid_spec <- safe_pull(df_comb, "specific", "RSV_Severe_0_COVID_Severe")
      flu_covid_spec <- safe_pull(df_comb, "specific", "0_Flu_Severe_COVID_Severe")
      rsv_flu_covid_spec <- safe_pull(df_comb, "specific", "RSV_Severe_Flu_Severe_COVID_Severe")
      
      rsv_sens <- safe_pull(df_comb, "sensitive", "RSV_Severe_0_0")
      flu_sens <- safe_pull(df_comb, "sensitive", "0_Flu_Severe_0")
      covid_sens <- safe_pull(df_comb, "sensitive", "0_0_COVID_Severe")
      rsv_flu_sens <- safe_pull(df_comb, "sensitive", "RSV_Severe_Flu_Severe_0")
      rsv_covid_sens <- safe_pull(df_comb, "sensitive", "RSV_Severe_0_COVID_Severe")
      flu_covid_sens <- safe_pull(df_comb, "sensitive", "0_Flu_Severe_COVID_Severe")
      rsv_flu_covid_sens <- safe_pull(df_comb, "sensitive", "RSV_Severe_Flu_Severe_COVID_Severe")
      
      #combine as tibble
      df_plot <- tibble(
        combo = factor(rep(c("RSV", "Flu", "COVID", "RSV & Flu", "RSV & COVID",
                             "Flu & COVID", "RSV & Flu & COVID"), 2),
                       levels = c("RSV", "Flu", "COVID", "RSV & Flu", "RSV & COVID",
                                  "Flu & COVID", "RSV & Flu & COVID")),
        n = c(rsv_spec, flu_spec, covid_spec, rsv_flu_spec, rsv_covid_spec, 
              flu_covid_spec, rsv_flu_covid_spec, rsv_sens, flu_sens,
              covid_sens, rsv_flu_sens, rsv_covid_sens, flu_covid_sens, 
              rsv_flu_covid_sens),
        codelist_type = fct_rev(factor(c(rep("Specific", 7), rep("Sensitive", 7)),
                        levels = c("Specific", "Sensitive"), ordered = TRUE))
      )
      
    }
    
  } else {
    
    if (outcome == "mild") {
      
      rsv_total_spec <- safe_pull(df_comb, "specific", "RSV_Mild_Total")
      flu_total_spec <- safe_pull(df_comb, "specific", "Flu_Mild_Total")
      
      rsv_total_sens <- safe_pull(df_comb, "sensitive", "RSV_Mild_Total")
      flu_total_sens <- safe_pull(df_comb, "sensitive", "Flu_Mild_Total")
      
      rsv_spec <- safe_pull(df_comb, "specific", "RSV_Mild_0")
      flu_spec <- safe_pull(df_comb, "specific", "0_Flu_Mild")
      rsv_flu_spec <- safe_pull(df_comb, "specific", "RSV_Mild_Flu_Mild")
      
      rsv_sens <- safe_pull(df_comb, "sensitive", "RSV_Mild_0")
      flu_sens <- safe_pull(df_comb, "sensitive", "0_Flu_Mild")
      rsv_flu_sens <- safe_pull(df_comb, "sensitive", "RSV_Mild_Flu_Mild")
      
      #combine as tibble
      df_plot <- tibble(
        combo = factor(rep(c("RSV", "Flu", "RSV & Flu"), 2),
                       levels = c("RSV", "Flu", "RSV & Flu")),
        n = c(rsv_spec, flu_spec, rsv_flu_spec, 
              rsv_sens, flu_sens, rsv_flu_sens),
        codelist_type = fct_rev(factor(c(rep("Specific", 3), rep("Sensitive", 3)),
                        levels = c("Specific", "Sensitive"), ordered = TRUE))
      )
      
    } else {
      
      rsv_total_spec <- safe_pull(df_comb, "specific", "RSV_Severe_Total")
      flu_total_spec <- safe_pull(df_comb, "specific", "Flu_Severe_Total")
      
      rsv_total_sens <- safe_pull(df_comb, "sensitive", "RSV_Severe_Total")
      flu_total_sens <- safe_pull(df_comb, "sensitive", "Flu_Severe_Total")
      
      rsv_spec <- safe_pull(df_comb, "specific", "RSV_Severe_0")
      flu_spec <- safe_pull(df_comb, "specific", "0_Flu_Severe")
      rsv_flu_spec <- safe_pull(df_comb, "specific", "RSV_Severe_Flu_Severe")
      
      rsv_sens <- safe_pull(df_comb, "sensitive", "RSV_Severe_0")
      flu_sens <- safe_pull(df_comb, "sensitive", "0_Flu_Severe")
      rsv_flu_sens <- safe_pull(df_comb, "sensitive", "RSV_Severe_Flu_Severe")
      
      #combine as tibble
      df_plot <- tibble(
        combo = factor(rep(c("RSV", "Flu", "RSV & Flu"), 2), 
                       levels = c("RSV", "Flu", "RSV & Flu")),
        n = c(rsv_spec, flu_spec, rsv_flu_spec,  
              rsv_sens, flu_sens, rsv_flu_sens),
        codelist_type = fct_rev(factor(c(rep("Specific", 3), rep("Sensitive", 3)),
                        levels = c("Specific", "Sensitive"), ordered = TRUE))
      )
      
    }
    
  }
  
  df_plot <- df_plot %>%
    mutate(n = if_else(is.na(n), 0, n))
  
  #stacked bar plot
  diag <- ggplot(df_plot, aes(x = combo, y = n, fill = codelist_type), alpha = 0.75) +
    geom_bar(stat = "identity", position = "stack") + 
    scale_y_continuous(labels = scales::label_comma()) +
    scale_fill_manual(values = c("#D81B60", "#1E88E5"), name  = "Codelist Type") +
    theme_minimal(base_size = 12) + ggeasy::easy_center_title() +
    theme(legend.position = "bottom", plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.background = element_rect(fill = "white", colour = "white")) +
      labs(x = "Infection Outcomes", y = "Number Identified", 
           title = "Increase in the Identification of\nOutcome Episodes",
           subtitle = paste0("Season: ", season, ", Outcome: ", str_to_title(outcome))) 
  
  diag2 <- grid.arrange(diag, newpage = TRUE)
  
  ggsave(diag2, filename = paste0(here::here("post_check", "plots",
                                  "exploratory_analyses"), "/", cohort,
                                  "_stacked_bar_diag_", season, "_",
                                  outcome, ".png"), height = 8, width = 15,
         device = "png")
  
}
