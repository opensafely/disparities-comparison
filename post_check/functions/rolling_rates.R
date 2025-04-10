library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(ggplot2)
library(zoo)

source(here("analysis", "design", "design.R"))

#helper function to create and save plots
create_rolling_plots <- function(season, phenotype) {

  start <- year(study_dates[[paste0(season, "_start_date")]])
  end <- year(study_dates[[paste0(season, "_end_date")]])
  
  outcomes <- c(paste0(pathogen, "_primary_date"),
                paste0(pathogen, "_secondary_date"))
  
  df <- read_csv(here::here("post_check", "output", "results", "rates",
                 "weekly", paste0("rates_over_time_", pathogen, "_",
                 cohort, "_", start, "_", end, "_", phenotype, "_",
                 investigation_type, ".csv")))
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
  } else {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
  }
  
  df <- df %>%
    mutate(
      group = factor(group, levels = levels),
      characteristic = case_when(
        group %in% c("0-2m", "3-5m", "6-11m", "12-23m", "65-74y",
                     "75-89y", "90y+") ~ "Age Group",
        group %in% c("Female", "Male") ~ "Sex",
        group %in% c("White", "Mixed", "Asian or Asian British",
                     "Black or Black British", "Other Ethnic Groups",
                     "Unknown") ~ "Ethnicity",
        group %in% c("1 (least deprived)", "2", "3", "4",
                     "5 (most deprived)") ~ "IMD Quintile",
        group %in% c("Multiple of the Same Generation", "Living Alone",
                     "One Other Generation", "Two Other Generations",
                     "Three Other Generations") ~ "Household Composition",
        group %in% c("Urban Major Conurbation", "Urban Minor Conurbation",
                     "Urban City and Town", "Rural Town and Fringe",
                     "Rural Village and Dispersed") ~ "Rurality"
      )
    )
  
  characteristics <- unique(df$characteristic)
  
  cols <- tibble(
    var = c("Age Group", "Sex", "Ethnicity", "IMD Quintile",
            "Household Composition", "Rurality"),
    col = c('#1f77b4', '#ffbb78', '#2ca02c', '#ff9896',
            '#aec7e8', '#ff7f0e')
  )
  
  plots <- list()
  
  for (characteristic in characteristics) {
    
    col <- cols %>%
      filter(var == characteristic) %>%
      pull(col)
    
    alpha_length <- df %>%
      filter(characteristic == !!characteristic)
    alpha_length <- length(unique(alpha_length$group))
    
    pathogen_print <- case_when(
      pathogen == "rsv" ~ "RSV",
      pathogen == "flu" ~ "Influenza",
      pathogen == "covid" ~ "COVID-19",
      pathogen == "overall_resp" ~ "Overall Respiratory"
    )
    
    df <- df %>%
      mutate(
        outcome = case_when(
          event == paste0(pathogen, "_primary_date") ~
            paste0("Mild ", pathogen_print),
          event == paste0(pathogen, "_secondary_date") ~
            paste0("Severe ", pathogen_print),
        ),
      )
    
    options(scipen = 999)
    
    plots[[characteristic]] <- df %>%
      filter(characteristic == !!characteristic) %>%
      ggplot(aes(x = interval_start, y = rate_1000_py_midpoint10_derived,
                 group = group, alpha = group)) +
      geom_line(stat = "smooth", method = "loess", col = col, lwd = 1,
                span = 0.25, se = FALSE) + theme_bw() +
      labs(x = "", y = "",
           alpha = "Group") + scale_alpha_manual(values = c(
           seq(1, 0.25, length.out = alpha_length))) + facet_wrap( ~ outcome)
    
  }
  
  return(plots)
  
}

#helper function to create and save plots
create_rolling_plots_overall <- function(df, pathogen, phenotype) {
  
  cols <- scales::seq_gradient_pal(
    "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
  cols <- c(cols[2], cols[3], cols[5], cols[8])
  
  pathogen_print <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19",
    pathogen == "overall_resp" ~ "Overall Respiratory"
  )
  
  df <- df %>%
    mutate(
      outcome = case_when(
        event == paste0(pathogen, "_primary_date") ~
          paste0("Mild ", pathogen_print),
        event == paste0(pathogen, "_secondary_date") ~
          paste0("Severe ", pathogen_print),
      ),
    )
  
  options(scipen = 999)
  
  plot <- df %>%
    ggplot(aes(x = interval_start, y = rate_py)) +
    geom_line(stat = "smooth", method = "loess", span = 0.25, se = FALSE,
              col = cols, lwd = 1, alpha = codelist_type) + theme_bw() +
    labs(x = "Date", y = "Rate per 1000 person-years\n(Midpoint 10 Derived)",
         col = "Season", alpha = "Phenotype") + facet_wrap(~ outcome, ncol = 2) +
    ggtitle(paste0("30-Day Rolling Rate of ", str_to_title(gsub("_", " ",
            pathogen_print))))
  
  return(plot)
  
}
