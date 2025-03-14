library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(khroma)
library(RColorBrewer)

## create output directories ----
fs::dir_create(here::here("post_check", "functions"))

#define a function to plot a characteristic over time
rate_viz <- function(df, pathogen, outcome_type) {
  
  df_rates <- df %>%
    filter(str_detect(Outcome, pathogen),
           str_detect(Outcome, outcome_type)) 
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (least deprived)", "2", "3", "4", "5 (most deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (least deprived)", "2", "3", "4", "5 (most deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Maternal Age", "Never",
                "Former", "Current", "Unknown", "Maternal Drug Usage (No)",
                "Maternal Drug Usage (Yes)", "Maternal Drinking (No)",
                "Maternal Drinking (Yes)", "Maternal Flu Vaccine (No)",
                "Maternal Flu Vaccine (Yes)", "Maternal Pertussis Vaccine (No)",
                "Maternal Pertussis Vaccine (Yes)")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("All", "2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (least deprived)", "2", "3", "4",
                "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  } else if (cohort == "adults") {
    
    levels <- c("All", "18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Has Asthma (No)", "Has Asthma (Yes)", "Has COPD (No)",
                "Has COPD (Yes)", "Has Cystic Fibrosis (No)",
                "Has Cystic Fibrosis (Yes)", "Has Other Resp. Diseases (No)",
                "Has Other Resp. Diseases (Yes)", "Has Diabetes (No)",
                "Has Diabetes (Yes)", "Has Addison's Disease (No)",
                "Has Addison's Disease (Yes)", "Severely Obese (No)",
                "Severely Obese (Yes)", "Has CHD (No)", "Has CHD (Yes)",
                "Has CLD (No)", "Has CLD (Yes)", "Has CKD (No)",
                "Has CKD (Yes)", "Has CND (No)", "Has CND (Yes)",
                "Had Cancer Within 3 Years (No)",
                "Had Cancer Within 3 Years (Yes)", "Immunosuppressed (No)",
                "Immunosuppressed (Yes)", "Has Sickle Cell Disease (No)",
                "Has Sickle Cell Disease (Yes)", "Never", "Current", "Former",
                "Hazardous Drinking (No)", "Hazardous Drinking (Yes)",
                "Drug Usage (No)", "Drug Usage (Yes)")
   
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  }
  
  if (investigation_type == "primary") {

    cols <- scales::seq_gradient_pal("#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
    cols <- c(
      cols[1], cols[1], cols[2], cols[2], cols[3], cols[3], cols[4], cols[4],
      cols[5], cols[5], cols[6], cols[6], cols[7], cols[7], cols[8], cols[8]
    )
    
    if (pathogen == "COVID") cols <- cols[7:16]
    
  } else {
    
    cols <- scales::seq_gradient_pal("#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
    
    if (pathogen == "RSV") cols <- cols[2]
    if (pathogen == "Flu") cols <- cols[3]
    if (pathogen == "COVID") cols <- cols[4]
    
  }
  
  if (investigation_type == "primary") {
    
    orders <- c(
      "2016-17, Specific",
      "2016-17, Sensitive",
      "2017-18, Specific",
      "2017-18, Sensitive",
      "2018-19, Specific",
      "2018-19, Sensitive",
      "2019-20, Specific",
      "2019-20, Sensitive",
      "2020-21, Specific",
      "2020-21, Sensitive",
      "2021-22, Specific",
      "2021-22, Sensitive",
      "2022-23, Specific",
      "2022-23, Sensitive",
      "2023-24, Specific",
      "2023-24, Sensitive"
    )
    
  } else {
    
    if (pathogen == "RSV") orders <- "2017-18, Specific"
    if (pathogen == "Flu") orders <- "2018-19, Specific"
    if (pathogen == "COVID") orders <- "2020-21, Specific"
    
  }
  
  df_rates <- df_rates %>%
    rowwise() %>%
    mutate(
      Group = if_else(Characteristic %in% c( "Total",
        "Age Group", "Sex", "Ethnicity", "IMD Quintile",
        "Household Composition Category", "Rurality Classification",
        "Maternal Age", "Maternal Smoking Status",
        "Time Since Last Covid Vaccine", "Smoking Status"), Group,
        paste0(Characteristic, " (", Group, ")"))
    ) %>%
    ungroup() %>%
    mutate(
      Group = forcats::fct_relevel(factor(Group), levels),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      across(Group, \(x) factor(
        x,
        levels = levels(x),
        labels = str_wrap(levels(x), width = 8)
      )),
      fill_type = case_when(
        pathogen == "Overall Respiratory" ~ list(subset),
        TRUE ~ list(paste0(subset, ", ", str_to_title(codelist_type))))[[1]]
    )
  df_rates <- df_rates %>%
    mutate(
      Group = if_else(str_detect(Group, "\\((\\w+)\\)"),
                      str_extract(Group, "(?<=\\()\\w+(?=\\))"), Group)
    )
  
  if (pathogen != "Overall Respiratory") {
    
    df_rates <- df_rates %>%
      mutate(
        fill_type = factor(fill_type, levels = orders)
      )
    
  }
  
  if (investigation_type == "primary") {
    
    df_rates %>%
      ggplot(aes(fill = fill_type, y = Rate_Midpoint10_Derived,
                 x = Group, alpha = fill_type)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      facet_wrap(~Characteristic, scales = "free_x") +
      theme_bw() + scale_fill_manual(name = "Season and Phenotype",
                                     values = cols) +
      scale_alpha_manual(name = "Season and Phenotype",
                         values = c(rep(c(1, 0.5), 8))) +
      labs(title = paste0("Rates of ", outcome_type, " ", pathogen),
           x = "Characteristic", y = "Rate per 1000 person-years") +
      theme(axis.text = element_text(size = 8))
    
  } else {
    
    df_rates %>%
      filter(Characteristic != "Rurality Classification") %>%
      ggplot(aes(fill = fill_type, y = Rate_Midpoint10_Derived,
                 x = Group, alpha = fill_type)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      facet_wrap(~Characteristic, scales = "free_x", ncol = 4) +
      theme_bw() + scale_fill_manual(name = "Season and Phenotype",
                                     values = cols) +
      scale_alpha_manual(name = "Season and Phenotype",
                         values = c(rep(c(1, 0.5), 8))) +
      labs(title = paste0("Rates of ", outcome_type, " ", pathogen),
           x = "Characteristic", y = "Rate per 1000 person-years") +
      theme(axis.text = element_text(size = 8), legend.position = "none")
    
  }
  
}

#define a function to plot a characteristic over time - different formatting
rate_viz_season <- function(df, pathogen, outcome_type) {
  
  df_rates <- df %>%
    filter(str_detect(Outcome, pathogen),
           str_detect(Outcome, outcome_type)) 
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (least deprived)", "2", "3", "4", "5 (most deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (least deprived)", "2", "3", "4", "5 (most deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Maternal Age", "Never",
                "Former", "Current", "Unknown", "Maternal Drug Usage (No)",
                "Maternal Drug Usage (Yes)", "Maternal Drinking (No)",
                "Maternal Drinking (Yes)", "Maternal Flu Vaccine (No)",
                "Maternal Flu Vaccine (Yes)", "Maternal Pertussis Vaccine (No)",
                "Maternal Pertussis Vaccine (Yes)")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("All", "2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (least deprived)", "2", "3", "4",
                "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  } else if (cohort == "adults") {
    
    levels <- c("All", "18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Has Asthma (No)", "Has Asthma (Yes)", "Has COPD (No)",
                "Has COPD (Yes)", "Has Cystic Fibrosis (No)",
                "Has Cystic Fibrosis (Yes)", "Has Other Resp. Diseases (No)",
                "Has Other Resp. Diseases (Yes)", "Has Diabetes (No)",
                "Has Diabetes (Yes)", "Has Addison's Disease (No)",
                "Has Addison's Disease (Yes)", "Severely Obese (No)",
                "Severely Obese (Yes)", "Has CHD (No)", "Has CHD (Yes)",
                "Has CLD (No)", "Has CLD (Yes)", "Has CKD (No)",
                "Has CKD (Yes)", "Has CND (No)", "Has CND (Yes)",
                "Had Cancer Within 3 Years (No)",
                "Had Cancer Within 3 Years (Yes)", "Immunosuppressed (No)",
                "Immunosuppressed (Yes)", "Has Sickle Cell Disease (No)",
                "Has Sickle Cell Disease (Yes)", "Never", "Current", "Former",
                "Hazardous Drinking (No)", "Hazardous Drinking (Yes)",
                "Drug Usage (No)", "Drug Usage (Yes)")
    
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  }
 
  if (investigation_type == "primary") {
    
    cols <- scales::seq_gradient_pal("#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
    cols <- c(
      cols[1], cols[1], cols[2], cols[2], cols[3], cols[3], cols[4], cols[4],
      cols[5], cols[5], cols[6], cols[6], cols[7], cols[7], cols[8], cols[8]
    )
    
    if (pathogen == "COVID") cols <- cols[7:16]
    
  } else {
    
    cols <- scales::seq_gradient_pal("#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
    if (pathogen == "RSV") cols <- cols[2]
    if (pathogen == "Flu") cols <- cols[3]
    if (pathogen == "COVID") cols <- cols[5]
    
  }
  
  if (investigation_type == "primary") {
    
    orders <- c(
      "2016-17, Specific",
      "2016-17, Sensitive",
      "2017-18, Specific",
      "2017-18, Sensitive",
      "2018-19, Specific",
      "2018-19, Sensitive",
      "2019-20, Specific",
      "2019-20, Sensitive",
      "2020-21, Specific",
      "2020-21, Sensitive",
      "2021-22, Specific",
      "2021-22, Sensitive",
      "2022-23, Specific",
      "2022-23, Sensitive",
      "2023-24, Specific",
      "2023-24, Sensitive"
    )
    
  } else {
    
    if (pathogen == "RSV") orders <- "2017-18, Specific"
    if (pathogen == "Flu") orders <- "2018-19, Specific"
    if (pathogen == "COVID") orders <- "2020-21, Specific"
    
  }
  
  df_rates <- df_rates %>%
    rowwise() %>%
    mutate(
      Group = if_else(Characteristic %in% c( "Total",
        "Age Group", "Sex", "Ethnicity", "IMD Quintile",
        "Household Composition Category", "Rurality Classification",
        "Maternal Age", "Maternal Smoking Status",
        "Time Since Last Covid Vaccine", "Smoking Status"), Group,
        paste0(Characteristic, " (", Group, ")"))
    ) %>%
    ungroup() %>%
    mutate(
      Group = forcats::fct_relevel(factor(Group), levels),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      across(Group, \(x) factor(
        x,
        levels = levels(x),
        labels = str_wrap(levels(x), width = 8)
      )),
      fill_type = case_when(
        pathogen == "Overall Respiratory" ~ list(subset),
        TRUE ~ list(paste0(subset, ", ", str_to_title(codelist_type))))[[1]]
    )
  df_rates <- df_rates %>%
    mutate(
      Group = if_else(str_detect(Group, "\\((\\w+)\\)"),
                      str_extract(Group, "(?<=\\()\\w+(?=\\))"), Group)
    )
  
  if (pathogen != "Overall Respiratory") {
    
    df_rates <- df_rates %>%
      mutate(
        fill_type = factor(fill_type, levels = orders)
      )
    
  }
  
  plot_list <- list()
  
  for (group in unique(df_rates$Characteristic)) {
  
  plot_list[[group]] <- df_rates %>%
    filter(Characteristic == !!group) %>%
    ggplot(aes(y = Rate_Midpoint10_Derived, x = Group,
               alpha = fill_type, fill = fill_type)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    facet_wrap(~subset, scales = "free_x") +
    theme_bw() + scale_fill_manual(name = "Season and Phenotype",
                                   values = cols) +
    scale_alpha_manual(name = "Season and Phenotype",
                       values = c(rep(c(1, 0.5), 8))) +
    labs(title = paste0("Rates of ", outcome_type, " ", pathogen,
                        " by ", group), x = "Group",
         y = "Rate per 1000 person-years")
    
  }

  return(plot_list)
  
}

#define a function to plot a characteristic over time - different formatting
rate_viz_mult <- function(df, pathogen, outcome_type) {
  
  df_rates <- df %>%
    filter(str_detect(Outcome, pathogen),
           str_detect(Outcome, outcome_type)) 
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (least deprived)", "2", "3", "4", "5 (most deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (least deprived)", "2", "3", "4", "5 (most deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Maternal Age", "Never",
                "Former", "Current", "Unknown", "Maternal Drug Usage (No)",
                "Maternal Drug Usage (Yes)", "Maternal Drinking (No)",
                "Maternal Drinking (Yes)", "Maternal Flu Vaccine (No)",
                "Maternal Flu Vaccine (Yes)", "Maternal Pertussis Vaccine (No)",
                "Maternal Pertussis Vaccine (Yes)")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("All", "2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (least deprived)", "2", "3", "4",
                "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  } else if (cohort == "adults") {
    
    levels <- c("All", "18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season (No)",
                  "Vaccinated against influenza in previous season (Yes)",
                  "Vaccinated against influenza in current season (No)",
                  "Vaccinated against influenza in current season (Yes)")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season (No)",
                  "Vaccinated against COVID-19 in current season (Yes)")
      
    }
    
  }
  
  cols <- scales::seq_gradient_pal("#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
  cols <- c(
    cols[1], cols[1], cols[2], cols[2], cols[3], cols[3], cols[4], cols[4],
    cols[5], cols[5], cols[6], cols[6], cols[7], cols[7], cols[8], cols[8]
  )
  
  if (pathogen == "COVID") cols <- cols[7:16]
  
  orders <- c(
    "2016-17, Specific",
    "2016-17, Sensitive",
    "2017-18, Specific",
    "2017-18, Sensitive",
    "2018-19, Specific",
    "2018-19, Sensitive",
    "2019-20, Specific",
    "2019-20, Sensitive",
    "2020-21, Specific",
    "2020-21, Sensitive",
    "2021-22, Specific",
    "2021-22, Sensitive",
    "2022-23, Specific",
    "2022-23, Sensitive",
    "2023-24, Specific",
    "2023-24, Sensitive"
  )
  
  df_rates <- df_rates %>%
    rowwise() %>%
    mutate(
      Group = if_else(Characteristic %in% c( "Total",
        "Age Group", "Sex", "Ethnicity", "IMD Quintile",
        "Household Composition Category", "Rurality Classification",
        "Maternal Age", "Maternal Smoking Status",
        "Time Since Last Covid Vaccine", "Smoking Status"), Group,
        paste0(Characteristic, " (", Group, ")"))
    ) %>%
    ungroup() %>%
    mutate(
      Group = forcats::fct_relevel(factor(Group), levels),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      across(Group, \(x) factor(
        x,
        levels = levels(x),
        labels = str_wrap(levels(x), width = 8)
      )),
      fill_type = case_when(
        pathogen == "Overall Respiratory" ~ list(subset),
        TRUE ~ list(paste0(subset, ", ", str_to_title(codelist_type))))[[1]]
    )
  df_rates <- df_rates %>%
    mutate(
      Group = if_else(str_detect(Group, "\\((\\w+)\\)"),
                      str_extract(Group, "(?<=\\()\\w+(?=\\))"), Group)
    )
  
  if (pathogen != "Overall Respiratory") {
    
    df_rates <- df_rates %>%
      mutate(
        fill_type = factor(fill_type, levels = orders)
      )
    
  }
  
  plot_list <- list()
  
  for (group in unique(df_rates$Characteristic)) {
    
    plot_list[[group]] <- df_rates %>%
      filter(Characteristic == !!group) %>%
      ggplot(aes(y = Rate_Midpoint10_Derived, x = subset,
                 alpha = fill_type, fill = fill_type)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      facet_wrap(~Group, scales = "free_x") +
      theme_bw() + scale_fill_manual(name = "Season and Phenotype",
                                    values = cols) +
      scale_alpha_manual(name = "Season and Phenotype",
                         values = c(rep(c(1, 0.5), 8))) +
      labs(title = paste0("Rates of ", outcome_type, " ", pathogen,
                          " by ", group, " Over Seasons"), x = "Season",
           y = "Rate per 1000 person-years")
    
  }
  
  return(plot_list)
  
}
