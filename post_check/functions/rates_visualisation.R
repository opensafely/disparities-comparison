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
                "Former", "Current", "Unknown", "Maternal Drug Usage",
                "Maternal Drinking", "Maternal Flu Vaccine",
                "Maternal Pertussis Vaccine")
    
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
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
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
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
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
                "Has Asthma", "Has COPD", "Has Cystic Fibrosis",
                "Has Other Resp. Diseases", "Has Diabetes",
                "Has Addison's Disease", "Severely Obese", "Has CHD", "Has CKD",
                "Has CLD", "Has CND", "Had Cancer Within 3 Years",
                "Immunosuppressed", "Has Sickle Cell Disease", "Smoking Status",
                "Hazardous Drinking", "Drug Usage", "Never", "Current",
                "Former", "Unknown")
   
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
    }
    
  }
  
  f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)[1:8]
  cols <- f("Dark2")
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
    mutate(
      Group = forcats::fct_relevel(factor(Group), levels),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      across(Group, \(x) factor(
        x,
        levels = levels(x),
        labels = str_wrap(levels(x), width = 15)
      )),
      fill_type = case_when(
        pathogen == "Overall Respiratory" ~ list(subset),
        TRUE ~ list(paste0(subset, ", ", str_to_title(codelist_type))))[[1]]
    )
  
  if (pathogen != "Overall Respiratory") {
    
    df_rates <- df_rates %>%
      mutate(
        fill_type = factor(fill_type, levels = orders)
      )
    
  }
  
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
         x = "Characteristic", y = "Rate per 1000 person-years")
  
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
                "Former", "Current", "Unknown", "Maternal Drug Usage",
                "Maternal Drinking", "Maternal Flu Vaccine",
                "Maternal Pertussis Vaccine")
    
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
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
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
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
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
                "Has Asthma", "Has COPD", "Has Cystic Fibrosis",
                "Has Other Resp. Diseases", "Has Diabetes",
                "Has Addison's Disease", "Severely Obese", "Has CHD", "Has CKD",
                "Has CLD", "Has CND", "Had Cancer Within 3 Years",
                "Immunosuppressed", "Has Sickle Cell Disease", "Smoking Status",
                "Hazardous Drinking", "Drug Usage", "Never", "Current",
                "Former", "Unknown")
    
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
    }
    
  }
 
  f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)[1:8]
  cols <- f("Dark2")
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
    mutate(
      Group = forcats::fct_relevel(factor(Group), levels),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      across(Group, \(x) factor(
        x,
        levels = levels(x),
        labels = str_wrap(levels(x), width = 15)
      )),
      fill_type = case_when(
        pathogen == "Overall Respiratory" ~ list(subset),
        TRUE ~ list(paste0(subset, ", ", str_to_title(codelist_type))))[[1]]
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
  
  return(list(plot_list))
  
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
                "Former", "Current", "Unknown", "Maternal Drug Usage",
                "Maternal Drinking", "Maternal Flu Vaccine",
                "Maternal Pertussis Vaccine")
    
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
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
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
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
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
                "Has Asthma", "Has COPD", "Has Cystic Fibrosis",
                "Has Other Resp. Diseases", "Has Diabetes",
                "Has Addison's Disease", "Severely Obese", "Has CHD", "Has CKD",
                "Has CLD", "Has CND", "Had Cancer Within 3 Years",
                "Immunosuppressed", "Has Sickle Cell Disease", "Smoking Status",
                "Hazardous Drinking", "Drug Usage", "Never", "Current",
                "Former", "Unknown")
    
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
    if (pathogen == "flu") {
      
      levels <- c(levels, "Vaccinated against influenza in previous season",
                  "Vaccinated against influenza in current season")
      
    } else if (pathogen == "covid") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+",
                  "Vaccinated against COVID-19 in current season")
      
    }
    
  }
  
  f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)[1:8]
  cols <- f("Dark2")
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
    mutate(
      Group = forcats::fct_relevel(factor(Group), levels),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      fill_type = case_when(
        pathogen == "Overall Respiratory" ~ list(subset),
        TRUE ~ list(paste0(subset, ", ", str_to_title(codelist_type))))[[1]]
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
                          " by ", group), x = "Season",
           y = "Rate per 1000 person-years")
    
  }
  
  return(list(plot_list))
  
}
