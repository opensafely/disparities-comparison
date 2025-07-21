library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#define a function to plot a characteristic over time
rate_viz <- function(df, pathogen, outcome_type, interest = "no") {
  
  df <- df %>%
    mutate(
      Group = case_when(
        Group == "1 (least deprived)" ~ "5 (least deprived)",
        Group == "2" ~ "4",
        Group == "4" ~ "2",
        Group == "5 (most deprived)" ~ "1 (most deprived)",
        TRUE ~ Group
      )
    )
  
  df_rates <- df %>%
    filter(str_detect(Outcome, pathogen),
           str_detect(Outcome, outcome_type)) 
  
  if (interest == "yes") {
    
    filt <- case_when(
      pathogen == "RSV" ~ "2017_18",
      pathogen == "Flu" ~ "2018_19",
      pathogen == "COVID" ~ "2020_21"
    )
    
    df_rates <- df_rates %>%
      filter(subset == !!filt)
    
  }
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Never",
                "Former", "Current", "No", "Yes")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("All", "2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (most deprived)", "2", "3", "4",
                "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  } else if (cohort == "adults") {
    
    levels <- c("All", "18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
                "4", "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "No", "Yes", "Current", "Former", "Never")
   
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
                "4", "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  }
  
  if (investigation_type == "primary") {
    
    if (interest == "yes") {
      
      cols <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
      
      cols <- case_when(
        pathogen == "RSV" ~ c(cols[2], cols[2]),
        pathogen == "Flu" ~ c(cols[3], cols[3]),
        pathogen == "COVID" ~ c(cols[5], cols[5])
      )
      
    } else {
      
      cols <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
      cols <- c(
        cols[1], cols[1], cols[2], cols[2], cols[3], cols[3], cols[4], cols[4],
        cols[5], cols[5], cols[6], cols[6], cols[7], cols[7], cols[8], cols[8]
      )
      
      if (pathogen == "COVID") cols <- cols[7:16]
      
      if (pathogen == "Overall Respiratory") {
        
        cols <- scales::seq_gradient_pal(
          "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
        
      }
      
    }
    
  } else {
    
    cols <- scales::seq_gradient_pal(
      "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
    
    if (pathogen == "RSV") cols <- cols[2]
    if (pathogen == "Flu") cols <- cols[3]
    if (pathogen == "COVID") cols <- cols[4]
    
  }
  
  if (interest == "yes") {
    
    orders <- case_when(
      pathogen == "RSV" ~ c("2017-18, Specific", "2017-18, Sensitive"),
      pathogen == "Flu" ~ c("2018-19, Specific", "2018-19, Sensitive"),
      pathogen == "COVID" ~ c("2020-21, Specific", "2020-21, Sensitive")
    )
    
  } else if (investigation_type == "primary" & pathogen %in% c("RSV", "Flu")) {
    
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
    
  } else if (investigation_type == "primary" & pathogen == "COVID") {
    
    orders <- c(
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
    
  } else if (investigation_type == "primary" &
             pathogen == "Overall Respiratory") {
    
    orders <- c(
      "2016-17, Sensitive",
      "2017-18, Sensitive",
      "2018-19, Sensitive",
      "2019-20, Sensitive",
      "2020-21, Sensitive",
      "2021-22, Sensitive",
      "2022-23, Sensitive",
      "2023-24, Sensitive"
    )
    
  } else {
    
    if (pathogen == "RSV") orders <- "2017-18, Specific"
    if (pathogen == "Flu") orders <- "2018-19, Specific"
    if (pathogen == "COVID") orders <- "2020-21, Specific"
    
  }
  
  faceting <- c(
    "Total", "Age Group", "Sex", "Ethnicity", "IMD Quintile",
    "Household Composition Category", "Rurality Classification",
    "Maternal Smoking Status", "Maternal Drinking",
    "Maternal Drug Usage", "Maternal Influenza Vaccination Status",
    "Maternal Pertussis Vaccination Status",
    "Vaccinated against influenza in previous season",
    "Vaccinated against influenza in current season",
    "Time Since Last COVID-19 Vaccination",
    "Vaccinated against COVID-19 in current season",
    "Has Asthma", "Has COPD", "Has Cystic Fibrosis",
    "Has Other Resp. Diseases", "Has Diabetes",
    "Has Addison's Disease", "Severely Obese", "Has CHD",
    "Has CKD", "Has CLD", "Has CND", "Had Cancer Within 3 Years",
    "Immunosuppressed", "Has Sickle Cell Disease",
    "Hazardous Drinking", "Drug Usage", "Smoking Status"
  )
  
  df_rates <- df_rates %>%
    rowwise() %>%
    mutate(
      Group = if_else(Characteristic %in% c( "Total",
        "Age Group", "Sex", "Ethnicity", "IMD Quintile",
        "Household Composition Category", "Rurality Classification",
        "Average Maternal Age", "Maternal Smoking Status",
        "Time Since Last COVID-19 Vaccination", "Smoking Status"), Group,
        paste0(Characteristic, " (", Group, ")"))
    ) %>%
    ungroup() %>%
    mutate(
      Group = if_else(str_detect(Group, "\\((\\w+)\\)"),
                      str_extract(Group, "(?<=\\()\\w+(?=\\))"), Group)
    ) %>%
    mutate(
      Group = factor(Group, levels, labels = str_wrap(levels, width = 8.5)),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      fill_type = case_when(
        pathogen == "Overall Respiratory" ~ list(subset),
        TRUE ~ list(paste0(subset, ", ", str_to_title(codelist_type))))[[1]],
      Characteristic = factor(Characteristic, levels = faceting)
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
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Characteristic, scales = "free_x") +
      theme_bw() + scale_fill_manual(name = "Season and Phenotype",
                                     values = cols) +
      scale_alpha_manual(name = "Season and Phenotype",
                         values = c(rep(c(1, 0.5), 8))) +
      labs(title = paste0("Rates of ", outcome_type, " ", pathogen),
           x = "Characteristic", y = "Rate per 1000 person-years") +
      theme(text = element_text(size = 12))
    
  } else {
    
    df_rates %>%
      filter(Characteristic != "Rurality Classification") %>%
      ggplot(aes(fill = fill_type, y = Rate_Midpoint10_Derived,
                 x = Group, alpha = fill_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Characteristic, scales = "free_x", ncol = 4) +
      theme_bw() + scale_fill_manual(name = "Season and Phenotype",
                                     values = cols) +
      scale_alpha_manual(name = "Season and Phenotype",
                         values = c(rep(c(1, 0.5), 8))) +
      labs(title = paste0("Rates of ", outcome_type, " ", pathogen),
           x = "Characteristic", y = "Rate per 1000 person-years") +
      theme(text = element_text(size = 12), legend.position = "none")
    
  }
  
}

#define a function to plot a characteristic over time - different formatting
rate_viz_season <- function(df, pathogen, outcome_type, interest = "no") {
  
  df <- df %>%
    mutate(
      Group = case_when(
        Group == "1 (least deprived)" ~ "5 (least deprived)",
        Group == "2" ~ "4",
        Group == "4" ~ "2",
        Group == "5 (most deprived)" ~ "1 (most deprived)",
        TRUE ~ Group
      )
    )
  
  df_rates <- df %>%
    filter(str_detect(Outcome, pathogen),
           str_detect(Outcome, outcome_type)) 
  
  if (interest == "yes") {
    
    filt <- case_when(
      pathogen == "RSV" ~ "2017_18",
      pathogen == "Flu" ~ "2018_19",
      pathogen == "COVID" ~ "2020_21"
    )
    
    df_rates <- df_rates %>%
      filter(subset == !!filt)
    
  }
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Never",
                "Former", "Current", "No", "Yes")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("All", "2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (most deprived)", "2", "3", "4",
                "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  } else if (cohort == "adults") {
    
    levels <- c("All", "18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
                "4", "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "No", "Yes", "Current", "Former", "Never")
    
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
                "4", "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  }
 
  if (investigation_type == "primary") {
    
    if (interest == "yes") {
      
      cols <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
      
      cols <- case_when(
        pathogen == "RSV" ~ c(cols[2], cols[2]),
        pathogen == "Flu" ~ c(cols[3], cols[3]),
        pathogen == "COVID" ~ c(cols[5], cols[5])
      )
      
    } else {
      
      cols <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
      cols <- c(
        cols[1], cols[1], cols[2], cols[2], cols[3], cols[3], cols[4], cols[4],
        cols[5], cols[5], cols[6], cols[6], cols[7], cols[7], cols[8], cols[8]
      )
      
      if (pathogen == "COVID") cols <- cols[7:16]
      
      if (pathogen == "Overall Respiratory") {
        
        cols <- scales::seq_gradient_pal(
          "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
        
      }
      
    }
    
  } else {
    
    cols <- scales::seq_gradient_pal(
      "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
    
    if (pathogen == "RSV") cols <- cols[2]
    if (pathogen == "Flu") cols <- cols[3]
    if (pathogen == "COVID") cols <- cols[4]
    
  }
  
  if (interest == "yes") {
    
    orders <- case_when(
      pathogen == "RSV" ~ c("2017-18, Specific", "2017-18, Sensitive"),
      pathogen == "Flu" ~ c("2018-19, Specific", "2018-19, Sensitive"),
      pathogen == "COVID" ~ c("2020-21, Specific", "2020-21, Sensitive")
    )
    
  } else if (investigation_type == "primary" & pathogen %in% c("RSV", "Flu")) {
    
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
    
  } else if (investigation_type == "primary" & pathogen == "COVID") {
    
    orders <- c(
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
    
  } else if (investigation_type == "primary" &
             pathogen == "Overall Respiratory") {
    
    orders <- c(
      "2016-17, Sensitive",
      "2017-18, Sensitive",
      "2018-19, Sensitive",
      "2019-20, Sensitive",
      "2020-21, Sensitive",
      "2021-22, Sensitive",
      "2022-23, Sensitive",
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
        "Average Maternal Age", "Maternal Smoking Status",
        "Time Since Last COVID-19 Vaccination", "Smoking Status"), Group,
        paste0(Characteristic, " (", Group, ")"))
    ) %>%
    ungroup() %>%
    mutate(
      Group = if_else(str_detect(Group, "\\((\\w+)\\)"),
                      str_extract(Group, "(?<=\\()\\w+(?=\\))"), Group)
    ) %>%
    mutate(
      Group = factor(Group, levels, labels = str_wrap(levels, width = 8.5)),
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
    ggplot(aes(y = Rate_Midpoint10_Derived, x = Group,
               alpha = fill_type, fill = fill_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~subset, scales = "free_x") +
    theme_bw() + scale_fill_manual(name = "Season and Phenotype",
                                   values = cols) +
    scale_alpha_manual(name = "Season and Phenotype",
                       values = c(rep(c(1, 0.5), 8))) +
    labs(title = paste0("Rates of ", outcome_type, " ", pathogen,
                        " by ", group), x = "Group",
         y = "Rate per 1000 person-years") +
    theme(text = element_text(size = 12))
    
  }

  return(plot_list)
  
}

#define a function to plot a characteristic over time - different formatting
rate_viz_mult <- function(df, pathogen, outcome_type, interest = "no") {
  
  df <- df %>%
    mutate(
      Group = case_when(
        Group == "1 (least deprived)" ~ "5 (least deprived)",
        Group == "2" ~ "4",
        Group == "4" ~ "2",
        Group == "5 (most deprived)" ~ "1 (most deprived)",
        TRUE ~ Group
      )
    )
  
  df_rates <- df %>%
    filter(str_detect(Outcome, pathogen),
           str_detect(Outcome, outcome_type)) 
  
  if (interest == "yes") {
    
    filt <- case_when(
      pathogen == "RSV" ~ "2017_18",
      pathogen == "Flu" ~ "2018_19",
      pathogen == "COVID" ~ "2020_21"
    )
    
    df_rates <- df_rates %>%
      filter(subset == !!filt)
    
  }
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("All", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male",
                "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups", "Unknown",
                "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Never",
                "Former", "Current", "No", "Yes")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("All", "2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (most deprived)", "2", "3", "4",
                "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  } else if (cohort == "adults") {
    
    levels <- c("All", "18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
                "4", "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "No", "Yes", "Current", "Former", "Never")
    
  } else {
    
    levels <- c("All", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
                "4", "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
    if (pathogen == "Flu") {
      
      levels <- c(levels, "No", "Yes")
      
    } else if (pathogen == "COVID") {
      
      levels <- c(levels, "0-6m", "6-12m", "12m+", "No", "Yes")
      
    }
    
  }
  
  
  if (investigation_type == "primary") {
    
    if (interest == "yes") {
      
      cols <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
      
      cols <- case_when(
        pathogen == "RSV" ~ c(cols[2], cols[2]),
        pathogen == "Flu" ~ c(cols[3], cols[3]),
        pathogen == "COVID" ~ c(cols[5], cols[5])
      )
      
    } else {
      
      cols <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
      cols <- c(
        cols[1], cols[1], cols[2], cols[2], cols[3], cols[3], cols[4], cols[4],
        cols[5], cols[5], cols[6], cols[6], cols[7], cols[7], cols[8], cols[8]
      )
      
      if (pathogen == "COVID") cols <- cols[7:16]
      
      if (pathogen == "Overall Respiratory") {
        
        cols <- scales::seq_gradient_pal(
          "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
        
      }
      
    }
    
  } else {
    
    cols <- scales::seq_gradient_pal(
      "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
    
    if (pathogen == "RSV") cols <- cols[2]
    if (pathogen == "Flu") cols <- cols[3]
    if (pathogen == "COVID") cols <- cols[4]
    
  }
  
  if (interest == "yes") {
    
    orders <- case_when(
      pathogen == "RSV" ~ c("2017-18, Specific", "2017-18, Sensitive"),
      pathogen == "Flu" ~ c("2018-19, Specific", "2018-19, Sensitive"),
      pathogen == "COVID" ~ c("2020-21, Specific", "2020-21, Sensitive")
    )
    
  } else if (investigation_type == "primary" & pathogen %in% c("RSV", "Flu")) {
    
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
    
  } else if (investigation_type == "primary" & pathogen == "COVID") {
    
    orders <- c(
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
    
  } else if (investigation_type == "primary" &
             pathogen == "Overall Respiratory") {
    
    orders <- c(
      "2016-17, Sensitive",
      "2017-18, Sensitive",
      "2018-19, Sensitive",
      "2019-20, Sensitive",
      "2020-21, Sensitive",
      "2021-22, Sensitive",
      "2022-23, Sensitive",
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
        "Maternal Smoking Status",
        "Time Since Last COVID-19 Vaccination", "Smoking Status"), Group,
        paste0(Characteristic, " (", Group, ")"))
    ) %>%
    ungroup() %>%
    mutate(
      Group = if_else(str_detect(Group, "\\((\\w+)\\)"),
                      str_extract(Group, "(?<=\\()\\w+(?=\\))"), Group)
    ) %>%
    mutate(
      Group = factor(Group, levels, labels = str_wrap(levels, width = 8.5)),
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
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Group, scales = "free_x") +
      theme_bw() + scale_fill_manual(name = "Season and Phenotype",
                                    values = cols) +
      scale_alpha_manual(name = "Season and Phenotype",
                         values = c(rep(c(1, 0.5), 8))) +
      labs(title = paste0("Rates of ", outcome_type, " ", pathogen,
                          " by ", group, " Over Seasons"), x = "Season",
           y = "Rate per 1000 person-years") +
      theme(text = element_text(size = 12))
    
  }
  
  return(plot_list)
  
}
