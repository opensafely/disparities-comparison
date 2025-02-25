library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(khroma)

## create output directories ----
fs::dir_create(here::here("post_check", "functions"))

#define a function to plot a characteristic over time
character_viz <- function(df) {
  
  names(df) <- c("characteristic", "count", "percentage", "subset")
  
  age_groups <- case_when(
    cohort == "adults" ~ 2,
    cohort == "older_adults" ~ 3,
    TRUE ~ 4
  )
  
  # Define mapping of characteristic to number of categories
  group_counts <- tibble(
    group = c("Total", "Age Group", "Sex", "Ethnicity", "IMD",
              "Rurality", "Household Composition", "Maternal Age",
              "Maternal Smoking Status", "Maternal Drinking",
              "Maternal Drug Usage", "Maternal Flu Vaccination",
              "Maternal Pertussis Vaccination", "Prior Flu Vaccine",
              "Time Since Last Covid Vaccine", "Has Asthma", "Has COPD",
              "Has Cystic Fibrosis", "Has Other Resp. Diseases",
              "Has Diabetes", "Has Addison's Disease", "Severely Obese",
              "Has CHD", "Has CKD", "Has CLD", "Has CND",
              "Had Cancer Within 3 Years", "Immunosuppressed",
              "Has Sickle Cell Disease", "Smoking Status",
              "Hazardous Drinking", "Drug Usage"),
    count = c(1, age_groups, 2, 6, 5, 5, 5, 1, 4, 1, 1, 1, 1, 1, 3, 1, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1)
  )
  
  group_counts <- group_counts %>%
    filter(group %in% unique(df$characteristic)) %>%
    filter(group != "Household Composition") %>%
    group_by(group) %>%
    uncount(weights = 8) %>%
    mutate(season = c("2016_17", "2017_18", "2018_19", "2019_20",
                      "2020_21", "2021_22", "2022_23", "2023_24"))
  
  if (cohort != "infants" & cohort != "infants_subgroup") {
    
    group_counts_hh <- group_counts %>%
      filter(group == "Household Composition") %>%
      mutate(season = "2020_21") 
    
    group_counts <- bind_rows(group_counts, group_counts_hh)
  
  }
  
  df_groups <- uncount(group_counts, weights = count) %>%
    arrange(season)
  
  df <- df %>%
    filter(!is.na(percentage)) %>%
    cbind(df_groups)
  
  df <- df %>%
    mutate(
      percentage = substring(percentage, 1, nchar(percentage) - 1) %>%
        as.numeric(),
    )
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Maternal Age", "Never", "Former", "Current", "Unknown",
                "Maternal Drug Usage", "Maternal Drinking",
                "Maternal Flu Vaccine", "Maternal Pertussis Vaccine")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (least deprived)", "2", "3", "4",
                "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Prior Flu Vaccine",
                "0-6m", "6-12m", "12m+")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("Has Asthma", "Has COPD", "Has Cystic Fibrosis",
                "Has Other Resp. Diseases", "Has Diabetes",
                "Has Addison's Disease", "Severely Obese", "Has CHD",
                "Has CKD", "Has CLD", "Has CND", "Had Cancer Within 3 Years",
                "Immunosuppressed", "Has Sickle Cell Disease", "Smoking Status",
                "Hazardous Drinking", "Drug Usage", "Never", "Current",
                "Former", "Unknown")
    
  } else {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
  }
  
  df %>%
    filter(group != "Total") %>%
    mutate(
      characteristic = forcats::fct_relevel(factor(characteristic),
                                            levels),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      across(characteristic, \(x) factor(
        x,
        levels = levels(x),
        labels = str_wrap(levels(x), width = 15)
      ))
    ) %>%
    ggplot(aes(fill = subset, y = percentage, x = characteristic)) +
    geom_bar(stat = "identity", position = "dodge", color = "white") +
    facet_wrap(~group, scales = "free") +
    #scale_fill_viridis_d(option = "mako") + 
    theme_bw() + scale_fill_brewer(palette = "PuOr") +
    labs(title = "Participant Characteristics", x = "Characteristic",
         y = "Percentage (%)") +
    guides(fill = guide_legend(title = "Season")) 
  
}

#define a function to plot a characteristic over time - different formatting
character_viz_mult <- function(df) {
  
  names(df) <- c("characteristic", "count", "percentage", "subset")
  
  age_groups <- case_when(
    cohort == "adults" ~ 2,
    cohort == "older_adults" ~ 3,
    TRUE ~ 4
  )
  
  # Define mapping of characteristic to number of categories
  group_counts <- tibble(
    group = c("Total", "Age Group", "Sex", "Ethnicity", "IMD",
              "Rurality", "Household Composition", "Maternal Age",
              "Maternal Smoking Status", "Maternal Drinking",
              "Maternal Drug Usage", "Maternal Flu Vaccination",
              "Maternal Pertussis Vaccination", "Prior Flu Vaccine",
              "Time Since Last Covid Vaccine", "Has Asthma", "Has COPD",
              "Has Cystic Fibrosis", "Has Other Resp. Diseases",
              "Has Diabetes", "Has Addison's Disease", "Severely Obese",
              "Has CHD", "Has CKD", "Has CLD", "Has CND",
              "Had Cancer Within 3 Years", "Immunosuppressed",
              "Has Sickle Cell Disease", "Smoking Status",
              "Hazardous Drinking", "Drug Usage"),
    count = c(1, age_groups, 2, 6, 5, 5, 5, 1, 4, 1, 1, 1, 1, 1, 3, 1, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1)
  )
  
  group_counts <- group_counts %>%
    filter(group %in% unique(df$characteristic)) %>%
    filter(group != "Household Composition") %>%
    group_by(group) %>%
    uncount(weights = 8) %>%
    mutate(season = c("2016_17", "2017_18", "2018_19", "2019_20",
                      "2020_21", "2021_22", "2022_23", "2023_24"))
  
  if (cohort != "infants" & cohort != "infants_subgroup") {
    
    group_counts_hh <- group_counts %>%
      filter(group == "Household Composition") %>%
      mutate(season = "2020_21") 
    
    group_counts <- bind_rows(group_counts, group_counts_hh)
    
  }
  
  df_groups <- uncount(group_counts, weights = count) %>%
    arrange(season)
  
  df <- df %>%
    filter(!is.na(percentage)) %>%
    cbind(df_groups)
  
  df <- df %>%
    mutate(
      percentage = substring(percentage, 1, nchar(percentage) - 1) %>%
        as.numeric(),
    )
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)", "2",
                "3", "4", "5 (most deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Maternal Age", "Never", "Former", "Current", "Unknown",
                "Maternal Drug Usage", "Maternal Drinking",
                "Maternal Flu Vaccine", "Maternal Pertussis Vaccine")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (least deprived)", "2", "3", "4",
                "5 (most deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Prior Flu Vaccine",
                "0-6m", "6-12m", "12m+")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("Has Asthma", "Has COPD", "Has Cystic Fibrosis",
                "Has Other Resp. Diseases", "Has Diabetes",
                "Has Addison's Disease", "Severely Obese", "Has CHD",
                "Has CKD", "Has CLD", "Has CND", "Had Cancer Within 3 Years",
                "Immunosuppressed", "Has Sickle Cell Disease",
                "Smoking Status", "Hazardous Drinking", "Drug Usage",
                "Never", "Current", "Former", "Unknown")
    
  } else {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (least deprived)",
                "2", "3", "4", "5 (most deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Prior Flu Vaccine", "0-6m", "6-12m", "12m+")
    
  }
  
  df <- df %>% 
    filter(group != "Total") %>%
    mutate(
      characteristic = forcats::fct_relevel(factor(characteristic),
                                            levels),
      subset = str_to_title(gsub("_", "-", subset))
    ) %>%
    mutate(
      across(characteristic, \(x) factor(
        x,
        levels = levels(x),
        labels = str_wrap(levels(x), width = 12)
      ))
    )
  
  all_groups <- unique(df$group)
  
  plot_list <- list()
  
  for (group in all_groups) {
    
    plot_list[[group]] <- df %>%
      filter(group == !!group) %>%
      ggplot(aes(fill = characteristic, y = percentage,
                 x = subset)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      theme_bw() + #theme(plot.title = element_text(size = 12)) +
      scale_fill_light(aesthetics = "fill") +
      labs(x = "Season", y = "Percentage (%)") + #, title = group) +
      guides(fill = guide_legend(
        title = group,
        theme = theme(legend.text = element_text(size = 8),
                      legend.key.height = unit(1, "cm")))) 
    
  }
  
  plot_title <- title <- ggdraw() + 
    draw_label(
      "Participant Characteristics",
      x = 0,
      hjust = 0
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 7)
    )
  
  plot_row <- plot_grid(plotlist = plot_list, ncol = length(all_groups))
  
  plot_grid(title, plot_row, ncol = 1, rel_heights = c(0.1, 1))
  
}
