library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(khroma)

#define a function to plot a characteristic over time
character_viz <- function(df, scaling) {
  
  names(df) <- c("characteristic", "count", "percentage", "subset")
  
  df <- df %>%
    mutate(
      characteristic = case_when(
        characteristic == "1 (least deprived)" ~ "5 (least deprived)",
        characteristic == "2" ~ "4",
        characteristic == "4" ~ "2",
        characteristic == "5 (most deprived)" ~ "1 (most deprived)",
        TRUE ~ characteristic
      )
    )
  
  age_groups <- case_when(
    cohort == "adults" ~ 2,
    cohort == "older_adults" ~ 3,
    TRUE ~ 4
  )
  
  # Define mapping of characteristic to number of categories
  group_counts <- tibble(
    group = c("Total", "Age Group", "Sex", "Ethnicity", "IMD",
              "Household Composition", "Rurality", 
              "Maternal Smoking Status", "Maternal Drinking",
              "Maternal Drug Usage", "Maternal Flu Vaccination",
              "Maternal Pertussis Vaccination", "Prior Flu Vaccine",
              "Time Since Last Covid Vaccine", "Smoking Status",
              "Hazardous Drinking", "Drug Usage", "Asthma", "COPD",
              "Cystic Fibrosis", "Other Chronic Respiratory Disease",
              "Diabetes", "Addisons", "Severe Obesity", "Chronic Heart Disease",
              "Chronic Kidney Disease", "Chronic Liver Disease",
              "Chronic Neurological Disease", "Cancer Within 3 Years",
              "Immunosuppressed", "Sickle Cell Disease"),
    count = c(1, age_groups, 2, 6, 5, 5, 5, 4, 1, 1, 1, 1, 1, 3, 4, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )
  
  if (investigation_type == "secondary") {
    
    group_counts2 <- group_counts %>%
      filter(group %in% unique(df$characteristic)) %>%
      group_by(group) %>%
      uncount(weights = 3) %>%
      mutate(season = c("2017_18", "2018_19", "2020_21"))
    
  } else if (investigation_type == "sensitivity") {
    
    group_counts2 <- group_counts %>%
      filter(group %in% unique(df$characteristic)) %>%
      group_by(group) %>%
      uncount(weights = 2) %>%
      mutate(season = c("2017_18", "2018_19"))  
    
  } else {
    
    group_counts2 <- group_counts %>%
      filter(group %in% unique(df$characteristic)) %>%
      filter(group != "Household Composition",
             group != "Time Since Last Covid Vaccine") %>%
      group_by(group) %>%
      uncount(weights = 8) %>%
      mutate(season = c("2016_17", "2017_18", "2018_19", "2019_20",
                        "2020_21", "2021_22", "2022_23", "2023_24"))
    
  }
  
  if (cohort != "infants" & cohort != "infants_subgroup" &
      investigation_type == "primary") {
    
    group_counts_hh <- group_counts %>%
      filter(group == "Household Composition") %>%
      mutate(season = "2020_21") 
    
    group_counts_covid <- group_counts %>%
      filter(group == "Time Since Last Covid Vaccine") %>%
      uncount(weights = 3) %>%
      mutate(season = c("2021_22", "2022_23", "2023_24"))
    
    group_counts <- bind_rows(group_counts2, group_counts_hh,
                              group_counts_covid)
    
    group_counts <- group_counts %>%
      arrange(season)
    
  } else {
    
    group_counts <- group_counts2
    
  }
  
  df_groups <- uncount(group_counts, weights = count) %>%
    arrange(season) %>%
    mutate(
      group = factor(group, levels = c(
        "Total", "Age Group", "Sex", "Ethnicity", "IMD",
        "Household Composition", "Rurality", 
        "Maternal Smoking Status", "Maternal Drinking",
        "Maternal Drug Usage", "Maternal Flu Vaccination",
        "Maternal Pertussis Vaccination", "Prior Flu Vaccine",
        "Time Since Last Covid Vaccine", "Smoking Status",
        "Hazardous Drinking", "Drug Usage", "Asthma", "COPD",
        "Cystic Fibrosis", "Other Chronic Respiratory Disease",
        "Diabetes", "Addisons", "Severe Obesity", "Chronic Heart Disease",
        "Chronic Kidney Disease", "Chronic Liver Disease",
        "Chronic Neurological Disease", "Cancer Within 3 Years",
        "Immunosuppressed", "Sickle Cell Disease"))
    ) %>%
    arrange(season, factor(group, levels = levels(group)))  
  
  df <- df %>%
    filter(!is.na(percentage)) %>%
    cbind(df_groups)
  
  df <- df %>%
    mutate(
      percentage = if_else(str_detect(percentage, "<0.1%"), "0.05%",
                           percentage)
    ) %>%
    mutate(
      percentage = substring(percentage, 1, nchar(percentage) - 1) %>%
        as.numeric(),
    )
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Yes", "Never", "Former", "Current")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (most deprived)", "2", "3", "4",
                "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Yes",
                "0-6m", "6-12m", "12m+")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Yes", "0-6m", "6-12m", "12m+")
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("Yes", "Never", "Current", "Former", "Unknown")
    
  } else {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
                "4", "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Yes", "0-6m", "6-12m", "12m+")
    
  }
  
  if (investigation_type == "primary") {
    
    cc <- scales::seq_gradient_pal(
      "#F05039", "#1F449c", "Lab")(seq(0, 1, length.out = 8))
    
  } else if (investigation_type == "secondary") {
    
    cc_full <- scales::seq_gradient_pal(
      "#F05039", "#1F449c", "Lab")(seq(0, 1, length.out = 8))
    
    cc <- c(cc_full[2], cc_full[3], cc_full[5])
    
  } else {
    
        cc_full <- scales::seq_gradient_pal(
      "#F05039", "#1F449c", "Lab")(seq(0, 1, length.out = 8))
    
    cc <- c(cc_full[2], cc_full[3], cc_full[5])
    
  }
  
  if (investigation_type == "secondary") {
    
    group_order <- c("Asthma", "COPD", "Cystic Fibrosis",
                     "Other Chronic Respiratory Disease", "Diabetes",
                     "Addisons", "Severe Obesity", "Chronic Heart Disease",
                     "Chronic Kidney Disease", "Chronic Liver Disease",
                     "Chronic Neurological Disease", "Cancer Within 3 Years",
                     "Immunosuppressed", "Sickle Cell Disease",
                     "Hazardous Drinking", "Drug Usage", "Smoking Status")
    
  } else {
    
    group_order <- c("Age Group", "Sex", "Ethnicity", "IMD",
                     "Household Composition", "Rurality", 
                     "Maternal Smoking Status", "Maternal Drinking",
                     "Maternal Drug Usage", "Maternal Flu Vaccination",
                     "Maternal Pertussis Vaccination", "Prior Flu Vaccine",
                     "Time Since Last Covid Vaccine")
    
  }
  
  if (scaling == "yes") {
    
    scales <- "free_x"
    
  } else {
    
    scales <- "free"
    
  }

  df %>%
    filter(group != "Total") %>%
    mutate(
      characteristic = if_else(
        group %in% c(
          "Age Group", "Sex", "Ethnicity", "IMD", "Household Composition",
          "Rurality", "Maternal Smoking Status",
          "Time Since Last Covid Vaccine", "Smoking Status"),
        characteristic, "Yes")
    ) %>%
    mutate(
      characteristic = factor(characteristic, levels = levels,
                              labels = str_wrap(levels, width = 12)),
      subset = str_to_title(gsub("_", "-", subset)),
      group = factor(group, levels = group_order, 
                     labels = str_wrap(group_order, width = 20))
    ) %>%
    ggplot(aes(fill = subset, y = percentage, x = characteristic)) +
    geom_bar(stat = "identity", position = "dodge", color = "white") +
    geom_hline(yintercept = 0.095, linetype = "dashed", color = "black",
               alpha = 0.5) +
    facet_wrap(~group, scales = scales, ncol = 3) +
    theme_bw() + scale_fill_manual(values = cc) +
    labs(title = "Participant Characteristics", x = "Characteristic",
         y = "Percentage (%)") +
    guides(fill = guide_legend(title = "Season")) +
    theme(text = element_text(size = 12))
  
}

#define a function to plot a characteristic over time - different formatting
character_viz_mult <- function(df, scaling) {
  
  names(df) <- c("characteristic", "count", "percentage", "subset")
  
  df <- df %>%
    mutate(
      characteristic = case_when(
        characteristic == "1 (least deprived)" ~ "5 (least deprived)",
        characteristic == "2" ~ "4",
        characteristic == "4" ~ "2",
        characteristic == "5 (most deprived)" ~ "1 (most deprived)",
        TRUE ~ characteristic
      )
    )
  
  age_groups <- case_when(
    cohort == "adults" ~ 2,
    cohort == "older_adults" ~ 3,
    TRUE ~ 4
  )
  
  # Define mapping of characteristic to number of categories
  group_counts <- tibble(
    group = c("Total", "Age Group", "Sex", "Ethnicity", "IMD",
              "Household Composition", "Rurality", 
              "Maternal Smoking Status", "Maternal Drinking",
              "Maternal Drug Usage", "Maternal Flu Vaccination",
              "Maternal Pertussis Vaccination", "Prior Flu Vaccine",
              "Time Since Last Covid Vaccine", "Smoking Status",
              "Hazardous Drinking", "Drug Usage", "Asthma", "COPD",
              "Cystic Fibrosis", "Other Chronic Respiratory Disease",
              "Diabetes", "Addisons", "Severe Obesity", "Chronic Heart Disease",
              "Chronic Kidney Disease", "Chronic Liver Disease",
              "Chronic Neurological Disease", "Cancer Within 3 Years",
              "Immunosuppressed", "Sickle Cell Disease"),
    count = c(1, age_groups, 2, 6, 5, 5, 5, 4, 1, 1, 1, 1, 1, 3, 4, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )
  
  if (investigation_type == "secondary") {
    
    group_counts2 <- group_counts %>%
      filter(group %in% unique(df$characteristic)) %>%
      group_by(group) %>%
      uncount(weights = 3) %>%
      mutate(season = c("2017_18", "2018_19", "2020_21"))
    
  } else if (investigation_type == "sensitivity") {
    
    group_counts2 <- group_counts %>%
      filter(group %in% unique(df$characteristic)) %>%
      group_by(group) %>%
      uncount(weights = 2) %>%
      mutate(season = c("2017_18", "2018_19"))  
    
  } else {
    
    group_counts2 <- group_counts %>%
      filter(group %in% unique(df$characteristic)) %>%
      filter(group != "Household Composition",
             group != "Time Since Last Covid Vaccine") %>%
      group_by(group) %>%
      uncount(weights = 8) %>%
      mutate(season = c("2016_17", "2017_18", "2018_19", "2019_20",
                        "2020_21", "2021_22", "2022_23", "2023_24"))
    
  }
  
  if (cohort != "infants" & cohort != "infants_subgroup" &
      investigation_type == "primary") {
    
    group_counts_hh <- group_counts %>%
      filter(group == "Household Composition") %>%
      mutate(season = "2020_21") 
    
    group_counts_covid <- group_counts %>%
      filter(group == "Time Since Last Covid Vaccine") %>%
      uncount(weights = 3) %>%
      mutate(season = c("2021_22", "2022_23", "2023_24"))
    
    group_counts <- bind_rows(group_counts2, group_counts_hh,
                              group_counts_covid)
    
    group_counts <- group_counts %>%
      arrange(season)
    
  } else {
    
    group_counts <- group_counts2
    
  }
  
  df_groups <- uncount(group_counts, weights = count) %>%
    arrange(season) %>%
    mutate(
      group = factor(group, levels = c(
        "Total", "Age Group", "Sex", "Ethnicity", "IMD",
        "Household Composition", "Rurality", 
        "Maternal Smoking Status", "Maternal Drinking",
        "Maternal Drug Usage", "Maternal Flu Vaccination",
        "Maternal Pertussis Vaccination", "Prior Flu Vaccine",
        "Time Since Last Covid Vaccine", "Smoking Status",
        "Hazardous Drinking", "Drug Usage", "Asthma", "COPD",
        "Cystic Fibrosis", "Other Chronic Respiratory Disease",
        "Diabetes", "Addisons", "Severe Obesity", "Chronic Heart Disease",
        "Chronic Kidney Disease", "Chronic Liver Disease",
        "Chronic Neurological Disease", "Cancer Within 3 Years",
        "Immunosuppressed", "Sickle Cell Disease"))
    ) %>%
    arrange(season, factor(group, levels = levels(group)))  
  
  df <- df %>%
    filter(!is.na(percentage)) %>%
    cbind(df_groups)
  
  df <- df %>%
    mutate(
      percentage = if_else(str_detect(percentage, "<0.1%"), "0.05%",
                           percentage)
    ) %>%
    mutate(
      percentage = substring(percentage, 1, nchar(percentage) - 1) %>%
        as.numeric()
    )
  
  #define levels
  levels <- list()
  
  if (cohort == "infants") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed")
    
  } else if (cohort == "infants_subgroup") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Yes", "Never", "Former", "Current")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female",
                "Male", "White", "Mixed", "Asian or Asian British",
                "Black or Black British", "Other Ethnic Groups",
                "Unknown", "1 (most deprived)", "2", "3", "4",
                "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation",
                "Two Other Generations", "Three Other Generations",
                "Urban Major Conurbation", "Urban Minor Conurbation",
                "Urban City and Town", "Rural Town and Fringe",
                "Rural Village and Dispersed", "Yes",
                "0-6m", "6-12m", "12m+")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)",
                "2", "3", "4", "5 (least deprived)",
                "Multiple of the Same Generation", "Living Alone",
                "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Yes", "0-6m", "6-12m", "12m+")
    
  } else if (cohort == "older_adults" & investigation_type == "secondary") {
    
    levels <- c("Yes", "Never", "Current", "Former", "Unknown")
    
  } else {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
                "4", "5 (least deprived)", "Multiple of the Same Generation",
                "Living Alone", "One Other Generation", "Two Other Generations",
                "Three Other Generations", "Urban Major Conurbation",
                "Urban Minor Conurbation", "Urban City and Town",
                "Rural Town and Fringe", "Rural Village and Dispersed",
                "Yes", "0-6m", "6-12m", "12m+")
    
  }
  
  if (investigation_type == "secondary") {
    
    group_order <- c("Asthma", "COPD", "Cystic Fibrosis",
                     "Other Chronic Respiratory Disease", "Diabetes",
                     "Addisons", "Severe Obesity", "Chronic Heart Disease",
                     "Chronic Kidney Disease", "Chronic Liver Disease",
                     "Chronic Neurological Disease", "Cancer Within 3 Years",
                     "Immunosuppressed", "Sickle Cell Disease",
                     "Hazardous Drinking", "Drug Usage", "Smoking Status")
    
  } else {
    
    group_order <- c("Age Group", "Sex", "Ethnicity", "IMD",
                     "Household Composition", "Rurality", 
                     "Maternal Smoking Status", "Maternal Drinking",
                     "Maternal Drug Usage", "Maternal Flu Vaccination",
                     "Maternal Pertussis Vaccination", "Prior Flu Vaccine",
                     "Time Since Last Covid Vaccine")
    
  }
  
  df <- df %>% 
    filter(group != "Total") %>%
    mutate(
      characteristic = if_else(
        group %in% c(
          "Age Group", "Sex", "Ethnicity", "IMD", "Household Composition",
          "Rurality", "Maternal Smoking Status",
          "Time Since Last Covid Vaccine", "Smoking Status"),
        characteristic, "Yes")
    ) %>%
    mutate(
      characteristic = factor(characteristic, levels = levels,
                              labels = str_wrap(levels, width = 12)),
      subset = str_to_title(gsub("_", "-", subset)),
      group = factor(group, levels = group_order)
    )
  
  all_groups <- group_order[group_order %in% levels(df$group)]
  
  if (cohort == "infants_subgroup") {
    
    all_groups <- all_groups[!all_groups %in% c(
      "Household Composition", "Prior Flu Vaccine",
      "Time Since Last Covid Vaccine")]
    
  } else if (cohort == "infants") {
    
    all_groups <- all_groups[all_groups %in% c(
      "Age Group", "Sex", "Ethnicity", "IMD", "Rurality"
    )]
    
  }
  
  
  plot_list <- list()
  
  cols2 <- tibble(
    var = c("Age Group", "Sex", "Ethnicity", "IMD",
            "Household Composition", "Rurality", 
            "Maternal Smoking Status", "Maternal Drinking",
            "Maternal Drug Usage", "Maternal Flu Vaccination",
            "Maternal Pertussis Vaccination",
            "Prior Flu Vaccine", "Time Since Last Covid Vaccine",
            "Asthma", "COPD", "Cystic Fibrosis",
            "Other Chronic Respiratory Disease", "Diabetes",
            "Addisons", "Severe Obesity", "Chronic Heart Disease",
            "Chronic Kidney Disease", "Chronic Liver Disease",
            "Chronic Neurological Disease", "Cancer Within 3 Years",
            "Immunosuppressed", "Sickle Cell Disease",
            "Hazardous Drinking", "Drug Usage", "Smoking Status"),
    col = c('#1f77b4', '#ffbb78', '#2ca02c', '#ff9896',
            '#aec7e8', '#ff7f0e',
            '#c49c94', '#e377c2',
            '#c5b0d5', '#8c564b',
            '#f7b6d2',
            '#98df8a', '#98df8a',
            '#d177f4', '#7b98f4', '#37aabe',
            '#bcbd22', '#c5b0d5',
            '#dbdb8d', '#17becf', '#9edae5',
            '#b884f4', '#ec62f4',
            '#38a8cb', '#9d8ff4',
            '#98df8a', '#43a1f4',
            '#8c564b', '#e377c2', '#9467bd')
    )
  
  if (scaling == "yes") {

    lower <- min(df$percentage)
    upper <- max(df$percentage)

  } else {

    lower <- NA
    upper <- NA

  }
  
  for (group in all_groups) {
    
    fill_col <- cols2 %>%
      filter(var == !!group) %>%
      pull(col)
    
    alpha_length <- df %>%
      filter(group == !!group)
    alpha_length <- length(unique(alpha_length$characteristic))
    
    plot_list[[group]] <- df %>%
      filter(group == !!group) %>%
      ggplot(aes(alpha = characteristic, y = percentage,
                 x = subset)) + coord_cartesian(ylim = c(lower, upper)) +
      geom_bar(stat = "identity", position = "dodge",
               color = "white", fill = fill_col) +
      geom_hline(yintercept = 0.095, linetype = "dashed", color = "black",
                 alpha = 0.5) +
      theme_bw() + scale_alpha_manual(
        values = c(seq(1, 0.25, length.out = alpha_length))) +
      labs(x = "Season", y = "Percentage (%)") + 
      guides(alpha = guide_legend(
               title = str_wrap(group, width = 10))) +
      theme(text = element_text(size = 12))
    
  }
  
  if (investigation_type == "secondary") {
    
    plot_title <- ggdraw() + 
        draw_label(
          "Participant Characteristics by Season",
          x = 0,
          hjust = 0
        ) + theme_bw() +
        theme(
          plot.margin = margin(0, 0, 0, 7),
          panel.border = element_blank(),
        )
    
    title1 <- "Participant Characteristics by Season (Panel A)"
    title2 <- "Participant Characteristics by Season (Panel B)"
    title3 <- "Participant Characteristics by Season (Panel C)"
    
    plot_row1 <- plot_grid(plotlist = plot_list[1:6], nrow = 2)
    plot_row2 <- plot_grid(plotlist = plot_list[7:12], nrow = 2)
    plot_row3 <- plot_grid(plotlist = plot_list[13:17], nrow = 2)
    
    one <- plot_grid(plot_title, plot_row1, ncol = 1,
                     rel_heights = c(0.1, 1))
    two <- plot_grid(plot_title, plot_row2, ncol = 1,
                     rel_heights = c(0.1, 1))
    three <- plot_grid(plot_title, plot_row3, ncol = 1,
                       rel_heights = c(0.1, 1))
    
    return(list(one, two, three, title1, title2, title3))
    
  } else if (investigation_type == "primary" & cohort == "infants_subgroup") {
    
    plot_title <- ggdraw() + 
      draw_label(
        "Participant Characteristics by Season",
        x = 0,
        hjust = 0
      ) + theme_bw() +
      theme(
        plot.margin = margin(0, 0, 0, 7),
        panel.border = element_blank(),
      )
    
    title1 <- "Participant Characteristics by Season (Panel A)"
    title2 <- "Participant Characteristics by Season (Panel B)"
    title3 <- "Participant Characteristics by Season (Panel C)"
    
    plot_row1 <- plot_grid(plotlist = plot_list[1:4], nrow = 2)
    plot_row2 <- plot_grid(plotlist = plot_list[5:8], nrow = 2)
    plot_row3 <- plot_grid(plotlist = plot_list[9:10], nrow = 2)
    
    one <- plot_grid(plot_title, plot_row1, ncol = 1,
                     rel_heights = c(0.1, 1))
    two <- plot_grid(plot_title, plot_row2, ncol = 1,
                     rel_heights = c(0.1, 1))
    three <- plot_grid(plot_title, plot_row3, ncol = 1,
                       rel_heights = c(0.1, 1))
    
    return(list(one, two, three, title1, title2, title3))
    
  } else {
    
    plot_title <- ggdraw() + 
      draw_label(
        "Participant Characteristics by Season",
        x = 0,
        hjust = 0
      ) +
      theme(
        plot.margin = margin(0, 0, 0, 7)
      )
    
    title1 <- "Participant Characteristics by Season (Panel A)"
    title2 <- "Participant Characteristics by Season (Panel B)"
    
    plot_row1 <- plot_grid(plotlist = plot_list[1:3], nrow = 2)
    plot_row2 <- plot_grid(plotlist = plot_list[4:6], nrow = 2)
    
    one <- plot_grid(plot_title, plot_row1, ncol = 1, rel_heights = c(0.1, 1))
    two <- plot_grid(plot_title, plot_row2, ncol = 1, rel_heights = c(0.1, 1))
    
    return(list(one, two, title1, title2))
    
  }
  
}
