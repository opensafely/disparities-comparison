library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(ggplot2)
library(zoo)
library(viridis)

#get study dates
source(here::here("analysis", "design", "design.R"))

#helper function to create and save plots
create_rolling_plots <- function(season, phenotype) {

  start <- year(study_dates[[paste0(season, "_start_date")]])
  end <- year(study_dates[[paste0(season, "_end_date")]])
  
  outcomes <- c(paste0(pathogen, "_primary_date"),
                paste0(pathogen, "_secondary_date"))
  
  df <- read_csv(here::here("output", "results", "rates",
                 "weekly", paste0("rates_over_time_", pathogen, "_",
                 cohort, "_", start, "_", end, "_", phenotype, "_",
                 investigation_type, ".csv"))) %>%
    mutate(
      group = case_when(
        group == "1 (least deprived)" ~ "5 (least deprived)",
        group == "2" ~ "4",
        group == "4" ~ "2",
        group == "5 (most deprived)" ~ "1 (most deprived)",
        TRUE ~ group
      )
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
    
  } else {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male", "White",
                "Mixed", "Asian or Asian British", "Black or Black British",
                "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2",
                "3", "4", "5 (least deprived)", "Multiple of the Same Generation",
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
        group %in% c("1 (most deprived)", "2", "3", "4",
                     "5 (least deprived)") ~ "IMD Quintile",
        group %in% c("Multiple of the Same Generation", "Living Alone",
                     "One Other Generation", "Two Other Generations",
                     "Three Other Generations") ~ "Household Composition",
        group %in% c("Urban Major Conurbation", "Urban Minor Conurbation",
                     "Urban City and Town", "Rural Town and Fringe",
                     "Rural Village and Dispersed") ~ "Rurality"
      )
    )
  
  characteristics <- unique(df$characteristic)
  
  plots <- list()
  
  for (characteristic in characteristics) {
    
    # col <- cols %>%
    #   filter(var == characteristic) %>%
    #   pull(col)
    # 
    # alpha_length <- df %>%
    #   filter(characteristic == !!characteristic)
    # alpha_length <- length(unique(alpha_length$group))
    
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
    
    alpha_type <- if_else(phenotype == "specific", 1, 0.5)
    
    plots[[characteristic]] <- df %>%
      filter(characteristic == !!characteristic) %>%
      ggplot(aes(x = interval_start, y = rate_1000_py_midpoint10_derived,
                 group = group, col = group)) +
      geom_line(stat = "identity", linewidth = 1, alpha = alpha_type) +
      scale_color_viridis_d() + theme_bw() +
      labs(x = paste0(start, "-", end), y = "", col = "Group") +
      facet_wrap( ~ outcome) + theme(legend.position = "none")
    
  }
  
  return(plots)
  
}

#helper function to create and save plots
create_rolling_plots_overall <- function(df) {
  
  scaleFUN <- function(x) sprintf("%.3f", signif(x, digits = 1))
  cols <- scales::seq_gradient_pal(
    "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))
  
  if (pathogen == "covid") cols <- cols[4:8]
  
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
  
  df <- df %>%
    mutate(
      codelist_type = factor(str_to_title(codelist_type),
                             levels = c("Specific", "Sensitive"))
    )
  
  df <- df %>%
    mutate(
      season_start = case_when(
        str_detect(subset, "_") ~ as.Date(
          paste0(substr(subset, 1, 4), "-09-01"))
      ),
      days_since_season_start = difftime(interval_start, season_start,
                                         units = "days") + 1,
      month_label = format(interval_start, "%b"),
      subset = gsub("_", "-", subset)
    )
  
  my_breaks <- function(x) {
    
    seq <- seq(0, max(x), length.out = 4)
    
    seq[1] <- 0
    
    return(seq)
    
  }
  
  get_consistent_limits <- function(x, bottom_pad_percent = 0.025,
                                    top_pad_percent = 0) {
    
    data_range <- max(x) - 0
    bottom_pad <- data_range * bottom_pad_percent
    top_pad <- data_range * top_pad_percent
    
    # Return the limits with consistent padding
    return(c(0 - bottom_pad, max(x) + top_pad))
    
  }
  
  if (pathogen != "overall_resp") {
    
    alph <- c(1, 0.5) 
    
  } else {
    
    alph <- 0.5
    
  }
  
  plot_mild <- df %>%
    filter(str_detect(outcome, "Mild")) %>%
    ggplot(aes(x = days_since_season_start,
               y = rate_1000_py_midpoint10_derived,
               col = subset, alpha = codelist_type)) +
    geom_line(stat = "identity", linewidth = 1) +  theme_bw() +
    scale_color_manual(values = cols) +
    scale_alpha_manual(values = alph) +
    labs(x = "", y = "", col = "Season", alpha = "Phenotype",
         subtitle = paste0("Mild ", pathogen_print)) +
    facet_wrap(~ codelist_type, ncol = 2, scales = "free") +
    scale_x_continuous(
      breaks = cumsum(c(0, 30, 31, 30, 31, 28, 31, 30, 31, 30, 31, 31)) - 15,
      labels = c("09", "10", "11", "12", "01", "02", "03",
                 "04", "05", "06", "07", "08")) +
    scale_y_continuous(labels = scaleFUN, breaks = my_breaks,
                       limits = get_consistent_limits) +
    theme(legend.position = "none",
          plot.subtitle = element_text(size = 12),
          axis.text = element_text(size = 12))
  
  plot_severe <- df %>%
    filter(str_detect(outcome, "Severe")) %>%
    ggplot(aes(x = days_since_season_start,
               y = rate_1000_py_midpoint10_derived,
               col = subset, alpha = codelist_type)) +
    geom_line(stat = "identity", linewidth = 1) +  theme_bw() +
    scale_color_manual(values = cols) +
    scale_alpha_manual(values = alph) +
    labs(x = "", y = "", col = "Season", alpha = "Phenotype",
         subtitle = paste0("Severe ", pathogen_print)) +
    facet_wrap(~ codelist_type, ncol = 2, scales = "free") +
    scale_x_continuous(
      breaks = cumsum(c(0, 30, 31, 30, 31, 28, 31, 30, 31, 30, 31, 31)) - 15,
      labels = c("09", "10", "11", "12", "01", "02", "03",
                 "04", "05", "06", "07", "08")) +
    scale_y_continuous(labels = scaleFUN, breaks = my_breaks,
                       limits = get_consistent_limits) +
    theme(legend.position = "none",
          plot.subtitle = element_text(size = 12),
          axis.text = element_text(size = 12))
  
  legend <- get_legend(plot_mild +
               theme(legend.position = "right",
                     legend.box = "verticle",
                     legend.key.size = unit(0.5, "cm"),
                     legend.text = element_text(size = 12),
                     legend.title = element_text(size = 12)))
  
  plot <- plot_grid(
    plot_mild, plot_severe,
    nrow = 1
  )
  
  if (pathogen == "overall_resp") {
    
    return(list(plot_mild = plot_mild, plot_severe = plot_severe,
                plot  = plot, legend = legend))
    
  } else {
  
    return(list(plot = plot, legend = legend))
    
  }
  
}
