library(tidyverse)
library(ggplot2)
library(scales)
library(stringr)
library(ggeasy)
library(forcats)
library(VennDiagram)
library(ggpubr)
library(cowplot)

#define function to create a bar chart of multiple infection outcomes
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

#define function to create a venn diagram of multiple infection outcomes
venn_plot <- function(input, seasons) {
  
  filtered_input_all <- input %>%
    filter(subset %in% seasons & outcome_type %in% c("mild", "severe")) %>%
    mutate(n = `n (midpoint 10 rounded)`)
  
  # Helper function to safely extract count or return 0 if not found
  safe_pull <- function(df, codelist, combo_spec) {
    
    count <- df %>% 
      filter(codelist_type == codelist) %>%
      filter(combo == !!combo_spec) %>% select(n) %>% pull()
    if (length(count) == 0) return(0) else return(as.numeric(count))
    if (is.na(count)) return(0) else return(as.numeric(count))
    
  }
  
  # Initialize empty list to collect results
  plot_list <- list()
  
  # Loop over each season and outcome
  for (season in seasons) {
    
    # Filter input data by season
    filtered_input <- filtered_input_all %>% filter(subset == season)
    
    if (season %in% c("2019_20", "2020_21", "2021_22", "2022_23", 
                      "2023_24")) {
      
      rsv_spec_total_mild <- safe_pull(
        filtered_input, "specific", "RSV_Mild_Total")
      flu_spec_total_mild <- safe_pull(
        filtered_input, "specific", "Flu_Mild_Total")
      covid_spec_total_mild <- safe_pull(
        filtered_input, "specific", "COVID_Mild_Total")
      
      rsv_sens_total_mild <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_Total")
      flu_sens_total_mild <- safe_pull(
        filtered_input, "sensitive", "Flu_Mild_Total")
      covid_sens_total_mild <- safe_pull(
        filtered_input, "sensitive", "COVID_Mild_Total")
      
      rsv_spec_total_severe <- safe_pull(
        filtered_input, "specific", "RSV_Severe_Total")
      flu_spec_total_severe <- safe_pull(
        filtered_input, "specific", "Flu_Severe_Total")
      covid_spec_total_severe <- safe_pull(
        filtered_input, "specific", "COVID_Severe_Total")
      
      rsv_sens_total_severe <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_Total")
      flu_sens_total_severe <- safe_pull(
        filtered_input, "sensitive", "Flu_Severe_Total")
      covid_sens_total_severe <- safe_pull(
        filtered_input, "sensitive", "COVID_Severe_Total")
      
      rsv_spec_mild <- safe_pull(
        filtered_input, "specific", "RSV_Mild_0_0")
      flu_spec_mild <- safe_pull(
        filtered_input, "specific", "0_Flu_Mild_0")
      covid_spec_mild <- safe_pull(
        filtered_input, "specific", "0_0_COVID_Mild")
      rsv_flu_spec_mild <- safe_pull(
        filtered_input, "specific", "RSV_Mild_Flu_Mild_0")
      rsv_covid_spec_mild <- safe_pull(
        filtered_input, "specific", "RSV_Mild_0_COVID_Mild")
      flu_covid_spec_mild <- safe_pull(
        filtered_input, "specific", "0_Flu_Mild_COVID_Mild")
      rsv_flu_covid_spec_mild <- safe_pull(
        filtered_input, "specific", "RSV_Mild_Flu_Mild_COVID_Mild")
      
      rsv_sens_mild <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_0_0")
      flu_sens_mild <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Mild_0")
      covid_sens_mild <- safe_pull(
        filtered_input, "sensitive", "0_0_COVID_Mild")
      rsv_flu_sens_mild <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_Flu_Mild_0")
      rsv_covid_sens_mild <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_0_COVID_Mild")
      flu_covid_sens_mild <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Mild_COVID_Mild")
      rsv_flu_covid_sens_mild <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_Flu_Mild_COVID_Mild")
      
      rsv_spec_severe <- safe_pull(
        filtered_input, "specific", "RSV_Severe_0_0")
      flu_spec_severe <- safe_pull(
        filtered_input, "specific", "0_Flu_Severe_0")
      covid_spec_severe <- safe_pull(
        filtered_input, "specific", "0_0_COVID_Severe")
      rsv_flu_spec_severe <- safe_pull(
        filtered_input, "specific", "RSV_Severe_Flu_Severe_0")
      rsv_covid_spec_severe <- safe_pull(
        filtered_input, "specific", "RSV_Severe_0_COVID_Severe")
      flu_covid_spec_severe <- safe_pull(
        filtered_input, "specific", "0_Flu_Severe_COVID_Severe")
      rsv_flu_covid_spec_severe <- safe_pull(
        filtered_input, "specific", "RSV_Severe_Flu_Severe_COVID_Severe")
      
      rsv_sens_severe <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_0_0")
      flu_sens_severe <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Severe_0")
      covid_sens_severe <- safe_pull(
        filtered_input, "sensitive", "0_0_COVID_Severe")
      rsv_flu_sens_severe <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_Flu_Severe_0")
      rsv_covid_sens_severe <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_0_COVID_Severe")
      flu_covid_sens_severe <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Severe_COVID_Severe")
      rsv_flu_covid_sens_severe <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_Flu_Severe_COVID_Severe")
      
      diag_spec_mild <- draw.triple.venn(
        area1 = rsv_spec_total_mild,
        area2 = flu_spec_total_mild,
        area3 = covid_spec_total_mild, 
        n12 = rsv_flu_spec_mild + rsv_flu_covid_spec_mild,
        n13 = rsv_covid_spec_mild + rsv_flu_covid_spec_mild,
        n23 = flu_covid_spec_mild + rsv_flu_covid_spec_mild,
        n123 = rsv_flu_covid_spec_mild,
        category = c("RSV", "Influenza", "COVID-19"),
        fill = "#1E88E5",
        alpha = 0.5,
        cat.fontfamily = "sans",
        cat.cex = 1,
        cat.dist = 0.035,
        ext.text = TRUE,
        euler.d = FALSE,
        scaled = FALSE,
        fontfamily = "sans",
        cex = 1,
        print.mode = "raw",
        ind = FALSE,
        sigdigs = 2
      )
      
      diag_sens_mild <- draw.triple.venn(
        area1 = rsv_sens_total_mild,
        area2 = flu_sens_total_mild,
        area3 = covid_sens_total_mild, 
        n12 = rsv_flu_sens_mild + rsv_flu_covid_sens_mild,
        n13 = rsv_covid_sens_mild + rsv_flu_covid_sens_mild,
        n23 = flu_covid_sens_mild + rsv_flu_covid_sens_mild,
        n123 = rsv_flu_covid_sens_mild,
        category = c("RSV", "Influenza", "COVID-19"),
        fill = "#D81B60",
        alpha = 0.5,
        cat.fontfamily = "sans",
        cat.cex = 1,
        cat.dist = 0.035,
        ext.text = TRUE,
        euler.d = FALSE,
        scaled = FALSE,
        fontfamily = "sans",
        cex = 1,
        print.mode = "raw",
        ind = FALSE,
        sigdigs = 2
      )
      
      diag_spec_severe <- draw.triple.venn(
        area1 = rsv_spec_total_severe,
        area2 = flu_spec_total_severe,
        area3 = covid_spec_total_severe, 
        n12 = rsv_flu_spec_severe + rsv_flu_covid_spec_severe,
        n13 = rsv_covid_spec_severe + rsv_flu_covid_spec_severe,
        n23 = flu_covid_spec_severe + rsv_flu_covid_spec_severe,
        n123 = rsv_flu_covid_spec_severe,
        category = c("RSV", "Influenza", "COVID-19"),
        fill = "#1E88E5",
        alpha = 0.5,
        cat.fontfamily = "sans",
        cat.cex = 1,
        cat.dist = 0.035,
        ext.text = TRUE,
        euler.d = FALSE,
        scaled = FALSE,
        fontfamily = "sans",
        cex = 1,
        print.mode = "raw",
        ind = FALSE,
        sigdigs = 2
      )
      
      diag_sens_severe <- draw.triple.venn(
        area1 = rsv_sens_total_severe,
        area2 = flu_sens_total_severe,
        area3 = covid_sens_total_severe, 
        n12 = rsv_flu_sens_severe + rsv_flu_covid_sens_severe,
        n13 = rsv_covid_sens_severe + rsv_flu_covid_sens_severe,
        n23 = flu_covid_sens_severe + rsv_flu_covid_sens_severe,
        n123 = rsv_flu_covid_sens_severe,
        category = c("RSV", "Influenza", "COVID-19"),
        fill = "#D81B60",
        alpha = 0.5,
        cat.fontfamily = "sans",
        cat.cex = 1,
        cat.dist = 0.035,
        ext.text = TRUE,
        euler.d = FALSE,
        scaled = FALSE,
        fontfamily = "sans",
        cex = 1,
        print.mode = "raw",
        ind = FALSE,
        sigdigs = 2
      )
      
    } else {
      
      rsv_spec_total_mild <- safe_pull(
        filtered_input, "specific", "RSV_Mild_Total")
      flu_spec_total_mild <- safe_pull(
        filtered_input, "specific", "Flu_Mild_Total")
      
      rsv_sens_total_mild <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_Total")
      flu_sens_total_mild <- safe_pull(
        filtered_input, "sensitive", "Flu_Mild_Total")
      
      rsv_spec_total_severe <- safe_pull(
        filtered_input, "specific", "RSV_Severe_Total")
      flu_spec_total_severe <- safe_pull(
        filtered_input, "specific", "Flu_Severe_Total")
      
      rsv_sens_total_severe <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_Total")
      flu_sens_total_severe <- safe_pull(
        filtered_input, "sensitive", "Flu_Severe_Total")
      
      rsv_spec_mild <- safe_pull(
        filtered_input, "specific", "RSV_Mild_0")
      flu_spec_mild <- safe_pull(
        filtered_input, "specific", "0_Flu_Mild")
      rsv_flu_spec_mild <- safe_pull(
        filtered_input, "specific", "RSV_Mild_Flu_Mild")
      
      rsv_sens_mild <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_0")
      flu_sens_mild <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Mild")
      rsv_flu_sens_mild <- safe_pull(
        filtered_input, "sensitive", "RSV_Mild_Flu_Mild")
      
      rsv_spec_severe <- safe_pull(
        filtered_input, "specific", "RSV_Severe_0")
      flu_spec_severe <- safe_pull(
        filtered_input, "specific", "0_Flu_Severe")
      rsv_flu_spec_severe <- safe_pull(
        filtered_input, "specific", "RSV_Severe_Flu_Severe")
      
      rsv_sens_severe <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_0")
      flu_sens_severe <- safe_pull(
        filtered_input, "sensitive", "0_Flu_Severe")
      rsv_flu_sens_severe <- safe_pull(
        filtered_input, "sensitive", "RSV_Severe_Flu_Severe")
      
      diag_spec_mild <- draw.pairwise.venn(
        area1 = rsv_spec_total_mild,
        area2 = flu_spec_total_mild,
        cross.area = rsv_flu_spec_mild,
        category = c("RSV", "Influenza"),
        fill = "#1E88E5",
        alpha = 0.5,
        fontfamily = "sans",
        cat.fontfamily = "sans",
        cat.cex = 1,
        cat.dist = 0.035,
        ext.text = TRUE,
        euler.d = FALSE,
        scaled = FALSE,
        inverted = if_else(rsv_spec_total_mild > flu_spec_total_mild,
                           FALSE, TRUE),
        cex = 1,
        print.mode = "raw",
        ind = FALSE,
        sigdigs = 2
      )
      
      diag_sens_mild <- draw.pairwise.venn(
        area1 = rsv_sens_total_mild,
        area2 = flu_sens_total_mild,
        cross.area = rsv_flu_sens_mild,
        category = c("RSV", "Influenza"),
        fill = "#D81B60",
        alpha = 0.5,
        fontfamily = "sans",
        cat.fontfamily = "sans",
        cat.cex = 1,
        cat.dist = 0.035,
        ext.text = TRUE,
        euler.d = FALSE,
        scaled = FALSE,
        inverted = if_else(rsv_sens_total_mild > flu_sens_total_mild,
                           FALSE, TRUE),
        cex = 1,
        print.mode = "raw",
        ind = FALSE,
        sigdigs = 2
      )
      
      diag_spec_severe <- draw.pairwise.venn(
        area1 = rsv_spec_total_severe,
        area2 = flu_spec_total_severe,
        cross.area = rsv_flu_spec_severe,
        category = c("RSV", "Influenza"),
        fill = "#1E88E5",
        alpha = 0.5,
        fontfamily = "sans",
        cat.fontfamily = "sans",
        cat.cex = 1,
        cat.dist = 0.035,
        ext.text = TRUE,
        euler.d = FALSE,
        scaled = FALSE,
        inverted = if_else(rsv_spec_total_severe > flu_spec_total_severe,
                           FALSE, TRUE),
        cex = 1,
        print.mode = "raw",
        ind = FALSE,
        sigdigs = 2
      )
      
      diag_sens_severe <- draw.pairwise.venn(
        area1 = rsv_sens_total_severe,
        area2 = flu_sens_total_severe,
        cross.area = rsv_flu_sens_severe,
        category = c("RSV", "Influenza"),
        fill = "#D81B60",
        alpha = 0.5,
        fontfamily = "sans",
        cat.fontfamily = "sans",
        cat.cex = 1,
        cat.dist = 0.035,
        ext.text = TRUE,
        euler.d = FALSE,
        scaled = FALSE,
        inverted = if_else(rsv_sens_total_severe > flu_sens_total_severe,
                           FALSE, TRUE),
        cex = 1,
        print.mode = "raw",
        ind = FALSE,
        sigdigs = 2
      )
      
    }
    
    # Create a dummy dataset
    legend_data <- data.frame(
      phenotype = factor(c("Specific", "Sensitive"),
                         levels = c("Specific", "Sensitive")),
      x = 1:2,
      y = 1:2
    )
    
    # Create ggplot object just for the legend
    legend_plot <- ggplot(legend_data, aes(x = x, y = y, fill = phenotype)) +
      geom_tile(alpha = 0.5) +
      scale_fill_manual(
        name = "Phenotype",
        values = c("Specific" = "#1E88E5", "Sensitive" = "#D81B60")
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12)
      )
    
    # Extract just the legend
    gg_legend <- cowplot::get_legend(legend_plot)
    
    diag2 <- grid.arrange(gTree(children = diag_spec_mild),
                          ggplot() + theme_void(),
                          gTree(children = diag_sens_mild),
                          gTree(children = diag_spec_severe),
                          ggplot() + theme_void(),
                          gTree(children = diag_sens_severe),
                          ncol = 3, nrow = 2, widths = c(1, 0.05, 1),
                          left = text_grob(
                            c("Mild", "Severe"),
                            x = 0.2, hjust = 0.1, vjust = 0,
                            y = c(0.75, 0.25), size = 14,
                            just = "left", face = "bold"))
    
    diag <- grid.arrange(diag2, gg_legend,
                         ncol = 2, widths = c(0.9, 0.1),
                         top = text_grob(
                           "Pathogens Identified Within One Episode (Patient Level)",
                           size = 14, face = "bold"),
                         bottom = text_grob(
                           paste0("Season: ", gsub("_", "-", season)),
                           size = 14, face = "bold", hjust = 0.6,
                           vjust = 0.2), padding = unit(1, "line"))
    
    ggsave(diag, filename = paste0(
      here::here("post_check", "plots", "exploratory_analyses", "condensed"),
      "/", cohort, "_phenotype_sensitivity_venn_", season, ".png"),
      height = 12, width = 15, device = "png")
    
  }
  
}
