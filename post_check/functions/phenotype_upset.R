library(UpSetR)
library(tidyverse)
library(here)
library(ggplot2)
library(grid)
library(gridExtra)
library(stringr)
library(data.table)
library(RColorBrewer)
library(tibble)
library(ggpubr)

#define function to create upset plot of multiple infection outcomes
upset_plot <- function(input, seasons) {
  
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
      combo = case_when(
        combo == "RSV & COVID" ~ "RSV&COVID",
        combo == "RSV & Flu" ~ "RSV&Flu",
        combo == "Flu & COVID" ~ "Flu&COVID",
        combo == "RSV & Flu & COVID" ~ "RSV&Flu&COVID",
        TRUE ~ combo
      )
    )
  
  outcomes <- c("Mild", "Severe")
  codelists <- c("Specific", "Sensitive")
  subsets <- unique(df_plot$subset)
  
  plots <- list()
  
  for (outcome in outcomes) {
    
    plot_list <- list()
    
    for (season in subsets) {
      
      plot <- list()
      
      for (phenotype in codelists) {
        
        input2 <- as.data.table(df_plot %>%
          filter(outcome_type == outcome, subset == season,
                 codelist_type == phenotype) %>%
          select(combo, n))
        input_expr <- tibble::deframe(input2)
        
        col <- if_else(phenotype == "Specific", "#1E88E5", "#D81B60")
        f <- function(pal) brewer.pal(3, pal)
        cols <- f("Set2")
        
        uu <- upset(fromExpression(input_expr),
                    nsets = 3,
                    keep.order = T,
                    mb.ratio = c(0.6, 0.4),
                    number.angles = 0,
                    text.scale = 1.1,
                    point.size = 2.8,
                    line.size = 1,
                    main.bar.color = col,
                    sets.bar.color = cols,
                    sets = c("RSV", "Flu", "COVID")
        )
        
        plot[[phenotype]] <- plot_grid(
          NULL,
          plot_grid(NULL, uu$Sizes, nrow = 2, rel_heights = c(0.75, 0.25)),
          NULL,
          plot_grid(uu$Main_bar, uu$Matrix, NULL, nrow = 3,
                    rel_heights = c(3, 1, 0.45), align = 'hv'),
          ncol = 4, align = 'hv', rel_widths = c(0.01, 0.25, 0.05, 0.75)
        )
        
      }
      
      plot_list[[season]] <- plot_grid(
        plot[["Specific"]],
        plot[["Sensitive"]]
      )
      
    }
    
    plot_label <- ggdraw() +
      draw_label(
        "Specific Phenotype",
        x = 0.5, y = 0, hjust = 1.8, vjust = -2.5,
        fontface = 'bold', size = 14
      ) +
      draw_label(
        "Sensitive Phenotype",
        x = 1, y = 0, hjust = 1.75, vjust = -2.5,
        fontface = 'bold', size = 14
      ) +
      theme(plot.background = element_rect(
        fill = "white", colour = "white"))
    
    plots[[outcome]] <- plot_grid(plot_grid(
      plotlist = plot_list,
      ncol = 1, align = "v",
      axis = "tb",
      labels = gsub("_", "-", seasons),
      label_size = 12),
      plot_label,
      nrow = 2, rel_heights = c(0.95, 0.05)
    ) %>%
    annotate_figure(
      top = text_grob(
        paste0("Identification of ", outcome, " Outcomes in ",
               str_to_title(gsub("_", " ", cohort)), " by Season of Interest"),
        color = "black", face = "bold", size = 16),
    )
    
  }
  
  return(plots)
  
}
