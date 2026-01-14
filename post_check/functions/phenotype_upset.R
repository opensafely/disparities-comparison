library(UpSetR)
library(tidyverse)
library(ggplot2)
library(grid)
library(stringr)
library(data.table)
library(RColorBrewer)
library(tibble)
library(scales)
library(forcats)
library(cowplot)
library(ingrid)

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
      combo = factor(case_when(
        combo == "RSV & COVID" ~ "RSV&COVID",
        combo == "RSV & Flu" ~ "RSV&Flu",
        combo == "Flu & COVID" ~ "Flu&COVID",
        combo == "RSV & Flu & COVID" ~ "RSV&Flu&COVID",
        TRUE ~ as.character(combo)
      ), levels = c("RSV", "Flu", "COVID", "RSV&COVID", "RSV&Flu",
                    "Flu&COVID", "RSV&Flu&COVID"))
    )
  
  df_plot <- df_plot %>%
    mutate(combo = gsub("COVID", "COVID-19",
                        gsub("Flu", "Influenza", combo)))
  
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
        
        col <- if_else(phenotype == "Specific", "#71797E", "#71797E")
        f <- function(pal) brewer.pal(3, pal)
        cols <- f("Set2")
        
        uu <- upset(fromExpression(input_expr),
                    nsets = 3,
                    keep.order = T,
                    order.by = "degree",
                    decreasing = FALSE,
                    mb.ratio = c(0.8, 0.2),
                    text.scale = c(1.25, 1.25, 1.25, 1.25, 1.25, 1.25),
                    point.size = 2,
                    line.size = 1,
                    mainbar.y.label = "Intersection Cases",
                    # set_size.show = FALSE,
                    # set_size.angles = 45,
                    # scale.sets = "log10",
                    empty.intersections = TRUE,
                    # main.bar.color = col,
                    sets.bar.color = cols,
                    # matrix.color = cols,
                    sets = c("COVID-19", "Influenza", "RSV")
        )

        sizes_data <- data.frame(
          set = names(input_expr),
          size = as.numeric(input_expr)
        )
        sizes_data <- sizes_data %>% 
          mutate(
            size = case_when(
              str_detect(set, "RSV") ~ sum(
                sizes_data %>% filter(str_detect(set, "RSV")) %>% pull(size)
              ),
              str_detect(set, "Influenza") ~ sum(
                sizes_data %>% filter(str_detect(set, "Influenza")) %>% pull(size)
              ),
              str_detect(set, "COVID-19") ~ sum(
                sizes_data %>% filter(str_detect(set, "COVID-19")) %>% pull(size)
              )
            )
          ) %>% 
          filter(set %in% c("RSV", "Influenza", "COVID-19")) %>% 
          mutate(
            set = factor(set, levels = c("RSV", "Influenza", "COVID-19"))
          )
        
        cols_dat <- tibble(
          set = c("RSV", "Influenza", "COVID-19"),
          cols = cols
        )

        sizes_data <- merge(sizes_data, cols_dat, by = "set")

        sizes_plot <- ggplot(sizes_data, aes(x = set, y = size)) +
          geom_col(aes(fill = set), show.legend = TRUE) +
          scale_fill_manual(values = cols,
            labels = c("RSV", "Influenza", "COVID-19"),
            name = "Virus"
          ) +
          ggrepel::geom_text_repel(aes(label = size), size = 3, direction = "y") +
          labs(x = NULL) +
          scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.1))) +
          #expand_limits(x = c(-0.1, 4)) +
          theme_bw() + theme(
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.direction = "horizontal",
            legend.position = "bottom",
            legend.title = element_text(size = 14)
          ) +
          guides(fill = guide_legend(label.position = "left"))
        
        legend <- get_legend(sizes_plot)

        plot[[phenotype]] <- plot_grid(
          plot_grid(sizes_plot + theme(legend.position = "none"),
                    NULL, nrow = 2, rel_heights = c(0.95, 0.05)),
          plot_grid(uu$Matrix, uu$Main_bar, NULL, nrow = 3,
                    rel_heights = c(1, 2.75, 0.05), align = 'hv'),
          ncol = 2, align = 'hv', rel_widths = c(0.25, 0.75)
        )
        
      }
      
      plot_list[[season]] <- plot_grid(
        plot[["Specific"]],
        NULL,
        plot[["Sensitive"]],
        ncol = 3, rel_widths = c(0.8, 0.01, 0.8)
      )
      
    }
    
    plot_label <- ggdraw() +
      draw_label(
        "Specific Phenotype",
        x = 0.5, y = 0, hjust = 1.35, vjust = 0.5,
        fontface = 'bold', size = 14# color = "#71797E"
      ) +
      draw_label(
        "Sensitive Phenotype",
        x = 1, y = 0, hjust = 1.55, vjust = 0.5,
        fontface = 'bold', size = 14#, color = "#71797E"
      ) +
      theme(plot.background = element_rect(
        fill = "white", colour = "white"))
    
    other_plot_label <- ggdraw() +
      draw_label(
        "Total Cases",
        x = 1, y = 0, hjust = 12.68, vjust = -1.2, size = 12
      ) +
      draw_label(
        "Total Cases",
        x = 1, y = 0, hjust = 5.9, vjust = -1.2, size = 12
      ) +
      theme(plot.background = element_rect(
        fill = "white", colour = "white"))
    
    plots[[outcome]] <- plot_grid(
      other_plot_label,
      plot_grid(
        plotlist = plot_list,
        ncol = 1, align = "v",
        axis = "tb",
        labels = gsub("_", "-", seasons),
        vjust = 0.2,
        hjust = 0.5,
        label_size = 12
      ), ncol = 1, rel_heights = c(0.01, 0.99)
    )
    
  }
  
  plot_final2 <- plot_grid(
    NULL,
    plot_grid(NULL, plots[["Mild"]], ncol = 2, rel_widths = c(0.05, 1)),
    NULL,
    plot_grid(NULL, plots[["Severe"]], ncol = 2, rel_widths = c(0.05, 1)),
    nrow = 4,
    rel_heights = c(0.035, 0.5, 0.02, 0.5),
    labels = c("", "A. Mild", "", "B. Severe"),
    label_size = 14,
    label_fontface = "bold",
    hjust = c(0, -0.1, 0, -0.1), 
    vjust = -2
  )
  plot_final <- plot_grid(
    legend,
    plot_label,
    plot_final2,
    nrow = 3, rel_heights = c(0.025, 0.01, 0.925)
  ) #%>%
    # annotate_figure(
    #   top = text_grob(
    #     paste0("Identification of Outcomes in ",
    #            str_to_title(gsub("_", " ", cohort)),
    #            " by Season of Interest"),
    #     color = "black", face = "bold", size = 16, 
    #     vjust = 1.25)
    # )
  
  return(plot_final)
  
}

#define function to create upset plot of multiple infection outcomes
upset_plot_supplement <- function(input, seasons) {
  
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
      combo = factor(case_when(
        combo == "RSV & COVID" ~ "RSV&COVID",
        combo == "RSV & Flu" ~ "RSV&Flu",
        combo == "Flu & COVID" ~ "Flu&COVID",
        combo == "RSV & Flu & COVID" ~ "RSV&Flu&COVID",
        TRUE ~ as.character(combo)
      ), levels = c("RSV", "Flu", "COVID", "RSV&COVID", "RSV&Flu",
                    "Flu&COVID", "RSV&Flu&COVID"))
    )
  
  df_plot <- df_plot %>%
    mutate(combo = gsub("COVID", "COVID-19",
                        gsub("Flu", "Influenza", combo)))
  
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
        
        col <- if_else(phenotype == "Specific", "#71797E", "#71797E")
        f <- function(pal) brewer.pal(3, pal)
        cols <- f("Set2")
        
        uu <- upset(fromExpression(input_expr),
                    nsets = 3,
                    keep.order = T,
                    order.by = "degree",
                    decreasing = FALSE,
                    mb.ratio = c(0.8, 0.2),
                    text.scale = c(1.25, 1.25, 1.25, 1.25, 1.25, 1.25),
                    point.size = 2,
                    line.size = 1,
                    mainbar.y.label = "Intersection Cases",
                    # set_size.show = FALSE,
                    # set_size.angles = 45,
                    # scale.sets = "log10",
                    empty.intersections = TRUE,
                    # main.bar.color = col,
                    sets.bar.color = cols,
                    # matrix.color = cols,
                    sets = c("COVID-19", "Influenza", "RSV")
        )

        sizes_data <- data.frame(
          set = names(input_expr),
          size = as.numeric(input_expr)
        )
        sizes_data <- sizes_data %>% 
          mutate(
            size = case_when(
              str_detect(set, "RSV") ~ sum(
                sizes_data %>% filter(str_detect(set, "RSV")) %>% pull(size)
              ),
              str_detect(set, "Influenza") ~ sum(
                sizes_data %>% filter(str_detect(set, "Influenza")) %>% pull(size)
              ),
              str_detect(set, "COVID-19") ~ sum(
                sizes_data %>% filter(str_detect(set, "COVID-19")) %>% pull(size)
              )
            )
          ) %>% 
          filter(set %in% c("RSV", "Influenza", "COVID-19")) %>% 
          mutate(
            set = factor(set, levels = c("RSV", "Influenza", "COVID-19"))
          )
        
        cols_dat <- tibble(
          set = c("RSV", "Influenza", "COVID-19"),
          cols = cols
        )

        sizes_data <- merge(sizes_data, cols_dat, by = "set")

        sizes_plot <- ggplot(sizes_data, aes(x = set, y = size)) +
          geom_col(aes(fill = set), show.legend = TRUE) +
          scale_fill_manual(values = cols,
            labels = c("RSV", "Influenza", "COVID-19"),
            name = "Virus"
          ) +
          ggrepel::geom_text_repel(aes(label = size), size = 3, direction = "y") +
          labs(x = NULL) +
          scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.1))) +
          #expand_limits(x = c(-0.1, 4)) +
          theme_bw() + theme(
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.direction = "horizontal",
            legend.position = "bottom",
            legend.title = element_text(size = 14)
          ) +
          guides(fill = guide_legend(label.position = "left"))
        
        legend <- get_legend(sizes_plot)

        plot[[phenotype]] <- plot_grid(
          plot_grid(sizes_plot + theme(legend.position = "none"),
                    NULL, nrow = 2, rel_heights = c(0.95, 0.05)),
          plot_grid(uu$Matrix, uu$Main_bar, NULL, nrow = 3,
                    rel_heights = c(1, 2.75, 0.05), align = 'hv'),
          ncol = 2, align = 'hv', rel_widths = c(0.25, 0.75)
        )
        
      }
      
      plot_list[[season]] <- plot_grid(
        plot[["Specific"]],
        NULL,
        plot[["Sensitive"]],
        ncol = 3, rel_widths = c(0.8, 0.01, 0.8)
      )
      
    }
    
    plot_label <- ggdraw() +
      draw_label(
        "Specific Phenotype",
        x = 0.5, y = 0, hjust = 1.35, vjust = -0.75,
        fontface = 'bold', size = 14# color = "#71797E"
      ) +
      draw_label(
        "Sensitive Phenotype",
        x = 1, y = 0, hjust = 1.55, vjust = -0.75,
        fontface = 'bold', size = 14#, color = "#71797E"
      ) +
      theme(plot.background = element_rect(
        fill = "white", colour = "white"))
    
    other_plot_label <- ggdraw() +
      draw_label(
        "Total Cases",
        x = 1, y = 0, hjust = 12.68, vjust = -1.2, size = 12
      ) +
      draw_label(
        "Total Cases",
        x = 1, y = 0, hjust = 5.9, vjust = -1.2, size = 12
      ) +
      theme(plot.background = element_rect(
        fill = "white", colour = "white"))
    
    plots[[outcome]] <- plot_grid(
      other_plot_label,
      plot_grid(
        plotlist = plot_list,
        ncol = 1, align = "v",
        axis = "tb",
        labels = gsub("_", "-", seasons),
        vjust = 0.2,
        hjust = 0.5,
        label_size = 12
      ), ncol = 1, rel_heights = c(0.01, 0.99)
    )
    
  }
  
  # plot_final2 <- plot_grid(
  #   NULL,
  #   plot_grid(NULL, plots[["Mild"]], ncol = 2, rel_widths = c(0.05, 1)),
  #   NULL,
  #   plot_grid(NULL, plots[["Severe"]], ncol = 2, rel_widths = c(0.05, 1)),
  #   nrow = 4,
  #   rel_heights = c(0.035, 0.5, 0.01, 0.5),
  #   labels = c("", "A. Mild", "", "B. Severe"),
  #   label_size = 14,
  #   label_fontface = "bold",
  #   hjust = c(0, -0.1, 0, -0.05), 
  #   vjust = -2
  # )
  # plot_final <- plot_grid(
  #   legend,
  #   plot_label,
  #   plot_final2,
  #   nrow = 3, rel_heights = c(0.025, 0.01, 0.925)
  # ) %>%
    # annotate_figure(
    #   top = text_grob(
    #     paste0("Identification of Outcomes in ",
    #            str_to_title(gsub("_", " ", cohort)),
    #            " by Season of Interest"),
    #     color = "black", face = "bold", size = 16, 
    #     vjust = 1.25)
    # )
  
  plot_mild <- plot_grid(
    legend,
    plot_label,
    plot_grid(NULL, plots[["Mild"]], ncol = 2, rel_widths = c(0.05, 1)),
    nrow = 3, rel_heights = c(0.02, 0.02, 0.925)
  )

  plot_severe <- plot_grid(
    legend,
    plot_label,
    plot_grid(NULL, plots[["Severe"]], ncol = 2, rel_widths = c(0.05, 1)),
    nrow = 3, rel_heights = c(0.02, 0.02, 0.925)
  )
  
  return(list(plot_mild, plot_severe))
  
}
