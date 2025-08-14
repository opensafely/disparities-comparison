library(UpSetR)
library(plyr)
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
  
  #modify upset plot code
  NoAttBasePlot <- function (legend, size_plot_height, Main_bar_plot, Matrix_plot, 
    hratios, Size_plot, query_legend, set_metadata, set_metadata_plots, 
    newpage) {
    top <- 1
    bottom <- 100
    if ((!is.null(legend)) && (query_legend != tolower("none"))) {
        if (query_legend == tolower("top")) {
            top <- 3
            bottom <- 102
            legend_top <- 1
            legend_bottom <- 3
            size_plot_height <- (size_plot_height + 2)
        }
        else if (query_legend == tolower("bottom")) {
            legend_top <- 101
            legend_bottom <- 103
        }
    }
    if (is.null(set_metadata)) {
        matrix_and_mainbar_right <- 100
        matrix_and_mainbar_left <- 21
        size_bar_right <- 20
        size_bar_left <- 1
    }
    else if (!is.null(set_metadata)) {
        matrix_and_mainbar_right <- set_metadata$ncols + 100
        matrix_and_mainbar_left <- set_metadata$ncols + 21
        size_bar_right <- set_metadata$ncols + 20
        size_bar_left <- set_metadata$ncols + 1
        metadata_right <- set_metadata$ncols
        metadata_left <- 1
    }
    if (newpage) {
        grid::grid.newpage()
    }
    if ((!is.null(legend)) && (query_legend != tolower("none"))) {
        if (query_legend == tolower("top")) {
            pushViewport(viewport(layout = grid.layout(102, matrix_and_mainbar_right)))
        }
        else if (query_legend == tolower("bottom")) {
            pushViewport(viewport(layout = grid.layout(103, matrix_and_mainbar_right)))
        }
    }
    else if ((is.null(legend)) || (query_legend == tolower("none"))) {
        pushViewport(viewport(layout = grid.layout(100, matrix_and_mainbar_right)))
    }
    # Modified
    vp = UpSetR:::vplayout(top:bottom, 1:(matrix_and_mainbar_right-matrix_and_mainbar_left))
    pushViewport(vp)
    grid.draw(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios))
    popViewport()
    # Modified
    vp = UpSetR:::vplayout(size_plot_height:bottom, (matrix_and_mainbar_right-matrix_and_mainbar_left-1):96)
    pushViewport(vp)
    grid.draw(arrangeGrob(Size_plot))
    popViewport()
    if (!is.null(set_metadata)) {
        for (i in 1:length(set_metadata_plots)) {
            if (i != 1) {
                metadata_left <- 1 + metadata_right
                metadata_right <- metadata_right + set_metadata$plots[[i]]$assign
            }
            else {
                metadata_left <- 1
                metadata_right <- set_metadata$plots[[i]]$assign
            }
            vp = UpSetR:::vplayout(size_plot_height:bottom, metadata_left:metadata_right)
            pushViewport(vp)
            grid.draw(arrangeGrob(set_metadata_plots[[i]]))
            popViewport()
        }
    }
    if ((!is.null(legend)) && (query_legend != tolower("none"))) {
        vp = UpSetR:::vplayout(legend_top:legend_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
        pushViewport(vp)
        grid.draw(arrangeGrob(legend))
        popViewport()
    }
  }
  
  label <- function(x) {
    ifelse(x > 10000, 
           scales::scientific_format(digits = 1)(x), 
           x)
  }

  Make_size_plot <- function (Set_size_data, sbar_color, ratios, ylabel, scale_sets, 
      text_scale, set_size_angles, set_size.show, set_size.scale_max, 
      set_size.number_size) {
      if (length(text_scale) > 1 && length(text_scale) <= 6) {
          x_axis_title_scale <- text_scale[3]
          x_axis_tick_label_scale <- text_scale[4]
      }
      else {
          x_axis_title_scale <- text_scale
          x_axis_tick_label_scale <- text_scale
      }
      if (ylabel == "Set Size" && scale_sets != "identity") {
          ylabel <- paste("Set Size", paste0("( ", 
              scale_sets, " )"))
          if (scale_sets == "log2") {
              Set_size_data$y <- log2(Set_size_data$y)
          }
          if (scale_sets == "log10") {
              Set_size_data$y <- log10(Set_size_data$y)
          }
      }
      if (!is.null(set_size.number_size)) {
          num.size <- (set_size.number_size/2.845276) * x_axis_tick_label_scale
      }
      else {
          num.size <- (7/2.845276) * x_axis_tick_label_scale
      }
      Size_plot <- (ggplot(data = Set_size_data, aes_string(x = "x", 
          y = "y")) + geom_bar(stat = "identity", colour = sbar_color, 
          width = 0.4, fill = sbar_color, position = "identity") + 
          scale_x_continuous(limits = c(0.5, (nrow(Set_size_data) + 
              0.5)), breaks = c(0, max(Set_size_data)), expand = c(0, 0)) +
          scale_y_continuous(breaks = seq(
            0, round_any(max(Set_size_data), 500), length.out = 3),
            labels = label) +
          theme(panel.background = element_rect(fill = "white"), 
          plot.margin = unit(c(-0.11, -1.3, 0.5, 0.5), "lines"), 
          axis.title.x = element_text(size = 8.3 * x_axis_title_scale), 
          axis.text.x = element_text(size = 7 * x_axis_tick_label_scale, 
              vjust = 1, hjust = 0.5), axis.line = element_line(colour = "gray0"), 
          axis.line.y = element_blank(), axis.line.x = element_line(colour = "gray0", 
              size = 0.3), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
          panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
          xlab(NULL) + ylab(ylabel) + coord_flip())
      if (set_size.show == TRUE) {
          Size_plot <- (Size_plot + geom_text(aes(label = y, vjust = 0.5, 
              hjust = 1.2, angle = set_size_angles), size = num.size))
      }
      if (scale_sets == "log10") {
          if (!is.null(set_size.scale_max)) {
              Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max, 
                  0), trans = log10_reverse_trans()))
          }
          else {
              Size_plot <- (Size_plot + scale_y_continuous(trans = log10_reverse_trans()))
          }
      }
      else if (scale_sets == "log2") {
          if (!is.null(set_size.scale_max)) {
              Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max, 
                  0), trans = log2_reverse_trans()))
          }
          else {
              Size_plot <- (Size_plot + scale_y_continuous(trans = log2_reverse_trans()))
          }
      }
      else {
          if (!is.null(set_size.scale_max)) {
              Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max, 
                  0), trans = "reverse"))
          }
          else {
              #Modified
              #Size_plot <- (Size_plot + scale_y_continuous(trans = "reverse"))
          }
      }
      Size_plot <- ggplot_gtable(ggplot_build(Size_plot))
      return(Size_plot)
  }

  assignInNamespace(x="NoAttBasePlot", value=NoAttBasePlot, ns="UpSetR")
  assignInNamespace(x="Make_size_plot", value=Make_size_plot, ns="UpSetR")
  
  
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
                    order.by = "degree",
                    decreasing = FALSE,
                    mb.ratio = c(0.8, 0.2),
                    number.angles = 0,
                    show.numbers = FALSE,
                    text.scale = 1.25,
                    point.size = 2,
                    line.size = 1,
                    set_size.angles = 45,
                    empty.intersections = TRUE,
                    main.bar.color = col,
                    sets.bar.color = cols,
                    sets = c("COVID-19", "Influenza", "RSV")
        )
        
        plot[[phenotype]] <- plot_grid(
          plot_grid(uu$Main_bar, uu$Matrix, NULL, nrow = 3,
                    rel_heights = c(2.75, 1.25, 0.65), align = 'hv'),
          plot_grid(NULL, uu$Sizes, nrow = 2, rel_heights = c(0.85, 0.45)),
          NULL,
          ncol = 3, align = 'hv', rel_widths = c(0.75, 0.2, 0.1)
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
        x = 0.5, y = 0, hjust = 2.2, vjust = -2.5,
        fontface = 'bold', size = 14
      ) +
      draw_label(
        "Sensitive Phenotype",
        x = 1, y = 0, hjust = 2.2, vjust = -2.5,
        fontface = 'bold', size = 14
      ) +
      theme(plot.background = element_rect(
        fill = "white", colour = "white"))
    
    plots[[outcome]] <- plot_grid(
      plotlist = plot_list,
      ncol = 1, align = "v",
      axis = "tb",
      labels = gsub("_", "-", seasons),
      vjust = 0,
      hjust = 0.5,
      label_size = 12
      )
    
  }
  
  plot_final2 <- plot_grid(
    NULL,
    plot_grid(NULL, plots[["Mild"]], ncol = 2, rel_widths = c(0.05, 1)),
    NULL,
    plot_grid(NULL, plots[["Severe"]], ncol = 2, rel_widths = c(0.05, 1)),
    nrow = 4,
    rel_heights = c(0.035, 0.5, 0.02, 0.5),
    labels = c("", "Mild", "", "Severe"),
    label_size = 14,
    label_fontface = "bold",
    hjust = c(0, -0.1, 0, -0.05), 
    vjust = -1.45
  )
  plot_final <- plot_grid(
    plot_final2,
    plot_label,
    nrow = 2, rel_heights = c(0.95, 0.025)
  ) %>%
    annotate_figure(
      top = text_grob(
        paste0("Identification of Outcomes in ",
               str_to_title(gsub("_", " ", cohort)),
               " by Season of Interest"),
        color = "black", face = "bold", size = 16, 
        vjust = 1.25)
    )
  
  return(plot_final)
  
}
