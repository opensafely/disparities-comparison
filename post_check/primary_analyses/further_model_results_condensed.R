  library(tidyverse)
  library(here)
  library(arrow)
  library(broom)
  library(broom.helpers)
  library(lubridate)
  library(cowplot)
  library(stringr)
  library(patchwork)
  library(ggpubr)

  #import plot function
  source(here::here("post_check", "functions", "forest.R"))
  ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
  investigation_type <- "primary"

  ### --- NEW CONSISTENT PIPELINE (all cohorts) ---
  make_facet_outcome_plots <- function(df_input, df_dummy, pathogen, model_type) {
    mild_dat <- forest_year_further_mult(df_input, df_dummy, pathogen, model_type, "Mild", return_data = TRUE)
    severe_dat <- forest_year_further_mult(df_input, df_dummy, pathogen, model_type, "Severe", return_data = TRUE)
    both_dat <- bind_rows(mild_dat, severe_dat)
    
    list(
      specific = forest_over_time_plot(
        both_dat %>% filter(codelist_type %in% c("reference", "specific")),
        pathogen = pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = FALSE
      ) + theme(legend.position = "none"),
      sensitive = forest_over_time_plot(
        both_dat %>% filter(codelist_type %in% c("reference", "sensitive")),
        pathogen = pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = FALSE
      ) + theme(legend.position = "none")
    )
  }

  assemble_condensed <- function(rsv_plot, flu_plot, covid_plot, legend_left, legend_mid = NULL) {
    rsv_row <- rsv_plot
    flu_row <- flu_plot
    covid_plot_with_mid <- covid_plot
    if (!is.null(legend_mid) && requireNamespace("cowplot", quietly = TRUE)) {
      covid_plot_with_mid <- cowplot::ggdraw(covid_plot) +
        cowplot::draw_grob(legend_mid, x = 0.44, y = 0.08, width = 0.12, height = 0.84)
    }
    covid_row <- plot_grid(NULL, legend_left, covid_plot_with_mid, NULL, ncol = 4, rel_widths = c(-0.1, 0.9, 5.1, -0.16), align = "h", axis = "tb")

    combined <- plot_grid(
      NULL,
      rsv_row,
      NULL,
      flu_row,
      NULL,
      covid_row,
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(0.05, 1, -0.03, 1.25, -0.03, 1.35)
    )

    # Single column titles (once per page).
    if (requireNamespace("cowplot", quietly = TRUE)) {
      combined <- cowplot::ggdraw(combined) +
        cowplot::draw_label("Mild", x = 0.275, y = 1, hjust = 0.5, vjust = 1.5,
                            fontface = "bold", size = 9) +
        cowplot::draw_label("Severe", x = 0.74, y = 1, hjust = 0.5, vjust = 1.5,
                            fontface = "bold", size = 9)
    }

    combined
  }

  run_cohort_condensed <- function(cohort, model_type = "ethnicity_ses") {
    # `forest_year_further_mult()` uses `cohort` as a global.
    cohort <<- cohort
    # RSV
    pathogen <- "rsv"
    df_input_rsv_raw <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")))
    df_few <<- df_input_rsv_raw %>% filter(term == "too few events")
    df_input_rsv <- df_input_rsv_raw %>% filter(term != "too few events")
    df_dummy_rsv <- read_feather(
      here::here("output", "data", paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow"))
    )
    rsv_plots <- make_facet_outcome_plots(df_input_rsv, df_dummy_rsv, pathogen, model_type)
    
    # Flu
    pathogen <- "flu"
    df_input_flu_raw <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")))
    df_few <<- df_input_flu_raw %>% filter(term == "too few events")
    df_input_flu <- df_input_flu_raw %>% filter(term != "too few events")
    df_dummy_flu <- read_feather(
      here::here("output", "data", paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow"))
    )
    flu_plots <- make_facet_outcome_plots(df_input_flu, df_dummy_flu, pathogen, model_type)
    
    # COVID-19
    pathogen <- "covid"
    df_input_covid_raw <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                        paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")))
    df_few <<- df_input_covid_raw %>% filter(term == "too few events")
    df_input_covid <- df_input_covid_raw %>% filter(term != "too few events")
    df_dummy_covid <- read_feather(
      here::here("output", "data", paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow"))
    ) %>%
      mutate(
        covid_vaccination_immunity_date = if (!"covid_vaccination_immunity_date" %in% names(.)) NA else covid_vaccination_immunity_date,
        time_since_last_covid_vaccination = if (!"time_since_last_covid_vaccination" %in% names(.)) NA_character_ else time_since_last_covid_vaccination
      ) %>%
      mutate(
        subset = "2021_22",
        time_since_last_covid_vaccination = if_else(
          is.na(covid_vaccination_immunity_date), "6-12m",
          time_since_last_covid_vaccination
        )
      )
    covid_plots <- make_facet_outcome_plots(df_input_covid, df_dummy_covid, pathogen, model_type)
    
    # Shared legend (split in two parts).
    legend_dat <- bind_rows(
      forest_year_further_mult(df_input_flu, df_dummy_flu, "flu", model_type, "Mild", return_data = TRUE),
      forest_year_further_mult(df_input_flu, df_dummy_flu, "flu", model_type, "Severe", return_data = TRUE)
    ) %>% filter(codelist_type %in% c("reference", "specific"))
    
    preferred_group_order <- c(
      "Sex",
      "Age Group",
      "Ethnicity",
      "IMD Quintile",
      "Rurality",
      "Prior Vaccination",
      "Current Vaccination"
    )
    group_present <- unique(as.character(legend_dat$labels))
    group_order <- c(
      intersect(preferred_group_order, group_present),
      setdiff(group_present, preferred_group_order)
    )
    split_idx <- ceiling(length(group_order) / 2)
    groups_left <- group_order[seq_len(split_idx)]
    groups_mid <- group_order[(split_idx + 1):length(group_order)]
    groups_mid <- groups_mid[!is.na(groups_mid)]

    shared_legend_left <- get_legend({
      forest_over_time_plot(
        legend_dat %>% filter(as.character(labels) %in% groups_left),
        pathogen = "flu",
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = FALSE
      ) +
        theme(legend.position = "left")
    })

    shared_legend_mid <- if (length(groups_mid) > 0) get_legend({
      legend_dat <- bind_rows(
        forest_year_further_mult(df_input_flu, df_dummy_flu, "flu", model_type, "Mild", return_data = TRUE),
        forest_year_further_mult(df_input_flu, df_dummy_flu, "flu", model_type, "Severe", return_data = TRUE)
      ) %>% 
        filter(codelist_type %in% c("reference", "specific"), as.character(labels) %in% groups_mid)
      forest_over_time_plot(legend_dat, pathogen = "flu", model_type = model_type, facet_outcome = TRUE, label_levels = FALSE) +
        theme(legend.position = "left")
    }) else NULL
    
    specific_condensed <- assemble_condensed(rsv_plots$specific, flu_plots$specific, covid_plots$specific, shared_legend_left, shared_legend_mid)
    sensitive_condensed <- assemble_condensed(rsv_plots$sensitive, flu_plots$sensitive, covid_plots$sensitive, shared_legend_left, shared_legend_mid)
    
    out_dir <- here("post_check", "plots", "primary_analyses", "condensed_models")
    ggsave(
      here(out_dir, paste0(cohort, "_", model_type, "_further_specific_mild_vs_severe.png")),
      specific_condensed,
      height = 11.69,
      width = 8.27
    )
    ggsave(
      here(out_dir, paste0(cohort, "_", model_type, "_further_sensitive_mild_vs_severe.png")),
      sensitive_condensed,
      height = 11.69,
      width = 8.27
    )
  }

  purrr::walk(
    c("older_adults", "adults", "children_and_adolescents", "infants"),
    run_cohort_condensed
  )

  # Legacy code below preserved for reference only.
  if (FALSE) {

  ###--- older adults
  cohort <- "older_adults"

  ##rsv
  pathogen <- "rsv"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_specific_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  rsv_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  rsv_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##flu
  pathogen <- "flu"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_specific_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  flu_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  flu_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##covid
  pathogen <- "covid"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2021_2022_specific_primary.arrow"))) %>%
    mutate(
      subset = "2021_22",
      time_since_last_covid_vaccination = if_else(
        is.na(covid_vaccination_immunity_date), "6-12m",
        time_since_last_covid_vaccination)
    )

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  covid_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2021_2022_specific_primary.arrow"))) %>%
    mutate(
      subset = "2021_22",
      time_since_last_covid_vaccination = if_else(
        is.na(covid_vaccination_immunity_date), "6-12m",
        time_since_last_covid_vaccination)
    )

  #plot both phenotypes together
  covid_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ### --- NEW: Mild/Severe faceted; Specific/Sensitive separate outputs ---
  make_facet_outcome_plots <- function(df_input, df_dummy, pathogen, cohort, model_type) {
    mild_dat <- forest_year_further_mult(df_input, df_dummy, pathogen, model_type, "Mild", return_data = TRUE)
    severe_dat <- forest_year_further_mult(df_input, df_dummy, pathogen, model_type, "Severe", return_data = TRUE)
    both_dat <- bind_rows(mild_dat, severe_dat)
    
    list(
      specific = forest_over_time_plot(
        both_dat %>% filter(codelist_type == "specific"),
        pathogen = pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = TRUE,
        jitter_width = 0.12
      ) + theme(legend.position = "none"),
      sensitive = forest_over_time_plot(
        both_dat %>% filter(codelist_type == "sensitive"),
        pathogen = pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = TRUE,
        jitter_width = 0.12
      ) + theme(legend.position = "none")
    )
  }

  # Rebuild plots for each pathogen as mild/severe facets
  pathogen <- "rsv"
  df_input_rsv <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                    paste0(cohort, "_further_", pathogen,
                                            "_model_outputs_collated.csv"))) %>%
    filter(term != "too few events")
  df_dummy_rsv <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow"))
  )
  rsv_plots <- make_facet_outcome_plots(df_input_rsv, df_dummy_rsv, pathogen, cohort, "ethnicity_ses")

  pathogen <- "flu"
  df_input_flu <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                    paste0(cohort, "_further_", pathogen,
                                            "_model_outputs_collated.csv"))) %>%
    filter(term != "too few events")
  df_dummy_flu <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow"))
  )
  flu_plots <- make_facet_outcome_plots(df_input_flu, df_dummy_flu, pathogen, cohort, "ethnicity_ses")

  pathogen <- "covid"
  df_input_covid <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                      paste0(cohort, "_further_", pathogen,
                                              "_model_outputs_collated.csv"))) %>%
    filter(term != "too few events")
  df_dummy_covid <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow"))
  ) %>%
    mutate(
      subset = "2021_22",
      time_since_last_covid_vaccination = if_else(
        is.na(covid_vaccination_immunity_date), "6-12m",
        time_since_last_covid_vaccination
      )
    )
  covid_plots <- make_facet_outcome_plots(df_input_covid, df_dummy_covid, pathogen, cohort, "ethnicity_ses")

  # One shared legend (same for specific/sensitive; pull from flu)
  shared_legend <- get_legend(
    forest_over_time_plot(
      bind_rows(
        forest_year_further_mult(df_input_flu, df_dummy_flu, "flu", "ethnicity_ses", "Mild", return_data = TRUE),
        forest_year_further_mult(df_input_flu, df_dummy_flu, "flu", "ethnicity_ses", "Severe", return_data = TRUE)
      ) %>% filter(codelist_type == "specific"),
      pathogen = "flu",
      model_type = "ethnicity_ses",
      facet_outcome = TRUE,
      label_levels = TRUE,
      jitter_width = 0.12
    )
  )

  assemble_condensed <- function(rsv_plot, flu_plot, covid_plot, legend) {
    rsv_row <- plot_grid(NULL, rsv_plot, ncol = 2, rel_widths = c(0.9, 5.1), align = "h", axis = "tb")
    flu_row <- plot_grid(NULL, flu_plot, ncol = 2, rel_widths = c(0.9, 5.1), align = "h", axis = "tb")
    covid_row <- plot_grid(legend, covid_plot, ncol = 2, rel_widths = c(0.9, 5.1), align = "h", axis = "tb")
    
    plot_grid(
      rsv_row,
      NULL,
      flu_row,
      NULL,
      covid_row,
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, -0.03, 1.25, -0.03, 1.35)
    )
  }

  specific_condensed <- assemble_condensed(rsv_plots$specific, flu_plots$specific, covid_plots$specific, shared_legend)
  sensitive_condensed <- assemble_condensed(rsv_plots$sensitive, flu_plots$sensitive, covid_plots$sensitive, shared_legend)

  ggsave(
    here("post_check", "plots", "primary_analyses", "condensed_models",
        paste0(cohort, "_ethnicity_ses_further_specific_mild_vs_severe.png")),
    specific_condensed,
    height = 11.69,
    width = 8.27
  )

  ggsave(
    here("post_check", "plots", "primary_analyses", "condensed_models",
        paste0(cohort, "_ethnicity_ses_further_sensitive_mild_vs_severe.png")),
    sensitive_condensed,
    height = 11.69,
    width = 8.27
  )

  ##mild

  # Create versions of your plots without legends
  rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none",
                                            axis.text.x = element_blank(),
                                            axis.ticks = element_blank(),
                                            axis.title.x = element_blank(),
                                            strip.text.y = element_blank(),
                                            strip.background.y = element_blank())
  flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none",
                                            axis.title.x = element_text(size = 12),
                                            strip.text.y = element_blank(),
                                            strip.background.y = element_blank())
  covid_plot <- covid_ethnicity_ses_mild + theme(legend.position = "none",
                                                axis.text.x = element_blank(),
                                                axis.ticks = element_blank(),
                                                axis.title.x = element_blank(),
                                                strip.text.y = element_blank(),
                                                strip.background.y = element_blank()) +
    coord_cartesian(xlim = c(2019, 2023)) +
    scale_x_continuous(
      breaks = 2019:2023,
      labels = paste0(2019:2023, "-", stringr::str_sub(as.character(2020:2024), 3, 4))
    )

  # Extract the legend from the original plot
  legend <- get_legend(flu_ethnicity_ses_mild)

  # Build rows on a shared two-column scaffold so widths align.
  rsv_row <- plot_grid(NULL, rsv_plot, ncol = 2, rel_widths = c(0, 5.1), align = "h", axis = "tb")
  flu_row <- plot_grid(NULL, flu_plot, ncol = 2, rel_widths = c(0, 5.1), align = "h", axis = "tb")
  covid_row <- plot_grid(NULL, legend, covid_plot, NULL, ncol = 4, rel_widths = c(0.58, 0.8, 2.6, 0.01), align = "h", axis = "tb")

  # Combine all plots
  combined_plot_mild <- plot_grid(
    rsv_row,
    NULL,
    flu_row,
    NULL,
    covid_row,
    ncol = 1,
    align = 'v',
    axis = 'lr',
    rel_heights = c(1, -0.03, 1.25, -0.03, 1.35)
  )

  # Add annotations
  ethnicity_ses_mild <- annotate_figure(
    combined_plot_mild, 
    top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                          str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                    x = 1.5, hjust = 0, vjust = -16,
                    y = c(0.836, 0.515, 0.195), 
                    just = "left", face = "bold")
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
        paste0(cohort, "_mild_ethnicity_ses_further_2", ".png")),
        ethnicity_ses_mild, height = 18, width = 15)

  ##severe

  # Create versions of your plots without legends
  rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none",
                                              axis.text.x = element_blank(),
                                              axis.ticks = element_blank(),
                                              axis.title.x = element_blank(),
                                              axis.title.y = element_blank(),
                                              axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank())
  flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none",
                                              axis.title.x = element_text(size = 12),
                                              axis.title.y = element_blank(),
                                              axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank())
  covid_plot <- covid_ethnicity_ses_severe + theme(legend.position = "none",
                                                  axis.text.x = element_blank(),
                                                  axis.ticks = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  axis.title.y = element_blank(),
                                                  axis.text.y = element_blank(),
                                                  axis.ticks.y = element_blank()) +
    coord_cartesian(xlim = c(2019, 2023)) +
    scale_x_continuous(
      breaks = 2019:2023,
      labels = paste0(2019:2023, "-", stringr::str_sub(as.character(2020:2024), 3, 4))
    )

  # Extract the legend from the original plot
  legend <- get_legend(flu_ethnicity_ses_severe)

  # Build rows on a shared two-column scaffold so widths align.
  rsv_row <- plot_grid(NULL, rsv_plot, ncol = 2, rel_widths = c(0, 5.1), align = "h", axis = "tb")
  flu_row <- plot_grid(NULL, flu_plot, ncol = 2, rel_widths = c(0, 5.1), align = "h", axis = "tb")
  covid_row <- plot_grid(NULL, legend, covid_plot, NULL, ncol = 4, rel_widths = c(0.38, 0.8, 2.6, 0.01), align = "h", axis = "tb")

  # Combine all plots
  combined_plot_severe <- plot_grid(
    rsv_row,
    NULL,
    flu_row,
    NULL,
    covid_row,
    ncol = 1,
    align = 'v',
    axis = 'lr',
    rel_heights = c(1, -0.03, 1.25, -0.03, 1.35)
  )

  # Add annotations
  ethnicity_ses_severe <- annotate_figure(
    combined_plot_severe, 
    top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                          str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                    x = 1.5, hjust = 0, vjust = -16,
                    y = c(0.836, 0.515, 0.195), 
                    just = "left", face = "bold")
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
              paste0(cohort, "_severe_ethnicity_ses_further_2", ".png")),
        ethnicity_ses_severe, height = 18, width = 15)

  #plot mild and severe together
  final_combined <- plot_grid(
    annotate_figure(combined_plot_mild, top = text_grob(
      "Mild Outcomes", hjust = 0.35, vjust = 0.8, size = 10, face = "bold")),
    annotate_figure(combined_plot_severe, top = text_grob(
      "Severe Outcomes", hjust = 0.78, vjust = 0.8, size = 10, face = "bold")),
    ncol = 2,
    rel_widths = c(1, 1)
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
              paste0(cohort, "_all_ethnicity_ses_further_2", ".png")),
        final_combined, height = 11.69, width = 8.27)

  ##overall respiratory
  pathogen <- "overall_and_all_cause"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_sensitive_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
  filter(term == "too few events")

  df_input <- df_input %>%
  filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  overall_resp_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  overall_resp_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  #plot mild and severe together
  final_combined <- plot_grid(
  overall_resp_ethnicity_ses_mild %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                              str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                        just = "left", face = "bold")
    ),
  overall_resp_ethnicity_ses_severe %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                              str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                        just = "left", face = "bold")
    ),
  nrow = 2
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_overall_resp_ethnicity_ses_further_2", ".png")),
        final_combined, height = 10, width = 18)
  }

  if (FALSE) {
  ###--- adults
  cohort <- "adults"

  ##rsv
  pathogen <- "rsv"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_specific_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  rsv_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  rsv_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##flu
  pathogen <- "flu"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_specific_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  flu_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  flu_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##covid
  pathogen <- "covid"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2021_2022_specific_primary.arrow"))) %>%
    mutate(
      subset = "2021_22",
      time_since_last_covid_vaccination = if_else(
        is.na(covid_vaccination_immunity_date), "6-12m",
        time_since_last_covid_vaccination)
    )

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  covid_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2021_2022_specific_primary.arrow"))) %>%
    mutate(
      subset = "2021_22",
      time_since_last_covid_vaccination = if_else(
        is.na(covid_vaccination_immunity_date), "6-12m",
        time_since_last_covid_vaccination)
    )

  #plot both phenotypes together
  covid_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##mild

  # Create versions of your plots without legends
  rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none",
                                            axis.text.x = element_blank(),
                                            axis.ticks = element_blank(),
                                            axis.title.x = element_blank())
  flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none",
                                            axis.title.x = element_text(size = 12))
  covid_plot <- covid_ethnicity_ses_mild + theme(legend.position = "none",
                                                axis.text.x = element_blank(),
                                                axis.ticks = element_blank(),
                                                axis.title.x = element_blank())

  # Extract the legend from the original plot
  legend <- get_legend(flu_ethnicity_ses_mild)

  # Create the bottom row with legend and COVID plot
  bottom_row <- plot_grid(
    legend, 
    covid_plot, 
    NULL,
    rel_widths = c(1, 3.15, -0.05), 
    nrow = 1
  )

  # Combine all plots
  combined_plot_mild <- plot_grid(
    rsv_plot,
    NULL,
    flu_plot,
    NULL,
    bottom_row,
    ncol = 1,
    align = 'v',
    axis = 'lr',
    rel_heights = c(1, -0.035, 1.25, -0.0475, 1.1)
  )

  # Add annotations
  ethnicity_ses_mild <- annotate_figure(
    combined_plot_mild, 
    top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                          str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                    x = 1.5, hjust = 0, vjust = -16,
                    y = c(0.836, 0.515, 0.195), 
                    just = "left", face = "bold")
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
        paste0(cohort, "_mild_ethnicity_ses_further", ".png")),
        ethnicity_ses_mild, height = 15, width = 15)

  ##severe

  # Create versions of your plots without legends
  rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none",
                                              axis.text.x = element_blank(),
                                              axis.ticks = element_blank(),
                                              axis.title.x = element_blank())
  flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none",
                                              axis.title.x = element_text(size = 12))
  covid_plot <- covid_ethnicity_ses_severe + theme(legend.position = "none",
                                                  axis.text.x = element_blank(),
                                                  axis.ticks = element_blank(),
                                                  axis.title.x = element_blank())

  # Extract the legend from the original plot
  legend <- get_legend(flu_ethnicity_ses_severe)

  # Create the bottom row with legend and COVID plot
  bottom_row <- plot_grid(
    legend, 
    covid_plot,
    NULL, 
    rel_widths = c(1, 3.15, -0.05), 
    nrow = 1
  )

  # Combine all plots
  combined_plot_severe <- plot_grid(
    rsv_plot,
    NULL,
    flu_plot,
    NULL,
    bottom_row,
    ncol = 1,
    align = 'v',
    axis = 'lr',
    rel_heights = c(1, -0.035, 1.25, -0.0475, 1.1)
  )

  # Add annotations
  ethnicity_ses_severe <- annotate_figure(
    combined_plot_severe, 
    top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                          str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                    x = 1.5, hjust = 0, vjust = -16,
                    y = c(0.836, 0.515, 0.195), 
                    just = "left", face = "bold")
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
              paste0(cohort, "_severe_ethnicity_ses_further", ".png")),
        ethnicity_ses_severe, height = 15, width = 15)

  #plot mild and severe together
  final_combined <- plot_grid(
    NULL,
    plot_grid(
      annotate_figure(combined_plot_mild, top = text_grob(
        "Mild Outcomes", hjust = -0.7, vjust = 1.45, size = 12.5, face = "bold")),
      NULL,
      annotate_figure(combined_plot_severe, top = text_grob(
        "Severe Outcomes", hjust = -0.5, vjust = 1.45, size = 12.5, face = "bold")),
      nrow = 3,
      rel_heights = c(1, -0.01, 1)
    ),
    NULL,
    ncol = 3,
    rel_widths = c(-0.075, 1, -0.065)
  ) %>%
    annotate_figure(
      left = text_grob(c("RSV", "Influenza",
                        "RSV", "Influenza"), 
                      x = 1,
                      y = c(0.8844, 0.74,
                            0.38685, 0.24305), 
                      hjust = 0, vjust = -16,
                      just = "left", face = "bold"),
      right = text_grob(c("COVID-19", "COVID-19"),
                        x = 1, y = c(0.5584, 0.061),
                        hjust = 10, vjust = -16,
                        just = "left", face = "bold")
    )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
              paste0(cohort, "_all_ethnicity_ses_further", ".png")),
        final_combined, height = 21, width = 14)

  ##overall respiratory
  pathogen <- "overall_and_all_cause"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_sensitive_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
  filter(term == "too few events")

  df_input <- df_input %>%
  filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  overall_resp_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  overall_resp_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  #plot mild and severe together
  final_combined <- plot_grid(
  overall_resp_ethnicity_ses_mild %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                              str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                        just = "left", face = "bold")
    ),
  overall_resp_ethnicity_ses_severe %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                              str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                        just = "left", face = "bold")
    ),
  nrow = 2
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_overall_resp_ethnicity_ses_further", ".png")),
        final_combined, height = 10, width = 18)

  ###--- children and adolescents
  cohort <- "children_and_adolescents"

  ##rsv
  pathogen <- "rsv"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_specific_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  rsv_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  rsv_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##flu
  pathogen <- "flu"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_specific_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  flu_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  flu_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##covid
  pathogen <- "covid"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2021_2022_specific_primary.arrow"))) %>%
    mutate(
      subset = "2021_22",
      time_since_last_covid_vaccination = if_else(
        is.na(covid_vaccination_immunity_date), "6-12m",
        time_since_last_covid_vaccination)
    )

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  covid_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2021_2022_specific_primary.arrow"))) %>%
    mutate(
      subset = "2021_22",
      time_since_last_covid_vaccination = if_else(
        is.na(covid_vaccination_immunity_date), "6-12m",
        time_since_last_covid_vaccination)
    )

  #plot both phenotypes together
  covid_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##mild

  # Create versions of your plots without legends
  rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none",
                                            axis.text.x = element_blank(),
                                            axis.ticks = element_blank(),
                                            axis.title.x = element_blank())
  flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none",
                                            axis.title.x = element_text(size = 12))
  covid_plot <- covid_ethnicity_ses_mild + theme(legend.position = "none",
                                                axis.text.x = element_blank(),
                                                axis.ticks = element_blank(),
                                                axis.title.x = element_blank())

  # Extract the legend from the original plot
  legend <- get_legend(flu_ethnicity_ses_mild)

  # Create the bottom row with legend and COVID plot
  bottom_row <- plot_grid(
    legend, 
    covid_plot, 
    NULL,
    rel_widths = c(1, 3.15, -0.05), 
    nrow = 1
  )

  # Combine all plots
  combined_plot_mild <- plot_grid(
    rsv_plot,
    NULL,
    flu_plot,
    NULL,
    bottom_row,
    ncol = 1,
    align = 'v',
    axis = 'lr',
    rel_heights = c(1, -0.035, 1.25, -0.0475, 1.1)
  )

  # Add annotations
  ethnicity_ses_mild <- annotate_figure(
    combined_plot_mild, 
    top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                          str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                    x = 1.5, hjust = 0, vjust = -16,
                    y = c(0.836, 0.515, 0.195), 
                    just = "left", face = "bold")
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
        paste0(cohort, "_mild_ethnicity_ses_further", ".png")),
        ethnicity_ses_mild, height = 15, width = 15)

  ##severe

  # Create versions of your plots without legends
  rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none",
                                              axis.text.x = element_blank(),
                                              axis.ticks = element_blank(),
                                              axis.title.x = element_blank())
  flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none",
                                              axis.title.x = element_text(size = 12))
  covid_plot <- covid_ethnicity_ses_severe + theme(legend.position = "none",
                                                  axis.text.x = element_blank(),
                                                  axis.ticks = element_blank(),
                                                  axis.title.x = element_blank())

  # Extract the legend from the original plot
  legend <- get_legend(flu_ethnicity_ses_severe)

  # Create the bottom row with legend and COVID plot
  bottom_row <- plot_grid(
    legend, 
    covid_plot,
    NULL, 
    rel_widths = c(1, 3.15, -0.05), 
    nrow = 1
  )

  # Combine all plots
  combined_plot_severe <- plot_grid(
    rsv_plot,
    NULL,
    flu_plot,
    NULL,
    bottom_row,
    ncol = 1,
    align = 'v',
    axis = 'lr',
    rel_heights = c(1, -0.035, 1.25, -0.0475, 1.1)
  )

  # Add annotations
  ethnicity_ses_severe <- annotate_figure(
    combined_plot_severe, 
    top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                          str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                    x = 1.5, hjust = 0, vjust = -16,
                    y = c(0.836, 0.515, 0.195), 
                    just = "left", face = "bold")
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
              paste0(cohort, "_severe_ethnicity_ses_further", ".png")),
        ethnicity_ses_severe, height = 15, width = 15)

  #plot mild and severe together
  final_combined <- plot_grid(
    NULL,
    plot_grid(
      annotate_figure(combined_plot_mild, top = text_grob(
        "Mild Outcomes", hjust = -0.7, vjust = 1.45, size = 12.5, face = "bold")),
      NULL,
      annotate_figure(combined_plot_severe, top = text_grob(
        "Severe Outcomes", hjust = -0.5, vjust = 1.45, size = 12.5, face = "bold")),
      nrow = 3,
      rel_heights = c(1, -0.01, 1)
    ),
    NULL,
    ncol = 3,
    rel_widths = c(-0.075, 1, -0.065)
  ) %>%
    annotate_figure(
      left = text_grob(c("RSV", "Influenza",
                        "RSV", "Influenza"), 
                      x = 1,
                      y = c(0.8844, 0.74,
                            0.38685, 0.24305), 
                      hjust = 0, vjust = -16,
                      just = "left", face = "bold"),
      right = text_grob(c("COVID-19", "COVID-19"),
                        x = 1, y = c(0.5584, 0.061),
                        hjust = 10, vjust = -16,
                        just = "left", face = "bold")
    )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
              paste0(cohort, "_all_ethnicity_ses_further", ".png")),
        final_combined, height = 21, width = 14)

  ##overall respiratory
  pathogen <- "overall_and_all_cause"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_sensitive_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
  filter(term == "too few events")

  df_input <- df_input %>%
  filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  overall_resp_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  overall_resp_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  #plot mild and severe together
  final_combined <- plot_grid(
  overall_resp_ethnicity_ses_mild %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                              str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                        just = "left", face = "bold")
    ),
  overall_resp_ethnicity_ses_severe %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                              str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                        just = "left", face = "bold")
    ),
  nrow = 2
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_overall_resp_ethnicity_ses_further", ".png")),
        final_combined, height = 10, width = 18)

  ###--- infants
  cohort <- "infants"

  ##rsv
  pathogen <- "rsv"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_specific_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  rsv_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  rsv_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##flu
  pathogen <- "flu"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_specific_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  flu_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  flu_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##covid
  pathogen <- "covid"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
    here::here("output", "data", paste0("input_processed_", cohort, 
              "_2021_2022_specific_primary.arrow")))

  #extract models for which there were too few events
  df_few <- df_input %>%
    filter(term == "too few events")

  df_input <- df_input %>%
    filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  covid_ethnicity_ses_mild <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  covid_ethnicity_ses_severe <- forest_year_further_mult(
    df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  ##mild

  # Create versions of your plots without legends
  rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none",
                                            axis.text.x = element_blank(),
                                            axis.ticks = element_blank(),
                                            axis.title.x = element_blank(),
                                            strip.text.y = element_blank(),
                                            strip.background.y = element_blank())
  flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none",
                                            axis.title.x = element_text(size = 12),
                                            strip.text.y = element_blank(),
                                            strip.background.y = element_blank())
  covid_plot <- covid_ethnicity_ses_mild + theme(legend.position = "none",
                                                axis.text.x = element_blank(),
                                                axis.ticks = element_blank(),
                                                axis.title.x = element_blank(),
                                                strip.text.y = element_blank(),
                                                strip.background.y = element_blank()) +
    coord_cartesian(xlim = c(2019, 2023)) +
    scale_x_continuous(
      breaks = 2019:2023,
      labels = paste0(2019:2023, "-", stringr::str_sub(as.character(2020:2024), 3, 4))
    )

  # Extract the legend from the original plot
  legend <- get_legend(flu_ethnicity_ses_mild)

  # Build rows on a shared two-column scaffold so widths align.
  rsv_row <- plot_grid(NULL, rsv_plot, ncol = 2, rel_widths = c(0, 5.1), align = "h", axis = "tb")
  flu_row <- plot_grid(NULL, flu_plot, ncol = 2, rel_widths = c(0, 5.1), align = "h", axis = "tb")
  covid_row <- plot_grid(NULL, legend, covid_plot, NULL, ncol = 4, rel_widths = c(0.58, 0.8, 2.6, 0.01), align = "h", axis = "tb")

  # Combine all plots
  combined_plot_mild <- plot_grid(
    rsv_row,
    NULL,
    flu_row,
    NULL,
    covid_row,
    ncol = 1,
    align = 'v',
    axis = 'lr',
    rel_heights = c(1, -0.03, 1.25, -0.03, 1.35)
  )

  # Add annotations
  ethnicity_ses_mild <- annotate_figure(
    combined_plot_mild, 
    top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                          str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                    x = 1.5, hjust = 0, vjust = -16,
                    y = c(0.836, 0.515, 0.195), 
                    just = "left", face = "bold")
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
        paste0(cohort, "_mild_ethnicity_ses_further_2", ".png")),
        ethnicity_ses_mild, height = 18, width = 15)

  ##severe

  # Create versions of your plots without legends
  rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none",
                                              axis.text.x = element_blank(),
                                              axis.ticks = element_blank(),
                                              axis.title.x = element_blank(),
                                              axis.title.y = element_blank(),
                                              axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank())
  flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none",
                                              axis.title.x = element_text(size = 12),
                                              axis.title.y = element_blank(),
                                              axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank())
  covid_plot <- covid_ethnicity_ses_severe + theme(legend.position = "none",
                                                  axis.text.x = element_blank(),
                                                  axis.ticks = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  axis.title.y = element_blank(),
                                                  axis.text.y = element_blank(),
                                                  axis.ticks.y = element_blank()) +
    coord_cartesian(xlim = c(2019, 2023)) +
    scale_x_continuous(
      breaks = 2019:2023,
      labels = paste0(2019:2023, "-", stringr::str_sub(as.character(2020:2024), 3, 4))
    )

  # Extract the legend from the original plot
  legend <- get_legend(flu_ethnicity_ses_severe)

  # Build rows on a shared two-column scaffold so widths align.
  rsv_row <- plot_grid(NULL, rsv_plot, ncol = 2, rel_widths = c(0, 5.1), align = "h", axis = "tb")
  flu_row <- plot_grid(NULL, flu_plot, ncol = 2, rel_widths = c(0, 5.1), align = "h", axis = "tb")
  covid_row <- plot_grid(NULL, legend, covid_plot, NULL, ncol = 4, rel_widths = c(0.38, 0.8, 2.6, 0.01), align = "h", axis = "tb")

  # Combine all plots
  combined_plot_severe <- plot_grid(
    rsv_row,
    NULL,
    flu_row,
    NULL,
    covid_row,
    ncol = 1,
    align = 'v',
    axis = 'lr',
    rel_heights = c(1, -0.03, 1.25, -0.03, 1.35)
  )

  # Add annotations
  ethnicity_ses_severe <- annotate_figure(
    combined_plot_severe, 
    top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                          str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                    x = 1.5, hjust = 0, vjust = -16,
                    y = c(0.836, 0.515, 0.195), 
                    just = "left", face = "bold")
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
              paste0(cohort, "_severe_ethnicity_ses_further_2", ".png")),
        ethnicity_ses_severe, height = 18, width = 15)

  final_combined <- plot_grid(
    annotate_figure(combined_plot_mild, top = text_grob(
      "Mild Outcomes", hjust = 0.35, vjust = 0.8, size = 10, face = "bold")),
    annotate_figure(combined_plot_severe, top = text_grob(
      "Severe Outcomes", hjust = 0.78, vjust = 0.8, size = 10, face = "bold")),
    ncol = 2,
    rel_widths = c(1, 1)
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
              paste0(cohort, "_all_ethnicity_ses_further_2", ".png")),
        final_combined, height = 11.69, width = 8.27)

  ##overall respiratory
  pathogen <- "overall_and_all_cause"

  #import collated model outputs
  df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                      paste0(cohort, "_further_", pathogen,
                      "_model_outputs_collated.csv")))
  df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
              "_2020_2021_sensitive_primary.arrow"))) 

  #extract models for which there were too few events
  df_few <- df_input %>%
  filter(term == "too few events")

  df_input <- df_input %>%
  filter(term != "too few events")

  ##create relevant forest plots - mild

  #plot both phenotypes together
  overall_resp_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
  )

  ##create relevant forest plots - severe

  #plot both phenotypes together
  overall_resp_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
  )

  #plot mild and severe together
  final_combined <- plot_grid(
  overall_resp_ethnicity_ses_mild %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                              str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                        just = "left", face = "bold")
    ),
  overall_resp_ethnicity_ses_severe %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                              str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                        just = "left", face = "bold")
  ),
  ncol = 2
  )

  ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_overall_resp_ethnicity_ses_further_2", ".png")),
        final_combined, height = 10, width = 18)
  }
