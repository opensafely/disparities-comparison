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
  source(here::here("post_check", "functions", "condensed_figures.R"))
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
        facet_outcome = TRUE
      ) + theme(legend.position = "none"),
      sensitive = forest_over_time_plot(
        both_dat %>% filter(codelist_type %in% c("reference", "sensitive")),
        pathogen = pathogen,
        model_type = model_type,
        facet_outcome = TRUE
      ) + theme(legend.position = "none")
    )
  }

  run_cohort_condensed <- function(cohort, model_type = "ethnicity_ses") {
    # `forest_year_further_mult()` reads `cohort` from the global environment.
    assign("cohort", cohort, envir = .GlobalEnv)

    pathogen <- "rsv"
    df_input_rsv <- load_collated_further(cohort, pathogen)
    df_dummy_rsv <- load_dummy_inputs(cohort, pathogen)
    rsv_plots <- make_facet_outcome_plots(df_input_rsv, df_dummy_rsv, pathogen, model_type)

    pathogen <- "flu"
    df_input_flu <- load_collated_further(cohort, pathogen)
    df_dummy_flu <- load_dummy_inputs(cohort, pathogen)
    flu_plots <- make_facet_outcome_plots(df_input_flu, df_dummy_flu, pathogen, model_type)

    pathogen <- "covid"
    df_input_covid <- load_collated_further(cohort, pathogen)
    df_dummy_covid <- load_dummy_inputs(cohort, pathogen)
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

    # Rurality always appears in the right-hand "flu" legend block (with other non-vax flu groups),
    # not in the left column — avoids splitting rurality away from the rest of the flu legend.
    groups_left_legend <- setdiff(groups_left, "Rurality")

    shared_legend_left <- get_legend({
      forest_over_time_plot(
        legend_dat %>% filter(as.character(labels) %in% groups_left_legend),
        pathogen = "flu",
        model_type = model_type,
        facet_outcome = TRUE,
        show_disruption_legend = FALSE
      ) +
        theme(legend.position = "left")
    })

    shared_legend_mid <- if (length(groups_mid) > 0) {
      # Build the entire right-hand legend as ONE legend object so its internal
      # alignment is handled by ggplot, not by stacking multiple grobs.
      vax_groups <- c("Prior Vaccination", "Current Vaccination")

      covid_vax_src <- bind_rows(
        forest_year_further_mult(df_input_covid, df_dummy_covid, "covid", model_type, "Mild", return_data = TRUE),
        forest_year_further_mult(df_input_covid, df_dummy_covid, "covid", model_type, "Severe", return_data = TRUE)
      ) %>%
        filter(
          codelist_type %in% c("reference", "specific"),
          as.character(labels) %in% intersect(vax_groups, groups_mid)
        )

      legend_dat_right <- bind_rows(
        # Keep non-vaccination groups (and flu prior vaccination), but avoid duplicating
        # current-vaccination entries which are sourced from COVID data below.
        legend_dat %>%
          filter(
            as.character(labels) %in% groups_mid,
            as.character(labels) != "Current Vaccination"
          ),
        covid_vax_src
      )

      get_legend({
        forest_over_time_plot(
          legend_dat_right,
          pathogen = "flu",
          model_type = model_type,
          facet_outcome = TRUE,
          show_disruption_legend = TRUE
        ) +
          theme(legend.position = "left", legend.title = element_blank())
      })
    } else NULL
    
    specific_condensed <- assemble_condensed_figure(
      rsv_plots$specific, flu_plots$specific, covid_plots$specific,
      shared_legend_left, shared_legend_mid
    )
    sensitive_condensed <- assemble_condensed_figure(
      rsv_plots$sensitive, flu_plots$sensitive, covid_plots$sensitive,
      shared_legend_left, shared_legend_mid
    )
    
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
