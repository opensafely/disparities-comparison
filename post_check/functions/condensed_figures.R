# Shared helpers for multi-virus condensed forest figures.
# Layout is the same as further_results_condensed_key_vars.R:
# three full-width ggplot panels (RSV, flu, COVID) stacked with cowplot.

load_collated_further <- function(cohort, pathogen) {
  raw <- readr::read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
  if (exists("df_few", envir = .GlobalEnv, inherits = FALSE)) {
    assign(
      "df_few",
      dplyr::bind_rows(
        get("df_few", envir = .GlobalEnv),
        raw %>% dplyr::filter(.data$term == "too few events")
      ),
      envir = .GlobalEnv
    )
  }
  raw %>% dplyr::filter(.data$term != "too few events")
}

load_collated_base <- function(cohort, pathogen) {
  readr::read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
}

load_dummy_inputs <- function(cohort, pathogen) {
  if (identical(pathogen, "covid")) {
    arrow::read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow")
      )
    ) %>%
      dplyr::mutate(
        covid_vaccination_immunity_date = if (
          !"covid_vaccination_immunity_date" %in% names(.)
        ) {
          NA
        } else {
          .data$covid_vaccination_immunity_date
        },
        time_since_last_covid_vaccination = if (
          !"time_since_last_covid_vaccination" %in% names(.)
        ) {
          NA_character_
        } else {
          .data$time_since_last_covid_vaccination
        }
      ) %>%
      dplyr::mutate(
        subset = "2021_22",
        time_since_last_covid_vaccination = dplyr::if_else(
          is.na(.data$covid_vaccination_immunity_date),
          "6-12m",
          .data$time_since_last_covid_vaccination
        )
      )
  } else {
    arrow::read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow")
      )
    )
  }
}

CONDENSED_FIG_WIDTH <- 8.5
CONDENSED_FIG_HEIGHT <- 14

# Same cowplot layout as further_results_condensed_key_vars.R
assemble_condensed_figure <- function(
    rsv_plot,
    flu_plot,
    covid_plot,
    legend_left,
    legend_mid = NULL
) {
  covid_body <- covid_plot
  if (!is.null(legend_mid)) {
    if (is.list(legend_mid)) {
      if (!is.null(legend_mid$eth)) {
        covid_body <- cowplot::ggdraw(covid_body) +
          cowplot::draw_grob(
            legend_mid$eth,
            x = 0.435,
            y = 0.52,
            width = 0.11,
            height = 0.4
          )
      }
      if (!is.null(legend_mid$imd)) {
        covid_body <- cowplot::ggdraw(covid_body) +
          cowplot::draw_grob(
            legend_mid$imd,
            x = 0.435,
            y = 0.06,
            width = 0.11,
            height = 0.4
          )
      }
    } else {
      covid_body <- cowplot::ggdraw(covid_body) +
        cowplot::draw_grob(
          legend_mid,
          x = 0.44,
          y = 0.08,
          width = 0.12,
          height = 0.84
        )
    }
  }

  legend_col_width <- if (is.null(legend_mid)) -0.55 else -0.3

  covid_row <- cowplot::plot_grid(
    NULL,
    legend_left,
    NULL,
    covid_body,
    NULL,
    ncol = 5,
    rel_widths = c(0.2, legend_col_width, 1.5, 3.6, -0.1),
    align = "h",
    axis = "tb"
  )

  combined <- cowplot::plot_grid(
    NULL,
    rsv_plot,
    NULL,
    flu_plot,
    NULL,
    covid_row,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(0.05, 1, -0.03, 1.25, -0.03, 1.35)
  )

  cowplot::ggdraw(combined) +
    cowplot::draw_label(
      "A. Mild", x = 0.275, y = 1, hjust = 0.5, vjust = 1.5,
      fontface = "bold", size = 9
    ) +
    cowplot::draw_label(
      "B. Severe", x = 0.74, y = 1, hjust = 0.5, vjust = 1.5,
      fontface = "bold", size = 9
    )
}

# ---- Further-only (all seasons on each virus plot) ----------------------------

make_facet_outcome_plots_key_vars <- function(df_input, df_dummy, pathogen, model_type) {
  mild_dat <- forest_year_further_mult_key_vars(
    df_input, df_dummy, pathogen, model_type, "Mild"
  )
  severe_dat <- forest_year_further_mult_key_vars(
    df_input, df_dummy, pathogen, model_type, "Severe"
  )
  both_dat <- dplyr::bind_rows(mild_dat, severe_dat)

  plot_one <- function(phenotype) {
    plot_dat <- both_dat %>%
      dplyr::filter(.data$codelist_type %in% c("reference", phenotype))
    forest_over_time_plot_all_seasons(
      forest_data = plot_dat,
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      show_disruption_legend = FALSE,
      log_y = TRUE
    ) +
      ggplot2::theme(legend.position = "none")
  }

  list(
    specific = plot_one("specific"),
    sensitive = plot_one("sensitive")
  )
}

build_shared_legends_key_vars <- function(legend_dat, model_type, legend_pathogen = "covid") {
  groups_left <- intersect("Age Group", unique(as.character(legend_dat$labels)))
  groups_mid <- intersect(
    c("Ethnicity", "IMD Quintile"),
    unique(as.character(legend_dat$labels))
  )

  legend_left <- if (length(groups_left) > 0) {
    cowplot::get_legend(
      forest_over_time_plot_all_seasons(
        legend_dat %>% dplyr::filter(as.character(.data$labels) %in% groups_left),
        pathogen = legend_pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        show_disruption_legend = TRUE,
        log_y = TRUE
      ) +
        ggplot2::theme(legend.position = "left", legend.title = element_blank())
    )
  } else {
    NULL
  }

  legend_mid <- if (length(groups_mid) > 0) {
    cowplot::get_legend(
      forest_over_time_plot_all_seasons(
        legend_dat %>% dplyr::filter(as.character(.data$labels) %in% groups_mid),
        pathogen = legend_pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        show_disruption_legend = FALSE,
        log_y = TRUE
      ) +
        ggplot2::theme(legend.position = "left", legend.title = element_blank())
    )
  } else {
    NULL
  }

  list(left = legend_left, mid = legend_mid)
}

run_cohort_condensed_key_vars <- function(
    cohort,
    model_type = "ethnicity_ses",
    out_root = here::here(
      "post_check", "plots", "primary_analyses", "condensed_models_key_vars"
    )
) {
  assign("cohort", cohort, envir = .GlobalEnv)

  pathogen <- "rsv"
  df_input <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  rsv_plots <- make_facet_outcome_plots_key_vars(df_input, df_dummy, pathogen, model_type)

  pathogen <- "flu"
  df_input <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  flu_plots <- make_facet_outcome_plots_key_vars(df_input, df_dummy, pathogen, model_type)

  pathogen <- "covid"
  df_input <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  covid_plots <- make_facet_outcome_plots_key_vars(df_input, df_dummy, pathogen, model_type)

  legend_dat <- dplyr::bind_rows(
    forest_year_further_mult_key_vars(
      df_input, df_dummy, "covid", model_type, "Mild"
    ),
    forest_year_further_mult_key_vars(
      df_input, df_dummy, "covid", model_type, "Severe"
    )
  ) %>%
    dplyr::filter(.data$codelist_type %in% c("reference", "specific"))

  shared_legends <- build_shared_legends_key_vars(legend_dat, model_type)

  specific_condensed <- assemble_condensed_figure(
    rsv_plots$specific,
    flu_plots$specific,
    covid_plots$specific,
    shared_legends$left,
    shared_legends$mid
  )
  sensitive_condensed <- assemble_condensed_figure(
    rsv_plots$sensitive,
    flu_plots$sensitive,
    covid_plots$sensitive,
    shared_legends$left,
    shared_legends$mid
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(cohort, "_", model_type, "_further_specific_mild_vs_severe_key_vars.png")
    ),
    specific_condensed,
    width = CONDENSED_FIG_WIDTH,
    height = CONDENSED_FIG_HEIGHT,
    bg = "white"
  )
  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(cohort, "_", model_type, "_further_sensitive_mild_vs_severe_key_vars.png")
    ),
    sensitive_condensed,
    width = CONDENSED_FIG_WIDTH,
    height = CONDENSED_FIG_HEIGHT,
    bg = "white"
  )

  invisible(
    list(
      specific = specific_condensed,
      sensitive = sensitive_condensed,
      per_virus = list(rsv = rsv_plots, flu = flu_plots, covid = covid_plots)
    )
  )
}

# ---- Base vs further, selected seasons ----------------------------------------

make_facet_outcome_plots_key_vars_seasons_base_vs_further <- function(
    df_min,
    df_full,
    df_dummy,
    pathogen,
    model_type,
    seasons = c("2017_18", "2018_19", "2020_21")
) {
  pathogen_seasons <- seasons_for_pathogen_compare(pathogen, seasons)
  years_include <- seasons_to_years(pathogen_seasons)

  mild_dat <- forest_year_base_vs_further_mult_key_vars(
    df_min, df_full, df_dummy, pathogen, model_type, "Mild", seasons
  )
  severe_dat <- forest_year_base_vs_further_mult_key_vars(
    df_min, df_full, df_dummy, pathogen, model_type, "Severe", seasons
  )
  both_dat <- dplyr::bind_rows(mild_dat, severe_dat)

  plot_one <- function(phenotype) {
    plot_dat <- both_dat %>%
      dplyr::filter(
        tolower(.data$codelist_type) %in% c("reference", tolower(phenotype))
      )
    if (nrow(plot_dat) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    forest_over_time_plot_compare(
      forest_data = plot_dat,
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      show_disruption_legend = FALSE,
      years_include = years_include,
      adjustment_dodge_width = 0.58,
      level_jitter_width = 0.12
    ) +
      ggplot2::theme(legend.position = "none")
  }

  list(
    specific = plot_one("specific"),
    sensitive = plot_one("sensitive")
  )
}

# Base vs further: minimally and fully adjusted connected by a line per level (no CIs).
make_facet_outcome_plots_key_vars_seasons_base_vs_further_stacked <- function(
    df_min,
    df_full,
    df_dummy,
    pathogen,
    model_type,
    seasons = c("2017_18", "2018_19", "2020_21")
) {
  pathogen_seasons <- seasons_for_pathogen_compare(pathogen, seasons)
  years_include <- seasons_to_years(pathogen_seasons)

  mild_dat <- forest_year_base_vs_further_mult_key_vars(
    df_min, df_full, df_dummy, pathogen, model_type, "Mild", seasons
  )
  severe_dat <- forest_year_base_vs_further_mult_key_vars(
    df_min, df_full, df_dummy, pathogen, model_type, "Severe", seasons
  )
  both_dat <- dplyr::bind_rows(mild_dat, severe_dat)

  plot_one <- function(phenotype) {
    plot_dat <- both_dat %>%
      dplyr::filter(
        tolower(.data$codelist_type) %in% c("reference", tolower(phenotype))
      )
    if (nrow(plot_dat) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    forest_over_time_plot_compare(
      forest_data = plot_dat,
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      show_disruption_legend = FALSE,
      show_ci = FALSE,
      years_include = years_include,
      adjustment_layout = "stack",
      level_jitter_width = 0.28
    ) +
      ggplot2::theme(legend.position = "none")
  }

  list(
    specific = plot_one("specific"),
    sensitive = plot_one("sensitive")
  )
}

# Further / minimal RR ratio by season (1 = no attenuation from further adjustment).
# Pass seasons = NULL for all seasons (same x-axis as further_results_condensed_key_vars).
make_facet_outcome_plots_key_vars_seasons_adjustment_ratio <- function(
    df_min,
    df_full,
    df_dummy,
    pathogen,
    model_type,
    seasons = c("2017_18", "2018_19", "2020_21"),
    y_lab = "Ratio of RRs (further / minimal)"
) {
  years_include <- if (is.null(seasons)) {
    NULL
  } else {
    seasons_to_years(seasons_for_pathogen_compare(pathogen, seasons))
  }

  mild_dat <- forest_year_base_vs_further_mult_key_vars(
    df_min, df_full, df_dummy, pathogen, model_type, "Mild", seasons
  )
  severe_dat <- forest_year_base_vs_further_mult_key_vars(
    df_min, df_full, df_dummy, pathogen, model_type, "Severe", seasons
  )
  both_dat <- dplyr::bind_rows(mild_dat, severe_dat)

  plot_one <- function(phenotype) {
    plot_dat <- both_dat %>%
      dplyr::filter(
        tolower(.data$codelist_type) %in% c("reference", tolower(phenotype))
      )
    if (nrow(plot_dat) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    if (is.null(years_include)) {
      ratio_dat <- collapse_base_further_to_ratio(plot_dat)
      forest_over_time_plot_all_seasons(
        forest_data = ratio_dat,
        pathogen = pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        show_disruption_legend = FALSE,
        y_lab = y_lab,
        log_y = FALSE
      )
    } else {
      forest_over_time_plot_compare_ratio(
        forest_data = plot_dat,
        pathogen = pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        show_disruption_legend = FALSE,
        years_include = years_include,
        jitter_width = 0.2,
        level_jitter_width = 0.12,
        y_lab = y_lab
      )
    } +
      ggplot2::theme(legend.position = "none")
  }

  list(
    specific = plot_one("specific"),
    sensitive = plot_one("sensitive")
  )
}

make_facet_outcome_plots_key_vars_adjustment_ratio <- function(
    df_min,
    df_full,
    df_dummy,
    pathogen,
    model_type,
    y_lab = "Ratio of RRs (further / minimal)"
) {
  make_facet_outcome_plots_key_vars_seasons_adjustment_ratio(
    df_min = df_min,
    df_full = df_full,
    df_dummy = df_dummy,
    pathogen = pathogen,
    model_type = model_type,
    seasons = NULL,
    y_lab = y_lab
  )
}

# All-season ratio panels: same plot builder as further_model_results_condensed.R.
make_facet_outcome_plots_key_vars_ratio_all_seasons <- function(
    df_min,
    df_full,
    df_dummy,
    pathogen,
    model_type,
    y_lab = "Ratio of RRs (further / minimal)"
) {
  mild_dat <- forest_year_base_vs_further_mult_key_vars(
    df_min, df_full, df_dummy, pathogen, model_type, "Mild", seasons = NULL
  )
  severe_dat <- forest_year_base_vs_further_mult_key_vars(
    df_min, df_full, df_dummy, pathogen, model_type, "Severe", seasons = NULL
  )
  both_dat <- dplyr::bind_rows(mild_dat, severe_dat)

  plot_one <- function(phenotype) {
    plot_dat <- both_dat %>%
      dplyr::filter(
        tolower(.data$codelist_type) %in% c("reference", tolower(phenotype))
      )
    if (nrow(plot_dat) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    ratio_dat <- collapse_base_further_to_ratio(plot_dat)
    forest_over_time_plot(
      forest_data = ratio_dat,
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      label_levels = FALSE,
      show_disruption_legend = FALSE,
      y_lab = y_lab,
      log_y = FALSE
    ) +
      ggplot2::theme(legend.position = "none")
  }

  list(
    specific = plot_one("specific"),
    sensitive = plot_one("sensitive")
  )
}

# Cowplot layout from further_model_results_condensed.R / further_results_condensed_key_vars.R.
assemble_condensed_figure_standard <- function(
    rsv_plot,
    flu_plot,
    covid_plot,
    legend_left,
    legend_mid = NULL
) {
  covid_plot_with_mid <- covid_plot
  if (!is.null(legend_mid)) {
    covid_plot_with_mid <- cowplot::ggdraw(covid_plot) +
      cowplot::draw_grob(legend_mid, x = 0.44, y = 0.08, width = 0.12, height = 0.84)
  }

  covid_row <- cowplot::plot_grid(
    NULL, legend_left, covid_plot_with_mid, NULL,
    ncol = 4,
    rel_widths = c(-0.1, 0.9, 5.1, -0.16),
    align = "h",
    axis = "tb"
  )

  combined <- cowplot::plot_grid(
    NULL,
    rsv_plot,
    NULL,
    flu_plot,
    NULL,
    covid_row,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(0.05, 1, -0.03, 1.25, -0.03, 1.35)
  )

  cowplot::ggdraw(combined) +
    cowplot::draw_label(
      "A. Mild", x = 0.275, y = 1, hjust = 0.5, vjust = 1.5,
      fontface = "bold", size = 9
    ) +
    cowplot::draw_label(
      "B. Severe", x = 0.74, y = 1, hjust = 0.5, vjust = 1.5,
      fontface = "bold", size = 9
    )
}

build_shared_legends_key_vars_ratio_all_seasons <- function(
    legend_dat,
    model_type,
    legend_pathogen = "covid",
    y_lab = "Ratio of RRs (further / minimal)"
) {
  groups_left <- intersect("Age Group", unique(as.character(legend_dat$labels)))
  groups_mid <- intersect(
    c("Ethnicity", "IMD Quintile"),
    unique(as.character(legend_dat$labels))
  )

  legend_left <- if (length(groups_left) > 0) {
    cowplot::get_legend(
      forest_over_time_plot(
        collapse_base_further_to_ratio(
          legend_dat %>% dplyr::filter(as.character(.data$labels) %in% groups_left)
        ),
        pathogen = legend_pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = FALSE,
        show_disruption_legend = TRUE,
        y_lab = y_lab,
        log_y = FALSE
      ) +
        ggplot2::theme(legend.position = "left", legend.title = element_blank())
    )
  } else {
    NULL
  }

  legend_mid <- if (length(groups_mid) > 0) {
    cowplot::get_legend(
      forest_over_time_plot(
        collapse_base_further_to_ratio(
          legend_dat %>% dplyr::filter(as.character(.data$labels) %in% groups_mid)
        ),
        pathogen = legend_pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = FALSE,
        show_disruption_legend = FALSE,
        y_lab = y_lab,
        log_y = FALSE
      ) +
        ggplot2::theme(legend.position = "left", legend.title = element_blank())
    )
  } else {
    NULL
  }

  list(left = legend_left, mid = legend_mid)
}

build_shared_legends_base_vs_further <- function(
    legend_dat,
    legend_pathogen = "covid",
    seasons = c("2017_18", "2018_19", "2020_21")
) {
  years_include <- seasons_to_years(seasons)
  labels_present <- unique(as.character(legend_dat$labels))

  legend_theme <- ggplot2::theme(
    legend.position = "left",
    legend.justification = c(0, 0.5),
    legend.box.just = "left",
    legend.box = "vertical",
    legend.text = ggplot2::element_text(size = 7.5)
  )

  legend_adj <- legend_dat %>%
    dplyr::filter(!is.na(.data$adjustment)) %>%
    dplyr::distinct(.data$adjustment, .data$outcome_type, .keep_all = TRUE)

  legend_src <- if (nrow(legend_adj) > 0) {
    legend_adj
  } else {
    legend_dat %>% dplyr::slice(1)
  }

  legend_adj_grob <- if (nrow(legend_adj) > 0) {
    cowplot::get_legend(
      forest_over_time_plot_compare(
        legend_adj,
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        years_include = years_include,
        show_disruption_legend = FALSE
      ) +
        legend_theme +
        ggplot2::guides(shape = "none", fill = "none", colour = "none")
    )
  } else {
    NULL
  }

  legend_age_grob <- if ("Age Group" %in% labels_present) {
    cowplot::get_legend(
      forest_over_time_plot_compare(
        legend_dat %>% dplyr::filter(as.character(.data$labels) == "Age Group"),
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        years_include = years_include,
        show_disruption_legend = FALSE
      ) +
        legend_theme +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::guides(fill = "none", alpha = "none")
    )
  } else {
    NULL
  }

  legend_disruption_grob <- cowplot::get_legend(
    forest_over_time_plot_compare(
      legend_src,
      pathogen = legend_pathogen,
      model_type = "ethnicity_ses",
      facet_outcome = TRUE,
      years_include = years_include,
      show_disruption_legend = TRUE
    ) +
      legend_theme +
      ggplot2::guides(shape = "none", colour = "none", alpha = "none")
  )

  legend_left_parts <- list(legend_adj_grob, legend_age_grob, legend_disruption_grob)
  legend_left_parts <- legend_left_parts[!vapply(legend_left_parts, is.null, logical(1))]

  legend_left <- if (length(legend_left_parts) == 0) {
    NULL
  } else if (length(legend_left_parts) == 1) {
    legend_left_parts[[1]]
  } else {
    cowplot::plot_grid(
      plotlist = legend_left_parts,
      ncol = 1,
      align = "v",
      axis = "l"
    )
  }

  legend_eth <- if ("Ethnicity" %in% labels_present) {
    cowplot::get_legend(
      forest_over_time_plot_compare(
        legend_dat %>% dplyr::filter(as.character(.data$labels) == "Ethnicity"),
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        years_include = years_include,
        show_disruption_legend = FALSE
      ) +
        legend_theme +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::guides(fill = "none", alpha = "none")
    )
  } else {
    NULL
  }

  legend_imd <- if ("IMD Quintile" %in% labels_present) {
    cowplot::get_legend(
      forest_over_time_plot_compare(
        legend_dat %>% dplyr::filter(as.character(.data$labels) == "IMD Quintile"),
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        years_include = years_include,
        show_disruption_legend = FALSE
      ) +
        legend_theme +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::guides(fill = "none", alpha = "none")
    )
  } else {
    NULL
  }

  list(
    left = legend_left,
    mid = list(eth = legend_eth, imd = legend_imd)
  )
}

build_shared_legends_adjustment_ratio <- function(
    legend_dat,
    legend_pathogen = "covid",
    seasons = c("2017_18", "2018_19", "2020_21"),
    y_lab = "Ratio of RRs (further / minimal)"
) {
  years_include <- if (is.null(seasons)) {
    NULL
  } else {
    seasons_to_years(seasons)
  }
  labels_present <- unique(as.character(legend_dat$labels))

  legend_theme <- ggplot2::theme(
    legend.position = "left",
    legend.justification = c(0, 0.5),
    legend.box.just = "left",
    legend.box = "vertical",
    legend.text = ggplot2::element_text(size = 7.5),
    legend.key.width = ggplot2::unit(1.1, "lines"),
    legend.key.height = ggplot2::unit(0.85, "lines"),
    legend.spacing.y = ggplot2::unit(0.15, "lines")
  )

  legend_src <- legend_dat %>% dplyr::slice(1)

  legend_age_grob <- if ("Age Group" %in% labels_present) {
    cowplot::get_legend(
      forest_over_time_plot_compare_ratio(
        legend_dat %>% dplyr::filter(as.character(.data$labels) == "Age Group"),
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        years_include = years_include,
        show_disruption_legend = FALSE,
        y_lab = y_lab
      ) +
        legend_theme +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::guides(fill = "none")
    )
  } else {
    NULL
  }

  legend_disruption_grob <- cowplot::get_legend(
    forest_over_time_plot_compare_ratio(
      legend_src,
      pathogen = legend_pathogen,
      model_type = "ethnicity_ses",
      facet_outcome = TRUE,
      years_include = years_include,
      show_disruption_legend = TRUE,
      y_lab = y_lab
    ) +
      legend_theme +
      ggplot2::guides(shape = "none", colour = "none", alpha = "none")
  )

  legend_left_parts <- list(legend_age_grob, legend_disruption_grob)
  legend_left_parts <- legend_left_parts[!vapply(legend_left_parts, is.null, logical(1))]

  legend_left <- if (length(legend_left_parts) == 0) {
    NULL
  } else if (length(legend_left_parts) == 1) {
    legend_left_parts[[1]]
  } else {
    cowplot::plot_grid(
      plotlist = legend_left_parts,
      ncol = 1,
      align = "v",
      axis = "l"
    )
  }

  legend_eth <- if ("Ethnicity" %in% labels_present) {
    cowplot::get_legend(
      forest_over_time_plot_compare_ratio(
        legend_dat %>% dplyr::filter(as.character(.data$labels) == "Ethnicity"),
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        years_include = years_include,
        show_disruption_legend = FALSE,
        y_lab = y_lab
      ) +
        legend_theme +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::guides(fill = "none")
    )
  } else {
    NULL
  }

  legend_imd <- if ("IMD Quintile" %in% labels_present) {
    cowplot::get_legend(
      forest_over_time_plot_compare_ratio(
        legend_dat %>% dplyr::filter(as.character(.data$labels) == "IMD Quintile"),
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        years_include = years_include,
        show_disruption_legend = FALSE,
        y_lab = y_lab
      ) +
        legend_theme +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::guides(fill = "none")
    )
  } else {
    NULL
  }

  list(
    left = legend_left,
    mid = list(eth = legend_eth, imd = legend_imd)
  )
}

run_cohort_condensed_key_vars_seasons_base_vs_further <- function(
    cohort,
    seasons = c("2017_18", "2018_19", "2020_21"),
    model_type = "ethnicity_ses",
    out_root = here::here(
      "post_check", "plots", "primary_analyses", "forest_models_by_virus"
    )
) {
  assign("cohort", cohort, envir = .GlobalEnv)
  season_slug <- paste(gsub("_", "", seasons), collapse = "_")

  pathogen <- "rsv"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  rsv_plots <- make_facet_outcome_plots_key_vars_seasons_base_vs_further(
    df_min, df_full, df_dummy, pathogen, model_type, seasons
  )

  pathogen <- "flu"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  flu_plots <- make_facet_outcome_plots_key_vars_seasons_base_vs_further(
    df_min, df_full, df_dummy, pathogen, model_type, seasons
  )

  pathogen <- "covid"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  covid_plots <- make_facet_outcome_plots_key_vars_seasons_base_vs_further(
    df_min, df_full, df_dummy, pathogen, model_type, seasons
  )

  legend_dat <- dplyr::bind_rows(
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, "covid", model_type, "Mild", seasons
    ),
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, "covid", model_type, "Severe", seasons
    )
  ) %>%
    dplyr::filter(.data$codelist_type %in% c("reference", "specific"))

  shared_legends <- build_shared_legends_base_vs_further(
    legend_dat,
    legend_pathogen = "covid",
    seasons = seasons_for_pathogen_compare("covid", seasons)
  )

  specific_condensed <- assemble_condensed_figure(
    rsv_plots$specific,
    flu_plots$specific,
    covid_plots$specific,
    shared_legends$left,
    shared_legends$mid
  )
  sensitive_condensed <- assemble_condensed_figure(
    rsv_plots$sensitive,
    flu_plots$sensitive,
    covid_plots$sensitive,
    shared_legends$left,
    shared_legends$mid
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type, "_base_vs_further_condensed_",
        season_slug, "_specific_mild_vs_severe.png"
      )
    ),
    specific_condensed,
    width = CONDENSED_FIG_WIDTH,
    height = CONDENSED_FIG_HEIGHT,
    bg = "white"
  )
  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type, "_base_vs_further_condensed_",
        season_slug, "_sensitive_mild_vs_severe.png"
      )
    ),
    sensitive_condensed,
    width = CONDENSED_FIG_WIDTH,
    height = CONDENSED_FIG_HEIGHT,
    bg = "white"
  )

  invisible(
    list(
      specific = specific_condensed,
      sensitive = sensitive_condensed,
      per_virus = list(rsv = rsv_plots, flu = flu_plots, covid = covid_plots)
    )
  )
}

run_cohort_condensed_key_vars_seasons_base_vs_further_stacked <- function(
    cohort,
    seasons = c("2017_18", "2018_19", "2020_21"),
    model_type = "ethnicity_ses",
    out_root = here::here(
      "post_check", "plots", "primary_analyses", "forest_models_by_virus"
    ),
    fig_width = CONDENSED_FIG_WIDTH,
    fig_height = CONDENSED_FIG_HEIGHT
) {
  assign("cohort", cohort, envir = .GlobalEnv)
  season_slug <- paste(gsub("_", "", seasons), collapse = "_")

  pathogen <- "rsv"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  rsv_plots <- make_facet_outcome_plots_key_vars_seasons_base_vs_further_stacked(
    df_min, df_full, df_dummy, pathogen, model_type, seasons
  )

  pathogen <- "flu"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  flu_plots <- make_facet_outcome_plots_key_vars_seasons_base_vs_further_stacked(
    df_min, df_full, df_dummy, pathogen, model_type, seasons
  )

  pathogen <- "covid"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  covid_plots <- make_facet_outcome_plots_key_vars_seasons_base_vs_further_stacked(
    df_min, df_full, df_dummy, pathogen, model_type, seasons
  )

  legend_dat <- dplyr::bind_rows(
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, "covid", model_type, "Mild", seasons
    ),
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, "covid", model_type, "Severe", seasons
    )
  ) %>%
    dplyr::filter(.data$codelist_type %in% c("reference", "specific"))

  shared_legends <- build_shared_legends_base_vs_further_stacked(
    legend_dat,
    legend_pathogen = "covid",
    seasons = seasons_for_pathogen_compare("covid", seasons)
  )

  specific_condensed <- assemble_condensed_figure(
    rsv_plots$specific,
    flu_plots$specific,
    covid_plots$specific,
    shared_legends$left,
    shared_legends$mid
  )
  sensitive_condensed <- assemble_condensed_figure(
    rsv_plots$sensitive,
    flu_plots$sensitive,
    covid_plots$sensitive,
    shared_legends$left,
    shared_legends$mid
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type, "_base_vs_further_stacked_",
        season_slug, "_specific_mild_vs_severe.png"
      )
    ),
    specific_condensed,
    width = fig_width,
    height = fig_height,
    bg = "white"
  )
  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type, "_base_vs_further_stacked_",
        season_slug, "_sensitive_mild_vs_severe.png"
      )
    ),
    sensitive_condensed,
    width = fig_width,
    height = fig_height,
    bg = "white"
  )

  invisible(
    list(
      specific = specific_condensed,
      sensitive = sensitive_condensed,
      per_virus = list(rsv = rsv_plots, flu = flu_plots, covid = covid_plots)
    )
  )
}

stacked_compare_legend_plot <- function(
    legend_dat,
    legend_pathogen,
    years_include,
    legend_theme,
    show_disruption_legend = FALSE,
    guides_keep = c("shape")
) {
  p <- forest_over_time_plot_compare(
    legend_dat,
    pathogen = legend_pathogen,
    model_type = "ethnicity_ses",
    facet_outcome = FALSE,
    show_disruption_legend = show_disruption_legend,
    show_ci = FALSE,
    years_include = years_include,
    adjustment_layout = "stack"
  ) +
    legend_theme +
    ggplot2::theme(legend.title = element_blank())

  guide_drop <- setdiff(c("shape", "alpha", "fill", "colour"), guides_keep)
  guide_specs <- stats::setNames(
    rep(list("none"), length(guide_drop)),
    guide_drop
  )
  p + ggplot2::guides(!!!guide_specs)
}

build_shared_legends_base_vs_further_stacked <- function(
    legend_dat,
    legend_pathogen = "covid",
    seasons = c("2017_18", "2018_19", "2020_21")
) {
  years_include <- seasons_to_years(seasons)
  labels_present <- unique(as.character(legend_dat$labels))

  legend_theme <- ggplot2::theme(
    legend.position = "left",
    legend.justification = c(0, 0.5),
    legend.box.just = "left",
    legend.box = "vertical",
    legend.text = ggplot2::element_text(size = 7.5),
    legend.key.width = ggplot2::unit(1.1, "lines"),
    legend.key.height = ggplot2::unit(0.85, "lines"),
    legend.spacing.y = ggplot2::unit(0.15, "lines")
  )

  legend_adj <- legend_dat %>%
    dplyr::filter(!is.na(.data$adjustment)) %>%
    dplyr::distinct(.data$adjustment, .keep_all = TRUE)

  legend_src <- if (nrow(legend_adj) > 0) legend_adj else legend_dat %>% dplyr::slice(1)

  legend_grob <- function(dat, guides_keep, show_disruption = FALSE) {
    if (nrow(dat) == 0) {
      return(NULL)
    }
    cowplot::get_legend(
      stacked_compare_legend_plot(
        dat,
        legend_pathogen = legend_pathogen,
        years_include = years_include,
        legend_theme = legend_theme,
        show_disruption_legend = show_disruption,
        guides_keep = guides_keep
      )
    )
  }

  # Match the legacy condensed-figure legend layout:
  # - left column: Adjustment + Age Group + Disruption
  # - mid column (overlay on COVID): Ethnicity + IMD Quintile
  legend_adj_grob <- if (nrow(legend_adj) > 0) legend_grob(legend_adj, guides_keep = "alpha") else NULL
  legend_age_grob <- if ("Age Group" %in% labels_present) {
    legend_grob(
      legend_dat %>% dplyr::filter(as.character(.data$labels) == "Age Group"),
      guides_keep = "shape"
    )
  } else {
    NULL
  }
  legend_disruption_grob <- legend_grob(legend_src, guides_keep = "fill", show_disruption = TRUE)

  legend_left_parts <- list(legend_adj_grob, legend_age_grob, legend_disruption_grob)
  legend_left_parts <- legend_left_parts[!vapply(legend_left_parts, is.null, logical(1))]

  legend_left <- if (length(legend_left_parts) == 0) {
    NULL
  } else if (length(legend_left_parts) == 1) {
    legend_left_parts[[1]]
  } else {
    cowplot::plot_grid(
      plotlist = legend_left_parts,
      ncol = 1,
      align = "v",
      axis = "l",
      rel_heights = rep(1, length(legend_left_parts))
    )
  }

  legend_eth <- if ("Ethnicity" %in% labels_present) {
    legend_grob(
      legend_dat %>% dplyr::filter(as.character(.data$labels) == "Ethnicity"),
      guides_keep = "shape"
    )
  } else {
    NULL
  }
  legend_imd <- if ("IMD Quintile" %in% labels_present) {
    legend_grob(
      legend_dat %>% dplyr::filter(as.character(.data$labels) == "IMD Quintile"),
      guides_keep = "shape"
    )
  } else {
    NULL
  }

  list(left = legend_left, mid = list(eth = legend_eth, imd = legend_imd))
}

run_cohort_condensed_key_vars_seasons_adjustment_ratio <- function(
    cohort,
    seasons = c("2017_18", "2018_19", "2020_21"),
    model_type = "ethnicity_ses",
    out_root = here::here(
      "post_check", "plots", "primary_analyses", "forest_model_ratios_by_virus"
    ),
    y_lab = "Ratio of RRs (further / minimal)"
) {
  assign("cohort", cohort, envir = .GlobalEnv)
  season_slug <- paste(gsub("_", "", seasons), collapse = "_")

  pathogen <- "rsv"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  rsv_plots <- make_facet_outcome_plots_key_vars_seasons_adjustment_ratio(
    df_min, df_full, df_dummy, pathogen, model_type, seasons, y_lab = y_lab
  )

  pathogen <- "flu"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  flu_plots <- make_facet_outcome_plots_key_vars_seasons_adjustment_ratio(
    df_min, df_full, df_dummy, pathogen, model_type, seasons, y_lab = y_lab
  )

  pathogen <- "covid"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  covid_plots <- make_facet_outcome_plots_key_vars_seasons_adjustment_ratio(
    df_min, df_full, df_dummy, pathogen, model_type, seasons, y_lab = y_lab
  )

  legend_dat <- dplyr::bind_rows(
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, "covid", model_type, "Mild", seasons
    ),
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, "covid", model_type, "Severe", seasons
    )
  ) %>%
    dplyr::filter(.data$codelist_type %in% c("reference", "specific"))

  shared_legends <- build_shared_legends_adjustment_ratio(
    legend_dat,
    legend_pathogen = "covid",
    seasons = seasons_for_pathogen_compare("covid", seasons),
    y_lab = y_lab
  )

  specific_condensed <- assemble_condensed_figure(
    rsv_plots$specific,
    flu_plots$specific,
    covid_plots$specific,
    shared_legends$left,
    shared_legends$mid
  )
  sensitive_condensed <- assemble_condensed_figure(
    rsv_plots$sensitive,
    flu_plots$sensitive,
    covid_plots$sensitive,
    shared_legends$left,
    shared_legends$mid
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type, "_adjustment_ratio_condensed_",
        season_slug, "_specific_mild_vs_severe.png"
      )
    ),
    specific_condensed,
    width = CONDENSED_FIG_WIDTH,
    height = CONDENSED_FIG_HEIGHT,
    bg = "white"
  )
  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type, "_adjustment_ratio_condensed_",
        season_slug, "_sensitive_mild_vs_severe.png"
      )
    ),
    sensitive_condensed,
    width = CONDENSED_FIG_WIDTH,
    height = CONDENSED_FIG_HEIGHT,
    bg = "white"
  )

  invisible(
    list(
      specific = specific_condensed,
      sensitive = sensitive_condensed,
      per_virus = list(rsv = rsv_plots, flu = flu_plots, covid = covid_plots)
    )
  )
}

# All seasons: plot builder and cowplot layout from further_model_results_condensed.R.
run_cohort_condensed_key_vars_adjustment_ratio <- function(
    cohort,
    model_type = "ethnicity_ses",
    out_root = here::here(
      "post_check", "plots", "primary_analyses", "condensed_model_ratios_key_vars"
    ),
    y_lab = "Ratio of RRs (further / minimal)"
) {
  assign("cohort", cohort, envir = .GlobalEnv)

  pathogen <- "rsv"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  rsv_plots <- make_facet_outcome_plots_key_vars_ratio_all_seasons(
    df_min, df_full, df_dummy, pathogen, model_type, y_lab = y_lab
  )

  pathogen <- "flu"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  flu_plots <- make_facet_outcome_plots_key_vars_ratio_all_seasons(
    df_min, df_full, df_dummy, pathogen, model_type, y_lab = y_lab
  )

  pathogen <- "covid"
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  covid_plots <- make_facet_outcome_plots_key_vars_ratio_all_seasons(
    df_min, df_full, df_dummy, pathogen, model_type, y_lab = y_lab
  )

  legend_dat <- dplyr::bind_rows(
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, "covid", model_type, "Mild", seasons = NULL
    ),
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, "covid", model_type, "Severe", seasons = NULL
    )
  ) %>%
    dplyr::filter(.data$codelist_type %in% c("reference", "specific"))

  shared_legends <- build_shared_legends_key_vars_ratio_all_seasons(
    legend_dat,
    model_type = model_type,
    legend_pathogen = "covid",
    y_lab = y_lab
  )

  specific_condensed <- assemble_condensed_figure_standard(
    rsv_plots$specific,
    flu_plots$specific,
    covid_plots$specific,
    shared_legends$left,
    shared_legends$mid
  )
  sensitive_condensed <- assemble_condensed_figure_standard(
    rsv_plots$sensitive,
    flu_plots$sensitive,
    covid_plots$sensitive,
    shared_legends$left,
    shared_legends$mid
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type,
        "_adjustment_ratio_specific_mild_vs_severe_key_vars.png"
      )
    ),
    specific_condensed,
    width = CONDENSED_FIG_WIDTH,
    height = CONDENSED_FIG_HEIGHT,
    bg = "white"
  )
  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type,
        "_adjustment_ratio_sensitive_mild_vs_severe_key_vars.png"
      )
    ),
    sensitive_condensed,
    width = CONDENSED_FIG_WIDTH,
    height = CONDENSED_FIG_HEIGHT,
    bg = "white"
  )

  invisible(
    list(
      specific = specific_condensed,
      sensitive = sensitive_condensed,
      per_virus = list(rsv = rsv_plots, flu = flu_plots, covid = covid_plots)
    )
  )
}
