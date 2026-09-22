# Covariate-row condensed forest layout:
# columns mild | severe; blocks Age / IMD / Ethnicity; sub-rows RSV / flu / COVID.
# Level colours (not shapes); ethnicity Unknown excluded via prepare_forest_plot_data().

COVARIATE_ROW_LEGEND_POINT_SIZE <- 0.6
COVARIATE_ROW_LEGEND_TITLE_SIZE <- 11
COVARIATE_ROW_LEGEND_TEXT_SIZE <- 10
COVARIATE_ROW_DISRUPTION_LEGEND_TEXT_SIZE <- 14
COVARIATE_ROW_POINTRANGE_FATTEN <- 4
COVARIATE_ROW_POINTRANGE_LINEWIDTH <- 0.6
COVARIATE_ROW_FIG_WIDTH <- 8.5
COVARIATE_ROW_FIG_HEIGHT <- 14

CONDENSED_COVARIATE_ROW_OUT_ROOT <- here::here(
  "post_check", "plots", "primary_analyses", "condensed_models_key_vars"
)

collect_further_pathogen_key_vars <- function(
    cohort,
    pathogen,
    model_type,
    drop_unknown_ethnicity = TRUE,
    seasons = NULL
) {
  df_input <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  dat <- dplyr::bind_rows(
    forest_year_further_mult_key_vars(
      df_input, df_dummy, pathogen, model_type, "Mild", return_data = TRUE
    ),
    forest_year_further_mult_key_vars(
      df_input, df_dummy, pathogen, model_type, "Severe", return_data = TRUE
    )
  )
  dat <- prepare_forest_plot_data(
    dat,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    pathogen = pathogen
  )
  if (!is.null(seasons)) {
    dat <- filter_forest_to_seasons(
      dat,
      seasons_for_pathogen_compare(pathogen, seasons)
    )
  }
  dat
}

filter_forest_phenotype <- function(dat, phenotype) {
  dat %>% dplyr::filter(.data$codelist_type %in% c("reference", phenotype))
}

# TRUE when the phenotype has real fitted estimates (not too-few reference shells).
forest_phenotype_has_estimates <- function(dat, phenotype) {
  if (is_empty_forest_data(dat) || !"codelist_type" %in% names(dat)) {
    return(FALSE)
  }
  ph_rows <- dat %>% dplyr::filter(.data$codelist_type == .env$phenotype)
  if (nrow(ph_rows) == 0L || !"estimate" %in% names(ph_rows)) {
    return(FALSE)
  }
  est <- ph_rows$estimate
  lo <- if ("conf.low" %in% names(ph_rows)) ph_rows$conf.low else rep(NA_real_, length(est))
  hi <- if ("conf.high" %in% names(ph_rows)) ph_rows$conf.high else rep(NA_real_, length(est))
  any(
    is.finite(est) & !(
      is.finite(lo) & is.finite(hi) &
        abs(est - 1) < 1e-3 & abs(lo - 1) < 1e-3 & abs(hi - 1) < 1e-3
    ),
    na.rm = TRUE
  )
}

filter_forest_covariate <- function(dat, covariate) {
  if (is_empty_forest_data(dat) || !"labels" %in% names(dat)) {
    return(dat)
  }
  dat %>% dplyr::filter(as.character(.data$labels) == covariate)
}

plot_covariate_pathogen_panel <- function(
    plot_dat,
    pathogen,
    model_type,
    covariate,
    show_x = FALSE,
    seasons = NULL
) {
  dat <- filter_forest_covariate(plot_dat, covariate)
  if (identical(pathogen, "covid")) {
    dat <- drop_forest_points_before_year(dat, FOREST_COVID_MIN_YEAR)
  }
  if (is_empty_forest_data(dat)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  level_order <- ordered_forest_covariate_levels(
    dat, covariate, model_type, pathogen
  )
  level_colour_values <- covariate_forest_level_colours(covariate, level_order)

  if (is.null(seasons)) {
    p <- forest_over_time_plot_all_seasons(
      forest_data = dat,
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      show_disruption_legend = FALSE,
      show_disruption_shading = !identical(pathogen, "covid"),
      disruption_legend_label = FOREST_COVID_DISRUPTION_LEGEND_LABEL,
      level_colour_values = level_colour_values,
      level_colour_title = covariate,
      drop_unknown_ethnicity = FALSE,
      pointrange_fatten = COVARIATE_ROW_POINTRANGE_FATTEN,
      pointrange_linewidth = COVARIATE_ROW_POINTRANGE_LINEWIDTH,
      log_y = TRUE,
      y_lab = forest_pathogen_y_lab(pathogen)
    ) +
      shared_forest_season_x_scale()
  } else {
    pathogen_seasons <- seasons_for_pathogen_compare(pathogen, seasons)
    p <- forest_over_time_plot(
      forest_data = dat,
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      label_levels = FALSE,
      show_disruption_legend = FALSE,
      show_disruption_shading = !identical(pathogen, "covid"),
      disruption_legend_label = FOREST_COVID_DISRUPTION_LEGEND_LABEL,
      level_colour_values = level_colour_values,
      level_colour_title = covariate,
      drop_unknown_ethnicity = FALSE,
      seasons = pathogen_seasons,
      equal_season_widths = TRUE,
      pointrange_fatten = COVARIATE_ROW_POINTRANGE_FATTEN,
      pointrange_linewidth = COVARIATE_ROW_POINTRANGE_LINEWIDTH,
      log_y = TRUE,
      y_lab = forest_pathogen_y_lab(pathogen)
    )
  }

  p +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = if (isTRUE(show_x)) {
        ggplot2::element_text(size = FOREST_AXIS_TEXT_X_SIZE)
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (isTRUE(show_x)) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      },
      panel.spacing.x = ggplot2::unit(0.18, "lines"),
      plot.margin = ggplot2::margin(
        t = 1,
        r = 4,
        b = if (isTRUE(show_x)) 4 else 1,
        l = 2.5
      ),
      axis.title.y = ggplot2::element_text(size = FOREST_AXIS_TEXT_Y_SIZE)
    )
}

build_covariate_row_level_legend <- function(
    legend_dat,
    model_type,
    covariate,
    legend_pathogen = "covid"
) {
  dat <- filter_forest_covariate(legend_dat, covariate)
  if (is_empty_forest_data(dat)) {
    return(NULL)
  }

  level_order <- ordered_forest_covariate_levels(
    dat, covariate, model_type, legend_pathogen
  )
  level_colour_values <- covariate_forest_level_colours(covariate, level_order)
  n_keys <- length(level_colour_values)
  legend_nrow <- if (identical(covariate, "Household Composition")) 2L else 1L
  legend_ncol <- as.integer(ceiling(n_keys / legend_nrow))

  legend_plot <- forest_over_time_plot_all_seasons(
    forest_data = dat,
    pathogen = legend_pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    show_disruption_legend = FALSE,
    show_disruption_shading = FALSE,
    level_colour_values = level_colour_values,
    level_colour_title = covariate,
    drop_unknown_ethnicity = FALSE,
    log_y = TRUE
  ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "left",
      legend.box.just = "left",
      legend.title = ggplot2::element_text(
        size = COVARIATE_ROW_LEGEND_TITLE_SIZE, face = "bold"
      ),
      legend.text = ggplot2::element_text(size = COVARIATE_ROW_LEGEND_TEXT_SIZE),
      legend.key.width = ggplot2::unit(1.0, "lines"),
      legend.key.height = ggplot2::unit(0.75, "lines"),
      legend.spacing.x = ggplot2::unit(0.35, "lines"),
      legend.spacing.y = ggplot2::unit(0.05, "lines"),
      legend.margin = ggplot2::margin(0, 4, 0, 4),
      legend.box.margin = ggplot2::margin(1, 2, 1, 2)
    ) +
    ggplot2::guides(
      fill = "none",
      shape = "none",
      color = ggplot2::guide_legend(
        title = covariate,
        nrow = legend_nrow,
        ncol = legend_ncol,
        byrow = TRUE,
        order = 1,
        override.aes = list(
          size = COVARIATE_ROW_LEGEND_POINT_SIZE,
          shape = 16
        )
      )
    )

  tryCatch(
    cowplot::get_legend(legend_plot),
    error = function(e) NULL
  )
}

build_covariate_row_disruption_legend <- function(
    legend_dat,
    model_type,
    covariate,
    legend_pathogen = "covid"
) {
  dat <- filter_forest_covariate(legend_dat, covariate)
  if (is_empty_forest_data(dat)) {
    return(NULL)
  }

  legend_plot <- forest_over_time_plot_all_seasons(
    forest_data = dat,
    pathogen = legend_pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    show_disruption_legend = TRUE,
    show_disruption_shading = TRUE,
    disruption_legend_label = FOREST_COVID_DISRUPTION_LEGEND_LABEL,
    drop_unknown_ethnicity = FALSE,
    log_y = TRUE
  ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = COVARIATE_ROW_DISRUPTION_LEGEND_TEXT_SIZE,
        face = "italic"
      ),
      legend.key.width = ggplot2::unit(1.4, "lines"),
      legend.key.height = ggplot2::unit(0.9, "lines"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(1, 2, 1, 2)
    ) +
    ggplot2::guides(
      color = "none",
      shape = "none",
      fill = ggplot2::guide_legend(
        title = NULL,
        nrow = 1,
        ncol = 1,
        order = 1,
        override.aes = list(alpha = 0.65)
      )
    )

  tryCatch(
    cowplot::get_legend(legend_plot),
    error = function(e) NULL
  )
}

covariate_row_legend_row <- function(legend_grob, disruption_grob = NULL) {
  if (is.null(legend_grob) && is.null(disruption_grob)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  if (!is.null(disruption_grob)) {
    return(cowplot::plot_grid(
      NULL,
      legend_grob %||% (ggplot2::ggplot() + ggplot2::theme_void()),
      disruption_grob,
      NULL,
      ncol = 4,
      rel_widths = c(0.04, 0.58, 0.34, 0.04)
    ))
  }
  cowplot::plot_grid(
    NULL,
    legend_grob %||% (ggplot2::ggplot() + ggplot2::theme_void()),
    NULL,
    ncol = 3,
    rel_widths = c(0.04, 0.92, 0.04)
  )
}

# Pathogens kept for a phenotype (order RSV → flu → COVID); drops too-few shells.
condensed_pathogens_with_estimates <- function(
    rsv_ph,
    flu_ph,
    covid_ph,
    phenotype
) {
  candidates <- list(
    list(dat = rsv_ph, pathogen = "rsv"),
    list(dat = flu_ph, pathogen = "flu"),
    list(dat = covid_ph, pathogen = "covid")
  )
  keep <- Filter(
    function(x) forest_phenotype_has_estimates(x$dat, phenotype),
    candidates
  )
  if (length(keep) == 0L) {
    keep <- candidates
  }
  keep
}

assemble_condensed_figure_by_covariate <- function(
    rsv_dat,
    flu_dat,
    covid_dat,
    legend_dat,
    model_type,
    phenotype,
    seasons = NULL,
    column_labels = c("A. Mild", "B. Severe")
) {
  rsv_ph <- filter_forest_phenotype(rsv_dat, phenotype)
  flu_ph <- filter_forest_phenotype(flu_dat, phenotype)
  covid_ph <- filter_forest_phenotype(covid_dat, phenotype)

  pathogens_keep <- condensed_pathogens_with_estimates(
    rsv_ph, flu_ph, covid_ph, phenotype
  )
  n_pathogens <- length(pathogens_keep)
  stack_body_h <- 1.06 * n_pathogens

  present <- unique(unlist(lapply(pathogens_keep, function(x) {
    if (is_empty_forest_data(x$dat) || !"labels" %in% names(x$dat)) {
      return(character())
    }
    as.character(x$dat$labels)
  })))
  covariates <- intersect(FOREST_COVARIATE_ROW_ORDER, present)

  blocks <- vector("list", length(covariates))
  block_heights <- numeric(length(covariates))

  for (i in seq_along(covariates)) {
    cov <- covariates[[i]]
    legend_h <- if (identical(cov, "Household Composition")) 0.55 else 0.36

    panels <- lapply(seq_along(pathogens_keep), function(j) {
      plot_covariate_pathogen_panel(
        pathogens_keep[[j]]$dat,
        pathogens_keep[[j]]$pathogen,
        model_type,
        cov,
        show_x = identical(j, n_pathogens),
        seasons = seasons
      )
    })

    pathogen_stack <- cowplot::plot_grid(
      plotlist = panels,
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = rep(1, n_pathogens)
    )

    disruption_grob <- if (identical(cov, "Age Group")) {
      build_covariate_row_disruption_legend(
        legend_dat,
        model_type = model_type,
        covariate = cov
      )
    } else {
      NULL
    }

    blocks[[i]] <- cowplot::plot_grid(
      covariate_row_legend_row(
        build_covariate_row_level_legend(
          legend_dat,
          model_type = model_type,
          covariate = cov
        ),
        disruption_grob
      ),
      pathogen_stack,
      ncol = 1,
      rel_heights = c(legend_h, stack_body_h)
    )
    block_heights[[i]] <- legend_h + stack_body_h
  }

  combined <- cowplot::plot_grid(
    plotlist = c(list(NULL), blocks),
    ncol = 1,
    rel_heights = c(0.12, block_heights),
    align = "v",
    axis = "lr"
  )

  left_lab <- column_labels[[1]] %||% "A. Mild"
  right_lab <- column_labels[[2]] %||% "B. Severe"

  fig <- cowplot::ggdraw(combined) +
    cowplot::draw_label(
      left_lab, x = 0.275, y = 1, hjust = 0.5, vjust = 1.2,
      fontface = "bold", size = 16
    ) +
    cowplot::draw_label(
      right_lab, x = 0.74, y = 1, hjust = 0.5, vjust = 1.2,
      fontface = "bold", size = 16
    )
  attr(fig, "n_pathogens") <- n_pathogens
  fig
}

run_cohort_condensed_key_vars_by_covariate <- function(
    cohort,
    model_type = "ethnicity_ses",
    drop_unknown_ethnicity = TRUE,
    seasons = NULL,
    out_root = CONDENSED_COVARIATE_ROW_OUT_ROOT
) {
  assign("cohort", cohort, envir = .GlobalEnv)

  rsv_dat <- collect_further_pathogen_key_vars(
    cohort, "rsv", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    seasons = seasons
  )
  flu_dat <- collect_further_pathogen_key_vars(
    cohort, "flu", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    seasons = seasons
  )
  covid_dat <- collect_further_pathogen_key_vars(
    cohort, "covid", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    seasons = seasons
  )

  legend_dat <- filter_forest_phenotype(covid_dat, "specific")

  specific_fig <- assemble_condensed_figure_by_covariate(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "specific",
    seasons = seasons
  )
  sensitive_fig <- assemble_condensed_figure_by_covariate(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "sensitive",
    seasons = seasons
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  fig_height_base <- if (identical(cohort, "infants_subgroup")) {
    COVARIATE_ROW_FIG_HEIGHT - 2
  } else if (identical(model_type, "full")) {
    # Extra household-composition block, but keep the all-virus figure compact.
    COVARIATE_ROW_FIG_HEIGHT + 2
  } else {
    COVARIATE_ROW_FIG_HEIGHT
  }

  # Scale height when a phenotype drops too-few pathogen rows (e.g. RSV specific).
  fig_height_for_n_pathogens <- function(n_pathogens) {
    n <- as.integer(n_pathogens %||% 3L)
    if (!is.finite(n) || n < 1L) {
      n <- 3L
    }
    fig_height_base * (n / 3)
  }

  season_slug <- if (is.null(seasons)) {
    ""
  } else {
    paste0("_", paste(gsub("_", "", seasons), collapse = "_"))
  }

  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type,
        "_further", season_slug, "_specific_mild_vs_severe_key_vars.png"
      )
    ),
    specific_fig,
    height = fig_height_for_n_pathogens(attr(specific_fig, "n_pathogens")),
    width = COVARIATE_ROW_FIG_WIDTH,
    bg = "white"
  )
  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type,
        "_further", season_slug, "_sensitive_mild_vs_severe_key_vars.png"
      )
    ),
    sensitive_fig,
    height = fig_height_for_n_pathogens(attr(sensitive_fig, "n_pathogens")),
    width = COVARIATE_ROW_FIG_WIDTH,
    bg = "white"
  )

  invisible(
    list(
      specific = specific_fig,
      sensitive = sensitive_fig
    )
  )
}

# ---- Base vs further (stacked adjustment) with same covariate-row layout ----
# Legends sit above each Age / IMD / Ethnicity block (matching further condensed).
# Points: minimally vs fully adjusted connected by a line (no CIs).

collect_base_vs_further_pathogen_key_vars <- function(
    cohort,
    pathogen,
    model_type,
    drop_unknown_ethnicity = TRUE
) {
  df_min <- load_collated_base(cohort, pathogen)
  df_full <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  dat <- dplyr::bind_rows(
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, pathogen, model_type, "Mild", seasons = NULL
    ),
    forest_year_base_vs_further_mult_key_vars(
      df_min, df_full, df_dummy, pathogen, model_type, "Severe", seasons = NULL
    )
  )
  prepare_forest_plot_data(
    dat,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    pathogen = pathogen
  )
}

plot_covariate_pathogen_panel_base_vs_further <- function(
    plot_dat,
    pathogen,
    model_type,
    covariate,
    show_x = FALSE
) {
  dat <- filter_forest_covariate(plot_dat, covariate)
  if (identical(pathogen, "covid")) {
    dat <- drop_forest_points_before_year(dat, FOREST_COVID_MIN_YEAR)
  }
  if (is_empty_forest_data(dat)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  p <- forest_over_time_plot_compare(
    forest_data = dat,
    pathogen = pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    show_disruption_legend = FALSE,
    # Match further condensed key-vars: shade RSV/flu only; COVID panels unshaded.
    show_disruption_shading = !identical(pathogen, "covid"),
    disruption_legend_label = FOREST_COVID_DISRUPTION_LEGEND_LABEL,
    show_ci = FALSE,
    years_include = NULL,
    adjustment_layout = "stack",
    level_jitter_width = 0.4,
    level_jitter_span_cap = 0.5,
    compact_layout = TRUE,
    log_y = TRUE,
    y_lab = forest_pathogen_y_lab(pathogen)
  )

  p +
    shared_forest_season_x_scale() +
    ggplot2::theme(
      legend.position = "none",
      strip.text.y.left = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = if (isTRUE(show_x)) {
        ggplot2::element_text(size = FOREST_AXIS_TEXT_X_SIZE)
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (isTRUE(show_x)) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      },
      panel.spacing.x = ggplot2::unit(0.18, "lines"),
      plot.margin = ggplot2::margin(
        t = 1,
        r = 4,
        b = if (isTRUE(show_x)) 4 else 1,
        l = 2.5
      ),
      axis.title.y = ggplot2::element_text(size = FOREST_AXIS_TEXT_Y_SIZE)
    )
}

build_covariate_row_adjustment_legend <- function(
    legend_dat,
    model_type,
    covariate = "Age Group",
    legend_pathogen = "covid"
) {
  dat <- filter_forest_covariate(legend_dat, covariate)
  if (is_empty_forest_data(dat) || !"adjustment" %in% names(dat)) {
    return(NULL)
  }
  if (length(unique(stats::na.omit(as.character(dat$adjustment)))) < 2L) {
    return(NULL)
  }

  # Standalone legend so point size / labels are not constrained by the
  # compare-plot geoms (override.aes size is unreliable with alpha guides).
  legend_df <- tibble::tibble(
    adjustment = factor(c("Minimal", "Full"), levels = c("Minimal", "Full")),
    alpha = c(FOREST_ADJ_ALPHA_MINIMAL, FOREST_ADJ_ALPHA_FULL),
    x = c(1, 2),
    y = 1
  )

  legend_plot <- ggplot2::ggplot(
    legend_df,
    ggplot2::aes(x = .data$x, y = .data$y, alpha = .data$adjustment)
  ) +
    ggplot2::geom_point(shape = 16, size = 6, colour = "black") +
    ggplot2::scale_alpha_manual(
      values = c(
        "Minimal" = FOREST_ADJ_ALPHA_MINIMAL,
        "Full" = FOREST_ADJ_ALPHA_FULL
      ),
      breaks = c("Minimal", "Full"),
      name = "Adjustment"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.box.just = "center",
      legend.title = ggplot2::element_text(
        size = COVARIATE_ROW_LEGEND_TITLE_SIZE + 1, face = "bold"
      ),
      legend.text = ggplot2::element_text(size = COVARIATE_ROW_LEGEND_TEXT_SIZE + 1),
      legend.key.width = ggplot2::unit(1.8, "lines"),
      legend.key.height = ggplot2::unit(1.4, "lines"),
      legend.spacing.x = ggplot2::unit(0.6, "lines"),
      legend.margin = ggplot2::margin(2, 8, 2, 8),
      legend.box.margin = ggplot2::margin(2, 2, 2, 2)
    ) +
    ggplot2::guides(
      alpha = ggplot2::guide_legend(
        title = "Adjustment",
        nrow = 1,
        ncol = 2,
        byrow = TRUE,
        override.aes = list(size = 4, shape = 16, colour = "black")
      )
    )

  tryCatch(
    cowplot::get_legend(legend_plot),
    error = function(e) NULL
  )
}

covariate_row_legend_row_stacked <- function(
    legend_grob,
    adj_grob = NULL,
    disruption_grob = NULL
) {
  if (is.null(legend_grob) && is.null(adj_grob) && is.null(disruption_grob)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  # Adjustment-only strip (shared figure legend under Mild/Severe titles).
  if (!is.null(adj_grob) && is.null(legend_grob) && is.null(disruption_grob)) {
    return(cowplot::plot_grid(
      NULL,
      adj_grob,
      NULL,
      ncol = 3,
      rel_widths = c(0.2, 0.6, 0.2)
    ))
  }

  # Match further condensed: level colours left, disruption right.
  # Adjustment (stacked-only) sits between them when all three are present.
  if (!is.null(adj_grob) && !is.null(disruption_grob)) {
    return(cowplot::plot_grid(
      NULL,
      legend_grob %||% (ggplot2::ggplot() + ggplot2::theme_void()),
      adj_grob,
      disruption_grob,
      NULL,
      ncol = 5,
      rel_widths = c(0.02, 0.36, 0.30, 0.28, 0.04)
    ))
  }
  if (!is.null(adj_grob)) {
    return(cowplot::plot_grid(
      NULL,
      legend_grob %||% (ggplot2::ggplot() + ggplot2::theme_void()),
      adj_grob,
      NULL,
      ncol = 4,
      rel_widths = c(0.02, 0.58, 0.36, 0.04)
    ))
  }
  covariate_row_legend_row(legend_grob, disruption_grob)
}

assemble_condensed_figure_by_covariate_base_vs_further <- function(
    rsv_dat,
    flu_dat,
    covid_dat,
    legend_dat,
    model_type,
    phenotype
) {
  rsv_ph <- filter_forest_phenotype(rsv_dat, phenotype)
  flu_ph <- filter_forest_phenotype(flu_dat, phenotype)
  covid_ph <- filter_forest_phenotype(covid_dat, phenotype)

  present <- unique(c(
    as.character(rsv_ph$labels),
    as.character(flu_ph$labels),
    as.character(covid_ph$labels)
  ))
  covariates <- intersect(FOREST_COVARIATE_ROW_ORDER, present)

  blocks <- vector("list", length(covariates))
  block_heights <- numeric(length(covariates))

  for (i in seq_along(covariates)) {
    cov <- covariates[[i]]
    is_age <- identical(cov, "Age Group")
    legend_h <- if (identical(cov, "Household Composition")) 0.55 else 0.36

    pathogen_stack <- cowplot::plot_grid(
      plot_covariate_pathogen_panel_base_vs_further(
        rsv_ph, "rsv", model_type, cov, show_x = FALSE
      ),
      plot_covariate_pathogen_panel_base_vs_further(
        flu_ph, "flu", model_type, cov, show_x = FALSE
      ),
      plot_covariate_pathogen_panel_base_vs_further(
        covid_ph, "covid", model_type, cov, show_x = TRUE
      ),
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 1, 1)
    )

    # Same as further condensed: level colours left; disruption on Age only.
    disruption_grob <- if (is_age) {
      build_covariate_row_disruption_legend(
        legend_dat,
        model_type = model_type,
        covariate = cov
      )
    } else {
      NULL
    }

    blocks[[i]] <- cowplot::plot_grid(
      covariate_row_legend_row(
        build_covariate_row_level_legend(
          legend_dat,
          model_type = model_type,
          covariate = cov
        ),
        disruption_grob
      ),
      pathogen_stack,
      ncol = 1,
      rel_heights = c(legend_h, 3.18)
    )
    block_heights[[i]] <- legend_h + 3.18
  }

  # Shared Adjustment legend under Mild/Severe titles (stacked-only guide).
  adj_legend_h <- 0.3
  adj_row <- covariate_row_legend_row_stacked(
    legend_grob = NULL,
    adj_grob = build_covariate_row_adjustment_legend(
      legend_dat,
      model_type = model_type,
      covariate = covariates[[1]] %||% "Age Group"
    ),
    disruption_grob = NULL
  )

  combined <- cowplot::plot_grid(
    plotlist = c(list(NULL, adj_row), blocks),
    ncol = 1,
    rel_heights = c(0.14, adj_legend_h, block_heights),
    align = "v",
    axis = "lr"
  )

  cowplot::ggdraw(combined) +
    cowplot::draw_label(
      "A. Mild", x = 0.275, y = 1, hjust = 0.5, vjust = 1.2,
      fontface = "bold", size = 16
    ) +
    cowplot::draw_label(
      "B. Severe", x = 0.74, y = 1, hjust = 0.5, vjust = 1.2,
      fontface = "bold", size = 16
    )
}

run_cohort_condensed_key_vars_base_vs_further_by_covariate <- function(
    cohort,
    model_type = "ethnicity_ses",
    drop_unknown_ethnicity = TRUE,
    out_root = CONDENSED_SEQUENTIAL_OUT_ROOT
) {
  assign("cohort", cohort, envir = .GlobalEnv)

  rsv_dat <- collect_base_vs_further_pathogen_key_vars(
    cohort, "rsv", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity
  )
  flu_dat <- collect_base_vs_further_pathogen_key_vars(
    cohort, "flu", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity
  )
  covid_dat <- collect_base_vs_further_pathogen_key_vars(
    cohort, "covid", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity
  )

  legend_dat <- filter_forest_phenotype(covid_dat, "specific")

  specific_fig <- assemble_condensed_figure_by_covariate_base_vs_further(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "specific"
  )
  sensitive_fig <- assemble_condensed_figure_by_covariate_base_vs_further(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "sensitive"
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    file.path(
      out_root,
      paste0(
        cohort, "_", model_type,
        "_base_vs_further_stacked_specific_mild_vs_severe.png"
      )
    ),
    specific_fig,
    height = COVARIATE_ROW_FIG_HEIGHT-2,
    width = COVARIATE_ROW_FIG_WIDTH,
    bg = "white"
  )
  ggplot2::ggsave(
    file.path(
      out_root,
      paste0(
        cohort, "_", model_type,
        "_base_vs_further_stacked_sensitive_mild_vs_severe.png"
      )
    ),
    sensitive_fig,
    height = COVARIATE_ROW_FIG_HEIGHT,
    width = COVARIATE_ROW_FIG_WIDTH,
    bg = "white"
  )

  invisible(
    list(
      specific = specific_fig,
      sensitive = sensitive_fig
    )
  )
}

# ---- Sensitivity further-only (RSV + flu) with covariate-row layout ----------
# Same Age / IMD / Ethnicity blocks as primary condensed; pathogen sub-rows are
# RSV then flu only. Both seasons (2017-18, 2018-19) on every panel.

COVARIATE_ROW_FIG_HEIGHT_SENSITIVITY <- 10.5

CONDENSED_SENSITIVITY_COVARIATE_ROW_OUT_ROOT <- here::here(
  "post_check", "plots", "sensitivity_analyses", "condensed_models_key_vars"
)

collect_further_pathogen_key_vars_sensitivity <- function(
    cohort,
    pathogen,
    model_type,
    reference_template = NULL,
    drop_unknown_ethnicity = TRUE
) {
  df_input <- load_collated_further_sensitivity(cohort, pathogen)
  df_dummy <- load_dummy_inputs_sensitivity(cohort, pathogen)
  dat <- dplyr::bind_rows(
    forest_key_vars_sensitivity_outcome_data(
      df_input, df_dummy, pathogen, model_type, "Mild",
      reference_template = reference_template
    ),
    forest_key_vars_sensitivity_outcome_data(
      df_input, df_dummy, pathogen, model_type, "Severe",
      reference_template = reference_template
    )
  )
  prepare_forest_plot_data(
    dat,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    pathogen = pathogen
  )
}

plot_covariate_pathogen_panel_sensitivity <- function(
    plot_dat,
    pathogen,
    model_type,
    covariate,
    show_x = FALSE
) {
  dat <- filter_forest_covariate(plot_dat, covariate)
  if (is_empty_forest_data(dat)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  seasons <- sensitivity_all_plot_seasons()
  level_order <- ordered_forest_covariate_levels(
    dat, covariate, model_type, pathogen
  )
  level_colour_values <- covariate_forest_level_colours(covariate, level_order)

  p <- forest_over_time_plot(
    forest_data = dat,
    pathogen = pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    label_levels = FALSE,
    show_disruption_legend = FALSE,
    show_disruption_shading = FALSE,
    level_colour_values = level_colour_values,
    level_colour_title = covariate,
    drop_unknown_ethnicity = FALSE,
    seasons = seasons,
    equal_season_widths = TRUE,
    pointrange_fatten = COVARIATE_ROW_POINTRANGE_FATTEN,
    pointrange_linewidth = COVARIATE_ROW_POINTRANGE_LINEWIDTH,
    log_y = TRUE,
    y_lab = forest_pathogen_y_lab(pathogen)
  )

  p +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = if (isTRUE(show_x)) {
        ggplot2::element_text(size = FOREST_AXIS_TEXT_X_SIZE)
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (isTRUE(show_x)) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      },
      panel.spacing.x = ggplot2::unit(0.18, "lines"),
      plot.margin = ggplot2::margin(
        t = 1,
        r = 4,
        b = if (isTRUE(show_x)) 4 else 1,
        l = 2.5
      ),
      axis.title.y = ggplot2::element_text(size = FOREST_AXIS_TEXT_Y_SIZE)
    )
}

build_covariate_row_level_legend_sensitivity <- function(
    legend_dat,
    model_type,
    covariate,
    legend_pathogen = "flu"
) {
  dat <- filter_forest_covariate(legend_dat, covariate)
  if (is_empty_forest_data(dat)) {
    return(NULL)
  }

  seasons <- sensitivity_all_plot_seasons()
  level_order <- ordered_forest_covariate_levels(
    dat, covariate, model_type, legend_pathogen
  )
  level_colour_values <- covariate_forest_level_colours(covariate, level_order)
  n_keys <- length(level_colour_values)

  legend_plot <- forest_over_time_plot(
    forest_data = dat,
    pathogen = legend_pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    label_levels = FALSE,
    show_disruption_legend = FALSE,
    show_disruption_shading = FALSE,
    level_colour_values = level_colour_values,
    level_colour_title = covariate,
    drop_unknown_ethnicity = FALSE,
    seasons = seasons,
    equal_season_widths = TRUE,
    log_y = TRUE
  ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "left",
      legend.box.just = "left",
      legend.title = ggplot2::element_text(
        size = COVARIATE_ROW_LEGEND_TITLE_SIZE, face = "bold"
      ),
      legend.text = ggplot2::element_text(size = COVARIATE_ROW_LEGEND_TEXT_SIZE),
      legend.key.width = ggplot2::unit(1.0, "lines"),
      legend.key.height = ggplot2::unit(0.75, "lines"),
      legend.spacing.x = ggplot2::unit(0.35, "lines"),
      legend.spacing.y = ggplot2::unit(0.05, "lines"),
      legend.margin = ggplot2::margin(0, 4, 0, 4),
      legend.box.margin = ggplot2::margin(1, 2, 1, 2)
    ) +
    ggplot2::guides(
      fill = "none",
      shape = "none",
      color = ggplot2::guide_legend(
        title = covariate,
        nrow = 1,
        ncol = n_keys,
        byrow = TRUE,
        order = 1,
        override.aes = list(
          size = COVARIATE_ROW_LEGEND_POINT_SIZE,
          shape = 16
        )
      )
    )

  tryCatch(
    cowplot::get_legend(legend_plot),
    error = function(e) NULL
  )
}

assemble_condensed_figure_by_covariate_sensitivity <- function(
    rsv_dat,
    flu_dat,
    legend_dat,
    model_type
) {
  present <- unique(c(
    as.character(rsv_dat$labels),
    as.character(flu_dat$labels)
  ))
  covariates <- intersect(FOREST_COVARIATE_ROW_ORDER, present)

  blocks <- vector("list", length(covariates))
  block_heights <- numeric(length(covariates))

  for (i in seq_along(covariates)) {
    cov <- covariates[[i]]
    legend_h <- 0.36

    pathogen_stack <- cowplot::plot_grid(
      plot_covariate_pathogen_panel_sensitivity(
        rsv_dat, "rsv", model_type, cov, show_x = FALSE
      ),
      plot_covariate_pathogen_panel_sensitivity(
        flu_dat, "flu", model_type, cov, show_x = TRUE
      ),
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 1)
    )

    # Sensitivity seasons are 2017-18 / 2018-19 only — no disruption bands.
    blocks[[i]] <- cowplot::plot_grid(
      covariate_row_legend_row(
        build_covariate_row_level_legend_sensitivity(
          legend_dat,
          model_type = model_type,
          covariate = cov
        ),
        NULL
      ),
      pathogen_stack,
      ncol = 1,
      rel_heights = c(legend_h, 2.12)
    )
    block_heights[[i]] <- legend_h + 2.12
  }

  combined <- cowplot::plot_grid(
    plotlist = c(list(NULL), blocks),
    ncol = 1,
    rel_heights = c(0.12, block_heights),
    align = "v",
    axis = "lr"
  )

  cowplot::ggdraw(combined) +
    cowplot::draw_label(
      "A. Mild", x = 0.275, y = 1, hjust = 0.5, vjust = 1.2,
      fontface = "bold", size = 16
    ) +
    cowplot::draw_label(
      "B. Severe", x = 0.74, y = 1, hjust = 0.5, vjust = 1.2,
      fontface = "bold", size = 16
    )
}

run_cohort_condensed_key_vars_sensitivity <- function(
    cohort,
    model_type = "ethnicity_ses",
    drop_unknown_ethnicity = TRUE,
    out_root = CONDENSED_SENSITIVITY_COVARIATE_ROW_OUT_ROOT
) {
  assign("cohort", cohort, envir = .GlobalEnv)
  assign("investigation_type", "sensitivity", envir = .GlobalEnv)

  df_input_rsv <- load_collated_further_sensitivity(cohort, "rsv")
  df_dummy_rsv <- load_dummy_inputs_sensitivity(cohort, "rsv")
  df_input_flu <- load_collated_further_sensitivity(cohort, "flu")
  df_dummy_flu <- load_dummy_inputs_sensitivity(cohort, "flu")

  reference_template <- sensitivity_key_vars_reference_template(
    df_input_rsv, df_dummy_rsv, df_input_flu, df_dummy_flu, model_type
  )

  rsv_dat <- collect_further_pathogen_key_vars_sensitivity(
    cohort, "rsv", model_type,
    reference_template = reference_template,
    drop_unknown_ethnicity = drop_unknown_ethnicity
  )
  flu_dat <- collect_further_pathogen_key_vars_sensitivity(
    cohort, "flu", model_type,
    reference_template = reference_template,
    drop_unknown_ethnicity = drop_unknown_ethnicity
  )

  legend_dat <- if (!is_empty_forest_data(flu_dat)) {
    flu_dat
  } else {
    rsv_dat
  }

  condensed <- assemble_condensed_figure_by_covariate_sensitivity(
    rsv_dat, flu_dat, legend_dat, model_type
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(cohort, "_", model_type, "_further_mild_vs_severe_key_vars.png")
    ),
    condensed,
    width = COVARIATE_ROW_FIG_WIDTH,
    height = COVARIATE_ROW_FIG_HEIGHT_SENSITIVITY,
    bg = "white"
  )

  invisible(list(condensed = condensed))
}

# ---- Infants vs maternally linked infants (minimally adjusted / base) --------
# Same covariate-row layout as further condensed key-vars, but columns are
# Infants | Maternally linked infants. Separate figures for Mild and Severe.

INFANT_COHORT_COMPARE_LABELS <- c(
  infants = "Infants",
  infants_subgroup = "Maternally linked infants"
)

CONDENSED_INFANT_COHORT_COMPARE_OUT_ROOT <- here::here(
  "post_check", "plots", "primary_analyses", "condensed_models_key_vars",
  "infants_vs_maternal"
)

collect_base_pathogen_key_vars <- function(
    cohort,
    pathogen,
    model_type,
    drop_unknown_ethnicity = TRUE,
    seasons = NULL
) {
  assign("cohort", cohort, envir = .GlobalEnv)
  df_input <- load_collated_base(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)

  dat <- dplyr::bind_rows(lapply(c("Mild", "Severe"), function(ot) {
    p0 <- forest(
      df = df_input,
      df_dummy = df_dummy,
      pathogen = pathogen,
      model_type = model_type,
      outcome_type = ot,
      further = "no"
    )
    d0 <- attr(p0, "forest_data")
    if (is.null(d0) || !is.data.frame(d0) || nrow(d0) == 0L) {
      return(NULL)
    }
    d0 %>% dplyr::mutate(outcome_type = ot)
  }))

  dat <- prepare_forest_plot_data(
    dat,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    pathogen = pathogen
  )
  if (!is.null(seasons)) {
    dat <- filter_forest_to_seasons(
      dat,
      seasons_for_pathogen_compare(pathogen, seasons)
    )
  }
  dat
}

# Minimally adjusted ethnicity_ses for both infant cohorts; facet columns are
# cohort labels (severity kept in `severity` for filtering into Mild/Severe figs).
collect_infant_cohorts_base_pathogen_key_vars <- function(
    pathogen,
    model_type,
    drop_unknown_ethnicity = TRUE,
    seasons = NULL
) {
  cohort_ids <- names(INFANT_COHORT_COMPARE_LABELS)
  facet_levels <- unname(INFANT_COHORT_COMPARE_LABELS)

  dplyr::bind_rows(lapply(cohort_ids, function(cohort_id) {
    dat <- collect_base_pathogen_key_vars(
      cohort = cohort_id,
      pathogen = pathogen,
      model_type = model_type,
      drop_unknown_ethnicity = drop_unknown_ethnicity,
      seasons = seasons
    )
    if (is_empty_forest_data(dat)) {
      return(NULL)
    }
    dat %>%
      dplyr::mutate(
        cohort = cohort_id,
        severity = as.character(.data$outcome_type),
        outcome_type = factor(
          INFANT_COHORT_COMPARE_LABELS[[cohort_id]],
          levels = facet_levels
        )
      )
  }))
}

filter_forest_severity <- function(dat, severity) {
  if (is_empty_forest_data(dat)) {
    return(dat)
  }
  severity_val <- as.character(severity)[[1]]
  if ("severity" %in% names(dat)) {
    return(dat %>% dplyr::filter(.data$severity == .env$severity_val))
  }
  dat %>% dplyr::filter(as.character(.data$outcome_type) == .env$severity_val)
}

run_infants_vs_maternal_base_condensed_key_vars <- function(
    model_type = "ethnicity_ses",
    drop_unknown_ethnicity = TRUE,
    seasons = NULL,
    out_root = CONDENSED_INFANT_COHORT_COMPARE_OUT_ROOT
) {
  assign("investigation_type", "primary", envir = .GlobalEnv)

  rsv_dat <- collect_infant_cohorts_base_pathogen_key_vars(
    "rsv", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    seasons = seasons
  )
  flu_dat <- collect_infant_cohorts_base_pathogen_key_vars(
    "flu", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    seasons = seasons
  )
  covid_dat <- collect_infant_cohorts_base_pathogen_key_vars(
    "covid", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    seasons = seasons
  )

  # Level colours / disruption legend: use infants (shared age/IMD/ethnicity).
  assign("cohort", "infants", envir = .GlobalEnv)
  legend_dat <- filter_forest_phenotype(covid_dat, "specific")

  column_labels <- c("A. Infants", "B. Maternally linked infants")
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  season_slug <- if (is.null(seasons)) {
    ""
  } else {
    paste0("_", paste(gsub("_", "", seasons), collapse = "_"))
  }

  figs <- list()
  for (severity in c("Mild", "Severe")) {
    rsv_s <- filter_forest_severity(rsv_dat, severity)
    flu_s <- filter_forest_severity(flu_dat, severity)
    covid_s <- filter_forest_severity(covid_dat, severity)
    legend_s <- filter_forest_severity(legend_dat, severity)

    for (phenotype in c("specific", "sensitive")) {
      fig <- assemble_condensed_figure_by_covariate(
        rsv_s, flu_s, covid_s, legend_s, model_type, phenotype,
        seasons = seasons,
        column_labels = column_labels
      )
      severity_slug <- tolower(severity)
      out_file <- here::here(
        out_root,
        paste0(
          "infants_vs_maternal_", model_type, "_base", season_slug,
          "_", phenotype, "_", severity_slug, "_key_vars.png"
        )
      )
      ggplot2::ggsave(
        out_file,
        fig,
        height = COVARIATE_ROW_FIG_HEIGHT - 2,
        width = COVARIATE_ROW_FIG_WIDTH,
        bg = "white"
      )
      figs[[paste(phenotype, severity_slug, sep = "_")]] <- fig
      message("Wrote: ", out_file)
    }
  }

  invisible(figs)
}
