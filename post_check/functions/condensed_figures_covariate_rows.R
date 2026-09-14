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
    drop_unknown_ethnicity = TRUE
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
  prepare_forest_plot_data(
    dat,
    drop_unknown_ethnicity = drop_unknown_ethnicity,
    pathogen = pathogen
  )
}

filter_forest_phenotype <- function(dat, phenotype) {
  dat %>% dplyr::filter(.data$codelist_type %in% c("reference", phenotype))
}

filter_forest_covariate <- function(dat, covariate) {
  dat %>% dplyr::filter(as.character(.data$labels) == covariate)
}

plot_covariate_pathogen_panel <- function(
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

  level_order <- ordered_forest_covariate_levels(
    dat, covariate, model_type, pathogen
  )
  level_colour_values <- covariate_forest_level_colours(covariate, level_order)

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
    y_lab = unname(FOREST_PATHOGEN_Y_LABS[[pathogen]])
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

assemble_condensed_figure_by_covariate <- function(
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
    legend_h <- 0.36

    pathogen_stack <- cowplot::plot_grid(
      plot_covariate_pathogen_panel(
        rsv_ph, "rsv", model_type, cov, show_x = FALSE
      ),
      plot_covariate_pathogen_panel(
        flu_ph, "flu", model_type, cov, show_x = FALSE
      ),
      plot_covariate_pathogen_panel(
        covid_ph, "covid", model_type, cov, show_x = TRUE
      ),
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 1, 1)
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
      rel_heights = c(legend_h, 3.18)
    )
    block_heights[[i]] <- legend_h + 3.18
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

run_cohort_condensed_key_vars_by_covariate <- function(
    cohort,
    model_type = "ethnicity_ses",
    drop_unknown_ethnicity = TRUE,
    out_root = CONDENSED_COVARIATE_ROW_OUT_ROOT
) {
  assign("cohort", cohort, envir = .GlobalEnv)

  rsv_dat <- collect_further_pathogen_key_vars(
    cohort, "rsv", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity
  )
  flu_dat <- collect_further_pathogen_key_vars(
    cohort, "flu", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity
  )
  covid_dat <- collect_further_pathogen_key_vars(
    cohort, "covid", model_type,
    drop_unknown_ethnicity = drop_unknown_ethnicity
  )

  legend_dat <- filter_forest_phenotype(covid_dat, "specific")

  specific_fig <- assemble_condensed_figure_by_covariate(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "specific"
  )
  sensitive_fig <- assemble_condensed_figure_by_covariate(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "sensitive"
  )

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type,
        "_further_specific_mild_vs_severe_key_vars.png"
      )
    ),
    specific_fig,
    height = COVARIATE_ROW_FIG_HEIGHT,
    width = COVARIATE_ROW_FIG_WIDTH,
    bg = "white"
  )
  ggplot2::ggsave(
    here::here(
      out_root,
      paste0(
        cohort, "_", model_type,
        "_further_sensitive_mild_vs_severe_key_vars.png"
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
