# Covariate-row layout with level colours (instead of shapes).
# Columns: mild & severe; rows: age group / IMD / ethnicity; sub-rows: RSV / flu / COVID.
# Age and IMD use separate high-contrast sequential palettes;
# ethnicity uses the batlow diverging palette.
# COVID panels omit disruption shading; RSV/flu shading legend reads
# "COVID-19 related disruption".
# Saves two output sets: with and without ethnicity "Unknown".

library(tidyverse)
library(here)
library(arrow)
library(cowplot)
library(khroma)

source(here::here("post_check", "functions", "forest.R"))
ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)
investigation_type <- "primary"
df_few <- tibble()

model_type <- "ethnicity_ses"

covariate_row_order <- c("Age Group", "IMD Quintile", "Ethnicity")

pathogen_y_labs <- c(
  rsv = "RSV",
  flu = "Influenza",
  covid = "COVID-19"
)

full_year_breaks <- 2016:2023
covid_min_year <- 2019L

# Age: high-contrast pink sequential; IMD: high-contrast blue sequential;
# ethnicity: batlow (colour-blind-safe diverging, via khroma).
AGE_LEVEL_PALETTE <- c(
  "#F48FB1", "#F06292", "#EC407A", "#E91E63",
  "#D81B60", "#C2185B", "#AD1457", "#880E4F"
)
IMD_LEVEL_PALETTE <- c(
  "#4FC3F7", "#29B6F6", "#039BE5", "#0288D1",
  "#0277BD", "#01579B", "#004BA0", "#002171"
)
BATLOW_PALETTE <- "batlow"

disruption_legend_label <- "COVID-19 related disruption"
COVARIATE_ROW_LEGEND_POINT_SIZE <- 0.6
COVARIATE_ROW_LEGEND_TITLE_SIZE <- 11
COVARIATE_ROW_LEGEND_TEXT_SIZE <- 10
COVARIATE_ROW_DISRUPTION_LEGEND_TEXT_SIZE <- 14
COVARIATE_ROW_POINTRANGE_FATTEN <- 4
COVARIATE_ROW_POINTRANGE_LINEWIDTH <- 0.6

season_start_year <- function(subset) {
  suppressWarnings(as.integer(stringr::str_extract(
    gsub("_", "-", as.character(subset)),
    "^[0-9]{4}"
  )))
}

# TEMPORARY: older collated further-model outputs used inverted IMD coding
# (1 = least, 5 = most). Remap to UK convention only when those inverted
# labels are present — same approach as table1_key_vars_panel_viz.R /
# reformat_model_estimates.R.
relabel_imd_current_outputs <- function(dat) {
  if (is_empty_forest_data(dat) || !"label" %in% names(dat)) {
    return(dat)
  }

  is_imd <- (!is.na(dat$variable) & dat$variable == "imd_quintile") |
    as.character(dat$labels) == "IMD Quintile"

  label_chr <- as.character(dat$label)
  term_chr <- if ("term" %in% names(dat)) {
    as.character(dat$term)
  } else {
    rep(NA_character_, nrow(dat))
  }

  imd_inverted <- any(
    is_imd & (
      label_chr %in% c("1 (least deprived)", "5 (most deprived)") |
        stringr::str_detect(
          dplyr::coalesce(term_chr, ""),
          "imd_quintile5 \\(most deprived\\)|imd_quintile1 \\(least deprived\\)"
        )
    ),
    na.rm = TRUE
  )
  if (!isTRUE(imd_inverted)) {
    return(dat)
  }

  remap_imd_label <- function(label, term) {
    label <- as.character(label)
    term <- dplyr::coalesce(as.character(term), "")
    dplyr::case_when(
      term == "imd_quintile2" | label == "2" ~ "4",
      term == "imd_quintile3" | label == "3" ~ "3",
      term == "imd_quintile4" | label == "4" ~ "2",
      term == "imd_quintile5 (most deprived)" | label == "5 (most deprived)" ~
        "1 (most deprived)",
      term == "imd_quintile5" | label == "5" ~ "1 (most deprived)",
      term == "imd_quintile1 (least deprived)" | label == "1 (least deprived)" ~
        "5 (least deprived)",
      term == "imd_quintile1" | label == "1" ~ "5 (least deprived)",
      # Already-UK labels such as "5 (least deprived)" are left unchanged.
      TRUE ~ label
    )
  }

  new_label <- dplyr::if_else(
    is_imd,
    remap_imd_label(label_chr, term_chr),
    label_chr
  )

  dat %>%
    mutate(
      label = new_label,
      term = if_else(
        is_imd & "term" %in% names(dat),
        paste0("imd_quintile", new_label),
        .data$term
      )
    )
}

drop_unknown_level <- function(dat) {
  if (is_empty_forest_data(dat) || !"label" %in% names(dat)) {
    return(dat)
  }
  dat %>%
    filter(!stringr::str_detect(as.character(.data$label), "(?i)^unknown$"))
}

drop_points_before_year <- function(dat, min_year) {
  if (is_empty_forest_data(dat) || !"subset" %in% names(dat)) {
    return(dat)
  }
  dat %>%
    mutate(.year = season_start_year(.data$subset)) %>%
    filter(is.na(.year) | .year >= min_year) %>%
    select(-.year)
}

shared_season_x_scale <- function() {
  ggplot2::scale_x_continuous(
    breaks = full_year_breaks,
    labels = paste0(
      full_year_breaks, "-",
      stringr::str_sub(as.character(full_year_breaks + 1L), 3, 4)
    ),
    limits = c(min(full_year_breaks) - 0.5, max(full_year_breaks) + 0.5),
    expand = ggplot2::expansion(mult = c(0, 0))
  )
}


clean_level_labels <- function(levels) {
  levels <- as.character(levels)
  levels[!is.na(levels) & levels != ""]
}

sample_cb_palette <- function(base_cols, n) {
  if (n == 0L) {
    return(character())
  }
  if (n == 1L) {
    return(base_cols[ceiling(length(base_cols) / 2)])
  }
  if (n <= length(base_cols)) {
    idx <- round(seq(1, length(base_cols), length.out = n))
    return(base_cols[idx])
  }
  grDevices::colorRampPalette(base_cols)(n)
}

sequential_level_colours <- function(levels, base_cols) {
  levels <- clean_level_labels(levels)
  n <- length(levels)
  if (n == 0L) {
    return(character())
  }
  stats::setNames(sample_cb_palette(base_cols, n), levels)
}

age_level_colours <- function(levels) {
  sequential_level_colours(levels, AGE_LEVEL_PALETTE)
}

imd_level_colours <- function(levels) {
  sequential_level_colours(levels, IMD_LEVEL_PALETTE)
}

ethnicity_level_colours <- function(levels) {
  levels <- clean_level_labels(levels)
  n <- length(levels)
  if (n == 0L) {
    return(character())
  }
  stats::setNames(khroma::colour(BATLOW_PALETTE)(n), levels)
}

covariate_level_colours <- function(covariate, levels) {
  if (identical(covariate, "Age Group")) {
    age_level_colours(levels)
  } else if (identical(covariate, "IMD Quintile")) {
    imd_level_colours(levels)
  } else if (identical(covariate, "Ethnicity")) {
    ethnicity_level_colours(levels)
  } else {
    sequential_level_colours(levels, AGE_LEVEL_PALETTE)
  }
}

ordered_covariate_levels <- function(dat, covariate, model_type, pathogen) {
  in_data <- dat %>%
    filter(as.character(.data$labels) == covariate) %>%
    pull(.data$label) %>%
    unique() %>%
    as.character()
  in_data <- in_data[!is.na(in_data) & in_data != ""]

  cohort_val <- if (exists("cohort", envir = .GlobalEnv)) {
    get("cohort", envir = .GlobalEnv)
  } else {
    NA_character_
  }
  investigation_val <- if (exists("investigation_type", envir = .GlobalEnv)) {
    get("investigation_type", envir = .GlobalEnv)
  } else {
    "primary"
  }

  level_order <- get_forest_level_order(
    cohort_val, model_type, pathogen, investigation_val, style = "year_mult"
  )
  ordered <- level_order[level_order %in% in_data]
  extras <- setdiff(in_data, ordered)
  c(ordered, extras)
}

load_collated_further <- function(cohort, pathogen) {
  raw <- read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
  df_few <<- bind_rows(df_few, raw %>% filter(term == "too few events"))
  raw %>% filter(term != "too few events")
}

load_dummy_inputs <- function(cohort, pathogen) {
  if (identical(pathogen, "covid")) {
    read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow")
      )
    ) %>%
      mutate(
        covid_vaccination_immunity_date = if (
          !"covid_vaccination_immunity_date" %in% names(.)
        ) NA else covid_vaccination_immunity_date,
        time_since_last_covid_vaccination = if (
          !"time_since_last_covid_vaccination" %in% names(.)
        ) NA_character_ else time_since_last_covid_vaccination
      ) %>%
      mutate(
        subset = "2021_22",
        time_since_last_covid_vaccination = if_else(
          is.na(covid_vaccination_immunity_date),
          "6-12m",
          time_since_last_covid_vaccination
        )
      )
  } else {
    read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow")
      )
    )
  }
}

collect_pathogen_key_vars <- function(
    cohort,
    pathogen,
    model_type,
    drop_unknown_ethnicity = TRUE
) {
  df_input <- load_collated_further(cohort, pathogen)
  df_dummy <- load_dummy_inputs(cohort, pathogen)
  dat <- bind_rows(
    forest_year_further_mult_key_vars(
      df_input, df_dummy, pathogen, model_type, "Mild", return_data = TRUE
    ),
    forest_year_further_mult_key_vars(
      df_input, df_dummy, pathogen, model_type, "Severe", return_data = TRUE
    )
  )
  if (isTRUE(drop_unknown_ethnicity)) {
    dat <- drop_unknown_level(dat)
  }
  dat <- relabel_imd_current_outputs(dat)
  if (identical(pathogen, "covid")) {
    dat <- drop_points_before_year(dat, covid_min_year)
  }
  dat
}

filter_phenotype <- function(dat, phenotype) {
  dat %>% filter(codelist_type %in% c("reference", phenotype))
}

filter_covariate <- function(dat, covariate) {
  dat %>% filter(as.character(labels) == covariate)
}

plot_covariate_pathogen <- function(
    plot_dat,
    pathogen,
    model_type,
    covariate,
    show_x = FALSE
) {
  dat <- filter_covariate(plot_dat, covariate)
  if (identical(pathogen, "covid")) {
    dat <- drop_points_before_year(dat, covid_min_year)
  }
  if (is_empty_forest_data(dat)) {
    return(ggplot() + theme_void())
  }

  level_order <- ordered_covariate_levels(dat, covariate, model_type, pathogen)
  level_colour_values <- covariate_level_colours(covariate, level_order)

  p <- forest_over_time_plot_all_seasons(
    forest_data = dat,
    pathogen = pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    show_disruption_legend = FALSE,
    show_disruption_shading = !identical(pathogen, "covid"),
    disruption_legend_label = disruption_legend_label,
    level_colour_values = level_colour_values,
    level_colour_title = covariate,
    pointrange_fatten = COVARIATE_ROW_POINTRANGE_FATTEN,
    pointrange_linewidth = COVARIATE_ROW_POINTRANGE_LINEWIDTH,
    log_y = TRUE,
    y_lab = unname(pathogen_y_labs[[pathogen]])
  )

  p <- p +
    shared_season_x_scale() +
    theme(
      legend.position = "none",
      strip.text.y.left = element_blank(),
      strip.text.y.right = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = if (isTRUE(show_x)) {
        element_text(size = FOREST_AXIS_TEXT_X_SIZE)
      } else {
        element_blank()
      },
      axis.ticks.x = if (isTRUE(show_x)) element_line() else element_blank(),
      panel.spacing.x = unit(0.18, "lines"),
      plot.margin = margin(
        t = 1,
        r = 4,
        b = if (isTRUE(show_x)) 4 else 1,
        l = 2.5
      ),
      axis.title.y = element_text(size = FOREST_AXIS_TEXT_Y_SIZE)
    )

  p
}

build_covariate_legend <- function(
    legend_dat,
    model_type,
    covariate,
    legend_pathogen = "covid"
) {
  dat <- filter_covariate(legend_dat, covariate)
  if (is_empty_forest_data(dat)) {
    return(NULL)
  }

  level_order <- ordered_covariate_levels(dat, covariate, model_type, legend_pathogen)
  level_colour_values <- covariate_level_colours(covariate, level_order)
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
    log_y = TRUE
  )

  legend_plot <- legend_plot +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "left",
      legend.box.just = "left",
      legend.title = element_text(size = COVARIATE_ROW_LEGEND_TITLE_SIZE, face = "bold"),
      legend.text = element_text(size = COVARIATE_ROW_LEGEND_TEXT_SIZE),
      legend.key.width = unit(1.0, "lines"),
      legend.key.height = unit(0.75, "lines"),
      legend.spacing.x = unit(0.35, "lines"),
      legend.spacing.y = unit(0.05, "lines"),
      legend.margin = margin(0, 4, 0, 4),
      legend.box.margin = margin(1, 2, 1, 2)
    ) +
    guides(
      fill = "none",
      shape = "none",
      color = guide_legend(
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

build_disruption_legend <- function(
    legend_dat,
    model_type,
    covariate,
    legend_pathogen = "covid"
) {
  dat <- filter_covariate(legend_dat, covariate)
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
    disruption_legend_label = disruption_legend_label,
    log_y = TRUE
  ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "right",
      legend.title = element_blank(),
      legend.text = element_text(
        size = COVARIATE_ROW_DISRUPTION_LEGEND_TEXT_SIZE,
        face = "italic"
      ),
      legend.key.width = unit(1.4, "lines"),
      legend.key.height = unit(0.9, "lines"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(1, 2, 1, 2)
    ) +
    guides(
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

legend_row <- function(legend_grob, disruption_grob = NULL) {
  if (is.null(legend_grob) && is.null(disruption_grob)) {
    return(ggplot() + theme_void())
  }
  if (!is.null(disruption_grob)) {
    return(plot_grid(
      NULL,
      legend_grob %||% (ggplot() + theme_void()),
      disruption_grob,
      NULL,
      ncol = 4,
      rel_widths = c(0.04, 0.58, 0.34, 0.04)
    ))
  }
  plot_grid(
    NULL,
    legend_grob %||% (ggplot() + theme_void()),
    NULL,
    ncol = 3,
    rel_widths = c(0.04, 0.92, 0.04)
  )
}

assemble_covariate_row_figure <- function(
    rsv_dat,
    flu_dat,
    covid_dat,
    legend_dat,
    model_type,
    phenotype
) {
  rsv_ph <- filter_phenotype(rsv_dat, phenotype)
  flu_ph <- filter_phenotype(flu_dat, phenotype)
  covid_ph <- filter_phenotype(covid_dat, phenotype)

  present <- unique(c(
    as.character(rsv_ph$labels),
    as.character(flu_ph$labels),
    as.character(covid_ph$labels)
  ))
  covariates <- intersect(covariate_row_order, present)

  blocks <- vector("list", length(covariates))
  block_heights <- numeric(length(covariates))

  for (i in seq_along(covariates)) {
    cov <- covariates[[i]]
    legend_h <- 0.36

    pathogen_stack <- plot_grid(
      plot_covariate_pathogen(rsv_ph, "rsv", model_type, cov, show_x = FALSE),
      plot_covariate_pathogen(flu_ph, "flu", model_type, cov, show_x = FALSE),
      plot_covariate_pathogen(covid_ph, "covid", model_type, cov, show_x = TRUE),
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 1, 1)
    )

    disruption_grob <- if (identical(cov, "Age Group")) {
      build_disruption_legend(
        legend_dat,
        model_type = model_type,
        covariate = cov
      )
    } else {
      NULL
    }

    blocks[[i]] <- plot_grid(
      legend_row(
        build_covariate_legend(
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

  combined <- plot_grid(
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

run_cohort_covariate_rows_colour_levels <- function(
    cohort,
    drop_unknown_ethnicity = TRUE
) {
  cohort <<- cohort

  rsv_dat <- collect_pathogen_key_vars(
    cohort, "rsv", model_type, drop_unknown_ethnicity = drop_unknown_ethnicity
  )
  flu_dat <- collect_pathogen_key_vars(
    cohort, "flu", model_type, drop_unknown_ethnicity = drop_unknown_ethnicity
  )
  covid_dat <- collect_pathogen_key_vars(
    cohort, "covid", model_type, drop_unknown_ethnicity = drop_unknown_ethnicity
  )

  legend_dat <- filter_phenotype(covid_dat, "specific")

  specific_fig <- assemble_covariate_row_figure(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "specific"
  )
  sensitive_fig <- assemble_covariate_row_figure(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "sensitive"
  )

  unknown_suffix <- if (isTRUE(drop_unknown_ethnicity)) {
    "excl_unknown"
  } else {
    "incl_unknown"
  }

  out_dir <- here::here(
    "post_check", "plots", "primary_analyses", "condensed_models_key_vars",
    "covariate_rows_colour_levels"
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    here::here(
      out_dir,
      paste0(
        cohort, "_", model_type,
        "_further_specific_mild_vs_severe_key_vars_covariate_rows_colour_levels_",
        unknown_suffix, ".png"
      )
    ),
    specific_fig,
    height = 14,
    width = 8.5
  )
  ggsave(
    here::here(
      out_dir,
      paste0(
        cohort, "_", model_type,
        "_further_sensitive_mild_vs_severe_key_vars_covariate_rows_colour_levels_",
        unknown_suffix, ".png"
      )
    ),
    sensitive_fig,
    height = 14,
    width = 8.5
  )
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohorts <- c(
    "older_adults", "adults", "children_and_adolescents",
    "infants", "infants_subgroup"
  )
} else {
  cohorts <- args
}

for (cohort in cohorts) {
  for (drop_unknown in c(TRUE, FALSE)) {
    unknown_tag <- if (drop_unknown) "excl unknown" else "incl unknown"
    message(
      "Running key-vars covariate-row colour levels (ethnicity_ses, ",
      unknown_tag, "): ", cohort
    )
    tryCatch(
      run_cohort_covariate_rows_colour_levels(
        cohort,
        drop_unknown_ethnicity = drop_unknown
      ),
      error = function(e) {
        message(
          "Failed: cohort=", cohort,
          " model_type=", model_type,
          " drop_unknown=", drop_unknown,
          " :: ", conditionMessage(e)
        )
        NULL
      }
    )
  }
}
