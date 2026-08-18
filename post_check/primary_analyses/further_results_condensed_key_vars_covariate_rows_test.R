# Test layout for further_results_condensed_key_vars.R
# Current production figure: pathogen rows (RSV / flu / COVID), mild vs severe columns,
#   covariates as facets within each pathogen, legends overlaid on the COVID row.
# This test instead uses:
#   columns: mild & severe
#   rows: age group / IMD / ethnicity
#   sub-rows: RSV / influenza / COVID
#   a small legend row for that covariate above each covariate block
# COVID uses the same 2016-23 seasonal axis as RSV/flu (full panel width) but
#   draws no points before 2019-20, including reference rows.
# Ethnicity "Unknown" is omitted from these test plots only.
# Current collated models used inverted IMD quintiles (1 = least, 5 = most).
# Reverse the whole 1–5 order for this output set only (1↔5, 2↔4, 3 unchanged)
# so display uses UK convention: 1 = most deprived, 5 = least deprived.

library(tidyverse)
library(here)
library(arrow)
library(cowplot)

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

season_start_year <- function(subset) {
  suppressWarnings(as.integer(stringr::str_extract(
    gsub("_", "-", as.character(subset)),
    "^[0-9]{4}"
  )))
}

# Current output set only: reverse the IMD quintile order (1↔5, 2↔4).
relabel_imd_current_outputs <- function(dat) {
  if (is_empty_forest_data(dat) || !"label" %in% names(dat)) {
    return(dat)
  }

  imd_quintile_number <- function(x) {
    x <- as.character(x)
    dplyr::case_when(
      stringr::str_detect(x, "imd_quintile1") | x %in% c("1", "1 (most deprived)", "1 (least deprived)") ~ 1L,
      stringr::str_detect(x, "imd_quintile2") | x == "2" ~ 2L,
      stringr::str_detect(x, "imd_quintile3") | x == "3" ~ 3L,
      stringr::str_detect(x, "imd_quintile4") | x == "4" ~ 4L,
      stringr::str_detect(x, "imd_quintile5") | x %in% c("5", "5 (most deprived)", "5 (least deprived)") ~ 5L,
      TRUE ~ NA_integer_
    )
  }

  canonical_imd_label <- function(q) {
    dplyr::case_when(
      q == 1L ~ "1 (most deprived)",
      q == 5L ~ "5 (least deprived)",
      q %in% 2:4 ~ as.character(q),
      TRUE ~ NA_character_
    )
  }

  is_imd <- (!is.na(dat$variable) & dat$variable == "imd_quintile") |
    as.character(dat$labels) == "IMD Quintile"

  q_label <- imd_quintile_number(dat$label)
  q_term <- if ("term" %in% names(dat)) imd_quintile_number(dat$term) else NA_integer_
  q_var_label <- if ("var_label" %in% names(dat)) {
    imd_quintile_number(dat$var_label)
  } else {
    NA_integer_
  }

  dat %>%
    mutate(
      label = if_else(
        is_imd & !is.na(q_label),
        canonical_imd_label(6L - q_label),
        as.character(.data$label)
      ),
      term = if_else(
        is_imd & "term" %in% names(dat) & !is.na(q_term),
        paste0("imd_quintile", canonical_imd_label(6L - q_term)),
        .data$term
      ),
      var_label = if_else(
        is_imd & "var_label" %in% names(dat) & !is.na(q_var_label),
        canonical_imd_label(6L - q_var_label),
        if ("var_label" %in% names(dat)) as.character(.data$var_label) else NA_character_
      )
    )
}

# Ethnicity "Unknown" only; leave other labels (e.g. Unknown Smoking Status) untouched.
drop_unknown_level <- function(dat) {
  if (is_empty_forest_data(dat) || !"label" %in% names(dat)) {
    return(dat)
  }
  dat %>%
    filter(!stringr::str_detect(as.character(.data$label), "(?i)^unknown$"))
}

# Keep COVID's visual axis aligned with RSV/flu, but never plot pre-2019-20 points.
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
    expand = ggplot2::expansion(mult = c(0.08, 0.08))
  )
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

collect_pathogen_key_vars <- function(cohort, pathogen, model_type) {
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
  dat <- drop_unknown_level(dat)
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

  p <- forest_over_time_plot_all_seasons(
    forest_data = dat,
    pathogen = pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    show_disruption_legend = FALSE,
    log_y = TRUE,
    y_lab = unname(pathogen_y_labs[[pathogen]])
  )

  # Shared 2016-23 axis so COVID panels are the same width as RSV/flu.
  # expand_limits keeps the empty 2016-18 seasons on the COVID scale without
  # adding points; the plot function still only expands COVID refs from 2019.
  # Y-axis: each panel goes up to at least 5, and as far as 10 if the data need it.
  p <- p +
    ggplot2::expand_limits(x = range(full_year_breaks)) +
    shared_season_x_scale() +
    ggplot2::scale_y_log10(
      breaks = log_rate_ratio_axis_breaks,
      minor_breaks = log_rate_ratio_axis_minor_breaks,
      labels = scales::label_number(accuracy = 0.1),
      limits = function(x) {
        lo <- x[[1]]
        hi <- x[[2]]
        if (!is.finite(hi)) {
          hi <- 5
        }
        c(lo, max(5, min(10, hi)))
      }
    ) +
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
      )
    )

  p
}

covariate_colour <- function(dat, covariate) {
  cols <- dat %>%
    filter_covariate(covariate) %>%
    mutate(col = as.character(col)) %>%
    filter(!is.na(col), col != "") %>%
    distinct(col) %>%
    pull(col)
  if (length(cols) == 0L) {
    return("black")
  }
  cols[[1L]]
}

# Horizontal legends must keep the same level order as the original vertical
# legends (get_forest_level_order / shape scale), left-to-right = top-to-bottom.
shape_breaks_in_level_order <- function(p, model_type, pathogen) {
  sc <- p$scales$get_scales("shape")
  breaks <- NULL
  if (!is.null(sc) && !is.null(sc$palette.raw) && length(names(sc$palette.raw)) > 0L) {
    breaks <- names(sc$palette.raw)
  } else if (!is.null(sc)) {
    br <- tryCatch(sc$get_breaks(), error = function(e) NULL)
    if (!is.null(br) && !inherits(br, "waiver") && length(br) > 0L) {
      breaks <- as.character(br)
    }
  }
  if (is.null(breaks) || length(breaks) == 0L) {
    return(NULL)
  }

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
  labs <- trimws(sub("^.*\\|", "", breaks))
  rank <- match(labs, level_order)
  extras <- is.na(rank)
  if (any(extras)) {
    rank[extras] <- length(level_order) + seq_len(sum(extras))
  }
  breaks[order(rank)]
}

build_covariate_legend <- function(
    legend_dat,
    model_type,
    covariate,
    include_disruption = FALSE,
    legend_pathogen = "covid"
) {
  dat <- filter_covariate(legend_dat, covariate)
  if (is_empty_forest_data(dat)) {
    return(NULL)
  }

  cov_col <- covariate_colour(dat, covariate)
  legend_plot <- forest_over_time_plot_all_seasons(
    forest_data = dat,
    pathogen = legend_pathogen,
    model_type = model_type,
    facet_outcome = TRUE,
    show_disruption_legend = include_disruption,
    log_y = TRUE
  )

  shape_breaks <- shape_breaks_in_level_order(
    legend_plot, model_type, legend_pathogen
  )
  n_keys <- if (is.null(shape_breaks) || length(shape_breaks) == 0L) {
    20L
  } else {
    length(shape_breaks)
  }

  legend_plot <- legend_plot +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "left",
      legend.box.just = "left",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      legend.key.width = unit(1.0, "lines"),
      legend.key.height = unit(0.75, "lines"),
      legend.spacing.x = unit(0.35, "lines"),
      legend.spacing.y = unit(0.05, "lines"),
      legend.margin = margin(0, 4, 0, 4),
      legend.box.margin = margin(1, 2, 1, 2)
    ) +
    guides(
      color = "none",
      shape = guide_legend(
        title = covariate,
        nrow = 1,
        ncol = n_keys,
        byrow = TRUE,
        order = 1,
        breaks = shape_breaks,
        override.aes = list(
          size = FOREST_LEGEND_OVERRIDE_CI,
          colour = cov_col,
          fill = cov_col
        )
      ),
      fill = if (isTRUE(include_disruption)) {
        forest_disruption_fill_guide(legend_position = "bottom")
      } else {
        "none"
      }
    )

  tryCatch(
    cowplot::get_legend(legend_plot),
    error = function(e) NULL
  )
}

legend_row <- function(legend_grob) {
  if (is.null(legend_grob)) {
    return(ggplot() + theme_void())
  }
  plot_grid(
    NULL,
    legend_grob,
    NULL,
    ncol = 3,
    rel_widths = c(0.06, 0.88, 0.06)
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
    include_disruption <- identical(cov, "Age Group")
    legend_h <- if (isTRUE(include_disruption)) 0.42 else 0.32

    pathogen_stack <- plot_grid(
      plot_covariate_pathogen(rsv_ph, "rsv", model_type, cov, show_x = FALSE),
      plot_covariate_pathogen(flu_ph, "flu", model_type, cov, show_x = FALSE),
      plot_covariate_pathogen(covid_ph, "covid", model_type, cov, show_x = TRUE),
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 1, 1.18)
    )

    blocks[[i]] <- plot_grid(
      legend_row(
        build_covariate_legend(
          legend_dat,
          model_type = model_type,
          covariate = cov,
          include_disruption = include_disruption
        )
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
    rel_heights = c(0.05, block_heights)
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

run_cohort_covariate_rows_test <- function(cohort) {
  cohort <<- cohort

  rsv_dat <- collect_pathogen_key_vars(cohort, "rsv", model_type)
  flu_dat <- collect_pathogen_key_vars(cohort, "flu", model_type)
  covid_dat <- collect_pathogen_key_vars(cohort, "covid", model_type)

  legend_dat <- filter_phenotype(covid_dat, "specific")

  specific_fig <- assemble_covariate_row_figure(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "specific"
  )
  sensitive_fig <- assemble_covariate_row_figure(
    rsv_dat, flu_dat, covid_dat, legend_dat, model_type, "sensitive"
  )

  out_dir <- here::here(
    "post_check", "plots", "primary_analyses", "condensed_models_key_vars",
    "covariate_rows_test"
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    here::here(
      out_dir,
      paste0(
        cohort, "_", model_type,
        "_further_specific_mild_vs_severe_key_vars_covariate_rows.png"
      )
    ),
    specific_fig,
    height = 16,
    width = 8.5
  )
  ggsave(
    here::here(
      out_dir,
      paste0(
        cohort, "_", model_type,
        "_further_sensitive_mild_vs_severe_key_vars_covariate_rows.png"
      )
    ),
    sensitive_fig,
    height = 16,
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
  message("Running key-vars covariate-row test (ethnicity_ses): ", cohort)
  tryCatch(
    run_cohort_covariate_rows_test(cohort),
    error = function(e) {
      message(
        "Failed: cohort=", cohort,
        " model_type=", model_type,
        " :: ", conditionMessage(e)
      )
      NULL
    }
  )
}
