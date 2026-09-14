# Shared ethnicity Unknown exclusion and level-colour palettes for forest plots.
# Age / IMD: high-contrast sequential; ethnicity: batlow (khroma).

FOREST_AGE_LEVEL_PALETTE <- c(
  "#F48FB1", "#F06292", "#EC407A", "#E91E63",
  "#D81B60", "#C2185B", "#AD1457", "#880E4F"
)
FOREST_IMD_LEVEL_PALETTE <- c(
  "#4FC3F7", "#29B6F6", "#039BE5", "#0288D1",
  "#0277BD", "#01579B", "#004BA0", "#002171"
)
FOREST_ETHNICITY_BATLOW_PALETTE <- "batlow"

FOREST_COVID_DISRUPTION_LEGEND_LABEL <- "COVID-19 related disruption"

FOREST_COVARIATE_ROW_ORDER <- c("Age Group", "IMD Quintile", "Ethnicity")

FOREST_PATHOGEN_Y_LABS <- c(
  rsv = "RSV",
  flu = "Influenza",
  covid = "COVID-19"
)

FOREST_FULL_YEAR_BREAKS <- 2016:2023
FOREST_COVID_MIN_YEAR <- 2019L

# Drop ethnicity "Unknown" only; leave other Unknown* labels (e.g. smoking).
drop_unknown_ethnicity_level <- function(dat) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) {
    return(dat)
  }
  if (!"label" %in% names(dat)) {
    return(dat)
  }
  is_unknown <- stringr::str_detect(
    as.character(dat$label), "(?i)^unknown$"
  )
  is_unknown[is.na(is_unknown)] <- FALSE

  if ("variable" %in% names(dat) || "labels" %in% names(dat)) {
    is_eth <- rep(FALSE, nrow(dat))
    if ("variable" %in% names(dat)) {
      is_eth <- is_eth | (
        !is.na(dat$variable) & dat$variable == "latest_ethnicity_group"
      )
    }
    if ("labels" %in% names(dat)) {
      is_eth <- is_eth | (as.character(dat$labels) == "Ethnicity")
    }
    dat[!(is_eth & is_unknown), , drop = FALSE]
  } else {
    dat[!is_unknown, , drop = FALSE]
  }
}

forest_season_start_year <- function(subset) {
  suppressWarnings(as.integer(stringr::str_extract(
    gsub("_", "-", as.character(subset)),
    "^[0-9]{4}"
  )))
}

drop_forest_points_before_year <- function(dat, min_year) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) {
    return(dat)
  }
  if (!"subset" %in% names(dat)) {
    return(dat)
  }
  dat %>%
    dplyr::mutate(.year = forest_season_start_year(.data$subset)) %>%
    dplyr::filter(is.na(.year) | .year >= min_year) %>%
    dplyr::select(-.year)
}

# Older collated further-model outputs used inverted IMD coding
# (1 = least, 5 = most). Remap to UK convention only when those labels appear.
relabel_imd_inverted_outputs <- function(dat) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) {
    return(dat)
  }
  if (!"label" %in% names(dat)) {
    return(dat)
  }

  is_imd <- FALSE
  if ("variable" %in% names(dat)) {
    is_imd <- !is.na(dat$variable) & dat$variable == "imd_quintile"
  }
  if ("labels" %in% names(dat)) {
    is_imd <- is_imd | as.character(dat$labels) == "IMD Quintile"
  }

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
      TRUE ~ label
    )
  }

  new_label <- dplyr::if_else(
    is_imd,
    remap_imd_label(label_chr, term_chr),
    label_chr
  )

  dat %>%
    dplyr::mutate(
      label = new_label,
      term = dplyr::if_else(
        is_imd & "term" %in% names(dat),
        paste0("imd_quintile", new_label),
        .data$term
      )
    )
}

prepare_forest_plot_data <- function(
    dat,
    drop_unknown_ethnicity = TRUE,
    pathogen = NULL
) {
  if (isTRUE(drop_unknown_ethnicity)) {
    dat <- drop_unknown_ethnicity_level(dat)
  }
  dat <- relabel_imd_inverted_outputs(dat)
  if (identical(pathogen, "covid")) {
    dat <- drop_forest_points_before_year(dat, FOREST_COVID_MIN_YEAR)
  }
  dat
}

clean_forest_level_labels <- function(levels) {
  levels <- as.character(levels)
  levels[!is.na(levels) & levels != ""]
}

sample_forest_cb_palette <- function(base_cols, n) {
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

sequential_forest_level_colours <- function(levels, base_cols) {
  levels <- clean_forest_level_labels(levels)
  n <- length(levels)
  if (n == 0L) {
    return(character())
  }
  stats::setNames(sample_forest_cb_palette(base_cols, n), levels)
}

age_forest_level_colours <- function(levels) {
  sequential_forest_level_colours(levels, FOREST_AGE_LEVEL_PALETTE)
}

imd_forest_level_colours <- function(levels) {
  sequential_forest_level_colours(levels, FOREST_IMD_LEVEL_PALETTE)
}

ethnicity_forest_level_colours <- function(levels) {
  levels <- clean_forest_level_labels(levels)
  n <- length(levels)
  if (n == 0L) {
    return(character())
  }
  if (!requireNamespace("khroma", quietly = TRUE)) {
    stop("Package 'khroma' is required for ethnicity level colours (batlow).")
  }
  stats::setNames(khroma::colour(FOREST_ETHNICITY_BATLOW_PALETTE)(n), levels)
}

covariate_forest_level_colours <- function(covariate, levels) {
  if (identical(covariate, "Age Group") || identical(covariate, "Maternal Age")) {
    age_forest_level_colours(levels)
  } else if (identical(covariate, "IMD Quintile")) {
    imd_forest_level_colours(levels)
  } else if (identical(covariate, "Ethnicity")) {
    ethnicity_forest_level_colours(levels)
  } else if (identical(covariate, "Sex")) {
    sequential_forest_level_colours(
      levels,
      c("#66C2A5", "#1B9E77", "#006D2C")
    )
  } else if (identical(covariate, "Household Composition")) {
    sequential_forest_level_colours(
      levels,
      c("#FDBB84", "#FC8D59", "#E34A33", "#B30000", "#7F0000", "#4A0000")
    )
  } else {
    sequential_forest_level_colours(
      levels,
      c("#B3B3B3", "#737373", "#525252", "#252525")
    )
  }
}

# Named colour vector for every level present in a (possibly multi-covariate) plot.
build_forest_plot_level_colours <- function(dat, model_type, pathogen) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) {
    return(character())
  }
  if (!all(c("labels", "label") %in% names(dat))) {
    return(character())
  }

  covs <- unique(as.character(dat$labels))
  covs <- covs[!is.na(covs) & covs != ""]
  if (length(covs) == 0L) {
    return(character())
  }

  preferred <- c(
    "Age Group", "Maternal Age", "Sex", "Ethnicity", "IMD Quintile",
    "Household Composition", "Rurality", "Prior Vaccination",
    "Current Vaccination", "Maternal Smoking Status"
  )
  covs <- c(intersect(preferred, covs), setdiff(covs, preferred))

  out <- character()
  for (cov in covs) {
    lvl <- ordered_forest_covariate_levels(dat, cov, model_type, pathogen)
    cols <- covariate_forest_level_colours(cov, lvl)
    # Later covariates win only for truly duplicated labels (rare).
    out <- c(out[!names(out) %in% names(cols)], cols)
  }
  out
}

ordered_forest_covariate_levels <- function(dat, covariate, model_type, pathogen) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) {
    return(character())
  }
  in_data <- dat %>%
    dplyr::filter(as.character(.data$labels) == covariate) %>%
    dplyr::pull(.data$label) %>%
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

shared_forest_season_x_scale <- function(
    year_breaks = FOREST_FULL_YEAR_BREAKS
) {
  ggplot2::scale_x_continuous(
    breaks = year_breaks,
    labels = paste0(
      year_breaks, "-",
      stringr::str_sub(as.character(year_breaks + 1L), 3, 4)
    ),
    limits = c(min(year_breaks) - 0.5, max(year_breaks) + 0.5),
    expand = ggplot2::expansion(mult = c(0, 0))
  )
}
