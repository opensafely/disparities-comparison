# Shared ethnicity Unknown exclusion and level-colour palettes for forest plots.
# Key covariates use a fixed registry; anything else gets a stable colour-blind
# khroma palette hashed from the covariate name (same name → same palette
# across all plots).

FOREST_AGE_LEVEL_PALETTE <- c(
  "#F48FB1", "#F06292", "#EC407A", "#E91E63",
  "#D81B60", "#C2185B", "#AD1457", "#880E4F"
)
FOREST_IMD_LEVEL_PALETTE <- c(
  "#4FC3F7", "#29B6F6", "#039BE5", "#0288D1",
  "#0277BD", "#01579B", "#004BA0", "#002171"
)
FOREST_SEX_LEVEL_PALETTE <- c("#66C2A5", "#1B9E77", "#006D2C")
# Household composition: purple sequential (distinct from age/IMD/sex).
FOREST_HH_LEVEL_PALETTE <- c(
  "#E1BEE7", "#CE93D8", "#BA68C8", "#AB47BC",
  "#8E24AA", "#6A1B9A", "#4A148C", "#311B92"
)
# Prior vaccination (flu + COVID share this ramp).
FOREST_PRIOR_VACC_LEVEL_PALETTE <- c(
  "#F6E8C3", "#DFC27D", "#BF812D", "#8C510A",
  "#543005", "#35978F", "#01665E"
)
# Lock colours to the full prior-vacc level set so flu (2 levels) and
# COVID (3 levels, incl. Last Spring) keep the same hex for shared labels.
FOREST_PRIOR_VACC_LOCK_LEVELS <- c(
  "Eligible and Vaccinated Last Autumn",
  "Not Vaccinated in Past Year",
  "Eligible and Vaccinated Last Spring"
)
# Current vaccination (flu + COVID share this ramp).
FOREST_CURRENT_VACC_LEVEL_PALETTE <- c(
  "#E0F3DB", "#A8DDB5", "#4EB3D3", "#2B8CBE",
  "#0868AC", "#084081"
)
FOREST_CURRENT_VACC_LOCK_LEVELS <- c(
  "Vaccinated in Current Year (Yes)",
  "Vaccinated in Current Year (No)"
)
# Pathogen-specific current-vacc wording → shared lock keys.
FOREST_CURRENT_VACC_LEVEL_ALIASES <- c(
  "Flu Vaccination (Yes)" = "Vaccinated in Current Year (Yes)",
  "Covid Vaccination (Yes)" = "Vaccinated in Current Year (Yes)",
  "Flu Vaccination (No)" = "Vaccinated in Current Year (No)",
  "Covid Vaccination (No)" = "Vaccinated in Current Year (No)"
)
FOREST_ETHNICITY_BATLOW_PALETTE <- "batlow"

# Colour-blind-safe khroma schemes used when a covariate is not in the registry.
# Order is fixed so hashing is stable across sessions / plots.
FOREST_KHROMA_FALLBACK_POOL <- c(
  "muted", "vibrant", "light", "dark",
  "mediumcontrast", "highcontrast",
  "sunset", "nightfall",
  "tokyo", "bamako", "lapaz", "imola",
  "turku", "hawaii", "lipari", "acton",
  "devon", "oslo", "nuuk", "bilbao"
)

# Central covariate → palette registry.
# type:
#   "hex"     — sample from a fixed colour ramp
#   "khroma"  — sample from a named khroma scheme
#   "alias"   — reuse another registry entry (`of`)
FOREST_COVARIATE_PALETTE_REGISTRY <- list(
  "Age Group" = list(type = "hex", colours = FOREST_AGE_LEVEL_PALETTE),
  "Maternal Age" = list(type = "alias", of = "Age Group"),
  "IMD Quintile" = list(type = "hex", colours = FOREST_IMD_LEVEL_PALETTE),
  "Ethnicity" = list(type = "khroma", name = FOREST_ETHNICITY_BATLOW_PALETTE),
  "Sex" = list(type = "hex", colours = FOREST_SEX_LEVEL_PALETTE),
  "Household Composition" = list(type = "hex", colours = FOREST_HH_LEVEL_PALETTE),
  "Rurality" = list(type = "khroma", name = "tokyo"),
  # Flu and COVID prior/current vaccination share one scheme each.
  # lock_levels keeps shared labels on the same hex even when one pathogen
  # has fewer levels present in a given plot.
  "Prior Vaccination" = list(
    type = "hex",
    colours = FOREST_PRIOR_VACC_LEVEL_PALETTE,
    lock_levels = FOREST_PRIOR_VACC_LOCK_LEVELS
  ),
  "Prior Vaccination (Flu)" = list(type = "alias", of = "Prior Vaccination"),
  "Prior Vaccination (COVID)" = list(type = "alias", of = "Prior Vaccination"),
  "Current Vaccination" = list(
    type = "hex",
    colours = FOREST_CURRENT_VACC_LEVEL_PALETTE,
    lock_levels = FOREST_CURRENT_VACC_LOCK_LEVELS,
    level_aliases = FOREST_CURRENT_VACC_LEVEL_ALIASES
  ),
  "Current Vaccination (Flu)" = list(type = "alias", of = "Current Vaccination"),
  "Current Vaccination (COVID)" = list(type = "alias", of = "Current Vaccination"),
  "Maternal Smoking Status" = list(type = "khroma", name = "hawaii"),
  "Maternal Drinking" = list(type = "khroma", name = "lipari"),
  "Maternal Drug Usage" = list(type = "khroma", name = "acton"),
  "Maternal Flu Vaccination" = list(type = "khroma", name = "devon"),
  "Maternal Pertussis Vaccination" = list(type = "khroma", name = "oslo"),
  "Smoking Status" = list(type = "khroma", name = "sunset"),
  "Hazardous Drinking" = list(type = "khroma", name = "nightfall"),
  "Drug Usage" = list(type = "khroma", name = "mediumcontrast")
)

FOREST_COVID_DISRUPTION_LEGEND_LABEL <- "COVID-19 related disruption"

# Facet column levels for Mild | Severe (default) or custom pairs (e.g. cohorts).
# Preserves existing factor levels when already a factor.
forest_facet_outcome_levels <- function(outcome_type) {
  if (is.factor(outcome_type)) {
    lv <- levels(outcome_type)
    return(lv[!is.na(lv) & nzchar(lv)])
  }
  present <- unique(as.character(outcome_type))
  present <- present[!is.na(present) & nzchar(present)]
  default <- c("Mild", "Severe")
  if (length(present) > 0L && all(present %in% default)) {
    return(intersect(default, present))
  }
  present
}

# Condensed key-variable row stack (top → bottom).
# Household Composition appears for full models; ethnicity_ses omits it.
FOREST_COVARIATE_ROW_ORDER <- c(
  "Age Group", "IMD Quintile", "Ethnicity", "Household Composition"
)

# Supplemental / full forest facet order (top → bottom).
# Keep Age → IMD → Ethnicity ahead of Sex and the remaining covariates.
FOREST_FACET_GROUP_ORDER <- c(
  "Age Group",
  "IMD Quintile",
  "Ethnicity",
  "Sex",
  "Household Composition",
  "Rurality",
  "Prior Vaccination",
  "Prior Vaccination (Flu)",
  "Prior Vaccination (COVID)",
  "Current Vaccination",
  "Maternal Age",
  "Maternal Pertussis Vaccination",
  "Maternal Flu Vaccination",
  "Maternal Smoking Status",
  "Maternal Drinking",
  "Maternal Drug Usage"
)

FOREST_PATHOGEN_Y_LABS <- c(
  rsv = "RSV",
  flu = "Influenza",
  covid = "COVID-19",
  overall_resp = "Overall Respiratory Virus",
  overall_and_all_cause = "Overall Respiratory Virus"
)

# Safe lookup: unknown pathogen codes return NULL (never subscript-error).
forest_pathogen_y_lab <- function(pathogen) {
  key <- as.character(pathogen)[[1]]
  if (is.na(key) || !nzchar(key) || !key %in% names(FOREST_PATHOGEN_Y_LABS)) {
    return(NULL)
  }
  unname(FOREST_PATHOGEN_Y_LABS[[key]])
}

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

# Older processed inputs used inverted IMD labels (1 = least, 5 = most).
# Detect and remap to UK convention (1 = most, 5 = least).
imd_labels_are_inverted <- function(x) {
  labs <- unique(as.character(x))
  labs <- labs[!is.na(labs) & labs != ""]
  any(labs %in% c("1 (least deprived)", "5 (most deprived)"))
}

remap_imd_label_to_uk <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "1 (least deprived)" ~ "5 (least deprived)",
    x == "5 (most deprived)" ~ "1 (most deprived)",
    x == "2" ~ "4",
    x == "4" ~ "2",
    TRUE ~ x
  )
}

# Fix imd_quintile factor on dummy / processed inputs before GLM attach.
normalise_imd_quintile_to_uk <- function(dat) {
  if (is.null(dat) || !is.data.frame(dat) || !"imd_quintile" %in% names(dat)) {
    return(dat)
  }
  if (!imd_labels_are_inverted(dat$imd_quintile)) {
    return(dat)
  }
  dat %>%
    dplyr::mutate(
      imd_quintile = factor(
        remap_imd_label_to_uk(.data$imd_quintile),
        levels = FOREST_IMD_LEVELS
      ),
      imd_quintile = stats::relevel(.data$imd_quintile, ref = "5 (least deprived)")
    )
}

# Fix IMD labels/terms on forest plot data after attach to a stale inverted dummy.
normalise_forest_imd_labels <- function(dat) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) {
    return(dat)
  }
  has_label <- "label" %in% names(dat)
  has_term <- "term" %in% names(dat)
  if (!has_label && !has_term) {
    return(dat)
  }

  is_imd <- rep(FALSE, nrow(dat))
  if ("variable" %in% names(dat)) {
    is_imd <- is_imd | (!is.na(dat$variable) & dat$variable == "imd_quintile")
  }
  if ("labels" %in% names(dat)) {
    is_imd <- is_imd |
      (!is.na(dat$labels) & as.character(dat$labels) == "IMD Quintile")
  }
  if (!any(is_imd)) {
    return(dat)
  }

  check_vals <- character()
  if (has_label) {
    check_vals <- c(check_vals, as.character(dat$label[is_imd]))
  }
  if (has_term) {
    check_vals <- c(
      check_vals,
      stringr::str_remove(as.character(dat$term[is_imd]), "^imd_quintile")
    )
  }
  if (!imd_labels_are_inverted(check_vals)) {
    return(dat)
  }

  if (has_label) {
    dat <- dat %>%
      dplyr::mutate(
        label = dplyr::if_else(
          is_imd,
          remap_imd_label_to_uk(.data$label),
          as.character(.data$label)
        )
      )
  }
  if (has_term) {
    dat <- dat %>%
      dplyr::mutate(
        term = dplyr::if_else(
          is_imd,
          paste0(
            "imd_quintile",
            remap_imd_label_to_uk(
              stringr::str_remove(as.character(.data$term), "^imd_quintile")
            )
          ),
          as.character(.data$term)
        )
      )
  }
  dat
}

# Dichotomous maternal exposures: drop the "No" (reference) level so facets
# only show the "Yes" contrast (label may already be remapped to the facet name).
FOREST_MATERNAL_BINARY_VARS <- c(
  "maternal_drinking",
  "maternal_drug_usage",
  "maternal_flu_vaccination",
  "maternal_pertussis_vaccination"
)
FOREST_MATERNAL_BINARY_FACETS <- c(
  "Maternal Drinking",
  "Maternal Drug Usage",
  "Maternal Flu Vaccination",
  "Maternal Pertussis Vaccination"
)

drop_maternal_binary_no_levels <- function(dat) {
  if (!is.data.frame(dat) || nrow(dat) == 0L) {
    return(dat)
  }
  if (!"label" %in% names(dat) && !"variable" %in% names(dat)) {
    return(dat)
  }

  lab <- if ("label" %in% names(dat)) {
    as.character(dat$label)
  } else {
    rep(NA_character_, nrow(dat))
  }
  # broom.helpers may still carry the variable prefix before cleaning.
  is_no <- !is.na(lab) & (
    lab == "No" |
      lab %in% paste0(FOREST_MATERNAL_BINARY_VARS, "No")
  )

  by_var <- if ("variable" %in% names(dat)) {
    as.character(dat$variable) %in% FOREST_MATERNAL_BINARY_VARS
  } else {
    rep(FALSE, nrow(dat))
  }
  by_facet <- if ("labels" %in% names(dat)) {
    as.character(dat$labels) %in% FOREST_MATERNAL_BINARY_FACETS
  } else {
    rep(FALSE, nrow(dat))
  }
  is_maternal_binary <- by_var | by_facet

  # Also drop reference rows for these dichotomous maternal exposures when the
  # Yes level has already been remapped to the facet name (label != "No").
  is_ref <- if ("reference_row" %in% names(dat)) {
    as.logical(dat$reference_row)
  } else if ("is_ref_level" %in% names(dat)) {
    as.logical(dat$is_ref_level)
  } else {
    rep(FALSE, nrow(dat))
  }
  is_ref[is.na(is_ref)] <- FALSE

  drop <- is_maternal_binary & (is_no | is_ref)
  if (!any(drop)) {
    return(dat)
  }
  dat[!drop, , drop = FALSE]
}

prepare_forest_plot_data <- function(
    dat,
    drop_unknown_ethnicity = TRUE,
    pathogen = NULL
) {
  if (isTRUE(drop_unknown_ethnicity)) {
    dat <- drop_unknown_ethnicity_level(dat)
  }
  dat <- normalise_forest_imd_labels(dat)
  if (identical(pathogen, "covid")) {
    dat <- drop_forest_points_before_year(dat, FOREST_COVID_MIN_YEAR)
  }
  dat <- drop_maternal_binary_no_levels(dat)
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

# Assign colours from a fixed level order, then subset to levels present.
# level_aliases maps alternate labels onto lock_levels keys (e.g. Flu/COVID
# current-vacc wording → shared Yes/No keys).
locked_forest_level_colours <- function(
    levels,
    base_cols,
    lock_levels,
    level_aliases = NULL
) {
  levels <- clean_forest_level_labels(levels)
  if (length(levels) == 0L) {
    return(character())
  }
  lock_levels <- clean_forest_level_labels(lock_levels)
  lock_cols <- sequential_forest_level_colours(lock_levels, base_cols)

  resolve_key <- function(lvl) {
    if (!is.null(level_aliases) && lvl %in% names(level_aliases)) {
      return(unname(level_aliases[[lvl]]))
    }
    lvl
  }
  keys <- vapply(levels, resolve_key, character(1), USE.NAMES = FALSE)

  out <- stats::setNames(rep(NA_character_, length(levels)), levels)
  known <- keys %in% names(lock_cols)
  out[known] <- unname(lock_cols[keys[known]])

  extras <- unique(levels[!known])
  if (length(extras) > 0L) {
    n_lock <- length(lock_levels)
    n_extra <- length(extras)
    extended <- grDevices::colorRampPalette(base_cols)(n_lock + n_extra)
    extra_cols <- stats::setNames(
      extended[(n_lock + 1L):(n_lock + n_extra)],
      extras
    )
    for (e in extras) {
      out[names(out) == e] <- extra_cols[[e]]
    }
  }
  out
}

khroma_forest_level_colours <- function(levels, palette_name) {
  levels <- clean_forest_level_labels(levels)
  n <- length(levels)
  if (n == 0L) {
    return(character())
  }
  if (!requireNamespace("khroma", quietly = TRUE)) {
    stop(
      "Package 'khroma' is required for forest level colours (",
      palette_name, ")."
    )
  }
  stats::setNames(khroma::colour(palette_name)(n), levels)
}

# Stable 0-based index into a pool from a covariate name (base R only).
# Uses double arithmetic to avoid R integer overflow.
forest_covariate_hash_index <- function(covariate, n_pool) {
  n_pool <- as.integer(n_pool)
  if (!is.finite(n_pool) || n_pool < 1L) {
    return(0L)
  }
  bytes <- utf8ToInt(enc2utf8(as.character(covariate)[[1]]))
  h <- 0
  for (b in bytes) {
    h <- (h * 31 + b) %% 2147483647
  }
  as.integer(h %% n_pool)
}

resolve_forest_covariate_palette <- function(covariate, depth = 0L) {
  covariate <- as.character(covariate)[[1]]
  if (depth > 5L) {
    return(NULL)
  }
  entry <- FOREST_COVARIATE_PALETTE_REGISTRY[[covariate]]
  if (is.null(entry)) {
    return(NULL)
  }
  if (identical(entry$type, "alias")) {
    return(resolve_forest_covariate_palette(entry$of, depth + 1L))
  }
  entry
}

fallback_forest_covariate_palette <- function(covariate) {
  idx <- forest_covariate_hash_index(
    covariate, length(FOREST_KHROMA_FALLBACK_POOL)
  )
  list(
    type = "khroma",
    name = FOREST_KHROMA_FALLBACK_POOL[[idx + 1L]]
  )
}

covariate_forest_level_colours <- function(covariate, levels) {
  entry <- resolve_forest_covariate_palette(covariate)
  if (is.null(entry)) {
    entry <- fallback_forest_covariate_palette(covariate)
  }

  if (identical(entry$type, "hex")) {
    if (!is.null(entry$lock_levels)) {
      locked_forest_level_colours(
        levels,
        entry$colours,
        entry$lock_levels,
        entry$level_aliases
      )
    } else {
      sequential_forest_level_colours(levels, entry$colours)
    }
  } else if (identical(entry$type, "khroma")) {
    khroma_forest_level_colours(levels, entry$name)
  } else {
    fallback <- fallback_forest_covariate_palette(covariate)
    khroma_forest_level_colours(levels, fallback$name)
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

  preferred <- names(FOREST_COVARIATE_PALETTE_REGISTRY)
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
