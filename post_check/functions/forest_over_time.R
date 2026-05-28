# Fitted models use 12m+ as reference; `tidy_attach_model()` may mark 0-6m instead.
# Collated coefficients are contrasts for 0-6m and 6-12m only — add an explicit 12m+ row.
fix_covid_prior_vacc_reference_rows <- function(tidy_forest) {
  if (!"time_since_last_covid_vaccination" %in% tidy_forest$variable) {
    return(tidy_forest)
  }

  tidy_forest <- tidy_forest %>%
    dplyr::mutate(
      reference_row = dplyr::if_else(
        variable == "time_since_last_covid_vaccination",
        FALSE,
        reference_row
      )
    )

  ref_template <- tidy_forest %>%
    dplyr::filter(variable == "time_since_last_covid_vaccination") %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      term = "time_since_last_covid_vaccination12m+",
      label = "12m+",
      reference_row = TRUE,
      estimate = 1,
      conf.low = 1,
      conf.high = 1,
      std.error = 0,
      statistic = NA_real_,
      p.value = NA_real_
    )

  dplyr::bind_rows(tidy_forest, ref_template)
}

# Standard log rate-ratio y-axis ticks; omit values outside each facet's limits.
log_rate_ratio_axis_breaks <- function(limits) {
  candidates <- c(0.1, 0.5, 1, 2, 5, 10)
  rng <- range(limits, finite = TRUE, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) return(numeric(0))
  if (rng[1] <= 0) {
    pos <- limits[is.finite(limits) & limits > 0]
    if (length(pos) == 0) return(numeric(0))
    rng[1] <- min(pos)
  }
  breaks <- candidates[candidates >= rng[1] & candidates <= rng[2]]
  if (length(breaks) > 0) return(breaks)
  candidates[which.min(pmax(rng[1] - candidates, candidates - rng[2]))]
}

# Linear y-axis breaks for ratio-of-RR plots (reference at 1).
ratio_axis_breaks <- function(limits) {
  vals <- limits[is.finite(limits)]
  if (length(vals) == 0) {
    return(numeric(0))
  }
  br <- pretty(range(vals, na.rm = TRUE), n = 5)
  rng <- range(vals, na.rm = TRUE)
  if (rng[1] <= 1 && rng[2] >= 1) {
    br <- sort(unique(c(br, 1)))
  }
  br
}

# Log-scale minor ticks (1–9 within each decade); omit values outside facet limits.
log_rate_ratio_axis_minor_breaks <- function(limits) {
  minor_breaks <- rep(1:9, 7) * (10^rep(-3:3, each = 9))
  rng <- range(limits, finite = TRUE, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) return(numeric(0))
  if (rng[1] <= 0) {
    pos <- limits[is.finite(limits) & limits > 0]
    if (length(pos) == 0) return(numeric(0))
    rng[1] <- min(pos)
  }
  minor_breaks[minor_breaks >= rng[1] & minor_breaks <= rng[2]]
}

# Map `time_since_last_covid_vaccination` model terms to plot/legend labels.
# Terms are encoded like `time_since_last_covid_vaccination0-6m`, not `0-6m`.
# Vectorised for use inside dplyr::mutate() / case_when().
format_covid_prior_vacc_label <- function(term, label, variable,
                                          reference_row = FALSE) {
  n <- max(length(term), length(label), length(variable), length(reference_row))
  term_chr <- as.character(term)
  label_chr <- as.character(label)
  variable_chr <- as.character(variable)
  ref <- as.logical(reference_row)
  term_chr <- rep_len(term_chr, n)
  label_chr <- rep_len(label_chr, n)
  variable_chr <- rep_len(variable_chr, n)
  ref <- rep_len(ref, n)
  ref[is.na(ref)] <- FALSE
  term_chr[is.na(term_chr)] <- ""
  label_chr[is.na(label_chr)] <- ""

  dplyr::case_when(
    variable_chr != "time_since_last_covid_vaccination" ~ label_chr,
    ref & stringr::str_detect(term_chr, "12m") ~ "Not Vaccinated in Past Year",
    ref & stringr::str_detect(term_chr, "0-6m") ~ "Eligible and Vaccinated Last Spring",
    stringr::str_detect(label_chr, "time_since_last_covid_vaccination") ~
      repair_covid_prior_vacc_label(label_chr),
    stringr::str_detect(term_chr, "0-6m") | label_chr == "0-6m" ~
      "Eligible and Vaccinated Last Spring",
    stringr::str_detect(term_chr, "6-12m") | label_chr == "6-12m" ~
      "Eligible and Vaccinated Last Autumn",
    stringr::str_detect(term_chr, "12m\\+") | stringr::str_detect(label_chr, "12m\\+") ~
      "Not Vaccinated in Past Year",
    TRUE ~ label_chr
  )
}

# Repair labels already concatenated with the variable name.
repair_covid_prior_vacc_label <- function(label) {
  label_chr <- as.character(label)
  dplyr::case_when(
    stringr::str_detect(label_chr, "0-6m") ~ "Eligible and Vaccinated Last Spring",
    stringr::str_detect(label_chr, "6-12m") ~ "Eligible and Vaccinated Last Autumn",
    stringr::str_detect(label_chr, "12m\\+") ~ "Not Vaccinated in Past Year",
    TRUE ~ label_chr
  )
}

key_exposure_variables <- function() {
  c(
    "imd_quintile",                # IMD
    "latest_ethnicity_group",      # Ethnicity
    "composition_category"         # Household size/composition (project variable name)
  )
}

# COVID dummy inputs omit household composition; join from the season-specific file.
enrich_dummy_household_composition <- function(df_dummy, cohort) {
  if (is.null(df_dummy) || !is.data.frame(df_dummy)) {
    return(df_dummy)
  }
  if ("composition_category" %in% names(df_dummy)) {
    return(df_dummy)
  }
  if (!"patient_id" %in% names(df_dummy)) {
    return(df_dummy)
  }

  hh_path <- here::here(
    "output", "data",
    paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow")
  )
  if (!file.exists(hh_path)) {
    return(df_dummy)
  }

  hh_source <- arrow::read_feather(hh_path)
  hh_cols <- intersect(
    c(
      "patient_id", "composition_category", "household_size",
      "household_size_cat", "household_pseudo_id"
    ),
    names(hh_source)
  )
  if (!"composition_category" %in% hh_cols) {
    return(df_dummy)
  }

  hh <- hh_source %>%
    dplyr::select(dplyr::all_of(hh_cols)) %>%
    dplyr::distinct(.data$patient_id, .keep_all = TRUE)

  dplyr::left_join(df_dummy, hh, by = "patient_id")
}

# Age plus the substantive exposure(s) adjusted for in each model type.
model_key_exposure_variables <- function(model_type, cohort = NULL) {
  cohort_val <- cohort
  if (is.null(cohort_val) && exists("cohort", envir = .GlobalEnv)) {
    cohort_val <- get("cohort", envir = .GlobalEnv)
  }
  is_infant <- cohort_val %in% c("infants", "infants_subgroup")

  exposures <- switch(
    model_type,
    ethnicity = "latest_ethnicity_group",
    ses = "imd_quintile",
    composition = if (is_infant) character(0) else "composition_category",
    ethnicity_ses = c("latest_ethnicity_group", "imd_quintile"),
    ethnicity_composition = if (is_infant) {
      "latest_ethnicity_group"
    } else {
      c("latest_ethnicity_group", "composition_category")
    },
    ses_composition = if (is_infant) {
      "imd_quintile"
    } else {
      c("imd_quintile", "composition_category")
    },
    full = if (is_infant) {
      c("latest_ethnicity_group", "imd_quintile")
    } else {
      c("latest_ethnicity_group", "imd_quintile", "composition_category")
    },
    character(0)
  )

  unique(c("age_band", exposures))
}

filter_forest_to_model_key_vars <- function(forest_data, model_type, cohort = NULL) {
  if (is.null(forest_data) || !is.data.frame(forest_data) || nrow(forest_data) == 0) {
    return(forest_data)
  }
  vars_keep <- model_key_exposure_variables(model_type, cohort)
  vars_keep <- intersect(vars_keep, unique(forest_data$variable))
  if (length(vars_keep) == 0) {
    return(forest_data[0, , drop = FALSE])
  }
  forest_data %>%
    dplyr::filter(.data$variable %in% vars_keep)
}

key_exposure_display_names <- function() {
  tibble::tibble(
    variable = key_exposure_variables(),
    exposure = c("IMD", "Ethnicity", "Household composition")
  )
}

collect_key_exposure_forest_data <- function(
  df_min,
  df_full,
  df_dummy,
  pathogen,
  outcome_type,
  model_types = c(
    "ses",
    "ethnicity",
    "composition",
    "ethnicity_ses",
    "ses_composition",
    "ethnicity_composition",
    "full"
  )
) {
  # `forest()` depends on several globals in this repo, notably `cohort` and `investigation_type`.
  # We assume callers (scripts) set those appropriately, as existing plotting scripts do.
  # Here:
  # - minimally adjusted = base (df_min; further="no")
  # - fully adjusted = further models (df_full; further="yes")

  plots_min <- purrr::map(
    model_types,
    ~ forest(
      df = df_min,
      df_dummy = df_dummy,
      pathogen = pathogen,
      model_type = .x,
      outcome_type = outcome_type,
      further = "no"
    )
  )
  plots_full <- purrr::map(
    model_types,
    ~ forest(
      df = df_full,
      df_dummy = df_dummy,
      pathogen = pathogen,
      model_type = .x,
      outcome_type = outcome_type,
      further = "yes"
    )
  )

  forest_min <- purrr::map2_dfr(
    plots_min,
    model_types,
    ~ {
      d <- attr(.x, "forest_data")
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(NULL)
      d %>%
        dplyr::mutate(
          model_type = .y,
          adjustment = "Minimally adjusted"
        )
    }
  )
  forest_full <- purrr::map2_dfr(
    plots_full,
    model_types,
    ~ {
      d <- attr(.x, "forest_data")
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(NULL)
      d %>%
        dplyr::mutate(
          model_type = .y,
          adjustment = "Fully adjusted"
        )
    }
  )

  forest_data <- dplyr::bind_rows(forest_min, forest_full)
  if (nrow(forest_data) == 0) return(forest_data)

  key_vars <- key_exposure_variables()
  key_vars <- intersect(key_vars, unique(forest_data$variable))

  forest_data %>%
    dplyr::filter(.data$variable %in% key_vars) %>%
    dplyr::left_join(key_exposure_display_names(), by = "variable") %>%
    dplyr::mutate(
      exposure = dplyr::coalesce(.data$exposure, .data$variable),
      subset = gsub("_", "-", .data$subset),
      year = suppressWarnings(as.integer(stringr::str_extract(.data$subset, "^[0-9]{4}"))),
      adjustment = factor(.data$adjustment, levels = c("Minimally adjusted", "Fully adjusted"))
    ) %>%
    dplyr::filter(!is.na(.data$year))
}

forest_key_exposures_over_time_plot <- function(
  df_min,
  df_full,
  df_dummy,
  pathogen,
  outcome_type,
  model_type = "full",
  adjustment = c("base", "further"),
  show_ci = TRUE,
  show_disruption_legend = TRUE
) {
  adjustment <- match.arg(adjustment)

  forest_data <- collect_key_exposure_forest_data(
    df_min = df_min,
    df_full = df_full,
    df_dummy = df_dummy,
    pathogen = pathogen,
    outcome_type = outcome_type,
    model_types = c(model_type)
  )

  if (is.null(forest_data) || nrow(forest_data) == 0) return(ggplot() + theme_void())

  target_adj <- if (identical(adjustment, "further")) "Fully adjusted" else "Minimally adjusted"

  d_use <- forest_data %>%
    dplyr::filter(.data$model_type == model_type, as.character(.data$adjustment) == target_adj)

  if (nrow(d_use) == 0) return(ggplot() + theme_void())

  p <- forest_over_time_plot(
    forest_data = d_use,
    pathogen = pathogen,
    model_type = model_type,
    outcome_type = outcome_type,
    facet_outcome = FALSE,
    label_levels = FALSE,
    show_ci = show_ci,
    show_disruption_legend = show_disruption_legend
  )

  adj_title <- if (identical(adjustment, "further")) "Fully adjusted (further)" else "Minimally adjusted (base)"
  p + labs(title = paste0(adj_title, " — ", pathogen, " ", outcome_type))
}

forest_key_exposures_three_column_plot <- function(
  df_min,
  df_full,
  df_dummy,
  pathogen,
  outcome_type,
  adjustment = c("base", "further"),
  show_ci = TRUE,
  show_disruption_legend = TRUE
) {
  adjustment <- match.arg(adjustment)
  # Build each panel separately from its own model output to avoid
  # any accidental reuse/merging across model types.
  cohort_val <- if (exists("cohort", envir = .GlobalEnv)) get("cohort", envir = .GlobalEnv) else NA_character_
  allow_full <- !(cohort_val %in% c("infants", "infants_subgroup")) &&
    ("composition_category" %in% names(df_dummy))
  df_use <- if (identical(adjustment, "further")) df_full else df_min
  further_flag <- if (identical(adjustment, "further")) "yes" else "no"

  extract_key_forest_data <- function(model_type) {
    p0 <- forest(
      df = df_use,
      df_dummy = df_dummy,
      pathogen = pathogen,
      model_type = model_type,
      outcome_type = outcome_type,
      further = further_flag
    )
    d0 <- attr(p0, "forest_data")
    if (is.null(d0) || !is.data.frame(d0) || nrow(d0) == 0) return(NULL)
    filter_forest_to_model_key_vars(d0, model_type, cohort_val)
  }

  mk_panel <- function(model_type, title_text) {
    d <- extract_key_forest_data(model_type)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    forest_over_time_plot(
      forest_data = d,
      pathogen = pathogen,
      model_type = model_type,
      outcome_type = outcome_type,
      facet_outcome = FALSE,
      label_levels = FALSE,
      show_ci = show_ci,
      show_disruption_legend = show_disruption_legend
    ) +
      ggtitle(title_text) +
      theme(legend.position = "none")
  }

  p_ses <- mk_panel("ses", "IMD only")
  p_eth <- mk_panel("ethnicity", "Ethnicity only")
  p_ses_eth <- mk_panel("ethnicity_ses", "Ethnicity and IMD")

  if (allow_full) {
    p_comp <- mk_panel("composition", "Household composition only")
    p_eth_comp <- mk_panel("ethnicity_composition", "Ethnicity and household composition")
    p_ses_comp <- mk_panel("ses_composition", "IMD and household composition")
    p_full <- mk_panel("full", "Full model")
  } else {
    p_comp <- NULL
    p_eth_comp <- NULL
    p_ses_comp <- NULL
    p_full <- NULL
  }

  # Column 1: univariate exposure panels (IMD, ethnicity, and composition when available).
  col1 <- if (allow_full) {
    cowplot::plot_grid(
      p_ses,
      p_eth,
      p_comp,
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 1, 1)
    )
  } else {
    cowplot::plot_grid(
      p_ses,
      p_eth,
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 1)
    )
  }

  # Column 2: pairwise exposure combinations.
  col2 <- if (allow_full) {
    cowplot::plot_grid(
      p_ses_eth,
      p_ses_comp,
      p_eth_comp,
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(1, 1, 1)
    )
  } else {
    p_ses_eth
  }

  # Column 3: full model (all three exposures).
  col3 <- p_full

  # Shared legend (levels + disruption shading).
  legend_data <- dplyr::bind_rows(
    extract_key_forest_data("ses"),
    extract_key_forest_data("ethnicity"),
    extract_key_forest_data("ethnicity_ses"),
    if (allow_full) {
      list(
        extract_key_forest_data("composition"),
        extract_key_forest_data("ethnicity_composition"),
        extract_key_forest_data("ses_composition"),
        extract_key_forest_data("full")
      )
    } else {
      NULL
    }
  )

  legend_model_type <- if (allow_full) "full" else "ethnicity_ses"
  legend_src <- forest_over_time_plot(
    forest_data = legend_data,
    pathogen = pathogen,
    model_type = legend_model_type,
    outcome_type = outcome_type,
    facet_outcome = FALSE,
    label_levels = FALSE,
    show_ci = show_ci,
    show_disruption_legend = show_disruption_legend
  ) + theme(legend.position = "right")
  legend <- cowplot::get_legend(legend_src)

  pathogen_title <- dplyr::case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19",
    pathogen == "overall_resp" ~ "Overall Respiratory Virus",
    TRUE ~ pathogen
  )
  adj_title <- if (identical(adjustment, "further")) "Fully adjusted (further)" else "Minimally adjusted (base)"
  title <- paste0(adj_title, " — ", pathogen_title, " ", outcome_type, ": key exposures over time")

  cols <- if (is.null(col3)) {
    cowplot::plot_grid(col1, col2, ncol = 2, rel_widths = c(1, 1), align = "h", axis = "tb")
  } else {
    cowplot::plot_grid(col1, col2, col3, ncol = 3, rel_widths = c(1, 1, 1), align = "h", axis = "tb")
  }

  header <- cowplot::ggdraw() +
    cowplot::draw_label(title, x = 0, hjust = 0, fontface = "bold", size = 11)

  body <- cowplot::ggdraw() +
    cowplot::draw_plot(cols, x = 0, y = 0, width = 0.82, height = 1) +
    cowplot::draw_plot(legend, x = 0.83, y = 0, width = 0.17, height = 1)

  cowplot::plot_grid(header, body, ncol = 1, rel_heights = c(0.07, 1))
}

# Normalise season labels for matching (2017_18 and 2017-18 are equivalent).
normalize_season_label <- function(x) {
  gsub("_", "-", as.character(x))
}

filter_forest_to_seasons <- function(forest_data, seasons) {
  if (is.null(forest_data) || nrow(forest_data) == 0) {
    return(forest_data)
  }
  if (is.null(seasons)) {
    return(forest_data)
  }
  seasons_norm <- normalize_season_label(seasons)
  forest_data %>%
    dplyr::mutate(subset = as.character(.data$subset)) %>%
    dplyr::filter(normalize_season_label(.data$subset) %in% seasons_norm)
}

seasons_to_years <- function(seasons) {
  yrs <- suppressWarnings(
    as.integer(stringr::str_extract(normalize_season_label(seasons), "^[0-9]{4}"))
  )
  sort(unique(yrs[!is.na(yrs)]))
}

# Compare figures: COVID data and x-axis are 2020-21 only; RSV/flu use all selected seasons.
seasons_for_pathogen_compare <- function(
    pathogen,
    seasons = c("2017_18", "2018_19", "2020_21")
) {
  if (!identical(pathogen, "covid")) {
    return(seasons)
  }
  norm <- normalize_season_label(seasons)
  out <- seasons[norm == "2020-21"]
  if (length(out) == 0) "2020_21" else out
}

# Collect ethnicity_ses forest data for base and further models (selected seasons).
collect_ethnicity_ses_base_vs_further_data <- function(
  df_min,
  df_full,
  df_dummy,
  pathogen,
  outcome_types = c("Mild", "Severe"),
  seasons = c("2017_18", "2018_19", "2020_21")
) {
  cohort_val <- if (exists("cohort", envir = .GlobalEnv)) {
    get("cohort", envir = .GlobalEnv)
  } else {
    NA_character_
  }

  extract_ethnicity_ses <- function(df_use, further_flag, adjustment_label, outcome_type) {
    p0 <- forest(
      df = df_use,
      df_dummy = df_dummy,
      pathogen = pathogen,
      model_type = "ethnicity_ses",
      outcome_type = outcome_type,
      further = further_flag
    )
    d0 <- attr(p0, "forest_data")
    if (is.null(d0) || !is.data.frame(d0) || nrow(d0) == 0) {
      return(NULL)
    }
    d0 %>%
      filter_forest_to_seasons(seasons) %>%
      filter_forest_to_model_key_vars(., "ethnicity_ses", cohort_val) %>%
      dplyr::mutate(
        adjustment = adjustment_label,
        outcome_type = outcome_type
      )
  }

  purrr::map_dfr(
    outcome_types,
    function(outcome_type) {
      dplyr::bind_rows(
        extract_ethnicity_ses(df_min, "no", "Minimally adjusted", outcome_type),
        extract_ethnicity_ses(df_full, "yes", "Fully adjusted", outcome_type)
      )
    }
  )
}

# Ratio of fully- vs minimally-adjusted RRs (further / base); 1 = no attenuation.
collapse_base_further_to_ratio <- function(
  forest_data,
  base_label = "Minimally adjusted",
  further_label = "Fully adjusted"
) {
  if (is.null(forest_data) || nrow(forest_data) == 0) {
    return(forest_data)
  }
  if (!"adjustment" %in% names(forest_data)) {
    return(forest_data)
  }

  value_cols <- c("estimate", "conf.low", "conf.high", "std.error")
  join_cols <- c(
    "subset", "term", "variable", "label", "labels", "codelist_type", "col",
    "outcome_type"
  )
  join_cols <- intersect(join_cols, names(forest_data))

  dedupe_by_join <- function(df) {
    df %>%
      dplyr::arrange(dplyr::desc(is.finite(.data$estimate))) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(join_cols)), .keep_all = TRUE)
  }

  base_df <- forest_data %>%
    dplyr::filter(.data$adjustment == base_label) %>%
    dedupe_by_join() %>%
    dplyr::select(-dplyr::any_of("adjustment")) %>%
    dplyr::rename_with(
      ~ paste0(., "_base"),
      .cols = dplyr::any_of(value_cols)
    )

  further_df <- forest_data %>%
    dplyr::filter(.data$adjustment == further_label) %>%
    dedupe_by_join() %>%
    dplyr::select(-dplyr::any_of("adjustment")) %>%
    dplyr::rename_with(
      ~ paste0(., "_further"),
      .cols = dplyr::any_of(value_cols)
    )

  merged <- dplyr::inner_join(base_df, further_df, by = join_cols)

  se_log_rr_from_ci <- function(est, lo, hi) {
    dplyr::if_else(
      is.finite(est) & est > 0 & is.finite(lo) & lo > 0 & is.finite(hi) & hi > 0,
      (log(hi) - log(lo)) / (2 * stats::qnorm(0.975)),
      NA_real_
    )
  }

  merged %>%
    dplyr::mutate(
      se_log_base = dplyr::coalesce(
        .data$std.error_base,
        se_log_rr_from_ci(
          .data$estimate_base, .data$conf.low_base, .data$conf.high_base
        )
      ),
      se_log_further = dplyr::coalesce(
        .data$std.error_further,
        se_log_rr_from_ci(
          .data$estimate_further, .data$conf.low_further, .data$conf.high_further
        )
      ),
      log_ratio = log(.data$estimate_further) - log(.data$estimate_base),
      se_log_ratio = sqrt(.data$se_log_base^2 + .data$se_log_further^2),
      estimate = exp(.data$log_ratio),
      conf.low = exp(.data$log_ratio - stats::qnorm(0.975) * .data$se_log_ratio),
      conf.high = exp(.data$log_ratio + stats::qnorm(0.975) * .data$se_log_ratio)
    ) %>%
    dplyr::mutate(
      estimate = dplyr::if_else(
        !is.finite(.data$estimate_base) | .data$estimate_base <= 0 |
          !is.finite(.data$estimate_further) | .data$estimate_further <= 0,
        NA_real_,
        .data$estimate
      ),
      conf.low = dplyr::if_else(
        !is.finite(.data$se_log_ratio) | .data$se_log_ratio <= 0,
        .data$estimate,
        .data$conf.low
      ),
      conf.high = dplyr::if_else(
        !is.finite(.data$se_log_ratio) | .data$se_log_ratio <= 0,
        .data$estimate,
        .data$conf.high
      )
    ) %>%
    dplyr::select(
      -dplyr::ends_with("_base"),
      -dplyr::ends_with("_further"),
      -dplyr::starts_with("se_log_"),
      -log_ratio
    )
}

# One virus panel: ethnicity_ses, base vs further, Mild | Severe columns (no title/legend).
forest_ethnicity_ses_base_vs_further_panel <- function(
  df_min,
  df_full,
  df_dummy,
  pathogen,
  outcome_types = c("Mild", "Severe"),
  seasons = c("2017_18", "2018_19", "2020_21"),
  show_ci = TRUE,
  show_disruption_legend = FALSE,
  adjustment_dodge_width = 0.58,
  level_jitter_width = 0.12
) {
  forest_data <- collect_ethnicity_ses_base_vs_further_data(
    df_min = df_min,
    df_full = df_full,
    df_dummy = df_dummy,
    pathogen = pathogen,
    outcome_types = outcome_types,
    seasons = seasons
  )
  if (nrow(forest_data) == 0) {
    return(ggplot() + theme_void())
  }

  years_include <- suppressWarnings(
    as.integer(stringr::str_extract(normalize_season_label(seasons), "^[0-9]{4}"))
  )
  years_include <- years_include[!is.na(years_include)]
  facet_outcome <- length(outcome_types) > 1L

  forest_over_time_plot_compare(
    forest_data = forest_data,
    pathogen = pathogen,
    model_type = "ethnicity_ses",
    outcome_type = NULL,
    facet_outcome = facet_outcome,
    label_levels = FALSE,
    show_ci = show_ci,
    show_disruption_legend = show_disruption_legend,
    years_include = years_include,
    jitter_width = level_jitter_width,
    adjustment_dodge_width = adjustment_dodge_width,
    level_jitter_width = level_jitter_width
  ) +
    theme(legend.position = "none")
}

# Stack RSV / Flu / COVID base-vs-further panels with shared legends (Mild | Severe columns).
assemble_condensed_base_vs_further <- function(
    rsv_plot,
    flu_plot,
    covid_plot,
    legend_left,
    legend_mid = NULL,
    legend_adj = NULL
) {
  covid_body <- covid_plot
  if (!is.null(legend_mid)) {
    covid_body <- cowplot::ggdraw(covid_body) +
      cowplot::draw_grob(legend_mid, x = 0.44, y = 0.08, width = 0.12, height = 0.84)
  }
  if (!is.null(legend_adj)) {
    covid_body <- cowplot::ggdraw(covid_body) +
      cowplot::draw_grob(legend_adj, x = 0.56, y = 0.08, width = 0.1, height = 0.84)
  }

  covid_row <- cowplot::plot_grid(
    NULL, legend_left, covid_body, NULL,
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

build_shared_legends_base_vs_further <- function(
  legend_dat,
  legend_pathogen = "covid",
  seasons = c("2017_18", "2018_19", "2020_21")
) {
  years_include <- suppressWarnings(
    as.integer(stringr::str_extract(normalize_season_label(seasons), "^[0-9]{4}"))
  )
  years_include <- years_include[!is.na(years_include)]

  groups_mid <- intersect(
    c("Ethnicity", "IMD Quintile"),
    unique(as.character(legend_dat$labels))
  )

  # Left column: adjustment type only (base vs further).
  legend_left <- cowplot::get_legend(
    forest_over_time_plot_compare(
      legend_dat %>% dplyr::slice(1),
      pathogen = legend_pathogen,
      model_type = "ethnicity_ses",
      facet_outcome = TRUE,
      label_levels = FALSE,
      show_disruption_legend = FALSE,
      years_include = years_include
    ) +
      theme(legend.position = "left") +
      guides(shape = "none", fill = "none", color = "none")
  )

  legend_mid <- if (length(groups_mid) > 0) {
    cowplot::get_legend(
      forest_over_time_plot_compare(
        legend_dat %>% dplyr::filter(as.character(.data$labels) %in% groups_mid),
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        label_levels = FALSE,
        show_disruption_legend = FALSE,
        years_include = years_include
      ) +
        theme(legend.position = "left", legend.title = element_blank()) +
        guides(fill = "none", alpha = "none")
    )
  } else {
    NULL
  }

  list(left = legend_left, mid = legend_mid, adj = NULL)
}

# Ethnicity + IMD model: base and further adjustments on one over-time plot for selected seasons.
forest_ethnicity_ses_over_time_base_vs_further_plot <- function(
  df_min,
  df_full,
  df_dummy,
  pathogen,
  outcome_types = c("Mild", "Severe"),
  seasons = c("2017_18", "2018_19", "2020_21"),
  show_ci = TRUE,
  show_disruption_legend = FALSE,
  adjustment_dodge_width = 0.58,
  level_jitter_width = 0.12
) {
  pathogen_title <- dplyr::case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19",
    TRUE ~ pathogen
  )
  season_label <- paste(normalize_season_label(seasons), collapse = ", ")
  facet_outcome <- length(outcome_types) > 1L

  forest_ethnicity_ses_base_vs_further_panel(
    df_min = df_min,
    df_full = df_full,
    df_dummy = df_dummy,
    pathogen = pathogen,
    outcome_types = outcome_types,
    seasons = seasons,
    show_ci = show_ci,
    show_disruption_legend = show_disruption_legend,
    adjustment_dodge_width = adjustment_dodge_width,
    level_jitter_width = level_jitter_width
  ) +
    labs(
      title = paste0(
        "Ethnicity and IMD — ", pathogen_title,
        if (facet_outcome) " (Mild and Severe)" else paste0(" ", outcome_types[[1]]),
        ": base vs further (", season_label, ")"
      )
    )
}

# COVID: axis seasons and synthetic RR=1 points only where estimates exist.
covid_x_breaks_from_data <- function(plot_df, year_breaks) {
  data_years <- plot_df %>%
    dplyr::filter(
      is.finite(.data$estimate),
      !.data$is_ref_level | abs(.data$estimate - 1) >= 1e-3
    ) %>%
    dplyr::pull(.data$year)
  data_years <- sort(unique(as.integer(data_years[!is.na(data_years)])))
  if (length(data_years) == 0L) {
    data_years <- sort(unique(as.integer(plot_df$year[!is.na(plot_df$year)])))
  }
  if (length(data_years) == 0L) {
    return(as.integer(year_breaks))
  }
  x_breaks <- intersect(as.integer(year_breaks), data_years)
  if (length(x_breaks) == 0L) {
    data_years
  } else {
    x_breaks
  }
}

covid_filter_ref_to_axis_years <- function(plot_df, x_breaks, pathogen) {
  if (!identical(pathogen, "covid") || length(x_breaks) == 0L) {
    return(plot_df)
  }
  plot_df %>%
    dplyr::filter(!.data$is_ref_level | .data$year %in% as.integer(x_breaks))
}

# Standard key-exposure over-time plots (all seasons, calendar year x-axis).
# Used by forest_key_exposures_three_column_plot() and forest().
forest_over_time_plot <- function(
  forest_data,
  pathogen,
  model_type,
  outcome_type = NULL,
  facet_outcome = FALSE,
  label_levels = FALSE,
  jitter_width = 0.2,
  show_ci = TRUE,
  fixed_axes = FALSE,
  show_disruption_legend = TRUE,
  y_lab = NULL,
  log_y = TRUE
) {
  if (is.null(forest_data) || nrow(forest_data) == 0) {
    return(ggplot() + theme_void())
  }

  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19",
    pathogen == "overall_resp" ~ "Overall Respiratory Virus",
    TRUE ~ pathogen
  )

  model_title <- case_when(
    model_type == "ethnicity" ~ "Ethnicity",
    model_type == "ses" ~ "IMD Quintile",
    model_type == "composition" ~ "Household Composition",
    model_type == "ethnicity_ses" ~ "Ethnicity and IMD Quintile",
    model_type == "ethnicity_composition" ~ "Ethnicity and Household Composition",
    model_type == "ses_composition" ~ "IMD Quintile and Household Composition",
    model_type == "full" ~ "Ethnicity, IMD Quintile, and Household Composition",
    TRUE ~ model_type
  )

  if (!is.null(outcome_type)) {
    title <- paste0(outcome_type, " ", pathogen_title, " by ", model_title)
  } else {
    title <- paste0(pathogen_title, " by ", model_title)
  }

  plot_df <- forest_data %>%
    filter(!is.na(subset), !is.na(estimate)) %>%
    mutate(
      subset = gsub("_", "-", subset),
      year = suppressWarnings(as.integer(stringr::str_extract(subset, "^[0-9]{4}"))),
      characteristic = dplyr::coalesce(as.character(labels), stringr::str_to_title(gsub("_", " ", variable))),
      characteristic_base = dplyr::case_when(
        characteristic == "Current Vaccination" ~ "Current\nVaccination",
        characteristic == "Prior Vaccination" ~ "Prior\nVaccination",
        characteristic == "IMD Quintile" ~ "IMD\nQuintile",
        characteristic == "Age Group" ~ "Age\nGroup",
        TRUE ~ characteristic
      ),
      series = paste(variable, label, codelist_type, sep = " | "),
      shape_type = factor(
        stringr::str_to_title(as.character(codelist_type)),
        levels = c("Reference", "Specific", "Sensitive")
      )
    ) %>%
    mutate(
      # "Reference level" rows in the model output are typically encoded as RR=1 with CI=(1,1).
      # This is distinct from the "reference phenotype" (codelist_type == "reference").
      is_ref_level = is.finite(estimate) & is.finite(conf.low) & is.finite(conf.high) &
        abs(estimate - 1) < 1e-3 & abs(conf.low - 1) < 1e-3 & abs(conf.high - 1) < 1e-3
    ) %>%
    filter(!is.na(year)) %>%
    arrange(characteristic, series, year)

  # COVID-specific: don't draw reference-level RR=1 points before eligibility.
  # - Current COVID vaccination: from 2020-21 onwards (year >= 2020)
  # - Prior COVID vaccination: from 2021-22 onwards (year >= 2021)
  if (identical(pathogen, "covid")) {
    plot_df <- plot_df %>%
      mutate(
        is_ref_level = is_ref_level & !(
          (characteristic_base %in% c("Current\nVaccination", "Current Vaccination") & year < 2020) |
            (characteristic_base %in% c("Prior\nVaccination", "Prior Vaccination") & year < 2021)
        )
      )
  }

  if (!"outcome_type" %in% names(plot_df)) {
    plot_df <- plot_df %>% mutate(outcome_type = NA_character_)
  }

  if (facet_outcome) {
    plot_df <- plot_df %>%
      mutate(outcome_type = factor(outcome_type, levels = c("Mild", "Severe")))
  }

  # Seasonal year axis.
  # For COVID-19 plots, only show years from 2019-20 onwards.
  year_breaks <- if (identical(pathogen, "covid")) 2019:2023 else 2016:2023
  x_breaks <- year_breaks

  # Remove vaccination points in early COVID seasons.
  # - Current COVID vaccination: suppress before 2020-21 (year < 2020)
  # - Prior COVID vaccination: suppress before 2021-22 (year < 2021)
  # This applies to ALL points (not just reference-level RR=1 rows).
  if (identical(pathogen, "covid")) {
    plot_df <- plot_df %>%
      filter(!(
        (labels %in% c("Current Vaccination") & year < 2020) |
          (labels %in% c("Prior Vaccination") & year < 2021) |
          (characteristic_base %in% c("Current\nVaccination", "Current Vaccination") & year < 2020) |
          (characteristic_base %in% c("Prior\nVaccination", "Prior Vaccination") & year < 2021)
      ))
    x_breaks <- covid_x_breaks_from_data(plot_df, year_breaks)
  }

  # Ensure every reference level has an RR=1 point in each plotted season.
  ref_points <- plot_df %>%
    filter(is_ref_level) %>%
    distinct(labels, outcome_type, variable, label, codelist_type, col, characteristic, characteristic_base, year)

  if (nrow(ref_points) > 0) {
    ref_template <- ref_points %>%
      select(-year) %>%
      distinct() %>%
      mutate(
        min_year_allowed = dplyr::case_when(
          identical(pathogen, "covid") &
            (labels %in% c("Current Vaccination") |
               characteristic_base %in% c("Current\nVaccination", "Current Vaccination")) ~ 2020L,
          identical(pathogen, "covid") &
            (labels %in% c("Prior Vaccination") |
               characteristic_base %in% c("Prior\nVaccination", "Prior Vaccination")) ~ 2021L,
          TRUE ~ min(x_breaks)
        )
      )

    ref_years <- x_breaks
    ref_grid <- ref_template %>%
      tidyr::crossing(year = ref_years) %>%
      filter(year >= min_year_allowed) %>%
      left_join(
        ref_points %>%
          transmute(
            labels, outcome_type, variable, label, codelist_type, col, characteristic, characteristic_base,
            year,
            has_ref = TRUE
          ),
        by = c("labels", "outcome_type", "variable", "label", "codelist_type", "col", "characteristic", "characteristic_base", "year")
      ) %>%
      mutate(has_ref = dplyr::coalesce(has_ref, FALSE)) %>%
      filter(!has_ref) %>%
      mutate(
        subset = paste0(year, "-", stringr::str_sub(as.character(year + 1L), 3, 4)),
        estimate = 1,
        conf.low = 1,
        conf.high = 1,
        is_ref_level = TRUE,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        term = NA_character_,
        model_name = NA_character_,
        investigation_type = NA_character_
      )

    if (nrow(ref_grid) > 0) {
      plot_df <- bind_rows(plot_df, ref_grid)
    }
  }

  plot_df <- covid_filter_ref_to_axis_years(plot_df, x_breaks, pathogen)

  # Split prior-vaccination into pathogen-specific colour groups
  # while keeping the facet label as "Prior Vaccination".
  plot_df <- plot_df %>%
    mutate(
      labels_facet = as.character(labels),
      labels_col = dplyr::case_when(
        as.character(labels) == "Prior Vaccination" & as.character(variable) == "prior_flu_vaccination" ~ "Prior Vaccination (Flu)",
        as.character(labels) == "Prior Vaccination" & as.character(variable) == "time_since_last_covid_vaccination" ~ "Prior Vaccination (COVID)",
        TRUE ~ as.character(labels)
      )
    )

  reference_lines <- plot_df %>%
    distinct(characteristic_base, labels_col, col, outcome_type) %>%
    mutate(yintercept = 1)

  colour_map <- plot_df %>%
    mutate(labels_col = as.character(labels_col), col = as.character(col)) %>%
    filter(!is.na(labels_col), !is.na(col)) %>%
    distinct(labels_col, col) %>%
    deframe()

  # Ensure prior-vaccination subgroups have distinct, stable colours.
  # (This makes the "not collapsed" behaviour visible in the legend/plot.)
  if ("Prior Vaccination (Flu)" %in% names(colour_map)) {
    colour_map[["Prior Vaccination (Flu)"]] <- "#66C2A4"
  }
  if ("Prior Vaccination (COVID)" %in% names(colour_map)) {
    colour_map[["Prior Vaccination (COVID)"]] <- "#98DF8A"
  }

  # Enforce stable group ordering across all plots/legends.
  preferred_group_order <- c(
    "Sex",
    "Age Group",
    "Ethnicity",
    "IMD Quintile",
    "Rurality",
    "Prior Vaccination (Flu)",
    "Prior Vaccination (COVID)",
    "Current Vaccination"
  )
  group_order <- c(
    intersect(preferred_group_order, names(colour_map)),
    setdiff(names(colour_map), preferred_group_order)
  )
  colour_map <- colour_map[group_order]

  cohort_val <- if (exists("cohort", envir = .GlobalEnv)) get("cohort", envir = .GlobalEnv) else NA_character_
  investigation_val <- if (exists("investigation_type", envir = .GlobalEnv)) get("investigation_type", envir = .GlobalEnv) else NA_character_
  level_order <- get_forest_level_order(
    cohort_val, model_type, pathogen, investigation_val, style = "year_mult"
  )
  # Current vaccination: unify Yes/No wording in the legend.
  # (Keep this in sync with the `label = case_when(...)` recoding below.)
  level_order <- dplyr::if_else(
    stringr::str_detect(level_order, "Vaccination\\s*\\(Yes\\)\\s*$"),
    "Vaccinated in Current Year (Yes)",
    level_order
  )
  level_order <- dplyr::if_else(
    stringr::str_detect(level_order, "Vaccination\\s*\\(No\\)\\s*$"),
    "Vaccinated in Current Year (No)",
    level_order
  )

  # Shapes:
  # - legend shows level names (no "(Reference)" entry)
  # - any reference level is shown as a filled circle (in plot + legend)
  # - shapes can repeat across colours
  # - colour legend is removed; group is shown as facet label
  plot_df <- plot_df %>%
    mutate(
      label = dplyr::case_when(
        # Current vaccination: unify Yes/No wording in the legend.
        labels == "Current Vaccination" & stringr::str_detect(as.character(label), "\\(Yes\\)\\s*$") ~ "Vaccinated in Current Year (Yes)",
        labels == "Current Vaccination" & stringr::str_detect(as.character(label), "\\(No\\)\\s*$") ~ "Vaccinated in Current Year (No)",
        TRUE ~ as.character(label)
      ),
      labels_facet = factor(labels_facet, levels = c(
        intersect(c("Sex", "Age Group", "Ethnicity", "IMD Quintile", "Rurality", "Prior Vaccination", "Current Vaccination"), unique(labels_facet)),
        setdiff(unique(labels_facet), c("Sex", "Age Group", "Ethnicity", "IMD Quintile", "Rurality", "Prior Vaccination", "Current Vaccination"))
      )),
      labels_col = factor(labels_col, levels = group_order),
      label = as.character(label)
    )

  # When we build a shared legend that combines Flu + COVID vaccination rows, the
  # pathogen-specific `level_order` may not include COVID-only levels (e.g. "Eligible and Vaccinated Last Spring"),
  # which would otherwise become NA in the legend. Expand `level_order` to include any observed labels.
  observed_levels <- plot_df$label
  observed_levels <- observed_levels[!is.na(observed_levels) & observed_levels != ""]
  # Prefer a sensible order for common vaccination levels if they appear.
  preferred_extra_levels <- c(
    "Eligible and Vaccinated Last Autumn",
    "Not Vaccinated in Past Year",
    "Eligible and Vaccinated Last Spring"
  )
  extras <- setdiff(unique(observed_levels), level_order)
  extras <- c(intersect(preferred_extra_levels, extras), setdiff(extras, preferred_extra_levels))
  level_order <- c(level_order, extras)

  plot_df <- plot_df %>% mutate(label = factor(label, levels = level_order))

  # Shapes:
  # - ONLY reference points use a filled circle (16)
  # - use a small set of non-circle, easily distinguishable shapes
  # - shapes can repeat across groups, but must not repeat within a group
  # - assign the minimum number needed per group (i.e., max levels within that group)
  # Use a small, distinguishable set (includes open square = 0).
  shape_pool <- c(15, 17, 18, 0, 2, 5, 6, 22, 23, 24, 25)
  max_nonref <- plot_df %>%
    group_by(labels_col, variable, label) %>%
    summarise(is_ref = any(is_ref_level), .groups = "drop") %>%
    group_by(labels_col) %>%
    summarise(n_nonref = sum(!is_ref), .groups = "drop") %>%
    summarise(max_n = max(n_nonref, na.rm = TRUE)) %>%
    pull(max_n)
  if (is.finite(max_nonref) && max_nonref > length(shape_pool)) {
    stop("Not enough distinct shapes for number of levels within a group. Please extend `shape_pool`.")
  }

  shape_key_df <- plot_df %>%
    group_by(labels_col, variable, label) %>%
    summarise(is_ref = any(is_ref_level), .groups = "drop") %>%
    arrange(labels_col, variable, label) %>%
    group_by(labels_col, variable) %>%
    mutate(
      nonref_idx = cumsum(!is_ref),
      shape_val = dplyr::if_else(
        is_ref,
        16L,
        as.integer(shape_pool[pmax(nonref_idx, 1L)])
      ),
      shape_key = paste0(as.character(labels_col), " | ", as.character(variable), " | ", as.character(label))
    ) %>%
    ungroup()

  plot_df <- plot_df %>%
    left_join(shape_key_df %>% select(labels_col, variable, label, shape_key), by = c("labels_col", "variable", "label")) %>%
    mutate(shape_key = factor(shape_key, levels = shape_key_df$shape_key))

  shape_map <- stats::setNames(shape_key_df$shape_val, shape_key_df$shape_key)
  shape_labels <- stats::setNames(stringr::str_wrap(as.character(shape_key_df$label), width = 18), shape_key_df$shape_key)
  shape_legend_cols <- {
    keys <- names(shape_map)
    grp <- sub("\\s*\\|\\s*.*$", "", keys)
    cols <- unname(colour_map[grp])
    cols[is.na(cols)] <- "black"
    cols
  }

  # Always jitter horizontally within each group/year/outcome to improve separation.
  # Offsets follow the legend order of `shape_key`.
  plot_df <- plot_df %>%
    group_by(labels_facet, year, outcome_type) %>%
    arrange(as.integer(shape_key), .by_group = TRUE) %>%
    mutate(
      jitter_rank = dplyr::row_number(),
      jitter_n = dplyr::n(),
      # Total horizontal span occupied by the set around the true year.
      span = pmin(0.9, jitter_width * sqrt(pmax(jitter_n, 1L))),
      step = dplyr::if_else(jitter_n > 1, span / (jitter_n - 1), 0),
      x_plot = year + (jitter_rank - (jitter_n + 1) / 2) * step
    ) %>%
    ungroup()

  y_vals_for_range <- c(plot_df$estimate, plot_df$conf.low, plot_df$conf.high)
  y_vals_for_range <- y_vals_for_range[is.finite(y_vals_for_range)]
  if (isTRUE(log_y)) {
    y_vals_for_range <- y_vals_for_range[y_vals_for_range > 0]
    if (length(y_vals_for_range) == 0) {
      y_vals_for_range <- c(0.1, 10)
    }
    shade_ymin <- max(min(y_vals_for_range), 1e-6)
    shade_ymax <- max(y_vals_for_range)
  } else {
    if (length(y_vals_for_range) == 0) {
      y_vals_for_range <- c(0.5, 1.5)
    }
    pad <- diff(range(y_vals_for_range)) * 0.05
    if (!is.finite(pad) || pad <= 0) {
      pad <- 0.1
    }
    shade_ymin <- min(y_vals_for_range) - pad
    shade_ymax <- max(y_vals_for_range) + pad
  }
  disruption_label <- "Disrupted routes of transmission"
  disruption_label_wrapped <- stringr::str_wrap(disruption_label, width = 18)
  shading_df <- tidyr::crossing(
    year_band = c(2020, 2021),
    labels_facet = levels(plot_df$labels_facet),
    outcome_type = if (isTRUE(facet_outcome)) levels(plot_df$outcome_type) else NA_character_
  ) %>%
    dplyr::filter(year_band %in% x_breaks) %>%
    mutate(
      ymin = shade_ymin,
      ymax = shade_ymax,
      disruption = disruption_label_wrapped
    )

  base_plot <- ggplot(
    plot_df,
    aes(
      x = x_plot,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      group = series,
      color = labels_col
    )
  ) +
    geom_rect(
      data = shading_df,
      aes(
        xmin = year_band - 0.5,
        xmax = year_band + 0.5,
        ymin = ymin,
        ymax = ymax,
        fill = disruption
      ),
      inherit.aes = FALSE,
      alpha = 0.5
    ) +
    geom_hline(
      data = reference_lines,
      aes(yintercept = yintercept, color = labels_col),
      inherit.aes = FALSE,
      linetype = 2,
      linewidth = 0.4,
      alpha = 0.8
    ) +
    {
      if (isTRUE(show_ci)) {
        geom_pointrange(
          aes(shape = shape_key),
          alpha = 0.8,
          linewidth = 0.35,
          fatten = 2.4,
          na.rm = TRUE,
          position = position_identity()
        )
      } else {
        geom_point(
          aes(shape = shape_key),
          alpha = 0.85,
          size = 1.6,
          na.rm = TRUE
        )
      }
    } +
    scale_x_continuous(
      breaks = x_breaks,
      labels = paste0(x_breaks, "-", stringr::str_sub(as.character(x_breaks + 1), 3, 4)),
      # Add extra padding so jittered points never clip at edges.
      #limits = c(min(year_breaks) - 0.6, max(year_breaks) + 0.6),
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    {
      if (isTRUE(log_y)) {
        if (isTRUE(fixed_axes)) {
          scale_y_log10(
            breaks = scales::log_breaks(n = 3),
            minor_breaks = log_rate_ratio_axis_minor_breaks,
            labels = scales::label_number(accuracy = 0.1)
          )
        } else {
          scale_y_log10(
            breaks = log_rate_ratio_axis_breaks,
            minor_breaks = log_rate_ratio_axis_minor_breaks,
            labels = scales::label_number(accuracy = 0.1)
          )
        }
      } else {
        scale_y_continuous(
          breaks = ratio_axis_breaks,
          labels = scales::label_number(accuracy = 0.01),
          expand = ggplot2::expansion(mult = c(0.06, 0.06))
        )
      }
    } +
    {
      if (isTRUE(log_y)) {
        annotation_logticks(base = 10, sides = "l")
      }
    } +
    scale_color_manual(values = colour_map, drop = FALSE) +
    scale_fill_manual(
      values = setNames("grey85", disruption_label_wrapped),
      breaks = disruption_label_wrapped,
      drop = FALSE
    ) +
    scale_shape_manual(values = shape_map, labels = shape_labels, drop = FALSE) +
    labs(
      title = NULL,
      x = NULL,
      y = if (!is.null(y_lab)) {
        y_lab
      } else {
        paste(pathogen_title, "Rate Ratio")
      },
      color = "Characteristic",
      fill = NULL,
      shape = "Level"
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 6.5),
      axis.text.y = element_text(size = 6.5),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      strip.background = element_blank(),
      strip.text.y.left = element_blank(),
      strip.text.y.right = element_text(size = 6.25, face = "bold"),
      strip.text.x = element_blank(),
      panel.spacing.x = unit(if (identical(pathogen, "covid")) 8.3 else 0.18, "lines"),
      plot.margin = margin(5.5, if (identical(pathogen, "covid")) 1 else 4, 5.5, 2.5),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.key.width = unit(if (isTRUE(show_ci)) 1.4 else 2.2, "lines"),
      legend.key.height = unit(if (isTRUE(show_ci)) 1.4 else 2.2, "lines")
    )

  base_plot <- base_plot +
    guides(
      color = "none",
      fill = if (isTRUE(show_disruption_legend)) {
        guide_legend(
          ncol = 1,
          order = 2,
          override.aes = list(alpha = 0.5)
        )
      } else {
        "none"
      },
      shape = guide_legend(
        ncol = 1,
        order = 1,
        override.aes = list(
          size = 0.5,
          colour = shape_legend_cols,
          fill = shape_legend_cols
        )
      )
    )

  # For test-model style: keep panel heights fixed, but allow y-ranges to vary.
  facet_scales <- "free_y"
  if (facet_outcome) {
    if (requireNamespace("ggh4x", quietly = TRUE)) {
      base_plot <- base_plot + ggh4x::facet_grid2(
        labels_facet ~ outcome_type,
        scales = facet_scales,
        space = "fixed",
        axes = "y",
        labeller = labeller(labels_facet = label_wrap_gen(width = 14))
      )
    } else {
      base_plot <- base_plot + facet_grid(
        labels_facet ~ outcome_type,
        scales = facet_scales,
        space = "fixed",
        labeller = labeller(labels_facet = label_wrap_gen(width = 14))
      )
    }
  } else {
    if (requireNamespace("ggh4x", quietly = TRUE)) {
      base_plot <- base_plot + ggh4x::facet_grid2(
        labels_facet ~ .,
        scales = facet_scales,
        space = "fixed",
        axes = "y",
        labeller = labeller(labels_facet = label_wrap_gen(width = 14))
      )
    } else {
      base_plot <- base_plot + facet_grid(
        labels_facet ~ .,
        scales = facet_scales,
        space = "fixed",
        labeller = labeller(labels_facet = label_wrap_gen(width = 14))
      )
    }
  }

  return(base_plot)
}

# Selected-season plots comparing minimally vs fully adjusted models on one figure.
# Used by forest_ethnicity_ses_over_time_base_vs_further_plot().
forest_over_time_plot_compare <- function(
  forest_data,
  pathogen,
  model_type,
  outcome_type = NULL,
  facet_outcome = FALSE,
  label_levels = FALSE,
  jitter_width = 0.2,
  show_ci = TRUE,
  fixed_axes = FALSE,
  show_disruption_legend = TRUE,
  years_include = NULL,
  season_axis_years = NULL,
  disruption_season_width_scale = 1,
  adjustment_dodge_width = 0.22,
  adjustment_layout = c("dodge", "stack"),
  level_jitter_width = NULL,
  y_lab = NULL,
  log_y = TRUE
) {
  adjustment_layout <- match.arg(adjustment_layout)
  if (is.null(forest_data) || nrow(forest_data) == 0) {
    return(ggplot() + theme_void())
  }

  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19",
    pathogen == "overall_resp" ~ "Overall Respiratory Virus",
    TRUE ~ pathogen
  )

  model_title <- case_when(
    model_type == "ethnicity" ~ "Ethnicity",
    model_type == "ses" ~ "IMD Quintile",
    model_type == "composition" ~ "Household Composition",
    model_type == "ethnicity_ses" ~ "Ethnicity and IMD Quintile",
    model_type == "ethnicity_composition" ~ "Ethnicity and Household Composition",
    model_type == "ses_composition" ~ "IMD Quintile and Household Composition",
    model_type == "full" ~ "Ethnicity, IMD Quintile, and Household Composition",
    TRUE ~ model_type
  )

  if (!is.null(outcome_type)) {
    title <- paste0(outcome_type, " ", pathogen_title, " by ", model_title)
  } else {
    title <- paste0(pathogen_title, " by ", model_title)
  }

  plot_df <- forest_data %>%
    filter(!is.na(subset), !is.na(estimate)) %>%
    mutate(
      subset = gsub("_", "-", subset),
      year = suppressWarnings(as.integer(stringr::str_extract(subset, "^[0-9]{4}"))),
      characteristic = dplyr::coalesce(as.character(labels), stringr::str_to_title(gsub("_", " ", variable))),
      characteristic_base = dplyr::case_when(
        characteristic == "Current Vaccination" ~ "Current\nVaccination",
        characteristic == "Prior Vaccination" ~ "Prior\nVaccination",
        characteristic == "IMD Quintile" ~ "IMD\nQuintile",
        characteristic == "Household Composition" ~ "Household\nComposition",
        characteristic == "Age Group" ~ "Age\nGroup",
        TRUE ~ characteristic
      ),
      series = {
        parts <- list(
          as.character(.data$variable),
          as.character(.data$label),
          as.character(.data$codelist_type)
        )
        if ("adjustment" %in% names(.)) {
          parts <- c(parts, list(as.character(.data$adjustment)))
        }
        if ("outcome_type" %in% names(.) && any(!is.na(.data$outcome_type))) {
          parts <- c(parts, list(as.character(.data$outcome_type)))
        }
        purrr::pmap_chr(parts, paste, sep = " | ")
      },
      shape_type = factor(
        stringr::str_to_title(as.character(codelist_type)),
        levels = c("Reference", "Specific", "Sensitive")
      )
    ) %>%
    mutate(
      # "Reference level" rows in the model output are typically encoded as RR=1 with CI=(1,1).
      # This is distinct from the "reference phenotype" (codelist_type == "reference").
      is_ref_level = is.finite(estimate) & is.finite(conf.low) & is.finite(conf.high) &
        abs(estimate - 1) < 1e-3 & abs(conf.low - 1) < 1e-3 & abs(conf.high - 1) < 1e-3
    ) %>%
    filter(!is.na(year)) %>%
    arrange(characteristic, series, year)

  if (!is.null(years_include)) {
    years_include <- as.integer(years_include)
    plot_df <- plot_df %>% dplyr::filter(.data$year %in% years_include)
    if (nrow(plot_df) == 0) {
      return(ggplot() + theme_void())
    }
  }

  # COVID-specific: don't draw reference-level RR=1 points before eligibility.
  # - Current COVID vaccination: from 2020-21 onwards (year >= 2020)
  # - Prior COVID vaccination: from 2021-22 onwards (year >= 2021)
  if (identical(pathogen, "covid")) {
    plot_df <- plot_df %>%
      mutate(
        is_ref_level = is_ref_level & !(
          (characteristic_base %in% c("Current\nVaccination", "Current Vaccination") & year < 2020) |
            (characteristic_base %in% c("Prior\nVaccination", "Prior Vaccination") & year < 2021)
        )
      )
  }

  if (!"outcome_type" %in% names(plot_df)) {
    plot_df <- plot_df %>% mutate(outcome_type = NA_character_)
  }

  if (facet_outcome) {
    plot_df <- plot_df %>%
      mutate(outcome_type = factor(outcome_type, levels = c("Mild", "Severe")))
  }

  # Seasonal year axis.
  # For COVID-19 plots, only show years from 2019-20 onwards.
  year_breaks <- if (identical(pathogen, "covid")) 2019:2023 else 2016:2023
  x_breaks <- year_breaks
  if (!is.null(years_include)) {
    x_breaks <- as.integer(years_include)
    year_breaks <- x_breaks
  }

  # Non-consecutive season subsets (e.g. 2017-18, 2018-19, 2020-21): use evenly
  # spaced x positions so the missing 2019-20 season does not leave a gap.
  use_discrete_season_axis <- !is.null(years_include)
  season_axis <- NULL
  if (isTRUE(use_discrete_season_axis)) {
    season_axis <- tibble::tibble(
      year = sort(as.integer(years_include)),
      season_x = seq_along(.data$year),
      season_label = paste0(
        .data$year, "-",
        stringr::str_sub(as.character(.data$year + 1L), 3, 4)
      )
    )
    plot_df <- plot_df %>%
      dplyr::left_join(season_axis, by = "year")
  }

  # Remove vaccination points in early COVID seasons.
  # - Current COVID vaccination: suppress before 2020-21 (year < 2020)
  # - Prior COVID vaccination: suppress before 2021-22 (year < 2021)
  # This applies to ALL points (not just reference-level RR=1 rows).
  if (identical(pathogen, "covid")) {
    plot_df <- plot_df %>%
      filter(!(
        (labels %in% c("Current Vaccination") & year < 2020) |
          (labels %in% c("Prior Vaccination") & year < 2021) |
          (characteristic_base %in% c("Current\nVaccination", "Current Vaccination") & year < 2020) |
          (characteristic_base %in% c("Prior\nVaccination", "Prior Vaccination") & year < 2021)
      ))
    if (is.null(years_include)) {
      x_breaks <- covid_x_breaks_from_data(plot_df, year_breaks)
    } else {
      x_breaks <- intersect(
        as.integer(x_breaks),
        covid_x_breaks_from_data(plot_df, year_breaks)
      )
      if (length(x_breaks) == 0L) {
        x_breaks <- covid_x_breaks_from_data(plot_df, year_breaks)
      }
    }
  }

  # Ensure every reference level has an RR=1 point in each plotted season.
  ref_point_cols <- c(
    "labels", "outcome_type", "variable", "label", "codelist_type", "col",
    "characteristic", "characteristic_base", "year"
  )
  if ("adjustment" %in% names(plot_df)) {
    ref_point_cols <- c(ref_point_cols, "adjustment")
  }

  ref_points <- plot_df %>%
    filter(is_ref_level) %>%
    distinct(dplyr::across(dplyr::all_of(ref_point_cols)))

  if (nrow(ref_points) > 0) {
    ref_template <- ref_points %>%
      select(-year) %>%
      distinct() %>%
      mutate(
        min_year_allowed = dplyr::case_when(
          identical(pathogen, "covid") &
            (labels %in% c("Current Vaccination") |
               characteristic_base %in% c("Current\nVaccination", "Current Vaccination")) ~ 2020L,
          identical(pathogen, "covid") &
            (labels %in% c("Prior Vaccination") |
               characteristic_base %in% c("Prior\nVaccination", "Prior Vaccination")) ~ 2021L,
          TRUE ~ min(x_breaks)
        )
      )

    ref_join_cols <- c(
      "labels", "outcome_type", "variable", "label", "codelist_type", "col",
      "characteristic", "characteristic_base", "year"
    )
    if ("adjustment" %in% names(ref_points)) {
      ref_join_cols <- c(ref_join_cols, "adjustment")
    }

    ref_years <- x_breaks
    if (identical(pathogen, "covid")) {
      data_years <- plot_df %>%
        dplyr::filter(
          is.finite(.data$estimate),
          !.data$is_ref_level | abs(.data$estimate - 1) >= 1e-3
        ) %>%
        dplyr::pull(.data$year) %>%
        unique()
      if (length(data_years) > 0L) {
        ref_years <- intersect(as.integer(x_breaks), as.integer(data_years))
      }
    }

    ref_grid <- ref_template %>%
      tidyr::crossing(year = ref_years) %>%
      filter(year >= min_year_allowed) %>%
      left_join(
        ref_points %>%
          transmute(
            dplyr::across(dplyr::all_of(ref_join_cols)),
            has_ref = TRUE
          ),
        by = ref_join_cols
      ) %>%
      mutate(has_ref = dplyr::coalesce(has_ref, FALSE)) %>%
      filter(!has_ref) %>%
      mutate(
        subset = paste0(year, "-", stringr::str_sub(as.character(year + 1L), 3, 4)),
        estimate = 1,
        conf.low = 1,
        conf.high = 1,
        is_ref_level = TRUE,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        term = NA_character_,
        model_name = NA_character_,
        investigation_type = NA_character_
      )

    if (nrow(ref_grid) > 0) {
      if (isTRUE(use_discrete_season_axis)) {
        ref_grid <- ref_grid %>%
          dplyr::left_join(season_axis, by = "year")
      }
      plot_df <- bind_rows(plot_df, ref_grid)
    }
  }

  plot_df <- covid_filter_ref_to_axis_years(plot_df, x_breaks, pathogen)

  # Split prior-vaccination into pathogen-specific colour groups
  # while keeping the facet label as "Prior Vaccination".
  plot_df <- plot_df %>%
    mutate(
      labels_facet = as.character(labels),
      labels_col = dplyr::case_when(
        as.character(labels) == "Prior Vaccination" & as.character(variable) == "prior_flu_vaccination" ~ "Prior Vaccination (Flu)",
        as.character(labels) == "Prior Vaccination" & as.character(variable) == "time_since_last_covid_vaccination" ~ "Prior Vaccination (COVID)",
        TRUE ~ as.character(labels)
      )
    )

  colour_map <- plot_df %>%
    mutate(labels_col = as.character(labels_col), col = as.character(col)) %>%
    filter(!is.na(labels_col), !is.na(col)) %>%
    distinct(labels_col, col) %>%
    deframe()

  # Ensure prior-vaccination subgroups have distinct, stable colours.
  # (This makes the "not collapsed" behaviour visible in the legend/plot.)
  if ("Prior Vaccination (Flu)" %in% names(colour_map)) {
    colour_map[["Prior Vaccination (Flu)"]] <- "#66C2A4"
  }
  if ("Prior Vaccination (COVID)" %in% names(colour_map)) {
    colour_map[["Prior Vaccination (COVID)"]] <- "#98DF8A"
  }

  # Enforce stable group ordering across all plots/legends.
  preferred_group_order <- c(
    "Sex",
    "Age Group",
    "Ethnicity",
    "IMD Quintile",
    "Household Composition",
    "Rurality",
    "Prior Vaccination (Flu)",
    "Prior Vaccination (COVID)",
    "Current Vaccination"
  )
  group_order <- c(
    intersect(preferred_group_order, names(colour_map)),
    setdiff(names(colour_map), preferred_group_order)
  )
  colour_map <- colour_map[group_order]

  cohort_val <- if (exists("cohort", envir = .GlobalEnv)) get("cohort", envir = .GlobalEnv) else NA_character_
  investigation_val <- if (exists("investigation_type", envir = .GlobalEnv)) get("investigation_type", envir = .GlobalEnv) else NA_character_
  level_order <- get_forest_level_order(
    cohort_val, model_type, pathogen, investigation_val, style = "year_mult"
  )
  # Current vaccination: unify Yes/No wording in the legend.
  # (Keep this in sync with the `label = case_when(...)` recoding below.)
  level_order <- dplyr::if_else(
    stringr::str_detect(level_order, "Vaccination\\s*\\(Yes\\)\\s*$"),
    "Vaccinated in Current Year (Yes)",
    level_order
  )
  level_order <- dplyr::if_else(
    stringr::str_detect(level_order, "Vaccination\\s*\\(No\\)\\s*$"),
    "Vaccinated in Current Year (No)",
    level_order
  )

  # Shapes:
  # - legend shows level names (no "(Reference)" entry)
  # - any reference level is shown as a filled circle (in plot + legend)
  # - shapes can repeat across colours
  # - colour legend is removed; group is shown as facet label
  plot_df <- plot_df %>%
    mutate(
      label = dplyr::case_when(
        # Current vaccination: unify Yes/No wording in the legend.
        labels == "Current Vaccination" & stringr::str_detect(as.character(label), "\\(Yes\\)\\s*$") ~ "Vaccinated in Current Year (Yes)",
        labels == "Current Vaccination" & stringr::str_detect(as.character(label), "\\(No\\)\\s*$") ~ "Vaccinated in Current Year (No)",
        .data$variable == "time_since_last_covid_vaccination" ~
          format_covid_prior_vacc_label(
            term = if ("term" %in% names(.)) .data$term else NA_character_,
            label = .data$label,
            variable = .data$variable,
            reference_row = if ("is_ref_level" %in% names(.)) .data$is_ref_level else FALSE
          ),
        stringr::str_detect(as.character(.data$label), "time_since_last_covid_vaccination") ~
          repair_covid_prior_vacc_label(.data$label),
        TRUE ~ as.character(.data$label)
      ),
      labels_facet = factor(labels_facet, levels = c(
        intersect(
          c(
            "Sex", "Age Group", "Ethnicity", "IMD Quintile", "Household Composition",
            "Rurality", "Prior Vaccination", "Current Vaccination"
          ),
          unique(labels_facet)
        ),
        setdiff(
          unique(labels_facet),
          c(
            "Sex", "Age Group", "Ethnicity", "IMD Quintile", "Household Composition",
            "Rurality", "Prior Vaccination", "Current Vaccination"
          )
        )
      )),
      labels_col = factor(labels_col, levels = group_order),
      label = as.character(label)
    )

  # When we build a shared legend that combines Flu + COVID vaccination rows, the
  # pathogen-specific `level_order` may not include COVID-only levels (e.g. "Eligible and Vaccinated Last Spring"),
  # which would otherwise become NA in the legend. Expand `level_order` to include any observed labels.
  observed_levels <- plot_df$label
  observed_levels <- observed_levels[!is.na(observed_levels) & observed_levels != ""]
  # Prefer a sensible order for common vaccination levels if they appear.
  preferred_extra_levels <- c(
    "Eligible and Vaccinated Last Autumn",
    "Not Vaccinated in Past Year",
    "Eligible and Vaccinated Last Spring"
  )
  extras <- setdiff(unique(observed_levels), level_order)
  extras <- c(intersect(preferred_extra_levels, extras), setdiff(extras, preferred_extra_levels))
  level_order <- c(level_order, extras)

  plot_df <- plot_df %>% mutate(label = factor(label, levels = level_order))

  # Shapes:
  # - ONLY reference points use a filled circle (16)
  # - use a small set of non-circle, easily distinguishable shapes
  # - shapes can repeat across groups, but must not repeat within a group
  # - assign the minimum number needed per group (i.e., max levels within that group)
  # Use a small, distinguishable set (includes open square = 0).
  shape_pool <- c(15, 17, 18, 0, 2, 5, 6, 22, 23, 24, 25)
  max_nonref <- plot_df %>%
    group_by(labels_col, variable, label) %>%
    summarise(is_ref = any(is_ref_level), .groups = "drop") %>%
    group_by(labels_col) %>%
    summarise(n_nonref = sum(!is_ref), .groups = "drop") %>%
    summarise(max_n = max(n_nonref, na.rm = TRUE)) %>%
    pull(max_n)
  if (is.finite(max_nonref) && max_nonref > length(shape_pool)) {
    stop("Not enough distinct shapes for number of levels within a group. Please extend `shape_pool`.")
  }

  shape_key_df <- plot_df %>%
    group_by(labels_col, variable, label) %>%
    summarise(is_ref = any(is_ref_level), .groups = "drop") %>%
    arrange(labels_col, variable, label) %>%
    group_by(labels_col, variable) %>%
    mutate(
      nonref_idx = cumsum(!is_ref),
      shape_val = dplyr::if_else(
        is_ref,
        16L,
        as.integer(shape_pool[pmax(nonref_idx, 1L)])
      ),
      shape_key = paste0(as.character(labels_col), " | ", as.character(variable), " | ", as.character(label))
    ) %>%
    ungroup()

  plot_df <- plot_df %>%
    left_join(shape_key_df %>% select(labels_col, variable, label, shape_key), by = c("labels_col", "variable", "label")) %>%
    mutate(shape_key = factor(shape_key, levels = shape_key_df$shape_key))

  shape_map <- stats::setNames(shape_key_df$shape_val, shape_key_df$shape_key)
  shape_labels <- stats::setNames(stringr::str_wrap(as.character(shape_key_df$label), width = 18), shape_key_df$shape_key)
  shape_legend_cols <- {
    keys <- names(shape_map)
    grp <- sub("\\s*\\|\\s*.*$", "", keys)
    cols <- unname(colour_map[grp])
    cols[is.na(cols)] <- "black"
    cols
  }

  show_adjustment <- "adjustment" %in% names(plot_df) &&
    length(unique(plot_df$adjustment[!is.na(plot_df$adjustment)])) > 1L
  adjustment_stacked <- isTRUE(show_adjustment) && identical(adjustment_layout, "stack")
  if (adjustment_stacked) {
    show_ci <- FALSE
  }

  if (show_adjustment) {
    plot_df <- plot_df %>%
      dplyr::filter(!is.na(.data$adjustment)) %>%
      mutate(
        adjustment = factor(
          .data$adjustment,
          levels = c("Minimally adjusted", "Fully adjusted")
        ),
        adj_alpha = dplyr::case_when(
          .data$adjustment == "Minimally adjusted" ~ 1,
          .data$adjustment == "Fully adjusted" ~ 0.55,
          TRUE ~ NA_real_
        )
      )
  }

  reference_line_cols <- c(
    "characteristic_base", "labels_col", "col", "outcome_type", "labels_facet"
  )
  reference_lines <- plot_df %>%
    distinct(dplyr::across(dplyr::all_of(reference_line_cols))) %>%
    mutate(yintercept = 1)

  # Horizontal positioning: dodge side by side (default), or share a season column and
  # connect minimally vs fully adjusted estimates with a line (adjustment_layout = "stack").
  within_adj_jitter <- if (!is.null(level_jitter_width)) {
    level_jitter_width
  } else {
    jitter_width
  }
  within_adj_span_cap <- if (adjustment_stacked) 0.38 else 0.16

  if (show_adjustment && !adjustment_stacked) {
    plot_df <- plot_df %>%
      mutate(
        adj_offset = dplyr::if_else(
          .data$adjustment == "Minimally adjusted",
          -adjustment_dodge_width / 2,
          adjustment_dodge_width / 2
        )
      )

    if (isTRUE(use_discrete_season_axis)) {
      plot_df <- plot_df %>%
        group_by(labels_facet, season_x, outcome_type, adjustment) %>%
        arrange(as.integer(shape_key), .by_group = TRUE) %>%
        mutate(
          jitter_rank = dplyr::row_number(),
          jitter_n = dplyr::n(),
          span = pmin(
            within_adj_span_cap,
            within_adj_jitter * sqrt(pmax(jitter_n, 1L))
          ),
          step = dplyr::if_else(jitter_n > 1, span / (jitter_n - 1), 0),
          level_offset = (jitter_rank - (jitter_n + 1) / 2) * step,
          x_plot = season_x + adj_offset + level_offset
        ) %>%
        ungroup()
    } else {
      plot_df <- plot_df %>%
        group_by(labels_facet, year, outcome_type, adjustment) %>%
        arrange(as.integer(shape_key), .by_group = TRUE) %>%
        mutate(
          jitter_rank = dplyr::row_number(),
          jitter_n = dplyr::n(),
          span = pmin(
            within_adj_span_cap,
            within_adj_jitter * sqrt(pmax(jitter_n, 1L))
          ),
          step = dplyr::if_else(jitter_n > 1, span / (jitter_n - 1), 0),
          level_offset = (jitter_rank - (jitter_n + 1) / 2) * step,
          x_plot = year + adj_offset + level_offset
        ) %>%
        ungroup()
    }
  } else if (show_adjustment && adjustment_stacked) {
    if (isTRUE(use_discrete_season_axis)) {
      plot_df <- plot_df %>%
        group_by(labels_facet, season_x, outcome_type) %>%
        mutate(
          level_id = paste(
            as.character(.data$variable),
            as.character(.data$label),
            as.character(.data$codelist_type),
            sep = " | "
          ),
          jitter_rank = dplyr::dense_rank(.data$level_id),
          jitter_n = dplyr::n_distinct(.data$level_id),
          span = pmin(
            within_adj_span_cap,
            within_adj_jitter * sqrt(pmax(jitter_n, 1L))
          ),
          step = dplyr::if_else(jitter_n > 1, span / (jitter_n - 1), 0),
          level_offset = (jitter_rank - (jitter_n + 1) / 2) * step,
          x_plot = season_x + level_offset
        ) %>%
        ungroup()
    } else {
      plot_df <- plot_df %>%
        group_by(labels_facet, year, outcome_type) %>%
        mutate(
          level_id = paste(
            as.character(.data$variable),
            as.character(.data$label),
            as.character(.data$codelist_type),
            sep = " | "
          ),
          jitter_rank = dplyr::dense_rank(.data$level_id),
          jitter_n = dplyr::n_distinct(.data$level_id),
          span = pmin(
            within_adj_span_cap,
            within_adj_jitter * sqrt(pmax(jitter_n, 1L))
          ),
          step = dplyr::if_else(jitter_n > 1, span / (jitter_n - 1), 0),
          level_offset = (jitter_rank - (jitter_n + 1) / 2) * step,
          x_plot = year + level_offset
        ) %>%
        ungroup()
    }
  } else if (isTRUE(use_discrete_season_axis)) {
    plot_df <- plot_df %>%
      group_by(labels_facet, season_x, outcome_type) %>%
      arrange(as.integer(shape_key), .by_group = TRUE) %>%
      mutate(
        jitter_rank = dplyr::row_number(),
        jitter_n = dplyr::n(),
        span = pmin(0.9, jitter_width * sqrt(pmax(jitter_n, 1L))),
        step = dplyr::if_else(jitter_n > 1, span / (jitter_n - 1), 0),
        x_plot = season_x + (jitter_rank - (jitter_n + 1) / 2) * step
      ) %>%
      ungroup()
  } else {
    plot_df <- plot_df %>%
      group_by(labels_facet, year, outcome_type) %>%
      arrange(as.integer(shape_key), .by_group = TRUE) %>%
      mutate(
        jitter_rank = dplyr::row_number(),
        jitter_n = dplyr::n(),
        span = pmin(0.9, jitter_width * sqrt(pmax(jitter_n, 1L))),
        step = dplyr::if_else(jitter_n > 1, span / (jitter_n - 1), 0),
        x_plot = year + (jitter_rank - (jitter_n + 1) / 2) * step
      ) %>%
      ungroup()
  }

  if (adjustment_stacked) {
    season_key <- if (isTRUE(use_discrete_season_axis)) "season_x" else "year"
    plot_df <- plot_df %>%
      dplyr::arrange(
        .data$labels_facet,
        .data$outcome_type,
        .data[[season_key]],
        .data$adjustment
      ) %>%
      dplyr::mutate(
        connect_id = paste(
          as.character(.data$labels_facet),
          as.character(.data$outcome_type),
          as.character(.data$variable),
          as.character(.data$label),
          as.character(.data$codelist_type),
          as.character(.data$labels_col),
          .data[[season_key]],
          sprintf("%.4f", .data$x_plot),
          sep = " | "
        )
      )
  }

  y_vals_for_range <- c(plot_df$estimate, plot_df$conf.low, plot_df$conf.high)
  y_vals_for_range <- y_vals_for_range[is.finite(y_vals_for_range)]
  if (isTRUE(log_y)) {
    y_vals_for_range <- y_vals_for_range[y_vals_for_range > 0]
    if (length(y_vals_for_range) == 0) {
      y_vals_for_range <- c(0.1, 10)
    }
    shade_ymin <- max(min(y_vals_for_range), 1e-6)
    shade_ymax <- max(y_vals_for_range)
  } else {
    if (length(y_vals_for_range) == 0) {
      y_vals_for_range <- c(0.5, 1.5)
    }
    pad <- diff(range(y_vals_for_range)) * 0.05
    if (!is.finite(pad) || pad <= 0) {
      pad <- 0.1
    }
    shade_ymin <- min(y_vals_for_range) - pad
    shade_ymax <- max(y_vals_for_range) + pad
  }
  disruption_label <- "Disrupted routes of transmission"
  disruption_label_wrapped <- stringr::str_wrap(disruption_label, width = 18)
  shading_df <- tidyr::crossing(
    year_band = c(2020, 2021),
    labels_facet = levels(plot_df$labels_facet),
    outcome_type = if (isTRUE(facet_outcome)) levels(plot_df$outcome_type) else NA_character_
  ) %>%
    dplyr::filter(year_band %in% x_breaks) %>%
    mutate(
      ymin = shade_ymin,
      ymax = shade_ymax,
      disruption = disruption_label_wrapped
    )

  if (isTRUE(use_discrete_season_axis)) {
    shading_df <- shading_df %>%
      dplyr::left_join(
        season_axis %>% dplyr::rename(year_band = year),
        by = "year_band"
      ) %>%
      dplyr::filter(!is.na(.data$season_x)) %>%
      dplyr::mutate(
        xmin = .data$season_x - 0.5,
        xmax = .data$season_x + 0.5
      )
  } else {
    shading_df <- shading_df %>%
      dplyr::mutate(
        xmin = .data$year_band - 0.5,
        xmax = .data$year_band + 0.5
      )
  }

  base_plot <- ggplot(
    plot_df,
    aes(
      x = x_plot,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      group = series,
      color = labels_col,
      alpha = if (show_adjustment) adj_alpha else NULL
    )
  ) +
    geom_rect(
      data = shading_df,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = disruption
      ),
      inherit.aes = FALSE,
      alpha = 0.5
    ) +
    geom_hline(
      data = reference_lines,
      aes(yintercept = yintercept, color = labels_col),
      inherit.aes = FALSE,
      linetype = 2,
      linewidth = 0.4,
      alpha = 0.8
    ) +
    {
      if (adjustment_stacked) {
        geom_line(
          aes(group = connect_id),
          linewidth = 0.35,
          alpha = 0.75,
          na.rm = TRUE,
          show.legend = FALSE
        )
      }
    } +
    {
      if (isTRUE(show_ci)) {
        if (show_adjustment) {
          geom_pointrange(
            aes(shape = shape_key),
            linewidth = 0.35,
            na.rm = TRUE,
            position = position_identity()
          )
        } else {
          geom_pointrange(
            aes(shape = shape_key),
            alpha = 0.8,
            linewidth = 0.35,
            na.rm = TRUE,
            position = position_identity()
          )
        }
      } else if (show_adjustment) {
        geom_point(
          aes(shape = shape_key),
          size = if (adjustment_stacked) 2.0 else 1.8,
          na.rm = TRUE
        )
      } else {
        geom_point(
          aes(shape = shape_key),
          alpha = 0.85,
          size = if (adjustment_stacked) 2.0 else 1.8,
          na.rm = TRUE
        )
      }
    } +
    {
      if (isTRUE(use_discrete_season_axis)) {
        scale_x_continuous(
          breaks = season_axis$season_x,
          labels = season_axis$season_label,
          expand = expansion(mult = c(0.08, 0.08))
        )
      } else {
        scale_x_continuous(
          breaks = x_breaks,
          labels = paste0(x_breaks, "-", stringr::str_sub(as.character(x_breaks + 1), 3, 4)),
          expand = expansion(mult = c(0.08, 0.08))
        )
      }
    } +
    {
      if (isTRUE(log_y)) {
        scale_y_log10(
          breaks = log_rate_ratio_axis_breaks,
          minor_breaks = log_rate_ratio_axis_minor_breaks,
          labels = scales::label_number(accuracy = 0.1)
        )
      } else {
        scale_y_continuous(
          breaks = ratio_axis_breaks,
          labels = scales::label_number(accuracy = 0.01),
          expand = ggplot2::expansion(mult = c(0.06, 0.06))
        )
      }
    } +
    {
      if (isTRUE(log_y)) {
        annotation_logticks(base = 10, sides = "l")
      }
    } +
    scale_color_manual(values = colour_map, drop = FALSE) +
    {
      if (show_adjustment) {
        scale_alpha(
          range = c(0.55, 1),
          breaks = c(1, 0.55),
          labels = c("Minimally adjusted (base)", "Fully adjusted (further)"),
          name = "Adjustment",
          limits = c(0.55, 1)
        )
      }
    } +
    scale_fill_manual(
      values = setNames("grey85", disruption_label_wrapped),
      breaks = disruption_label_wrapped,
      drop = FALSE
    ) +
    scale_shape_manual(values = shape_map, labels = shape_labels, drop = FALSE) +
    labs(
      title = NULL,
      x = NULL,
      y = if (!is.null(y_lab)) {
        y_lab
      } else {
        paste(pathogen_title, "Rate Ratio")
      },
      color = "Characteristic",
      fill = NULL,
      shape = "Level"
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 6.5),
      axis.text.y = element_text(size = 6.5),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      strip.background = element_blank(),
      strip.text.y.left = element_blank(),
      strip.text.y.right = element_text(size = 6.25, face = "bold"),
      strip.text.x = element_blank(),
      panel.spacing.x = unit(if (identical(pathogen, "covid")) 8.3 else 0.18, "lines"),
      plot.margin = margin(5.5, if (identical(pathogen, "covid")) 1 else 4, 5.5, 2.5),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.key.width = unit(if (isTRUE(show_ci)) 1.4 else 2.2, "lines"),
      legend.key.height = unit(if (isTRUE(show_ci)) 1.4 else 2.2, "lines")
    )

  base_plot <- base_plot +
    guides(
      color = "none",
      fill = if (isTRUE(show_disruption_legend)) {
        guide_legend(
          ncol = 1,
          order = if (show_adjustment) 3 else 2,
          override.aes = list(alpha = 0.5)
        )
      } else {
        "none"
      },
      alpha = if (show_adjustment) {
        guide_legend(
          ncol = 1,
          order = 2,
          override.aes = list(shape = 16, size = if (adjustment_stacked) 2.0 else 1.4, colour = "black", fill = "black")
        )
      } else {
        "none"
      },
      shape = guide_legend(
        ncol = 1,
        order = 1,
        override.aes = list(
          size = if (adjustment_stacked || !isTRUE(show_ci)) 1.8 else 0.5,
          colour = shape_legend_cols,
          fill = shape_legend_cols,
          alpha = 1
        )
      )
    )

  facet_labeller <- labeller(labels_facet = label_wrap_gen(width = 14))

  # For test-model style: keep panel heights fixed, but allow y-ranges to vary.
  facet_scales <- "free_y"
  if (facet_outcome) {
    if (requireNamespace("ggh4x", quietly = TRUE)) {
      base_plot <- base_plot + ggh4x::facet_grid2(
        labels_facet ~ outcome_type,
        scales = facet_scales,
        space = "fixed",
        axes = "y",
        labeller = facet_labeller
      )
    } else {
      base_plot <- base_plot + facet_grid(
        labels_facet ~ outcome_type,
        scales = facet_scales,
        space = "fixed",
        labeller = facet_labeller
      )
    }
  } else {
    if (requireNamespace("ggh4x", quietly = TRUE)) {
      base_plot <- base_plot + ggh4x::facet_grid2(
        labels_facet ~ .,
        scales = facet_scales,
        space = "fixed",
        axes = "y",
        labeller = facet_labeller
      )
    } else {
      base_plot <- base_plot + facet_grid(
        labels_facet ~ .,
        scales = facet_scales,
        space = "fixed",
        labeller = facet_labeller
      )
    }
  }

  return(base_plot)
}

# Year-faceted forest-style plot:
# - years are facets (columns)
# - levels are on the x-axis
# - estimate is on the y-axis (log scale)
# - CI is encoded as point size (wider CI -> bigger point)
forest_year_facet_points_plot <- function(
  forest_data,
  pathogen,
  model_type,
  facet_outcome = TRUE
) {
  if (is.null(forest_data) || nrow(forest_data) == 0) {
    return(ggplot() + theme_void())
  }

  pathogen_title <- dplyr::case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19",
    pathogen == "overall_resp" ~ "Overall Respiratory Virus",
    TRUE ~ pathogen
  )
  
  cohort_val <- if (exists("cohort", envir = .GlobalEnv)) get("cohort", envir = .GlobalEnv) else NA_character_
  investigation_val <- if (exists("investigation_type", envir = .GlobalEnv)) get("investigation_type", envir = .GlobalEnv) else NA_character_
  
  # Avoid dplyr masking of scalar args by data columns of same name.
  model_type_arg <- model_type
  pathogen_arg <- pathogen
  
  get_level_order <- function(cohort_val, model_type, pathogen, investigation_val) {
    get_forest_level_order(
      cohort_val, model_type, pathogen, investigation_val, style = "year_mult"
    )
  }

  year_levels <- if (identical(pathogen, "covid")) {
    c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24")
  } else {
    c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24")
  }

  plot_df <- forest_data %>%
    filter(!is.na(subset), !is.na(estimate)) %>%
    mutate(
      subset = gsub("_", "-", subset),
      year_facet = factor(
        subset,
        levels = year_levels
      ),
      characteristic = dplyr::coalesce(as.character(labels), stringr::str_to_title(gsub("_", " ", variable))),
      characteristic_base = dplyr::case_when(
        characteristic == "Current Vaccination" ~ "Current Vaccination",
        characteristic == "Prior Vaccination" ~ "Prior Vaccination",
        TRUE ~ characteristic
      ),
      shape_type = factor(
        stringr::str_to_title(as.character(codelist_type)),
        levels = c("Reference", "Specific", "Sensitive")
      ),
      outcome_type = if ("outcome_type" %in% names(.)) outcome_type else NA_character_
    ) %>%
    mutate(
      outcome_type = factor(outcome_type, levels = c("Mild", "Severe")),
      level_y = {
        ord <- get_level_order(cohort_val, model_type_arg, pathogen_arg, investigation_val)
        extras <- setdiff(unique(label), ord)
        factor(label, levels = rev(c(ord, extras)))
      }
    )

  # Preserve the legend/group order coming from `forest()` (often already a factor).
  legend_order <- if (is.factor(forest_data$labels)) {
    levels(forest_data$labels)
  } else {
    plot_df %>% distinct(labels) %>% pull(labels) %>% as.character()
  }
  legend_order <- legend_order[!is.na(legend_order) & legend_order != ""]

  plot_df <- plot_df %>%
    mutate(labels = factor(as.character(labels), levels = legend_order))

  colour_map <- plot_df %>%
    filter(!is.na(labels), !is.na(col)) %>%
    distinct(labels, col) %>%
    arrange(factor(as.character(labels), levels = legend_order)) %>%
    deframe()

  reference_level_points <- plot_df %>%
    filter(shape_type == "Reference") %>%
    distinct(level_y, characteristic_base, outcome_type, year_facet, labels, estimate)

  # Add reference points for missing year facets (reference level only).
  ref_years <- levels(plot_df$year_facet)
  ref_missing <- plot_df %>%
    filter(shape_type == "Reference") %>%
    distinct(level_y, characteristic_base, outcome_type, labels) %>%
    tidyr::crossing(year_facet = factor(ref_years, levels = ref_years)) %>%
    left_join(
      reference_level_points %>%
        transmute(level_y, characteristic_base, outcome_type, labels, year_facet, has_estimate = TRUE),
      by = c("level_y", "characteristic_base", "outcome_type", "labels", "year_facet")
    ) %>%
    mutate(has_estimate = dplyr::coalesce(has_estimate, FALSE)) %>%
    filter(!has_estimate) %>%
    mutate(
      estimate = 1,
      shape_type = factor("Reference", levels = c("Reference", "Specific", "Sensitive"))
    )

  p <- ggplot(plot_df, aes(x = estimate, y = level_y, color = labels)) +
    geom_vline(xintercept = 1, linetype = 2, linewidth = 0.35, alpha = 0.8, color = "black") +
    geom_point(
      data = ref_missing,
      aes(x = estimate, y = level_y, color = labels),
      inherit.aes = FALSE,
      alpha = 0.75,
      #size = 1.25
    ) +
    ggplot2::geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, alpha = 0.55, na.rm = TRUE) +
    geom_point(aes(shape = shape_type), alpha = 0.85, #size = 1.25,
                   na.rm = TRUE) +
    geom_point(
      data = reference_level_points,
      aes(x = estimate, y = level_y, color = labels),
      inherit.aes = FALSE,
      alpha = 0.9,
      #size = 1.25
    ) +
    scale_x_log10(breaks = c(0.1, 0.3, 1, 3, 10), labels = scales::label_number(accuracy = 0.1)) +
    scale_color_manual(values = colour_map, breaks = legend_order, drop = FALSE) +
    scale_shape_manual(values = c(Reference = 16, Specific = 17, Sensitive = 15), guide = "none") +
    labs(x = paste(pathogen_title, "Rate Ratio", sep = " "), y = NULL, color = "Characteristic") +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x = element_text(size = 6.5),
      axis.text.y = element_text(size = 6.2),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      strip.text = element_text(size = 8),
      panel.spacing.x = unit(0.05, "lines"),
      panel.spacing.y = unit(0.18, "lines"),
      strip.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )

  if (facet_outcome) {
    # Columns: year facets, with Mild/Severe nested inside each year.
    year_levels <- levels(plot_df$year_facet)
    col_levels <- c(
      paste0("Mild|", year_levels),
      paste0("Severe|", year_levels)
    )
    plot_df <- plot_df %>%
      mutate(
        col_facet = factor(paste0(as.character(outcome_type), "|", as.character(year_facet)),
                           levels = col_levels)
      )

    p <- ggplot(plot_df, aes(x = estimate, y = level_y, color = labels)) +
      geom_vline(xintercept = 1, linetype = 2, linewidth = 0.35, alpha = 0.8, color = "black") +
      geom_point(
        data = ref_missing %>%
          mutate(
            col_facet = factor(paste0(as.character(outcome_type), "|", as.character(year_facet)),
                               levels = col_levels)
          ),
        aes(x = estimate, y = level_y, color = labels),
        inherit.aes = FALSE,
        alpha = 0.75,
        #size = 1.25
      ) +
      ggplot2::geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, alpha = 0.55, na.rm = TRUE) +
      geom_point(aes(shape = shape_type), alpha = 0.85, #size = 1.25,
                 na.rm = TRUE) +
      geom_point(
        data = reference_level_points %>%
          mutate(
            col_facet = factor(paste0(as.character(outcome_type), "|", as.character(year_facet)),
                               levels = col_levels)
          ),
        aes(x = estimate, y = level_y, color = labels),
        inherit.aes = FALSE,
        alpha = 0.9,
        #size = 1.25
      ) +
      scale_x_log10(breaks = c(0.1, 0.3, 1, 3, 10), labels = scales::label_number(accuracy = 0.1)) +
      scale_color_manual(values = colour_map, breaks = legend_order, drop = FALSE) +
      scale_shape_manual(values = c(Reference = 16, Specific = 17, Sensitive = 15), guide = "none") +
      labs(x = paste(pathogen_title, "Rate Ratio", sep = " "), y = NULL, color = "Characteristic") +
      facet_grid(
        characteristic_base ~ col_facet,
        scales = "free_y",
        space = "free_y",
        drop = FALSE,
        labeller = labeller(col_facet = function(x) sub(".*\\|", "", x))
      ) +
      theme_bw(base_size = 11) +
      theme(
        axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 6.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        strip.text = element_text(size = 8),
        panel.spacing.x = unit(0.05, "lines"),
        panel.spacing.y = unit(0.18, "lines"),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")
      )

  } else {
    p <- p + facet_grid(characteristic_base ~ year_facet, scales = "free_y", space = "free_y", drop = FALSE)
  }

  p
}

# Ratio of adjustment levels on the same layout as forest_over_time_plot_compare().
forest_over_time_plot_compare_ratio <- function(
  forest_data,
  pathogen,
  model_type,
  outcome_type = NULL,
  facet_outcome = FALSE,
  jitter_width = 0.2,
  show_ci = FALSE,
  fixed_axes = FALSE,
  show_disruption_legend = TRUE,
  years_include = NULL,
  season_axis_years = NULL,
  disruption_season_width_scale = 1,
  level_jitter_width = NULL,
  y_lab = "Ratio of RRs (further / minimal)",
  base_label = "Minimally adjusted",
  further_label = "Fully adjusted"
) {
  ratio_data <- collapse_base_further_to_ratio(
    forest_data,
    base_label = base_label,
    further_label = further_label
  )
  if (is.null(ratio_data) || nrow(ratio_data) == 0) {
    return(ggplot() + theme_void())
  }

  show_ci <- FALSE

  if (is.null(years_include)) {
    return(
      forest_over_time_plot_all_seasons(
        forest_data = ratio_data,
        pathogen = pathogen,
        model_type = model_type,
        outcome_type = outcome_type,
        facet_outcome = facet_outcome,
        jitter_width = jitter_width,
        show_ci = show_ci,
        fixed_axes = fixed_axes,
        show_disruption_legend = show_disruption_legend,
        y_lab = y_lab,
        log_y = FALSE
      )
    )
  }

  forest_over_time_plot_compare(
    forest_data = ratio_data,
    pathogen = pathogen,
    model_type = model_type,
    outcome_type = outcome_type,
    facet_outcome = facet_outcome,
    jitter_width = jitter_width,
    show_ci = show_ci,
    fixed_axes = fixed_axes,
    show_disruption_legend = show_disruption_legend,
    years_include = years_include,
    season_axis_years = season_axis_years,
    disruption_season_width_scale = disruption_season_width_scale,
    adjustment_dodge_width = 0,
    level_jitter_width = if (is.null(level_jitter_width)) 0.12 else level_jitter_width,
    y_lab = y_lab,
    log_y = FALSE
  )
}
