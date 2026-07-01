# Shared factor level order for forest / over-time plots.
# Age and IMD quintiles increase along the y-axis (youngest/least deprived at bottom).
# style = "year_mult" matches forest_year_further_mult() ordering (differs for sex, smoking labels, flu/covid).
FOREST_IMD_LEVELS <- c(
  "5 (least deprived)", "4", "3", "2", "1 (most deprived)"
)
FOREST_ETHNICITY_LEVELS <- c(
  "White",
  "Other Ethnic Groups",
  "Mixed",
  "Black or Black British",
  "Asian or Asian British",
  "Unknown"
)

# Rank labels against `get_forest_level_order()`; unseen labels follow in first-seen order.
forest_label_rank <- function(label, level_order) {
  label_chr <- as.character(label)
  ord <- unique(as.character(unlist(level_order)))
  ord <- ord[!is.na(ord) & ord != ""]
  rank <- match(label_chr, ord)
  unseen <- is.na(rank)
  if (any(unseen)) {
    extras <- label_chr[unseen]
    extras <- extras[!is.na(extras) & extras != ""]
    extras <- unique(extras)
    rank[unseen] <- match(label_chr[unseen], extras) + length(ord)
  }
  rank
}

# Condensed / over-time plots: shape legend and jitter follow `level_order`.
build_forest_shape_key_df <- function(plot_df, level_order, shape_pool) {
  plot_df %>%
    dplyr::mutate(
      .label_chr = as.character(.data$label),
      .label_rank = forest_label_rank(.data$label, level_order)
    ) %>%
    dplyr::group_by(.data$labels_col, .data$variable, .data$.label_chr, .data$.label_rank) %>%
    dplyr::summarise(
      is_ref = any(.data$is_ref_level),
      label = dplyr::first(.data$.label_chr),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$labels_col, .data$variable, .data$.label_rank) %>%
    dplyr::group_by(.data$labels_col, .data$variable) %>%
    dplyr::mutate(
      nonref_idx = cumsum(!.data$is_ref),
      shape_val = dplyr::if_else(
        .data$is_ref,
        16L,
        as.integer(shape_pool[pmax(.data$nonref_idx, 1L)])
      ),
      shape_key = paste0(
        as.character(.data$labels_col), " | ",
        as.character(.data$variable), " | ",
        .data$label
      )
    ) %>%
    dplyr::ungroup()
}

# Join on character labels to avoid factor-level mismatches.
join_forest_shape_keys <- function(plot_df, shape_key_df) {
  plot_df %>%
    dplyr::mutate(.label_chr = as.character(.data$label)) %>%
    dplyr::left_join(
      shape_key_df %>% dplyr::select("labels_col", "variable", "label", "shape_key"),
      by = c("labels_col", "variable", ".label_chr" = "label")
    ) %>%
    dplyr::mutate(shape_key = factor(.data$shape_key, levels = shape_key_df$shape_key))
}

# Horizontal offsets within a season column; order follows `shape_key`.
assign_forest_shape_jitter <- function(
  plot_df,
  group_cols,
  x_col,
  jitter_scale,
  span_cap,
  one_rank_per_shape = FALSE,
  dodge_offset_col = NULL
) {
  plot_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::arrange(as.integer(.data$shape_key), .by_group = TRUE) %>%
    dplyr::mutate(
      jitter_rank = if (isTRUE(one_rank_per_shape)) {
        dplyr::dense_rank(as.integer(.data$shape_key))
      } else {
        dplyr::row_number()
      },
      jitter_n = if (isTRUE(one_rank_per_shape)) {
        dplyr::n_distinct(as.integer(.data$shape_key))
      } else {
        dplyr::n()
      },
      span = pmin(span_cap, jitter_scale * sqrt(pmax(.data$jitter_n, 1L))),
      step = dplyr::if_else(.data$jitter_n > 1, .data$span / (.data$jitter_n - 1), 0),
      level_offset = (.data$jitter_rank - (.data$jitter_n + 1) / 2) * .data$step,
      x_plot = .data[[x_col]] +
        (if (!is.null(dodge_offset_col)) .data[[dodge_offset_col]] else 0) +
        .data$level_offset
    ) %>%
    dplyr::ungroup()
}

get_forest_level_order <- function(
  cohort_val,
  model_type,
  pathogen,
  investigation_val,
  style = c("default", "year_mult")
) {
  style <- match.arg(style)
  year_mult <- identical(style, "year_mult")
  levels <- list()

  if (cohort_val %in% c("infants", "infants_subgroup")) {
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Male", "Female")
  } else if (cohort_val == "children_and_adolescents") {
    age_levels <- c("2-5y", "6-9y", "10-13y", "14-17y")
    levels <- if (year_mult) {
      c(age_levels, "Female", "Male")
    } else {
      c(age_levels, "Male", "Female")
    }
  } else if (cohort_val == "adults") {
    age_levels <- c("18-39y", "40-64y")
    levels <- if (year_mult) {
      c(age_levels, "Female", "Male")
    } else {
      c(age_levels, "Male", "Female")
    }
  } else {
    levels <- c("65-74y", "75-89y", "90y+", "Male", "Female")
  }

  if (model_type == "ethnicity") {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Town and Fringe",
        "Rural Village and Dispersed", FOREST_ETHNICITY_LEVELS
      ),
      levels
    )
  } else if (model_type == "ses") {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", FOREST_IMD_LEVELS
      ),
      levels
    )
  } else if (model_type == "composition" &&
             !cohort_val %in% c("infants", "infants_subgroup")) {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "Three Other Generations",
        "Two Other Generations", "One Other Generation",
        "Living Alone", "Multiple of the Same Generation"
      ),
      levels
    )
  } else if (model_type == "ethnicity_ses") {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", FOREST_IMD_LEVELS,
        FOREST_ETHNICITY_LEVELS
      ),
      levels
    )
  } else if (model_type == "ethnicity_composition" &&
             !cohort_val %in% c("infants", "infants_subgroup")) {
    eth_comp <- c(
      "Urban Major Conurbation", "Urban Minor Conurbation",
      "Urban City and Town", "Rural Village and Dispersed",
      "Rural Town and Fringe", "Three Other Generations",
      "Two Other Generations", "One Other Generation",
      "Living Alone", "Multiple of the Same Generation",
      FOREST_ETHNICITY_LEVELS
    )
    levels <- c(eth_comp, levels)
  } else if (model_type == "ses_composition" &&
             !cohort_val %in% c("infants", "infants_subgroup")) {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "Three Other Generations",
        "Two Other Generations", "One Other Generation",
        "Living Alone", "Multiple of the Same Generation",
        FOREST_IMD_LEVELS
      ),
      levels
    )
  } else if (model_type == "full" &&
             !cohort_val %in% c("infants", "infants_subgroup")) {
    full_levels <- c(
      "Urban Major Conurbation", "Urban Minor Conurbation",
      "Urban City and Town", "Rural Village and Dispersed",
      "Rural Town and Fringe", "Three Other Generations",
      "Two Other Generations", "One Other Generation",
      "Living Alone", "Multiple of the Same Generation",
      FOREST_IMD_LEVELS,
      FOREST_ETHNICITY_LEVELS
    )
    levels <- c(full_levels, levels)
  }

  if (cohort_val == "infants_subgroup") {
    smoking <- if (year_mult) {
      c(
        "Current Smoker", "Former Smoker", "Never Smoker"
      )
    } else {
      c(
        "Maternal Current Smoking", "Maternal Former Smoking",
        "Maternal Never Smoking", "Maternal Unknown Smoking Status"
      )
    }
    levels <- c(
      "Maternal Pertussis Vaccination",
      "Maternal Flu Vaccination", "Maternal Drug Usage",
      "Maternal Drinking", "Binary Variables (Reference)",
      smoking,
      "Maternal Age", "Maternal Age (Average)", levels
    )
  } else if (
    cohort_val != "infants" && pathogen == "flu" &&
    (year_mult || investigation_val == "primary")
  ) {
    levels <- c(
      "Flu Vaccination (Yes)", "Flu Vaccination (No)",
      "Eligible and Vaccinated Last Autumn",
      "Not Vaccinated in Past Year", levels
    )
  } else if (
    cohort_val != "infants" && pathogen == "covid" &&
    (year_mult || investigation_val == "primary")
  ) {
    levels <- c(
      "Covid Vaccination (Yes)", "Covid Vaccination (No)",
      "Not Vaccinated in Past Year",
      "Eligible and Vaccinated Last Autumn",
      "Eligible and Vaccinated Last Spring", levels
    )
  } else if (!year_mult && investigation_val == "secondary") {
    levels <- c(
      "Drug Usage", "Hazardous Drinking", "Sickle Cell",
      "Immunosuppressed", "Cancer Within 3 Yrs", "CND", "CKD",
      "CLD", "CHD", "Severely Obese", "Addisons", "Diabetes",
      "Other Resp. Cond.", "Cystic Fibrosis", "COPD", "Asthma",
      "Binary Variables (Reference)", "Current", "Former",
      "Never", levels
    )
  }

  unique(as.character(unlist(levels)))
}
