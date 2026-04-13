get_level_order_forest <- function(cohort_val, model_type, pathogen, investigation_val) {
  levels <- list()
  
  if (cohort_val == "infants" | cohort_val == "infants_subgroup") {
    levels <- c("12-23m", "6-11m", "3-5m", "0-2m", "Male", "Female")
  } else if (cohort_val == "children_and_adolescents") {
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Male", "Female")
  } else if (cohort_val == "adults") {
    levels <- c("40-64y", "18-39y", "Male", "Female")
  } else {
    levels <- c("90y+", "75-89y", "65-74y", "Male", "Female")
  }
  
  if (model_type == "ethnicity") {
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed", "Other Ethnic Groups",
                  "Black or Black British", "Asian or Asian British",
                  "Mixed", "White"),
                levels)
  } else if (model_type == "ses") {
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "1 (most deprived)",
                  "2", "3", "4", "5 (least deprived)"), levels)
  } else if (model_type == "composition" & cohort_val != "infants" &
             cohort_val != "infants_subgroup") {
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation"), levels)
  } else if (model_type == "ethnicity_ses") {
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "1 (most deprived)",
                  "2", "3", "4", "5 (least deprived)",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
  } else if (model_type == "ethnicity_composition" & cohort_val != "infants" &
             cohort_val != "infants_subgroup") {
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
  } else if (model_type == "ses_composition" & cohort_val != "infants" &
             cohort_val != "infants_subgroup") {
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "1 (most deprived)", "2", "3", "4",
                  "5 (least deprived)"), levels)
  } else if (model_type == "full" & cohort_val != "infants" &
             cohort_val != "infants_subgroup") {
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
  }
  
  if (cohort_val == "infants_subgroup") {
    levels <- c("Maternal Pertussis Vaccination",
                "Maternal Flu Vaccination", "Maternal Drug Usage",
                "Maternal Drinking", "Binary Variables (Reference)",
                "Maternal Current Smoking", "Maternal Former Smoking",
                "Maternal Never Smoking", "Maternal Age",
                "Maternal Age (Average)", levels)
  } else if (cohort_val != "infants" & pathogen == "flu" & investigation_val == "primary") {
    levels <- c("Flu Vaccination (Yes)", "Flu Vaccination (No)",
                "Eligible and Vaccinated Last Autumn",
                "Not Vaccinated in Past Year", levels)
  } else if (cohort_val != "infants" & pathogen == "covid" & investigation_val == "primary") {
    levels <- c("Covid Vaccination (Yes)", "Covid Vaccination (No)",
                "Not Vaccinated in Past Year",
                "Eligible and Vaccinated Last Autumn",
                "Eligible and Vaccinated Last Spring", levels)
  } else if (investigation_val == "secondary") {
    levels <- c("Drug Usage", "Hazardous Drinking", "Sickle Cell",
                "Immunosuppressed", "Cancer Within 3 Yrs", "CND", "CKD",
                "CLD", "CHD", "Severely Obese", "Addisons", "Diabetes",
                "Other Resp. Cond.", "Cystic Fibrosis", "COPD", "Asthma",
                "Binary Variables (Reference)", "Current", "Former",
                "Never", levels)
  }
  
  levels
}

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
  show_disruption_legend = TRUE
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
  }

  # Ensure every reference level has an RR=1 point in every plotted season.
  # RSV/Flu: all x-axis seasons shown; COVID: 2019-20 onwards (x_breaks already reflects this).
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

    ref_grid <- ref_template %>%
      tidyr::crossing(year = x_breaks) %>%
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
  level_order <- get_level_order_forest(cohort_val, model_type, pathogen, investigation_val)
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

  # Shading band y-range must be finite and >0 for log scale.
  y_vals_for_range <- c(plot_df$estimate, plot_df$conf.low, plot_df$conf.high)
  y_vals_for_range <- y_vals_for_range[is.finite(y_vals_for_range) & y_vals_for_range > 0]
  if (length(y_vals_for_range) == 0) y_vals_for_range <- c(0.1, 10)
  shade_ymin <- max(min(y_vals_for_range), 1e-6)
  shade_ymax <- max(y_vals_for_range)
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
      if (isTRUE(fixed_axes)) {
        # Exactly 3 breaks per facet on a log scale (nice powers-of-10 style ticks).
        scale_y_log10(
          breaks = scales::log_breaks(n = 3),
          labels = scales::label_number(accuracy = 0.1)
        )
      } else {
        scale_y_log10(
          breaks = function(x) {
            rng <- range(x, finite = TRUE, na.rm = TRUE)
            if (!is.finite(rng[1]) || !is.finite(rng[2])) return(numeric(0))
            if (rng[1] <= 0) {
              pos <- x[is.finite(x) & x > 0]
              if (length(pos) == 0) return(numeric(0))
              rng[1] <- min(pos)
            }
            if (rng[1] == rng[2]) return(rng[1])
            seq(rng[1], rng[2], length.out = 3)
          },
          labels = scales::label_number(accuracy = 0.1)
        )
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
      y = paste(pathogen_title, "Rate Ratio"),
      color = "Characteristic",
      fill = NULL,
      shape = "Level"
    ) +
    theme_bw(base_size = 11) +
    theme(
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
      legend.key.width = unit(1.4, "lines")
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
        override.aes = list(size = 0.5, colour = shape_legend_cols, fill = shape_legend_cols)
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
  
  get_level_order <- get_level_order_forest

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
      size = 1.25
    ) +
    ggplot2::geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, alpha = 0.55, na.rm = TRUE) +
    geom_point(aes(shape = shape_type), alpha = 0.85, size = 1.25, na.rm = TRUE) +
    geom_point(
      data = reference_level_points,
      aes(x = estimate, y = level_y, color = labels),
      inherit.aes = FALSE,
      alpha = 0.9,
      size = 1.25
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
        size = 1.25
      ) +
      ggplot2::geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, alpha = 0.55, na.rm = TRUE) +
      geom_point(aes(shape = shape_type), alpha = 0.85, size = 1.25, na.rm = TRUE) +
      geom_point(
        data = reference_level_points %>%
          mutate(
            col_facet = factor(paste0(as.character(outcome_type), "|", as.character(year_facet)),
                               levels = col_levels)
          ),
        aes(x = estimate, y = level_y, color = labels),
        inherit.aes = FALSE,
        alpha = 0.9,
        size = 1.25
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
