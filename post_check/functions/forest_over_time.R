forest_over_time_plot <- function(forest_data, pathogen, model_type, outcome_type) {
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

  title <- paste0(outcome_type, " ", pathogen_title, " by ", model_title)

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
    filter(!is.na(year)) %>%
    arrange(characteristic, series, year)

  level_summary <- plot_df %>%
    distinct(characteristic, label) %>%
    group_by(characteristic) %>%
    summarise(
      level_values = {
        vals <- sort(unique(label))
        if (length(vals) > 8) {
          vals <- c(vals[1:8], "...")
        }
        wrapped_vals <- stringr::str_wrap(vals, width = 16)
        paste(wrapped_vals, collapse = "\n")
      },
      .groups = "drop"
    )

  plot_df <- plot_df %>%
    left_join(level_summary, by = "characteristic") %>%
    mutate(characteristic_facet = level_values)

  # Use a fixed seasonal year axis so pathogen-specific rows (e.g., COVID-19)
  # align horizontally with RSV/Influenza rows even when early years are absent.
  year_breaks <- 2016:2023

  all_year_reference_points <- plot_df %>%
    distinct(
      series, label, labels, col, characteristic, characteristic_facet
    ) %>%
    tidyr::crossing(year = year_breaks) %>%
    mutate(
      estimate = 1,
      conf.low = 1,
      conf.high = 1,
      shape_type = factor("Reference", levels = c("Reference", "Specific", "Sensitive"))
    )
  
  colour_map <- plot_df %>%
    filter(!is.na(labels), !is.na(col)) %>%
    distinct(labels, col) %>%
    deframe()

  ggplot(
    plot_df,
    aes(
      x = year,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      group = series,
      color = labels
    )
  ) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_pointrange(
      aes(shape = shape_type),
      alpha = 0.8,
      linewidth = 0.25,
      fatten = 1.6,
      na.rm = TRUE,
      position = position_dodge(width = 0.25)
    ) +
    geom_pointrange(
      data = all_year_reference_points,
      aes(
        x = year,
        y = estimate,
        ymin = conf.low,
        ymax = conf.high,
        shape = shape_type,
        color = labels,
        group = series
      ),
      alpha = 0.8,
      linewidth = 0.25,
      fatten = 1.6,
      na.rm = TRUE,
      position = position_dodge(width = 0.25)
    ) +
    scale_x_continuous(
      breaks = year_breaks,
      labels = paste0(year_breaks, "-", stringr::str_sub(as.character(year_breaks + 1), 3, 4)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_log10(
      breaks = c(0.1, 0.3, 1, 3, 10),
      labels = scales::label_number(accuracy = 0.1),
      limits = c(0.1, 10)
    ) +
    scale_color_manual(values = colour_map, drop = FALSE) +
    facet_grid(characteristic_facet ~ ., scales = "fixed", space = "fixed") +
    labs(
      title = NULL,
      x = NULL,
      y = "Rate Ratio",
      color = "Characteristic",
      shape = "Estimate Type"
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
      axis.text.y = element_text(size = 6.5),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      strip.background = element_blank(),
      strip.text.y = element_text(
        angle = 0,
        hjust = 0,
        lineheight = 0.8,
        size = 6,
        margin = margin(l = 4, r = 4)
      ),
      panel.spacing.y = unit(0.18, "lines"),
      plot.margin = margin(5.5, 12, 5.5, 5.5),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8)
    )
}
