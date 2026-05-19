# Shared helpers for multi-virus condensed forest figures.

load_collated_further <- function(cohort, pathogen) {
  raw <- readr::read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
  if (exists("df_few", envir = .GlobalEnv, inherits = FALSE)) {
    assign(
      "df_few",
      dplyr::bind_rows(
        get("df_few", envir = .GlobalEnv),
        raw %>% dplyr::filter(.data$term == "too few events")
      ),
      envir = .GlobalEnv
    )
  }
  raw %>% dplyr::filter(.data$term != "too few events")
}

load_collated_base <- function(cohort, pathogen) {
  readr::read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
}

load_dummy_inputs <- function(cohort, pathogen) {
  if (identical(pathogen, "covid")) {
    arrow::read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow")
      )
    ) %>%
      dplyr::mutate(
        covid_vaccination_immunity_date = if (
          !"covid_vaccination_immunity_date" %in% names(.)
        ) {
          NA
        } else {
          .data$covid_vaccination_immunity_date
        },
        time_since_last_covid_vaccination = if (
          !"time_since_last_covid_vaccination" %in% names(.)
        ) {
          NA_character_
        } else {
          .data$time_since_last_covid_vaccination
        }
      ) %>%
      dplyr::mutate(
        subset = "2021_22",
        time_since_last_covid_vaccination = dplyr::if_else(
          is.na(.data$covid_vaccination_immunity_date),
          "6-12m",
          .data$time_since_last_covid_vaccination
        )
      )
  } else {
    arrow::read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow")
      )
    )
  }
}

# Cowplot layout tuned for A4 mild | severe columns (rel_widths negative = spacer grobs).
assemble_condensed_figure <- function(
    rsv_plot,
    flu_plot,
    covid_plot,
    legend_left,
    legend_mid = NULL
) {
  covid_body <- covid_plot
  if (!is.null(legend_mid)) {
    covid_body <- cowplot::ggdraw(covid_body) +
      cowplot::draw_grob(legend_mid, x = 0.44, y = 0.08, width = 0.12, height = 0.84)
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

  legend_left <- cowplot::get_legend(
    forest_over_time_plot_compare(
      legend_dat %>% dplyr::slice(1),
      pathogen = legend_pathogen,
      model_type = "ethnicity_ses",
      facet_outcome = TRUE,
      show_disruption_legend = FALSE,
      years_include = years_include
    ) +
      ggplot2::theme(legend.position = "left") +
      ggplot2::guides(shape = "none", fill = "none", color = "none")
  )

  legend_mid <- if (length(groups_mid) > 0) {
    cowplot::get_legend(
      forest_over_time_plot_compare(
        legend_dat %>% dplyr::filter(as.character(.data$labels) %in% groups_mid),
        pathogen = legend_pathogen,
        model_type = "ethnicity_ses",
        facet_outcome = TRUE,
        show_disruption_legend = FALSE,
        years_include = years_include
      ) +
        ggplot2::theme(legend.position = "left", legend.title = element_blank()) +
        ggplot2::guides(fill = "none", alpha = "none")
    )
  } else {
    NULL
  }

  list(left = legend_left, mid = legend_mid)
}

build_shared_legends_key_vars <- function(legend_dat, model_type, legend_pathogen = "covid") {
  groups_left <- intersect("Age Group", unique(as.character(legend_dat$labels)))
  groups_mid <- intersect(
    c("Ethnicity", "IMD Quintile"),
    unique(as.character(legend_dat$labels))
  )

  legend_left <- if (length(groups_left) > 0) {
    cowplot::get_legend({
      forest_over_time_plot(
        legend_dat %>% dplyr::filter(as.character(.data$labels) %in% groups_left),
        pathogen = legend_pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        show_disruption_legend = TRUE
      ) +
        ggplot2::theme(legend.position = "left", legend.title = element_blank())
    })
  } else {
    NULL
  }

  legend_mid <- if (length(groups_mid) > 0) {
    cowplot::get_legend({
      forest_over_time_plot(
        legend_dat %>% dplyr::filter(as.character(.data$labels) %in% groups_mid),
        pathogen = legend_pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        show_disruption_legend = FALSE
      ) +
        ggplot2::theme(legend.position = "left", legend.title = element_blank())
    })
  } else {
    NULL
  }

  list(left = legend_left, mid = legend_mid)
}
