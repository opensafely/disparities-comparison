library(tidyverse)
library(here)
library(ggplot2)
library(cowplot)
library(scales)
library(RColorBrewer)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
options(scipen = 999)

#create function to import data
import <- function(pathogen, cohort) {
  
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  df <- read_csv(here::here(
    "post_check", "output", "collated", "descriptive", "over_time",
    paste0(cohort, "_", "counts_over_time_all_monthly_", pathogen, ".csv"))) %>%
    mutate(virus = pathogen_title) %>%
    arrange(month) %>%
    mutate(
      event = case_when(
        str_detect(event, "primary") ~ "Mild",
        str_detect(event, "secondary") ~ "Severe"
      )
    ) %>%
    select(c(month, event,
             cohort_events = n,
             codelist_type, virus)) %>%
    filter(month >= "2016-09-01") %>%
    tidyr::complete(
      month = seq(ymd("2016-09-01"), ymd("2024-08-31"), by = "1 month"),
      event,
      codelist_type,
      virus,
      fill = list(cohort_events = 0)
    ) %>% 
    mutate(
      subset = case_when(
        month >= ymd("2016-09-01") & month <= ymd("2017-09-01") ~ "2016-17",
        month >= ymd("2017-09-01") & month <= ymd("2018-09-01") ~ "2017-18",
        month >= ymd("2018-09-01") & month <= ymd("2019-09-01") ~ "2018-19",
        month >= ymd("2019-09-01") & month <= ymd("2020-09-01") ~ "2019-20",
        month >= ymd("2020-09-01") & month <= ymd("2021-09-01") ~ "2020-21",
        month >= ymd("2021-09-01") & month <= ymd("2022-09-01") ~ "2021-22",
        month >= ymd("2022-09-01") & month <= ymd("2023-09-01") ~ "2022-23",
        month >= ymd("2023-09-01") & month <= ymd("2024-09-01") ~ "2023-24",
      ),
      cohort = cohort
    )
  
  return(df)
  
}

#import data
df_rsv_older_adults <- import("rsv", "older_adults")
df_rsv_adults <- import("rsv", "adults")
df_rsv_children_and_adolescents <- import("rsv", "children_and_adolescents")
df_rsv_infants <- import("rsv", "infants")
df_rsv <- bind_rows(
  df_rsv_older_adults,
  df_rsv_adults,
  df_rsv_children_and_adolescents,
  df_rsv_infants
)

#import data
df_flu_older_adults <- import("flu", "older_adults")
df_flu_adults <- import("flu", "adults")
df_flu_children_and_adolescents <- import("flu", "children_and_adolescents")
df_flu_infants <- import("flu", "infants")
df_flu <- bind_rows(
  df_flu_older_adults,
  df_flu_adults,
  df_flu_children_and_adolescents,
  df_flu_infants
)

#import data
df_covid_older_adults <- import("covid", "older_adults")
df_covid_adults <- import("covid", "adults")
df_covid_children_and_adolescents <- import("covid", "children_and_adolescents")
df_covid_infants <- import("covid", "infants")
df_covid <- bind_rows(
  df_covid_older_adults,
  df_covid_adults,
  df_covid_children_and_adolescents,
  df_covid_infants
)

#combine all viruses
df_check <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>%
  filter(codelist_type == "specific") %>% 
  select(event, virus, cohort, cohort_events) %>%
  group_by(across(c(event, virus))) %>% 
  mutate(
    total_events = sum(cohort_events)
  ) %>% 
  group_by(across(c(event, virus, cohort))) %>% 
  mutate(
    perc_burden = (sum(cohort_events)/total_events)*100,
    cohort = factor(cohort, levels = c("older_adults", "adults", "children_and_adolescents", "infants"),
                    labels = c("Older Adults", "Adults", "Children and Young People", "Infants"),
                    ordered = T)
  ) %>% 
  ungroup() %>%
  select(-c(cohort_events, total_events)) %>%
  unique()
df_all <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>%
  filter(codelist_type == "specific") %>% 
  group_by(across(c(subset, month, event, virus))) %>% 
  mutate(
    total_events = sum(cohort_events)
  ) %>% 
  group_by(across(c(subset, month, event, virus, cohort))) %>% 
  mutate(
    perc_burden = cohort_events/total_events,
    cohort = factor(cohort, levels = c("older_adults", "adults", "children_and_adolescents", "infants"),
                    labels = c("Older Adults", "Adults", "Children and Young People", "Infants"),
                    ordered = T)
  ) %>% 
  ungroup() %>% 
  select(
    subset, month, event, virus, total_events, cohort, cohort_events, perc_burden
  )

cohort_colours <- setNames(
  RColorBrewer::brewer.pal(4, "Set2"),
  c("Older Adults", "Adults", "Children and Young People", "Infants")
)
cohort_levels <- names(cohort_colours)
Y_AXIS_EXPAND <- 0.1    # extra y-axis space above the data
Y_LABEL_SPACING <- 5     # multiplier on gap between labels (>1 = further apart)

load_rates_season_all <- function() {
  cohort_ids <- c("older_adults", "adults", "children_and_adolescents", "infants")

  purrr::map_dfr(cohort_ids, function(cohort) {
    readr::read_csv(
      here::here(
        "post_check", "output", "collated", "descriptive",
        paste0(cohort, "_rates_primary_collated.csv")
      ),
      show_col_types = FALSE
    ) %>%
      dplyr::filter(
        .data$Characteristic == "Total",
        .data$Group == "All",
        .data$investigation_type == "primary"
      ) %>%
      dplyr::mutate(
        cohort = factor(
          cohort,
          levels = cohort_ids,
          labels = cohort_levels,
          ordered = TRUE
        ),
        virus = dplyr::case_when(
          stringr::str_detect(.data$Outcome, "^RSV") ~ "RSV",
          stringr::str_detect(.data$Outcome, "^Flu") ~ "Influenza",
          stringr::str_detect(.data$Outcome, "^COVID") ~ "COVID-19",
          TRUE ~ NA_character_
        ),
        event = dplyr::if_else(
          stringr::str_detect(.data$Outcome, "Mild"),
          "A. Mild",
          "B. Severe"
        ),
        subset = stringr::str_replace(.data$subset, "_", "-")
      ) %>%
      dplyr::mutate(
        month = lubridate::ymd(
          paste0(
            as.integer(stringr::str_sub(.data$subset, 1, 4)) + 1L,
            "-02-01"
          )
        ),
        rate_1000_py_midpoint10_derived = .data$Rate_Midpoint10_Derived
      ) %>%
      dplyr::filter(!is.na(.data$virus))
  })
}

rates_season_all <- load_rates_season_all()

build_burden_labels <- function(
  virus_name,
  plot_data,
  y_axis_expand = Y_AXIS_EXPAND,
  label_spacing = Y_LABEL_SPACING
) {
  event_levels <- c("A. Mild", "B. Severe")
  event_labels <- c("Mild" = "A. Mild", "Severe" = "B. Severe")
  n_labels <- length(cohort_levels)

  virus_plot_data <- plot_data %>%
    filter(.data$virus == virus_name)

  facet_stats <- virus_plot_data %>%
    group_by(event, month) %>%
    summarise(stack_total = sum(cohort_events), .groups = "drop") %>%
    group_by(event) %>%
    summarise(
      facet_ymax = max(.data$stack_total),
      month = max(.data$month[.data$stack_total > 0], na.rm = TRUE),
      .groups = "drop"
    )

  df_check %>%
    filter(.data$virus == virus_name) %>%
    mutate(
      event = factor(
        event_labels[.data$event],
        levels = event_levels
      ),
      cohort = factor(.data$cohort, levels = cohort_levels),
      label = sprintf("%.1f%%", .data$perc_burden)
    ) %>%
    inner_join(facet_stats, by = "event") %>%
    arrange(event, cohort) %>%
    mutate(
      virus = factor(virus_name, levels = levels(plot_data$virus)),
      rank = as.integer(.data$cohort),
      panel_ymax = .data$facet_ymax * (1 + .env$y_axis_expand),
      label_margin = .data$facet_ymax * .env$y_axis_expand,
      label_gap = (.data$label_margin / (n_labels - 1)) * .env$label_spacing,
      y_anchor = .data$panel_ymax - (rank - 1) * label_gap
    )
}

build_burden_labels_all <- function(
  plot_data,
  y_axis_expand = Y_AXIS_EXPAND,
  label_spacing = Y_LABEL_SPACING
) {
  purrr::map_dfr(
    levels(plot_data$virus),
    function(virus_name) {
      build_burden_labels(
        virus_name,
        plot_data,
        y_axis_expand = y_axis_expand,
        label_spacing = label_spacing
      )
    }
  )
}

stacked <- function(
    area = FALSE,
    show_legend = FALSE,
    rate_cohorts_overlay = NULL
) {

  si_labels <- function(digits, width) {
    f <- label_number(accuracy = 10^(-digits), scale_cut = cut_si(""))
    function(x) sprintf(paste0("%", width, "s"), f(x))
  }

  x_scale <- scale_x_date(
    limits = c(ymd("2016-01-01"), ymd("2025-06-01")),
    breaks = seq(ymd("2016-01-01"), ymd("2025-01-01"), by = "1 year"),
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0.04))
  )

  plot_data <- df_all %>%
    mutate(
      virus = factor(
        virus,
        levels = c("RSV", "Influenza", "COVID-19")
      ),
      event = factor(
        event,
        levels = c("Mild", "Severe"),
        labels = c("A. Mild", "B. Severe")
      )
    )

  label_data <- build_burden_labels_all(plot_data)

  rate_overlay_data <- NULL
  rate_to_count_scale <- NULL
  if (!is.null(rate_cohorts_overlay)) {
    facet_count_max <- plot_data %>%
      group_by(virus, event, month) %>%
      summarise(stack_total = sum(cohort_events), .groups = "drop") %>%
      group_by(virus, event) %>%
      summarise(max_count = max(stack_total), .groups = "drop")

    rate_overlay_data <- rates_season_all %>%
      filter(
        cohort %in% rate_cohorts_overlay,
        codelist_type == "specific"
      ) %>%
      mutate(
        virus = factor(virus, levels = c("RSV", "Influenza", "COVID-19")),
        event = factor(event, levels = c("A. Mild", "B. Severe"))
      ) %>%
      left_join(facet_count_max, by = c("virus", "event")) %>%
      group_by(virus, event) %>%
      mutate(
        max_rate = max(rate_1000_py_midpoint10_derived, na.rm = TRUE),
        rate_to_count_scale = if_else(
          is.finite(max_rate) & max_rate > 0,
          max_count / max_rate,
          1
        )
      ) %>%
      ungroup() %>%
      mutate(
        rate_y = rate_1000_py_midpoint10_derived * rate_to_count_scale
      )

    rate_to_count_scale <- rate_overlay_data %>%
      group_by(virus, event) %>%
      summarise(rate_to_count_scale = dplyr::first(rate_to_count_scale), .groups = "drop") %>%
      summarise(scale_ref = median(rate_to_count_scale, na.rm = TRUE)) %>%
      pull(scale_ref)

    if (!is.finite(rate_to_count_scale) || rate_to_count_scale <= 0) {
      rate_to_count_scale <- 1
    }
  }

  y_scale <- if (is.null(rate_cohorts_overlay)) {
    scale_y_continuous(
      labels = si_labels(digits = 1, width = 8),
      expand = expansion(mult = c(0, Y_AXIS_EXPAND))
    )
  } else {
    scale_y_continuous(
      labels = si_labels(digits = 1, width = 8),
      expand = expansion(mult = c(0, Y_AXIS_EXPAND)),
      sec.axis = sec_axis(
        transform = ~ . / rate_to_count_scale,
        name = "Rate per 1000 person-years"
      )
    )
  }

  p <- plot_data %>%
    ggplot(aes(x = month, y = cohort_events, fill = cohort, group = cohort)) +
    ggh4x::facet_grid2(
      virus ~ event,
      scales = "free_y",
      independent = "y",
      space = "fixed"
    ) +
    theme_bw() +
    x_scale +
    y_scale +
    labs(x = "", y = "Monthly Cases", fill = "Cohort") +
    scale_fill_manual(values = cohort_colours) +
    theme(
      strip.text.x = element_blank(),
      strip.text.y = element_text(face = "bold"),
      strip.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = if (show_legend) "top" else "none",
      plot.margin = margin(10, 22, 5.5, 5.5)
    )

  if (area) {
    p <- p + geom_area(stat = "identity", position = "stack")
  } else {
    p <- p + geom_bar(stat = "identity", position = "stack")
  }

  if (is.null(rate_cohorts_overlay)) {
    p <- p + scale_color_manual(values = cohort_colours, guide = "none")
  } else {
    p <- p +
      geom_line(
        data = rate_overlay_data,
        aes(
          x = month,
          y = rate_y,
          colour = cohort,
          group = cohort
        ),
        inherit.aes = FALSE,
        linewidth = 0.55,
        alpha = 0.5
      ) +
      geom_point(
        data = rate_overlay_data,
        aes(x = month, y = rate_y, colour = cohort),
        inherit.aes = FALSE,
        size = 1.8,
        alpha = 0.95
      ) +
      scale_colour_manual(
        values = cohort_colours,
        breaks = rate_cohorts_overlay,
        name = "Rate (cohort)"
      ) +
      guides(
        fill = guide_legend(order = 1),
        colour = guide_legend(order = 2)
      )
  }

  p + geom_text(
    data = label_data,
    aes(x = month, y = y_anchor, label = label, color = cohort),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE
  )

}

assemble_cohort_burden_figure <- function(plot_body, legend_plot) {
  legend <- cowplot::get_legend(
    legend_plot +
      theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "center"
      )
  )

  combined <- cowplot::plot_grid(
    legend,
    NULL,
    plot_body,
    ncol = 1,
    rel_heights = c(0.07, 0.02, 1)
  )

  cowplot::ggdraw(combined) +
    cowplot::draw_label(
      "A. Mild", x = 0.28, y = 0.925, hjust = 0.5, vjust = 0.5,
      fontface = "bold", size = 10
    ) +
    cowplot::draw_label(
      "B. Severe", x = 0.75, y = 0.925, hjust = 0.5, vjust = 0.5,
      fontface = "bold", size = 10
    )
}

area <- assemble_cohort_burden_figure(
  stacked(area = TRUE),
  stacked(area = TRUE, show_legend = TRUE)
)

bar <- assemble_cohort_burden_figure(
  stacked(),
  stacked(show_legend = TRUE)
)

out_dir <- here::here("post_check", "plots", "primary_analyses")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  here::here(out_dir, "cohort_burden_over_time_area.png"),
  area,
  width = 10,
  height = 6
)
ggsave(
  here::here(out_dir, "cohort_burden_over_time_bar.png"),
  bar,
  width = 10,
  height = 6
)

bar_adults_rates <- assemble_cohort_burden_figure(
  stacked(rate_cohorts_overlay = c("Older Adults", "Adults")),
  stacked(
    rate_cohorts_overlay = c("Older Adults", "Adults"),
    show_legend = TRUE
  )
)

bar_children_rates <- assemble_cohort_burden_figure(
  stacked(rate_cohorts_overlay = c("Infants", "Children and Young People")),
  stacked(
    rate_cohorts_overlay = c("Infants", "Children and Young People"),
    show_legend = TRUE
  )
)

ggsave(
  here::here(out_dir, "cohort_burden_over_time_overlay_rates_adults_older_adults.png"),
  bar_adults_rates,
  width = 10,
  height = 6
)
ggsave(
  here::here(out_dir, "cohort_burden_over_time_overlay_rates_infants_children.png"),
  bar_children_rates,
  width = 10,
  height = 6
)

## rates per 1000 person years in each cohort over time

source(here::here("post_check", "functions", "forest_over_time.R"))

CONDENSED_RATES_FIG_WIDTH <- 8.5
CONDENSED_RATES_FIG_HEIGHT <- 8.5

PATHOGEN_RATE_COLOURS <- setNames(
  scales::seq_gradient_pal("#F05039", "#1F449c", "Lab")(c(2 / 8, 3 / 8, 5 / 8)),
  c("rsv", "flu", "covid")
)

pathogen_title_from_key <- function(pathogen) {
  dplyr::case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19",
    TRUE ~ pathogen
  )
}

load_cohort_total_rates <- function(cohort) {
  readr::read_csv(
    here::here(
      "post_check", "output", "collated", "descriptive",
      paste0(cohort, "_rates_primary_collated.csv")
    ),
    show_col_types = FALSE
  ) %>%
    dplyr::filter(
      .data$Characteristic == "Total",
      .data$Group == "All",
      .data$investigation_type == "primary"
    ) %>%
    dplyr::mutate(
      pathogen = dplyr::case_when(
        stringr::str_detect(.data$Outcome, "^RSV") ~ "rsv",
        stringr::str_detect(.data$Outcome, "^Flu") ~ "flu",
        stringr::str_detect(.data$Outcome, "^COVID") ~ "covid",
        TRUE ~ NA_character_
      ),
      outcome_type = dplyr::if_else(
        stringr::str_detect(.data$Outcome, "Mild"),
        "Mild",
        "Severe"
      ),
      year = as.integer(stringr::str_extract(.data$subset, "^[0-9]{4}")),
      season_label = stringr::str_replace(.data$subset, "_", "-")
    ) %>%
    dplyr::filter(!is.na(.data$pathogen), !is.na(.data$year))
}

season_x_breaks <- function(pathogen) {
  if (identical(pathogen, "covid")) {
    2019:2023
  } else {
    2016:2023
  }
}

prepare_total_rates_plot_data <- function(rates_df, pathogen, codelist_type) {
  rates_df %>%
    dplyr::filter(
      .data$pathogen == !!pathogen,
      .data$codelist_type == !!codelist_type
    ) %>%
    dplyr::filter(.data$year %in% season_x_breaks(pathogen)) %>%
    dplyr::mutate(
      outcome_type = factor(.data$outcome_type, levels = c("Mild", "Severe")),
      season_label = factor(
        .data$season_label,
        levels = paste0(
          season_x_breaks(pathogen), "-",
          stringr::str_sub(as.character(season_x_breaks(pathogen) + 1L), 3, 4)
        )
      )
    )
}

total_rates_season_panel <- function(plot_data, pathogen) {
  if (nrow(plot_data) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  pathogen_title <- pathogen_title_from_key(pathogen)
  x_breaks <- season_x_breaks(pathogen)
  line_colour <- unname(PATHOGEN_RATE_COLOURS[[pathogen]])

  p <- plot_data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$year,
        y = .data$Rate_Midpoint10_Derived,
        group = .data$outcome_type
      )
    ) +
    ggplot2::geom_line(colour = line_colour, linewidth = 0.45) +
    ggplot2::geom_point(colour = line_colour, size = FOREST_POINT_SIZE) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = paste0(
        x_breaks, "-",
        stringr::str_sub(as.character(x_breaks + 1L), 3, 4)
      ),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0.06, 0.06))
    ) +
    ggplot2::labs(
      x = NULL,
      y = paste0(pathogen_title, "\nRate per 1000 person-years")
    ) +
    ggplot2::theme_bw(base_size = FOREST_BASE_SIZE) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = FOREST_AXIS_TEXT_X_SIZE),
      axis.text.y = ggplot2::element_text(size = FOREST_AXIS_TEXT_Y_SIZE),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black"),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(
        if (identical(pathogen, "covid")) 8.3 else 0.18,
        "lines"
      ),
      plot.margin = ggplot2::margin(
        5.5, if (identical(pathogen, "covid")) 1 else 4, 5.5, 2.5
      )
    )

  if (requireNamespace("ggh4x", quietly = TRUE)) {
    p <- p + ggh4x::facet_grid2(
      . ~ outcome_type,
      scales = "free_y",
      space = "fixed",
      axes = "y"
    )
  } else {
    p <- p + ggplot2::facet_grid(. ~ outcome_type, scales = "free_y", space = "fixed")
  }

  p
}

assemble_condensed_total_rates_figure <- function(rsv_plot, flu_plot, covid_plot) {
  combined <- cowplot::plot_grid(
    NULL,
    rsv_plot,
    NULL,
    flu_plot,
    NULL,
    covid_plot,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(0.05, 1, -0.02, 1.25, -0.02, 1.35)
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

make_condensed_total_rates_figure <- function(rates_df, codelist_type) {
  assemble_condensed_total_rates_figure(
    total_rates_season_panel(
      prepare_total_rates_plot_data(rates_df, "rsv", codelist_type),
      "rsv"
    ),
    total_rates_season_panel(
      prepare_total_rates_plot_data(rates_df, "flu", codelist_type),
      "flu"
    ),
    total_rates_season_panel(
      prepare_total_rates_plot_data(rates_df, "covid", codelist_type),
      "covid"
    )
  )
}

run_cohort_condensed_total_rates <- function(
    cohort,
    out_root = here::here(
      "post_check", "plots", "primary_analyses", "outcome_ascertainment"
    )
) {
  rates_df <- load_cohort_total_rates(cohort)
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  for (codelist_type in c("specific", "sensitive")) {
    fig <- make_condensed_total_rates_figure(rates_df, codelist_type)
    ggsave(
      here::here(
        out_root,
        paste0(cohort, "_total_rates_by_season_", codelist_type, ".png")
      ),
      fig,
      width = CONDENSED_RATES_FIG_WIDTH,
      height = CONDENSED_RATES_FIG_HEIGHT
    )
  }

  invisible(rates_df)
}

rate_cohorts <- c(
  "older_adults", "adults", "children_and_adolescents",
  "infants", "infants_subgroup"
)

for (cohort in rate_cohorts) {
  message("Running condensed total rates by season: ", cohort)
  tryCatch(
    run_cohort_condensed_total_rates(cohort),
    error = function(e) {
      message(
        "Failed: cohort=", cohort, " :: ", conditionMessage(e)
      )
      NULL
    }
  )
}
