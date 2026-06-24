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

stacked <- function(area = FALSE, show_legend = FALSE) {

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
    scale_y_continuous(
      labels = si_labels(digits = 1, width = 8),
      expand = expansion(mult = c(0, Y_AXIS_EXPAND))
    ) +
    labs(x = "", y = "Monthly Cases", fill = "Cohort") +
    scale_fill_manual(values = cohort_colours) +
    scale_color_manual(values = cohort_colours, guide = "none") +
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

  p + geom_text(
    data = label_data,
    aes(x = month, y = y_anchor, label = label, color = cohort),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.2,
    fontface = "bold"
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
