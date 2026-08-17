library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(scales)

source(here::here("analysis", "design", "design.R"))
source(here::here("analysis", "functions", "redaction.R"))

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2023-09-01")
  study_end_date <- as.Date("2024-08-31")
  alternatives <- c("alternative", "second_alternative")
} else {
  study_start_date <- as.Date(study_dates[[args[[1]]]])
  study_end_date <- as.Date(study_dates[[args[[2]]]])
  alternatives <- args[[3]]
}

cohorts <- c("older_adults", "adults", "children_and_adolescents", "infants")
pathogens <- c("rsv", "flu", "covid")

pathogen_labels <- c(
  rsv = "RSV",
  flu = "Influenza",
  covid = "COVID-19"
)

alt_labels <- c(
  alternative = "Alternative",
  second_alternative = "Second alternative"
)

out_dir <- here::here("output", "additional_sensitivity")
fs::dir_create(out_dir)

processed_path <- function(cohort, codelist_type, investigation_type) {
  here::here(
    "output", "data",
    paste0(
      "input_processed_", cohort, "_",
      year(study_start_date), "_", year(study_end_date), "_",
      codelist_type, "_", investigation_type, ".arrow"
    )
  )
}

read_processed <- function(path) {
  read_feather(
    path,
    col_select = any_of(c(
      "patient_id",
      paste0(rep(pathogens, each = 2), c("_primary_date", "_secondary_date"))
    ))
  )
}

monthly_counts <- function(df, pathogen) {
  date_cols <- paste0(pathogen, c("_primary_date", "_secondary_date"))
  date_cols <- intersect(date_cols, names(df))
  months <- seq(
    floor_date(study_start_date, "month"),
    floor_date(study_end_date, "month"),
    by = "month"
  )

  df %>%
    select(all_of(date_cols)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "event",
      values_to = "date"
    ) %>%
    filter(!is.na(date)) %>%
    mutate(month = floor_date(as.Date(date), "month")) %>%
    count(month, event, name = "n") %>%
    complete(month = months, event = date_cols, fill = list(n = 0)) %>%
    mutate(
      n = roundmid_any(n),
      severity = if_else(str_detect(event, "primary"), "Mild", "Severe"),
      severity = factor(severity, levels = c("Mild", "Severe"))
    ) %>%
    arrange(month, event)
}

plot_panel <- function(df, severity, pathogen_label) {
  ggplot(df, aes(x = month, y = n, colour = phenotype, group = phenotype)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_colour_manual(
      values = c("Original" = "#1F449c", "Alternative" = "#F05039"),
      breaks = c("Original", "Alternative")
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(
      limits = c(0, NA),
      labels = comma,
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(subtitle = paste(severity, pathogen_label), x = NULL, y = "Cases (midpoint 10)") +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size = 12)
    )
}

for (alt in alternatives) {
  alt_label <- unname(alt_labels[[alt]])
  if (is.na(alt_label)) alt_label <- str_to_sentence(gsub("_", " ", alt))

  cohort_counts <- list()

  for (cohort in cohorts) {
    original_path <- processed_path(cohort, "sensitive", "primary")
    alt_path <- processed_path(cohort, alt, "additional_sensitivity")
    if (!file.exists(original_path) || !file.exists(alt_path)) {
      message("Skipping ", cohort, " / ", alt, ": processed file missing")
      next
    }

    df_original <- read_processed(original_path)
    df_alt <- read_processed(alt_path)

    for (pathogen in pathogens) {
      date_cols <- paste0(pathogen, c("_primary_date", "_secondary_date"))
      if (!all(date_cols %in% names(df_original)) ||
          !all(date_cols %in% names(df_alt))) {
        message("Skipping ", pathogen, " for ", cohort, ": date columns missing")
        next
      }

      cohort_counts[[length(cohort_counts) + 1]] <- bind_rows(
        monthly_counts(df_original, pathogen) %>%
          mutate(phenotype = "Original"),
        monthly_counts(df_alt, pathogen) %>%
          mutate(phenotype = "Alternative")
      ) %>%
        mutate(pathogen = pathogen, cohort = cohort)
    }
  }

  if (length(cohort_counts) == 0) {
    message("No data for ", alt)
    next
  }

  plot_df <- bind_rows(cohort_counts) %>%
    group_by(month, event, severity, phenotype, pathogen) %>%
    summarise(n = sum(n), .groups = "drop")

  for (pathogen in unique(plot_df$pathogen)) {
    pathogen_label <- unname(pathogen_labels[[pathogen]])
    df_pathogen <- filter(plot_df, pathogen == .env$pathogen)

    p_mild <- plot_panel(filter(df_pathogen, severity == "Mild"), "Mild", pathogen_label)
    p_severe <- plot_panel(filter(df_pathogen, severity == "Severe"), "Severe", pathogen_label)

    legend <- get_legend(
      p_mild +
        theme(
          legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 12)
        )
    )

    title <- ggdraw() +
      draw_label(
        paste0(
          "All cohorts: ", pathogen_label,
          " — original vs ", tolower(alt_label)
        ),
        fontface = "bold",
        x = 0,
        hjust = 0,
        size = 14
      ) +
      theme(plot.margin = margin(4, 0, 0, 8))

    plot_an <- plot_grid(
      title,
      plot_grid(
        plot_grid(p_mild, p_severe, nrow = 1),
        legend,
        ncol = 2,
        rel_widths = c(1, 0.18)
      ),
      ncol = 1,
      rel_heights = c(0.1, 1)
    )

    outfile <- file.path(
      out_dir,
      paste0("all_cohorts_", pathogen, "_", alt, "_monthly_counts.png")
    )
    ggsave(outfile, plot_an, width = 11, height = 5)
    message("Wrote ", outfile)
  }
}
