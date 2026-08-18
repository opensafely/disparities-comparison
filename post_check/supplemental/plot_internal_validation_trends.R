library(tidyverse)
library(here)
library(cowplot)

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

cohorts <- c("older_adults", "adults", "children_and_adolescents",
             "infants", "infants_subgroup")

cohort_labels <- c(
  older_adults = "Older Adults",
  adults = "Adults",
  children_and_adolescents = "Children and Young People",
  infants = "Infants",
  infants_subgroup = "Maternally Linked Infants"
)

all_seasons <- c("2016-17", "2017-18", "2018-19", "2019-20",
                 "2020-21", "2021-22", "2022-23", "2023-24")

classification_levels <- c(
  "Consistent classification",
  "Inconsistent classification",
  "Undetermined classification"
)

classification_cols <- c(
  "Consistent classification" = "#1B9E77",
  "Inconsistent classification" = "#D95F02",
  "Undetermined classification" = "#7570B3"
)

# Parse "1,970 (29.3%)" -> list(count, pct); NA stays NA
parse_count_pct <- function(x) {
  x <- as.character(x)
  tibble(
    raw = x,
    count = if_else(
      is.na(x) | x == "NA",
      NA_real_,
      as.numeric(str_replace_all(str_extract(x, "^[0-9,]+"), ",", ""))
    ),
    pct = if_else(
      is.na(x) | x == "NA",
      NA_real_,
      as.numeric(str_match(x, "\\(([0-9.]+)%\\)")[, 2])
    )
  ) %>%
    select(count, pct)
}

read_validation_long <- function(cohort) {
  path <- here::here(
    "post_check", "supplemental", "internal_validation",
    paste0(cohort, "_internal_validation_formatted.csv")
  )

  read_csv(path, show_col_types = FALSE) %>%
    pivot_longer(
      cols = all_of(intersect(all_seasons, names(.))),
      names_to = "season",
      values_to = "cell"
    ) %>%
    bind_cols(parse_count_pct(.$cell)) %>%
    select(-cell) %>%
    mutate(
      cohort = .env$cohort,
      cohort_label = unname(cohort_labels[.env$cohort]),
      season = factor(season, levels = all_seasons),
      pathogen = str_remove(secondary_care_outcome, " Hospitalisation"),
      pathogen = factor(pathogen, levels = c("RSV", "Influenza", "COVID-19")),
      phenotype = recode(
        as.character(phenotype),
        Specific = "Narrow",
        Sensitive = "Broad",
        .default = as.character(phenotype)
      ),
      phenotype = factor(phenotype, levels = c("Narrow", "Broad")),
      outcome = factor(
        outcome,
        levels = c("Not detected", classification_levels)
      )
    )
}

df_all <- map_dfr(cohorts, read_validation_long)

out_dir <- here::here("post_check", "plots", "supplemental", "internal_validation")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- % not detected (of hospitalisations) over seasons --------------------
# Not-detected rate is identical by phenotype (same secondary pop, any mild),
# so use a single series (Narrow).
plot_not_detected <- function(df, cohort) {
  df_nd <- df %>%
    filter(
      cohort == .env$cohort,
      outcome == "Not detected",
      phenotype == "Narrow"
    )

  ggplot(df_nd, aes(x = season, y = pct, group = 1)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    facet_wrap(~pathogen, nrow = 1) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.08))) +
    labs(
      title = paste0(cohort_labels[[cohort]], ": % not detected"),
      subtitle = "% of pathogen-specific hospitalisations with no mild primary in window",
      x = "Season",
      y = "% not detected"
    ) +
    theme_bw(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      legend.position = "none",
      strip.background = element_blank()
    )
}

# --- Classification mix among detected (stacked % of detected) ------------
plot_classification_mix <- function(df, cohort) {
  df_det <- df %>%
    filter(
      cohort == .env$cohort,
      outcome %in% classification_levels,
      !is.na(pct),
      !is.na(count)
    ) %>%
    mutate(
      outcome = factor(as.character(outcome), levels = classification_levels)
    )

  # Detected n for each bar (denominator of the stacked %)
  df_n <- df_det %>%
    group_by(phenotype, pathogen, season) %>%
    summarise(detected = sum(count), .groups = "drop")

  ggplot(df_det, aes(x = season, y = pct, fill = outcome)) +
    geom_col(width = 0.75, colour = "white", linewidth = 0.2) +
    geom_text(
      data = df_n,
      aes(x = season, y = 100, label = scales::comma(detected)),
      inherit.aes = FALSE,
      vjust = -0.35,
      size = 3.25
    ) +
    facet_grid(phenotype ~ pathogen) +
    scale_fill_manual(values = classification_cols, name = NULL) +
    scale_y_continuous(
      breaks = seq(0, 100, 25),
      expand = expansion(mult = c(0, 0.1))
    ) +
    # Use coord_cartesian (not scale limits) so stacks that sum to ~100.1%
    # from rounding are not dropped as out-of-range; clip=off for n labels.
    coord_cartesian(ylim = c(0, 100), clip = "off") +
    labs(
      title = paste0(cohort_labels[[cohort]], ": classification among detected"),
      subtitle = paste0(
        "% of mild outcomes that are consistent / inconsistent / undetermined; ",
        "numbers above bars are detected mild outcomes (n)"
      ),
      x = "Season",
      y = "% of detected"
    ) +
    theme_bw(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      legend.position = "bottom",
      strip.background = element_blank(),
      plot.margin = margin(t = 10, r = 8, b = 4, l = 8),
      panel.spacing.y = unit(1.2, "lines")
    )
}

# --- Combined one-page summary per cohort ---------------------------------
plot_cohort_trends <- function(df, cohort) {
  p_nd <- plot_not_detected(df, cohort) +
    theme(plot.title = element_text(size = 14))
  p_mix <- plot_classification_mix(df, cohort) +
    theme(plot.title = element_text(size = 14))

  plot_grid(p_nd, p_mix, ncol = 1, rel_heights = c(0.9, 1.2), align = "v")
}

# Also: all-cohort overlay for not-detected (one pathogen per panel)
plot_not_detected_all_cohorts <- function(df) {
  df_nd <- df %>%
    filter(outcome == "Not detected", phenotype == "Narrow") %>%
    mutate(
      cohort_label = factor(
        cohort_label,
        levels = unname(cohort_labels)
      )
    )

  ggplot(df_nd, aes(x = season, y = pct, group = cohort_label, colour = cohort_label)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.6) +
    facet_wrap(~pathogen, nrow = 1) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.08))) +
    labs(
      title = "% not detected over seasons, by cohort",
      subtitle = "% of pathogen-specific hospitalisations with no mild primary in window",
      x = "Season",
      y = "% not detected",
      colour = "Cohort"
    ) +
    theme_bw(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      legend.position = "bottom",
      strip.background = element_blank()
    )
}

# Save per-cohort figures
walk(cohorts, function(cohort) {
  p <- plot_cohort_trends(df_all, cohort)
  ggsave(
    file.path(out_dir, paste0(cohort, "_validation_trends.png")),
    p, width = 12, height = 10
  )
})

# Save all-cohort not-detected overlay
ggsave(
  file.path(out_dir, "all_cohorts_not_detected_trends.png"),
  plot_not_detected_all_cohorts(df_all),
  width = 12, height = 6
)

message("Wrote figures to ", out_dir)
