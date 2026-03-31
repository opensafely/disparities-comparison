library(tidyverse)
library(here)
library(arrow)
library(cowplot)

# Import plotting + forest helpers.
source(here::here("post_check", "functions", "forest.R"))
ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

investigation_type <- "primary"

# This script creates condensed plots for the "base" models (no 'further' in filename),
# i.e. models that include age/sex + exposures only (not the additional 'further' adjustments).

make_base_over_time_plot <- function(df_input, df_dummy, pathogen, model_type, phenotype) {
  df_use <- df_input %>%
    filter(tolower(codelist_type) %in% c("reference", tolower(phenotype)))

  mild_full <- forest(df_use, df_dummy, pathogen, model_type, "Mild", further = "no")
  severe_full <- forest(df_use, df_dummy, pathogen, model_type, "Severe", further = "no")

  # Remove legends from panels; keep a single shared legend.
  list(
    mild = mild_full + theme(legend.position = "none"),
    severe = severe_full + theme(legend.position = "none"),
    legend = get_legend(mild_full + theme(legend.position = "left"))
  )
}

assemble_base_condensed <- function(rsv, flu, covid) {
  # Each element is list(mild, severe, legend)
  rsv_row <- plot_grid(rsv$mild, rsv$severe, ncol = 2, align = "h", axis = "tb")
  flu_row <- plot_grid(flu$mild, flu$severe, ncol = 2, align = "h", axis = "tb")
  covid_row <- plot_grid(covid$mild, covid$severe, ncol = 2, align = "h", axis = "tb")

  # Put one shared legend at the left of the COVID row (as in the further script style).
  covid_with_legend <- plot_grid(covid$legend, covid_row, ncol = 2, rel_widths = c(0.9, 5.1), align = "h", axis = "tb")

  combined <- plot_grid(
    rsv_row,
    NULL,
    flu_row,
    NULL,
    covid_with_legend,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(1, -0.03, 1.25, -0.03, 1.35)
  )

  # Single column titles (once per page).
  if (requireNamespace("cowplot", quietly = TRUE)) {
    combined <- cowplot::ggdraw(combined) +
      cowplot::draw_label("Mild", x = 0.35, y = 1.01, hjust = 0.5, vjust = 1, fontface = "bold", size = 9) +
      cowplot::draw_label("Severe", x = 0.74, y = 1.01, hjust = 0.5, vjust = 1, fontface = "bold", size = 9)
  }

  combined
}

run_cohort_base_condensed <- function(cohort, model_type = "ethnicity_ses") {
  # `forest()` uses `cohort` as a global.
  cohort <<- cohort

  out_dir <- here("post_check", "plots", "primary_analyses", "condensed_models_base")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  load_inputs <- function(pathogen) {
    df_input <- read_csv(
      here::here("post_check", "output", "collated", "analytic", paste0(cohort, "_", pathogen, "_model_outputs_collated.csv")),
      show_col_types = FALSE
    )

    df_dummy <- if (identical(pathogen, "covid")) {
      read_feather(here::here("output", "data", paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow"))) %>%
        mutate(
          covid_vaccination_immunity_date = if (!"covid_vaccination_immunity_date" %in% names(.)) NA else covid_vaccination_immunity_date,
          time_since_last_covid_vaccination = if (!"time_since_last_covid_vaccination" %in% names(.)) NA_character_ else time_since_last_covid_vaccination
        ) %>%
        mutate(
          subset = "2021_22",
          time_since_last_covid_vaccination = if_else(is.na(covid_vaccination_immunity_date), "6-12m", time_since_last_covid_vaccination)
        )
    } else {
      read_feather(here::here("output", "data", paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow")))
    }

    list(df_input = df_input, df_dummy = df_dummy)
  }

  rsv_in <- load_inputs("rsv")
  flu_in <- load_inputs("flu")
  covid_in <- load_inputs("covid")

  for (phenotype in c("specific", "sensitive")) {
    rsv <- make_base_over_time_plot(rsv_in$df_input, rsv_in$df_dummy, "rsv", model_type, phenotype)
    flu <- make_base_over_time_plot(flu_in$df_input, flu_in$df_dummy, "flu", model_type, phenotype)
    covid <- make_base_over_time_plot(covid_in$df_input, covid_in$df_dummy, "covid", model_type, phenotype)

    final <- assemble_base_condensed(rsv, flu, covid)

    ggsave(
      here(out_dir, paste0(cohort, "_", model_type, "_base_", phenotype, "_mild_vs_severe.png")),
      final,
      height = 11.69,
      width = 8.27
    )

    rm(rsv, flu, covid, final)
    graphics.off()
    invisible(gc())
  }
}

purrr::walk(
  c("older_adults", "adults", "children_and_adolescents", "infants"),
  run_cohort_base_condensed
)

