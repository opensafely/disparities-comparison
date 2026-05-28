library(tidyverse)
library(here)
library(arrow)
library(cowplot)

source(here::here("post_check", "functions", "forest.R"))
ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)
investigation_type <- "primary"
df_few <- tibble()

model_type <- "ethnicity_ses"

load_collated_further <- function(cohort, pathogen) {
  raw <- read_csv(
    here::here(
      "post_check", "output", "collated", "analytic",
      paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")
    ),
    show_col_types = FALSE
  )
  df_few <<- bind_rows(df_few, raw %>% filter(term == "too few events"))
  raw %>% filter(term != "too few events")
}

load_dummy_inputs <- function(cohort, pathogen) {
  if (identical(pathogen, "covid")) {
    read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2021_2022_specific_primary.arrow")
      )
    ) %>%
      mutate(
        covid_vaccination_immunity_date = if (
          !"covid_vaccination_immunity_date" %in% names(.)
        ) NA else covid_vaccination_immunity_date,
        time_since_last_covid_vaccination = if (
          !"time_since_last_covid_vaccination" %in% names(.)
        ) NA_character_ else time_since_last_covid_vaccination
      ) %>%
      mutate(
        subset = "2021_22",
        time_since_last_covid_vaccination = if_else(
          is.na(covid_vaccination_immunity_date),
          "6-12m",
          time_since_last_covid_vaccination
        )
      )
  } else {
    read_feather(
      here::here(
        "output", "data",
        paste0("input_processed_", cohort, "_2020_2021_specific_primary.arrow")
      )
    )
  }
}

make_facet_outcome_plots_key_vars <- function(df_input, df_dummy, pathogen, model_type) {
  mild_dat <- forest_year_further_mult_key_vars(
    df_input, df_dummy, pathogen, model_type, "Mild", return_data = TRUE
  )
  severe_dat <- forest_year_further_mult_key_vars(
    df_input, df_dummy, pathogen, model_type, "Severe", return_data = TRUE
  )
  both_dat <- bind_rows(mild_dat, severe_dat)

  list(
    specific = forest_over_time_plot(
      both_dat %>% filter(codelist_type %in% c("reference", "specific")),
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      label_levels = FALSE,
      show_disruption_legend = FALSE
    ) + theme(legend.position = "none"),
    sensitive = forest_over_time_plot(
      both_dat %>% filter(codelist_type %in% c("reference", "sensitive")),
      pathogen = pathogen,
      model_type = model_type,
      facet_outcome = TRUE,
      label_levels = FALSE,
      show_disruption_legend = FALSE
    ) + theme(legend.position = "none")
  )
}

assemble_condensed_key_vars <- function(
    rsv_plot,
    flu_plot,
    covid_plot,
    legend_left,
    legend_mid = NULL
) {
  covid_plot_with_mid <- covid_plot
  if (!is.null(legend_mid)) {
    covid_plot_with_mid <- cowplot::ggdraw(covid_plot) +
      cowplot::draw_grob(legend_mid, x = 0.44, y = 0.08, width = 0.12, height = 0.84)
  }

  covid_row <- plot_grid(
    NULL, legend_left, covid_plot_with_mid, NULL,
    ncol = 4,
    rel_widths = c(-0.1, 0.9, 5.1, -0.16),
    align = "h",
    axis = "tb"
  )

  combined <- plot_grid(
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

build_shared_legends_key_vars <- function(legend_dat, model_type, legend_pathogen = "covid") {
  groups_left <- intersect("Age Group", unique(as.character(legend_dat$labels)))
  groups_mid <- intersect(
    c("Ethnicity", "IMD Quintile"),
    unique(as.character(legend_dat$labels))
  )

  legend_left <- if (length(groups_left) > 0) {
    get_legend({
      forest_over_time_plot(
        legend_dat %>% filter(as.character(labels) %in% groups_left),
        pathogen = legend_pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = FALSE,
        show_disruption_legend = TRUE
      ) +
        theme(legend.position = "left", legend.title = element_blank())
    })
  } else {
    NULL
  }

  legend_mid <- if (length(groups_mid) > 0) {
    get_legend({
      forest_over_time_plot(
        legend_dat %>% filter(as.character(labels) %in% groups_mid),
        pathogen = legend_pathogen,
        model_type = model_type,
        facet_outcome = TRUE,
        label_levels = FALSE,
        show_disruption_legend = FALSE
      ) +
        theme(legend.position = "left", legend.title = element_blank())
    })
  } else {
    NULL
  }

  list(left = legend_left, mid = legend_mid)
}

run_cohort_condensed_key_vars <- function(cohort) {
  cohort <<- cohort

  pathogen <- "rsv"
  df_input_rsv <- load_collated_further(cohort, pathogen)
  df_dummy_rsv <- load_dummy_inputs(cohort, pathogen)
  rsv_plots <- make_facet_outcome_plots_key_vars(
    df_input_rsv, df_dummy_rsv, pathogen, model_type
  )

  pathogen <- "flu"
  df_input_flu <- load_collated_further(cohort, pathogen)
  df_dummy_flu <- load_dummy_inputs(cohort, pathogen)
  flu_plots <- make_facet_outcome_plots_key_vars(
    df_input_flu, df_dummy_flu, pathogen, model_type
  )

  pathogen <- "covid"
  df_input_covid <- load_collated_further(cohort, pathogen)
  df_dummy_covid <- load_dummy_inputs(cohort, pathogen)
  covid_plots <- make_facet_outcome_plots_key_vars(
    df_input_covid, df_dummy_covid, pathogen, model_type
  )

  legend_dat <- bind_rows(
    forest_year_further_mult_key_vars(
      df_input_covid, df_dummy_covid, "covid", model_type, "Mild", return_data = TRUE
    ),
    forest_year_further_mult_key_vars(
      df_input_covid, df_dummy_covid, "covid", model_type, "Severe", return_data = TRUE
    )
  ) %>%
    filter(codelist_type %in% c("reference", "specific"))

  shared_legends <- build_shared_legends_key_vars(legend_dat, model_type)

  specific_condensed <- assemble_condensed_key_vars(
    rsv_plots$specific,
    flu_plots$specific,
    covid_plots$specific,
    shared_legends$left,
    shared_legends$mid
  )
  sensitive_condensed <- assemble_condensed_key_vars(
    rsv_plots$sensitive,
    flu_plots$sensitive,
    covid_plots$sensitive,
    shared_legends$left,
    shared_legends$mid
  )

  out_dir <- here::here(
    "post_check", "plots", "primary_analyses", "condensed_models_key_vars"
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    here::here(
      out_dir,
      paste0(cohort, "_", model_type, "_further_specific_mild_vs_severe_key_vars.png")
    ),
    specific_condensed,
    height = 14,
    width = 8.5
  )
  ggsave(
    here::here(
      out_dir,
      paste0(cohort, "_", model_type, "_further_sensitive_mild_vs_severe_key_vars.png")
    ),
    sensitive_condensed,
    height = 14,
    width = 8.5
  )
}

cohorts <- c("older_adults", "adults", "children_and_adolescents", "infants", "infants_subgroup")

for (cohort in cohorts) {
  message("Running key-vars condensed (ethnicity_ses): ", cohort)
  tryCatch(
    run_cohort_condensed_key_vars(cohort),
    error = function(e) {
      message(
        "Failed: cohort=", cohort,
        " model_type=", model_type,
        " :: ", conditionMessage(e)
      )
      NULL
    }
  )
}
