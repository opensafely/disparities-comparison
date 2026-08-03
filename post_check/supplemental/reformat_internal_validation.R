library(tidyverse)
library(here)

cohorts <- c("older_adults", "adults", "children_and_adolescents",
             "infants", "infants_subgroup")

all_seasons <- c("2016-17", "2017-18", "2018-19", "2019-20",
                 "2020-21", "2021-22", "2022-23", "2023-24")

outcome_order <- c(
  "Not detected",
  "Consistent classification",
  "Inconsistent classification",
  "Undetermined classification"
)

# Spec-stage "other" = sensitive mild in window but no specific mild.
# Fold into broad if sens was broad, otherwise into bucket.
fold_spec_other <- function(df) {
  df %>%
    mutate(
      spec_stage = case_when(
        spec_stage != "other" ~ spec_stage,
        sens_stage == "broad" ~ "broad",
        TRUE ~ "bucket"
      )
    )
}

collapse_outcome_group <- function(outcome, pathogen) {
  case_when(
    outcome == "no_mild" ~ "Not detected",
    outcome == "mild" ~ NA_character_,
    outcome %in% c("bucket", "broad") ~ "Undetermined classification",
    str_detect(outcome, fixed(pathogen)) ~ "Consistent classification",
    TRUE ~ "Inconsistent classification"
  )
}

format_count_pct <- function(count, pct) {
  if_else(
    is.na(count),
    NA_character_,
    sprintf("%s (%.1f%%)",
            format(count, big.mark = ",", trim = TRUE, scientific = FALSE),
            pct)
  )
}

season_has_covid <- function(season) {
  as.integer(substr(season, 1, 4)) >= 2019L
}

secondary_hospitalisation_total <- function(df_pops, season) {

  df_pops %>%
    filter(
      subset == .env$season,
      denominator %in% c("total_patients_rsv", "total_patients_flu",
                         "total_patients_covid")
    ) %>%
    select(
      population,
      outcome = denominator,
      denom = denominator_n,
      rounded = count,
      pct,
      subset
    ) %>%
    mutate(
      population = gsub("_pop$", "", population),
      phenotype = "sens_stage"
    )

}

prep_validation_season <- function(df_counts, df_pops, season) {

  total_patients_sec <- secondary_hospitalisation_total(df_pops, season)
  df_counts_season <- df_counts %>% filter(subset == .env$season)

  flow_counts_list <- list()

  for (pathogen in c("rsv", "flu", "covid")) {

    if (pathogen == "covid" && !season_has_covid(season)) {
      next
    }

    df_pops_filt <- total_patients_sec %>%
      filter(
        population == pathogen,
        outcome == paste0("total_patients_", pathogen)
      )

    if (nrow(df_pops_filt) == 0) {
      next
    }

    df_counts_filt <- df_counts_season %>%
      mutate(population = gsub("_pop$", "", population)) %>%
      filter(population == pathogen) %>%
      fold_spec_other() %>%
      pivot_longer(
        cols = ends_with("_stage"),
        names_to = "phenotype",
        values_to = "outcome"
      ) %>%
      group_by(population, phenotype, outcome, subset) %>%
      summarise(rounded = sum(rounded, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        denom = df_pops_filt$denom,
        pct = 100 * rounded / denom
      )

    flow_counts_list[[pathogen]] <- bind_rows(
      tibble(
        population = pathogen,
        outcome = "no_mild",
        denom = df_pops_filt$denom,
        rounded = df_pops_filt$denom - df_pops_filt$rounded,
        pct = (df_pops_filt$denom - df_pops_filt$rounded) /
          df_pops_filt$denom * 100,
        subset = season,
        phenotype = "spec_stage"
      ),
      tibble(
        population = pathogen,
        outcome = "no_mild",
        denom = df_pops_filt$denom,
        rounded = df_pops_filt$denom - df_pops_filt$rounded,
        pct = (df_pops_filt$denom - df_pops_filt$rounded) /
          df_pops_filt$denom * 100,
        subset = season,
        phenotype = "sens_stage"
      ),
      df_counts_filt
    )

  }

  bind_rows(flow_counts_list)

}

reformat_internal_validation <- function(cohort) {

  df_counts <- read_csv(
    here::here("post_check", "output", "collated", "descriptive",
               paste0(cohort, "_validation_counts_collated.csv")),
    show_col_types = FALSE
  )

  df_pops <- read_csv(
    here::here("post_check", "output", "collated", "descriptive",
               paste0(cohort, "_validation_pops_collated.csv")),
    show_col_types = FALSE
  )

  seasons <- unique(df_counts$subset)

  df_long <- map_dfr(seasons, function(season) {
    prep_validation_season(df_counts, df_pops, season)
  })

  df_grouped <- df_long %>%
    mutate(
      outcome = collapse_outcome_group(outcome, population)
    ) %>%
    filter(!is.na(outcome)) %>%
    group_by(population, phenotype, outcome, subset) %>%
    summarise(
      rounded = sum(rounded, na.rm = TRUE),
      denom = dplyr::first(denom),
      .groups = "drop"
    )

  # Detected = all mild outcomes (excludes "Not detected")
  df_formatted <- df_grouped %>%
    group_by(population, phenotype, subset) %>%
    mutate(
      detected = sum(rounded[outcome != "Not detected"], na.rm = TRUE),
      pct = case_when(
        outcome == "Not detected" ~ 100 * rounded / denom,
        detected > 0 ~ 100 * rounded / detected,
        TRUE ~ NA_real_
      ),
      cell = format_count_pct(rounded, pct)
    ) %>%
    ungroup() %>%
    mutate(
      phenotype = factor(
        phenotype,
        levels = c("spec_stage", "sens_stage"),
        labels = c("Specific", "Sensitive")
      ),
      secondary_care_outcome = factor(
        population,
        levels = c("rsv", "flu", "covid"),
        labels = c("RSV Hospitalisation", "Influenza Hospitalisation",
                   "COVID-19 Hospitalisation")
      ),
      outcome = factor(outcome, levels = outcome_order),
      season = gsub("_", "-", subset)
    ) %>%
    select(phenotype, secondary_care_outcome, outcome, season, cell) %>%
    pivot_wider(
      names_from = season,
      values_from = cell
    ) %>%
    arrange(phenotype, secondary_care_outcome, outcome)

  missing_cols <- setdiff(all_seasons, names(df_formatted))
  df_formatted[missing_cols] <- NA_character_

  df_formatted %>%
    select(phenotype, secondary_care_outcome, outcome, all_of(all_seasons))

}

out_dir <- here::here("post_check", "supplemental", "internal_validation")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

validation_list <- map(cohorts, reformat_internal_validation) %>%
  set_names(cohorts)

iwalk(validation_list, function(df, cohort) {
  write_csv(
    df,
    file.path(out_dir, paste0(cohort, "_internal_validation_formatted.csv"))
  )
})
