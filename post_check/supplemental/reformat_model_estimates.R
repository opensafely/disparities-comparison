library(tidyverse)
library(here)
library(readr)
library(stringr)

source(here::here("post_check", "functions", "forest_level_order.R"))

# Reformat collated further ethnicity_ses model outputs into supplemental-style
# tables: Variable | Category | season columns with point estimate (95% CI).
# Specific phenotype definition only. One table per cohort × phenotype.
# Row and level ordering matches forest plot key-variable ordering.

cohorts <- c(
  "older_adults",
  "adults",
  "children_and_adolescents",
  "infants",
  "infants_subgroup"
)

pathogens <- c("rsv", "flu", "covid")

season_levels <- c(
  "2016-17", "2017-18", "2018-19", "2019-20",
  "2020-21", "2021-22", "2022-23", "2023-24"
)

age_levels_by_cohort <- list(
  older_adults = c("65-74y", "75-89y", "90y+"),
  adults = c("18-39y", "40-64y"),
  children_and_adolescents = c("2-5y", "6-9y", "10-13y", "14-17y"),
  infants = c("0-2m", "3-5m", "6-11m", "12-23m"),
  infants_subgroup = c("0-2m", "3-5m", "6-11m", "12-23m")
)

# Table-only order (rural → urban); forest plots keep get_forest_level_order().
rurality_levels <- c(
  "Rural Town and Fringe",
  "Rural Village and Dispersed",
  "Urban City and Town",
  "Urban Minor Conurbation",
  "Urban Major Conurbation"
)

# Table-only sex order; forest plots keep get_forest_level_order().
sex_levels <- c("Female", "Male")

# Matches forest_over_time facet order for further models.
variable_facet_order <- c(
  "Age Group", 
  "Sex",
  "Ethnicity",
  "IMD quintile",
  "Rurality",
  "Prior Flu Vaccination",
  "Current Flu Vaccination",
  "Prior COVID Vaccination",
  "Current COVID Vaccination",
  "Maternal Pertussis Vaccination",
  "Maternal Flu Vaccination",
  "Maternal Drug Usage",
  "Maternal Drinking",
  "Maternal Smoking Status",
  "Maternal Age"
)

format_est_ci <- function(estimate, conf.low, conf.high, digits = 2) {
  ifelse(
    is.na(estimate) | is.na(conf.low) | is.na(conf.high),
    NA_character_,
    sprintf(
      paste0("%.", digits, "f (%.", digits, "f-%.", digits, "f)"),
      estimate, conf.low, conf.high
    )
  )
}

classify_forest_label <- function(label, cohort, pathogen, age_levels) {
  if (label %in% rurality_levels) {
    return(list(
      variable = "Rurality",
      category = label,
      is_reference = label == "Rural Town and Fringe"
    ))
  }
  if (label %in% FOREST_IMD_LEVELS) {
    return(list(
      variable = "IMD quintile",
      category = label,
      is_reference = label == "5 (least deprived)"
    ))
  }
  if (label %in% FOREST_ETHNICITY_LEVELS) {
    return(list(
      variable = "Ethnicity",
      category = label,
      is_reference = label == "White"
    ))
  }
  if (label %in% age_levels) {
    return(list(
      variable = "Age Group",
      category = label,
      is_reference = label == age_levels[1]
    ))
  }
  if (label %in% c("Male", "Female")) {
    return(list(
      variable = "Sex",
      category = label,
      is_reference = label == "Female"
    ))
  }

  if (pathogen == "flu") {
    if (label == "Flu Vaccination (Yes)") {
      return(list(
        variable = "Current Flu Vaccination",
        category = "Yes",
        is_reference = FALSE
      ))
    }
    if (label == "Flu Vaccination (No)") {
      return(list(
        variable = "Current Flu Vaccination",
        category = "No",
        is_reference = TRUE
      ))
    }
    if (label == "Eligible and Vaccinated Last Autumn") {
      return(list(
        variable = "Prior Flu Vaccination",
        category = "Yes",
        is_reference = FALSE
      ))
    }
    if (label == "Not Vaccinated in Past Year") {
      return(list(
        variable = "Prior Flu Vaccination",
        category = "No",
        is_reference = TRUE
      ))
    }
  }

  if (pathogen == "covid") {
    if (label == "Covid Vaccination (Yes)") {
      return(list(
        variable = "Current COVID Vaccination",
        category = "Yes",
        is_reference = FALSE
      ))
    }
    if (label == "Covid Vaccination (No)") {
      return(list(
        variable = "Current COVID Vaccination",
        category = "No",
        is_reference = TRUE
      ))
    }
    if (label == "Not Vaccinated in Past Year") {
      return(list(
        variable = "Prior COVID Vaccination",
        category = "12m+",
        is_reference = TRUE
      ))
    }
    if (label == "Eligible and Vaccinated Last Autumn") {
      return(list(
        variable = "Prior COVID Vaccination",
        category = "6-12m",
        is_reference = FALSE
      ))
    }
    if (label == "Eligible and Vaccinated Last Spring") {
      return(list(
        variable = "Prior COVID Vaccination",
        category = "0-6m",
        is_reference = FALSE
      ))
    }
  }

  if (cohort == "infants_subgroup") {
    if (label == "Maternal Pertussis Vaccination") {
      return(list(
        variable = "Maternal Pertussis Vaccination",
        category = "Yes",
        is_reference = FALSE
      ))
    }
    if (label == "Maternal Flu Vaccination") {
      return(list(
        variable = "Maternal Flu Vaccination",
        category = "Yes",
        is_reference = FALSE
      ))
    }
    if (label == "Maternal Drug Usage") {
      return(list(
        variable = "Maternal Drug Usage",
        category = "Yes",
        is_reference = FALSE
      ))
    }
    if (label == "Maternal Drinking") {
      return(list(
        variable = "Maternal Drinking",
        category = "Yes",
        is_reference = FALSE
      ))
    }
    if (label == "Current Smoker") {
      return(list(
        variable = "Maternal Smoking Status",
        category = "Current",
        is_reference = FALSE
      ))
    }
    if (label == "Former Smoker") {
      return(list(
        variable = "Maternal Smoking Status",
        category = "Former",
        is_reference = FALSE
      ))
    }
    if (label == "Never Smoker") {
      return(list(
        variable = "Maternal Smoking Status",
        category = "Never",
        is_reference = TRUE
      ))
    }
    if (label == "Maternal Age") {
      return(list(
        variable = "Maternal Age",
        category = "Per year",
        is_reference = FALSE
      ))
    }
  }

  NULL
}

implicit_reference_rows <- function(present_variables, mapped) {
  binary_vars <- c(
    "Maternal Pertussis Vaccination",
    "Maternal Flu Vaccination",
    "Maternal Drug Usage",
    "Maternal Drinking"
  )

  refs <- purrr::map_dfr(binary_vars, function(var) {
    if (!var %in% present_variables) {
      return(NULL)
    }
    yes_rank <- mapped %>%
      filter(variable == var, category == "Yes") %>%
      pull(label_rank) %>%
      first()
    if (is.na(yes_rank)) {
      return(NULL)
    }
    tibble(
      variable = var,
      category = "No",
      is_reference = TRUE,
      label_rank = yes_rank + 0.1
    )
  })

  if ("Maternal Smoking Status" %in% present_variables) {
    never_rank <- mapped %>%
      filter(variable == "Maternal Smoking Status", category == "Never") %>%
      pull(label_rank) %>%
      first()
    if (!is.na(never_rank)) {
      refs <- bind_rows(
        refs,
        tibble(
          variable = "Maternal Smoking Status",
          category = "Unknown Smoking Status",
          is_reference = FALSE,
          label_rank = never_rank + 0.2
        )
      )
    }
  }

  refs
}

category_skeleton <- function(cohort, pathogen, present_variables) {
  age_levels <- age_levels_by_cohort[[cohort]]
  level_order <- get_forest_level_order(
    cohort, "ethnicity_ses", pathogen, "primary", style = "year_mult"
  )

  mapped <- purrr::map_dfr(seq_along(level_order), function(i) {
    label <- level_order[[i]]
    row <- classify_forest_label(label, cohort, pathogen, age_levels)
    if (is.null(row)) {
      return(NULL)
    }
    tibble(
      variable = row$variable,
      category = row$category,
      is_reference = row$is_reference,
      label_rank = i
    )
  })

  mapped <- bind_rows(
    mapped,
    implicit_reference_rows(present_variables, mapped)
  ) %>%
    filter(variable %in% present_variables) %>%
    distinct(variable, category, .keep_all = TRUE)

  mapped %>%
    mutate(
      label_rank = case_when(
        variable == "Rurality" ~ as.numeric(match(category, rurality_levels)),
        variable == "Sex" ~ as.numeric(match(category, sex_levels)),
        TRUE ~ label_rank
      ),
      variable = factor(variable, levels = variable_facet_order)
    ) %>%
    arrange(variable, label_rank) %>%
    mutate(row_order = row_number()) %>%
    select(variable, category, is_reference, row_order)
}

parse_term_rows <- function(df) {
  df %>%
    filter(
      !term %in% c("(Intercept)", "too few events"),
      !is.na(estimate)
    ) %>%
    mutate(
      variable = case_when(
        str_starts(term, "age_band") ~ "Age Group",
        str_starts(term, "sex") ~ "Sex",
        str_starts(term, "latest_ethnicity_group") ~ "Ethnicity",
        str_starts(term, "imd_quintile") ~ "IMD quintile",
        str_starts(term, "rurality_classification") ~ "Rurality",
        term == "prior_flu_vaccinationYes" ~ "Prior Flu Vaccination",
        term == "vax_status" & str_detect(model_name, "Influenza") ~
          "Current Flu Vaccination",
        term == "vax_status" & str_detect(model_name, "COVID") ~
          "Current COVID Vaccination",
        str_starts(term, "time_since_last_covid_vaccination") ~
          "Prior COVID Vaccination",
        term == "maternal_age" ~ "Maternal Age",
        str_starts(term, "maternal_smoking_status") ~ "Maternal Smoking Status",
        term == "maternal_drinkingYes" ~ "Maternal Drinking",
        term == "maternal_drug_usageYes" ~ "Maternal Drug Usage",
        term == "maternal_flu_vaccinationYes" ~ "Maternal Flu Vaccination",
        term == "maternal_pertussis_vaccinationYes" ~
          "Maternal Pertussis Vaccination",
        TRUE ~ NA_character_
      ),
      category = case_when(
        str_starts(term, "age_band") ~ str_remove(term, "^age_band"),
        str_starts(term, "sex") ~ str_remove(term, "^sex"),
        str_starts(term, "latest_ethnicity_group") ~
          str_remove(term, "^latest_ethnicity_group"),
        term == "imd_quintile2" ~ "4",
        term == "imd_quintile3" ~ "3",
        term == "imd_quintile4" ~ "2",
        term == "imd_quintile5 (most deprived)" ~ "1 (most deprived)",
        str_starts(term, "rurality_classification") ~
          str_remove(term, "^rurality_classification"),
        term == "prior_flu_vaccinationYes" ~ "Yes",
        term == "vax_status" ~ "Yes",
        term == "time_since_last_covid_vaccination0-6m" ~ "0-6m",
        term == "time_since_last_covid_vaccination6-12m" ~ "6-12m",
        term == "maternal_age" ~ "Per year",
        term == "maternal_smoking_statusCurrent" ~ "Current",
        term == "maternal_smoking_statusFormer" ~ "Former",
        term == "maternal_smoking_statusUnknown Smoking Status" ~
          "Unknown Smoking Status",
        term %in% c(
          "maternal_drinkingYes",
          "maternal_drug_usageYes",
          "maternal_flu_vaccinationYes",
          "maternal_pertussis_vaccinationYes"
        ) ~ "Yes",
        TRUE ~ NA_character_
      ),
      season = gsub("_", "-", subset),
      est_ci = as.character(format_est_ci(estimate, conf.low, conf.high)),
      is_reference = FALSE
    ) %>%
    mutate(
      category = if_else(category == "Other Ethnic Groups", "Chinese or Other", category)
    ) %>%
    filter(!is.na(variable), !is.na(category)) %>%
    select(variable, category, season, est_ci, is_reference)
}

phenotype_from_model_name <- function(model_name) {
  str_remove(model_name, " by Ethnicity and IMD Quintile$")
}

reformat_further_ethnicity_ses <- function(cohort, pathogen) {

  path <- here::here(
    "post_check", "output", "collated", "analytic",
    paste0(cohort, "_further_", pathogen, "_model_outputs_collated.csv")
  )

  if (!file.exists(path)) {
    warning("Missing file: ", path)
    return(invisible(NULL))
  }

  df_input <- read_csv(path, show_col_types = FALSE) %>%
    filter(
      model_type == "ethnicity_ses",
      codelist_type == "specific"
    )

  if (nrow(df_input) == 0) {
    warning("No further ethnicity_ses (specific) rows for ", cohort, " / ",
            pathogen)
    return(invisible(NULL))
  }

  phenos <- sort(unique(phenotype_from_model_name(df_input$model_name)))
  out_dir <- here::here("post_check", "supplemental", "model_estimates")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  for (pheno in phenos) {

    df_pheno <- df_input %>%
      filter(phenotype_from_model_name(model_name) == pheno)

    sparse_seasons <- df_pheno %>%
      filter(term == "too few events") %>%
      distinct(subset) %>%
      pull(subset)

    parsed <- parse_term_rows(df_pheno)

    if (nrow(parsed) == 0) {
      message("Skipping ", cohort, " / ", pathogen, " / ", pheno,
              " (no estimable coefficients for specific phenotype)")
      next
    }

    present_variables <- unique(c(
      "Age Group", "Sex", "Ethnicity", "IMD quintile", "Rurality",
      parsed$variable
    ))

    skeleton <- category_skeleton(cohort, pathogen, present_variables)
    refs <- skeleton %>% filter(is_reference)

    seasons_with_estimates <- unique(parsed$season)

    ref_long <- tidyr::crossing(refs, season = season_levels) %>%
      mutate(
        est_ci = if_else(
          season %in% seasons_with_estimates,
          "1.00",
          NA_character_
        ),
        est_ci = as.character(est_ci)
      ) %>%
      select(variable, category, season, est_ci, is_reference)

    if (length(sparse_seasons) > 0) {
      sparse_season_labels <- gsub("_", "-", sparse_seasons)
      parsed <- parsed %>% filter(!season %in% sparse_season_labels)
      ref_long <- ref_long %>%
        mutate(
          est_ci = if_else(
            season %in% sparse_season_labels,
            NA_character_,
            est_ci
          )
        )
    }

    table_long <- bind_rows(parsed, ref_long) %>%
      mutate(
        season = factor(season, levels = season_levels),
        variable = as.character(variable),
        category = as.character(category)
      ) %>%
      arrange(desc(is_reference)) %>%
      distinct(variable, category, season, .keep_all = TRUE) %>%
      select(-is_reference)

    table_wide <- skeleton %>%
      mutate(
        variable = as.character(variable),
        category = as.character(category)
      ) %>%
      select(variable, category, row_order) %>%
      left_join(table_long, by = c("variable", "category")) %>%
      tidyr::complete(
        nesting(variable, category, row_order),
        season = factor(season_levels, levels = season_levels)
      ) %>%
      arrange(row_order, season) %>%
      select(variable, category, row_order, season, est_ci) %>%
      tidyr::pivot_wider(
        names_from = season,
        values_from = est_ci
      ) %>%
      arrange(row_order) %>%
      select(-row_order) %>%
      rename(Variable = variable, Category = category)

    missing_seasons <- setdiff(season_levels, names(table_wide))
    for (s in missing_seasons) {
      table_wide[[s]] <- NA_character_
    }
    table_wide <- table_wide %>%
      select(Variable, Category, all_of(season_levels)) %>%
      mutate(across(all_of(season_levels), as.character))

    pheno_slug <- pheno %>%
      tolower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("_+", "_") %>%
      str_remove("^_|_$")

    outfile <- paste0(
      cohort, "_further_", pheno_slug,
      "_specific_ethnicity_ses_estimates.csv"
    )

    write_csv(table_wide, file.path(out_dir, outfile))
    message("Wrote ", outfile)
  }

  invisible(TRUE)
}

out_dir <- here::here("post_check", "supplemental", "model_estimates")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
old_files <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(old_files) > 0) {
  file.remove(old_files)
}

for (cohort in cohorts) {
  for (pathogen in pathogens) {
    reformat_further_ethnicity_ses(cohort, pathogen)
  }
}
