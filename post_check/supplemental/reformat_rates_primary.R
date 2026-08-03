library(tidyverse)
library(here)

cohorts <- c("older_adults", "adults", "children_and_adolescents",
             "infants", "infants_subgroup")

all_seasons <- c("2016_17", "2017_18", "2018_19", "2019_20",
                 "2020_21", "2021_22", "2022_23", "2023_24")

reformat_rates <- function(cohort) {

  df <- read_csv(here::here("post_check", "output", "collated",
                             "descriptive",
                             paste0(cohort, "_rates_primary_collated.csv")),
                 show_col_types = FALSE)

  df_total <- df %>%
    filter(Characteristic == "Total", Group == "All") %>%
    filter(Outcome %in% c("RSV Mild", "RSV Severe",
                          "Flu Mild", "Flu Severe",
                          "COVID Mild", "COVID Severe")) %>%
    mutate(
      pathogen = case_when(
        str_starts(Outcome, "RSV")   ~ "RSV",
        str_starts(Outcome, "Flu")   ~ "Influenza",
        str_starts(Outcome, "COVID") ~ "COVID-19"
      ),
      severity = case_when(
        str_ends(Outcome, "Mild")   ~ "Mild",
        str_ends(Outcome, "Severe") ~ "Severe"
      ),
      phenotype = str_to_title(codelist_type),
      season    = gsub("_", "-", subset)
    ) %>%
    select(pathogen, phenotype, severity, season,
           Rate_Midpoint10_Derived) %>%
    mutate(Rate_Midpoint10_Derived = round(Rate_Midpoint10_Derived, 2)) %>%
    pivot_wider(
      names_from  = season,
      values_from = Rate_Midpoint10_Derived
    ) %>%
    mutate(
      pathogen  = factor(pathogen,  levels = c("RSV", "Influenza", "COVID-19")),
      phenotype = factor(phenotype, levels = c("Specific", "Sensitive")),
      severity  = factor(severity,  levels = c("Mild", "Severe"))
    ) %>%
    arrange(pathogen, phenotype, severity)

  season_cols <- gsub("_", "-", all_seasons)
  missing_cols <- setdiff(season_cols, names(df_total))
  df_total[missing_cols] <- NA_real_

  df_total <- df_total %>%
    select(pathogen, phenotype, severity, all_of(season_cols))

  df_total
}

rates_list <- map(cohorts, reformat_rates) %>%
  set_names(cohorts)

out_dir <- here::here("post_check", "supplemental", "rates_primary")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

iwalk(rates_list, function(df, cohort) {
  write_csv(df,
            file.path(out_dir,
                      paste0(cohort, "_rates_primary_total_formatted.csv")))
})
