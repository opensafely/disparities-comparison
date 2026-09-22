library(tidyverse)
library(here)

# Age-group analysis cohorts (exclude infants_subgroup — nested within infants)
cohorts <- c("older_adults", "adults", "children_and_adolescents", "infants")

all_seasons <- c("2016_17", "2017_18", "2018_19", "2019_20",
                 "2020_21", "2021_22", "2022_23", "2023_24")

outcomes <- c("RSV Mild", "RSV Severe",
              "Flu Mild", "Flu Severe",
              "COVID Mild", "COVID Severe")

df <- map_dfr(cohorts, function(cohort) {
  read_csv(
    here::here(
      "post_check", "output", "collated", "descriptive",
      paste0(cohort, "_rates_primary_collated.csv")
    ),
    show_col_types = FALSE
  ) %>%
    mutate(cohort = cohort)
})

burden <- df %>%
  filter(
    Characteristic == "Total",
    Group == "All",
    Outcome %in% outcomes
  ) %>%
  group_by(Outcome, codelist_type, subset) %>%
  summarise(
    n_cases = sum(Events_Midpoint10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
  select(pathogen, phenotype, severity, season, n_cases) %>%
  pivot_wider(
    names_from  = season,
    values_from = n_cases
  ) %>%
  mutate(
    pathogen  = factor(pathogen,  levels = c("RSV", "Influenza", "COVID-19")),
    phenotype = factor(phenotype, levels = c("Specific", "Sensitive"),
                       labels = c("Narrow", "Broad")),
    severity  = factor(severity,  levels = c("Mild", "Severe"))
  ) %>%
  arrange(pathogen, phenotype, severity)

season_cols <- gsub("_", "-", all_seasons)
missing_cols <- setdiff(season_cols, names(burden))
burden[missing_cols] <- NA_real_

burden <- burden %>%
  select(pathogen, phenotype, severity, all_of(season_cols))

out_dir <- here::here("post_check", "supplemental", "case_burden")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(
  burden,
  file.path(out_dir, "all_ages_case_burden_formatted.csv")
)

# Per-pathogen tables
burden %>%
  group_split(pathogen) %>%
  walk(function(df_pathogen) {
    virus_stub <- case_when(
      df_pathogen$pathogen[1] == "RSV"        ~ "rsv",
      df_pathogen$pathogen[1] == "Influenza"  ~ "flu",
      df_pathogen$pathogen[1] == "COVID-19"   ~ "covid"
    )
    write_csv(
      df_pathogen,
      file.path(out_dir, paste0(virus_stub, "_case_burden_formatted.csv"))
    )
  })
