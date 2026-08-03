library(tidyverse)
library(here)

all_seasons <- c("2016-17", "2017-18", "2018-19", "2019-20",
                 "2020-21", "2021-22", "2022-23", "2023-24")

viruses <- c("rsv", "flu", "covid")

reformat_cases <- function(virus) {

  df <- read_csv(
    here::here("post_check", "supplemental", "surveillance",
               paste0(virus, "_cases_all_seasons.csv")),
    show_col_types = FALSE
  )

  df_total <- df %>%
    group_by(virus, codelist_type, event, season) %>%
    summarise(n_cases = sum(total_events, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      phenotype = str_to_title(codelist_type),
      severity  = event
    ) %>%
    select(virus, phenotype, severity, season, n_cases) %>%
    pivot_wider(
      names_from  = season,
      values_from = n_cases
    ) %>%
    mutate(
      virus     = factor(virus, levels = c("RSV", "Influenza", "COVID-19")),
      phenotype = factor(phenotype, levels = c("Specific", "Sensitive")),
      severity  = factor(severity, levels = c("Mild", "Severe"))
    ) %>%
    arrange(virus, phenotype, severity)

  missing_cols <- setdiff(all_seasons, names(df_total))
  df_total[missing_cols] <- NA_real_

  df_total %>%
    select(virus, phenotype, severity, all_of(all_seasons))
}

cases_list <- map(viruses, reformat_cases) %>%
  set_names(viruses)

out_dir <- here::here("post_check", "supplemental", "all_cohort_cases")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

iwalk(cases_list, function(df, virus) {
  write_csv(df,
            file.path(out_dir, paste0(virus, "_all_cases_formatted.csv")))
})

# Combined table across viruses
write_csv(
  bind_rows(cases_list) %>%
    mutate(
      virus     = factor(virus, levels = c("RSV", "Influenza", "COVID-19")),
      phenotype = factor(phenotype, levels = c("Specific", "Sensitive")),
      severity  = factor(severity, levels = c("Mild", "Severe"))
    ) %>%
    arrange(virus, phenotype, severity),
  file.path(out_dir, "all_viruses_all_cases_formatted.csv")
)
