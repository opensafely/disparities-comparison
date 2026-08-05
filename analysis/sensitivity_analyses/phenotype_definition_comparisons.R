library(tidyverse)
library(here)
library(lubridate)
library(readr)

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2023-09-01"
  study_end_date <- "2024-08-31"
  cohort <- "older_adults"
  codelist_type <- "alternative"
} else {
  cohort <- args[[1]]
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  codelist_type <- args[[4]]
}

#import redaction function
source(here::here("analysis", "functions", "redaction.R"))

##phenotype sensitivity

#import the data from the original definitions
df <- read_csv(here::here("output", "exploratory", paste0(
  "phenotype_sensitivity_", cohort, "_", year(study_start_date), "_", 
  year(study_end_date), ".csv"))) %>% 
  filter(codelist_type == "sensitive") %>% 
  mutate(n = roundmid_any(n))

#and the alternative definitions
df_alt <- read_csv(here::here("output", "additional_sensitivity", paste0(
  "phenotype_sensitivity_testing_", cohort, "_", year(study_start_date), "_", 
  year(study_end_date), "_", codelist_type, ".csv"))) %>% 
  filter(codelist_type == "sensitive") 

#compare n between original and alternative definitions
df_change <- df %>%
  select(combo, outcome_type, n) %>%
  rename(n_original = n) %>%
  full_join(
    df_alt %>%
      select(combo, outcome_type, n) %>%
      rename(n_alt = n),
    by = c("combo", "outcome_type")
  ) %>%
  mutate(
    n_change = n_alt - n_original,
    pct_change = (n_alt - n_original) / n_original * 100
  )

#save
write_csv(df_change, here::here("output", "additional_sensitivity", paste0(
  "changes_in_phenotype_sensitivity_", cohort, "_", year(study_start_date), "_", 
  year(study_end_date), "_", codelist_type, ".csv")))

##reinfections

#import the data from the original definitions
df <- read_csv(here::here("output", "exploratory", paste0(
  "reinfections_", cohort, "_", year(study_start_date), "_", 
  year(study_end_date), "_sensitive.csv")))

#and the alternative definitions
df_alt <- read_csv(here::here("output", "additional_sensitivity", paste0(
  "reinfections_", cohort, "_", year(study_start_date), "_", 
  year(study_end_date), "_", codelist_type, ".csv")))

#compare n between original and alternative definitions (long format)
df_change <- df %>%
  select(infection_type, outcome_type,
         number_infected_midpoint10, number_reinfected_midpoint10) %>%
  pivot_longer(
    cols = c(number_infected_midpoint10, number_reinfected_midpoint10),
    names_to = "measure",
    values_to = "n_original"
  ) %>%
  mutate(measure = recode(measure,
    number_infected_midpoint10 = "infected",
    number_reinfected_midpoint10 = "reinfected"
  )) %>%
  full_join(
    df_alt %>%
      select(infection_type, outcome_type,
             number_infected_midpoint10, number_reinfected_midpoint10) %>%
      pivot_longer(
        cols = c(number_infected_midpoint10, number_reinfected_midpoint10),
        names_to = "measure",
        values_to = "n_alt"
      ) %>%
      mutate(measure = recode(measure,
        number_infected_midpoint10 = "infected",
        number_reinfected_midpoint10 = "reinfected"
      )),
    by = c("infection_type", "outcome_type", "measure")
  ) %>%
  mutate(
    n_change = n_alt - n_original,
    pct_change = (n_alt - n_original) / n_original * 100
  )

#save
write_csv(df_change, here::here("output", "additional_sensitivity", paste0(
  "changes_in_reinfections_", cohort, "_", year(study_start_date), "_", 
  year(study_end_date), "_", codelist_type, ".csv")))
