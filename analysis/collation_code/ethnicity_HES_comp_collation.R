library(plyr)
library(tidyverse)
library(here)
library(arrow)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "infants"
} else {
  cohort <- args[[1]]
}

## create output directories ----
fs::dir_create(here::here("output", "collated", "descriptive"))

##ethnicity HES presence comparisons

#import contingency tables
collated_ethnicity_hes_comp_tables = rbind(
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_",
           cohort, "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_tables_",
           cohort, "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24")
)
#and chi squared results
collated_ethnicity_hes_comp = rbind(
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_",
           cohort, "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_",
           cohort, "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24")
)
#and chi squared results by age
collated_ethnicity_hes_comp_by_group = rbind(
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_",
           cohort, "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23"),
  read_csv(here::here("output", "exploratory", paste0("ethnicity_HES_comp_by_group_",
           cohort, "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24")
)

#save as csv
write_csv(collated_ethnicity_hes_comp_tables, paste0(here::here("output",
          "collated", "descriptive"), "/", cohort,
          "_ethnicity_HES_comp_tables_collated.csv"))

write_csv(collated_ethnicity_hes_comp, paste0(here::here("output",
          "collated", "descriptive"), "/", cohort,
          "_ethnicity_HES_comp_collated.csv"))

write_csv(collated_ethnicity_hes_comp, paste0(here::here("output",
          "collated", "descriptive"), "/", cohort,
          "_ethnicity_HES_comp_by_group_collated.csv"))
