library(plyr)
library(tidyverse)
library(here)
library(arrow)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "adults"
} else {
  cohort <- args[[1]]
}

## create output directories ----
fs::dir_create(here::here("output", "collated", "descriptive", "over_time"))

##rates over time

#rsv
collated_rates_over_time_rsv = rbind(
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_rsv", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_multiple_episodes, paste0(here::here("output",
          "collated", "descriptive", "over_time"), 
          "/", cohort, "_rates_over_time_all_rsv.csv"))

#flu
collated_rates_over_time_flu = rbind(
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_flu", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_multiple_episodes, paste0(here::here("output",
          "collated", "descriptive", "over_time"), 
          "/", cohort, "_rates_over_time_all_flu.csv"))

#covid
collated_rates_over_time_covid = rbind(
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_covid", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_multiple_episodes, paste0(here::here("output",
          "collated", "descriptive", "over_time"), 
          "/", cohort, "_rates_over_time_all_covid.csv"))

#overall
collated_rates_over_time_overall_resp = rbind(
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_overall_resp", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_overall_resp", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_overall_resp", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_overall_resp", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_overall_resp", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_overall_resp", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_overall_resp", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "rates", "weekly", "all",
           paste0("rates_over_time_overall_resp", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_multiple_episodes, paste0(here::here("output",
          "collated", "descriptive", "over_time"), 
          "/", cohort, "_rates_over_time_all_overall_resp.csv"))
