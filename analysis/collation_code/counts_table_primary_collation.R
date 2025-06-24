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

##counts over time

#rsv
collated_counts_over_time_all_monthly_rsv = rbind(
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_rsv", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_counts_over_time_all_monthly_rsv, paste0(here::here(
          "output", "collated", "descriptive", "over_time"), 
          "/", cohort, "_counts_over_time_all_monthly_rsv.csv"))

#flu
collated_counts_over_time_all_monthly_flu = rbind(
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_flu", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_counts_over_time_all_monthly_flu, paste0(here::here(
          "output", "collated", "descriptive", "over_time"), 
          "/", cohort, "_counts_over_time_all_monthly_flu.csv"))

#covid
collated_counts_over_time_all_monthly_covid = rbind(
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_covid", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_counts_over_time_all_monthly_covid, paste0(here::here(
          "output", "collated", "descriptive", "over_time"), 
          "/", cohort, "_counts_over_time_all_monthly_covid.csv"))

#overall
collated_counts_over_time_all_monthly_overall_resp = rbind(
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_overall_resp", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_overall_resp", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_overall_resp", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_overall_resp", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_overall_resp", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_overall_resp", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_overall_resp", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_monthly_overall_resp", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_counts_over_time_all_monthly_overall_resp, paste0(here::here(
          "output", "collated", "descriptive", "over_time"), 
          "/", cohort, "_counts_over_time_all_monthly_overall_resp.csv"))

##weekly

#rsv
collated_counts_over_time_all_weekly_rsv = rbind(
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_rsv", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_counts_over_time_all_weekly_rsv, paste0(here::here(
          "output", "collated", "descriptive", "over_time"), 
          "/", cohort, "_counts_over_time_all_weekly_rsv.csv"))

#flu
collated_counts_over_time_all_weekly_flu = rbind(
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2016_2017_specific_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2017_2018_specific_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2018_2019_specific_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_flu", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_counts_over_time_all_weekly_flu, paste0(here::here(
          "output", "collated", "descriptive", "over_time"), 
          "/", cohort, "_counts_over_time_all_weekly_flu.csv"))

#covid
collated_counts_over_time_all_weekly_covid = rbind(
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2019_2020_specific_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2020_2021_specific_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2021_2022_specific_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2022_2023_specific_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2023_2024_specific_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "specific"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_covid", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_counts_over_time_all_weekly_covid, paste0(here::here(
          "output", "collated", "descriptive", "over_time"), 
          "/", cohort, "_counts_over_time_all_weekly_covid.csv"))

#overall
collated_counts_over_time_all_weekly_overall_resp = rbind(
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_overall_resp", "_", cohort,
           "_2016_2017_sensitive_primary.csv"))) %>%
    mutate(subset = "2016_17", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_overall_resp", "_", cohort,
           "_2017_2018_sensitive_primary.csv"))) %>%
    mutate(subset = "2017_18", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_overall_resp", "_", cohort,
           "_2018_2019_sensitive_primary.csv"))) %>%
    mutate(subset = "2018_19", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_overall_resp", "_", cohort,
           "_2019_2020_sensitive_primary.csv"))) %>%
    mutate(subset = "2019_20", codelist_type = "sensitive"),
  read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_overall_resp", "_", cohort,
           "_2020_2021_sensitive_primary.csv"))) %>%
    mutate(subset = "2020_21", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_overall_resp", "_", cohort,
           "_2021_2022_sensitive_primary.csv"))) %>%
    mutate(subset = "2021_22", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_overall_resp", "_", cohort,
           "_2022_2023_sensitive_primary.csv"))) %>%
    mutate(subset = "2022_23", codelist_type = "sensitive"),
    read_csv(here::here("output", "results", "counts",
           paste0("counts_over_time_all_weekly_overall_resp", "_", cohort,
           "_2023_2024_sensitive_primary.csv"))) %>%
    mutate(subset = "2023_24", codelist_type = "sensitive")
)

#save as csv
write_csv(collated_counts_over_time_all_weekly_overall_resp, paste0(here::here(
          "output", "collated", "descriptive", "over_time"), 
          "/", cohort, "_counts_over_time_all_weekly_overall_resp.csv"))
