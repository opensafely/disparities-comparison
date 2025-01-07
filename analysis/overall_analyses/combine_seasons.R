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

source(here::here("analysis", "functions", "redaction.R"))

## create output directories ----
fs::dir_create(here::here("analysis", "overall_analyses"))

##multiple outcomes

# import phenotype sensitivity by cohort
collated_input_processed = rbind(
  read_feather(here::here("output", "data", paste0("input_processed_", cohort,
               "_2016_2017_specific_primary.arrow"))) %>% mutate(subset = "2016_17"),
  read_feather(here::here("output", "data", paste0("input_processed_", cohort,
               "_2017_2018_specific_primary.arrow"))) %>% mutate(subset = "2017_18"),
  read_feather(here::here("output", "data", paste0("input_processed_", cohort,
               "_2018_2019_specific_primary.arrow"))) %>% mutate(subset = "2018_19"),
  read_feather(here::here("output", "data", paste0("input_processed_", cohort,
               "_2019_2020_specific_primary.arrow"))) %>% mutate(subset = "2019_20"),
  read_feather(here::here("output", "data", paste0("input_processed_", cohort,
               "_2020_2021_specific_primary.arrow"))) %>% mutate(subset = "2020_21"),
  read_feather(here::here("output", "data", paste0("input_processed_", cohort,
               "_2021_2022_specific_primary.arrow"))) %>% mutate(subset = "2021_22"),
  read_feather(here::here("output", "data", paste0("input_processed_", cohort,
               "_2022_2023_specific_primary.arrow"))) %>% mutate(subset = "2022_23"),
  read_feather(here::here("output", "data", paste0("input_processed_", cohort,
               "_2023_2024_specific_primary.arrow"))) %>% mutate(subset = "2023_24"),
)

## create output directories ----
fs::dir_create(here::here("data", "overall_analyses"))

#save as csv
write_feather(collated_input_processed, paste0(here::here("output", "data",
              "overall_analyses"), "/", "input_processed_combined_", cohort,
              "_specific_primary.arrow"))
