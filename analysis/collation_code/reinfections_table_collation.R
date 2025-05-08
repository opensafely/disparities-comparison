library(plyr)
library(tidyverse)
library(here)
library(arrow)
library(ggplot2)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "adults"
  codelist_type <- "specific"
} else {
  cohort <- args[[1]]
  codelist_type <- args[[2]]
}

source(here::here("analysis", "functions", "redaction.R"))

## create output directories ----
fs::dir_create(here::here("output", "collated", "descriptive"))

##multiple outcomes

# import phenotype sensitivity by cohort
collated_reinfections = rbind(
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2016_2017", "_", codelist_type, ".csv"))) %>%
    mutate(subset = "2016_17", codelist_type = codelist_type),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2017_2018", "_", codelist_type, ".csv"))) %>%
    mutate(subset = "2017_18", codelist_type = codelist_type),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2018_2019", "_", codelist_type, ".csv"))) %>%
    mutate(subset = "2018_19", codelist_type = codelist_type),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2019_2020", "_", codelist_type, ".csv"))) %>%
    mutate(subset = "2019_20", codelist_type = codelist_type),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2020_2021", "_", codelist_type, ".csv"))) %>%
    mutate(subset = "2020_21", codelist_type = codelist_type),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2021_2022", "_", codelist_type, ".csv"))) %>%
    mutate(subset = "2021_22", codelist_type = codelist_type),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2022_2023", "_", codelist_type, ".csv"))) %>%
    mutate(subset = "2022_23", codelist_type = codelist_type),
  read_csv(here::here("output", "exploratory", paste0("reinfections_",
           cohort, "_2023_2024", "_", codelist_type, ".csv"))) %>%
    mutate(subset = "2023_24", codelist_type = codelist_type),
)

#save as csv
write_csv(collated_reinfections, paste0(here::here("output", "collated",
          "descriptive"), "/", cohort, "_reinfections_", codelist_type,
          "_collated.csv"))
