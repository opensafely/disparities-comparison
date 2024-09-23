library(tidyverse)
library(here)
library(arrow)
library(ggplot2)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "adults"
} else {
  cohort <- args[[1]]
}

## create output directories ----
fs::dir_create(here("output", "collated", "descriptive"))

##rates

# import rates table by cohort 
collated_rates_specific_sensitivity = rbind(
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2016_2017_specific_sensitivity.csv"))) 
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity", 
             subset = "2016_17"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2017_2018_specific_sensitivity.csv")))
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity", 
             subset = "2017_18"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity",
             subset = "2018_19"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2019_2020_specific_sensitivity.csv")))
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity",
             subset = "2019_20"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2020_2021_specific_sensitivity.csv"))) 
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity",
             subset = "2020_21"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2021_2022_specific_sensitivity.csv"))) 
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity",
             subset = "2021_22"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2022_2023_specific_sensitivity.csv"))) 
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity",
             subset = "2022_23")
)

#save as csv
write_csv(collated_rates_specific_sensitivity, paste0(here::here("output", "collated", "descriptive"), 
          "/", cohort, "_rates_specific_sensitivity_collated.csv"))
