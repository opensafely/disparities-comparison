library(plyr)
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
fs::dir_create(here::here("output", "collated", "descriptive"))

##rates

# import rates table by cohort 
collated_rates_specific_sensitivity = rbind(
read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2017_2018_specific_sensitivity.csv")))
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity", 
             subset = "2017_18"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(codelist_type = "specific", investigation_type = "sensitivity",
             subset = "2018_19")
)

#save as csv
write_csv(collated_rates_specific_sensitivity, paste0(here::here("output",
          "collated", "descriptive"), "/", cohort,
          "_rates_sensitivity_collated.csv"))
