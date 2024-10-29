library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(plyr)

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
collated_rates_sensitive_primary = rbind(
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2016_2017_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2016_17"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2017_2018_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2017_18"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2018_2019_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2018_19"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2019_2020_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2019_20"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2020_2021_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2020_21"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2021_2022_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2021_22"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2022_2023_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2022_23")
)

#perform rounding and redaction
collated_rates_specific_secondary <- collated_rates_specific_secondary %>%
  mutate(Events = round_any(Events, 5)) %>%
  mutate(Events = ifelse(Events <= 10, "<=10", Events),
         Rate = ifelse(Events == "<=10", "Redacted", Rate))

#rename events column
colnames(collated_rates_specific_secondary)[colnames(collated_rates_specific_secondary) == "Events"] <- "Events (rounded)"

#save as csv
write_csv(collated_rates_sensitive_primary, paste0(here::here("output", "collated", "descriptive"), 
          "/", cohort, "_rates_sensitive_primary_collated.csv"))
