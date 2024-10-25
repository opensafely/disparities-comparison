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
collated_rates_specific_secondary = rbind(
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2017_2018_specific_secondary.csv")))
  %>% mutate(codelist_type = "specific", investigation_type = "secondary",
             subset = "2017_18"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2018_2019_specific_secondary.csv")))
  %>% mutate(codelist_type = "specific", investigation_type = "secondary",
             subset = "2018_19"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
           "_2020_2021_specific_secondary.csv")))
  %>% mutate(codelist_type = "specific", investigation_type = "secondary",
             subset = "2020_21")
)

#redact events less than or equal to 7, if there are <=7 events, redact rate
collated_rates_specific_secondary <- collated_rates_specific_secondary %>%
  mutate(Events = ifelse(Events <= 7, "<=7", Events),
         Rate = ifelse(Events == "<=7", "Redacted", Rate))

#save as csv
write_csv(collated_rates_specific_secondary, paste0(here::here("output", "collated", "descriptive"), 
          "/", cohort, "_rates_specific_secondary_collated.csv"))
