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

##multiple outcomes

# import phenotype sensitivity by cohort
collated_multiple_outcomes = rbind(
  read_csv(here::here("output", "exploratory", paste0("multiple_outcomes_", 
           cohort, "_2016_2017.csv"))) %>% mutate(subset = "2016_17"),
  read_csv(here::here("output", "exploratory", paste0("multiple_outcomes_", 
           cohort, "_2017_2018.csv"))) %>% mutate(subset = "2017_18"),
  read_csv(here::here("output", "exploratory", paste0("multiple_outcomes_", 
           cohort, "_2018_2019.csv"))) %>% mutate(subset = "2018_19"),
  read_csv(here::here("output", "exploratory", paste0("multiple_outcomes_", 
           cohort, "_2019_2020.csv"))) %>% mutate(subset = "2019_20"),
  read_csv(here::here("output", "exploratory", paste0("multiple_outcomes_", 
           cohort, "_2020_2021.csv"))) %>% mutate(subset = "2020_21"),
  read_csv(here::here("output", "exploratory", paste0("multiple_outcomes_", 
           cohort, "_2021_2022.csv"))) %>% mutate(subset = "2021_22"),
  read_csv(here::here("output", "exploratory", paste0("multiple_outcomes_", 
           cohort, "_2022_2023.csv"))) %>% mutate(subset = "2022_23")
)

#perform redaction and rounding 
collated_multiple_outcomes <- collated_multiple_outcomes %>% 
  mutate_at(vars(contains("n")), ~round_any(as.numeric(.), 5)) %>% 
  mutate_at(vars(contains("n")), ~ifelse(. <= 10, "<=10", .))

#rename n column
colnames(collated_multiple_outcomes)[colnames(collated_multiple_outcomes) == "n"] <- "n (rounded)"

#save as csv
write_csv(collated_multiple_outcomes, paste0(here::here("output", "collated", "descriptive"), 
          "/", cohort, "_multiple_outcomes_collated.csv"))
