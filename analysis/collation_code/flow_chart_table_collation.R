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

##flowchart

# import flow chart info by cohort
collated_flow_chart = rbind(
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2016_2017.csv"))) %>% mutate(subset = "2016_17"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2017_2018.csv"))) %>% mutate(subset = "2017_18"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2018_2019.csv"))) %>% mutate(subset = "2018_19"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2019_2020.csv"))) %>% mutate(subset = "2019_20"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2020_2021.csv"))) %>% mutate(subset = "2020_21"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2021_2022.csv"))) %>% mutate(subset = "2021_22"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2022_2023.csv"))) %>% mutate(subset = "2022_23")
)

#redact counts 7 and below
collated_flow_chart <- collated_flow_chart %>% 
  mutate_at(vars(ends_with("count")), ~ ifelse(. <= 7, "<=7", .))

#save as csv
write_csv(collated_flow_chart, paste0(here::here("output", "collated", "descriptive"),
          "/", cohort, "_flow_chart_collated.csv"))
