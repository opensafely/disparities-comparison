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

##flowchart

# import flow chart info by cohort
collated_flowchart = rbind(
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

#save as csv
write_csv(collated_flowchart, paste0(here::here("output", "flowchart"), "/", 
          "flowchart_collated", cohort, "_", year(study_start_date), "_",
          year(study_end_date),".csv"))

##table 1

# import table 1 by cohort
collated_table1 = rbind(
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2016_2017.csv"))) %>% mutate(subset = "2016_17"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2017_2018.csv"))) %>% mutate(subset = "2017_18"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort,
                      "_2018_2019.csv"))) %>% mutate(subset = "2018_19"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2019_2020.csv"))) %>% mutate(subset = "2019_20"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2020_2021.csv"))) %>% mutate(subset = "2020_21"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2021_2022.csv"))) %>% mutate(subset = "2021_22"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2022_2023.csv"))) %>% mutate(subset = "2022_23")
)

## create output directories ----
fs::dir_create(here("output", "table1"))

#save as csv
write_csv(collated_table1, paste0(here::here("output", "table1"), "/", 
          "table1_collated", cohort, "_", year(study_start_date), "_", 
          year(study_end_date),".csv"))

##rates

# import results table by cohort
collated_results1 = rbind(
  read_csv(here::here("output", "results", paste0("results1_", cohort, 
                      "_2016_2017.csv"))) %>% mutate(subset = "2016_17"),
  read_csv(here::here("output", "results", paste0("results1_", cohort, 
                      "_2017_2018.csv"))) %>% mutate(subset = "2017_18"),
  read_csv(here::here("output", "results", paste0("results1_", cohort, 
                      "_2018_2019.csv"))) %>% mutate(subset = "2018_19"),
  read_csv(here::here("output", "results", paste0("results1_", cohort, 
                      "_2019_2020.csv"))) %>% mutate(subset = "2019_20"),
  read_csv(here::here("output", "results", paste0("results1_", cohort, 
                      "_2020_2021.csv"))) %>% mutate(subset = "2020_21"),
  read_csv(here::here("output", "results", paste0("results1_", cohort, 
                      "_2021_2022.csv"))) %>% mutate(subset = "2021_22"),
  read_csv(here::here("output", "results", paste0("results1_", cohort, 
                      "_2022_2023.csv"))) %>% mutate(subset = "2022_23")
)

## create output directories ----
fs::dir_create(here("output", "results"))

#save as csv
write_csv(collated_results1, paste0(here::here("output", "results"), "/", 
                                  "results1_collated", cohort, "_", year(study_start_date), "_", 
                                  year(study_end_date),".csv"))

##model outputs

# import results table by cohort
collated_model_outputs = rbind(
  read_csv(here::here("output", "results", paste0("model_outputs_", cohort, 
                                                  "_2016_2017.csv"))) %>% mutate(subset = "2016_17"),
  read_csv(here::here("output", "results", paste0("model_outputs_", cohort, 
                                                  "_2017_2018.csv"))) %>% mutate(subset = "2017_18"),
  read_csv(here::here("output", "results", paste0("model_outputs_", cohort, 
                                                  "_2018_2019.csv"))) %>% mutate(subset = "2018_19"),
  read_csv(here::here("output", "results", paste0("model_outputs_", cohort, 
                                                  "_2019_2020.csv"))) %>% mutate(subset = "2019_20"),
  read_csv(here::here("output", "results", paste0("model_outputs_", cohort, 
                                                  "_2020_2021.csv"))) %>% mutate(subset = "2020_21"),
  read_csv(here::here("output", "results", paste0("model_outputs_", cohort, 
                                                  "_2021_2022.csv"))) %>% mutate(subset = "2021_22"),
  read_csv(here::here("output", "results", paste0("model_outputs_", cohort, 
                                                  "_2022_2023.csv"))) %>% mutate(subset = "2022_23")
)

## create output directories ----
fs::dir_create(here("output", "results"))

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results"), "/", 
                                    "model_outputs_collated", cohort, "_", year(study_start_date), "_", 
                                    year(study_end_date),".csv"))
