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

source(here::here("analysis", "functions", "redaction.R"))

## create output directories ----
fs::dir_create(here::here("output", "collated", "descriptive"))

##table 1
tab_names <- c(Characteristic = "**Characteristic**", N = "N", "%" = "%")

# import table 1 by cohort
collated_table1 = rbind(
  read_csv(here::here("output", "table1", paste0("secondary_table1_", cohort, 
           "_2017_2018.csv"))) %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2017_18")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = roundmid_any(N)),
  read_csv(here::here("output", "table1", paste0("secondary_table1_", cohort,
           "_2018_2019.csv"))) %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2018_19")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = roundmid_any(N)),
  read_csv(here::here("output", "table1", paste0("secondary_table1_", cohort, 
           "_2020_2021.csv")))  %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2020_21")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = roundmid_any(N))
)

#rename N column
colnames(collated_table1)[colnames(collated_table1) == "N"] <- "N (midpoint 10 rounded)"

#save as csv
write_csv(collated_table1, paste0(here::here("output", "collated", "descriptive"), 
          "/", cohort, "_secondary_table1_collated.csv"))
