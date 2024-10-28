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
fs::dir_create(here("output", "collated", "descriptive"))

##table 1
tab_names <- c(Characteristic = "**Characteristic**", N = "N", "%" = "%")

# import table 1 by cohort
collated_table1 = rbind(
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
           "_2016_2017.csv"))) %>% 
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2016_17")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = round_any(N, 5)) %>%
    mutate(N = ifelse(N <= 10, "<=10", N)) %>%
    mutate(`%` = ifelse(N == "<=10", "Redacted", `%`)),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
           "_2017_2018.csv"))) %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2017_18")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = round_any(N, 5)) %>%
    mutate(N = ifelse(N <= 10, "<=10", N)) %>%
    mutate(`%` = ifelse(N == "<=10", "Redacted", `%`)),
  read_csv(here::here("output", "table1", paste0("table1_", cohort,
           "_2018_2019.csv"))) %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2018_19")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = round_any(N, 5)) %>%
    mutate(N = ifelse(N <= 10, "<=10", N)) %>%
    mutate(`%` = ifelse(N == "<=10", "Redacted", `%`)),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
           "_2019_2020.csv")))  %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2019_20")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = round_any(N, 5)) %>%
    mutate(N = ifelse(N <= 10, "<=10", N)) %>%
    mutate(`%` = ifelse(N == "<=10", "Redacted", `%`)),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
           "_2020_2021.csv")))  %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2020_21")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = round_any(N, 5)) %>%
    mutate(N = ifelse(N <= 10, "<=10", N)) %>%
    mutate(`%` = ifelse(N == "<=10", "Redacted", `%`)),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
           "_2021_2022.csv")))  %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2021_22")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = round_any(N, 5)) %>%
    mutate(N = ifelse(N <= 10, "<=10", N)) %>%
    mutate(`%` = ifelse(N == "<=10", "Redacted", `%`)),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
           "_2022_2023.csv")))  %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2022_23")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = round_any(N, 5)) %>%
    mutate(N = ifelse(N <= 10, "<=10", N)) %>%
    mutate(`%` = ifelse(N == "<=10", "Redacted", `%`)),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
           "_2023_2024.csv")))  %>%     
    rename(all_of(tab_names)) %>%
    mutate(N = if_else(row_number() == 1, pull(.[2, 2]), N),
           `%` = if_else(row_number() == 1, "100%", `%`)) %>%
    slice(-2) %>% 
    mutate(subset = "2023_24")  %>% 
    mutate(N = as.numeric(N)) %>%
    mutate(N = round_any(N, 5)) %>%
    mutate(N = ifelse(N <= 10, "<=10", N)) %>%
    mutate(`%` = ifelse(N == "<=10", "Redacted", `%`))
)

#rename N column
colnames(collated_table1)[colnames(collated_table1) == "N"] <- "N (rounded)"

#save as csv
write_csv(collated_table1, paste0(here::here("output", "collated", "descriptive"), 
          "/", cohort, "_table1_collated.csv"))
