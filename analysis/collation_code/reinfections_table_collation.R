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

roundmid_any <- function(x, to=10){
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

## create output directories ----
fs::dir_create(here("output", "collated", "descriptive"))

##multiple outcomes

# import phenotype sensitivity by cohort
collated_reinfections = rbind(
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2016_2017.csv"))) %>% mutate(subset = "2016_17"),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2017_2018.csv"))) %>% mutate(subset = "2017_18"),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2018_2019.csv"))) %>% mutate(subset = "2018_19"),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2019_2020.csv"))) %>% mutate(subset = "2019_20"),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2020_2021.csv"))) %>% mutate(subset = "2020_21"),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2021_2022.csv"))) %>% mutate(subset = "2021_22"),
  read_csv(here::here("output", "exploratory", paste0("reinfections_", 
           cohort, "_2022_2023.csv"))) %>% mutate(subset = "2022_23"),
  read_csv(here::here("output", "exploratory", paste0("reinfections_",
           cohort, "_2023_2024.csv"))) %>% mutate(subset = "2023_24")
)

#perform redaction and rounding 
collated_reinfections <- collated_reinfections %>% 
  mutate_at(vars(contains("n")), ~roundmid_any(as.numeric(.)))

#rename n column
colnames(collated_reinfections)[colnames(collated_reinfections) == "n"] <- "n (midpoint 6 rounded)"

#save as csv
write_csv(collated_reinfections, paste0(here::here("output", "collated", "descriptive"), 
          "/", cohort, "_reinfections_collated.csv"))
