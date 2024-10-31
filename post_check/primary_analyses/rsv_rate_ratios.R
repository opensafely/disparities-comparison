library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here("post_check", "primary_analyses"))

cohort <- "older_adults"
pathogen <- "rsv"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs 
                     paste0(cohort, pathogen, "model_outputs_collated.csv")))