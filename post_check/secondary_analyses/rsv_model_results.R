library(tidyverse)
library(here)
library(arrow)
library(broom)
library(broom.helpers)
library(lubridate)
library(cowplot)
library(stringr)

## create output directories ----
fs::dir_create(here("post_check", "secondary_analyses"))

cohort <- "older_adults"
pathogen <- "rsv"
investigation_type <- "secondary"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "analytic", "secondary", paste0(cohort, "_", pathogen,
                     "_model_outputs_collated_secondary.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2017_2018_specific_secondary.arrow"))) 

#import plot function
source(here::here("post_check", "functions", "forest.R"))

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

#too few events for all models
