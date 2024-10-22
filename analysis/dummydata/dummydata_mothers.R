##create a dummy dataset

library(tidyverse)
library(arrow)
library(here)
library(glue)
library(EnvStats)

#remotes::install_github("https://github.com/wjchulme/dd4d")
library(dd4d)

## create output directories ----
fs::dir_create(here("analysis", "dummydata"))

#define population size for dummy data
population_size <- 100000

#define index date and study start date
source(here("analysis", "design", "design.R"))
study_start_date <- as.Date(study_dates$season1_start_date)
study_end_date <- as.Date(study_dates$season1_end_date)
index_date <- study_start_date

#define index day and study start day
index_day <- 0L
study_start_day <- as.integer(study_start_date - index_date)
study_end_day <- as.integer(study_end_date - index_date)

#define known variables
known_variables <- c(
  "index_date",
  "index_day"
)

#define a list which will contain all of the variables to be simulated
sim_list = lst(
  
  ##maternal characteristics
  
  #id used to link mother and baby
  mother_id = bn_node(
    ~ as.integer(1:100000 + 100000),
    missing_rate = ~ 0.8
  ),
  
  #whether the mother is registered with the practice
  mother_registered = bn_node(
    ~ rbernoulli(n = ..n, p = 0.8)
  ),
  
  #date of deregistration
  mother_deregistration_day = bn_node(
    ~ as.integer(runif(n = ..n, index_day, index_day + 365)),
    missing_rate = ~ 0.99
  ),
  
  #age 
  maternal_age = bn_node(
    ~ as.integer(rnorm(n = ..n, mean = 30, sd = 5))
  ),
  
  #smoking status
  maternal_smoking_status = bn_node(
    ~ rfactor(n = ..n, levels = c(
      "Current", #smoker
      "Former", #ever-smoked
      "Never", #never smoked
      "Unknown" #missing
    ), p = c(0.1, 0.2, 0.7, 0))
  ),
  
  #drinking 
  maternal_drinking = bn_node(
    ~ rbernoulli(n = ..n, p = 0.05)
  ),
  
  #drug usage
  maternal_drug_usage = bn_node(
    ~ rbernoulli(n = ..n, p = 0.01)
  ),
  
  #flu vaccination
  maternal_flu_vaccination = bn_node(
    ~ rbernoulli(n = ..n, p = 0.4)
  ),
  
  #pertussis vaccination
  maternal_pertussis_vaccination = bn_node(
    ~ rbernoulli(n = ..n, p = 0.5)
  )
  
)

bn <- bn_create(sim_list, known_variables = known_variables)

set.seed(10)

dummydata <- bn_simulate(bn, pop_size = population_size, keep_all = FALSE, .id = "patient_id")

dummydata$patient_start_day <- study_start_day
dummydata$patient_end_day <- study_end_day

dummydata_processed <- dummydata %>%
  mutate(across(ends_with("_day"), ~ as.Date(as.character(index_date + .)))) %>%
  rename_with(~str_replace(., "_day", "_date"), ends_with("_day"))

dummydata_processed <- dummydata_processed %>%
  select(-patient_id) %>%
  mutate(patient_id = mother_id) %>%
  mutate(
    mother_id_present = if_else(is.na(patient_id), FALSE, TRUE),
    mother_registered = if_else(is.na(patient_id), NA, mother_registered),
    mother_deregistration_date = if_else(is.na(patient_id), NA_Date_, mother_deregistration_date),
    maternal_age = if_else(is.na(patient_id), NA_integer_, maternal_age),
    maternal_smoking_status = if_else(is.na(patient_id), NA_character_, maternal_smoking_status),
    maternal_drinking = if_else(is.na(patient_id), NA, maternal_drinking),
    maternal_drug_usage = if_else(is.na(patient_id), NA, maternal_drug_usage),
    maternal_flu_vaccination = if_else(is.na(patient_id), NA, maternal_flu_vaccination),
    maternal_pertussis_vaccination = if_else(is.na(patient_id), NA, maternal_pertussis_vaccination)
  )

#filter out infants without linkage
dummydata_processed <- dummydata_processed %>%
  filter(mother_id_present == TRUE)

fs::dir_create(here("analysis", "dummydata"))
write_feather(dummydata_processed, sink = here("analysis", "dummydata", 
              paste0("dummyextract_maternal_", year(study_start_date), "_",
                     year(study_end_date), ".arrow")))
