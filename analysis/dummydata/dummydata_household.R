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

#define helper function to calculate household ID based on household size
calculate_household_id <- function(household_sizes) {
  
  #initialise a vector to store the household IDs
  household_ids <- integer(population_size)
  
  #define position in the vector to assign the next ID
  position <- 1
  
  #loop through the household sizes
  for (i in seq_along(household_sizes)) {
    #assign the household ID to the vector
    household_ids[position:(position + household_sizes[i] - 1)] <- i
    
    #update the position
    position <- position + household_sizes[i]
  }
  household_ids <- household_ids[sample(length(household_sizes))]
  return(household_ids)
}


#define a list which will contain all of the variables to be simulated
sim_list = lst(
  
  #whether the patient is registered with the practice
  registered = bn_node(
    ~ rbernoulli(n = ..n, p = 0.99),
  ),
  
  #date of deregistration
  deregistration_day = bn_node(
    ~ as.integer(runif(n = ..n, index_day, index_day + 365)),
    missing_rate = ~ 0.99
  ),
  
  #age of the patient
  age = bn_node(
    ~ as.integer(rnormTrunc(n = ..n, mean = 60, sd = 14, min = 0, max = 120)), 
    missing_rate = ~ 0.001
  ),
  
  #rurality classification
  rural_urban_classification = bn_node(
    ~ as.integer(runif(n = ..n, min = 1, max = 8), missing_rate = ~ 0.001)
  ),
  
  # number of people in household
  household_size = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 3))
  ),
  
  #household ID (to determine composition) - sample based on size of household
  household_pseudo_id = bn_node(
    ~ calculate_household_id(household_size)
  )
  
)

bn <- bn_create(sim_list, known_variables = known_variables)

bn_plot(bn)
bn_plot(bn, connected_only = TRUE)

set.seed(10)

dummydata <- bn_simulate(bn, pop_size = population_size, keep_all = FALSE, .id = "patient_id")

dummydata$patient_start_day <- study_start_day
dummydata$patient_end_day <- study_end_day

dummydata_processed <- dummydata %>%
  mutate(across(ends_with("_day"), ~ as.Date(as.character(index_date + .)))) %>%
  rename_with(~str_replace(., "_day", "_date"), ends_with("_day"))

fs::dir_create(here("analysis", "dummydata"))
write_feather(dummydata_processed, sink = here("analysis", "dummydata", 
  paste0("dummyextract_household_", year(study_start_date), "_", year(study_end_date), ".arrow")))
