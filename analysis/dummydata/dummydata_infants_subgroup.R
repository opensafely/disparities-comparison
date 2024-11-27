##create a dummy dataset

library(tidyverse)
library(arrow)
library(here)
library(glue)
library(EnvStats)

#remotes::install_github("https://github.com/wjchulme/dd4d")
library(dd4d)

## create output directories ----
fs::dir_create(here::here("analysis", "dummydata"))

#define population size for dummy data
population_size <- 100000

#define index date and study start date
source(here::here("analysis", "design", "design.R"))
study_start_date <- as.Date(study_dates$season1_start_date) #change depending on season you want data for 
study_end_date <- as.Date(study_dates$season1_end_date) #change depending on season you want data for
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

#define helper function to calculate size of household
calculate_household_size <- function(household_pseudo_id) {
  
  household_size <- as.data.frame(household_pseudo_id) %>%
    group_by(household_pseudo_id) %>%
    mutate(household_size = n()) 
  return(household_size$household_size)
  
}

#define a list which will contain all of the variables to be simulated
sim_list = lst(
  
  #patient specific index date
  patient_index_day = bn_node(
    ~ index_day
  ),
  
  #patient specific end date
  patient_end_day = bn_node(
    ~ as.integer(if_else(runif(n = ..n) < 0.02, 
                 as.integer(runif(n = ..n, index_day + 1, index_day + 365)), 
                 index_day + 365))
  ),
  
  #babys registration status
  registered = bn_node(
    ~ rbernoulli(n = ..n, p = 0.99)
  ),
  
  #babys deregistration
  deregistration_day = bn_node(
    ~ as.integer(runif(n = ..n, index_day, index_day + 365)),
    missing_rate = ~ 0.99
  ),
  
  #sex of the patient
  sex = bn_node(
    ~ rfactor(n = ..n, levels = c("female", "male", "intersex", "unknown"),
              p = c(0.51, 0.49, 0, 0))
  ),
  
  #age of the patient
  age = bn_node(
    ~ as.integer(rnormTrunc(n = ..n, mean = 12, sd = 4, min = 0, max = 23))
  ),
  
  #appropriate age for cohort
  is_appropriate_age = bn_node(
    ~ if_else(age >= 0 & age <= 23, TRUE, FALSE)
  ),
  
  #sustainability transformation partnership code (here a pseudocode just represented by a number)
  stp = bn_node(
    ~ factor(as.integer(runif(n = ..n, 1, 36)), levels = 1:36)
  ),
  
  #region the patient lives in
  region = bn_node(
    ~ rfactor(n = ..n, levels = c(
      "North East",
      "North West",
      "Yorkshire and The Humber",
      "East Midlands",
      "West Midlands",
      "East",
      "London",
      "South East",
      "South West"
    ), p = c(0.2, 0.2, 0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05))
  ),
  
  #practice ID
  practice_pseudo_id = bn_node(
    ~ as.integer(rnormTrunc(n = ..n, mean = 500, sd = 500, min = 0))
  ),
  
  #birth day - this will be the index date for maternal characteristics
  #rely on age to generate this
  birth_day = bn_node(
    ~ pmin(index_day, as.integer(runif(n = ..n, 
      min = index_day - age * 30.44 - 15, 
      max = index_day - age * 30.44 + 15)))
  ),
  
  #day of death for patient (want most to be alive)
  death_day = bn_node(
    ~ as.integer(runif(n = ..n, index_day, index_day + 2000)),
    missing_rate = ~ 0.999
  ),
  
  #rurality classification
  rural_urban_classification = bn_node(
    ~ as.integer(runif(n = ..n, min = 1, max = 8), missing_rate = ~ 0.001)
  ),
  
  #gestational age
  gestational_age = bn_node(
    ~ rnorm(n = ..n, mean = 38, sd = 2)
  ),
  
  ##exposures
  
  #index of multiple deprivation
  imd_rounded = bn_node(
    ~ as.integer(round(runif(n = ..n, min = 0, max = 32844), digits = -2))
  ),
  
  #ethnicity (group 6)
  latest_ethnicity_group = bn_node(
    ~ rfactor(n = ..n, levels = c(
      "5",
      "4",
      "3",
      "2",
      "1"
    ), p = c(0.02, 0.04, 0.1, 0.03, 0.81)),
    missing_rate = ~ 0.05
  ),
  
  #household ID (to determine composition)
  household_pseudo_id = bn_node(
    ~ as.integer(runif(n = ..n, min = 1, max = 30000))
  ),
  
  # number of people in household
  household_size = bn_node(
    ~ calculate_household_size(household_pseudo_id)
  ),
  
  ##maternal characteristics
  
  #id used to link mother and baby
  mother_id = bn_node(
    ~ as.integer(c(1:99000, 1:1000) + 100000),
    missing_rate = ~ 0.8
  ),
  
  #mothers registration
  mother_registered = bn_node(
    ~ if_else(!is.na(mother_id), rbernoulli(n = ..n, p = 0.8), NA)
  ),
  
  ##outcomes 
  
  #rsv primary care
  rsv_primary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.01)
  ),
  
  #date
  rsv_primary_day = bn_node(
    ~ if_else(rsv_primary == TRUE, 
              as.integer(runif(n = ..n, index_day, index_day + 365)), 
              NA_integer_)
  ),
  
  #rsv primary care - second episode
  rsv_primary_second = bn_node(
    ~ if_else(rsv_primary == TRUE,
              rbernoulli(n = ..n, p = 0.005), 
              NA_real_)
  ),
  
  #date
  rsv_primary_second_day = bn_node(
    ~ if_else(rsv_primary_second == TRUE, 
              as.integer(runif(n = ..n, rsv_primary_day + 14, index_day + 365)), 
              NA_integer_)
  ),
  
  #rsv secondary care
  rsv_secondary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.05)
  ),
  
  #date
  rsv_secondary_day = bn_node(
    ~ if_else(rsv_secondary == TRUE, 
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #rsv secondary length of stay
  rsv_los = bn_node(
    ~ if_else(rsv_secondary == TRUE,
              as.integer(rpois(n = ..n, lambda = 45)),
              NA_integer_)
  ),
  
  #rsv secondary care - second episode
  rsv_secondary_second = bn_node(
    ~ if_else(rsv_secondary == TRUE,
              rbernoulli(n = ..n, p = 0.01), 
              NA_integer_)
  ),
  
  #date
  rsv_secondary_second_day = bn_node(
    ~ if_else(rsv_secondary_second == TRUE, 
              as.integer(runif(n = ..n, rsv_secondary_day + 14, index_day + 365)), 
              NA_integer_)
  ),
  
  #rsv secondary length of stay - second episode
  rsv_los_second = bn_node(
    ~ if_else(rsv_secondary_second == TRUE,
              as.integer(rpois(n = ..n, lambda = 45)),
              NA_integer_)
  ),
  
  #flu primary care
  flu_primary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.01)
  ),
  
  #date
  flu_primary_day = bn_node(
    ~ if_else(flu_primary == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #flu primary care - second episode
  flu_primary_second = bn_node(
    ~ if_else(flu_primary == TRUE,
              rbernoulli(n = ..n, p = 0.005),
              NA_real_)
  ),
  
  #date
  flu_primary_second_day = bn_node(
    ~ if_else(flu_primary_second == TRUE,
              as.integer(runif(n = ..n, flu_primary_day + 14, index_day + 365)),
              NA_integer_)
  ),
  
  #flu secondary care
  flu_secondary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #date
  flu_secondary_day = bn_node(
    ~ if_else(flu_secondary == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #flu secondary length of stay
  flu_los = bn_node(
    ~ if_else(flu_secondary == TRUE,
              as.integer(rpois(n = ..n, lambda = 45)),
              NA_integer_)
  ),
  
  #flu secondary care - second episode
  flu_secondary_second = bn_node(
    ~ if_else(flu_secondary == TRUE,
              rbernoulli(n = ..n, p = 0.01),
              NA_real_)
  ),
  
  #date
  flu_secondary_second_day = bn_node(
    ~ if_else(flu_secondary_second == TRUE,
              as.integer(runif(n = ..n, flu_secondary_day + 14, index_day + 365)),
              NA_integer_)
  ),
  
  #flu secondary length of stay - second episode
  flu_los_second = bn_node(
    ~ if_else(flu_secondary_second == TRUE,
              as.integer(rpois(n = ..n, lambda = 45)),
              NA_integer_)
  ),
  
  #covid primary care
  covid_primary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.05)
  ),
  
  #date
  covid_primary_day = bn_node(
    ~ if_else(covid_primary == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #covid primary care - second episode
  covid_primary_second = bn_node(
    ~ if_else(covid_primary == TRUE,
              rbernoulli(n = ..n, p = 0.005),
              NA_real_)
  ),
  
  #date
  covid_primary_second_day = bn_node(
    ~ if_else(covid_primary_second == TRUE,
              as.integer(runif(n = ..n, covid_primary_day + 14, index_day + 365)),
              NA_integer_)
  ),
  
  #covid secondary care
  covid_secondary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #date
  covid_secondary_day = bn_node(
    ~ if_else(covid_secondary == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #covid secondary length of stay
  covid_los = bn_node(
    ~ if_else(covid_secondary == TRUE,
              as.integer(rpois(n = ..n, lambda = 45)),
              NA_integer_)
  ),
  
  #covid secondary care - second episode
  covid_secondary_second = bn_node(
    ~ if_else(covid_secondary == TRUE,
              rbernoulli(n = ..n, p = 0.01),
              NA_real_)
  ),
  
  #date
  covid_secondary_second_day = bn_node(
    ~ if_else(covid_secondary_second == TRUE,
              as.integer(runif(n = ..n, covid_secondary_day + 14, index_day + 365)),
              NA_integer_)
  ),
  
  #covid secondary length of stay - second episode
  covid_los_second = bn_node(
    ~ if_else(covid_secondary_second == TRUE,
              as.integer(rpois(n = ..n, lambda = 45)),
              NA_integer_)
  ),
  
  #unspecified respiratory infection primary care 
  overall_resp_primary = bn_node(
    ~ if_else(rsv_primary == 1| flu_primary == 1| covid_primary == 1, 1, 
              rbernoulli(n = ..n, p = 0.05))
  ),
  
  #date
  overall_resp_primary_day = bn_node(
    ~ if_else(overall_resp_primary == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #unspecified respiratory infection primary care - second episode
  overall_resp_primary_second = bn_node(
    ~ if_else(rsv_primary_second == 1| flu_primary_second == 1| covid_primary_second == 1,
              TRUE, if_else(overall_resp_primary == TRUE, rbernoulli(n = ..n, p = 0.01),
                            NA_real_))
  ),
  
  #date
  overall_resp_primary_second_day = bn_node(
    ~ if_else(overall_resp_primary_second == TRUE,
              as.integer(runif(n = ..n, overall_resp_primary_day + 14, index_day + 365)),
              NA_integer_)
  ),
  
  #unspecified respiratory infection secondary care 
  overall_resp_secondary = bn_node(
    ~ if_else(rsv_secondary == 1| flu_secondary == 1| covid_secondary == 1,
              TRUE, rbernoulli(n = ..n, p = 0.05))
  ),
  
  #date
  overall_resp_secondary_day = bn_node(
    ~ if_else(overall_resp_secondary == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #unspecified respiratory infection secondary length of stay
  overall_resp_los = bn_node(
    ~ if_else(overall_resp_secondary == TRUE,
              as.integer(rpois(n = ..n, lambda = 45)),
              NA_integer_)
  ),
  
  #unspecified respiratory infection secondary care - second episode
  overall_resp_secondary_second = bn_node(
    ~ if_else(rsv_secondary_second == 1| flu_secondary_second == 1| covid_secondary_second == 1,
              TRUE, if_else(overall_resp_secondary == TRUE, rbernoulli(n = ..n, p = 0.01),
                            NA_real_))
  ),
  
  #date
  overall_resp_secondary_second_day = bn_node(
    ~ if_else(overall_resp_secondary_second == TRUE,
              as.integer(runif(n = ..n, overall_resp_secondary_day + 14, index_day + 365)),
              NA_integer_)
  ),
  
  #unspecified respiratory infection secondary length of stay - second episode
  overall_resp_los_second = bn_node(
    ~ if_else(overall_resp_secondary_second == TRUE,
              as.integer(rpois(n = ..n, lambda = 45)),
              NA_integer_)
  ),
  
  ##mortality outcomes
  
  #rsv mortality
  rsv_mortality = bn_node(
    ~ if_else(!is.na(death_day), rbernoulli(n = ..n, p = 0.1), NA_real_)
  ),
  
  #date
  rsv_mortality_day = bn_node(
    ~ if_else(rsv_mortality == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #flu mortality
  flu_mortality = bn_node(
    ~ if_else(!is.na(death_day), rbernoulli(n = ..n, p = 0.1), NA_real_)
  ),
  
  #date
  flu_mortality_day = bn_node(
    ~ if_else(flu_mortality == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #covid mortality
  covid_mortality = bn_node(
    ~ if_else(!is.na(death_day), rbernoulli(n = ..n, p = 0.1), NA_real_)
  ),
  
  #date
  covid_mortality_day = bn_node(
    ~ if_else(covid_mortality == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #unspecified respiratory infection secondary care
  overall_resp_mortality = bn_node(
    ~ if_else(!is.na(death_day), if_else(rsv_mortality == 1| 
                                           flu_mortality == 1| covid_mortality == 1, TRUE, 
                                         rbernoulli(n = ..n, p = 0.2)), NA_real_)
  ),
  
  #date
  overall_resp_mortality_day = bn_node(
    ~ if_else(overall_resp_mortality == TRUE,
              as.integer(runif(n = ..n, index_day, index_day + 365)),
              NA_integer_)
  ),
  
  #all cause mortality
  all_cause_mortality = bn_node(
    ~ if_else(death_day <= index_day + 365, TRUE, FALSE)
  ),
  
  #date
  all_cause_mortality_day = bn_node(
    ~ if_else(death_day <= index_day + 365, death_day, NA_integer_)
  ),
  
  ##exclusion criteria
  
  #part of risk group
  risk_group_infants = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #severe immunodeficiency
  severe_immunodeficiency = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #care home resident
  care_home = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
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

dummydata_processed <- dummydata_processed %>%
  mutate(mother_id_present = if_else(is.na(mother_id), FALSE, TRUE))

#filter out infants without linkage
dummydata_processed <- dummydata_processed %>%
  filter(mother_id_present == TRUE)

fs::dir_create(here::here("analysis", "dummydata"))
write_feather(dummydata_processed, sink = here::here("analysis", "dummydata", 
  paste0("dummyextract_infants_subgroup_", year(study_start_date), "_", year(study_end_date), ".arrow")))
