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

#define helper function to calculate size of household
calculate_household_size <- function(household_pseudo_id) {
  
  household_size <- as.data.frame(household_pseudo_id) %>%
    group_by(household_pseudo_id) %>%
    mutate(household_size = n()) 
  return(household_size$household_size)
  
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
  
  #sex of the patient
  sex = bn_node(
    ~ rfactor(n = ..n, levels = c("female", "male", "intersex", "unknown"),
              p = c(0.51, 0.49, 0, 0))
  ),
  
  #age of the patient
  age = bn_node(
    ~ as.integer(rnormTrunc(n = ..n, mean = 60, sd = 14, min = 5, max = 17)), 
    missing_rate = ~ 0.001
  ),
  
  #sustainability transformation partnership code (here a pseudocode just represented by a number)
  stp = bn_node(
    ~ factor(as.integer(runif(n = ..n, 1, 36)), levels = 1:36),
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
    ), p = c(0.2, 0.2, 0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)),
  ),
  
  #practice ID
  practice_pseudo_id = bn_node(
    ~ as.integer(rnormTrunc(n = ..n, mean = 500, sd = 500, min = 0))
  ),
  
  #day of death for patient (want most to be alive)
  death_day = bn_node(
    ~ as.integer(runif(n = ..n, index_day, index_day + 2000)),
    missing_rate = ~ 0.99
  ),
  
  #rurality classification
  rural_urban_classification = bn_node(
    ~ as.integer(runif(n = ..n, min = 1, max = 8), missing_rate = ~ 0.001)
  ),
  
  ##exposures
  
  #index of multiple deprivation
  imd_rounded = bn_node(
    ~ as.integer(round(runif(n = ..n, min = 0, max = 32844), digits = -2)),
    missing_rate = ~ 0.05
  ),
  
  #ethnicity (group 6)
  latest_ethnicity_code = bn_node(
    ~ rfactor(n = ..n, levels = c(
      "1",
      "2",
      "3",
      "4",
      "5",
      "6"
    ), p = c(0.81, 0.03, 0.1, 0.04, 0.02, 0.05))
  ),
  
  #household ID (to determine composition)
  household_pseudo_id = bn_node(
    ~ as.integer(runif(n = ..n, min = 1, max = 30000))
  ),
  
  # number of people in household
  household_size = bn_node(
    ~ calculate_household_size(household_pseudo_id)
  ),
  
  ##comorbidities
  
  #has asthma 
  has_asthma_reactive_airway = bn_node(
    ~ rbernoulli(n = ..n, p = 0.15)
  ),

  #prior flu vaccination
  prior_flu_vaccination = bn_node(
    ~ rbernoulli(n = ..n, p = 0.75)
  ),
  
  #current flu vaccination
  flu_vaccination = bn_node(
    ~rbernoulli(n = ..n, p = 0.75)
  ),
  
  #date of last covid vaccination
  last_covid_vaccination_day = bn_node(
    ~ as.integer(runif(n = ..n, index_day, index_day + 365))
  ),
  
  #current covid vaccination
  covid_vaccination = bn_node(
    ~ rbernoulli(n = ..n, p = 0.5)
  ),
  
  ##outcomes 
  
  #rsv primary care
  rsv_primary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.01)
  ),
  
  #date
  rsv_primary_day = bn_node(
    ~ ifelse(rsv_primary == TRUE, 
             as.integer(runif(n = ..n, index_day, index_day + 365)), 
             NA)
  ),
  
  #number of cases
  rsv_primary_cases = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 1))
  ),
  
  #rsv secondary care
  rsv_secondary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.05)
  ),
  
  #date
  rsv_secondary_day = bn_node(
    ~ ifelse(rsv_secondary == TRUE, 
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #rsv secondary length of stay
  rsv_los = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 45))
  ),
  
  #number of cases
  rsv_secondary_cases = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 1))
  ),
  
  #flu primary care
  flu_primary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.01)
  ),
  
  #date
  flu_primary_day = bn_node(
    ~ ifelse(flu_primary == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #number of cases
  flu_primary_cases = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 1))
  ),
  
  #flu secondary care
  flu_secondary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #date
  flu_secondary_day = bn_node(
    ~ ifelse(flu_secondary == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #flu secondary length of stay
  flu_los = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 45))
  ),
  
  #number of cases
  flu_secondary_cases = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 1))
  ),
  
  #covid primary care
  covid_primary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.05)
  ),
  
  #date
  covid_primary_day = bn_node(
    ~ ifelse(covid_primary == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #number of cases
  covid_primary_cases = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 1))
  ),
  
  #covid secondary care
  covid_secondary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #date
  covid_secondary_day = bn_node(
    ~ ifelse(covid_secondary == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #covid secondary length of stay
  covid_los = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 45))
  ),
  
  #number of cases
  covid_secondary_cases = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 1))
  ),
  
  #unspecified respiratory infection primary care 
  overall_resp_primary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.3)
  ),
  
  #date
  overall_resp_primary_day = bn_node(
    ~ ifelse(overall_resp_primary == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #number of cases
  overall_resp_primary_cases = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 1))
  ),
  
  #unspecified respiratory infection secondary care 
  overall_resp_secondary = bn_node(
    ~ rbernoulli(n = ..n, p = 0.25)
  ),
  
  #date
  overall_resp_secondary_day = bn_node(
    ~ ifelse(overall_resp_secondary == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #unspecified respiratory infection secondary length of stay
  overall_resp_los = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 45))
  ),
  
  #number of cases
  overall_resp_secondary_cases = bn_node(
    ~ as.integer(rpois(n = ..n, lambda = 1))
  ),
  
  ##mortality outcomes
  
  #rsv mortality
  rsv_mortality = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #date
  rsv_mortality_day = bn_node(
    ~ ifelse(rsv_mortality == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #flu mortality
  flu_mortality = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #date
  flu_mortality_day = bn_node(
    ~ ifelse(flu_mortality == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #covid mortality
  covid_mortality = bn_node(
    ~ rbernoulli(n = ..n, p = 0.1)
  ),
  
  #date
  covid_mortality_day = bn_node(
    ~ ifelse(covid_mortality == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #unspecified respiratory infection secondary care
  overall_resp_mortality = bn_node(
    ~ rbernoulli(n = ..n, p = 0.2)
  ),
  
  #date
  overall_resp_mortality_day = bn_node(
    ~ ifelse(overall_resp_mortality == TRUE,
             as.integer(runif(n = ..n, index_day, index_day + 365)),
             NA)
  ),
  
  #all cause mortality
  all_cause_mortality = bn_node(
    ~ ifelse(death_day <= index_day + 365, TRUE, FALSE)
  ),
  
  #date
  all_cause_mortality_day = bn_node(
    ~ ifelse(death_day <= index_day + 365, death_day, NA)
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

fs::dir_create(here("analysis", "dummydata"))
write_feather(dummydata_processed, sink = here("analysis", "dummydata", 
  paste0("dummyextract_children_and_adolescents_", year(study_start_date), "_", year(study_end_date), ".arrow")))
