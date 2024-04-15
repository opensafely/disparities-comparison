library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(gtsummary)
library(survival)

## create output directories ----
fs::dir_create(here("analysis"))

#import redaction functions
#source(here("analysis", "functions", "redaction.R"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "adults"
  codelist_type <- "specific"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}
covid_season_min <- as.Date("2019-09-01")

# roundmid_any <- function(x, to=6){
#   # like round_any, but centers on (integer) midpoint of the rounding points
#   ceiling(x/to)*to - (floor(to/2)*(x!=0))
# }

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

## create output directories ----
fs::dir_create(here("output", "models"))

lab <- ifelse(cohort == "infants", "Age (Months)",
       ifelse(cohort == "infants_subgroup", "Age (Months)", "Age (Years)"))

plot_age <- ggplot(data = df_input, aes(age, frequency(age))) + geom_col(width = 0.9) +
  xlab(lab) + ylab("Frequency")

ggsave(
  plot = plot_age,
  filename = paste0("descriptive_", cohort, "_", year(study_start_date),
    "_", year(study_end_date), "_", codelist_type, "_",
    investigation_type,".png"), path = here::here("output", "models"),
)

#calculate person time for rsv mild outcomes
py_rsv_primary <- pyears(time_rsv_primary ~ rsv_primary_inf, data = df_input, data.frame = T)[["data"]]
py_1000_rsv_primary <- py_rsv_primary %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
rate_rsv_mild <- py_1000_rsv_primary[py_1000_rsv_primary$rsv_primary_inf == 1,]$rsv_mild_rate

#calculate person time for rsv severe outcomes
py_rsv_secondary <- pyears(time_rsv_secondary ~ rsv_secondary_inf, data = df_input, data.frame = T)[["data"]]
py_1000_rsv_secondary <- py_rsv_secondary %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
rate_rsv_severe <- py_1000_rsv_secondary[py_1000_rsv_secondary$rsv_secondary_inf == 1,]$rsv_severe_rate

#calculate person time for rsv mortality
py_rsv_mortality <- pyears(time_rsv_mortality ~ rsv_mortality_inf, data = df_input, data.frame = T)[["data"]]
py_1000_rsv_mortality <- py_rsv_mortality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
rate_rsv_mortality <- py_1000_rsv_mortality[py_1000_rsv_mortality$rsv_mortality_inf == 1,]$rsv_mortality_rate

#calculate person time for flu mild outcomes
py_flu_primary <- pyears(time_flu_primary ~ flu_primary_inf, data = df_input, data.frame = T)[["data"]]
py_1000_flu_primary <- py_flu_primary %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
rate_flu_mild <- py_1000_flu_primary[py_1000_flu_primary$flu_primary_inf == 1,]$flu_mild_rate

#calculate person time for flu severe outcomes
py_flu_secondary <- pyears(time_flu_secondary ~ flu_secondary_inf, data = df_input, data.frame = T)[["data"]]
py_1000_flu_secondary <- py_flu_secondary %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
rate_flu_severe <- py_1000_flu_secondary[py_1000_flu_secondary$flu_secondary_inf == 1,]$flu_severe_rate

#calculate person time for flu mortality
py_flu_mortality <- pyears(time_flu_mortality ~ flu_mortality_inf, data = df_input, data.frame = T)[["data"]]
py_1000_flu_mortality <- py_flu_mortality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
rate_flu_mortality <- py_1000_flu_mortality[py_1000_flu_mortality$flu_mortality_inf == 1,]$flu_mortality_rate

if (study_start_date >= covid_season_min) {
  #calculate person time for covid mild outcomes
  py_covid_primary <- pyears(time_covid_primary ~ covid_primary_inf, 
                             data = df_input, data.frame = T)[["data"]]
  py_1000_covid_primary <- py_covid_primary %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  rate_covid_mild <- py_1000_covid_primary[py_1000_covid_primary$covid_primary_inf == 1,]$covid_mild_rate
  
  #calculate person time for covid severe outcomes
  py_covid_secondary <- pyears(time_covid_secondary ~ covid_secondary_inf, 
                               data = df_input, data.frame = T)[["data"]]
  py_1000_covid_secondary <- py_covid_secondary %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  rate_covid_severe <- py_1000_covid_secondary[py_1000_covid_secondary$covid_secondary_inf == 1,]$covid_severe_rate
  
  #calculate person time for covid mortality
  py_covid_mortality <- pyears(time_covid_mortality ~ covid_mortality_inf, 
                              data = df_input, data.frame = T)[["data"]]
  py_1000_covid_mortality <- py_covid_mortality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  rate_covid_mortality <- py_1000_covid_mortality[py_1000_covid_mortality$covid_mortality_inf == 1,]$covid_mortality_rate
}

if (codelist_type == "sensitive") {
  #calculate person time for overall respiratory mild outcomes
  py_overall_resp_primary <- pyears(time_overall_resp_primary ~ overall_resp_primary_inf, 
                                    data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_primary <- py_overall_resp_primary %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  rate_overall_resp_mild <- py_1000_overall_resp_primary[py_1000_overall_resp_primary$overall_resp_primary_inf == 1,]$overall_resp_mild_rate
  
  #calculate person time for overall respiratory severe outcomes
  py_overall_resp_secondary <- pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf, 
                                      data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_secondary <- py_overall_resp_secondary %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  rate_overall_resp_severe <- py_1000_overall_resp_secondary[py_1000_overall_resp_secondary$overall_resp_secondary_inf == 1,]$overall_resp_severe_rate
  
  #calculate person time for overall respiratory mortality
  py_overall_resp_mortality <- pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf, 
                                     data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_mortality <- py_overall_resp_mortality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  rate_overall_resp_mortality <- py_1000_overall_resp_mortality[py_1000_overall_resp_mortality$overall_resp_mortality_inf == 1,]$overall_resp_mortality_rate
}

#calculate person time for all cause mortality
py_all_cause_mortality <- pyears(time_all_cause_mortality ~ all_cause_mortality_inf, 
                                data = df_input, data.frame = T)[["data"]]
py_1000_all_cause_mortality <- py_all_cause_mortality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
rate_all_cause_mortality <- py_1000_all_cause_mortality[py_1000_all_cause_mortality$all_cause_mortality_inf == 1,]$all_cause_mortality_rate

#create results table
results <- data.frame(
  Outcome = c("RSV mild", "RSV severe", "RSV mortality", "Flu mild", "Flu severe", "Flu mortality", "All cause mortality"),
  Rate = c(rate_rsv_mild, rate_rsv_severe, rate_rsv_mortality, rate_flu_mild, rate_flu_severe, rate_flu_mortality, rate_all_cause_mortality)
)

if (study_start_date >= covid_season_min) {
  results <- rbind(
    results,
    data.frame(
      Outcome = c("COVID mild", "COVID severe", "COVID mortality"),
      Rate = c(rate_covid_mild, rate_covid_severe, rate_covid_mortality)
    )
  )
}

if (codelist_type == "sensitive") {
  results <- rbind(
    results,
    data.frame(
      Outcome = c("Overall respiratory mild", "Overall respiratory severe", "Overall respiratory mortality"),
      Rate = c(rate_overall_resp_mild, rate_overall_resp_severe, rate_overall_resp_mortality)
    )
  )
}

##calculate survival times for rsv by risk groups 

#calculate person time for rsv mild outcomes by ethnicity
py_rsv_primary_ethnicity <- pyears(time_rsv_primary ~ rsv_primary_inf + latest_ethnicity_group,
                                   data = df_input, data.frame = T)[["data"]]
py_1000_rsv_primary_ethnicity<- py_rsv_primary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
rate_rsv_mild_ethnicity <- py_1000_rsv_primary_ethnicity[py_1000_rsv_primary_ethnicity$rsv_primary_inf == 1,]$rsv_mild_rate

#calculate person time for rsv severe by ethnicity
py_rsv_secondary_ethnicity <- pyears(time_rsv_secondary ~ rsv_secondary + latest_ethnicity_group,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_rsv_secondary_ethnicity <- py_rsv_secondary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
rate_rsv_severe_ethnicity <- py_1000_rsv_secondary_ethnicity[py_1000_rsv_secondary_ethnicity$rsv_secondary == 1,]$rsv_severe_rate

#calculate person time for rsv mortality by ethnicity
py_rsv_mortality_ethnicity <- pyears(time_rsv_mortality ~ rsv_mortality + latest_ethnicity_group,
                                    data = df_input, data.frame = T)[["data"]]
py_1000_rsv_mortality_ethnicity <- py_rsv_mortality_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
rate_rsv_motrality_ethnicity <- py_1000_rsv_mortality_ethnicity[py_1000_rsv_mortality_ethnicity$rsv_mortality == 1,]$rsv_mortality_rate

#calculate person time for flu mild outcomes by ethnicity
py_flu_primary_ethnicity <- pyears(time_flu_primary ~ flu_primary + latest_ethnicity_group,
                                  data = df_input, data.frame = T)[["data"]]
py_1000_flu_primary_ethnicity <- py_flu_primary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
rate_flu_mild_ethnicity <- py_1000_flu_primary_ethnicity[py_1000_flu_primary_ethnicity$flu_primary == 1,]$flu_mild_rate

#calculate person time for flu severe by ethnicity
py_flu_secondary_ethnicity <- pyears(time_flu_secondary ~ flu_secondary + latest_ethnicity_group,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_flu_secondary_ethnicity <- py_flu_secondary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
rate_flu_severe_ethnicity <- py_1000_flu_secondary_ethnicity[py_1000_flu_secondary_ethnicity$flu_secondary == 1,]$flu_severe_rate

#calculate person time for flu mortality by ethnicity
py_flu_mortality_ethnicity <- pyears(time_flu_mortality ~ flu_mortality + latest_ethnicity_group,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_flu_mortality_ethnicity <- py_flu_mortality_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
rate_flu_motrality_ethnicity <- py_1000_flu_mortality_ethnicity[py_1000_flu_mortality_ethnicity$flu_mortality == 1,]$flu_mortality_rate

if (study_start_date >= covid_season_min) {
  #calculate person time for covid mild outcomes by ethnicity
  py_covid_primary_ethnicity <- pyears(time_covid_primary ~ covid_primary_inf + latest_ethnicity_group,
                                     data = df_input, data.frame = T)[["data"]]
  py_1000_covid_primary_ethnicity<- py_covid_primary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  rate_covid_mild_ethnicity <- py_1000_covid_primary_ethnicity[py_1000_covid_primary_ethnicity$covid_primary_inf == 1,]$covid_mild_rate
  
  #calculate person time for covid severe by ethnicity
  py_covid_secondary_ethnicity <- pyears(time_covid_secondary ~ covid_secondary + latest_ethnicity_group,
                                       data = df_input, data.frame = T)[["data"]]
  py_1000_covid_secondary_ethnicity <- py_covid_secondary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  rate_covid_severe_ethnicity <- py_1000_covid_secondary_ethnicity[py_1000_covid_secondary_ethnicity$covid_secondary == 1,]$covid_severe_rate
  
  #calculate person time for covid mortality by ethnicity
  py_covid_mortality_ethnicity <- pyears(time_covid_mortality ~ covid_mortality + latest_ethnicity_group,
                                       data = df_input, data.frame = T)[["data"]]
  py_1000_covid_mortality_ethnicity <- py_covid_mortality_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  rate_covid_motrality_ethnicity <- py_1000_covid_mortality_ethnicity[py_1000_covid_mortality_ethnicity$covid_mortality == 1,]$covid_mortality_rate
}
  