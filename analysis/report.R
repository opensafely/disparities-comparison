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
  Outcome = c("RSV mild", "RSV severe", "RSV mortality", "Flu mild", "Flu severe", "Flu mortality"),
  Rate = c(rate_rsv_mild, rate_rsv_severe, rate_rsv_mortality, rate_flu_mild, rate_flu_severe, rate_flu_mortality),
  Group = c("Overall", "Overall", "Overall", "Overall", "Overall", "Overall")
)

if (study_start_date >= covid_season_min) {
  results <- rbind(
    results,
    data.frame(
      Outcome = c("COVID mild", "COVID severe", "COVID mortality"),
      Rate = c(rate_covid_mild, rate_covid_severe, rate_covid_mortality),
      Group = c("Overall", "Overall", "Overall")
    )
  )
}

if (codelist_type == "sensitive") {
  results <- rbind(
    results,
    data.frame(
      Outcome = c("Overall respiratory mild", "Overall respiratory severe", "Overall respiratory mortality"),
      Rate = c(rate_overall_resp_mild, rate_overall_resp_severe, rate_overall_resp_mortality),
      Group = c("Overall", "Overall", "Overall")
    )
  )
}

#add all cause mortality rates to results table
results <- rbind(
  results,
  data.frame(
    Outcome = "All cause mortality",
    Rate = rate_all_cause_mortality,
    Group = "Overall"
  )
)

##calculate survival times for rsv by risk groups 

#calculate person time for rsv mild outcomes by ethnicity
py_rsv_primary_ethnicity <- pyears(time_rsv_primary ~ rsv_primary_inf + latest_ethnicity_group,
                                   data = df_input, data.frame = T)[["data"]]
py_1000_rsv_primary_ethnicity<- py_rsv_primary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
py_1000_rsv_primary_ethnicity <- as.data.table(py_1000_rsv_primary_ethnicity)
rate_rsv_mild_ethnicity <- rlang::duplicate(py_1000_rsv_primary_ethnicity)
rate_rsv_mild_ethnicity <- rate_rsv_mild_ethnicity[rsv_primary_inf == 1, .(rsv_mild_rate, latest_ethnicity_group)]

#calculate person time for rsv severe by ethnicity
py_rsv_secondary_ethnicity <- pyears(time_rsv_secondary ~ rsv_secondary_inf + latest_ethnicity_group,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_rsv_secondary_ethnicity <- py_rsv_secondary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
py_1000_rsv_secondary_ethnicity <- as.data.table(py_1000_rsv_secondary_ethnicity)
rate_rsv_severe_ethnicity <- rlang::duplicate(py_1000_rsv_secondary_ethnicity)
rate_rsv_severe_ethnicity <- rate_rsv_severe_ethnicity[rsv_secondary_inf == 1, .(rsv_severe_rate, latest_ethnicity_group)]

#calculate person time for rsv mortality by ethnicity
py_rsv_mortality_ethnicity <- pyears(time_rsv_mortality ~ rsv_mortality_inf + latest_ethnicity_group,
                                    data = df_input, data.frame = T)[["data"]]
py_1000_rsv_mortality_ethnicity <- py_rsv_mortality_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
py_1000_rsv_mortality_ethnicity <- as.data.table(py_1000_rsv_mortality_ethnicity)
rate_rsv_mortality_ethnicity <- rlang::duplicate(py_1000_rsv_mortality_ethnicity)
rate_rsv_mortality_ethnicity <- rate_rsv_mortality_ethnicity[rsv_mortality_inf == 1, .(rsv_mortality_rate, latest_ethnicity_group)]

#calculate person time for flu mild outcomes by ethnicity
py_flu_primary_ethnicity <- pyears(time_flu_primary ~ flu_primary_inf + latest_ethnicity_group,
                                  data = df_input, data.frame = T)[["data"]]
py_1000_flu_primary_ethnicity <- py_flu_primary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
py_1000_flu_primary_ethnicity <- as.data.table(py_1000_flu_primary_ethnicity)
rate_flu_mild_ethnicity <- rlang::duplicate(py_1000_flu_primary_ethnicity)
rate_flu_mild_ethnicity <- rate_flu_mild_ethnicity[flu_primary_inf == 1, .(flu_mild_rate, latest_ethnicity_group)]

#calculate person time for flu severe by ethnicity
py_flu_secondary_ethnicity <- pyears(time_flu_secondary ~ flu_secondary_inf + latest_ethnicity_group,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_flu_secondary_ethnicity <- py_flu_secondary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
py_1000_flu_secondary_ethnicity <- as.data.table(py_1000_flu_secondary_ethnicity)
rate_flu_severe_ethnicity <- rlang::duplicate(py_1000_flu_secondary_ethnicity)
rate_flu_severe_ethnicity <- rate_flu_severe_ethnicity[flu_secondary_inf == 1, .(flu_severe_rate, latest_ethnicity_group)]

#calculate person time for flu mortality by ethnicity
py_flu_mortality_ethnicity <- pyears(time_flu_mortality ~ flu_mortality_inf + latest_ethnicity_group,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_flu_mortality_ethnicity <- py_flu_mortality_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
py_1000_flu_mortality_ethnicity <- as.data.table(py_1000_flu_mortality_ethnicity)
rate_flu_mortality_ethnicity <- rlang::duplicate(py_1000_flu_mortality_ethnicity)
rate_flu_mortality_ethnicity <- rate_flu_mortality_ethnicity[flu_mortality_inf == 1, .(flu_mortality_rate, latest_ethnicity_group)]

if (study_start_date >= covid_season_min) {
  #calculate person time for covid mild outcomes by ethnicity
  py_covid_primary_ethnicity <- pyears(time_covid_primary ~ covid_primary_inf + latest_ethnicity_group,
                                     data = df_input, data.frame = T)[["data"]]
  py_1000_covid_primary_ethnicity<- py_covid_primary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  py_1000_covid_primary_ethnicity <- as.data.table(py_1000_covid_primary_ethnicity)
  rate_covid_mild_ethnicity <- rlang::duplicate(py_1000_covid_primary_ethnicity)
  rate_covid_mild_ethnicity <- rate_covid_mild_ethnicity[covid_primary_inf == 1, .(covid_mild_rate, latest_ethnicity_group)]
  
  #calculate person time for covid severe by ethnicity
  py_covid_secondary_ethnicity <- pyears(time_covid_secondary ~ covid_secondary_inf + latest_ethnicity_group,
                                       data = df_input, data.frame = T)[["data"]]
  py_1000_covid_secondary_ethnicity <- py_covid_secondary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  py_1000_covid_secondary_ethnicity <- as.data.table(py_1000_covid_secondary_ethnicity)
  rate_covid_severe_ethnicity <- rlang::duplicate(py_1000_covid_secondary_ethnicity)
  rate_covid_severe_ethnicity <- rate_covid_severe_ethnicity[covid_secondary_inf == 1, .(covid_severe_rate, latest_ethnicity_group)]
  
  #calculate person time for covid mortality by ethnicity
  py_covid_mortality_ethnicity <- pyears(time_covid_mortality ~ covid_mortality_inf + latest_ethnicity_group,
                                       data = df_input, data.frame = T)[["data"]]
  py_1000_covid_mortality_ethnicity <- py_covid_mortality_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  py_1000_covid_mortality_ethnicity <- as.data.table(py_1000_covid_mortality_ethnicity)
  rate_covid_mortality_ethnicity <- rlang::duplicate(py_1000_covid_mortality_ethnicity)
  rate_covid_mortality_ethnicity <- rate_covid_mortality_ethnicity[covid_mortality_inf == 1, .(covid_mortality_rate, latest_ethnicity_group)]
}

if (codelist_type == "sensitive") {
  #calculate person time for overall respiratory
  py_overall_resp_primary_ethnicity <- pyears(time_overall_resp_primary ~ overall_resp_primary_inf + latest_ethnicity_group,
                                       data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_primary_ethnicity<- py_overall_resp_primary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  py_1000_overall_resp_primary_ethnicity <- as.data.table(py_1000_overall_resp_primary_ethnicity)
  rate_overall_resp_mild_ethnicity <- rlang::duplicate(py_1000_overall_resp_primary_ethnicity)
  rate_overall_resp_mild_ethnicity <- rate_overall_resp_mild_ethnicity[overall_resp_primary_inf == 1, .(overall_resp_mild_rate, latest_ethnicity_group)]
  
  #calculate person time for overall_resp severe by ethnicity
  py_overall_resp_secondary_ethnicity <- pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + latest_ethnicity_group,
                                         data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_secondary_ethnicity <- py_overall_resp_secondary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  py_1000_overall_resp_secondary_ethnicity <- as.data.table(py_1000_overall_resp_secondary_ethnicity)
  rate_overall_resp_severe_ethnicity <- rlang::duplicate(py_1000_overall_resp_secondary_ethnicity)
  rate_overall_resp_severe_ethnicity <- rate_overall_resp_severe_ethnicity[overall_resp_secondary_inf == 1, .(overall_resp_severe_rate, latest_ethnicity_group)]
  
  #calculate person time for overall_resp mortality by ethnicity
  py_overall_resp_mortality_ethnicity <- pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + latest_ethnicity_group,
                                         data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_mortality_ethnicity <- py_overall_resp_mortality_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  py_1000_overall_resp_mortality_ethnicity <- as.data.table(py_1000_overall_resp_mortality_ethnicity)
  rate_overall_resp_mortality_ethnicity <- rlang::duplicate(py_1000_overall_resp_mortality_ethnicity)
  rate_overall_resp_mortality_ethnicity <- rate_overall_resp_mortality_ethnicity[overall_resp_mortality_inf == 1, .(overall_resp_mortality_rate, latest_ethnicity_group)]
}

#calculate person time for all cause mortality by ethnicity
py_all_cause_mortality_ethnicity <- pyears(time_all_cause_mortality ~ all_cause_mortality_inf + latest_ethnicity_group,
                                           data = df_input, data.frame = T)[["data"]]
py_1000_all_cause_mortality_ethnicity <- py_all_cause_mortality_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
py_1000_all_cause_mortality_ethnicity <- as.data.table(py_1000_all_cause_mortality_ethnicity)
rate_all_cause_mortality_ethnicity <- rlang::duplicate(py_1000_all_cause_mortality_ethnicity)
rate_all_cause_mortality_ethnicity <- rate_all_cause_mortality_ethnicity[all_cause_mortality_inf == 1, .(all_cause_mortality_rate, latest_ethnicity_group)]

#add these to results table with 'Group' as ethnicity
results <- rbind(
  results,
  data.frame(
    Outcome = c(rep("RSV mild", 6), rep("RSV severe", 6), rep("RSV mortality", 6), 
                rep("Flu mild", 6), rep("Flu severe", 6), rep("Flu mortality", 6)),
    Rate = c(rate_rsv_mild_ethnicity$rsv_mild_rate, rate_rsv_severe_ethnicity$rsv_severe_rate, 
             rate_rsv_mortality_ethnicity$rsv_mortality_rate, rate_flu_mild_ethnicity$flu_mild_rate, 
             rate_flu_severe_ethnicity$flu_severe_rate, rate_flu_mortality_ethnicity$flu_mortality_rate),
    Group = c(rate_rsv_mild_ethnicity$latest_ethnicity_group, rate_rsv_severe_ethnicity$latest_ethnicity_group,
              rate_rsv_mortality_ethnicity$latest_ethnicity_group, rate_flu_mild_ethnicity$latest_ethnicity_group,
              rate_flu_severe_ethnicity$latest_ethnicity_group, rate_flu_mortality_ethnicity$latest_ethnicity_group)
    )
  )

if (study_start_date >= covid_season_min) {
  #add covid results to results table with 'Group' as ethnicity
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("COVID mild", 6), rep("COVID severe", 6), rep("COVID mortality", 6)),
      Rate = c(rate_covid_mild_ethnicity$covid_mild_rate, 
               rate_covid_severe_ethnicity$covid_severe_rate, 
               rate_covid_mortality_ethnicity$covid_mortality_rate),
      Group = c(rate_covid_mild_ethnicity$latest_ethnicity_group, 
                rate_covid_severe_ethnicity$latest_ethnicity_group,
                rate_covid_mortality_ethnicity$latest_ethnicity_group)
    )
  )
}

if (codelist_type == "sensitive") {
  #add overall respiratory results to results table
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("Overall respiratory mild", 6), rep("Overall respiratory severe", 6), 
                  rep("Overall respiratory mortality", 6)),
      Rate = c(rate_overall_resp_mild_ethnicity$overall_resp_mild_rate, 
               rate_overall_resp_severe_ethnicity$overall_resp_severe_rate, 
               rate_overall_resp_mortality_ethnicity$overall_resp_mortality_rate),
      Group = c(rate_overall_resp_mild_ethnicity$latest_ethnicity_group, 
                rate_overall_resp_severe_ethnicity$latest_ethnicity_group,
                rate_overall_resp_mortality_ethnicity$latest_ethnicity_group)
    )
  )
}

#add all cause mortality to results table
results <- rbind(
  results,
  data.frame(
    Outcome = rep("All cause mortality", 6),
    Rate = rate_all_cause_mortality_ethnicity$all_cause_mortality_rate,
    Group = rate_all_cause_mortality_ethnicity$latest_ethnicity_group
    )
)

#calculate person time for rsv mild outcomes by socioeconomic status
py_rsv_primary_socio <- pyears(time_rsv_primary ~ rsv_primary_inf + imd_quintile,
                                   data = df_input, data.frame = T)[["data"]]
py_1000_rsv_primary_socio<- py_rsv_primary_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
py_1000_rsv_primary_socio <- as.data.table(py_1000_rsv_primary_socio)
rate_rsv_mild_socio <- rlang::duplicate(py_1000_rsv_primary_socio)
rate_rsv_mild_socio <- rate_rsv_mild_socio[rsv_primary_inf == 1, .(rsv_mild_rate, imd_quintile)]

#calculate person time for rsv severe by socio
py_rsv_secondary_socio <- pyears(time_rsv_secondary ~ rsv_secondary_inf + imd_quintile,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_rsv_secondary_socio <- py_rsv_secondary_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
py_1000_rsv_secondary_socio <- as.data.table(py_1000_rsv_secondary_socio)
rate_rsv_severe_socio <- rlang::duplicate(py_1000_rsv_secondary_socio)
rate_rsv_severe_socio <- rate_rsv_severe_socio[rsv_secondary_inf == 1, .(rsv_severe_rate, imd_quintile)]

#calculate person time for rsv mortality by socio
py_rsv_mortality_socio <- pyears(time_rsv_mortality ~ rsv_mortality_inf + imd_quintile,
                                    data = df_input, data.frame = T)[["data"]]
py_1000_rsv_mortality_socio <- py_rsv_mortality_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
py_1000_rsv_mortality_socio <- as.data.table(py_1000_rsv_mortality_socio)
rate_rsv_mortality_socio <- rlang::duplicate(py_1000_rsv_mortality_socio)
rate_rsv_mortality_socio <- rate_rsv_mortality_socio[rsv_mortality_inf == 1, .(rsv_mortality_rate, imd_quintile)]

#calculate person time for flu mild outcomes by socio
py_flu_primary_socio <- pyears(time_flu_primary ~ flu_primary_inf + imd_quintile,
                                  data = df_input, data.frame = T)[["data"]]
py_1000_flu_primary_socio <- py_flu_primary_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
py_1000_flu_primary_socio <- as.data.table(py_1000_flu_primary_socio)
rate_flu_mild_socio <- rlang::duplicate(py_1000_flu_primary_socio)
rate_flu_mild_socio <- rate_flu_mild_socio[flu_primary_inf == 1, .(flu_mild_rate, imd_quintile)]

#calculate person time for flu severe by socio
py_flu_secondary_socio <- pyears(time_flu_secondary ~ flu_secondary_inf + imd_quintile,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_flu_secondary_socio <- py_flu_secondary_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
py_1000_flu_secondary_socio <- as.data.table(py_1000_flu_secondary_socio)
rate_flu_severe_socio <- rlang::duplicate(py_1000_flu_secondary_socio)
rate_flu_severe_socio <- rate_flu_severe_socio[flu_secondary_inf == 1, .(flu_severe_rate, imd_quintile)]

#calculate person time for flu mortality by socio
py_flu_mortality_socio <- pyears(time_flu_mortality ~ flu_mortality_inf + imd_quintile,
                                     data = df_input, data.frame = T)[["data"]]
py_1000_flu_mortality_socio <- py_flu_mortality_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
py_1000_flu_mortality_socio <- as.data.table(py_1000_flu_mortality_socio)
rate_flu_mortality_socio <- rlang::duplicate(py_1000_flu_mortality_socio)
rate_flu_mortality_socio <- rate_flu_mortality_socio[flu_mortality_inf == 1, .(flu_mortality_rate, imd_quintile)]

if (study_start_date >= covid_season_min) {
  #calculate person time for covid mild outcomes by socio
  py_covid_primary_socio <- pyears(time_covid_primary ~ covid_primary_inf + imd_quintile,
                                     data = df_input, data.frame = T)[["data"]]
  py_1000_covid_primary_socio<- py_covid_primary_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  py_1000_covid_primary_socio <- as.data.table(py_1000_covid_primary_socio)
  rate_covid_mild_socio <- rlang::duplicate(py_1000_covid_primary_socio)
  rate_covid_mild_socio <- rate_covid_mild_socio[covid_primary_inf == 1, .(covid_mild_rate, imd_quintile)]
  
  #calculate person time for covid severe by socio
  py_covid_secondary_socio <- pyears(time_covid_secondary ~ covid_secondary_inf + imd_quintile,
                                       data = df_input, data.frame = T)[["data"]]
  py_1000_covid_secondary_socio <- py_covid_secondary_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  py_1000_covid_secondary_socio <- as.data.table(py_1000_covid_secondary_socio)
  rate_covid_severe_socio <- rlang::duplicate(py_1000_covid_secondary_socio)
  rate_covid_severe_socio <- rate_covid_severe_socio[covid_secondary_inf == 1, .(covid_severe_rate, imd_quintile)]
  
  #calculate person time for covid mortality by socio
  py_covid_mortality_socio <- pyears(time_covid_mortality ~ covid_mortality_inf + imd_quintile,
                                       data = df_input, data.frame = T)[["data"]]
  py_1000_covid_mortality_socio <- py_covid_mortality_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  py_1000_covid_mortality_socio <- as.data.table(py_1000_covid_mortality_socio)
  rate_covid_mortality_socio <- rlang::duplicate(py_1000_covid_mortality_socio)
  rate_covid_mortality_socio <- rate_covid_mortality_socio[covid_mortality_inf == 1, .(covid_mortality_rate, imd_quintile)]
}

if (codelist_type == "sensitive") {
  #calculate person time for overall respiratory
  py_overall_resp_primary_socio <- pyears(time_overall_resp_primary ~ overall_resp_primary_inf + imd_quintile,
                                       data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_primary_socio<- py_overall_resp_primary_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  py_1000_overall_resp_primary_socio <- as.data.table(py_1000_overall_resp_primary_socio)
  rate_overall_resp_mild_socio <- rlang::duplicate(py_1000_overall_resp_primary_socio)
  rate_overall_resp_mild_socio <- rate_overall_resp_mild_socio[overall_resp_primary_inf == 1, .(overall_resp_mild_rate, imd_quintile)]
  
  #calculate person time for overall_resp severe by socio
  py_overall_resp_secondary_socio <- pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + imd_quintile,
                                         data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_secondary_socio <- py_overall_resp_secondary_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  py_1000_overall_resp_secondary_socio <- as.data.table(py_1000_overall_resp_secondary_socio)
  rate_overall_resp_severe_socio <- rlang::duplicate(py_1000_overall_resp_secondary_socio)
  rate_overall_resp_severe_socio <- rate_overall_resp_severe_socio[overall_resp_secondary_inf == 1, .(overall_resp_severe_rate, imd_quintile)]
  
  #calculate person time for overall_resp mortality by socio
  py_overall_resp_mortality_socio <- pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + imd_quintile,
                                         data = df_input, data.frame = T)[["data"]]
  py_1000_overall_resp_mortality_socio <- py_overall_resp_mortality_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  py_1000_overall_resp_mortality_socio <- as.data.table(py_1000_overall_resp_mortality_socio)
  rate_overall_resp_mortality_socio <- rlang::duplicate(py_1000_overall_resp_mortality_socio)
  rate_overall_resp_mortality_socio <- rate_overall_resp_mortality_socio[overall_resp_mortality_inf == 1, .(overall_resp_mortality_rate, imd_quintile)]
}

#calculate person time for all cause mortality by socio
py_all_cause_mortality_socio <- pyears(time_all_cause_mortality ~ all_cause_mortality_inf + imd_quintile,
                                           data = df_input, data.frame = T)[["data"]]
py_1000_all_cause_mortality_socio <- py_all_cause_mortality_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
py_1000_all_cause_mortality_socio <- as.data.table(py_1000_all_cause_mortality_socio)
rate_all_cause_mortality_socio <- rlang::duplicate(py_1000_all_cause_mortality_socio)
rate_all_cause_mortality_socio <- rate_all_cause_mortality_socio[all_cause_mortality_inf == 1, .(all_cause_mortality_rate, imd_quintile)]

#add these to results table with 'Group' as socio
results <- rbind(
  results,
  data.frame(
    Outcome = c(rep("RSV mild", 5), rep("RSV severe", 5), rep("RSV mortality", 5), 
                rep("Flu mild", 5), rep("Flu severe", 5), rep("Flu mortality", 5)),
    Rate = c(rate_rsv_mild_socio$rsv_mild_rate, rate_rsv_severe_socio$rsv_severe_rate, 
             rate_rsv_mortality_socio$rsv_mortality_rate, rate_flu_mild_socio$flu_mild_rate, 
             rate_flu_severe_socio$flu_severe_rate, rate_flu_mortality_socio$flu_mortality_rate),
    Group = c(rate_rsv_mild_socio$imd_quintile, rate_rsv_severe_socio$imd_quintile,
              rate_rsv_mortality_socio$imd_quintile, rate_flu_mild_socio$imd_quintile,
              rate_flu_severe_socio$imd_quintile, rate_flu_mortality_socio$imd_quintile)
    )
  )

if (study_start_date >= covid_season_min) {
  #add covid results to results table with 'Group' as socio
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("COVID mild", 5), rep("COVID severe", 5), rep("COVID mortality", 5)),
      Rate = c(rate_covid_mild_socio$covid_mild_rate, 
               rate_covid_severe_socio$covid_severe_rate, 
               rate_covid_mortality_socio$covid_mortality_rate),
      Group = c(rate_covid_mild_socio$imd_quintile, 
                rate_covid_severe_socio$imd_quintile,
                rate_covid_mortality_socio$imd_quintile)
    )
  )
}

if (codelist_type == "sensitive") {
  #add overall respiratory results to results table
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("Overall respiratory mild", 5), rep("Overall respiratory severe", 5), 
                  rep("Overall respiratory mortality", 5)),
      Rate = c(rate_overall_resp_mild_socio$overall_resp_mild_rate, 
               rate_overall_resp_severe_socio$overall_resp_severe_rate, 
               rate_overall_resp_mortality_socio$overall_resp_mortality_rate),
      Group = c(rate_overall_resp_mild_socio$imd_quintile, 
                rate_overall_resp_severe_socio$imd_quintile,
                rate_overall_resp_mortality_socio$imd_quintile)
    )
  )
}

#add all cause mortality to results table
results <- rbind(
  results,
  data.frame(
    Outcome = rep("All cause mortality", 5),
    Rate = rate_all_cause_mortality_socio$all_cause_mortality_rate,
    Group = rate_all_cause_mortality_socio$imd_quintile
    )
)
