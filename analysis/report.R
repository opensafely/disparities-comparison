library(tidyr)
library(dplyr)
library(here)
library(arrow)
library(ggplot2)
library(readr)
library(gtsummary)
library(lubridate)
library(survival)
library(gt)
library(tidyselect)
library(magrittr)
library(tibble)

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

# lab <- ifelse(cohort == "infants", "Age (Months)",
#        ifelse(cohort == "infants_subgroup", "Age (Months)", "Age (Years)"))
# 
# plot_age <- ggplot(data = df_input, aes(age, frequency(age))) + geom_col(width = 0.9) +
#   xlab(lab) + ylab("Frequency")
# 
# ggsave(
#   plot = plot_age,
#   filename = paste0("descriptive_", cohort, "_", year(study_start_date),
#     "_", year(study_end_date), "_", codelist_type, "_",
#     investigation_type,".png"), path = here::here("output", "models"),
# )

#calculate person time and rate for rsv mild outcomes
py_rsv_primary <- pyears(time_rsv_primary ~ rsv_primary_inf, data = df_input)
py_rsv_primary <- data.frame(
  pyears = py_rsv_primary[["pyears"]], 
  n = py_rsv_primary[["n"]]
  )
py_rsv_primary <- rownames_to_column(py_rsv_primary, "rsv_primary_inf")
py_1000_rsv_primary <- py_rsv_primary %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
rate_rsv_mild <- subset(py_1000_rsv_primary, rsv_primary_inf == 1)$rsv_mild_rate

#calculate person time and rate for rsv severe outcomes
py_rsv_secondary <- pyears(time_rsv_secondary ~ rsv_secondary_inf, data = df_input)
py_rsv_secondary <- data.frame(
  pyears = py_rsv_secondary[["pyears"]], 
  n = py_rsv_secondary[["n"]]
  )
py_rsv_secondary <- rownames_to_column(py_rsv_secondary, "rsv_secondary_inf")
py_1000_rsv_secondary <- py_rsv_secondary %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
rate_rsv_severe <- subset(py_1000_rsv_secondary, 
                          rsv_secondary_inf == 1)$rsv_severe_rate

#calculate person time and rate for rsv mortality
py_rsv_mortality <- pyears(time_rsv_mortality ~ rsv_mortality_inf, data = df_input)
py_rsv_mortality <- data.frame(
  pyears = py_rsv_mortality[["pyears"]], 
  n = py_rsv_mortality[["n"]]
  )
py_rsv_mortality <- rownames_to_column(py_rsv_mortality, "rsv_mortality_inf")
py_1000_rsv_mortality <- py_rsv_mortality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
rate_rsv_mortality <- subset(py_1000_rsv_mortality, 
                             rsv_mortality_inf == 1)$rsv_mortality_rate

#calculate person time and rate for flu mild outcomes
py_flu_primary <- pyears(time_flu_primary ~ flu_primary_inf, data = df_input)
py_flu_primary <- data.frame(
  pyears = py_flu_primary[["pyears"]], 
  n = py_flu_primary[["n"]]
  )
py_flu_primary <- rownames_to_column(py_flu_primary, "flu_primary_inf")
py_1000_flu_primary <- py_flu_primary %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
rate_flu_mild <- subset(py_1000_flu_primary, flu_primary_inf == 1)$flu_mild_rate

#calculate person time and rate for flu severe outcomes
py_flu_secondary <- pyears(time_flu_secondary ~ flu_secondary_inf, data = df_input)
py_flu_secondary <- data.frame(
  pyears = py_flu_secondary[["pyears"]], 
  n = py_flu_secondary[["n"]]
  )
py_flu_secondary <- rownames_to_column(py_flu_secondary, "flu_secondary_inf")
py_1000_flu_secondary <- py_flu_secondary %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
rate_flu_severe <- subset(py_1000_flu_secondary, 
                          flu_secondary_inf == 1)$flu_severe_rate

#calculate person time and rate for flu mortality
py_flu_mortality <- pyears(time_flu_mortality ~ flu_mortality_inf, data = df_input)
py_flu_mortality <- data.frame(
  pyears = py_flu_mortality[["pyears"]], 
  n = py_flu_mortality[["n"]]
  )
py_flu_mortality <- rownames_to_column(py_flu_mortality, "flu_mortality_inf")
py_1000_flu_mortality <- py_flu_mortality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
rate_flu_mortality <- subset(py_1000_flu_mortality, 
                             flu_mortality_inf == 1)$flu_mortality_rate

if (study_start_date >= covid_season_min) {
  #calculate person time and rate for covid mild outcomes
  py_covid_primary <- pyears(time_covid_primary ~ covid_primary_inf, 
                             data = df_input)
  py_covid_primary <- data.frame(
    pyears = py_covid_primary[["pyears"]], 
    n = py_covid_primary[["n"]]
    )
  py_covid_primary <- rownames_to_column(py_covid_primary, "covid_primary_inf")
  py_1000_covid_primary <- py_covid_primary %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  rate_covid_mild <- subset(py_1000_covid_primary, 
                            covid_primary_inf == 1)$covid_mild_rate
  
  #calculate person time and rate for covid severe outcomes
  py_covid_secondary <- pyears(time_covid_secondary ~ covid_secondary_inf, 
                               data = df_input)
  py_covid_secondary <- data.frame(
    pyears = py_covid_secondary[["pyears"]], 
    n = py_covid_secondary[["n"]]
    )
  py_covid_secondary <- rownames_to_column(py_covid_secondary, "covid_secondary_inf")
  py_1000_covid_secondary <- py_covid_secondary %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  rate_covid_severe <- subset(py_1000_covid_secondary, 
                              covid_secondary_inf == 1)$covid_severe_rate
  
  #calculate person time and rate for covid mortality
  py_covid_mortality <- pyears(time_covid_mortality ~ covid_mortality_inf, 
                              data = df_input)
  py_covid_mortality <- data.frame(
    pyears = py_covid_mortality[["pyears"]], 
    n = py_covid_mortality[["n"]]
    )
  py_covid_mortality <- rownames_to_column(py_covid_mortality, "covid_mortality_inf")
  py_1000_covid_mortality <- py_covid_mortality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  rate_covid_mortality <- subset(py_1000_covid_mortality, 
                                 covid_mortality_inf == 1)$covid_mortality_rate
}

if (codelist_type == "sensitive") {
  #calculate person time and rate for overall respiratory mild outcomes
  py_overall_resp_primary <- pyears(time_overall_resp_primary ~ overall_resp_primary_inf, 
                                    data = df_input)
  py_overall_resp_primary <- data.frame(
    pyears = py_overall_resp_primary[["pyears"]], 
    n = py_overall_resp_primary[["n"]]
    )
  py_overall_resp_primary <- rownames_to_column(py_overall_resp_primary,
                                                "overall_resp_primary_inf")
  py_1000_overall_resp_primary <- py_overall_resp_primary %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  rate_overall_resp_mild <- subset(py_1000_overall_resp_primary, 
                                   overall_resp_primary_inf == 1)$overall_resp_mild_rate
  
  #calculate person time and rate for overall respiratory severe outcomes
  py_overall_resp_secondary <- pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf, 
                                      data = df_input)
  py_overall_resp_secondary <- data.frame(
    pyears = py_overall_resp_secondary[["pyears"]], 
    n = py_overall_resp_secondary[["n"]]
    )
  py_overall_resp_secondary <- rownames_to_column(py_overall_resp_secondary,
                                                  "overall_resp_secondary_inf")
  py_1000_overall_resp_secondary <- py_overall_resp_secondary %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  rate_overall_resp_severe <- subset(py_1000_overall_resp_secondary, 
                                     overall_resp_secondary_inf == 1)$overall_resp_severe_rate
  
  #calculate person time and rate for overall respiratory mortality
  py_overall_resp_mortality <- pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf, 
                                     data = df_input)
  py_overall_resp_mortality <- data.frame(
    pyears = py_overall_resp_mortality[["pyears"]], 
    n = py_overall_resp_mortality[["n"]]
    )
  py_overall_resp_mortality <- rownames_to_column(py_overall_resp_mortality,
                                                 "overall_resp_mortality_inf")
  py_1000_overall_resp_mortality <- py_overall_resp_mortality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  rate_overall_resp_mortality <- subset(py_1000_overall_resp_mortality, 
                                        overall_resp_mortality_inf == 1)$overall_resp_mortality_rate
}

#calculate person time and rate for all cause mortality
py_all_cause_mortality <- pyears(time_all_cause_mortality ~ all_cause_mortality_inf, 
                                data = df_input)
py_all_cause_mortality <- data.frame(
  pyears = py_all_cause_mortality[["pyears"]], 
  n = py_all_cause_mortality[["n"]]
  )
py_all_cause_mortality <- rownames_to_column(py_all_cause_mortality, 
                                             "all_cause_mortality_inf")
py_1000_all_cause_mortality <- py_all_cause_mortality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
rate_all_cause_mortality <- subset(py_1000_all_cause_mortality, 
                                   all_cause_mortality_inf == 1)$all_cause_mortality_rate

#create results table
results <- data.frame(
  Outcome = c("RSV mild", "RSV severe", "RSV mortality", "Flu mild", 
              "Flu severe", "Flu mortality"),
  Rate = c(rate_rsv_mild, rate_rsv_severe, rate_rsv_mortality, rate_flu_mild, 
           rate_flu_severe, rate_flu_mortality),
  Characteristic = rep("Total", 6),
  Group = c(rep("Overall", 6))
)

if (study_start_date >= covid_season_min) {
  results <- rbind(
    results,
    data.frame(
      Outcome = c("COVID-19 mild", "COVID-19 severe", "COVID-19 mortality"),
      Rate = c(rate_covid_mild, rate_covid_severe, rate_covid_mortality),
      Characteristic = c(rep("Total", 3)),
      Group = c(rep("Overall", 3))
    )
  )
}

if (codelist_type == "sensitive") {
  results <- rbind(
    results,
    data.frame(
      Outcome = c("Overall respiratory mild", "Overall respiratory severe", 
                  "Overall respiratory mortality"),
      Rate = c(rate_overall_resp_mild, rate_overall_resp_severe, 
               rate_overall_resp_mortality),
      Characteristic = rep("Total", 3),
      Group = c(rep("Overall", 3))
    )
  )
}

#add all cause mortality rates to results table
results <- rbind(
  results,
  data.frame(
    Outcome = "All cause mortality",
    Rate = rate_all_cause_mortality,
    Characteristic = "Total",
    Group = "Overall"
  )
)

##calculate survival times for rsv by risk groups 

#calculate person time and rate for rsv mild outcomes by age group
py_rsv_primary_age <- as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + age_band,
                             data = df_input)[["pyears"]])
py_rsv_primary_age <- rownames_to_column(py_rsv_primary_age, "rsv_primary_inf")
py_rsv_primary_age <- gather(py_rsv_primary_age, age_band, pyears, -rsv_primary_inf)
py_rsv_primary_age$n <- gather(as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + age_band,
                                             data = df_input)[["n"]]))$value
py_1000_rsv_primary_age <- py_rsv_primary_age %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
rate_rsv_mild_age <- rlang::duplicate(py_1000_rsv_primary_age)
rate_rsv_mild_age <- subset(rate_rsv_mild_age, rsv_primary_inf == 1, 
                            select = c(rsv_mild_rate, age_band))

#calculate person time and rate for rsv severe by age group
py_rsv_secondary_age <- as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + age_band,
                               data = df_input)[["pyears"]])
py_rsv_secondary_age <- rownames_to_column(py_rsv_secondary_age, "rsv_secondary_inf")
py_rsv_secondary_age <- gather(py_rsv_secondary_age, age_band, pyears, -rsv_secondary_inf)
py_rsv_secondary_age$n <- gather(as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + age_band,
                                           data = df_input)[["n"]]))$value
py_1000_rsv_secondary_age <- py_rsv_secondary_age %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
rate_rsv_severe_age <- rlang::duplicate(py_1000_rsv_secondary_age)
rate_rsv_severe_age <- subset(rate_rsv_severe_age, rsv_secondary_inf == 1, 
                              select = c(rsv_severe_rate, age_band))

#calculate person time and rate for rsv mortality by age group
py_rsv_mortality_age <- as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + age_band,
                               data = df_input)[["pyears"]])
py_rsv_mortality_age <- rownames_to_column(py_rsv_mortality_age, "rsv_mortality_inf")
py_rsv_mortality_age <- gather(py_rsv_mortality_age, age_band, pyears, -rsv_mortality_inf)
py_rsv_mortality_age$n <- gather(as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + age_band,
                                           data = df_input)[["n"]]))$value
py_1000_rsv_mortality_age <- py_rsv_mortality_age %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
rate_rsv_mortality_age <- rlang::duplicate(py_1000_rsv_mortality_age)
rate_rsv_mortality_age <- subset(rate_rsv_mortality_age, rsv_mortality_inf == 1, 
                                 select = c(rsv_mortality_rate, age_band))

#calculate person time and rate for flu mild outcomes by age group
py_flu_primary_age <- as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + age_band,
                                           data = df_input)[["pyears"]])
py_flu_primary_age <- rownames_to_column(py_flu_primary_age, "flu_primary_inf")
py_flu_primary_age <- gather(py_flu_primary_age, age_band, pyears, -flu_primary_inf)
py_flu_primary_age$n <- gather(as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + age_band,
                                                    data = df_input)[["n"]]))$value
py_1000_flu_primary_age <- py_flu_primary_age %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
rate_flu_mild_age <- rlang::duplicate(py_1000_flu_primary_age)
rate_flu_mild_age <- subset(rate_flu_mild_age, flu_primary_inf == 1, 
                            select = c(flu_mild_rate, age_band))

#calculate person time and rate for flu severe by age group
py_flu_secondary_age <- as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + age_band,
                                             data = df_input)[["pyears"]])
py_flu_secondary_age <- rownames_to_column(py_flu_secondary_age, "flu_secondary_inf")
py_flu_secondary_age <- gather(py_flu_secondary_age, age_band, pyears, -flu_secondary_inf)
py_flu_secondary_age$n <- gather(as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + age_band,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_secondary_age <- py_flu_secondary_age %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
rate_flu_severe_age <- rlang::duplicate(py_1000_flu_secondary_age)
rate_flu_severe_age <- subset(rate_flu_severe_age, flu_secondary_inf == 1, 
                              select = c(flu_severe_rate, age_band))

#calculate person time and rate for flu mortality by age group
py_flu_mortality_age <- as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + age_band,
                                             data = df_input)[["pyears"]])
py_flu_mortality_age <- rownames_to_column(py_flu_mortality_age, "flu_mortality_inf")
py_flu_mortality_age <- gather(py_flu_mortality_age, age_band, pyears, -flu_mortality_inf)
py_flu_mortality_age$n <- gather(as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + age_band,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_mortality_age <- py_flu_mortality_age %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
rate_flu_mortality_age <- rlang::duplicate(py_1000_flu_mortality_age)
rate_flu_mortality_age <- subset(rate_flu_mortality_age, flu_mortality_inf == 1, 
                                 select = c(flu_mortality_rate, age_band))

if (study_start_date >= covid_season_min) {
  #calculate person time and rate for covid mild outcomes by age group
  py_covid_primary_age <- as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + age_band,
                                             data = df_input)[["pyears"]])
  py_covid_primary_age <- rownames_to_column(py_covid_primary_age, "covid_primary_inf")
  py_covid_primary_age <- gather(py_covid_primary_age, age_band, pyears, -covid_primary_inf)
  py_covid_primary_age$n <- gather(as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + age_band,
                                                      data = df_input)[["n"]]))$value
  py_1000_covid_primary_age <- py_covid_primary_age %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  rate_covid_mild_age <- rlang::duplicate(py_1000_covid_primary_age)
  rate_covid_mild_age <- subset(rate_covid_mild_age, covid_primary_inf == 1, 
                              select = c(covid_mild_rate, age_band))
  
  #calculate person time and rate for covid severe by age group
  py_covid_secondary_age <- as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + age_band,
                                               data = df_input)[["pyears"]])
  py_covid_secondary_age <- rownames_to_column(py_covid_secondary_age, "covid_secondary_inf")
  py_covid_secondary_age <- gather(py_covid_secondary_age, age_band, pyears, -covid_secondary_inf)
  py_covid_secondary_age$n <- gather(as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + age_band,
                                                        data = df_input)[["n"]]))$value
  py_1000_covid_secondary_age <- py_covid_secondary_age %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  rate_covid_severe_age <- rlang::duplicate(py_1000_covid_secondary_age)
  rate_covid_severe_age <- subset(rate_covid_severe_age, covid_secondary_inf == 1, 
                                select = c(covid_severe_rate, age_band))
  
  #calculate person time and rate for covid mortality by age group
  py_covid_mortality_age <- as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + age_band,
                                               data = df_input)[["pyears"]])
  py_covid_mortality_age <- rownames_to_column(py_covid_mortality_age, "covid_mortality_inf")
  py_covid_mortality_age <- gather(py_covid_mortality_age, age_band, pyears, -covid_mortality_inf)
  py_covid_mortality_age$n <- gather(as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + age_band,
                                                        data = df_input)[["n"]]))$value
  py_1000_covid_mortality_age <- py_covid_mortality_age %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  rate_covid_mortality_age <- rlang::duplicate(py_1000_covid_mortality_age)
  rate_covid_mortality_age <- subset(rate_covid_mortality_age, covid_mortality_inf == 1, 
                                   select = c(covid_mortality_rate, age_band))
}

if (codelist_type == "sensitive") {
  #calculate person time and rate for overall respiratory mild outcomes by age group
  py_overall_resp_primary_age <- as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + age_band,
                                             data = df_input)[["pyears"]])
  py_overall_resp_primary_age <- rownames_to_column(py_overall_resp_primary_age, "overall_resp_primary_inf")
  py_overall_resp_primary_age <- gather(py_overall_resp_primary_age, age_band, pyears, -overall_resp_primary_inf)
  py_overall_resp_primary_age$n <- gather(as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + age_band,
                                                      data = df_input)[["n"]]))$value
  py_1000_overall_resp_primary_age <- py_overall_resp_primary_age %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  rate_overall_resp_mild_age <- rlang::duplicate(py_1000_overall_resp_primary_age)
  rate_overall_resp_mild_age <- subset(rate_overall_resp_mild_age, overall_resp_primary_inf == 1, 
                              select = c(overall_resp_mild_rate, age_band))
  
  #calculate person time and rate for overall respiratory severe by age group
  py_overall_resp_secondary_age <- as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + age_band,
                                               data = df_input)[["pyears"]])
  py_overall_resp_secondary_age <- rownames_to_column(py_overall_resp_secondary_age, "overall_resp_secondary_inf")
  py_overall_resp_secondary_age <- gather(py_overall_resp_secondary_age, age_band, pyears, -overall_resp_secondary_inf)
  py_overall_resp_secondary_age$n <- gather(as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + age_band,
                                                        data = df_input)[["n"]]))$value
  py_1000_overall_resp_secondary_age <- py_overall_resp_secondary_age %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  rate_overall_resp_severe_age <- rlang::duplicate(py_1000_overall_resp_secondary_age)
  rate_overall_resp_severe_age <- subset(rate_overall_resp_severe_age, overall_resp_secondary_inf == 1, 
                                select = c(overall_resp_severe_rate, age_band))
  
  #calculate person time and rate for overall respiratory mortality by age group
  py_overall_resp_mortality_age <- as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + age_band,
                                               data = df_input)[["pyears"]])
  py_overall_resp_mortality_age <- rownames_to_column(py_overall_resp_mortality_age, "overall_resp_mortality_inf")
  py_overall_resp_mortality_age <- gather(py_overall_resp_mortality_age, age_band, pyears, -overall_resp_mortality_inf)
  py_overall_resp_mortality_age$n <- gather(as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + age_band,
                                                        data = df_input)[["n"]]))$value
  py_1000_overall_resp_mortality_age <- py_overall_resp_mortality_age %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  rate_overall_resp_mortality_age <- rlang::duplicate(py_1000_overall_resp_mortality_age)
  rate_overall_resp_mortality_age <- subset(rate_overall_resp_mortality_age, overall_resp_mortality_inf == 1, 
                                   select = c(overall_resp_mortality_rate, age_band))
}

#calculate person time and rate for all cause mortality by age group
py_all_cause_mortality_age <- as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + age_band,
                                    data = df_input)[["pyears"]])
py_all_cause_mortality_age <- rownames_to_column(py_all_cause_mortality_age, 
                                                 "all_cause_mortality_inf")
py_all_cause_mortality_age <- gather(py_all_cause_mortality_age, age_band, pyears,
                                    -all_cause_mortality_inf)
py_all_cause_mortality_age$n <- gather(as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + age_band,
                                                data = df_input)[["n"]]))$value
py_1000_all_cause_mortality_age <- py_all_cause_mortality_age %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
rate_all_cause_mortality_age <- rlang::duplicate(py_1000_all_cause_mortality_age)
rate_all_cause_mortality_age <- subset(rate_all_cause_mortality_age, 
                                       all_cause_mortality_inf == 1, 
                                       select = c(all_cause_mortality_rate, age_band))

#add these to the results table with 'Group' as age
results <- rbind(
  results,
  data.frame(
    Outcome = c(rep("RSV mild", nrow(rate_rsv_mild_age)),
                rep("RSV severe", nrow(rate_rsv_severe_age)), 
                rep("RSV mortality", nrow(rate_rsv_mortality_age)), 
                rep("Flu mild", nrow(rate_flu_mild_age)), 
                rep("Flu severe", nrow(rate_flu_severe_age)), 
                rep("Flu mortality", nrow(rate_flu_mortality_age))),
    Rate = c(rate_rsv_mild_age$rsv_mild_rate, rate_rsv_severe_age$rsv_severe_rate, 
             rate_rsv_mortality_age$rsv_mortality_rate, rate_flu_mild_age$flu_mild_rate, 
             rate_flu_severe_age$flu_severe_rate, rate_flu_mortality_age$flu_mortality_rate),
    Characteristic = rep("Age", nrow(rate_rsv_mild_age) + nrow(rate_rsv_severe_age) + 
                          nrow(rate_rsv_mortality_age) + nrow(rate_flu_mild_age) + 
                          nrow(rate_flu_severe_age) + nrow(rate_flu_mortality_age)),
    Group = c(rate_rsv_mild_age$age_band, rate_rsv_severe_age$age_band, 
              rate_rsv_mortality_age$age_band, rate_flu_mild_age$age_band, 
              rate_flu_severe_age$age_band, rate_flu_mortality_age$age_band)
  )
)

if (study_start_date >= covid_season_min) {
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("COVID-19 mild", nrow(rate_covid_mild_age)), 
                  rep("COVID-19 severe", nrow(rate_covid_severe_age)),
                  rep("COVID-19 mortality", nrow(rate_covid_mortality_age))),
      Rate = c(rate_covid_mild_age$covid_mild_rate, rate_covid_severe_age$covid_severe_rate, 
               rate_covid_mortality_age$covid_mortality_rate),
      Characteristic = rep("Age", nrow(rate_covid_mild_age) + 
                               nrow(rate_covid_severe_age) + 
                               nrow(rate_covid_mortality_age)),
      Group = c(rate_covid_mild_age$age_band, rate_covid_severe_age$age_band, 
                rate_covid_mortality_age$age_band)
    )
  )
}

if (codelist_type == "sensitive") {
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("Overall respiratory mild", nrow(rate_overall_resp_mild_age)), 
                  rep("Overall respiratory severe", nrow(rate_overall_resp_severe_age)), 
                  rep("Overall respiratory mortality", nrow(rate_overall_resp_mortality_age))),
      Rate = c(rate_overall_resp_mild_age$overall_resp_mild_rate, 
               rate_overall_resp_severe_age$overall_resp_severe_rate, 
               rate_overall_resp_mortality_age$overall_resp_mortality_rate),
      Characteristic = rep("Age", nrow(rate_overall_resp_mild_age)),
      Group = c(rate_overall_resp_mild_age$age_band, 
                rate_overall_resp_severe_age$age_band, 
                rate_overall_resp_mortality_age$age_band)
    )
  )
}

#add all cause mortality rates to results table
results <- rbind(
  results,
  data.frame(
    Outcome = rep("All cause mortality", nrow(rate_all_cause_mortality_age)),
    Rate = rate_all_cause_mortality_age$all_cause_mortality_rate,
    Characteristic = rep("Age", nrow(rate_all_cause_mortality_age)),
    Group = rate_all_cause_mortality_age$age_band
  )
)

#calculate person time and rate for rsv mild outcomes by sex
py_rsv_primary_sex <- as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + sex,
                                           data = df_input)[["pyears"]])
py_rsv_primary_sex <- rownames_to_column(py_rsv_primary_sex, "rsv_primary_inf")
py_rsv_primary_sex <- gather(py_rsv_primary_sex, sex, pyears, -rsv_primary_inf)
py_rsv_primary_sex$n <- gather(as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + sex,
                                                    data = df_input)[["n"]]))$value
py_1000_rsv_primary_sex <- py_rsv_primary_sex %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
rate_rsv_mild_sex <- rlang::duplicate(py_1000_rsv_primary_sex)
rate_rsv_mild_sex <- subset(rate_rsv_mild_sex, rsv_primary_inf == 1, 
                            select = c(rsv_mild_rate, sex))

#calculate person time and rate for rsv severe by sex 
py_rsv_secondary_sex <- as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + sex,
                                             data = df_input)[["pyears"]])
py_rsv_secondary_sex <- rownames_to_column(py_rsv_secondary_sex, "rsv_secondary_inf")
py_rsv_secondary_sex <- gather(py_rsv_secondary_sex, sex, pyears, -rsv_secondary_inf)
py_rsv_secondary_sex$n <- gather(as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + sex,
                                                      data = df_input)[["n"]]))$value
py_1000_rsv_secondary_sex <- py_rsv_secondary_sex %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
rate_rsv_severe_sex <- rlang::duplicate(py_1000_rsv_secondary_sex)
rate_rsv_severe_sex <- subset(rate_rsv_severe_sex, rsv_secondary_inf == 1, 
                              select = c(rsv_severe_rate, sex))

#calculate person time and rate for rsv mortality by sex
py_rsv_mortality_sex <- as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + sex,
                                             data = df_input)[["pyears"]])
py_rsv_mortality_sex <- rownames_to_column(py_rsv_mortality_sex, "rsv_mortality_inf")
py_rsv_mortality_sex <- gather(py_rsv_mortality_sex, sex, pyears, -rsv_mortality_inf)
py_rsv_mortality_sex$n <- gather(as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + sex,
                                                      data = df_input)[["n"]]))$value
py_1000_rsv_mortality_sex <- py_rsv_mortality_sex %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
rate_rsv_mortality_sex <- rlang::duplicate(py_1000_rsv_mortality_sex)
rate_rsv_mortality_sex <- subset(rate_rsv_mortality_sex, rsv_mortality_inf == 1, 
                                 select = c(rsv_mortality_rate, sex))

#calculate person time and rate for flu mild outcomes by sex
py_flu_primary_sex <- as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + sex,
                                           data = df_input)[["pyears"]])
py_flu_primary_sex <- rownames_to_column(py_flu_primary_sex, "flu_primary_inf")
py_flu_primary_sex <- gather(py_flu_primary_sex, sex, pyears, -flu_primary_inf)
py_flu_primary_sex$n <- gather(as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + sex,
                                                    data = df_input)[["n"]]))$value
py_1000_flu_primary_sex <- py_flu_primary_sex %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
rate_flu_mild_sex <- rlang::duplicate(py_1000_flu_primary_sex)
rate_flu_mild_sex <- subset(rate_flu_mild_sex, flu_primary_inf == 1, 
                            select = c(flu_mild_rate, sex))

#calculate person time and rate for flu severe by sex
py_flu_secondary_sex <- as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + sex,
                                             data = df_input)[["pyears"]])
py_flu_secondary_sex <- rownames_to_column(py_flu_secondary_sex, "flu_secondary_inf")
py_flu_secondary_sex <- gather(py_flu_secondary_sex, sex, pyears, -flu_secondary_inf)
py_flu_secondary_sex$n <- gather(as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + sex,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_secondary_sex <- py_flu_secondary_sex %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
rate_flu_severe_sex <- rlang::duplicate(py_1000_flu_secondary_sex)
rate_flu_severe_sex <- subset(rate_flu_severe_sex, flu_secondary_inf == 1, 
                              select = c(flu_severe_rate, sex))

#calculate person time and rate for flu mortality by sex group
py_flu_mortality_sex <- as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + sex,
                                             data = df_input)[["pyears"]])
py_flu_mortality_sex <- rownames_to_column(py_flu_mortality_sex, "flu_mortality_inf")
py_flu_mortality_sex <- gather(py_flu_mortality_sex, sex, pyears, -flu_mortality_inf)
py_flu_mortality_sex$n <- gather(as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + sex,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_mortality_sex <- py_flu_mortality_sex %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
rate_flu_mortality_sex <- rlang::duplicate(py_1000_flu_mortality_sex)
rate_flu_mortality_sex <- subset(rate_flu_mortality_sex, flu_mortality_inf == 1, 
                                 select = c(flu_mortality_rate, sex))

if (study_start_date >= covid_season_min) {
  #calculate person time and rate for covid mild outcomes by sex
  py_covid_primary_sex <- as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + sex,
                                             data = df_input)[["pyears"]])
  py_covid_primary_sex <- rownames_to_column(py_covid_primary_sex, "covid_primary_inf")
  py_covid_primary_sex <- gather(py_covid_primary_sex, sex, pyears, -covid_primary_inf)
  py_covid_primary_sex$n <- gather(as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + sex,
                                                      data = df_input)[["n"]]))$value
  py_1000_covid_primary_sex <- py_covid_primary_sex %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  rate_covid_mild_sex <- rlang::duplicate(py_1000_covid_primary_sex)
  rate_covid_mild_sex <- subset(rate_covid_mild_sex, covid_primary_inf == 1, 
                              select = c(covid_mild_rate, sex))
  
  #calculate person time and rate for covid severe by sex 
  py_covid_secondary_sex <- as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + sex,
                                               data = df_input)[["pyears"]])
  py_covid_secondary_sex <- rownames_to_column(py_covid_secondary_sex, "covid_secondary_inf")
  py_covid_secondary_sex <- gather(py_covid_secondary_sex, sex, pyears, -covid_secondary_inf)
  py_covid_secondary_sex$n <- gather(as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + sex,
                                                        data = df_input)[["n"]]))$value
  py_1000_covid_secondary_sex <- py_covid_secondary_sex %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  rate_covid_severe_sex <- rlang::duplicate(py_1000_covid_secondary_sex)
  rate_covid_severe_sex <- subset(rate_covid_severe_sex, covid_secondary_inf == 1, 
                                select = c(covid_severe_rate, sex))
  
  #calculate person time and rate for covid mortality by sex 
  py_covid_mortality_sex <- as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + sex,
                                               data = df_input)[["pyears"]])
  py_covid_mortality_sex <- rownames_to_column(py_covid_mortality_sex, "covid_mortality_inf")
  py_covid_mortality_sex <- gather(py_covid_mortality_sex, sex, pyears, -covid_mortality_inf)
  py_covid_mortality_sex$n <- gather(as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + sex,
                                                        data = df_input)[["n"]]))$value
  py_1000_covid_mortality_sex <- py_covid_mortality_sex %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  rate_covid_mortality_sex <- rlang::duplicate(py_1000_covid_mortality_sex)
  rate_covid_mortality_sex <- subset(rate_covid_mortality_sex, covid_mortality_inf == 1, 
                                   select = c(covid_mortality_rate, sex))
}

if (codelist_type == "sensitive") {
  #calculate person time and rate for overall respiratory mild outcomes by sex
  py_overall_resp_primary_sex <- as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + sex,
                                             data = df_input)[["pyears"]])
  py_overall_resp_primary_sex <- rownames_to_column(py_overall_resp_primary_sex, "overall_resp_primary_inf")
  py_overall_resp_primary_sex <- gather(py_overall_resp_primary_sex, sex, pyears, -overall_resp_primary_inf)
  py_overall_resp_primary_sex$n <- gather(as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + sex,
                                                      data = df_input)[["n"]]))$value
  py_1000_overall_resp_primary_sex <- py_overall_resp_primary_sex %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  rate_overall_resp_mild_sex <- rlang::duplicate(py_1000_overall_resp_primary_sex)
  rate_overall_resp_mild_sex <- subset(rate_overall_resp_mild_sex, overall_resp_primary_inf == 1, 
                              select = c(overall_resp_mild_rate, sex))
  
  #calculate person time and rate for overall respiratory severe by sex
  py_overall_resp_secondary_sex <- as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + sex,
                                               data = df_input)[["pyears"]])
  py_overall_resp_secondary_sex <- rownames_to_column(py_overall_resp_secondary_sex, "overall_resp_secondary_inf")
  py_overall_resp_secondary_sex <- gather(py_overall_resp_secondary_sex, sex, pyears, -overall_resp_secondary_inf)
  py_overall_resp_secondary_sex$n <- gather(as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + sex,
                                                        data = df_input)[["n"]]))$value
  py_1000_overall_resp_secondary_sex <- py_overall_resp_secondary_sex %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  rate_overall_resp_severe_sex <- rlang::duplicate(py_1000_overall_resp_secondary_sex)
  rate_overall_resp_severe_sex <- subset(rate_overall_resp_severe_sex, overall_resp_secondary_inf == 1, 
                                select = c(overall_resp_severe_rate, sex))
  
  #calculate person time and rate for overall respiratory mortality by sex
  py_overall_resp_mortality_sex <- as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + sex,
                                               data = df_input)[["pyears"]])
  py_overall_resp_mortality_sex <- rownames_to_column(py_overall_resp_mortality_sex, "overall_resp_mortality_inf")
  py_overall_resp_mortality_sex <- gather(py_overall_resp_mortality_sex, sex, pyears, -overall_resp_mortality_inf)
  py_overall_resp_mortality_sex$n <- gather(as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + sex,
                                                        data = df_input)[["n"]]))$value
  py_1000_overall_resp_mortality_sex <- py_overall_resp_mortality_sex %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  rate_overall_resp_mortality_sex <- rlang::duplicate(py_1000_overall_resp_mortality_sex)
  rate_overall_resp_mortality_sex <- subset(rate_overall_resp_mortality_sex, overall_resp_mortality_inf == 1, 
                                   select = c(overall_resp_mortality_rate, sex))
}

#calculate person time and rate for all cause mortality by sex
py_all_cause_mortality_sex <- as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + sex,
                                                   data = df_input)[["pyears"]])
py_all_cause_mortality_sex <- rownames_to_column(py_all_cause_mortality_sex, 
                                                 "all_cause_mortality_inf")
py_all_cause_mortality_sex <- gather(py_all_cause_mortality_sex, sex, pyears,
                                     -all_cause_mortality_inf)
py_all_cause_mortality_sex$n <- gather(as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + sex,
                                                            data = df_input)[["n"]]))$value
py_1000_all_cause_mortality_sex <- py_all_cause_mortality_sex %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
rate_all_cause_mortality_sex <- rlang::duplicate(py_1000_all_cause_mortality_sex)
rate_all_cause_mortality_sex <- subset(rate_all_cause_mortality_sex, 
                                       all_cause_mortality_inf == 1, 
                                       select = c(all_cause_mortality_rate, sex))

#add these to the results table with 'Group' as sex
results <- rbind(
  results,
  data.frame(
    Outcome = c(rep("RSV mild", 2), rep("RSV severe", 2), 
                rep("RSV mortality", 2), rep("Flu mild", 2), 
                rep("Flu severe", 2), rep("Flu mortality", 2)),
    Rate = c(rate_rsv_mild_sex$rsv_mild_rate, rate_rsv_severe_sex$rsv_severe_rate, 
             rate_rsv_mortality_sex$rsv_mortality_rate, rate_flu_mild_sex$flu_mild_rate,
             rate_flu_severe_sex$flu_severe_rate, rate_flu_mortality_sex$flu_mortality_rate),
    Characteristic = rep("Sex", 12),
    Group = c(rate_rsv_mild_sex$sex, rate_rsv_severe_sex$sex, rate_rsv_mortality_sex$sex,
              rate_flu_mild_sex$sex, rate_flu_severe_sex$sex, rate_flu_mortality_sex$sex)
  )
)

if (study_start_date >= covid_season_min) {
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("COVID-19 mild", 2), rep("COVID-19 severe", 2), 
                  rep("COVID-19 mortality", 2)),
      Rate = c(rate_covid_mild_sex$covid_mild_rate, rate_covid_severe_sex$covid_severe_rate, 
               rate_covid_mortality_sex$covid_mortality_rate),
      Characteristic = rep("Sex", 6),
      Group = c(rate_covid_mild_sex$sex, rate_covid_severe_sex$sex, rate_covid_mortality_sex$sex)
    )
  )
}

if (codelist_type == "sensitive") {
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("Overall respiratory mild", 2), rep("Overall respiratory severe", 2), 
                  rep("Overall respiratory mortality", 2)),
      Rate = c(rate_overall_resp_mild_sex$overall_resp_mild_rate, 
               rate_overall_resp_severe_sex$overall_resp_severe_rate, 
               rate_overall_resp_mortality_sex$overall_resp_mortality_rate),
      Characteristic = rep("Sex", 6),
      Group = c(rate_overall_resp_mild_sex$sex, rate_overall_resp_severe_sex$sex, 
                rate_overall_resp_mortality_sex$sex)
    )
  )
}

#add all cause mortality rates to results table with 'Group' as sex
results <- rbind(
  results,
  data.frame(
    Outcome = rep("All cause mortality", 2),
    Rate = rate_all_cause_mortality_sex$all_cause_mortality_rate,
    Characteristic = rep("Sex", 2),
    Group = rate_all_cause_mortality_sex$sex
  )
)

#calculate person time and rate for rsv mild outcomes by ethnicity
py_rsv_primary_ethnicity <- as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + latest_ethnicity_group,
                                           data = df_input)[["pyears"]])
py_rsv_primary_ethnicity <- rownames_to_column(py_rsv_primary_ethnicity, "rsv_primary_inf")
py_rsv_primary_ethnicity <- gather(py_rsv_primary_ethnicity, latest_ethnicity_group, pyears, -rsv_primary_inf)
py_rsv_primary_ethnicity$n <- gather(as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + latest_ethnicity_group,
                                                    data = df_input)[["n"]]))$value
py_1000_rsv_primary_ethnicity <- py_rsv_primary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
rate_rsv_mild_ethnicity <- rlang::duplicate(py_1000_rsv_primary_ethnicity)
rate_rsv_mild_ethnicity <- subset(rate_rsv_mild_ethnicity, rsv_primary_inf == 1, 
                            select = c(rsv_mild_rate, latest_ethnicity_group))

#calculate person time and rate for rsv severe by ethnicity 
py_rsv_secondary_ethnicity <- as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + latest_ethnicity_group,
                                             data = df_input)[["pyears"]])
py_rsv_secondary_ethnicity <- rownames_to_column(py_rsv_secondary_ethnicity, "rsv_secondary_inf")
py_rsv_secondary_ethnicity <- gather(py_rsv_secondary_ethnicity, latest_ethnicity_group, pyears, -rsv_secondary_inf)
py_rsv_secondary_ethnicity$n <- gather(as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + latest_ethnicity_group,
                                                      data = df_input)[["n"]]))$value
py_1000_rsv_secondary_ethnicity <- py_rsv_secondary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
rate_rsv_severe_ethnicity <- rlang::duplicate(py_1000_rsv_secondary_ethnicity)
rate_rsv_severe_ethnicity <- subset(rate_rsv_severe_ethnicity, rsv_secondary_inf == 1, 
                              select = c(rsv_severe_rate, latest_ethnicity_group))

#calculate person time and rate for rsv mortality by ethnicity
py_rsv_mortality_ethnicity <- as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + latest_ethnicity_group,
                                             data = df_input)[["pyears"]])
py_rsv_mortality_ethnicity <- rownames_to_column(py_rsv_mortality_ethnicity, "rsv_mortality_inf")
py_rsv_mortality_ethnicity <- gather(py_rsv_mortality_ethnicity, latest_ethnicity_group, pyears, -rsv_mortality_inf)
py_rsv_mortality_ethnicity$n <- gather(as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + latest_ethnicity_group,
                                                      data = df_input)[["n"]]))$value
py_1000_rsv_mortality_ethnicity <- py_rsv_mortality_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
rate_rsv_mortality_ethnicity <- rlang::duplicate(py_1000_rsv_mortality_ethnicity)
rate_rsv_mortality_ethnicity <- subset(rate_rsv_mortality_ethnicity, rsv_mortality_inf == 1, 
                                 select = c(rsv_mortality_rate, latest_ethnicity_group))

#calculate person time and rate for flu mild outcomes by ethnicity
py_flu_primary_ethnicity <- as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + latest_ethnicity_group,
                                           data = df_input)[["pyears"]])
py_flu_primary_ethnicity <- rownames_to_column(py_flu_primary_ethnicity, "flu_primary_inf")
py_flu_primary_ethnicity <- gather(py_flu_primary_ethnicity, latest_ethnicity_group, pyears, -flu_primary_inf)
py_flu_primary_ethnicity$n <- gather(as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + latest_ethnicity_group,
                                                    data = df_input)[["n"]]))$value
py_1000_flu_primary_ethnicity <- py_flu_primary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
rate_flu_mild_ethnicity <- rlang::duplicate(py_1000_flu_primary_ethnicity)
rate_flu_mild_ethnicity <- subset(rate_flu_mild_ethnicity, flu_primary_inf == 1, 
                            select = c(flu_mild_rate, latest_ethnicity_group))

#calculate person time and rate for flu severe by ethnicity
py_flu_secondary_ethnicity <- as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + latest_ethnicity_group,
                                             data = df_input)[["pyears"]])
py_flu_secondary_ethnicity <- rownames_to_column(py_flu_secondary_ethnicity, "flu_secondary_inf")
py_flu_secondary_ethnicity <- gather(py_flu_secondary_ethnicity, latest_ethnicity_group, pyears, -flu_secondary_inf)
py_flu_secondary_ethnicity$n <- gather(as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + latest_ethnicity_group,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_secondary_ethnicity <- py_flu_secondary_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
rate_flu_severe_ethnicity <- rlang::duplicate(py_1000_flu_secondary_ethnicity)
rate_flu_severe_ethnicity <- subset(rate_flu_severe_ethnicity, flu_secondary_inf == 1, 
                              select = c(flu_severe_rate, latest_ethnicity_group))

#calculate person time and rate for flu mortality by ethnicity group
py_flu_mortality_ethnicity <- as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + latest_ethnicity_group,
                                             data = df_input)[["pyears"]])
py_flu_mortality_ethnicity <- rownames_to_column(py_flu_mortality_ethnicity, "flu_mortality_inf")
py_flu_mortality_ethnicity <- gather(py_flu_mortality_ethnicity, latest_ethnicity_group, pyears, -flu_mortality_inf)
py_flu_mortality_ethnicity$n <- gather(as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + latest_ethnicity_group,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_mortality_ethnicity <- py_flu_mortality_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
rate_flu_mortality_ethnicity <- rlang::duplicate(py_1000_flu_mortality_ethnicity)
rate_flu_mortality_ethnicity <- subset(rate_flu_mortality_ethnicity, flu_mortality_inf == 1, 
                                 select = c(flu_mortality_rate, latest_ethnicity_group))

if (study_start_date >= covid_season_min) {
  #calculate person time and rate for covid mild outcomes by ethnicity
  py_covid_primary_ethnicity <- as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + latest_ethnicity_group,
                                               data = df_input)[["pyears"]])
  py_covid_primary_ethnicity <- rownames_to_column(py_covid_primary_ethnicity, "covid_primary_inf")
  py_covid_primary_ethnicity <- gather(py_covid_primary_ethnicity, latest_ethnicity_group, pyears, -covid_primary_inf)
  py_covid_primary_ethnicity$n <- gather(as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + latest_ethnicity_group,
                                                        data = df_input)[["n"]]))$value
  py_1000_covid_primary_ethnicity <- py_covid_primary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  rate_covid_mild_ethnicity <- rlang::duplicate(py_1000_covid_primary_ethnicity)
  rate_covid_mild_ethnicity <- subset(rate_covid_mild_ethnicity, covid_primary_inf == 1, 
                                select = c(covid_mild_rate, latest_ethnicity_group))
  
  #calculate person time and rate for covid severe by ethnicity 
  py_covid_secondary_ethnicity <- as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + latest_ethnicity_group,
                                                 data = df_input)[["pyears"]])
  py_covid_secondary_ethnicity <- rownames_to_column(py_covid_secondary_ethnicity, "covid_secondary_inf")
  py_covid_secondary_ethnicity <- gather(py_covid_secondary_ethnicity, latest_ethnicity_group, pyears, -covid_secondary_inf)
  py_covid_secondary_ethnicity$n <- gather(as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + latest_ethnicity_group,
                                                          data = df_input)[["n"]]))$value
  py_1000_covid_secondary_ethnicity <- py_covid_secondary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  rate_covid_severe_ethnicity <- rlang::duplicate(py_1000_covid_secondary_ethnicity)
  rate_covid_severe_ethnicity <- subset(rate_covid_severe_ethnicity, covid_secondary_inf == 1, 
                                  select = c(covid_severe_rate, latest_ethnicity_group))
  
  #calculate person time and rate for covid mortality by ethnicity 
  py_covid_mortality_ethnicity <- as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + latest_ethnicity_group,
                                                 data = df_input)[["pyears"]])
  py_covid_mortality_ethnicity <- rownames_to_column(py_covid_mortality_ethnicity, "covid_mortality_inf")
  py_covid_mortality_ethnicity <- gather(py_covid_mortality_ethnicity, latest_ethnicity_group, pyears, -covid_mortality_inf)
  py_covid_mortality_ethnicity$n <- gather(as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + latest_ethnicity_group,
                                                          data = df_input)[["n"]]))$value
  py_1000_covid_mortality_ethnicity <- py_covid_mortality_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  rate_covid_mortality_ethnicity <- rlang::duplicate(py_1000_covid_mortality_ethnicity)
  rate_covid_mortality_ethnicity <- subset(rate_covid_mortality_ethnicity, covid_mortality_inf == 1, 
                                     select = c(covid_mortality_rate, latest_ethnicity_group))
}

if (codelist_type == "sensitive") {
  #calculate person time and rate for overall respiratory mild outcomes by ethnicity
  py_overall_resp_primary_ethnicity <- as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + latest_ethnicity_group,
                                                      data = df_input)[["pyears"]])
  py_overall_resp_primary_ethnicity <- rownames_to_column(py_overall_resp_primary_ethnicity, "overall_resp_primary_inf")
  py_overall_resp_primary_ethnicity <- gather(py_overall_resp_primary_ethnicity, latest_ethnicity_group, pyears, -overall_resp_primary_inf)
  py_overall_resp_primary_ethnicity$n <- gather(as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + latest_ethnicity_group,
                                                               data = df_input)[["n"]]))$value
  py_1000_overall_resp_primary_ethnicity <- py_overall_resp_primary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  rate_overall_resp_mild_ethnicity <- rlang::duplicate(py_1000_overall_resp_primary_ethnicity)
  rate_overall_resp_mild_ethnicity <- subset(rate_overall_resp_mild_ethnicity, overall_resp_primary_inf == 1, 
                                       select = c(overall_resp_mild_rate, latest_ethnicity_group))
  
  #calculate person time and rate for overall respiratory severe by ethnicity
  py_overall_resp_secondary_ethnicity <- as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + latest_ethnicity_group,
                                                        data = df_input)[["pyears"]])
  py_overall_resp_secondary_ethnicity <- rownames_to_column(py_overall_resp_secondary_ethnicity, "overall_resp_secondary_inf")
  py_overall_resp_secondary_ethnicity <- gather(py_overall_resp_secondary_ethnicity, latest_ethnicity_group, pyears, -overall_resp_secondary_inf)
  py_overall_resp_secondary_ethnicity$n <- gather(as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + latest_ethnicity_group,
                                                                 data = df_input)[["n"]]))$value
  py_1000_overall_resp_secondary_ethnicity <- py_overall_resp_secondary_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  rate_overall_resp_severe_ethnicity <- rlang::duplicate(py_1000_overall_resp_secondary_ethnicity)
  rate_overall_resp_severe_ethnicity <- subset(rate_overall_resp_severe_ethnicity, overall_resp_secondary_inf == 1, 
                                         select = c(overall_resp_severe_rate, latest_ethnicity_group))
  
  #calculate person time and rate for overall respiratory mortality by ethnicity
  py_overall_resp_mortality_ethnicity <- as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + latest_ethnicity_group,
                                                        data = df_input)[["pyears"]])
  py_overall_resp_mortality_ethnicity <- rownames_to_column(py_overall_resp_mortality_ethnicity, "overall_resp_mortality_inf")
  py_overall_resp_mortality_ethnicity <- gather(py_overall_resp_mortality_ethnicity, latest_ethnicity_group, pyears, -overall_resp_mortality_inf)
  py_overall_resp_mortality_ethnicity$n <- gather(as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + latest_ethnicity_group,
                                                                 data = df_input)[["n"]]))$value
  py_1000_overall_resp_mortality_ethnicity <- py_overall_resp_mortality_ethnicity %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  rate_overall_resp_mortality_ethnicity <- rlang::duplicate(py_1000_overall_resp_mortality_ethnicity)
  rate_overall_resp_mortality_ethnicity <- subset(rate_overall_resp_mortality_ethnicity, overall_resp_mortality_inf == 1, 
                                            select = c(overall_resp_mortality_rate, latest_ethnicity_group))
}

#calculate person time and rate for all cause mortality by ethnicity
py_all_cause_mortality_ethnicity <- as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + latest_ethnicity_group,
                                                   data = df_input)[["pyears"]])
py_all_cause_mortality_ethnicity <- rownames_to_column(py_all_cause_mortality_ethnicity, 
                                                 "all_cause_mortality_inf")
py_all_cause_mortality_ethnicity <- gather(py_all_cause_mortality_ethnicity, latest_ethnicity_group, pyears,
                                     -all_cause_mortality_inf)
py_all_cause_mortality_ethnicity$n <- gather(as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + latest_ethnicity_group,
                                                            data = df_input)[["n"]]))$value
py_1000_all_cause_mortality_ethnicity <- py_all_cause_mortality_ethnicity %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
rate_all_cause_mortality_ethnicity <- rlang::duplicate(py_1000_all_cause_mortality_ethnicity)
rate_all_cause_mortality_ethnicity <- subset(rate_all_cause_mortality_ethnicity, 
                                       all_cause_mortality_inf == 1, 
                                       select = c(all_cause_mortality_rate, latest_ethnicity_group))

#add these to results table with 'Group' as ethnicity
results <- rbind(
  results,
  data.frame(
    Outcome = c(rep("RSV mild", 6), rep("RSV severe", 6), rep("RSV mortality", 6), 
                rep("Flu mild", 6), rep("Flu severe", 6), rep("Flu mortality", 6)),
    Rate = c(rate_rsv_mild_ethnicity$rsv_mild_rate, 
             rate_rsv_severe_ethnicity$rsv_severe_rate, 
             rate_rsv_mortality_ethnicity$rsv_mortality_rate, 
             rate_flu_mild_ethnicity$flu_mild_rate, 
             rate_flu_severe_ethnicity$flu_severe_rate, 
             rate_flu_mortality_ethnicity$flu_mortality_rate),
    Characteristic = rep("Ethnicity", 36),
    Group = c(rate_rsv_mild_ethnicity$latest_ethnicity_group, 
              rate_rsv_severe_ethnicity$latest_ethnicity_group,
              rate_rsv_mortality_ethnicity$latest_ethnicity_group, 
              rate_flu_mild_ethnicity$latest_ethnicity_group,
              rate_flu_severe_ethnicity$latest_ethnicity_group, 
              rate_flu_mortality_ethnicity$latest_ethnicity_group)
    )
  )

if (study_start_date >= covid_season_min) {
  #add covid results to results table with 'Group' as ethnicity
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("COVID-19 mild", 6), rep("COVID-19 severe", 6), rep("COVID-19 mortality", 6)),
      Rate = c(rate_covid_mild_ethnicity$covid_mild_rate, 
               rate_covid_severe_ethnicity$covid_severe_rate, 
               rate_covid_mortality_ethnicity$covid_mortality_rate),
      Characteristic = rep("Ethnicity", 18),
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
      Characteristic = rep("Ethnicity", 18),
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
    Characteristic = rep("Ethnicity", 6),
    Group = rate_all_cause_mortality_ethnicity$latest_ethnicity_group
    )
)

#calculate person time and rate for rsv mild outcomes by socioeconomic status
py_rsv_primary_socio <- as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + imd_quintile,
                                           data = df_input)[["pyears"]])
py_rsv_primary_socio <- rownames_to_column(py_rsv_primary_socio, "rsv_primary_inf")
py_rsv_primary_socio <- gather(py_rsv_primary_socio, imd_quintile, pyears, -rsv_primary_inf)
py_rsv_primary_socio$n <- gather(as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + imd_quintile,
                                                    data = df_input)[["n"]]))$value
py_1000_rsv_primary_socio <- py_rsv_primary_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
rate_rsv_mild_socio <- rlang::duplicate(py_1000_rsv_primary_socio)
rate_rsv_mild_socio <- subset(rate_rsv_mild_socio, rsv_primary_inf == 1, 
                            select = c(rsv_mild_rate, imd_quintile))

#calculate person time and rate for rsv severe by socioeconomic status 
py_rsv_secondary_socio <- as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + imd_quintile,
                                             data = df_input)[["pyears"]])
py_rsv_secondary_socio <- rownames_to_column(py_rsv_secondary_socio, "rsv_secondary_inf")
py_rsv_secondary_socio <- gather(py_rsv_secondary_socio, imd_quintile, pyears, -rsv_secondary_inf)
py_rsv_secondary_socio$n <- gather(as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + imd_quintile,
                                                      data = df_input)[["n"]]))$value
py_1000_rsv_secondary_socio <- py_rsv_secondary_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
rate_rsv_severe_socio <- rlang::duplicate(py_1000_rsv_secondary_socio)
rate_rsv_severe_socio <- subset(rate_rsv_severe_socio, rsv_secondary_inf == 1, 
                              select = c(rsv_severe_rate, imd_quintile))

#calculate person time and rate for rsv mortality by socioeconomic status
py_rsv_mortality_socio <- as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + imd_quintile,
                                             data = df_input)[["pyears"]])
py_rsv_mortality_socio <- rownames_to_column(py_rsv_mortality_socio, "rsv_mortality_inf")
py_rsv_mortality_socio <- gather(py_rsv_mortality_socio, imd_quintile, pyears, -rsv_mortality_inf)
py_rsv_mortality_socio$n <- gather(as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + imd_quintile,
                                                      data = df_input)[["n"]]))$value
py_1000_rsv_mortality_socio <- py_rsv_mortality_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
rate_rsv_mortality_socio <- rlang::duplicate(py_1000_rsv_mortality_socio)
rate_rsv_mortality_socio <- subset(rate_rsv_mortality_socio, rsv_mortality_inf == 1, 
                                 select = c(rsv_mortality_rate, imd_quintile))

#calculate person time and rate for flu mild outcomes by socioeconomic status
py_flu_primary_socio <- as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + imd_quintile,
                                           data = df_input)[["pyears"]])
py_flu_primary_socio <- rownames_to_column(py_flu_primary_socio, "flu_primary_inf")
py_flu_primary_socio <- gather(py_flu_primary_socio, imd_quintile, pyears, -flu_primary_inf)
py_flu_primary_socio$n <- gather(as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + imd_quintile,
                                                    data = df_input)[["n"]]))$value
py_1000_flu_primary_socio <- py_flu_primary_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
rate_flu_mild_socio <- rlang::duplicate(py_1000_flu_primary_socio)
rate_flu_mild_socio <- subset(rate_flu_mild_socio, flu_primary_inf == 1, 
                            select = c(flu_mild_rate, imd_quintile))

#calculate person time and rate for flu severe by socioeconomic status
py_flu_secondary_socio <- as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + imd_quintile,
                                             data = df_input)[["pyears"]])
py_flu_secondary_socio <- rownames_to_column(py_flu_secondary_socio, "flu_secondary_inf")
py_flu_secondary_socio <- gather(py_flu_secondary_socio, imd_quintile, pyears, -flu_secondary_inf)
py_flu_secondary_socio$n <- gather(as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + imd_quintile,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_secondary_socio <- py_flu_secondary_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
rate_flu_severe_socio <- rlang::duplicate(py_1000_flu_secondary_socio)
rate_flu_severe_socio <- subset(rate_flu_severe_socio, flu_secondary_inf == 1, 
                              select = c(flu_severe_rate, imd_quintile))

#calculate person time and rate for flu mortality by socioeconomic status group
py_flu_mortality_socio <- as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + imd_quintile,
                                             data = df_input)[["pyears"]])
py_flu_mortality_socio <- rownames_to_column(py_flu_mortality_socio, "flu_mortality_inf")
py_flu_mortality_socio <- gather(py_flu_mortality_socio, imd_quintile, pyears, -flu_mortality_inf)
py_flu_mortality_socio$n <- gather(as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + imd_quintile,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_mortality_socio <- py_flu_mortality_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
rate_flu_mortality_socio <- rlang::duplicate(py_1000_flu_mortality_socio)
rate_flu_mortality_socio <- subset(rate_flu_mortality_socio, flu_mortality_inf == 1, 
                                 select = c(flu_mortality_rate, imd_quintile))

if (study_start_date >= covid_season_min) {
  #calculate person time and rate for covid mild outcomes by socioeconomic status
  py_covid_primary_socio <- as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + imd_quintile,
                                               data = df_input)[["pyears"]])
  py_covid_primary_socio <- rownames_to_column(py_covid_primary_socio, "covid_primary_inf")
  py_covid_primary_socio <- gather(py_covid_primary_socio, imd_quintile, pyears, -covid_primary_inf)
  py_covid_primary_socio$n <- gather(as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + imd_quintile,
                                                        data = df_input)[["n"]]))$value
  py_1000_covid_primary_socio <- py_covid_primary_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  rate_covid_mild_socio <- rlang::duplicate(py_1000_covid_primary_socio)
  rate_covid_mild_socio <- subset(rate_covid_mild_socio, covid_primary_inf == 1, 
                                select = c(covid_mild_rate, imd_quintile))
  
  #calculate person time and rate for covid severe by socioeconomic status 
  py_covid_secondary_socio <- as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + imd_quintile,
                                                 data = df_input)[["pyears"]])
  py_covid_secondary_socio <- rownames_to_column(py_covid_secondary_socio, "covid_secondary_inf")
  py_covid_secondary_socio <- gather(py_covid_secondary_socio, imd_quintile, pyears, -covid_secondary_inf)
  py_covid_secondary_socio$n <- gather(as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + imd_quintile,
                                                          data = df_input)[["n"]]))$value
  py_1000_covid_secondary_socio <- py_covid_secondary_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  rate_covid_severe_socio <- rlang::duplicate(py_1000_covid_secondary_socio)
  rate_covid_severe_socio <- subset(rate_covid_severe_socio, covid_secondary_inf == 1, 
                                  select = c(covid_severe_rate, imd_quintile))
  
  #calculate person time and rate for covid mortality by socioeconomic status 
  py_covid_mortality_socio <- as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + imd_quintile,
                                                 data = df_input)[["pyears"]])
  py_covid_mortality_socio <- rownames_to_column(py_covid_mortality_socio, "covid_mortality_inf")
  py_covid_mortality_socio <- gather(py_covid_mortality_socio, imd_quintile, pyears, -covid_mortality_inf)
  py_covid_mortality_socio$n <- gather(as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + imd_quintile,
                                                          data = df_input)[["n"]]))$value
  py_1000_covid_mortality_socio <- py_covid_mortality_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  rate_covid_mortality_socio <- rlang::duplicate(py_1000_covid_mortality_socio)
  rate_covid_mortality_socio <- subset(rate_covid_mortality_socio, covid_mortality_inf == 1, 
                                     select = c(covid_mortality_rate, imd_quintile))
}

if (codelist_type == "sensitive") {
  #calculate person time and rate for overall respiratory mild outcomes by socioeconomic status
  py_overall_resp_primary_socio <- as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + imd_quintile,
                                                      data = df_input)[["pyears"]])
  py_overall_resp_primary_socio <- rownames_to_column(py_overall_resp_primary_socio, "overall_resp_primary_inf")
  py_overall_resp_primary_socio <- gather(py_overall_resp_primary_socio, imd_quintile, pyears, -overall_resp_primary_inf)
  py_overall_resp_primary_socio$n <- gather(as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + imd_quintile,
                                                               data = df_input)[["n"]]))$value
  py_1000_overall_resp_primary_socio <- py_overall_resp_primary_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  rate_overall_resp_mild_socio <- rlang::duplicate(py_1000_overall_resp_primary_socio)
  rate_overall_resp_mild_socio <- subset(rate_overall_resp_mild_socio, overall_resp_primary_inf == 1, 
                                       select = c(overall_resp_mild_rate, imd_quintile))
  
  #calculate person time and rate for overall respiratory severe by socioeconomic status
  py_overall_resp_secondary_socio <- as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + imd_quintile,
                                                        data = df_input)[["pyears"]])
  py_overall_resp_secondary_socio <- rownames_to_column(py_overall_resp_secondary_socio, "overall_resp_secondary_inf")
  py_overall_resp_secondary_socio <- gather(py_overall_resp_secondary_socio, imd_quintile, pyears, -overall_resp_secondary_inf)
  py_overall_resp_secondary_socio$n <- gather(as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + imd_quintile,
                                                                 data = df_input)[["n"]]))$value
  py_1000_overall_resp_secondary_socio <- py_overall_resp_secondary_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  rate_overall_resp_severe_socio <- rlang::duplicate(py_1000_overall_resp_secondary_socio)
  rate_overall_resp_severe_socio <- subset(rate_overall_resp_severe_socio, overall_resp_secondary_inf == 1, 
                                         select = c(overall_resp_severe_rate, imd_quintile))
  
  #calculate person time and rate for overall respiratory mortality by socioeconomic status
  py_overall_resp_mortality_socio <- as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + imd_quintile,
                                                        data = df_input)[["pyears"]])
  py_overall_resp_mortality_socio <- rownames_to_column(py_overall_resp_mortality_socio, "overall_resp_mortality_inf")
  py_overall_resp_mortality_socio <- gather(py_overall_resp_mortality_socio, imd_quintile, pyears, -overall_resp_mortality_inf)
  py_overall_resp_mortality_socio$n <- gather(as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + imd_quintile,
                                                                 data = df_input)[["n"]]))$value
  py_1000_overall_resp_mortality_socio <- py_overall_resp_mortality_socio %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  rate_overall_resp_mortality_socio <- rlang::duplicate(py_1000_overall_resp_mortality_socio)
  rate_overall_resp_mortality_socio <- subset(rate_overall_resp_mortality_socio, overall_resp_mortality_inf == 1, 
                                            select = c(overall_resp_mortality_rate, imd_quintile))
}

#calculate person time and rate for all cause mortality by socioeconomic status
py_all_cause_mortality_socio <- as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + imd_quintile,
                                                   data = df_input)[["pyears"]])
py_all_cause_mortality_socio <- rownames_to_column(py_all_cause_mortality_socio, 
                                                 "all_cause_mortality_inf")
py_all_cause_mortality_socio <- gather(py_all_cause_mortality_socio, imd_quintile, pyears,
                                     -all_cause_mortality_inf)
py_all_cause_mortality_socio$n <- gather(as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + imd_quintile,
                                                            data = df_input)[["n"]]))$value
py_1000_all_cause_mortality_socio <- py_all_cause_mortality_socio %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
rate_all_cause_mortality_socio <- rlang::duplicate(py_1000_all_cause_mortality_socio)
rate_all_cause_mortality_socio <- subset(rate_all_cause_mortality_socio, 
                                       all_cause_mortality_inf == 1, 
                                       select = c(all_cause_mortality_rate, imd_quintile))

#add these to results table with 'Group' as socioeconomic status
results <- rbind(
  results,
  data.frame(
    Outcome = c(rep("RSV mild", 5), rep("RSV severe", 5), rep("RSV mortality", 5), 
                rep("Flu mild", 5), rep("Flu severe", 5), rep("Flu mortality", 5)),
    Rate = c(rate_rsv_mild_socio$rsv_mild_rate, rate_rsv_severe_socio$rsv_severe_rate, 
             rate_rsv_mortality_socio$rsv_mortality_rate, rate_flu_mild_socio$flu_mild_rate, 
             rate_flu_severe_socio$flu_severe_rate, rate_flu_mortality_socio$flu_mortality_rate),
    Characteristic = rep("IMD Quintile", 30),
    Group = c(rate_rsv_mild_socio$imd_quintile, rate_rsv_severe_socio$imd_quintile,
              rate_rsv_mortality_socio$imd_quintile, rate_flu_mild_socio$imd_quintile,
              rate_flu_severe_socio$imd_quintile, rate_flu_mortality_socio$imd_quintile)
    )
  )

if (study_start_date >= covid_season_min) {
  #add covid results to results table with 'Group' as socioeconomic status
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("COVID-19 mild", 5), rep("COVID-19 severe", 5), rep("COVID-19 mortality", 5)),
      Rate = c(rate_covid_mild_socio$covid_mild_rate, 
               rate_covid_severe_socio$covid_severe_rate, 
               rate_covid_mortality_socio$covid_mortality_rate),
      Characteristic = rep("IMD Quintile", 15),
      Group = c(rate_covid_mild_socio$imd_quintile, 
                rate_covid_severe_socio$imd_quintile,
                rate_covid_mortality_socio$imd_quintile)
    )
  )
}

if (codelist_type == "sensitive") {
  #add overall respiratory results to results table with 'Group' as socioeconomic status
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("Overall respiratory mild", 5), rep("Overall respiratory severe", 5), 
                  rep("Overall respiratory mortality", 5)),
      Rate = c(rate_overall_resp_mild_socio$overall_resp_mild_rate, 
               rate_overall_resp_severe_socio$overall_resp_severe_rate, 
               rate_overall_resp_mortality_socio$overall_resp_mortality_rate),
      Characteristic = rep("IMD Quintile", 15),
      Group = c(rate_overall_resp_mild_socio$imd_quintile, 
                rate_overall_resp_severe_socio$imd_quintile,
                rate_overall_resp_mortality_socio$imd_quintile)
    )
  )
}

#add all cause mortality to results table with 'Group' as socioeconomic status
results <- rbind(
  results,
  data.frame(
    Outcome = rep("All cause mortality", 5),
    Rate = rate_all_cause_mortality_socio$all_cause_mortality_rate,
    Characteristic = rep("IMD Quintile", 5),
    Group = rate_all_cause_mortality_socio$imd_quintile
    )
)

#calculate person time and rate for rsv mild outcomes by rurality classification
py_rsv_primary_rurality <- as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + rurality_classification,
                                           data = df_input)[["pyears"]])
py_rsv_primary_rurality <- rownames_to_column(py_rsv_primary_rurality, "rsv_primary_inf")
py_rsv_primary_rurality <- gather(py_rsv_primary_rurality, rurality_classification, pyears, -rsv_primary_inf)
py_rsv_primary_rurality$n <- gather(as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + rurality_classification,
                                                    data = df_input)[["n"]]))$value
py_1000_rsv_primary_rurality <- py_rsv_primary_rurality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mild_rate = n/pyears_1000)
rate_rsv_mild_rurality <- rlang::duplicate(py_1000_rsv_primary_rurality)
rate_rsv_mild_rurality <- subset(rate_rsv_mild_rurality, rsv_primary_inf == 1, 
                            select = c(rsv_mild_rate, rurality_classification))

#calculate person time and rate for rsv severe by rurality classification 
py_rsv_secondary_rurality <- as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + rurality_classification,
                                             data = df_input)[["pyears"]])
py_rsv_secondary_rurality <- rownames_to_column(py_rsv_secondary_rurality, "rsv_secondary_inf")
py_rsv_secondary_rurality <- gather(py_rsv_secondary_rurality, rurality_classification, pyears, -rsv_secondary_inf)
py_rsv_secondary_rurality$n <- gather(as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + rurality_classification,
                                                      data = df_input)[["n"]]))$value
py_1000_rsv_secondary_rurality <- py_rsv_secondary_rurality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_severe_rate = n/pyears_1000)
rate_rsv_severe_rurality <- rlang::duplicate(py_1000_rsv_secondary_rurality)
rate_rsv_severe_rurality <- subset(rate_rsv_severe_rurality, rsv_secondary_inf == 1, 
                              select = c(rsv_severe_rate, rurality_classification))

#calculate person time and rate for rsv mortality by rurality classification
py_rsv_mortality_rurality <- as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + rurality_classification,
                                             data = df_input)[["pyears"]])
py_rsv_mortality_rurality <- rownames_to_column(py_rsv_mortality_rurality, "rsv_mortality_inf")
py_rsv_mortality_rurality <- gather(py_rsv_mortality_rurality, rurality_classification, pyears, -rsv_mortality_inf)
py_rsv_mortality_rurality$n <- gather(as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + rurality_classification,
                                                      data = df_input)[["n"]]))$value
py_1000_rsv_mortality_rurality <- py_rsv_mortality_rurality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         rsv_mortality_rate = n/pyears_1000)
rate_rsv_mortality_rurality <- rlang::duplicate(py_1000_rsv_mortality_rurality)
rate_rsv_mortality_rurality <- subset(rate_rsv_mortality_rurality, rsv_mortality_inf == 1, 
                                 select = c(rsv_mortality_rate, rurality_classification))

#calculate person time and rate for flu mild outcomes by rurality classification
py_flu_primary_rurality <- as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + rurality_classification,
                                           data = df_input)[["pyears"]])
py_flu_primary_rurality <- rownames_to_column(py_flu_primary_rurality, "flu_primary_inf")
py_flu_primary_rurality <- gather(py_flu_primary_rurality, rurality_classification, pyears, -flu_primary_inf)
py_flu_primary_rurality$n <- gather(as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + rurality_classification,
                                                    data = df_input)[["n"]]))$value
py_1000_flu_primary_rurality <- py_flu_primary_rurality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mild_rate = n/pyears_1000)
rate_flu_mild_rurality <- rlang::duplicate(py_1000_flu_primary_rurality)
rate_flu_mild_rurality <- subset(rate_flu_mild_rurality, flu_primary_inf == 1, 
                            select = c(flu_mild_rate, rurality_classification))

#calculate person time and rate for flu severe by rurality classification
py_flu_secondary_rurality <- as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + rurality_classification,
                                             data = df_input)[["pyears"]])
py_flu_secondary_rurality <- rownames_to_column(py_flu_secondary_rurality, "flu_secondary_inf")
py_flu_secondary_rurality <- gather(py_flu_secondary_rurality, rurality_classification, pyears, -flu_secondary_inf)
py_flu_secondary_rurality$n <- gather(as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + rurality_classification,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_secondary_rurality <- py_flu_secondary_rurality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_severe_rate = n/pyears_1000)
rate_flu_severe_rurality <- rlang::duplicate(py_1000_flu_secondary_rurality)
rate_flu_severe_rurality <- subset(rate_flu_severe_rurality, flu_secondary_inf == 1, 
                              select = c(flu_severe_rate, rurality_classification))

#calculate person time and rate for flu mortality by rurality classification group
py_flu_mortality_rurality <- as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + rurality_classification,
                                             data = df_input)[["pyears"]])
py_flu_mortality_rurality <- rownames_to_column(py_flu_mortality_rurality, "flu_mortality_inf")
py_flu_mortality_rurality <- gather(py_flu_mortality_rurality, rurality_classification, pyears, -flu_mortality_inf)
py_flu_mortality_rurality$n <- gather(as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + rurality_classification,
                                                      data = df_input)[["n"]]))$value
py_1000_flu_mortality_rurality <- py_flu_mortality_rurality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         flu_mortality_rate = n/pyears_1000)
rate_flu_mortality_rurality <- rlang::duplicate(py_1000_flu_mortality_rurality)
rate_flu_mortality_rurality <- subset(rate_flu_mortality_rurality, flu_mortality_inf == 1, 
                                 select = c(flu_mortality_rate, rurality_classification))

if (study_start_date >= covid_season_min) {
  #calculate person time and rate for covid mild outcomes by rurality classification
  py_covid_primary_rurality <- as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + rurality_classification,
                                               data = df_input)[["pyears"]])
  py_covid_primary_rurality <- rownames_to_column(py_covid_primary_rurality, "covid_primary_inf")
  py_covid_primary_rurality <- gather(py_covid_primary_rurality, rurality_classification, pyears, -covid_primary_inf)
  py_covid_primary_rurality$n <- gather(as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + rurality_classification,
                                                        data = df_input)[["n"]]))$value
  py_1000_covid_primary_rurality <- py_covid_primary_rurality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mild_rate = n/pyears_1000)
  rate_covid_mild_rurality <- rlang::duplicate(py_1000_covid_primary_rurality)
  rate_covid_mild_rurality <- subset(rate_covid_mild_rurality, covid_primary_inf == 1, 
                                select = c(covid_mild_rate, rurality_classification))
  
  #calculate person time and rate for covid severe by rurality classification 
  py_covid_secondary_rurality <- as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + rurality_classification,
                                                 data = df_input)[["pyears"]])
  py_covid_secondary_rurality <- rownames_to_column(py_covid_secondary_rurality, "covid_secondary_inf")
  py_covid_secondary_rurality <- gather(py_covid_secondary_rurality, rurality_classification, pyears, -covid_secondary_inf)
  py_covid_secondary_rurality$n <- gather(as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + rurality_classification,
                                                          data = df_input)[["n"]]))$value
  py_1000_covid_secondary_rurality <- py_covid_secondary_rurality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_severe_rate = n/pyears_1000)
  rate_covid_severe_rurality <- rlang::duplicate(py_1000_covid_secondary_rurality)
  rate_covid_severe_rurality <- subset(rate_covid_severe_rurality, covid_secondary_inf == 1, 
                                  select = c(covid_severe_rate, rurality_classification))
  
  #calculate person time and rate for covid mortality by rurality classification 
  py_covid_mortality_rurality <- as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + rurality_classification,
                                                 data = df_input)[["pyears"]])
  py_covid_mortality_rurality <- rownames_to_column(py_covid_mortality_rurality, "covid_mortality_inf")
  py_covid_mortality_rurality <- gather(py_covid_mortality_rurality, rurality_classification, pyears, -covid_mortality_inf)
  py_covid_mortality_rurality$n <- gather(as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + rurality_classification,
                                                          data = df_input)[["n"]]))$value
  py_1000_covid_mortality_rurality <- py_covid_mortality_rurality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           covid_mortality_rate = n/pyears_1000)
  rate_covid_mortality_rurality <- rlang::duplicate(py_1000_covid_mortality_rurality)
  rate_covid_mortality_rurality <- subset(rate_covid_mortality_rurality, covid_mortality_inf == 1, 
                                     select = c(covid_mortality_rate, rurality_classification))
}

if (codelist_type == "sensitive") {
  #calculate person time and rate for overall respiratory mild outcomes by rurality classification
  py_overall_resp_primary_rurality <- as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + rurality_classification,
                                                      data = df_input)[["pyears"]])
  py_overall_resp_primary_rurality <- rownames_to_column(py_overall_resp_primary_rurality, "overall_resp_primary_inf")
  py_overall_resp_primary_rurality <- gather(py_overall_resp_primary_rurality, rurality_classification, pyears, -overall_resp_primary_inf)
  py_overall_resp_primary_rurality$n <- gather(as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + rurality_classification,
                                                               data = df_input)[["n"]]))$value
  py_1000_overall_resp_primary_rurality <- py_overall_resp_primary_rurality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mild_rate = n/pyears_1000)
  rate_overall_resp_mild_rurality <- rlang::duplicate(py_1000_overall_resp_primary_rurality)
  rate_overall_resp_mild_rurality <- subset(rate_overall_resp_mild_rurality, overall_resp_primary_inf == 1, 
                                       select = c(overall_resp_mild_rate, rurality_classification))
  
  #calculate person time and rate for overall respiratory severe by rurality classification
  py_overall_resp_secondary_rurality <- as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + rurality_classification,
                                                        data = df_input)[["pyears"]])
  py_overall_resp_secondary_rurality <- rownames_to_column(py_overall_resp_secondary_rurality, "overall_resp_secondary_inf")
  py_overall_resp_secondary_rurality <- gather(py_overall_resp_secondary_rurality, rurality_classification, pyears, -overall_resp_secondary_inf)
  py_overall_resp_secondary_rurality$n <- gather(as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + rurality_classification,
                                                                 data = df_input)[["n"]]))$value
  py_1000_overall_resp_secondary_rurality <- py_overall_resp_secondary_rurality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_severe_rate = n/pyears_1000)
  rate_overall_resp_severe_rurality <- rlang::duplicate(py_1000_overall_resp_secondary_rurality)
  rate_overall_resp_severe_rurality <- subset(rate_overall_resp_severe_rurality, overall_resp_secondary_inf == 1, 
                                         select = c(overall_resp_severe_rate, rurality_classification))
  
  #calculate person time and rate for overall respiratory mortality by rurality classification
  py_overall_resp_mortality_rurality <- as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + rurality_classification,
                                                        data = df_input)[["pyears"]])
  py_overall_resp_mortality_rurality <- rownames_to_column(py_overall_resp_mortality_rurality, "overall_resp_mortality_inf")
  py_overall_resp_mortality_rurality <- gather(py_overall_resp_mortality_rurality, rurality_classification, pyears, -overall_resp_mortality_inf)
  py_overall_resp_mortality_rurality$n <- gather(as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + rurality_classification,
                                                                 data = df_input)[["n"]]))$value
  py_1000_overall_resp_mortality_rurality <- py_overall_resp_mortality_rurality %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           overall_resp_mortality_rate = n/pyears_1000)
  rate_overall_resp_mortality_rurality <- rlang::duplicate(py_1000_overall_resp_mortality_rurality)
  rate_overall_resp_mortality_rurality <- subset(rate_overall_resp_mortality_rurality, overall_resp_mortality_inf == 1, 
                                            select = c(overall_resp_mortality_rate, rurality_classification))
}

#calculate person time and rate for all cause mortality by rurality classification
py_all_cause_mortality_rurality <- as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + rurality_classification,
                                                   data = df_input)[["pyears"]])
py_all_cause_mortality_rurality <- rownames_to_column(py_all_cause_mortality_rurality, 
                                                 "all_cause_mortality_inf")
py_all_cause_mortality_rurality <- gather(py_all_cause_mortality_rurality, rurality_classification, pyears,
                                     -all_cause_mortality_inf)
py_all_cause_mortality_rurality$n <- gather(as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + rurality_classification,
                                                            data = df_input)[["n"]]))$value
py_1000_all_cause_mortality_rurality <- py_all_cause_mortality_rurality %>%
  mutate(pyears_1000 = pyears/nrow(df_input)*1000,
         all_cause_mortality_rate = n/pyears_1000)
rate_all_cause_mortality_rurality <- rlang::duplicate(py_1000_all_cause_mortality_rurality)
rate_all_cause_mortality_rurality <- subset(rate_all_cause_mortality_rurality, 
                                       all_cause_mortality_inf == 1, 
                                       select = c(all_cause_mortality_rate, rurality_classification))

#add these to results table with 'Group' as rurality classification
results <- rbind(
  results,
  data.frame(
    Outcome = c(rep("RSV mild", 5), rep("RSV severe", 5), rep("RSV mortality", 5), 
                rep("Flu mild", 5), rep("Flu severe", 5), rep("Flu mortality", 5)),
    Rate = c(rate_rsv_mild_rurality$rsv_mild_rate, 
             rate_rsv_severe_rurality$rsv_severe_rate, 
             rate_rsv_mortality_rurality$rsv_mortality_rate, 
             rate_flu_mild_rurality$flu_mild_rate, 
             rate_flu_severe_rurality$flu_severe_rate, 
             rate_flu_mortality_rurality$flu_mortality_rate),
    Characteristic = rep("Rurality Classification", 30),
    Group = c(rate_rsv_mild_rurality$rurality_classification, 
              rate_rsv_severe_rurality$rurality_classification,
              rate_rsv_mortality_rurality$rurality_classification,
              rate_flu_mild_rurality$rurality_classification,
              rate_flu_severe_rurality$rurality_classification, 
              rate_flu_mortality_rurality$rurality_classification)
    )
  )

if (study_start_date >= covid_season_min) {
  #add covid results to results table with 'Group' as rurality classification
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("COVID-19 mild", 5), rep("COVID-19 severe", 5), 
                  rep("COVID-19 mortality", 5)),
      Rate = c(rate_covid_mild_rurality$covid_mild_rate, 
               rate_covid_severe_rurality$covid_severe_rate, 
               rate_covid_mortality_rurality$covid_mortality_rate),
      Characteristic = rep("Rurality Classification", 15),
      Group = c(rate_covid_mild_rurality$rurality_classification, 
                rate_covid_severe_rurality$rurality_classification,
                rate_covid_mortality_rurality$rurality_classification)
    )
  ) 
}

if (codelist_type == "sensitive") {
  #add overall respiratory results to results table with 'Group' as rurality classification
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("Overall respiratory mild", 5), 
                  rep("Overall respiratory severe", 5), 
                  rep("Overall respiratory mortality", 5)),
      Rate = c(rate_overall_resp_mild_rurality$overall_resp_mild_rate, 
               rate_overall_resp_severe_rurality$overall_resp_severe_rate, 
               rate_overall_resp_mortality_rurality$overall_resp_mortality_rate),
      Characteristic = rep("Rurality Classification", 15),
      Group = c(rate_overall_resp_mild_rurality$rurality_classification, 
                rate_overall_resp_severe_rurality$rurality_classification,
                rate_overall_resp_mortality_rurality$rurality_classification)
    )
  )
}

#add all cause mortality to results table with 'Group' as rurality classification
results <- rbind(
  results,
  data.frame(
    Outcome = rep("All cause mortality", 5),
    Rate = rate_all_cause_mortality_rurality$all_cause_mortality_rate,
    Characteristic = rep("Rurality Classification", 5),
    Group = rate_all_cause_mortality_rurality$rurality_classification
    )
)

##insert section for rates for infant subgroup

#gestational age

#maternal age

#maternal smoking status

#maternal drinking

#maternal drug usage

#maternal pertussis vaccination status

#maternal influenza vaccination status 

##children and adolescents, adult and older adult cohort specific characteristics 

if (cohort == "children_and_adolescents" | cohort == "adults" | cohort == "older_adults") {
  if (study_start_date >= covid_season_min) {
    #calculate person time and rate for rsv mild outcomes by number of vaccines received against COVID-19
    py_rsv_primary_cov_vaccines <- as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + covid_vaccination_count,
                                               data = df_input)[["pyears"]])
    py_rsv_primary_cov_vaccines <- rownames_to_column(py_rsv_primary_cov_vaccines, "rsv_primary_inf")
    py_rsv_primary_cov_vaccines <- gather(py_rsv_primary_cov_vaccines, covid_vaccination_count, pyears, -rsv_primary_inf)
    py_rsv_primary_cov_vaccines$n <- gather(as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + covid_vaccination_count,
                                                        data = df_input)[["n"]]))$value
    py_1000_rsv_primary_cov_vaccines <- py_rsv_primary_cov_vaccines %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             rsv_mild_rate = n/pyears_1000)
    rate_rsv_mild_cov_vaccines <- rlang::duplicate(py_1000_rsv_primary_cov_vaccines)
    rate_rsv_mild_cov_vaccines <- subset(rate_rsv_mild_cov_vaccines, rsv_primary_inf == 1, 
                                select = c(rsv_mild_rate, covid_vaccination_count))
    
    #calculate person time and rate for rsv severe by number of vaccines received against COVID-19 
    py_rsv_secondary_cov_vaccines <- as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + covid_vaccination_count,
                                                 data = df_input)[["pyears"]])
    py_rsv_secondary_cov_vaccines <- rownames_to_column(py_rsv_secondary_cov_vaccines, "rsv_secondary_inf")
    py_rsv_secondary_cov_vaccines <- gather(py_rsv_secondary_cov_vaccines, covid_vaccination_count, pyears, -rsv_secondary_inf)
    py_rsv_secondary_cov_vaccines$n <- gather(as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + covid_vaccination_count,
                                                          data = df_input)[["n"]]))$value
    py_1000_rsv_secondary_cov_vaccines <- py_rsv_secondary_cov_vaccines %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             rsv_severe_rate = n/pyears_1000)
    rate_rsv_severe_cov_vaccines <- rlang::duplicate(py_1000_rsv_secondary_cov_vaccines)
    rate_rsv_severe_cov_vaccines <- subset(rate_rsv_severe_cov_vaccines, rsv_secondary_inf == 1, 
                                  select = c(rsv_severe_rate, covid_vaccination_count))
    
    #calculate person time and rate for rsv mortality by number of vaccines received against COVID-19
    py_rsv_mortality_cov_vaccines <- as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + covid_vaccination_count,
                                                 data = df_input)[["pyears"]])
    py_rsv_mortality_cov_vaccines <- rownames_to_column(py_rsv_mortality_cov_vaccines, "rsv_mortality_inf")
    py_rsv_mortality_cov_vaccines <- gather(py_rsv_mortality_cov_vaccines, covid_vaccination_count, pyears, -rsv_mortality_inf)
    py_rsv_mortality_cov_vaccines$n <- gather(as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + covid_vaccination_count,
                                                          data = df_input)[["n"]]))$value
    py_1000_rsv_mortality_cov_vaccines <- py_rsv_mortality_cov_vaccines %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             rsv_mortality_rate = n/pyears_1000)
    rate_rsv_mortality_cov_vaccines <- rlang::duplicate(py_1000_rsv_mortality_cov_vaccines)
    rate_rsv_mortality_cov_vaccines <- subset(rate_rsv_mortality_cov_vaccines, rsv_mortality_inf == 1, 
                                     select = c(rsv_mortality_rate, covid_vaccination_count))
    
    #calculate person time and rate for flu mild outcomes by number of vaccines received against COVID-19
    py_flu_primary_cov_vaccines <- as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + covid_vaccination_count,
                                               data = df_input)[["pyears"]])
    py_flu_primary_cov_vaccines <- rownames_to_column(py_flu_primary_cov_vaccines, "flu_primary_inf")
    py_flu_primary_cov_vaccines <- gather(py_flu_primary_cov_vaccines, covid_vaccination_count, pyears, -flu_primary_inf)
    py_flu_primary_cov_vaccines$n <- gather(as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + covid_vaccination_count,
                                                        data = df_input)[["n"]]))$value
    py_1000_flu_primary_cov_vaccines <- py_flu_primary_cov_vaccines %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             flu_mild_rate = n/pyears_1000)
    rate_flu_mild_cov_vaccines <- rlang::duplicate(py_1000_flu_primary_cov_vaccines)
    rate_flu_mild_cov_vaccines <- subset(rate_flu_mild_cov_vaccines, flu_primary_inf == 1, 
                                select = c(flu_mild_rate, covid_vaccination_count))
    
    #calculate person time and rate for flu severe by number of vaccines received against COVID-19
    py_flu_secondary_cov_vaccines <- as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + covid_vaccination_count,
                                                 data = df_input)[["pyears"]])
    py_flu_secondary_cov_vaccines <- rownames_to_column(py_flu_secondary_cov_vaccines, "flu_secondary_inf")
    py_flu_secondary_cov_vaccines <- gather(py_flu_secondary_cov_vaccines, covid_vaccination_count, pyears, -flu_secondary_inf)
    py_flu_secondary_cov_vaccines$n <- gather(as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + covid_vaccination_count,
                                                          data = df_input)[["n"]]))$value
    py_1000_flu_secondary_cov_vaccines <- py_flu_secondary_cov_vaccines %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             flu_severe_rate = n/pyears_1000)
    rate_flu_severe_cov_vaccines <- rlang::duplicate(py_1000_flu_secondary_cov_vaccines)
    rate_flu_severe_cov_vaccines <- subset(rate_flu_severe_cov_vaccines, flu_secondary_inf == 1, 
                                  select = c(flu_severe_rate, covid_vaccination_count))
    
    #calculate person time and rate for flu mortality by number of vaccines received against COVID-19 group
    py_flu_mortality_cov_vaccines <- as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + covid_vaccination_count,
                                                 data = df_input)[["pyears"]])
    py_flu_mortality_cov_vaccines <- rownames_to_column(py_flu_mortality_cov_vaccines, "flu_mortality_inf")
    py_flu_mortality_cov_vaccines <- gather(py_flu_mortality_cov_vaccines, covid_vaccination_count, pyears, -flu_mortality_inf)
    py_flu_mortality_cov_vaccines$n <- gather(as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + covid_vaccination_count,
                                                          data = df_input)[["n"]]))$value
    py_1000_flu_mortality_cov_vaccines <- py_flu_mortality_cov_vaccines %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             flu_mortality_rate = n/pyears_1000)
    rate_flu_mortality_cov_vaccines <- rlang::duplicate(py_1000_flu_mortality_cov_vaccines)
    rate_flu_mortality_cov_vaccines <- subset(rate_flu_mortality_cov_vaccines, flu_mortality_inf == 1, 
                                     select = c(flu_mortality_rate, covid_vaccination_count))
    
    if (study_start_date >= covid_season_min) {
      #calculate person time and rate for covid mild outcomes by number of vaccines received against COVID-19
      py_covid_primary_cov_vaccines <- as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + covid_vaccination_count,
                                                   data = df_input)[["pyears"]])
      py_covid_primary_cov_vaccines <- rownames_to_column(py_covid_primary_cov_vaccines, "covid_primary_inf")
      py_covid_primary_cov_vaccines <- gather(py_covid_primary_cov_vaccines, covid_vaccination_count, pyears, -covid_primary_inf)
      py_covid_primary_cov_vaccines$n <- gather(as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + covid_vaccination_count,
                                                            data = df_input)[["n"]]))$value
      py_1000_covid_primary_cov_vaccines <- py_covid_primary_cov_vaccines %>%
        mutate(pyears_1000 = pyears/nrow(df_input)*1000,
               covid_mild_rate = n/pyears_1000)
      rate_covid_mild_cov_vaccines <- rlang::duplicate(py_1000_covid_primary_cov_vaccines)
      rate_covid_mild_cov_vaccines <- subset(rate_covid_mild_cov_vaccines, covid_primary_inf == 1, 
                                    select = c(covid_mild_rate, covid_vaccination_count))
      
      #calculate person time and rate for covid severe by number of vaccines received against COVID-19 
      py_covid_secondary_cov_vaccines <- as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + covid_vaccination_count,
                                                     data = df_input)[["pyears"]])
      py_covid_secondary_cov_vaccines <- rownames_to_column(py_covid_secondary_cov_vaccines, "covid_secondary_inf")
      py_covid_secondary_cov_vaccines <- gather(py_covid_secondary_cov_vaccines, covid_vaccination_count, pyears, -covid_secondary_inf)
      py_covid_secondary_cov_vaccines$n <- gather(as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + covid_vaccination_count,
                                                              data = df_input)[["n"]]))$value
      py_1000_covid_secondary_cov_vaccines <- py_covid_secondary_cov_vaccines %>%
        mutate(pyears_1000 = pyears/nrow(df_input)*1000,
               covid_severe_rate = n/pyears_1000)
      rate_covid_severe_cov_vaccines <- rlang::duplicate(py_1000_covid_secondary_cov_vaccines)
      rate_covid_severe_cov_vaccines <- subset(rate_covid_severe_cov_vaccines, covid_secondary_inf == 1, 
                                      select = c(covid_severe_rate, covid_vaccination_count))
      
      #calculate person time and rate for covid mortality by number of vaccines received against COVID-19 
      py_covid_mortality_cov_vaccines <- as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + covid_vaccination_count,
                                                     data = df_input)[["pyears"]])
      py_covid_mortality_cov_vaccines <- rownames_to_column(py_covid_mortality_cov_vaccines, "covid_mortality_inf")
      py_covid_mortality_cov_vaccines <- gather(py_covid_mortality_cov_vaccines, covid_vaccination_count, pyears, -covid_mortality_inf)
      py_covid_mortality_cov_vaccines$n <- gather(as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + covid_vaccination_count,
                                                              data = df_input)[["n"]]))$value
      py_1000_covid_mortality_cov_vaccines <- py_covid_mortality_cov_vaccines %>%
        mutate(pyears_1000 = pyears/nrow(df_input)*1000,
               covid_mortality_rate = n/pyears_1000)
      rate_covid_mortality_cov_vaccines <- rlang::duplicate(py_1000_covid_mortality_cov_vaccines)
      rate_covid_mortality_cov_vaccines <- subset(rate_covid_mortality_cov_vaccines, covid_mortality_inf == 1, 
                                         select = c(covid_mortality_rate, covid_vaccination_count))
    }
    
    if (codelist_type == "sensitive") {
      #calculate person time and rate for overall respiratory mild outcomes by number of vaccines received against COVID-19
      py_overall_resp_primary_cov_vaccines <- as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + covid_vaccination_count,
                                                          data = df_input)[["pyears"]])
      py_overall_resp_primary_cov_vaccines <- rownames_to_column(py_overall_resp_primary_cov_vaccines, "overall_resp_primary_inf")
      py_overall_resp_primary_cov_vaccines <- gather(py_overall_resp_primary_cov_vaccines, covid_vaccination_count, pyears, -overall_resp_primary_inf)
      py_overall_resp_primary_cov_vaccines$n <- gather(as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + covid_vaccination_count,
                                                                   data = df_input)[["n"]]))$value
      py_1000_overall_resp_primary_cov_vaccines <- py_overall_resp_primary_cov_vaccines %>%
        mutate(pyears_1000 = pyears/nrow(df_input)*1000,
               overall_resp_mild_rate = n/pyears_1000)
      rate_overall_resp_mild_cov_vaccines <- rlang::duplicate(py_1000_overall_resp_primary_cov_vaccines)
      rate_overall_resp_mild_cov_vaccines <- subset(rate_overall_resp_mild_cov_vaccines, overall_resp_primary_inf == 1, 
                                           select = c(overall_resp_mild_rate, covid_vaccination_count))
      
      #calculate person time and rate for overall respiratory severe by number of vaccines received against COVID-19
      py_overall_resp_secondary_cov_vaccines <- as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + covid_vaccination_count,
                                                            data = df_input)[["pyears"]])
      py_overall_resp_secondary_cov_vaccines <- rownames_to_column(py_overall_resp_secondary_cov_vaccines, "overall_resp_secondary_inf")
      py_overall_resp_secondary_cov_vaccines <- gather(py_overall_resp_secondary_cov_vaccines, covid_vaccination_count, pyears, -overall_resp_secondary_inf)
      py_overall_resp_secondary_cov_vaccines$n <- gather(as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + covid_vaccination_count,
                                                                     data = df_input)[["n"]]))$value
      py_1000_overall_resp_secondary_cov_vaccines <- py_overall_resp_secondary_cov_vaccines %>%
        mutate(pyears_1000 = pyears/nrow(df_input)*1000,
               overall_resp_severe_rate = n/pyears_1000)
      rate_overall_resp_severe_cov_vaccines <- rlang::duplicate(py_1000_overall_resp_secondary_cov_vaccines)
      rate_overall_resp_severe_cov_vaccines <- subset(rate_overall_resp_severe_cov_vaccines, overall_resp_secondary_inf == 1, 
                                             select = c(overall_resp_severe_rate, covid_vaccination_count))
      
      #calculate person time and rate for overall respiratory mortality by number of vaccines received against COVID-19
      py_overall_resp_mortality_cov_vaccines <- as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + covid_vaccination_count,
                                                            data = df_input)[["pyears"]])
      py_overall_resp_mortality_cov_vaccines <- rownames_to_column(py_overall_resp_mortality_cov_vaccines, "overall_resp_mortality_inf")
      py_overall_resp_mortality_cov_vaccines <- gather(py_overall_resp_mortality_cov_vaccines, covid_vaccination_count, pyears, -overall_resp_mortality_inf)
      py_overall_resp_mortality_cov_vaccines$n <- gather(as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + covid_vaccination_count,
                                                                     data = df_input)[["n"]]))$value
      py_1000_overall_resp_mortality_cov_vaccines <- py_overall_resp_mortality_cov_vaccines %>%
        mutate(pyears_1000 = pyears/nrow(df_input)*1000,
               overall_resp_mortality_rate = n/pyears_1000)
      rate_overall_resp_mortality_cov_vaccines <- rlang::duplicate(py_1000_overall_resp_mortality_cov_vaccines)
      rate_overall_resp_mortality_cov_vaccines <- subset(rate_overall_resp_mortality_cov_vaccines, overall_resp_mortality_inf == 1, 
                                                select = c(overall_resp_mortality_rate, covid_vaccination_count))
    }
    
    #calculate person time and rate for all cause mortality by number of vaccines received against COVID-19
    py_all_cause_mortality_cov_vaccines <- as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + covid_vaccination_count,
                                                       data = df_input)[["pyears"]])
    py_all_cause_mortality_cov_vaccines <- rownames_to_column(py_all_cause_mortality_cov_vaccines, 
                                                     "all_cause_mortality_inf")
    py_all_cause_mortality_cov_vaccines <- gather(py_all_cause_mortality_cov_vaccines, covid_vaccination_count, pyears,
                                         -all_cause_mortality_inf)
    py_all_cause_mortality_cov_vaccines$n <- gather(as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + covid_vaccination_count,
                                                                data = df_input)[["n"]]))$value
    py_1000_all_cause_mortality_cov_vaccines <- py_all_cause_mortality_cov_vaccines %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             all_cause_mortality_rate = n/pyears_1000)
    rate_all_cause_mortality_cov_vaccines <- rlang::duplicate(py_1000_all_cause_mortality_cov_vaccines)
    rate_all_cause_mortality_cov_vaccines <- subset(rate_all_cause_mortality_cov_vaccines, 
                                           all_cause_mortality_inf == 1, 
                                           select = c(all_cause_mortality_rate, covid_vaccination_count))
    
    #add these to results table with 'Group' as number of vaccines received against COVID-19
    results <- rbind(
      results,
      data.frame(
        Outcome = c(rep("RSV mild", 5), rep("RSV severe", 5), rep("RSV mortality", 5), 
                    rep("Flu mild", 5), rep("Flu severe", 5), rep("Flu mortality", 5),
                    rep("COVID-19 mild", 5), rep("COVID-19 severe", 5), 
                    rep("COVID-19 mortality", 5)),
        Rate = c(rate_rsv_mild_cov_vaccines$rsv_mild_rate, 
                 rate_rsv_severe_cov_vaccines$rsv_severe_rate, 
                 rate_rsv_mortality_cov_vaccines$rsv_mortality_rate, 
                 rate_flu_mild_cov_vaccines$flu_mild_rate, 
                 rate_flu_severe_cov_vaccines$flu_severe_rate, 
                 rate_flu_mortality_cov_vaccines$flu_mortality_rate,
                 rate_covid_mild_cov_vaccines$covid_mild_rate,
                 rate_covid_severe_cov_vaccines$covid_severe_rate,
                 rate_covid_mortality_cov_vaccines$covid_mortality_rate),
        Characteristic = rep("Number of vaccines received against COVID-19", 45),
        Group = c(rate_rsv_mild_cov_vaccines$covid_vaccination_count, 
                  rate_rsv_severe_cov_vaccines$covid_vaccination_count,
                  rate_rsv_mortality_cov_vaccines$covid_vaccination_count,
                  rate_flu_mild_cov_vaccines$covid_vaccination_count,
                  rate_flu_severe_cov_vaccines$covid_vaccination_count, 
                  rate_flu_mortality_cov_vaccines$covid_vaccination_count,
                  rate_covid_mild_cov_vaccines$covid_vaccination_count,
                  rate_covid_severe_cov_vaccines$covid_vaccination_count,
                  rate_covid_mortality_cov_vaccines$covid_vaccination_count)
      )
    )
    
    if (codelist_type == "sensitive") {
      #add overall respiratory results to results table with 'Group' as number of vaccines received against COVID-19
      results <- rbind(
        results,
        data.frame(
          Outcome = c(rep("Overall respiratory mild", 5), 
                      rep("Overall respiratory severe", 5), 
                      rep("Overall respiratory mortality", 5)),
          Rate = c(rate_overall_resp_mild_cov_vaccines$overall_resp_mild_rate, 
                   rate_overall_resp_severe_cov_vaccines$overall_resp_severe_rate, 
                   rate_overall_resp_mortality_cov_vaccines$overall_resp_mortality_rate),
          Characteristic = rep("Number of vaccines received against COVID-19", 15),
          Group = c(rate_overall_resp_mild_cov_vaccines$covid_vaccination_count, 
                    rate_overall_resp_severe_cov_vaccines$covid_vaccination_count,
                    rate_overall_resp_mortality_cov_vaccines$covid_vaccination_count)
        )
      )
    }
    
    #add all cause mortality to results table with 'Group' as number of vaccines received against COVID-19
    results <- rbind(
      results,
      data.frame(
        Outcome = rep("All cause mortality", 5),
        Rate = rate_all_cause_mortality_cov_vaccines$all_cause_mortality_rate,
        Characteristic = rep("Number of vaccines received against COVID-19", 5),
        Group = rate_all_cause_mortality_cov_vaccines$covid_vaccination_count
      )
    )
  }
  
  #calculate person time and rate for rsv mild outcomes by vaccinated against influenza in a season
  py_rsv_primary_flu_vacc <- as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + flu_vaccination,
                                             data = df_input)[["pyears"]])
  py_rsv_primary_flu_vacc <- rownames_to_column(py_rsv_primary_flu_vacc, "rsv_primary_inf")
  py_rsv_primary_flu_vacc <- gather(py_rsv_primary_flu_vacc, flu_vaccination, pyears, -rsv_primary_inf)
  py_rsv_primary_flu_vacc$n <- gather(as.data.frame(pyears(time_rsv_primary ~ rsv_primary_inf + flu_vaccination,
                                                      data = df_input)[["n"]]))$value
  py_1000_rsv_primary_flu_vacc <- py_rsv_primary_flu_vacc %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           rsv_mild_rate = n/pyears_1000)
  rate_rsv_mild_flu_vacc <- rlang::duplicate(py_1000_rsv_primary_flu_vacc)
  rate_rsv_mild_flu_vacc <- subset(rate_rsv_mild_flu_vacc, rsv_primary_inf == 1, 
                              select = c(rsv_mild_rate, flu_vaccination))
  
  #calculate person time and rate for rsv severe by vaccinated against influenza in a season 
  py_rsv_secondary_flu_vacc <- as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + flu_vaccination,
                                               data = df_input)[["pyears"]])
  py_rsv_secondary_flu_vacc <- rownames_to_column(py_rsv_secondary_flu_vacc, "rsv_secondary_inf")
  py_rsv_secondary_flu_vacc <- gather(py_rsv_secondary_flu_vacc, flu_vaccination, pyears, -rsv_secondary_inf)
  py_rsv_secondary_flu_vacc$n <- gather(as.data.frame(pyears(time_rsv_secondary ~ rsv_secondary_inf + flu_vaccination,
                                                        data = df_input)[["n"]]))$value
  py_1000_rsv_secondary_flu_vacc <- py_rsv_secondary_flu_vacc %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           rsv_severe_rate = n/pyears_1000)
  rate_rsv_severe_flu_vacc <- rlang::duplicate(py_1000_rsv_secondary_flu_vacc)
  rate_rsv_severe_flu_vacc <- subset(rate_rsv_severe_flu_vacc, rsv_secondary_inf == 1, 
                                select = c(rsv_severe_rate, flu_vaccination))
  
  #calculate person time and rate for rsv mortality by vaccinated against influenza in a season
  py_rsv_mortality_flu_vacc <- as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + flu_vaccination,
                                               data = df_input)[["pyears"]])
  py_rsv_mortality_flu_vacc <- rownames_to_column(py_rsv_mortality_flu_vacc, "rsv_mortality_inf")
  py_rsv_mortality_flu_vacc <- gather(py_rsv_mortality_flu_vacc, flu_vaccination, pyears, -rsv_mortality_inf)
  py_rsv_mortality_flu_vacc$n <- gather(as.data.frame(pyears(time_rsv_mortality ~ rsv_mortality_inf + flu_vaccination,
                                                        data = df_input)[["n"]]))$value
  py_1000_rsv_mortality_flu_vacc <- py_rsv_mortality_flu_vacc %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           rsv_mortality_rate = n/pyears_1000)
  rate_rsv_mortality_flu_vacc <- rlang::duplicate(py_1000_rsv_mortality_flu_vacc)
  rate_rsv_mortality_flu_vacc <- subset(rate_rsv_mortality_flu_vacc, rsv_mortality_inf == 1, 
                                   select = c(rsv_mortality_rate, flu_vaccination))
  
  #calculate person time and rate for flu mild outcomes by vaccinated against influenza in a season
  py_flu_primary_flu_vacc <- as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + flu_vaccination,
                                             data = df_input)[["pyears"]])
  py_flu_primary_flu_vacc <- rownames_to_column(py_flu_primary_flu_vacc, "flu_primary_inf")
  py_flu_primary_flu_vacc <- gather(py_flu_primary_flu_vacc, flu_vaccination, pyears, -flu_primary_inf)
  py_flu_primary_flu_vacc$n <- gather(as.data.frame(pyears(time_flu_primary ~ flu_primary_inf + flu_vaccination,
                                                      data = df_input)[["n"]]))$value
  py_1000_flu_primary_flu_vacc <- py_flu_primary_flu_vacc %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           flu_mild_rate = n/pyears_1000)
  rate_flu_mild_flu_vacc <- rlang::duplicate(py_1000_flu_primary_flu_vacc)
  rate_flu_mild_flu_vacc <- subset(rate_flu_mild_flu_vacc, flu_primary_inf == 1, 
                              select = c(flu_mild_rate, flu_vaccination))
  
  #calculate person time and rate for flu severe by vaccinated against influenza in a season
  py_flu_secondary_flu_vacc <- as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + flu_vaccination,
                                               data = df_input)[["pyears"]])
  py_flu_secondary_flu_vacc <- rownames_to_column(py_flu_secondary_flu_vacc, "flu_secondary_inf")
  py_flu_secondary_flu_vacc <- gather(py_flu_secondary_flu_vacc, flu_vaccination, pyears, -flu_secondary_inf)
  py_flu_secondary_flu_vacc$n <- gather(as.data.frame(pyears(time_flu_secondary ~ flu_secondary_inf + flu_vaccination,
                                                        data = df_input)[["n"]]))$value
  py_1000_flu_secondary_flu_vacc <- py_flu_secondary_flu_vacc %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           flu_severe_rate = n/pyears_1000)
  rate_flu_severe_flu_vacc <- rlang::duplicate(py_1000_flu_secondary_flu_vacc)
  rate_flu_severe_flu_vacc <- subset(rate_flu_severe_flu_vacc, flu_secondary_inf == 1, 
                                select = c(flu_severe_rate, flu_vaccination))
  
  #calculate person time and rate for flu mortality by vaccinated against influenza in a season group
  py_flu_mortality_flu_vacc <- as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + flu_vaccination,
                                               data = df_input)[["pyears"]])
  py_flu_mortality_flu_vacc <- rownames_to_column(py_flu_mortality_flu_vacc, "flu_mortality_inf")
  py_flu_mortality_flu_vacc <- gather(py_flu_mortality_flu_vacc, flu_vaccination, pyears, -flu_mortality_inf)
  py_flu_mortality_flu_vacc$n <- gather(as.data.frame(pyears(time_flu_mortality ~ flu_mortality_inf + flu_vaccination,
                                                        data = df_input)[["n"]]))$value
  py_1000_flu_mortality_flu_vacc <- py_flu_mortality_flu_vacc %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           flu_mortality_rate = n/pyears_1000)
  rate_flu_mortality_flu_vacc <- rlang::duplicate(py_1000_flu_mortality_flu_vacc)
  rate_flu_mortality_flu_vacc <- subset(rate_flu_mortality_flu_vacc, flu_mortality_inf == 1, 
                                   select = c(flu_mortality_rate, flu_vaccination))
  
  if (study_start_date >= covid_season_min) {
    #calculate person time and rate for covid mild outcomes by vaccinated against influenza in a season
    py_covid_primary_flu_vacc <- as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + flu_vaccination,
                                                 data = df_input)[["pyears"]])
    py_covid_primary_flu_vacc <- rownames_to_column(py_covid_primary_flu_vacc, "covid_primary_inf")
    py_covid_primary_flu_vacc <- gather(py_covid_primary_flu_vacc, flu_vaccination, pyears, -covid_primary_inf)
    py_covid_primary_flu_vacc$n <- gather(as.data.frame(pyears(time_covid_primary ~ covid_primary_inf + flu_vaccination,
                                                          data = df_input)[["n"]]))$value
    py_1000_covid_primary_flu_vacc <- py_covid_primary_flu_vacc %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             covid_mild_rate = n/pyears_1000)
    rate_covid_mild_flu_vacc <- rlang::duplicate(py_1000_covid_primary_flu_vacc)
    rate_covid_mild_flu_vacc <- subset(rate_covid_mild_flu_vacc, covid_primary_inf == 1, 
                                  select = c(covid_mild_rate, flu_vaccination))
    
    #calculate person time and rate for covid severe by vaccinated against influenza in a season 
    py_covid_secondary_flu_vacc <- as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + flu_vaccination,
                                                   data = df_input)[["pyears"]])
    py_covid_secondary_flu_vacc <- rownames_to_column(py_covid_secondary_flu_vacc, "covid_secondary_inf")
    py_covid_secondary_flu_vacc <- gather(py_covid_secondary_flu_vacc, flu_vaccination, pyears, -covid_secondary_inf)
    py_covid_secondary_flu_vacc$n <- gather(as.data.frame(pyears(time_covid_secondary ~ covid_secondary_inf + flu_vaccination,
                                                            data = df_input)[["n"]]))$value
    py_1000_covid_secondary_flu_vacc <- py_covid_secondary_flu_vacc %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             covid_severe_rate = n/pyears_1000)
    rate_covid_severe_flu_vacc <- rlang::duplicate(py_1000_covid_secondary_flu_vacc)
    rate_covid_severe_flu_vacc <- subset(rate_covid_severe_flu_vacc, covid_secondary_inf == 1, 
                                    select = c(covid_severe_rate, flu_vaccination))
    
    #calculate person time and rate for covid mortality by vaccinated against influenza in a season 
    py_covid_mortality_flu_vacc <- as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + flu_vaccination,
                                                   data = df_input)[["pyears"]])
    py_covid_mortality_flu_vacc <- rownames_to_column(py_covid_mortality_flu_vacc, "covid_mortality_inf")
    py_covid_mortality_flu_vacc <- gather(py_covid_mortality_flu_vacc, flu_vaccination, pyears, -covid_mortality_inf)
    py_covid_mortality_flu_vacc$n <- gather(as.data.frame(pyears(time_covid_mortality ~ covid_mortality_inf + flu_vaccination,
                                                            data = df_input)[["n"]]))$value
    py_1000_covid_mortality_flu_vacc <- py_covid_mortality_flu_vacc %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             covid_mortality_rate = n/pyears_1000)
    rate_covid_mortality_flu_vacc <- rlang::duplicate(py_1000_covid_mortality_flu_vacc)
    rate_covid_mortality_flu_vacc <- subset(rate_covid_mortality_flu_vacc, covid_mortality_inf == 1, 
                                       select = c(covid_mortality_rate, flu_vaccination))
  }
  
  if (codelist_type == "sensitive") {
    #calculate person time and rate for overall respiratory mild outcomes by vaccinated against influenza in a season
    py_overall_resp_primary_flu_vacc <- as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + flu_vaccination,
                                                        data = df_input)[["pyears"]])
    py_overall_resp_primary_flu_vacc <- rownames_to_column(py_overall_resp_primary_flu_vacc, "overall_resp_primary_inf")
    py_overall_resp_primary_flu_vacc <- gather(py_overall_resp_primary_flu_vacc, flu_vaccination, pyears, -overall_resp_primary_inf)
    py_overall_resp_primary_flu_vacc$n <- gather(as.data.frame(pyears(time_overall_resp_primary ~ overall_resp_primary_inf + flu_vaccination,
                                                                 data = df_input)[["n"]]))$value
    py_1000_overall_resp_primary_flu_vacc <- py_overall_resp_primary_flu_vacc %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             overall_resp_mild_rate = n/pyears_1000)
    rate_overall_resp_mild_flu_vacc <- rlang::duplicate(py_1000_overall_resp_primary_flu_vacc)
    rate_overall_resp_mild_flu_vacc <- subset(rate_overall_resp_mild_flu_vacc, overall_resp_primary_inf == 1, 
                                         select = c(overall_resp_mild_rate, flu_vaccination))
    
    #calculate person time and rate for overall respiratory severe by vaccinated against influenza in a season
    py_overall_resp_secondary_flu_vacc <- as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + flu_vaccination,
                                                          data = df_input)[["pyears"]])
    py_overall_resp_secondary_flu_vacc <- rownames_to_column(py_overall_resp_secondary_flu_vacc, "overall_resp_secondary_inf")
    py_overall_resp_secondary_flu_vacc <- gather(py_overall_resp_secondary_flu_vacc, flu_vaccination, pyears, -overall_resp_secondary_inf)
    py_overall_resp_secondary_flu_vacc$n <- gather(as.data.frame(pyears(time_overall_resp_secondary ~ overall_resp_secondary_inf + flu_vaccination,
                                                                   data = df_input)[["n"]]))$value
    py_1000_overall_resp_secondary_flu_vacc <- py_overall_resp_secondary_flu_vacc %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             overall_resp_severe_rate = n/pyears_1000)
    rate_overall_resp_severe_flu_vacc <- rlang::duplicate(py_1000_overall_resp_secondary_flu_vacc)
    rate_overall_resp_severe_flu_vacc <- subset(rate_overall_resp_severe_flu_vacc, overall_resp_secondary_inf == 1, 
                                           select = c(overall_resp_severe_rate, flu_vaccination))
    
    #calculate person time and rate for overall respiratory mortality by vaccinated against influenza in a season
    py_overall_resp_mortality_flu_vacc <- as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + flu_vaccination,
                                                          data = df_input)[["pyears"]])
    py_overall_resp_mortality_flu_vacc <- rownames_to_column(py_overall_resp_mortality_flu_vacc, "overall_resp_mortality_inf")
    py_overall_resp_mortality_flu_vacc <- gather(py_overall_resp_mortality_flu_vacc, flu_vaccination, pyears, -overall_resp_mortality_inf)
    py_overall_resp_mortality_flu_vacc$n <- gather(as.data.frame(pyears(time_overall_resp_mortality ~ overall_resp_mortality_inf + flu_vaccination,
                                                                   data = df_input)[["n"]]))$value
    py_1000_overall_resp_mortality_flu_vacc <- py_overall_resp_mortality_flu_vacc %>%
      mutate(pyears_1000 = pyears/nrow(df_input)*1000,
             overall_resp_mortality_rate = n/pyears_1000)
    rate_overall_resp_mortality_flu_vacc <- rlang::duplicate(py_1000_overall_resp_mortality_flu_vacc)
    rate_overall_resp_mortality_flu_vacc <- subset(rate_overall_resp_mortality_flu_vacc, overall_resp_mortality_inf == 1, 
                                              select = c(overall_resp_mortality_rate, flu_vaccination))
  }
  
  #calculate person time and rate for all cause mortality by vaccinated against influenza in a season
  py_all_cause_mortality_flu_vacc <- as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + flu_vaccination,
                                                     data = df_input)[["pyears"]])
  py_all_cause_mortality_flu_vacc <- rownames_to_column(py_all_cause_mortality_flu_vacc, 
                                                   "all_cause_mortality_inf")
  py_all_cause_mortality_flu_vacc <- gather(py_all_cause_mortality_flu_vacc, flu_vaccination, pyears,
                                       -all_cause_mortality_inf)
  py_all_cause_mortality_flu_vacc$n <- gather(as.data.frame(pyears(time_all_cause_mortality ~ all_cause_mortality_inf + flu_vaccination,
                                                              data = df_input)[["n"]]))$value
  py_1000_all_cause_mortality_flu_vacc <- py_all_cause_mortality_flu_vacc %>%
    mutate(pyears_1000 = pyears/nrow(df_input)*1000,
           all_cause_mortality_rate = n/pyears_1000)
  rate_all_cause_mortality_flu_vacc <- rlang::duplicate(py_1000_all_cause_mortality_flu_vacc)
  rate_all_cause_mortality_flu_vacc <- subset(rate_all_cause_mortality_flu_vacc, 
                                         all_cause_mortality_inf == 1, 
                                         select = c(all_cause_mortality_rate, flu_vaccination))
  
  #add these to results table with 'Group' as vaccinated against influenza in season
  results <- rbind(
    results,
    data.frame(
      Outcome = c(rep("RSV mild", 2), rep("RSV severe", 2), rep("RSV mortality", 2), 
                  rep("Flu mild", 2), rep("Flu severe", 2), rep("Flu mortality", 2)),
      Rate = c(rate_rsv_mild_flu_vacc$rsv_mild_rate, 
               rate_rsv_severe_flu_vacc$rsv_severe_rate, 
               rate_rsv_mortality_flu_vacc$rsv_mortality_rate, 
               rate_flu_mild_flu_vacc$flu_mild_rate, 
               rate_flu_severe_flu_vacc$flu_severe_rate, 
               rate_flu_mortality_flu_vacc$flu_mortality_rate),
      Characteristic = rep("Vaccinated against Influenza in season", 12),
      Group = c(rate_rsv_mild_flu_vacc$flu_vaccination, 
                rate_rsv_severe_flu_vacc$flu_vaccination,
                rate_rsv_mortality_flu_vacc$flu_vaccination,
                rate_flu_mild_flu_vacc$flu_vaccination,
                rate_flu_severe_flu_vacc$flu_vaccination, 
                rate_flu_mortality_flu_vacc$flu_vaccination)
    )
  )
  
  if (study_start_date >= covid_season_min) {
    #add covid results to results table with 'Group' as vaccinated against influenza in season
    results <- rbind(
      results,
      data.frame(
        Outcome = c(rep("COVID-19 mild", 2), rep("COVID-19 severe", 2), 
                    rep("COVID-19 mortality", 2)),
        Rate = c(rate_covid_mild_flu_vacc$covid_mild_rate,
                 rate_covid_severe_flu_vacc$covid_severe_rate,
                 rate_covid_mortality_flu_vacc$covid_mortality_rate),
        Characteristic = rep("Vaccinated against Influenza in season", 6),
        Group = c(rate_covid_mild_flu_vacc$flu_vaccination,
                  rate_covid_severe_flu_vacc$flu_vaccination,
                  rate_covid_mortality_flu_vacc$flu_vaccination)
      )
    )
  }
  
  if (codelist_type == "sensitive") {
    #add overall respiratory results to results table with 'Group' as vaccinated against influenza in season
    results <- rbind(
      results,
      data.frame(
        Outcome = c(rep("Overall respiratory mild", 2), 
                    rep("Overall respiratory severe", 2), 
                    rep("Overall respiratory mortality", 2)),
        Rate = c(rate_overall_resp_mild_flu_vacc$overall_resp_mild_rate, 
                 rate_overall_resp_severe_flu_vacc$overall_resp_severe_rate, 
                 rate_overall_resp_mortality_flu_vacc$overall_resp_mortality_rate),
        Characteristic = rep("Vaccinated against Influenza in season", 6),
        Group = c(rate_overall_resp_mild_flu_vacc$flu_vaccination, 
                  rate_overall_resp_severe_flu_vacc$flu_vaccination,
                  rate_overall_resp_mortality_flu_vacc$flu_vaccination)
      )
    )
  }
  
  #add all cause mortality to results table with 'Group' as vaccinated against influenza in season
  results <- rbind(
    results,
    data.frame(
      Outcome = rep("All cause mortality", 2),
      Rate = rate_all_cause_mortality_flu_vacc$all_cause_mortality_rate,
      Characteristic = rep("Vaccinated against Influenza in season", 2),
      Group = rate_all_cause_mortality_flu_vacc$flu_vaccination
    )
  )
}

## create output directories ----
fs::dir_create(here("output", "results"))

#export

if (cohort == "infants") {
  table_groups = c("Total", "Age", "Sex", "Ethnicity", "IMD Quintile",
                   "Rurality Classification")
} else if (cohort == "infants_subgroup") {
  table_groups = c("Total", "Age", "Sex", "Ethnicity", "IMD Quintile",
                   "Rurality Classification", "Gestational Age", "Maternal Age",
                   "Maternal Smoking Status", "Maternal Drinking", "Maternal Drug Usage",
                   "Maternal Pertussis Vaccination Status", 
                   "Maternal Influenza Vaccination Status")
} else {
  if (study_start_date >= covid_season_min) {
    table_groups = c("Total", "Age", "Sex", "Ethnicity", "IMD Quintile",
                     "Rurality Classification",
                     "Number of vaccines received against COVID-19",
                     "Vaccinated against Influenza in season")
  } else {
    table_groups = c("Total", "Age", "Sex", "Ethnicity", "IMD Quintile",
                     "Rurality Classification",
                     "Vaccinated against Influenza in season")
  }
} 

#create gtsummary table which displays rate by group for each outcome type
results %>%
  mutate_if(is.numeric, round, digits = 4) %>%
  select(Outcome, Group, Characteristic, Rate) %>%
  spread(key = Outcome, value = Rate) %>%
  group_by(Characteristic) %>%
  gt(groupname_col = "Characteristic") %>%
  row_group_order(groups = c(table_groups)) %>%
  tab_header(
    title = "Rate per 1000 person-years of outcomes by characteristic",
    subtitle = "Group-wise breakdown"
  ) %>%
  gtsave(filename = paste0("results1_", cohort, "_", year(study_start_date),
                               "_", year(study_end_date), "_",
                           codelist_type, ".html"), 
         path = here::here("output", "results"))

if (length(args) == 0) {
  #export results table to csv
  results_table <- results %>%
    mutate_if(is.numeric, round, digits = 4) %>%
    select(Outcome, Group, Characteristic, Rate) %>%
    spread(key = Outcome, value = Rate) %>%
    group_by(Characteristic) %>%
    gt(groupname_col = "Characteristic") %>%
    row_group_order(groups = c(table_groups)) %>%
    tab_header(
      title = "Rate per 1000 person-years of outcomes by characteristic",
      subtitle = "Group-wise breakdown"
    )
  results_table_frame <- as.data.frame(results_table) %>%
    write_csv(file = paste0(here::here("output", "results"), "/", 
              "results1_", cohort, "_", year(study_start_date), "_", 
              year(study_end_date), "_", codelist_type, ".csv"))
} else {
  #export results table to csv
  results_table <- results %>%
    mutate_if(is.numeric, round, digits = 4) %>%
    select(Outcome, Group, Characteristic, Rate) %>%
    spread(key = Outcome, value = Rate) %>%
    group_by(Characteristic) %>%
    gt(groupname_col = "Characteristic") %>%
    row_group_order(groups = c(table_groups)) %>%
    tab_header(
      title = "Rate per 1000 person-years of outcomes by characteristic",
      subtitle = "Group-wise breakdown"
    )
  results_table_frame <- as.data.frame(results_table) %>%
      write_csv(path = paste0(here::here("output", "results"), "/", "results1_",
                              cohort, "_", year(study_start_date), "_",
                              year(study_end_date), "_", codelist_type, ".csv"))
}
