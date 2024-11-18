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

## create output directories ----
fs::dir_create(here::here("output", "collated", "secondary"))

##model outputs

#import covid results table by cohort
#(secondary investigation, specific phenotypes)
collated_model_outputs_covid_further = rbind(
  read_csv(here::here("output", "results", "models", "covid_secondary", 
                      paste0("further_covid_ethnicity_model_outputs_", 
                             cohort, "_2020_2021_specific_secondary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", "covid_secondary", 
                      paste0("further_covid_ses_model_outputs_", 
                             cohort, "_2020_2021_specific_secondary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", "covid_secondary", 
                      paste0("further_covid_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_secondary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", "covid_secondary", 
                      paste0("further_covid_ethnicity_ses_model_outputs_", 
                             cohort, "_2020_2021_specific_secondary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", "covid_secondary", 
                      paste0("further_covid_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_secondary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", "covid_secondary", 
                      paste0("further_covid_ses_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_secondary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", "covid_secondary", 
                      paste0("further_covid_full_model_outputs_", 
                             cohort, "_2020_2021_specific_secondary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2020_21")
)

#save as csv
write_csv(collated_model_outputs_covid_further, 
          paste0(here::here("output", "collated", "secondary"),
                 "/", cohort, "_further_covid_model_outputs_collated_secondary.csv"))
