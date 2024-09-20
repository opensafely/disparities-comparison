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
fs::dir_create(here("output", "collated", "sensitivity"))

##model outputs

#import overall and all cause results table by cohort 
#(sensitivity investigation, specific and sensitive phenotypes)
collated_model_outputs_overall_and_all_cause = rbind(
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2016_2017_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2016_2017_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2016_2017_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2016_2017_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2017_2018_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2017_2018_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_specific_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2017_2018_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2017_2018_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2019_2020_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2019_2020_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_specific_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2019_2020_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2019_2020_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2020_2021_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2020_2021_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2020_2021_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2020_2021_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2021_2022_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2021_2022_specific_sensitivity.csv")))
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_specific_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2021_2022_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2021_2022_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2022_2023_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2022_2023_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_specific_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2022_2023_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2022_2023_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2016_2017_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2016_2017_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2016_2017_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2016_2017_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2017_2018_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2017_2018_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_sensitive_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2017_2018_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2017_2018_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2018_2019_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2018_2019_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_sensitive_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2018_2019_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2018_2019_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2019_2020_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2019_2020_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_sensitive_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2019_2020_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2019_2020_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2020_2021_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2020_2021_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_sensitive_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2020_2021_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2020_2021_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2021_2022_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2021_2022_sensitive_sensitivity.csv")))
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_sensitive_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2021_2022_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2021_2022_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_model_outputs_", 
                             cohort, "_2022_2023_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_model_outputs_", 
                             cohort, "_2022_2023_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_sensitive_sensitivity.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
                             cohort, "_2022_2023_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("overall_and_all_cause_full_model_outputs_", 
                             cohort, "_2022_2023_sensitive_sensitivity.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "sensitivity", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs_overall_and_all_cause, 
          paste0(here::here("output", "collated", "sensitivity"),
          "/", cohort, "_overall_and_all_cause_model_outputs_collated_sensitivity.csv"))
