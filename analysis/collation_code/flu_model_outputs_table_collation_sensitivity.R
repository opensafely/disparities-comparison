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
fs::dir_create(here::here("output", "collated", "analytic", "sensitivity"))

##model outputs

#import flu results table by cohort 
#(sensitivity investigation, specific phenotypes)
collated_model_outputs_flu = rbind(
  read_csv(here::here("output", "results", "models", "flu_sensitivity", 
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", "flu_sensitivity", 
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", "flu_sensitivity", 
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_2018_2019_specific_sensitivity.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "sensitivity", subset = "2018_19")
)

#save as csv
write_csv(collated_model_outputs_flu, 
          paste0(here::here("output", "collated", "analytic", "sensitivity"),
                 "/", cohort, "_flu_model_outputs_collated_sensitivity.csv"))
