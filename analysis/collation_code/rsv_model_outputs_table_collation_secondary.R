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
fs::dir_create(here::here("output", "collated", "analytic", "secondary"))

##model outputs

#import rsv results table by cohort
#(secondary investigation, specific phenotypes)
collated_model_outputs_rsv = rbind(
  read_csv(here::here("output", "results", "models", "rsv_secondary", 
                      paste0("rsv_ethnicity_model_outputs_", 
                             cohort, "_2017_2018_specific_secondary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", "rsv_secondary", 
                      paste0("rsv_ses_model_outputs_", 
                             cohort, "_2017_2018_specific_secondary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", "rsv_secondary", 
                      paste0("rsv_ethnicity_ses_model_outputs_", 
                             cohort, "_2017_2018_specific_secondary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "secondary", subset = "2017_18")
)

#save as csv
write_csv(collated_model_outputs_rsv, paste0(here::here("output",
          "collated", "analytic", "secondary"), "/", cohort,
          "_rsv_model_outputs_collated_secondary.csv"))
