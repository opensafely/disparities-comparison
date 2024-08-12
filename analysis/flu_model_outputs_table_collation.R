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
fs::dir_create(here("output", "results", "collated"))

##model outputs

#import flu results table by cohort (primary analysis)
collated_model_outputs_flu = rbind(
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_hh_comp_model_outputs_", 
                             cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_full_model_outputs_", 
                             cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_hh_comp_model_outputs_", 
                             cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_full_model_outputs_", 
                             cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_hh_comp_model_outputs_", 
                             cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_full_model_outputs_", 
                             cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_hh_comp_model_outputs_", 
                             cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_full_model_outputs_", 
                             cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_hh_comp_model_outputs_", 
                             cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_full_model_outputs_", 
                             cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_2021_2022_specific_primary.csv")))
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_hh_comp_model_outputs_", 
                             cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_full_model_outputs_", 
                             cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ethnicity_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_ses_hh_comp_model_outputs_", 
                             cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
                      paste0("flu_full_model_outputs_", 
                             cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results", "collated"),
                                         "/", cohort, "_flu_model_outputs_collated.csv"))
