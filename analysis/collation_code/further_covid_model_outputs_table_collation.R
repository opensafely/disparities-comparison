library(tidyverse)
library(here)
library(arrow)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "adults"
} else {
  cohort <- args[[1]]
}

## create output directories ----
fs::dir_create(here::here("output", "collated", "analytic"))

##model outputs

#import covid results table by cohort 
#(primary investigation, specific and sensitive phenotypes)
if (cohort == "infants" | cohort == "infants_subgroup") {
  
  collated_model_outputs_covid_further = rbind(
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2019_2020_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2019_2020_specific_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2019_2020_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2021_2022_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2021_2022_specific_primary.csv")))
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2021_2022_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2022_2023_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2022_2023_specific_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2022_2023_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2023_2024_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2023_2024_specific_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2023_2024_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2021_2022_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2021_2022_sensitive_primary.csv")))
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2021_2022_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2022_2023_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2022_2023_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2022_2023_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2023_2024_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2023_2024_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2023_2024_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2023_24")
  )
  
} else {
  
  collated_model_outputs_covid_further = rbind(
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2019_2020_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2019_2020_specific_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2019_2020_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_hh_comp_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv")))
    %>% mutate(model_type = "composition", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_hh_comp_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_hh_comp_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_full_model_outputs_", 
                               cohort, "_2020_2021_specific_primary.csv"))) 
    %>% mutate(model_type = "full", codelist_type = "specific", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2021_2022_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2021_2022_specific_primary.csv")))
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2021_2022_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2022_2023_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2022_2023_specific_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2022_2023_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2023_2024_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2023_2024_specific_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2023_2024_specific_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2019_2020_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2019_2020_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2019_2020_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2019_20"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_hh_comp_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv")))
    %>% mutate(model_type = "composition", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_hh_comp_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive",
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_hh_comp_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_full_model_outputs_", 
                               cohort, "_2020_2021_sensitive_primary.csv"))) 
    %>% mutate(model_type = "full", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2020_21"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2021_2022_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2021_2022_sensitive_primary.csv")))
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2021_2022_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2021_22"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2022_2023_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2022_2023_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2022_2023_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2022_23"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_model_outputs_", 
                               cohort, "_2023_2024_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ses_model_outputs_", 
                               cohort, "_2023_2024_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2023_24"),
    read_csv(here::here("output", "results", "models", "covid_primary", 
                        paste0("further_covid_ethnicity_ses_model_outputs_", 
                               cohort, "_2023_2024_sensitive_primary.csv"))) 
    %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
               investigation_type = "primary", subset = "2023_24")
  )
  
}

#save as csv
write_csv(collated_model_outputs_covid_further,
          paste0(here::here("output", "collated", "analytic"),
          "/", cohort, "_further_covid_model_outputs_collated.csv"))
