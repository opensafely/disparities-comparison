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

##flowchart

# import flow chart info by cohort
collated_flow_chart = rbind(
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2016_2017.csv"))) %>% mutate(subset = "2016_17"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2017_2018.csv"))) %>% mutate(subset = "2017_18"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2018_2019.csv"))) %>% mutate(subset = "2018_19"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2019_2020.csv"))) %>% mutate(subset = "2019_20"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2020_2021.csv"))) %>% mutate(subset = "2020_21"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2021_2022.csv"))) %>% mutate(subset = "2021_22"),
  read_csv(here::here("output", "flow_chart", paste0("flow_chart_processed_", 
                      cohort, "_2022_2023.csv"))) %>% mutate(subset = "2022_23")
)

## create output directories ----
fs::dir_create(here("output", "flow_chart", "collated"))

#save as csv
write_csv(collated_flow_chart, paste0(here::here("output", "flow_chart", "collated"),
          "/", cohort, "_flow_chart_collated.csv"))

##table 1

# import table 1 by cohort
collated_table1 = rbind(
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2016_2017.csv"))) %>% mutate(subset = "2016_17"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2017_2018.csv"))) %>% mutate(subset = "2017_18"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort,
                      "_2018_2019.csv"))) %>% mutate(subset = "2018_19"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2019_2020.csv"))) %>% mutate(subset = "2019_20"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2020_2021.csv"))) %>% mutate(subset = "2020_21"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2021_2022.csv"))) %>% mutate(subset = "2021_22"),
  read_csv(here::here("output", "table1", paste0("table1_", cohort, 
                      "_2022_2023.csv"))) %>% mutate(subset = "2022_23")
)

## create output directories ----
fs::dir_create(here("output", "results", "collated"))

#save as csv
write_csv(collated_table1, paste0(here::here("output", "collated"), "/", 
          cohort, "_table1_collated.csv"))

##rates

# import rates table by cohort 
collated_rates = rbind(
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2016_2017_specific_primary.csv"))) 
  %>% mutate(codelist_type = "specific", investigation_type = "primary", 
             subset = "2016_17"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2016_2017_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2016_17"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2016_2017_sensitive_secondary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "secondary",
             subset = "2016_17"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2017_2018_specific_primary.csv")))
  %>% mutate(codelist_type = "specific", "investigation_type" = "primary", 
             subset = "2017_18"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2017_2018_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2017_18"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2017_2018_sensitive_secondary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "secondary",
             subset = "2017_18"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2018_2019_specific_primary.csv"))) 
  %>% mutate(codelist_type = "specific", "investigation_type" = "primary",
             subset = "2018_19"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2018_2019_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2018_19"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2018_2019_sensitive_secondary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "secondary",
             subset = "2018_19"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2019_2020_specific_primary.csv")))
  %>% mutate(codelist_type = "specific", "investigation_type" = "primary",
             subset = "2019_20"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2019_2020_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2019_20"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2019_2020_sensitive_secondary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "secondary",
             subset = "2019_20"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2020_2021_specific_primary.csv"))) 
  %>% mutate(codelist_type = "specific", "investigation_type" = "primary",
             subset = "2020_21"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2020_2021_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2020_21"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2020_2021_sensitive_secondary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "secondary",
             subset = "2020_21"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2021_2022_specific_primary.csv"))) 
  %>% mutate(codelist_type = "specific", "investigation_type" = "primary",
             subset = "2021_22"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2021_2022_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2021_22"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2021_2022_sensitive_secondary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "secondary",
             subset = "2021_22"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2022_2023_specific_primary.csv"))) 
  %>% mutate(codelist_type = "specific", "investigation_type" = "primary",
             subset = "2022_23"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2022_2023_sensitive_primary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "primary",
             subset = "2022_23"),
  read_csv(here::here("output", "results", "rates", paste0("rates_", cohort, 
                      "_2022_2023_sensitive_secondary.csv")))
  %>% mutate(codelist_type = "sensitive", investigation_type = "secondary",
             subset = "2022_23")
)

#save as csv
write_csv(collated_rates, paste0(here::here("output", "collated"), "/", 
          cohort, "_rates_collated.csv"))

##model outputs

#import rsv results table by cohort (primary analysis)
collated_model_outputs_rsv = rbind(
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv")))
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results", "collated"),
          "/", cohort, "_rsv_model_outputs_collated.csv"))

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

#import covid results table by cohort (primary analysis)
collated_model_outputs_covid = rbind(
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_ses_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_full_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_ses_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_full_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv")))
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_ses_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_full_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_ses_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_full_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results", "collated"),
                                         "/", cohort, "_covid_model_outputs_collated.csv"))

#import overall and all cause results table by cohort (primary analysis)
collated_model_outputs_overall_and_all_cause = rbind(
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2016_2017_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2017_2018_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2018_2019_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2019_2020_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2020_2021_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv")))
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2021_2022_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2022_2023_specific_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "specific", 
             investigation_type = "primary", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results", "collated"),
          "/", cohort, "_overall_and_all_cause_model_outputs_collated.csv"))

#import rsv results table by cohort (sensitivity analysis)
collated_model_outputs_rsv_sentivity = rbind(
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv")))
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_ses_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_ses_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("rsv_full_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results", "collated"),
          "/", cohort, "_rsv_model_outputs_sensitivity_collated.csv"))

#import flu results table by cohort (sensitivity analysis)
collated_model_outputs_flu_sensitivity = rbind(
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_ses_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_full_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_ses_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_full_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_ses_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_full_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_ses_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_full_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_ses_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_full_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv")))
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_ses_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_full_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_ses_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_ses_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("flu_full_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results", "collated"),
          "/", cohort, "_flu_model_outputs_sensitivity_collated.csv"))

#import covid results table by cohort (sensitivity analysis)
collated_model_outputs_covid = rbind(
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_ses_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_full_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_ses_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_full_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv")))
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_ses_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_full_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_ses_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_ses_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("covid_full_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results", "collated"),
          "/", cohort, "_covid_model_outputs_sensitivity_collated.csv"))

#import overall and all cause results table by cohort (sensitivity analysis)
collated_model_outputs_overall_and_all_cause = rbind(
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2016_2017_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2016_17"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2017_2018_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2017_18"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2018_2019_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2018_19"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2019_2020_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2020_21"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2020_2021_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2019_20"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv")))
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2021_2022_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2021_22"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv")))
  %>% mutate(model_type = "composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_ses_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ethnicity_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_ses_hh_comp_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "ses_composition", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23"),
  read_csv(here::here("output", "results", "models", 
           paste0("overall_and_all_cause_full_model_outputs_", 
           cohort, "_2022_2023_sensitive_primary.csv"))) 
  %>% mutate(model_type = "full", codelist_type = "sensitive", 
             investigation_type = "primary", subset = "2022_23")
)

#save as csv
write_csv(collated_model_outputs, paste0(here::here("output", "results", "collated"),
          "/", cohort, "_overall_and_all_cause_model_outputs_sensitivity_collated.csv"))
