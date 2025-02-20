library(tidyverse)
library(here)
library(arrow)
library(broom)
library(broom.helpers)
library(lubridate)
library(cowplot)
library(stringr)

## create output directories ----
fs::dir_create(here("post_check", "primary_analyses"))

cohort <- "infants"
pathogen <- "rsv"
investigation_type <- "primary"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen, "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2016_2017_specific_primary.arrow"))) 

#import plot function
source(here::here("post_check", "functions", "forest.R"))

##create relevant forest plots - mild

#ethnicity
rsv_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)$spec
rsv_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)$sens
rsv_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)$spec
rsv_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)$sens

#ses
rsv_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)$spec
rsv_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)$sens
rsv_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild"
)$spec
rsv_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)$spec
rsv_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)$sens
rsv_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)$spec
rsv_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)$sens


##create relevant forest plots - severe

#ethnicity
rsv_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)$spec
rsv_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)$sens
rsv_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)$spec
rsv_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)$sens

#ses
rsv_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)$spec
rsv_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)$sens
rsv_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe"
)$spec
rsv_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)$spec
rsv_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)$sens
rsv_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)$spec
rsv_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)$sens

#create list of plots
plotlist <- list(
  rsv_ethnicity_mild_spec,
  rsv_ethnicity_mild_spec_alt,
  rsv_ethnicity_mild_sens,
  rsv_ethnicity_mild_sens_alt,
  rsv_ethnicity_severe_spec,
  rsv_ethnicity_severe_spec_alt,
  rsv_ethnicity_severe_sens,
  rsv_ethnicity_severe_sens_alt,
  rsv_ses_mild_spec,
  rsv_ses_mild_spec_alt,
  rsv_ses_mild_sens,
  rsv_ses_mild_sens_alt,
  rsv_ses_severe_spec,
  rsv_ses_severe_spec_alt,
  rsv_ses_severe_sens,
  rsv_ses_severe_sens_alt,
  rsv_ethnicity_ses_mild_spec,
  rsv_ethnicity_ses_mild_spec_alt,
  rsv_ethnicity_ses_mild_sens,
  rsv_ethnicity_ses_mild_sens_alt,
  rsv_ethnicity_ses_severe_spec,
  rsv_ethnicity_ses_severe_spec_alt,
  rsv_ethnicity_ses_severe_sens,
  rsv_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  print(p)
}
