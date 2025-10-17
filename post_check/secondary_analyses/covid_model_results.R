library(tidyverse)
library(here)
library(arrow)
library(broom)
library(broom.helpers)
library(lubridate)
library(cowplot)
library(stringr)

cohort <- "older_adults"
pathogen <- "covid"
investigation_type <- "secondary"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "analytic", "secondary", paste0(cohort, "_", pathogen,
                     "_model_outputs_collated_secondary.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_secondary.arrow"))) 

#import plot function
source(here::here("post_check", "functions", "forest.R"))

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild : no mild covid secondary

# #ethnicity
# covid_ethnicity_mild_spec <- forest(
#   df_input, df_dummy, pathogen, "ethnicity", "Mild"
# )$spec
# covid_ethnicity_mild_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "ethnicity", "Mild"
# )$spec

# #ses
# covid_ses_mild_spec <- forest(
#   df_input, df_dummy, pathogen, "ses", "Mild"
# )$spec
# covid_ses_mild_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "ses", "Mild"
# )$spec

# #hh_comp
# covid_hh_comp_mild_spec <- forest(
#   df_input, df_dummy, pathogen, "composition", "Mild"
# )$spec
# covid_hh_comp_mild_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "composition", "Mild"
# )$spec

#ethnicity & ses
covid_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)$spec
covid_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)$spec

# #ethnicity & hh_comp
# covid_ethnicity_hh_comp_mild_spec <- forest(
#   df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
# )$spec
# covid_ethnicity_hh_comp_mild_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
# )$spec

# #ses & hh_comp
# covid_ses_hh_comp_mild_spec <- forest(
#   df_input, df_dummy, pathogen, "ses_composition", "Mild"
# )$spec
# covid_ses_hh_comp_mild_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "ses_composition", "Mild"
# )$spec

# #full
# covid_full_mild_spec <- forest(
#   df_input, df_dummy, pathogen, "full", "Mild"
# )$spec
# covid_full_mild_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "full", "Mild"
# )$spec

##create relevant forest plots - severe

# #ethnicity
# covid_ethnicity_severe_spec <- forest(
#   df_input, df_dummy, pathogen, "ethnicity", "Severe"
# )$spec
# covid_ethnicity_severe_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "ethnicity", "Severe"
# )$spec

# #ses
# covid_ses_severe_spec <- forest(
#   df_input, df_dummy, pathogen, "ses", "Severe"
# )$spec
# covid_ses_severe_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "ses", "Severe"
# )$spec

# #hh_comp
# covid_hh_comp_severe_spec <- forest(
#   df_input, df_dummy, pathogen, "composition", "Severe"
# )$spec
# covid_hh_comp_severe_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "composition", "Severe"
# )$spec

#ethnicity & ses
covid_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)$spec
covid_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)$spec

# #ethnicity & hh_comp
# covid_ethnicity_hh_comp_severe_spec <- forest(
#   df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
# )$spec
# covid_ethnicity_hh_comp_severe_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
# )$spec

# #ses & hh_comp
# covid_ses_hh_comp_severe_spec <- forest(
#   df_input, df_dummy, pathogen, "ses_composition", "Severe"
# )$spec
# covid_ses_hh_comp_severe_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "ses_composition", "Severe"
# )$spec

# #full
# covid_full_severe_spec <- forest(
#   df_input, df_dummy, pathogen, "full", "Severe"
# )$spec
# covid_full_severe_spec_alt <- forest_year(
#   df_input, df_dummy, pathogen, "full", "Severe"
# )$spec

#create list of plots
plotlist <- list(
  # covid_ethnicity_mild_spec,
  # covid_ethnicity_mild_spec_alt,
  # covid_ses_mild_spec,
  # covid_ses_mild_spec_alt,
  # covid_hh_comp_mild_spec,
  # covid_hh_comp_mild_spec_alt,
  covid_ethnicity_ses_mild_spec,
  covid_ethnicity_ses_mild_spec_alt,
  # covid_ethnicity_hh_comp_mild_spec,
  # covid_ethnicity_hh_comp_mild_spec_alt,
  # covid_ses_hh_comp_mild_spec,
  # covid_ses_hh_comp_mild_spec_alt,
  # covid_full_mild_spec,
  # covid_full_mild_spec_alt,
  # covid_ethnicity_severe_spec,
  # covid_ethnicity_severe_spec_alt,
  # covid_ses_severe_spec,
  # covid_ses_severe_spec_alt,
  # covid_hh_comp_severe_spec,
  # covid_hh_comp_severe_spec_alt,
  covid_ethnicity_ses_severe_spec,
  covid_ethnicity_ses_severe_spec_alt#,
  # covid_ethnicity_hh_comp_severe_spec,
  # covid_ethnicity_hh_comp_severe_spec_alt,
  # covid_ses_hh_comp_severe_spec,
  # covid_ses_hh_comp_severe_spec_alt,
  # covid_full_severe_spec,
  # covid_full_severe_spec_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "secondary_analyses", "models",
              paste0(saveas, ".png")), p, height = 8, width = 15)
  
}
