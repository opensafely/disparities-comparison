library(tidyverse)
library(here)
library(arrow)
library(broom)
library(broom.helpers)
library(lubridate)
library(cowplot)
library(stringr)

cohort <- "older_adults"
pathogen <- "flu"
investigation_type <- "secondary"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "analytic", "secondary", paste0(cohort, "_", pathogen,
                     "_model_outputs_collated_secondary.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2018_2019_specific_secondary.arrow"))) 

#import plot function
source(here::here("post_check", "functions", "forest.R"))

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild : no mild flu secondary

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)$spec
flu_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)$spec

#ses
flu_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)$spec
flu_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe"
)$spec

#ethnicity & ses
flu_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)$spec
flu_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)$spec

#create list of plots
plotlist <- list(
  flu_ethnicity_severe_spec,
  flu_ethnicity_severe_spec_alt,
  flu_ses_severe_spec,
  flu_ses_severe_spec_alt,
  flu_ethnicity_ses_severe_spec,
  flu_ethnicity_ses_severe_spec_alt
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
