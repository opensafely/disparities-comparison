library(tidyverse)
library(here)
library(arrow)
library(broom)
library(broom.helpers)
library(lubridate)
library(cowplot)
library(stringr)

#import plot function
source(here::here("post_check", "functions", "forest.R"))

pathogen <- "rsv"
investigation_type <- "primary"

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

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

#composition
rsv_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$spec
rsv_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$sens
rsv_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$spec
rsv_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild"
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

#ethnicity & composition - too few events
rsv_ethnicity_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$spec
rsv_ethnicity_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$sens
rsv_ethnicity_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$spec
rsv_ethnicity_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$sens

#ses & composition
rsv_ses_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$spec
rsv_ses_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$sens
rsv_ses_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$spec
rsv_ses_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$sens

#full
rsv_full_mild_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)$spec
rsv_full_mild_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)$sens
rsv_full_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild"
)$spec
rsv_full_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild"
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

#composition
rsv_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$spec
rsv_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$sens
rsv_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$spec
rsv_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe"
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

#ethnicity & composition - too few events
rsv_ethnicity_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$spec
rsv_ethnicity_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$sens
rsv_ethnicity_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$spec
rsv_ethnicity_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$sens

#ses & composition
rsv_ses_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$spec
rsv_ses_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$sens
rsv_ses_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$spec
rsv_ses_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$sens

#full
rsv_full_severe_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
)$spec
rsv_full_severe_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
)$sens
rsv_full_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe"
)$spec
rsv_full_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe"
)$sens

#create list of plots
plotlist <- list(
  rsv_ethnicity_mild_spec, rsv_ethnicity_mild_spec_alt,
  rsv_ethnicity_mild_sens, rsv_ethnicity_mild_sens_alt,
  rsv_ethnicity_severe_spec, rsv_ethnicity_severe_spec_alt,
  rsv_ethnicity_severe_sens, rsv_ethnicity_severe_sens_alt,
  rsv_ses_mild_spec, rsv_ses_mild_spec_alt,
  rsv_ses_mild_sens, rsv_ses_mild_sens_alt,
  rsv_ses_severe_spec, rsv_ses_severe_spec_alt,
  rsv_ses_severe_sens, rsv_ses_severe_sens_alt,
  rsv_composition_mild_spec, rsv_composition_mild_spec_alt,
  rsv_composition_mild_sens, rsv_composition_mild_sens_alt,
  rsv_ethnicity_ses_mild_spec, rsv_ethnicity_ses_mild_spec_alt,
  rsv_ethnicity_ses_mild_sens, rsv_ethnicity_ses_mild_sens_alt,
  rsv_ethnicity_ses_severe_spec, rsv_ethnicity_ses_severe_spec_alt,
  rsv_ethnicity_ses_severe_sens, rsv_ethnicity_ses_severe_sens_alt,
  rsv_ethnicity_composition_mild_spec, rsv_ethnicity_composition_mild_spec_alt,
  rsv_ethnicity_composition_mild_sens, rsv_ethnicity_composition_mild_sens_alt,
  rsv_ethnicity_composition_severe_spec, rsv_ethnicity_composition_severe_spec_alt,
  rsv_ethnicity_composition_severe_sens, rsv_ethnicity_composition_severe_sens_alt,
  rsv_ses_composition_mild_spec, rsv_ses_composition_mild_spec_alt,
  rsv_ses_composition_mild_sens, rsv_ses_composition_mild_sens_alt,
  rsv_ses_composition_severe_spec, rsv_ses_composition_severe_spec_alt,
  rsv_ses_composition_severe_sens, rsv_ses_composition_severe_sens_alt,
  rsv_full_mild_spec, rsv_full_mild_spec_alt,
  rsv_full_mild_sens, rsv_full_mild_sens_alt,
  rsv_full_severe_spec, rsv_full_severe_spec_alt,
  rsv_full_severe_sens, rsv_full_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "models", cohort,
              paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###adults

cohort <- "adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow")))
df_dummy <- df_dummy %>%
  mutate(
    age_band = if_else(age_band == "18-29y", "18-39y", age_band)
  )

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

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

#composition
rsv_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$spec
rsv_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$sens
rsv_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$spec
rsv_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild"
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

#ethnicity & composition
rsv_ethnicity_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$spec
rsv_ethnicity_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$sens
rsv_ethnicity_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$spec
rsv_ethnicity_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$sens

#ses & composition
rsv_ses_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$spec
rsv_ses_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$sens
rsv_ses_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$spec
rsv_ses_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$sens

#full
rsv_full_mild_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)$spec
rsv_full_mild_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)$sens
rsv_full_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild"
)$spec
rsv_full_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild"
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

#composition
rsv_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$spec
rsv_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$sens
rsv_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$spec
rsv_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe"
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

#ethnicity & composition
rsv_ethnicity_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$spec
rsv_ethnicity_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$sens
rsv_ethnicity_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$spec
rsv_ethnicity_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$sens

#ses & composition
rsv_ses_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$spec
rsv_ses_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$sens
rsv_ses_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$spec
rsv_ses_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$sens

#full
rsv_full_severe_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
)$spec
rsv_full_severe_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
)$sens
rsv_full_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe"
)$spec
rsv_full_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe"
)$sens

#create list of plots
plotlist <- list(
  rsv_ethnicity_mild_spec, rsv_ethnicity_mild_spec_alt,
  rsv_ethnicity_mild_sens, rsv_ethnicity_mild_sens_alt,
  rsv_ethnicity_severe_spec, rsv_ethnicity_severe_spec_alt,
  rsv_ethnicity_severe_sens, rsv_ethnicity_severe_sens_alt,
  rsv_ses_mild_spec, rsv_ses_mild_spec_alt,
  rsv_ses_mild_sens, rsv_ses_mild_sens_alt,
  rsv_ses_severe_spec, rsv_ses_severe_spec_alt,
  rsv_ses_severe_sens, rsv_ses_severe_sens_alt,
  rsv_composition_mild_spec, rsv_composition_mild_spec_alt,
  rsv_composition_mild_sens, rsv_composition_mild_sens_alt,
  rsv_ethnicity_ses_mild_spec, rsv_ethnicity_ses_mild_spec_alt,
  rsv_ethnicity_ses_mild_sens, rsv_ethnicity_ses_mild_sens_alt,
  rsv_ethnicity_ses_severe_spec, rsv_ethnicity_ses_severe_spec_alt,
  rsv_ethnicity_ses_severe_sens, rsv_ethnicity_ses_severe_sens_alt,
  rsv_ethnicity_composition_mild_spec, rsv_ethnicity_composition_mild_spec_alt,
  rsv_ethnicity_composition_mild_sens, rsv_ethnicity_composition_mild_sens_alt,
  rsv_ethnicity_composition_severe_spec, rsv_ethnicity_composition_severe_spec_alt,
  rsv_ethnicity_composition_severe_sens, rsv_ethnicity_composition_severe_sens_alt,
  rsv_ses_composition_mild_spec, rsv_ses_composition_mild_spec_alt,
  rsv_ses_composition_mild_sens, rsv_ses_composition_mild_sens_alt,
  rsv_ses_composition_severe_spec, rsv_ses_composition_severe_spec_alt,
  rsv_ses_composition_severe_sens, rsv_ses_composition_severe_sens_alt,
  rsv_full_mild_spec, rsv_full_mild_spec_alt,
  rsv_full_mild_sens, rsv_full_mild_sens_alt,
  rsv_full_severe_spec, rsv_full_severe_spec_alt,
  rsv_full_severe_sens, rsv_full_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "models", cohort,
              paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###children and adolescents

cohort <- "children_and_adolescents"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

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

#composition
rsv_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$spec
rsv_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$sens
rsv_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild"
)$spec
rsv_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild"
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

#ethnicity & composition
rsv_ethnicity_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$spec
rsv_ethnicity_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$sens
rsv_ethnicity_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$spec
rsv_ethnicity_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)$sens

#ses & composition
rsv_ses_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$spec
rsv_ses_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$sens
rsv_ses_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$spec
rsv_ses_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)$sens

#full
rsv_full_mild_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)$spec
rsv_full_mild_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)$sens
rsv_full_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild"
)$spec
rsv_full_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild"
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

#composition
rsv_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$spec
rsv_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$sens
rsv_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe"
)$spec
rsv_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe"
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

#ethnicity & composition
rsv_ethnicity_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$spec
rsv_ethnicity_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$sens
rsv_ethnicity_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$spec
rsv_ethnicity_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)$sens

#ses & composition
rsv_ses_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$spec
rsv_ses_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$sens
rsv_ses_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$spec
rsv_ses_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)$sens

#full
rsv_full_severe_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
)$spec
rsv_full_severe_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
)$sens
rsv_full_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe"
)$spec
rsv_full_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe"
)$sens

#create list of plots
plotlist <- list(
  rsv_ethnicity_mild_spec, rsv_ethnicity_mild_spec_alt,
  rsv_ethnicity_mild_sens, rsv_ethnicity_mild_sens_alt,
  rsv_ethnicity_severe_spec, rsv_ethnicity_severe_spec_alt,
  rsv_ethnicity_severe_sens, rsv_ethnicity_severe_sens_alt,
  rsv_ses_mild_spec, rsv_ses_mild_spec_alt,
  rsv_ses_mild_sens, rsv_ses_mild_sens_alt,
  rsv_ses_severe_spec, rsv_ses_severe_spec_alt,
  rsv_ses_severe_sens, rsv_ses_severe_sens_alt,
  rsv_composition_mild_spec, rsv_composition_mild_spec_alt,
  rsv_composition_mild_sens, rsv_composition_mild_sens_alt,
  rsv_ethnicity_ses_mild_spec, rsv_ethnicity_ses_mild_spec_alt,
  rsv_ethnicity_ses_mild_sens, rsv_ethnicity_ses_mild_sens_alt,
  rsv_ethnicity_ses_severe_spec, rsv_ethnicity_ses_severe_spec_alt,
  rsv_ethnicity_ses_severe_sens, rsv_ethnicity_ses_severe_sens_alt,
  rsv_ethnicity_composition_mild_spec, rsv_ethnicity_composition_mild_spec_alt,
  rsv_ethnicity_composition_mild_sens, rsv_ethnicity_composition_mild_sens_alt,
  rsv_ethnicity_composition_severe_spec, rsv_ethnicity_composition_severe_spec_alt,
  rsv_ethnicity_composition_severe_sens, rsv_ethnicity_composition_severe_sens_alt,
  rsv_ses_composition_mild_spec, rsv_ses_composition_mild_spec_alt,
  rsv_ses_composition_mild_sens, rsv_ses_composition_mild_sens_alt,
  rsv_ses_composition_severe_spec, rsv_ses_composition_severe_spec_alt,
  rsv_ses_composition_severe_sens, rsv_ses_composition_severe_sens_alt,
  rsv_full_mild_spec, rsv_full_mild_spec_alt,
  rsv_full_mild_sens, rsv_full_mild_sens_alt,
  rsv_full_severe_spec, rsv_full_severe_spec_alt,
  rsv_full_severe_sens, rsv_full_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "models", cohort,
              paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###infants

cohort <- "infants"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

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
  rsv_ethnicity_mild_spec, rsv_ethnicity_mild_spec_alt,
  rsv_ethnicity_mild_sens, rsv_ethnicity_mild_sens_alt,
  rsv_ethnicity_severe_spec, rsv_ethnicity_severe_spec_alt,
  rsv_ethnicity_severe_sens, rsv_ethnicity_severe_sens_alt,
  rsv_ses_mild_spec, rsv_ses_mild_spec_alt,
  rsv_ses_mild_sens, rsv_ses_mild_sens_alt,
  rsv_ses_severe_spec, rsv_ses_severe_spec_alt,
  rsv_ses_severe_sens, rsv_ses_severe_sens_alt,
  rsv_ethnicity_ses_mild_spec, rsv_ethnicity_ses_mild_spec_alt,
  rsv_ethnicity_ses_mild_sens, rsv_ethnicity_ses_mild_sens_alt,
  rsv_ethnicity_ses_severe_spec, rsv_ethnicity_ses_severe_spec_alt,
  rsv_ethnicity_ses_severe_sens, rsv_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "models", cohort,
              paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###infants_subgroup

cohort <- "infants_subgroup"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

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
  rsv_ethnicity_mild_spec, rsv_ethnicity_mild_spec_alt,
  rsv_ethnicity_mild_sens, rsv_ethnicity_mild_sens_alt,
  rsv_ethnicity_severe_spec, rsv_ethnicity_severe_spec_alt,
  rsv_ethnicity_severe_sens, rsv_ethnicity_severe_sens_alt,
  rsv_ses_mild_spec, rsv_ses_mild_spec_alt,
  rsv_ses_mild_sens, rsv_ses_mild_sens_alt,
  rsv_ses_severe_spec, rsv_ses_severe_spec_alt,
  rsv_ses_severe_sens, rsv_ses_severe_sens_alt,
  rsv_ethnicity_ses_mild_spec, rsv_ethnicity_ses_mild_spec_alt,
  rsv_ethnicity_ses_mild_sens, rsv_ethnicity_ses_mild_sens_alt,
  rsv_ethnicity_ses_severe_spec, rsv_ethnicity_ses_severe_spec_alt,
  rsv_ethnicity_ses_severe_sens, rsv_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "models", cohort,
              paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}
