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

#define seasons of interest
seasons <- function(pathogen) {
  
  case_when(
    pathogen == "rsv" ~ "2017_18",
    pathogen == "flu" ~ "2018_19",
    pathogen == "covid" ~ "2020_21"
  )
  
}

#--- rsv

pathogen <- "rsv"
investigation_type <- "primary"
season <- seasons(pathogen)

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
rsv_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
rsv_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
rsv_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
rsv_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
rsv_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
rsv_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
rsv_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
rsv_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
rsv_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
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
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###adults

cohort <- "adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
rsv_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
rsv_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
rsv_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
rsv_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
rsv_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
rsv_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
rsv_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
rsv_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
rsv_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
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
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###children and adolescents

cohort <- "children_and_adolescents"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
rsv_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
rsv_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
rsv_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
rsv_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
rsv_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
rsv_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
rsv_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
rsv_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
rsv_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
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
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###infants

cohort <- "infants"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
rsv_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
rsv_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
rsv_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
rsv_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
rsv_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
rsv_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
rsv_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
rsv_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
rsv_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
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
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###infants subgroup

cohort <- "infants_subgroup"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
rsv_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
rsv_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
rsv_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
rsv_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
rsv_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
rsv_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
rsv_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
rsv_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
rsv_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
rsv_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
rsv_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
rsv_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
rsv_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
rsv_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
rsv_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
rsv_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
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
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

#--- flu

pathogen <- "flu"
investigation_type <- "primary"
season <- seasons(pathogen)

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
flu_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
flu_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
flu_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
flu_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
flu_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
flu_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
flu_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
flu_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
flu_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  flu_ethnicity_mild_spec, flu_ethnicity_mild_spec_alt,
  flu_ethnicity_mild_sens, flu_ethnicity_mild_sens_alt,
  flu_ethnicity_severe_spec, flu_ethnicity_severe_spec_alt,
  flu_ethnicity_severe_sens, flu_ethnicity_severe_sens_alt,
  flu_ses_mild_spec, flu_ses_mild_spec_alt,
  flu_ses_mild_sens, flu_ses_mild_sens_alt,
  flu_ses_severe_spec, flu_ses_severe_spec_alt,
  flu_ses_severe_sens, flu_ses_severe_sens_alt,
  flu_ethnicity_ses_mild_spec, flu_ethnicity_ses_mild_spec_alt,
  flu_ethnicity_ses_mild_sens, flu_ethnicity_ses_mild_sens_alt,
  flu_ethnicity_ses_severe_spec, flu_ethnicity_ses_severe_spec_alt,
  flu_ethnicity_ses_severe_sens, flu_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###adults

cohort <- "adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
flu_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
flu_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
flu_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
flu_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
flu_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
flu_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
flu_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
flu_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
flu_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  flu_ethnicity_mild_spec, flu_ethnicity_mild_spec_alt,
  flu_ethnicity_mild_sens, flu_ethnicity_mild_sens_alt,
  flu_ethnicity_severe_spec, flu_ethnicity_severe_spec_alt,
  flu_ethnicity_severe_sens, flu_ethnicity_severe_sens_alt,
  flu_ses_mild_spec, flu_ses_mild_spec_alt,
  flu_ses_mild_sens, flu_ses_mild_sens_alt,
  flu_ses_severe_spec, flu_ses_severe_spec_alt,
  flu_ses_severe_sens, flu_ses_severe_sens_alt,
  flu_ethnicity_ses_mild_spec, flu_ethnicity_ses_mild_spec_alt,
  flu_ethnicity_ses_mild_sens, flu_ethnicity_ses_mild_sens_alt,
  flu_ethnicity_ses_severe_spec, flu_ethnicity_ses_severe_spec_alt,
  flu_ethnicity_ses_severe_sens, flu_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###children and adolescents

cohort <- "children_and_adolescents"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
flu_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
flu_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
flu_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
flu_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
flu_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
flu_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
flu_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
flu_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
flu_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  flu_ethnicity_mild_spec, flu_ethnicity_mild_spec_alt,
  flu_ethnicity_mild_sens, flu_ethnicity_mild_sens_alt,
  flu_ethnicity_severe_spec, flu_ethnicity_severe_spec_alt,
  flu_ethnicity_severe_sens, flu_ethnicity_severe_sens_alt,
  flu_ses_mild_spec, flu_ses_mild_spec_alt,
  flu_ses_mild_sens, flu_ses_mild_sens_alt,
  flu_ses_severe_spec, flu_ses_severe_spec_alt,
  flu_ses_severe_sens, flu_ses_severe_sens_alt,
  flu_ethnicity_ses_mild_spec, flu_ethnicity_ses_mild_spec_alt,
  flu_ethnicity_ses_mild_sens, flu_ethnicity_ses_mild_sens_alt,
  flu_ethnicity_ses_severe_spec, flu_ethnicity_ses_severe_spec_alt,
  flu_ethnicity_ses_severe_sens, flu_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###infants

cohort <- "infants"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
flu_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
flu_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
flu_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
flu_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
flu_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
flu_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
flu_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
flu_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
flu_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  flu_ethnicity_mild_spec, flu_ethnicity_mild_spec_alt,
  flu_ethnicity_mild_sens, flu_ethnicity_mild_sens_alt,
  flu_ethnicity_severe_spec, flu_ethnicity_severe_spec_alt,
  flu_ethnicity_severe_sens, flu_ethnicity_severe_sens_alt,
  flu_ses_mild_spec, flu_ses_mild_spec_alt,
  flu_ses_mild_sens, flu_ses_mild_sens_alt,
  flu_ses_severe_spec, flu_ses_severe_spec_alt,
  flu_ses_severe_sens, flu_ses_severe_sens_alt,
  flu_ethnicity_ses_mild_spec, flu_ethnicity_ses_mild_spec_alt,
  flu_ethnicity_ses_mild_sens, flu_ethnicity_ses_mild_sens_alt,
  flu_ethnicity_ses_severe_spec, flu_ethnicity_ses_severe_spec_alt,
  flu_ethnicity_ses_severe_sens, flu_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###infants subgroup

cohort <- "infants_subgroup"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
flu_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
flu_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
flu_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
flu_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
flu_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
flu_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
flu_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
flu_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
flu_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
flu_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
flu_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
flu_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
flu_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
flu_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
flu_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
flu_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  flu_ethnicity_mild_spec, flu_ethnicity_mild_spec_alt,
  flu_ethnicity_mild_sens, flu_ethnicity_mild_sens_alt,
  flu_ethnicity_severe_spec, flu_ethnicity_severe_spec_alt,
  flu_ethnicity_severe_sens, flu_ethnicity_severe_sens_alt,
  flu_ses_mild_spec, flu_ses_mild_spec_alt,
  flu_ses_mild_sens, flu_ses_mild_sens_alt,
  flu_ses_severe_spec, flu_ses_severe_spec_alt,
  flu_ses_severe_sens, flu_ses_severe_sens_alt,
  flu_ethnicity_ses_mild_spec, flu_ethnicity_ses_mild_spec_alt,
  flu_ethnicity_ses_mild_sens, flu_ethnicity_ses_mild_sens_alt,
  flu_ethnicity_ses_severe_spec, flu_ethnicity_ses_severe_spec_alt,
  flu_ethnicity_ses_severe_sens, flu_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

#--- covid

pathogen <- "covid"
investigation_type <- "primary"
season <- seasons(pathogen)

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
covid_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
covid_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
covid_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
covid_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#composition
covid_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$spec
covid_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$sens
covid_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$spec
covid_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
covid_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

#ethnicity & composition - too few events
covid_ethnicity_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$spec
covid_ethnicity_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$sens
covid_ethnicity_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$spec
covid_ethnicity_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$sens

#ses & composition
covid_ses_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$spec
covid_ses_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$sens
covid_ses_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$spec
covid_ses_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$sens

#full
covid_full_mild_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$spec
covid_full_mild_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$sens
covid_full_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$spec
covid_full_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
covid_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
covid_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
covid_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#composition
covid_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$spec
covid_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$sens
covid_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$spec
covid_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
covid_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#ethnicity & composition
covid_ethnicity_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$spec
covid_ethnicity_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$sens
covid_ethnicity_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$spec
covid_ethnicity_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$sens

#ses & composition
covid_ses_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$spec
covid_ses_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$sens
covid_ses_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$spec
covid_ses_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$sens

#full
covid_full_severe_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$spec
covid_full_severe_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$sens
covid_full_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$spec
covid_full_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  covid_ethnicity_mild_spec, covid_ethnicity_mild_spec_alt,
  covid_ethnicity_mild_sens, covid_ethnicity_mild_sens_alt,
  covid_ethnicity_severe_spec, covid_ethnicity_severe_spec_alt,
  covid_ethnicity_severe_sens, covid_ethnicity_severe_sens_alt,
  covid_ses_mild_spec, covid_ses_mild_spec_alt,
  covid_ses_mild_sens, covid_ses_mild_sens_alt,
  covid_ses_severe_spec, covid_ses_severe_spec_alt,
  covid_ses_severe_sens, covid_ses_severe_sens_alt,
  covid_composition_mild_spec, covid_composition_mild_spec_alt,
  covid_composition_mild_sens, covid_composition_mild_sens_alt,
  covid_ethnicity_ses_mild_spec, covid_ethnicity_ses_mild_spec_alt,
  covid_ethnicity_ses_mild_sens, covid_ethnicity_ses_mild_sens_alt,
  covid_ethnicity_ses_severe_spec, covid_ethnicity_ses_severe_spec_alt,
  covid_ethnicity_ses_severe_sens, covid_ethnicity_ses_severe_sens_alt,
  covid_ethnicity_composition_mild_spec, covid_ethnicity_composition_mild_spec_alt,
  covid_ethnicity_composition_mild_sens, covid_ethnicity_composition_mild_sens_alt,
  covid_ethnicity_composition_severe_spec, covid_ethnicity_composition_severe_spec_alt,
  covid_ethnicity_composition_severe_sens, covid_ethnicity_composition_severe_sens_alt,
  covid_ses_composition_mild_spec, covid_ses_composition_mild_spec_alt,
  covid_ses_composition_mild_sens, covid_ses_composition_mild_sens_alt,
  covid_ses_composition_severe_spec, covid_ses_composition_severe_spec_alt,
  covid_ses_composition_severe_sens, covid_ses_composition_severe_sens_alt,
  covid_full_mild_spec, covid_full_mild_spec_alt,
  covid_full_mild_sens, covid_full_mild_sens_alt,
  covid_full_severe_spec, covid_full_severe_spec_alt,
  covid_full_severe_sens, covid_full_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###adults

cohort <- "adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
covid_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
covid_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
covid_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
covid_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#composition
covid_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$spec
covid_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$sens
covid_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$spec
covid_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
covid_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

#ethnicity & composition
covid_ethnicity_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$spec
covid_ethnicity_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$sens
covid_ethnicity_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$spec
covid_ethnicity_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$sens

#ses & composition
covid_ses_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$spec
covid_ses_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$sens
covid_ses_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$spec
covid_ses_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$sens

#full
covid_full_mild_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$spec
covid_full_mild_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$sens
covid_full_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$spec
covid_full_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
covid_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
covid_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
covid_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#composition
covid_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$spec
covid_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$sens
covid_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$spec
covid_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
covid_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#ethnicity & composition
covid_ethnicity_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$spec
covid_ethnicity_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$sens
covid_ethnicity_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$spec
covid_ethnicity_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$sens

#ses & composition
covid_ses_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$spec
covid_ses_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$sens
covid_ses_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$spec
covid_ses_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$sens

#full
covid_full_severe_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$spec
covid_full_severe_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$sens
covid_full_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$spec
covid_full_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  covid_ethnicity_mild_spec, covid_ethnicity_mild_spec_alt,
  covid_ethnicity_mild_sens, covid_ethnicity_mild_sens_alt,
  covid_ethnicity_severe_spec, covid_ethnicity_severe_spec_alt,
  covid_ethnicity_severe_sens, covid_ethnicity_severe_sens_alt,
  covid_ses_mild_spec, covid_ses_mild_spec_alt,
  covid_ses_mild_sens, covid_ses_mild_sens_alt,
  covid_ses_severe_spec, covid_ses_severe_spec_alt,
  covid_ses_severe_sens, covid_ses_severe_sens_alt,
  covid_composition_mild_spec, covid_composition_mild_spec_alt,
  covid_composition_mild_sens, covid_composition_mild_sens_alt,
  covid_ethnicity_ses_mild_spec, covid_ethnicity_ses_mild_spec_alt,
  covid_ethnicity_ses_mild_sens, covid_ethnicity_ses_mild_sens_alt,
  covid_ethnicity_ses_severe_spec, covid_ethnicity_ses_severe_spec_alt,
  covid_ethnicity_ses_severe_sens, covid_ethnicity_ses_severe_sens_alt,
  covid_ethnicity_composition_mild_spec, covid_ethnicity_composition_mild_spec_alt,
  covid_ethnicity_composition_mild_sens, covid_ethnicity_composition_mild_sens_alt,
  covid_ethnicity_composition_severe_spec, covid_ethnicity_composition_severe_spec_alt,
  covid_ethnicity_composition_severe_sens, covid_ethnicity_composition_severe_sens_alt,
  covid_ses_composition_mild_spec, covid_ses_composition_mild_spec_alt,
  covid_ses_composition_mild_sens, covid_ses_composition_mild_sens_alt,
  covid_ses_composition_severe_spec, covid_ses_composition_severe_spec_alt,
  covid_ses_composition_severe_sens, covid_ses_composition_severe_sens_alt,
  covid_full_mild_spec, covid_full_mild_spec_alt,
  covid_full_mild_sens, covid_full_mild_sens_alt,
  covid_full_severe_spec, covid_full_severe_spec_alt,
  covid_full_severe_sens, covid_full_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###children and adolescents

cohort <- "children_and_adolescents"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
covid_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
covid_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
covid_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
covid_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#composition
covid_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$spec
covid_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$sens
covid_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$spec
covid_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
covid_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

#ethnicity & composition
covid_ethnicity_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$spec
covid_ethnicity_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$sens
covid_ethnicity_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$spec
covid_ethnicity_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)$sens

#ses & composition
covid_ses_composition_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$spec
covid_ses_composition_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$sens
covid_ses_composition_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$spec
covid_ses_composition_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)$sens

#full
covid_full_mild_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$spec
covid_full_mild_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$sens
covid_full_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$spec
covid_full_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
covid_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
covid_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
covid_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#composition
covid_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$spec
covid_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$sens
covid_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$spec
covid_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
covid_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#ethnicity & composition
covid_ethnicity_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$spec
covid_ethnicity_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$sens
covid_ethnicity_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$spec
covid_ethnicity_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)$sens

#ses & composition
covid_ses_composition_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$spec
covid_ses_composition_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$sens
covid_ses_composition_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$spec
covid_ses_composition_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)$sens

#full
covid_full_severe_spec <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$spec
covid_full_severe_sens <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$sens
covid_full_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$spec
covid_full_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  covid_ethnicity_mild_spec, covid_ethnicity_mild_spec_alt,
  covid_ethnicity_mild_sens, covid_ethnicity_mild_sens_alt,
  covid_ethnicity_severe_spec, covid_ethnicity_severe_spec_alt,
  covid_ethnicity_severe_sens, covid_ethnicity_severe_sens_alt,
  covid_ses_mild_spec, covid_ses_mild_spec_alt,
  covid_ses_mild_sens, covid_ses_mild_sens_alt,
  covid_ses_severe_spec, covid_ses_severe_spec_alt,
  covid_ses_severe_sens, covid_ses_severe_sens_alt,
  covid_composition_mild_spec, covid_composition_mild_spec_alt,
  covid_composition_mild_sens, covid_composition_mild_sens_alt,
  covid_ethnicity_ses_mild_spec, covid_ethnicity_ses_mild_spec_alt,
  covid_ethnicity_ses_mild_sens, covid_ethnicity_ses_mild_sens_alt,
  covid_ethnicity_ses_severe_spec, covid_ethnicity_ses_severe_spec_alt,
  covid_ethnicity_ses_severe_sens, covid_ethnicity_ses_severe_sens_alt,
  covid_ethnicity_composition_mild_spec, covid_ethnicity_composition_mild_spec_alt,
  covid_ethnicity_composition_mild_sens, covid_ethnicity_composition_mild_sens_alt,
  covid_ethnicity_composition_severe_spec, covid_ethnicity_composition_severe_spec_alt,
  covid_ethnicity_composition_severe_sens, covid_ethnicity_composition_severe_sens_alt,
  covid_ses_composition_mild_spec, covid_ses_composition_mild_spec_alt,
  covid_ses_composition_mild_sens, covid_ses_composition_mild_sens_alt,
  covid_ses_composition_severe_spec, covid_ses_composition_severe_spec_alt,
  covid_ses_composition_severe_sens, covid_ses_composition_severe_sens_alt,
  covid_full_mild_spec, covid_full_mild_spec_alt,
  covid_full_mild_sens, covid_full_mild_sens_alt,
  covid_full_severe_spec, covid_full_severe_spec_alt,
  covid_full_severe_sens, covid_full_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
        "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###infants

cohort <- "infants"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
covid_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
covid_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
covid_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
covid_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
covid_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
covid_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
covid_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
covid_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
covid_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  covid_ethnicity_mild_spec, covid_ethnicity_mild_spec_alt,
  covid_ethnicity_mild_sens, covid_ethnicity_mild_sens_alt,
  covid_ethnicity_severe_spec, covid_ethnicity_severe_spec_alt,
  covid_ethnicity_severe_sens, covid_ethnicity_severe_sens_alt,
  covid_ses_mild_spec, covid_ses_mild_spec_alt,
  covid_ses_mild_sens, covid_ses_mild_sens_alt,
  covid_ses_severe_spec, covid_ses_severe_spec_alt,
  covid_ses_severe_sens, covid_ses_severe_sens_alt,
  covid_ethnicity_ses_mild_spec, covid_ethnicity_ses_mild_spec_alt,
  covid_ethnicity_ses_mild_sens, covid_ethnicity_ses_mild_sens_alt,
  covid_ethnicity_ses_severe_spec, covid_ethnicity_ses_severe_spec_alt,
  covid_ethnicity_ses_severe_sens, covid_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
         "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

###infants_subgroup

cohort <- "infants_subgroup"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_", pathogen,
                            "_model_outputs_collated.csv")))
df_input <- df_input %>%
  filter(subset == !!season)
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
covid_ethnicity_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens
covid_ethnicity_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$spec
covid_ethnicity_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)$sens

#ses
covid_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens
covid_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$spec
covid_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_mild_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens
covid_ethnicity_ses_mild_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$spec
covid_ethnicity_ses_mild_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)$sens

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens
covid_ethnicity_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$spec
covid_ethnicity_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)$sens

#ses
covid_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens
covid_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$spec
covid_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)$sens

#ethnicity & ses
covid_ethnicity_ses_severe_spec <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens
covid_ethnicity_ses_severe_spec_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$spec
covid_ethnicity_ses_severe_sens_alt <- forest_year(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)$sens

#create list of plots
plotlist <- list(
  covid_ethnicity_mild_spec, covid_ethnicity_mild_spec_alt,
  covid_ethnicity_mild_sens, covid_ethnicity_mild_sens_alt,
  covid_ethnicity_severe_spec, covid_ethnicity_severe_spec_alt,
  covid_ethnicity_severe_sens, covid_ethnicity_severe_sens_alt,
  covid_ses_mild_spec, covid_ses_mild_spec_alt,
  covid_ses_mild_sens, covid_ses_mild_sens_alt,
  covid_ses_severe_spec, covid_ses_severe_spec_alt,
  covid_ses_severe_sens, covid_ses_severe_sens_alt,
  covid_ethnicity_ses_mild_spec, covid_ethnicity_ses_mild_spec_alt,
  covid_ethnicity_ses_mild_sens, covid_ethnicity_ses_mild_sens_alt,
  covid_ethnicity_ses_severe_spec, covid_ethnicity_ses_severe_spec_alt,
  covid_ethnicity_ses_severe_sens, covid_ethnicity_ses_severe_sens_alt
)

#plot all
for(p in plotlist) {
  
  print(p)
  title_name <- p$labels$title
  subtitle_name <- p$labels$subtitle
  saveas <- paste0(gsub(" ", "_", title_name), "_",
                   gsub(" ", "_", subtitle_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
         "models", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}
