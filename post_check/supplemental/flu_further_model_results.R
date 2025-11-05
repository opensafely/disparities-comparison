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

#define parameters for plots
pathogen <- "flu"
investigation_type <- "primary"

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
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
flu_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
flu_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#composition
flu_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#ethnicity & composition - too few events
flu_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
flu_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
flu_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
flu_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#composition
flu_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#ethnicity & composition - too few events
flu_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
flu_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
flu_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  flu_ethnicity_mild,
  flu_ethnicity_severe, 
  flu_ses_mild,
  flu_ses_severe, 
  flu_composition_mild,
  flu_composition_severe,
  flu_ethnicity_ses_mild,
  flu_ethnicity_ses_severe, 
  flu_ethnicity_composition_mild, 
  flu_ethnicity_composition_severe, 
  flu_ses_composition_mild,
  flu_ses_composition_severe,
  flu_full_mild,
  flu_full_severe
)
plot_names <- c(
  "flu_ethnicity_mild",
  "flu_ethnicity_severe",
  "flu_ses_mild",
  "flu_ses_severe",
  "flu_composition_mild",
  "flu_composition_severe",
  "flu_ethnicity_ses_mild",
  "flu_ethnicity_ses_severe",
  "flu_ethnicity_composition_mild",
  "flu_ethnicity_composition_severe",
  "flu_ses_composition_mild",
  "flu_ses_composition_severe",
  "flu_full_mild",
  "flu_full_severe"
)

for(i in seq_along(plotlist)) {
  p <- plotlist[[i]]
  name <- plot_names[i]
  
  print(p)
  
  ggsave(
    here("post_check", "plots", "supplemental", "models", cohort,
         paste0(cohort, "_", name, "_further.png")),
    p, height = 8, width = 15
  )
}

###adults

cohort <- "adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
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
flu_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
flu_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#composition
flu_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#ethnicity & composition - too few events
flu_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
flu_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
flu_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
flu_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#composition
flu_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#ethnicity & composition - too few events
flu_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
flu_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
flu_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  flu_ethnicity_mild,
  flu_ethnicity_severe, 
  flu_ses_mild,
  flu_ses_severe, 
  flu_composition_mild,
  flu_composition_severe,
  flu_ethnicity_ses_mild,
  flu_ethnicity_ses_severe, 
  flu_ethnicity_composition_mild, 
  flu_ethnicity_composition_severe, 
  flu_ses_composition_mild,
  flu_ses_composition_severe,
  flu_full_mild,
  flu_full_severe
)
plot_names <- c(
  "flu_ethnicity_mild",
  "flu_ethnicity_severe",
  "flu_ses_mild",
  "flu_ses_severe",
  "flu_composition_mild",
  "flu_composition_severe",
  "flu_ethnicity_ses_mild",
  "flu_ethnicity_ses_severe",
  "flu_ethnicity_composition_mild",
  "flu_ethnicity_composition_severe",
  "flu_ses_composition_mild",
  "flu_ses_composition_severe",
  "flu_full_mild",
  "flu_full_severe"
)

for(i in seq_along(plotlist)) {
  p <- plotlist[[i]]
  name <- plot_names[i]
  
  print(p)
  
  ggsave(
    here("post_check", "plots", "supplemental", "models", cohort,
         paste0(cohort, "_", name, "_further.png")),
    p, height = 8, width = 15
  )
}

###children and adolescents

cohort <- "children_and_adolescents"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
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
flu_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
flu_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#composition
flu_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#ethnicity & composition - too few events
flu_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
flu_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
flu_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
flu_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#composition
flu_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#ethnicity & composition - too few events
flu_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
flu_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
flu_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  flu_ethnicity_mild,
  flu_ethnicity_severe, 
  flu_ses_mild,
  flu_ses_severe, 
  flu_composition_mild,
  flu_composition_severe,
  flu_ethnicity_ses_mild,
  flu_ethnicity_ses_severe, 
  flu_ethnicity_composition_mild, 
  flu_ethnicity_composition_severe, 
  flu_ses_composition_mild,
  flu_ses_composition_severe,
  flu_full_mild,
  flu_full_severe
)
plot_names <- c(
  "flu_ethnicity_mild",
  "flu_ethnicity_severe",
  "flu_ses_mild",
  "flu_ses_severe",
  "flu_composition_mild",
  "flu_composition_severe",
  "flu_ethnicity_ses_mild",
  "flu_ethnicity_ses_severe",
  "flu_ethnicity_composition_mild",
  "flu_ethnicity_composition_severe",
  "flu_ses_composition_mild",
  "flu_ses_composition_severe",
  "flu_full_mild",
  "flu_full_severe"
)

for(i in seq_along(plotlist)) {
  p <- plotlist[[i]]
  name <- plot_names[i]
  
  print(p)
  
  ggsave(
    here("post_check", "plots", "supplemental", "models", cohort,
         paste0(cohort, "_", name, "_further.png")),
    p, height = 8, width = 15
  )
}

###infants

cohort <- "infants"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
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
flu_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
flu_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
flu_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  flu_ethnicity_mild,
  flu_ethnicity_severe,
  flu_ses_mild,
  flu_ses_severe,
  flu_ethnicity_ses_mild,
  flu_ethnicity_ses_severe
)
plot_names <- c(
  "flu_ethnicity_mild",
  "flu_ethnicity_severe",
  "flu_ses_mild",
  "flu_ses_severe",
  "flu_ethnicity_ses_mild",
  "flu_ethnicity_ses_severe"
)

for(i in seq_along(plotlist)) {
  p <- plotlist[[i]]
  name <- plot_names[i]
  
  print(p)
  
  ggsave(
    here("post_check", "plots", "supplemental", "models", cohort,
         paste0(cohort, "_", name, "_further.png")),
    p, height = 8, width = 15
  )
}

###infants_subgroup

cohort <- "infants_subgroup"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
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
flu_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
flu_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
flu_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
flu_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  flu_ethnicity_mild,
  flu_ethnicity_severe,
  flu_ses_mild,
  flu_ses_severe,
  flu_ethnicity_ses_mild,
  flu_ethnicity_ses_severe
)
plot_names <- c(
  "flu_ethnicity_mild",
  "flu_ethnicity_severe",
  "flu_ses_mild",
  "flu_ses_severe",
  "flu_ethnicity_ses_mild",
  "flu_ethnicity_ses_severe"
)

for(i in seq_along(plotlist)) {
  p <- plotlist[[i]]
  name <- plot_names[i]
  
  print(p)
  
  ggsave(
    here("post_check", "plots", "supplemental", "models", cohort,
         paste0(cohort, "_", name, "_further.png")),
    p, height = 10, width = 15
  )
}
