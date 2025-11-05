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
pathogen <- "covid"
investigation_type <- "primary"

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2021_2022_specific_primary.arrow"))) %>%
  mutate(
    subset = "2021_22",
    time_since_last_covid_vaccination = if_else(
      is.na(covid_vaccination_immunity_date), "6-12m",
      time_since_last_covid_vaccination)
    )

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#import dummy data with composition
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) %>% 
  mutate(subset = "2020_21")

#composition
covid_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
covid_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
covid_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#import dummy data with vaccination
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2021_2022_specific_primary.arrow"))) %>%
  mutate(
    subset = "2021_22",
    time_since_last_covid_vaccination = if_else(
      is.na(covid_vaccination_immunity_date), "6-12m",
      time_since_last_covid_vaccination)
    )

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#import dummy data with composition
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) %>% 
  mutate(subset = "2020_21")

#composition
covid_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
covid_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
covid_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  covid_ethnicity_mild,
  covid_ethnicity_severe, 
  covid_ses_mild,
  covid_ses_severe, 
  covid_composition_mild,
  covid_composition_severe,
  covid_ethnicity_ses_mild,
  covid_ethnicity_ses_severe, 
  covid_ethnicity_composition_mild, 
  covid_ethnicity_composition_severe, 
  covid_ses_composition_mild,
  covid_ses_composition_severe,
  covid_full_mild,
  covid_full_severe
)
plot_names <- c(
  "covid_ethnicity_mild",
  "covid_ethnicity_severe",
  "covid_ses_mild",
  "covid_ses_severe",
  "covid_composition_mild",
  "covid_composition_severe",
  "covid_ethnicity_ses_mild",
  "covid_ethnicity_ses_severe",
  "covid_ethnicity_composition_mild",
  "covid_ethnicity_composition_severe",
  "covid_ses_composition_mild",
  "covid_ses_composition_severe",
  "covid_full_mild",
  "covid_full_severe"
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
             "_2021_2022_specific_primary.arrow"))) %>%
  mutate(
    subset = "2021_22",
    time_since_last_covid_vaccination = if_else(
      is.na(covid_vaccination_immunity_date), "6-12m",
      time_since_last_covid_vaccination)
  )

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#import dummy data with composition
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) %>% 
  mutate(subset = "2020_21")

#composition
covid_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
covid_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
covid_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#import dummy data with vaccination
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2021_2022_specific_primary.arrow"))) %>%
  mutate(
    subset = "2021_22",
    time_since_last_covid_vaccination = if_else(
      is.na(covid_vaccination_immunity_date), "6-12m",
      time_since_last_covid_vaccination)
    )

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#import dummy data with composition
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) %>% 
  mutate(subset = "2020_21")

#composition
covid_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
covid_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
covid_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  covid_ethnicity_mild,
  covid_ethnicity_severe, 
  covid_ses_mild,
  covid_ses_severe, 
  covid_composition_mild,
  covid_composition_severe,
  covid_ethnicity_ses_mild,
  covid_ethnicity_ses_severe, 
  covid_ethnicity_composition_mild, 
  covid_ethnicity_composition_severe, 
  covid_ses_composition_mild,
  covid_ses_composition_severe,
  covid_full_mild,
  covid_full_severe
)
plot_names <- c(
  "covid_ethnicity_mild",
  "covid_ethnicity_severe",
  "covid_ses_mild",
  "covid_ses_severe",
  "covid_composition_mild",
  "covid_composition_severe",
  "covid_ethnicity_ses_mild",
  "covid_ethnicity_ses_severe",
  "covid_ethnicity_composition_mild",
  "covid_ethnicity_composition_severe",
  "covid_ses_composition_mild",
  "covid_ses_composition_severe",
  "covid_full_mild",
  "covid_full_severe"
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
             "_2021_2022_specific_primary.arrow"))) %>%
  mutate(
    subset = "2021_22",
    time_since_last_covid_vaccination = if_else(
      is.na(covid_vaccination_immunity_date), "6-12m",
      time_since_last_covid_vaccination)
  )

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#import dummy data with composition
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) %>% 
  mutate(subset = "2020_21")

#composition
covid_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
covid_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
covid_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#import dummy data with vaccination
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2021_2022_specific_primary.arrow"))) %>%
  mutate(
    subset = "2021_22",
    time_since_last_covid_vaccination = if_else(
      is.na(covid_vaccination_immunity_date), "6-12m",
      time_since_last_covid_vaccination)
    )

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#import dummy data with composition
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) %>% 
  mutate(subset = "2020_21")

#composition
covid_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
covid_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
covid_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  covid_ethnicity_mild,
  covid_ethnicity_severe, 
  covid_ses_mild,
  covid_ses_severe, 
  covid_composition_mild,
  covid_composition_severe,
  covid_ethnicity_ses_mild,
  covid_ethnicity_ses_severe, 
  covid_ethnicity_composition_mild, 
  covid_ethnicity_composition_severe, 
  covid_ses_composition_mild,
  covid_ses_composition_severe,
  covid_full_mild,
  covid_full_severe
)
plot_names <- c(
  "covid_ethnicity_mild",
  "covid_ethnicity_severe",
  "covid_ses_mild",
  "covid_ses_severe",
  "covid_composition_mild",
  "covid_composition_severe",
  "covid_ethnicity_ses_mild",
  "covid_ethnicity_ses_severe",
  "covid_ethnicity_composition_mild",
  "covid_ethnicity_composition_severe",
  "covid_ses_composition_mild",
  "covid_ses_composition_severe",
  "covid_full_mild",
  "covid_full_severe"
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
             "_2020_2021_specific_primary.arrow"))) %>% 
  mutate(subset = "2020_21")

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  covid_ethnicity_mild,
  covid_ethnicity_severe,
  covid_ses_mild,
  covid_ses_severe,
  covid_ethnicity_ses_mild,
  covid_ethnicity_ses_severe
)
plot_names <- c(
  "covid_ethnicity_mild",
  "covid_ethnicity_severe",
  "covid_ses_mild",
  "covid_ses_severe",
  "covid_ethnicity_ses_mild",
  "covid_ethnicity_ses_severe"
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
             "_2020_2021_specific_primary.arrow"))) %>% 
  mutate(subset = "2020_21")

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
covid_ethnicity_mild_ <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  covid_ethnicity_mild,
  covid_ethnicity_severe, 
  covid_ses_mild,
  covid_ses_severe,
  covid_ethnicity_ses_mild,
  covid_ethnicity_ses_severe
)
plot_names <- c(
  "covid_ethnicity_mild",
  "covid_ethnicity_severe",
  "covid_ses_mild",
  "covid_ses_severe",
  "covid_ethnicity_ses_mild",
  "covid_ethnicity_ses_severe"
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
