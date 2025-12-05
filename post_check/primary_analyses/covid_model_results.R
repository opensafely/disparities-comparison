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
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#composition
covid_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#ses & composition
covid_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#full
covid_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#composition
covid_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#ses & composition
covid_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#full
covid_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
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
         paste0(cohort, "_", name, ".png")),
    p, height = 8, width = 15
  )
}

#assign plot names to list
names(plotlist) <- plot_names

#save Rdata
save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_covid_model_results.RData")))

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
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#composition
covid_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#ses & composition
covid_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#full
covid_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#composition
covid_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#ses & composition
covid_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#full
covid_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
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
         paste0(cohort, "_", name, ".png")),
    p, height = 8, width = 15
  )
}

#assign plot names to list
names(plotlist) <- plot_names

#save Rdata
save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_covid_model_results.RData")))

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
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#composition
covid_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#ses & composition
covid_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#full
covid_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#composition
covid_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#ethnicity & composition - too few events
covid_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#ses & composition
covid_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#full
covid_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe"
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
         paste0(cohort, "_", name, ".png")),
    p, height = 8, width = 15
  )
}

#assign plot names to list
names(plotlist) <- plot_names

#save Rdata
save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_covid_model_results.RData")))

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
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
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
         paste0(cohort, "_", name, ".png")),
    p, height = 8, width = 15
  )
}

#assign plot names to list
names(plotlist) <- plot_names

#save Rdata
save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_covid_model_results.RData")))

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
covid_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#ses
covid_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#ethnicity & ses
covid_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#ethnicity
covid_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#ses
covid_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#ethnicity & ses
covid_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
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
         paste0(cohort, "_", name, ".png")),
    p, height = 8, width = 15
  )
}

#assign plot names to list
names(plotlist) <- plot_names

#save Rdata
save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_covid_model_results.RData")))
