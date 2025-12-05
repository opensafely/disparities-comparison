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
investigation_type <- "sensitivity"

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "analytic", "sensitivity", paste0(cohort, "_", pathogen,
                     "_model_outputs_collated_sensitivity.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2018_2019_specific_sensitivity.arrow"))) 


#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
flu_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#ses
flu_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#ethnicity & ses
flu_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#ses
flu_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#ethnicity & ses
flu_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
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
    here("post_check", "plots", "supplemental", "models", cohort, "sensitivity",
         paste0(cohort, "_", name, ".png")),
    p, height = 8, width = 15
  )
}

#assign plot names to list
names(plotlist) <- plot_names

#save Rdata
save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_flu_model_results_sensitivity.RData")))

###infants

cohort <- "infants"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "analytic", "sensitivity", paste0(cohort, "_", pathogen,
                     "_model_outputs_collated_sensitivity.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2018_2019_specific_sensitivity.arrow"))) 


#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
flu_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#ses
flu_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#ethnicity & ses
flu_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#ethnicity
flu_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#ses
flu_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#ethnicity & ses
flu_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
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
    here("post_check", "plots", "supplemental", "models", cohort, "sensitivity",
         paste0(cohort, "_", name, ".png")),
    p, height = 8, width = 15
  )
}

#assign plot names to list
names(plotlist) <- plot_names

#save Rdata
save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_flu_model_results_sensitivity.RData"))
    )
