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
pathogen <- "overall_and_all_cause"
investigation_type <- "primary"

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_sensitive_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
overall_and_all_cause_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
overall_and_all_cause_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#composition
overall_and_all_cause_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#ethnicity & composition - too few events
overall_and_all_cause_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
overall_and_all_cause_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
overall_and_all_cause_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
overall_and_all_cause_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
overall_and_all_cause_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#composition
overall_and_all_cause_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#ethnicity & composition - too few events
overall_and_all_cause_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
overall_and_all_cause_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
overall_and_all_cause_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  overall_and_all_cause_ethnicity_mild,
  overall_and_all_cause_ethnicity_severe, 
  overall_and_all_cause_ses_mild,
  overall_and_all_cause_ses_severe, 
  overall_and_all_cause_composition_mild,
  overall_and_all_cause_composition_severe,
  overall_and_all_cause_ethnicity_ses_mild,
  overall_and_all_cause_ethnicity_ses_severe, 
  overall_and_all_cause_ethnicity_composition_mild, 
  overall_and_all_cause_ethnicity_composition_severe, 
  overall_and_all_cause_ses_composition_mild,
  overall_and_all_cause_ses_composition_severe,
  overall_and_all_cause_full_mild,
  overall_and_all_cause_full_severe
)
plot_names <- c(
  "overall_and_all_cause_ethnicity_mild",
  "overall_and_all_cause_ethnicity_severe",
  "overall_and_all_cause_ses_mild",
  "overall_and_all_cause_ses_severe",
  "overall_and_all_cause_composition_mild",
  "overall_and_all_cause_composition_severe",
  "overall_and_all_cause_ethnicity_ses_mild",
  "overall_and_all_cause_ethnicity_ses_severe",
  "overall_and_all_cause_ethnicity_composition_mild",
  "overall_and_all_cause_ethnicity_composition_severe",
  "overall_and_all_cause_ses_composition_mild",
  "overall_and_all_cause_ses_composition_severe",
  "overall_and_all_cause_full_mild",
  "overall_and_all_cause_full_severe"
)

names(plotlist) <- plot_names

phenotype_plotlists <- save_supplemental_base_model_plots(
  plotlist, plot_names, cohort, plot_name_suffix = "_further"
)

plotlist_specific <- phenotype_plotlists$specific
plotlist_sensitive <- phenotype_plotlists$sensitive

save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_overall_resp_further_model_results.RData")))
save(plotlist_specific,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_specific.RData")))
save(plotlist_sensitive,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_sensitive.RData")))

###adults

cohort <- "adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_sensitive_primary.arrow")))
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
overall_and_all_cause_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
overall_and_all_cause_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#composition
overall_and_all_cause_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#ethnicity & composition - too few events
overall_and_all_cause_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
overall_and_all_cause_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
overall_and_all_cause_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
overall_and_all_cause_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
overall_and_all_cause_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#composition
overall_and_all_cause_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#ethnicity & composition - too few events
overall_and_all_cause_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
overall_and_all_cause_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
overall_and_all_cause_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  overall_and_all_cause_ethnicity_mild,
  overall_and_all_cause_ethnicity_severe, 
  overall_and_all_cause_ses_mild,
  overall_and_all_cause_ses_severe, 
  overall_and_all_cause_composition_mild,
  overall_and_all_cause_composition_severe,
  overall_and_all_cause_ethnicity_ses_mild,
  overall_and_all_cause_ethnicity_ses_severe, 
  overall_and_all_cause_ethnicity_composition_mild, 
  overall_and_all_cause_ethnicity_composition_severe, 
  overall_and_all_cause_ses_composition_mild,
  overall_and_all_cause_ses_composition_severe,
  overall_and_all_cause_full_mild,
  overall_and_all_cause_full_severe
)
plot_names <- c(
  "overall_and_all_cause_ethnicity_mild",
  "overall_and_all_cause_ethnicity_severe",
  "overall_and_all_cause_ses_mild",
  "overall_and_all_cause_ses_severe",
  "overall_and_all_cause_composition_mild",
  "overall_and_all_cause_composition_severe",
  "overall_and_all_cause_ethnicity_ses_mild",
  "overall_and_all_cause_ethnicity_ses_severe",
  "overall_and_all_cause_ethnicity_composition_mild",
  "overall_and_all_cause_ethnicity_composition_severe",
  "overall_and_all_cause_ses_composition_mild",
  "overall_and_all_cause_ses_composition_severe",
  "overall_and_all_cause_full_mild",
  "overall_and_all_cause_full_severe"
)

names(plotlist) <- plot_names

phenotype_plotlists <- save_supplemental_base_model_plots(
  plotlist, plot_names, cohort, plot_name_suffix = "_further"
)

plotlist_specific <- phenotype_plotlists$specific
plotlist_sensitive <- phenotype_plotlists$sensitive

save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_overall_resp_further_model_results.RData")))
save(plotlist_specific,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_specific.RData")))
save(plotlist_sensitive,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_sensitive.RData")))

###children and adolescents

cohort <- "children_and_adolescents"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_sensitive_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
overall_and_all_cause_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
overall_and_all_cause_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#composition
overall_and_all_cause_composition_mild <- forest(
  df_input, df_dummy, pathogen, "composition", "Mild", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

#ethnicity & composition - too few events
overall_and_all_cause_ethnicity_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild", "yes"
)

#ses & composition
overall_and_all_cause_ses_composition_mild <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Mild", "yes"
)

#full
overall_and_all_cause_full_mild <- forest(
  df_input, df_dummy, pathogen, "full", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
overall_and_all_cause_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
overall_and_all_cause_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#composition
overall_and_all_cause_composition_severe <- forest(
  df_input, df_dummy, pathogen, "composition", "Severe", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#ethnicity & composition - too few events
overall_and_all_cause_ethnicity_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe", "yes"
)

#ses & composition
overall_and_all_cause_ses_composition_severe <- forest(
  df_input, df_dummy, pathogen, "ses_composition", "Severe", "yes"
)

#full
overall_and_all_cause_full_severe <- forest(
  df_input, df_dummy, pathogen, "full", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  overall_and_all_cause_ethnicity_mild,
  overall_and_all_cause_ethnicity_severe, 
  overall_and_all_cause_ses_mild,
  overall_and_all_cause_ses_severe, 
  overall_and_all_cause_composition_mild,
  overall_and_all_cause_composition_severe,
  overall_and_all_cause_ethnicity_ses_mild,
  overall_and_all_cause_ethnicity_ses_severe, 
  overall_and_all_cause_ethnicity_composition_mild, 
  overall_and_all_cause_ethnicity_composition_severe, 
  overall_and_all_cause_ses_composition_mild,
  overall_and_all_cause_ses_composition_severe,
  overall_and_all_cause_full_mild,
  overall_and_all_cause_full_severe
)
plot_names <- c(
  "overall_and_all_cause_ethnicity_mild",
  "overall_and_all_cause_ethnicity_severe",
  "overall_and_all_cause_ses_mild",
  "overall_and_all_cause_ses_severe",
  "overall_and_all_cause_composition_mild",
  "overall_and_all_cause_composition_severe",
  "overall_and_all_cause_ethnicity_ses_mild",
  "overall_and_all_cause_ethnicity_ses_severe",
  "overall_and_all_cause_ethnicity_composition_mild",
  "overall_and_all_cause_ethnicity_composition_severe",
  "overall_and_all_cause_ses_composition_mild",
  "overall_and_all_cause_ses_composition_severe",
  "overall_and_all_cause_full_mild",
  "overall_and_all_cause_full_severe"
)

names(plotlist) <- plot_names

phenotype_plotlists <- save_supplemental_base_model_plots(
  plotlist, plot_names, cohort, plot_name_suffix = "_further"
)

plotlist_specific <- phenotype_plotlists$specific
plotlist_sensitive <- phenotype_plotlists$sensitive

save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_overall_resp_further_model_results.RData")))
save(plotlist_specific,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_specific.RData")))
save(plotlist_sensitive,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_sensitive.RData")))

###infants

cohort <- "infants"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_sensitive_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
overall_and_all_cause_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
overall_and_all_cause_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
overall_and_all_cause_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
overall_and_all_cause_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  overall_and_all_cause_ethnicity_mild,
  overall_and_all_cause_ethnicity_severe,
  overall_and_all_cause_ses_mild,
  overall_and_all_cause_ses_severe,
  overall_and_all_cause_ethnicity_ses_mild,
  overall_and_all_cause_ethnicity_ses_severe
)
plot_names <- c(
  "overall_and_all_cause_ethnicity_mild",
  "overall_and_all_cause_ethnicity_severe",
  "overall_and_all_cause_ses_mild",
  "overall_and_all_cause_ses_severe",
  "overall_and_all_cause_ethnicity_ses_mild",
  "overall_and_all_cause_ethnicity_ses_severe"
)

names(plotlist) <- plot_names

phenotype_plotlists <- save_supplemental_base_model_plots(
  plotlist, plot_names, cohort, plot_name_suffix = "_further"
)

plotlist_specific <- phenotype_plotlists$specific
plotlist_sensitive <- phenotype_plotlists$sensitive

save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_overall_resp_further_model_results.RData")))
save(plotlist_specific,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_specific.RData")))
save(plotlist_sensitive,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_sensitive.RData")))

###infants_subgroup

cohort <- "infants_subgroup"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                            "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_sensitive_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#ethnicity
overall_and_all_cause_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild", "yes"
)

#ses
overall_and_all_cause_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild", "yes"
)

##create relevant forest plots - severe

#ethnicity
overall_and_all_cause_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe", "yes"
)

#ses
overall_and_all_cause_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe", "yes"
)

#ethnicity & ses
overall_and_all_cause_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe", "yes"
)

#create list of plots
plotlist <- list(
  overall_and_all_cause_ethnicity_mild,
  overall_and_all_cause_ethnicity_severe,
  overall_and_all_cause_ses_mild,
  overall_and_all_cause_ses_severe,
  overall_and_all_cause_ethnicity_ses_mild,
  overall_and_all_cause_ethnicity_ses_severe
)
plot_names <- c(
  "overall_and_all_cause_ethnicity_mild",
  "overall_and_all_cause_ethnicity_severe",
  "overall_and_all_cause_ses_mild",
  "overall_and_all_cause_ses_severe",
  "overall_and_all_cause_ethnicity_ses_mild",
  "overall_and_all_cause_ethnicity_ses_severe"
)

names(plotlist) <- plot_names

phenotype_plotlists <- save_supplemental_base_model_plots(
  plotlist, plot_names, cohort, height = 10, plot_name_suffix = "_further"
)

plotlist_specific <- phenotype_plotlists$specific
plotlist_sensitive <- phenotype_plotlists$sensitive

save(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_overall_resp_further_model_results.RData")))
save(plotlist_specific,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_specific.RData")))
save(plotlist_sensitive,
     file = here("post_check", "supplemental", "dashboard",
                 paste0(cohort, "_overall_resp_further_model_results_sensitive.RData")))
