library(tidyverse)
library(here)
library(arrow)
library(broom)
library(broom.helpers)
library(lubridate)
library(cowplot)
library(stringr)
library(patchwork)
library(ggpubr)

#import plot function
source(here::here("post_check", "functions", "forest.R"))

investigation_type <- "primary"

###older adults

cohort <- "older_adults"

##rsv
pathogen <- "rsv"

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

#plot both phenotypes together
rsv_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
rsv_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
rsv_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#plot both phenotypes together
rsv_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#plot both phenotypes together
rsv_ethnicity_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#plot both phenotypes together
rsv_ses_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#plot both phenotypes together
rsv_full_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
rsv_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
rsv_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
rsv_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#plot both phenotypes together
rsv_ethnicity_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#plot both phenotypes together
rsv_ses_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#plot both phenotypes together
rsv_full_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Severe"
)

##flu
pathogen <- "flu"

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

#plot both phenotypes together
flu_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
flu_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
flu_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#plot both phenotypes together
flu_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#plot both phenotypes together
flu_ethnicity_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#plot both phenotypes together
flu_ses_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#plot both phenotypes together
flu_full_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
flu_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
flu_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
flu_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#plot both phenotypes together
flu_ethnicity_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#plot both phenotypes together
flu_ses_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#plot both phenotypes together
flu_full_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Severe"
)

##covid
pathogen <- "covid"

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

#plot both phenotypes together
covid_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
covid_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
covid_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#plot both phenotypes together
covid_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#plot both phenotypes together
covid_ethnicity_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#plot both phenotypes together
covid_ses_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#plot both phenotypes together
covid_full_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
covid_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
covid_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
covid_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#plot both phenotypes together
covid_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#plot both phenotypes together
covid_ethnicity_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#plot both phenotypes together
covid_ses_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#plot both phenotypes together
covid_full_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Severe"
)


#--- patch pathogens together
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

##mild

ethnicity_ses <- ggarrange(rsv_ethnicity_ses_mild, flu_ethnicity_ses_mild,
                           covid_ethnicity_ses_mild, nrow = 3,
                           common.legend = TRUE, legend = "right",
                           labels = c("RSV", "Flu", "COVID-19"),
                           hjust = 0, label.x = 0.01)
# 
# ethnicity_ses <- annotate_figure(ethnicity_ses, top = text_grob(
#   "Rate Ratios of Mild Disease", face = "bold", size = 14))

ggsave(here("post_check", "plots", "primary_analyses", "models", "condensed",
       paste0(cohort, "_ethnicity_ses", ".png")),
       ethnicity_ses, height = 10, width = 15)

###infants

cohort <- "infants"

##rsv
pathogen <- "rsv"

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

#plot both phenotypes together
rsv_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
rsv_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
rsv_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
rsv_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
rsv_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##flu
pathogen <- "flu"

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

#plot both phenotypes together
flu_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
flu_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
flu_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
flu_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
flu_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##covid
pathogen <- "covid"

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

#plot both phenotypes together
covid_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
covid_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
covid_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
covid_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
covid_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
covid_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)


#--- patch pathogens together
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

##mild

ethnicity_ses <- ggarrange(rsv_ethnicity_ses_mild, flu_ethnicity_ses_mild,
                           covid_ethnicity_ses_mild, nrow = 3,
                           common.legend = TRUE, legend = "right",
                           labels = c("RSV", "Flu", "COVID-19"),
                           hjust = 0, label.x = 0.01)
# 
# ethnicity_ses <- annotate_figure(ethnicity_ses, top = text_grob(
#   "Rate Ratios of Mild Disease", face = "bold", size = 14))

ggsave(here("post_check", "plots", "primary_analyses", "models", "condensed",
       paste0(cohort, "_ethnicity_ses", ".png")),
       ethnicity_ses, height = 10, width = 15)
