library(tidyverse)
library(here)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(stringr)
library(ggeasy)
library(forcats)
library(cowplot)

#import plot function
source(here::here("post_check", "functions", "phenotype_upset.R"))

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

seasons <- c("2017_18", "2018_19", "2020_21", "2023_24")

##  older adults 
cohort <- "older_adults"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

# phen_sens_mild <- upset_plot(df_input, seasons)$Mild
# 
# ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
#             paste0(cohort, "_mild_phenotype_sensitivity_seasons", ".png")),
#        phen_sens_mild, height = 10, width = 15)
# 
# phen_sens_severe <- upset_plot(df_input, seasons)$Severe
# 
# ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
#             paste0(cohort, "_severe_phenotype_sensitivity_seasons", ".png")),
#        phen_sens_severe, height = 10, width = 15)

phen_sens <- upset_plot(df_input, seasons)

ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
            paste0(cohort, "_both_phenotype_sensitivity_seasons", ".png")),
       phen_sens, height = 18, width = 12)

##  infants
cohort <- "infants"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

# phen_sens_mild <- upset_plot(df_input, seasons)$Mild
# 
# ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
#             paste0(cohort, "_mild_phenotype_sensitivity_seasons", ".png")),
#        phen_sens_mild, height = 10, width = 15)
# 
# phen_sens_severe <- upset_plot(df_input, seasons)$Severe
# 
# ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
#             paste0(cohort, "_severe_phenotype_sensitivity_seasons", ".png")),
#        phen_sens_severe, height = 10, width = 15)

phen_sens <- upset_plot(df_input, seasons)

ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
                  paste0(cohort, "_both_phenotype_sensitivity_seasons", ".png")),
       phen_sens, height = 18, width = 12)

##  infants subgroup
cohort <- "infants_subgroup"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

# phen_sens_mild <- upset_plot(df_input, seasons)$Mild
# 
# ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
#             paste0(cohort, "_mild_phenotype_sensitivity_seasons", ".png")),
#        phen_sens_mild, height = 10, width = 15)
# 
# phen_sens_severe <- upset_plot(df_input, seasons)$Severe
# 
# ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
#             paste0(cohort, "_severe_phenotype_sensitivity_seasons", ".png")),
#        phen_sens_severe, height = 10, width = 15)

phen_sens <- upset_plot(df_input, seasons)

ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
                  paste0(cohort, "_both_phenotype_sensitivity_seasons", ".png")),
       phen_sens, height = 18, width = 12)
