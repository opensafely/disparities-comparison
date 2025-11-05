library(tidyverse)
library(here)

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

phen_sens <- upset_plot(df_input, seasons)

ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
                  paste0(cohort, "_both_phenotype_sensitivity_seasons", ".png")),
       phen_sens, height = 18, width = 12)


## -- supplemental material

seasons <- c("2016_17", "2017_18", "2018_19", "2019_20",
             "2020_21", "2021_22", "2022_23", "2023_24")

##  older adults 
cohort <- "older_adults"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement(df_input, seasons)

phen_sens_mild <- phen_sens_both[[1]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_mild", ".png")),
       phen_sens_mild, height = 18, width = 12)

phen_sens_severe <- phen_sens_both[[2]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_severe", ".png")),
       phen_sens_severe, height = 18, width = 12)

##  adults 
cohort <- "adults"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement(df_input, seasons)

phen_sens_mild <- phen_sens_both[[1]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_mild", ".png")),
       phen_sens_mild, height = 18, width = 12)

phen_sens_severe <- phen_sens_both[[2]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_severe", ".png")),
       phen_sens_severe, height = 18, width = 12)

##  children and adolescents 
cohort <- "children_and_adolescents"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement(df_input, seasons)

phen_sens_mild <- phen_sens_both[[1]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_mild", ".png")),
       phen_sens_mild, height = 18, width = 12)

phen_sens_severe <- phen_sens_both[[2]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_severe", ".png")),
       phen_sens_severe, height = 18, width = 12)

##  infants 
cohort <- "infants"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement(df_input, seasons)

phen_sens_mild <- phen_sens_both[[1]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_mild", ".png")),
       phen_sens_mild, height = 18, width = 12)

phen_sens_severe <- phen_sens_both[[2]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_severe", ".png")),
       phen_sens_severe, height = 18, width = 12)

##  infants subgroup 
cohort <- "infants_subgroup"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement(df_input, seasons)

phen_sens_mild <- phen_sens_both[[1]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_mild", ".png")),
       phen_sens_mild, height = 18, width = 12)

phen_sens_severe <- phen_sens_both[[2]]

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_both_phenotype_sensitivity_severe", ".png")),
       phen_sens_severe, height = 18, width = 12)
