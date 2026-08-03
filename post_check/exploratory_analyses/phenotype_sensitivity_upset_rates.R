library(tidyverse)
library(here)

source(here::here("post_check", "functions", "phenotype_upset_rates.R"))

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

seasons <- c("2017_18", "2018_19", "2020_21", "2023_24")

## older adults ----------------------------------------------------------------
cohort <- "older_adults"

df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens <- upset_plot_rates(df_input, seasons)

ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
                  paste0(cohort, "_both_phenotype_sensitivity_seasons_rates.png")),
       phen_sens, height = 18, width = 12)

## infants ---------------------------------------------------------------------
cohort <- "infants"

df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens <- upset_plot_rates(df_input, seasons)

ggsave(here::here("post_check", "plots", "exploratory_analyses", "condensed",
                  paste0(cohort, "_both_phenotype_sensitivity_seasons_rates.png")),
       phen_sens, height = 18, width = 12)


## -- supplemental material ---------------------------------------------------

seasons <- c("2016_17", "2017_18", "2018_19", "2019_20",
             "2020_21", "2021_22", "2022_23", "2023_24")

## older adults ----------------------------------------------------------------
cohort <- "older_adults"

df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement_rates(df_input, seasons)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_mild_rates.png")),
       phen_sens_both[[1]], height = 18, width = 12)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_severe_rates.png")),
       phen_sens_both[[2]], height = 18, width = 12)

## adults ----------------------------------------------------------------------
cohort <- "adults"

df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement_rates(df_input, seasons)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_mild_rates.png")),
       phen_sens_both[[1]], height = 18, width = 12)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_severe_rates.png")),
       phen_sens_both[[2]], height = 18, width = 12)

## children and adolescents ----------------------------------------------------
cohort <- "children_and_adolescents"

df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement_rates(df_input, seasons)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_mild_rates.png")),
       phen_sens_both[[1]], height = 18, width = 12)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_severe_rates.png")),
       phen_sens_both[[2]], height = 18, width = 12)

## infants ---------------------------------------------------------------------
cohort <- "infants"

df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement_rates(df_input, seasons)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_mild_rates.png")),
       phen_sens_both[[1]], height = 18, width = 12)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_severe_rates.png")),
       phen_sens_both[[2]], height = 18, width = 12)

## infants subgroup ------------------------------------------------------------
cohort <- "infants_subgroup"

df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_phenotype_sensitivity_collated.csv")))

phen_sens_both <- upset_plot_supplement_rates(df_input, seasons)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_mild_rates.png")),
       phen_sens_both[[1]], height = 18, width = 12)

ggsave(here::here("post_check", "plots", "supplemental",
                  paste0(cohort, "_both_phenotype_sensitivity_severe_rates.png")),
       phen_sens_both[[2]], height = 18, width = 12)
