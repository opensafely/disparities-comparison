library(gtsummary)
library(tidyr)
library(readr)
library(dplyr)

#import functions
source(here::here("post_check", "functions", "episodes.R"))

seasons <- c("2017_18", "2018_19", "2020_21", "2023_24")

##older adults

cohort <- "older_adults"

#specific phenotype
phenotype <- "specific"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_older_adults <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_older_adults <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_older_adults <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

#sensitive phenotype
phenotype <- "sensitive"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_older_adults <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_older_adults <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_older_adults <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

# #import collated multiple episode outputs
# df_input <- read_csv(
#   here::here("post_check", "output", "collated", "descriptive",
#              paste0(cohort, "_multiple_episodes_collated.csv")))
# 
# multiple_episodes(df_input, "mild")
# multiple_episodes(df_input, "severe")

##infants

cohort <- "infants"

#specific phenotype
phenotype <- "specific"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_older_adults <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_older_adults <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_older_adults <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

#sensitive phenotype
phenotype <- "sensitive"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_older_adults <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_older_adults <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_older_adults <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_older_adults, here::here("post_check", "plots", "exploratory_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))
