library(VennDiagram)
library(tidyverse)
library(here)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(stringr)

#import plot function
source(here::here("post_check", "functions", "phenotype_venn.R"))

seasons <- c("2016_17", "2017_18", "2018_19", "2019_20", "2020_21", 
             "2021_22", "2022_23", "2023_24")

##  older adults 
cohort <- "older_adults"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                    "_phenotype_sensitivity_collated.csv")))

for (i in 1:length(seasons)) {
  venn(df_input, seasons[i], "mild")
  venn(df_input, seasons[i], "severe")
}

##  infants
cohort <- "infants"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                    "_phenotype_sensitivity_collated.csv")))

for (i in 1:length(seasons)) {
  venn(df_input, seasons[i], "mild")
  venn(df_input, seasons[i], "severe")
}
