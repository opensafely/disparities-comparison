library(tidyverse)
library(here)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(stringr)
#library(ggeasy)
library(forcats)
library(cowplot)

#import plot function
source(here::here("analysis", "testing", "functions", "phenotype_upset.R"))
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

seasons <- c("2017_18", "2018_19", "2020_21", "2023_24")

args <- commandArgs(trailingOnly = TRUE)
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cohort <- "older_adults"
  } else {
    cohort <- args[[1]]
  }
}

#import collated phenotype sensitivity data
df_input <- read_csv(here::here("output", "collated", "descriptive",
                     paste0(cohort, "_phenotype_sensitivity_collated.csv")))

phen_sens <- upset_plot(df_input, seasons)

ggsave(here::here("output", "testing", "plots", paste0(
       cohort, "_both_phenotype_sensitivity_seasons", ".png")),
       phen_sens, height = 18, width = 12)
