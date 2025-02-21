library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)

## create output directories ----
fs::dir_create(here("post_check", "primary_analyses"))

cohort <- "infants"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive",
                     paste0(cohort, "_table1_collated.csv")))

#import plot function
source(here::here("post_check", "functions", "characteristic_visualisation.R"))

##create relevant plots

#season
infants_season <- character_viz(df_input)

#characteristic
infants_characters <- character_viz_mult(df_input)

#create list of plots
plotlist <- list(infants_season, infants_characters)

#plot all
for(p in plotlist) {
  
  print(p)
  
}
