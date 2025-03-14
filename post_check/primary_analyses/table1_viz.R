library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)

#import plot function
source(here::here("post_check", "functions", "characteristic_visualisation.R"))

###infants

cohort <- "infants"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))

##create relevant plots

#season
infants_season <- character_viz(df_input)

#characteristic
infants_characters <- character_viz_mult(df_input)

#create list of plots
plotlist <- list(infants_characters[[1]], infants_characters[[2]])

#plot all
print(infants_season)
title_name <- infants_season$labels$title
saveas <- paste0(gsub(" ", "_", title_name))
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       infants_season, height = 8, width = 15)

t <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist) {
  
  print(p)
  t <- t+1
  title_name <- infants_characters[[t]]
  saveas <- title_name
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

###older adults

cohort <- "older_adults"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))

##create relevant plots

#season
older_adults_season <- character_viz(df_input)

#characteristic
older_adults_characters <- character_viz_mult(df_input)

#create list of plots
plotlist <- list(older_adults_characters[[1]], older_adults_characters[[2]])

#plot all
print(older_adults_season)
title_name <- older_adults_season$labels$title
saveas <- paste0(gsub(" ", "_", title_name))
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       older_adults_season, height = 8, width = 15)

t <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist) {
  
  print(p)
  t <- t+1
  title_name <- older_adults_characters[[t]]
  saveas <- title_name
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}
