library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)

cohort <- "older_adults"
investigation_type <- "secondary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                                "descriptive",
                     paste0(cohort, "_secondary_table1_collated.csv")))
df_input <- df_input %>%
  mutate(Characteristic = str_replace_all(Characteristic, "Diseases",
                                          "Disease")
  ) %>%
  filter(!str_detect(Characteristic, "Flu"))

#import plot function
source(here::here("post_check", "functions", "characteristic_visualisation.R"))

##create relevant plots

#season
older_adults_season <- character_viz(df_input, "none")
older_adults_season_scaled <- character_viz(df_input, "yes")

#characteristic
older_adults_characters <- character_viz_mult(df_input, "none")
older_adults_characters_scaled <- character_viz_mult(df_input, "yes")

#create list of plots
plotlist1 <- list(older_adults_characters_scaled[[1]],
                  older_adults_characters_scaled[[2]],
                  older_adults_characters_scaled[[3]])
plotlist2 <- list(older_adults_characters[[1]], older_adults_characters[[2]],
                  older_adults_characters[[3]])

#plot all
print(older_adults_season_scaled)
title_name <- older_adults_season_scaled$labels$title
saveas <- paste0(gsub(" ", "_", title_name), "_scaled")
ggsave(here("post_check", "plots", "secondary_analyses",
            paste0(saveas, ".png")), older_adults_season_scaled,
       height = 8, width = 15)
print(older_adults_season)
title_name <- older_adults_season$labels$title
saveas <- paste0(gsub(" ", "_", title_name))
ggsave(here("post_check", "plots", "secondary_analyses",
            paste0(saveas, ".png")), older_adults_season,
       height = 8, width = 15)

t <- 3
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist1) {

  print(p)
  t <- t+1
  title_name <- paste0(older_adults_characters_scaled[[t]], "_scaled")
  saveas <- title_name
  ggsave(here("post_check", "plots", "secondary_analyses",
              paste0(saveas, ".png")), p, height = 8, width = 15)
  
}
u <- 3
for(p in plotlist2) {
  
  print(p)
  u <- u+1
  title_name <- older_adults_characters[[u]]
  saveas <- title_name
  ggsave(here("post_check", "plots", "secondary_analyses",
              paste0(saveas, ".png")), p, height = 8, width = 15)
  
}
