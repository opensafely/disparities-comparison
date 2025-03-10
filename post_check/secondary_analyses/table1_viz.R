library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)

## create output directories ----
fs::dir_create(here("post_check", "secondary_analyses"))

cohort <- "older_adults"
investigation_type <- "secondary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive",
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
older_adults_season <- character_viz(df_input)

#characteristic
older_adults_characters <- character_viz_mult(df_input)

#create list of plots
plotlist <- list(older_adults_season, older_adults_characters[[1]],
                 older_adults_characters[[2]])

#plot all
for(p in plotlist) {
  
  print(p)
  
}
