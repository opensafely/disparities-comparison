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
                                paste0(cohort, "_rates_primary_collated.csv")))

#import plot function
source(here::here("post_check", "functions", "rates_visualisation.R"))

##plot rates for infants

#rsv
infants_rsv_mild <- rate_viz(df_input, "RSV", "Mild")
infants_season_rsv_mild <- rate_viz_season(df_input, "RSV", "Mild")
infants_characters_rsv_mild <- rate_viz_mult(df_input, "RSV", "Mild")
infants_rsv_severe <- rate_viz(df_input, "RSV", "severe")
infants_season_rsv_severe <- rate_viz_season(df_input, "RSV", "Severe")
infants_characters_rsv_severe <- rate_viz_mult(df_input, "RSV", "Severe")

#flu
infants_flu_mild <- rate_viz(df_input, "Flu", "Mild")
infants_season_flu_mild <- rate_viz_season(df_input, "Flu", "Mild")
infants_characters_flu_mild <- rate_viz_mult(df_input, "Flu", "Mild")
infants_flu_severe <- rate_viz(df_input, "Flu", "Severe")
infants_season_flu_severe <- rate_viz_season(df_input, "Flu", "Severe")
infants_characters_flu_severe <- rate_viz_mult(df_input, "Flu", "Severe")

#covid
infants_covid_mild <- rate_viz(df_input, "COVID", "Mild")
infants_season_covid_mild <- rate_viz_season(df_input, "COVID", "Mild")
infants_characters_covid_mild <- rate_viz_mult(df_input, "COVID", "Mild")
infants_covid_severe <- rate_viz(df_input, "COVID", "Severe")
infants_season_covid_severe <- rate_viz_season(df_input, "COVID", "Severe")
infants_characters_covid_severe <- rate_viz_mult(df_input, "COVID", "Severe")

#overall
infants_overall_mild <- rate_viz(df_input, "Overall Respiratory", "Mild")
infants_season_overall_mild <- rate_viz_season(df_input, "Overall Respiratory",
                                               "Mild")
infants_characters_overall_mild <- rate_viz_mult(df_input, "Overall Respiratory",
                                                 "Mild")
infants_overall_severe <- rate_viz(df_input, "Overall Respiratory", "Severe")
infants_season_overall_severe <- rate_viz_season(df_input, "Overall Respiratory",
                                                 "Severe")
infants_characters_overall_severe <- rate_viz_mult(df_input, "Overall Respiratory",
                                                   "Severe")


#create list of plots
plotlist <- list(
  infants_rsv_mild, infants_season_rsv_mild, infants_characters_rsv_mild,
  infants_rsv_severe, infants_season_rsv_severe, infants_characters_rsv_severe,
  infants_flu_mild, infants_season_flu_mild, infants_characters_flu_mild,
  infants_flu_severe, infants_season_flu_severe, infants_characters_flu_severe,
  infants_covid_mild, infants_season_covid_mild, infants_characters_covid_mild,
  infants_covid_severe, infants_season_covid_severe,
  infants_characters_covid_severe, infants_overall_mild,
  infants_season_overall_mild, infants_characters_overall_mild,
  infants_overall_severe, infants_season_overall_severe,
  infants_characters_overall_severe
)

#plot all
for(p in plotlist) {
  
  print(p)
  
}
