library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)

#import plot function
source(here::here("post_check", "functions", "rolling_rates.R"))

investigation_type <- "primary"

seasons <- function(pathogen) {
  case_when(
    pathogen == "rsv" ~ "season2",
    pathogen == "flu" ~ "season3",
    pathogen == "covid" ~ "season5"
  )
}

###older adults

cohort <- "older_adults"

##plot rates for older_adults

#rsv
pathogen <- "rsv"
season <- seasons(pathogen)
create_rolling_plots(season, "specific")
create_rolling_plots_overall(season, "specific")
create_rolling_plots(season, "sensitive")
create_rolling_plots_overall(season, "sensitive")

#flu
pathogen <- "flu"
season <- seasons(pathogen)
create_rolling_plots(season, "specific")
create_rolling_plots_overall(season, "specific")
create_rolling_plots(season, "sensitive")
create_rolling_plots_overall(season, "sensitive")

#covid
pathogen <- "covid"
season <- seasons(pathogen)
create_rolling_plots(season, "specific")
create_rolling_plots_overall(season, "specific")
create_rolling_plots(season, "sensitive")
create_rolling_plots_overall(season, "sensitive")

###infants 

cohort <- "infants"

##plot rates for infants

#rsv
pathogen <- "rsv"
season <- seasons(pathogen)
create_rolling_plots(season, "specific")
create_rolling_plots_overall(season, "specific")
create_rolling_plots(season, "sensitive")
create_rolling_plots_overall(season, "sensitive")

#flu
pathogen <- "flu"
season <- seasons(pathogen)
create_rolling_plots(season, "specific")
create_rolling_plots_overall(season, "specific")
create_rolling_plots(season, "sensitive")
create_rolling_plots_overall(season, "sensitive")

#covid
pathogen <- "covid"
season <- seasons(pathogen)
create_rolling_plots(season, "specific")
create_rolling_plots_overall(season, "specific")
create_rolling_plots(season, "sensitive")
create_rolling_plots_overall(season, "sensitive")
