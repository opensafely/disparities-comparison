library(tidyverse)
library(here)
library(readr)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "older_adults"
} else {
  cohort <- args[[1]]
}

## create output directories ----
fs::dir_create(here::here("output", "collated", "descriptive", "over_time"))

##rates

collate_rates_season <- function(seasons, pathogen, characteristic) {
  
  df_group <- tibble()
  
  for (i in 1:length(seasons)) {
    
    season <- seasons[i]
    
    if (pathogen != "overall_resp") {
      df_spec <- read_csv(here::here("output", "results", "rates", "weekly",
                          paste0("rates_over_time_", pathogen, "_", cohort, "_",
                                 season, "_specific_primary.csv")))
      
      df_characteristic_spec <- df_spec %>%
        filter(group == characteristic) %>%
        mutate(
          codelist_type = "specific",
          season = gsub("_(\\d{2})\\d{2}", "_\\1", season)
        )
      
    }
    
    df_sens <- read_csv(here::here("output", "results", "rates", "weekly",
                        paste0("rates_over_time_", pathogen, "_", cohort, "_",
                               season, "_sensitive_primary.csv")))
    
    df_characteristic_sens <- df_sens %>%
      filter(group == characteristic) %>%
      mutate(
        codelist_type = "sensitive",
        season = gsub("_(\\d{2})\\d{2}", "_\\1", season)
      )
    
    if (pathogen != "overall_resp") {
      df_characteristic <- bind_rows(df_characteristic_spec,
                                     df_characteristic_sens)
    } else {
      df_characteristic <- df_characteristic_sens
    }
    
  }
  
  df_group <- bind_rows(df_group, df_characteristic)
  
  write_csv(df_group, here::here("output", "collated", "descriptive",
            "over_time", paste0(cohort, "_rates_over_time_", pathogen, "_",
                                characteristic, "_collated.csv")))
  
}

##define helper function to collate the rates
collate_rates_characteristic <- function(characteristics, pathogens) {
  
  for (i in 1:length(pathogens)) {
    
    pathogen <- pathogens[i]
    
    for (j in 1:length(characteristics)) {
      
      characteristic <- characteristics[j]
      
      if (pathogen == "covid") {
        collate_rates_season(seasons_covid, pathogen, characteristic)
      } else {
        collate_rates_season(seasons, pathogen, characteristic)
      }
      
    }
    
    if (pathogen == "covid") {
      collate_rates_season(seasons_covid, pathogen, "composition_category")
    } else {
      collate_rates_season(seasons, pathogen, "composition_category")
    }
    
  }
  
}

#define pathogens
pathogens <- c("rsv", "flu", "covid", "overall_resp")

#define seasons 
seasons <- c("2016_2017", "2017_2018", "2018_2019", "2019_2020", "2020_2021",
             "2021_2022", "2022_2023", "2023_2024")
seasons_covid <- c("2019_2020", "2020_2021", "2021_2022", "2022_2023",
                   "2023_2024")

#define characteristics
characteristics <- c("age_band", "sex", "latest_ethnicity_group",
                     "imd_quintile", "rurality_classification")

#run function
collate_rates_characteristic(characteristics, pathogens)
