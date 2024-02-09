# # # # # # # # # # # # # # # # # # # # #
# Purpose: Define the study dates that are used throughout the rest of the project
# Notes:
# This script is separate from the design.R script as the dates are used by the study definition as well as analysis R scripts.
# # # # # # # # # # # # # # # # # # # # #

## create output directories ----
fs::dir_create(here::here("analysis", "design"))

# define key dates ----

study_dates <- tibble::lst(
  study_start_date = "2016-03-01", # first possible study entry date (when HES data is first available)
  study_end_date = "2024-01-28", # last study entry dates
  season1_start_date = "2016-09-01", #start of first season
  season1_end_date = "2017-08-31", #end of first season
  season2_start_date = "2017-09-01", #start of first season
  season2_end_date = "2018-08-31", #end of first season
  season3_start_date = "2018-09-01", #start of first season
  season3_end_date = "2019-08-31", #end of first season
  season4_start_date = "2019-09-01", #start of first season
  season4_end_date = "2020-08-31", #end of first season
  season5_start_date = "2020-09-01", #start of first season
  season5_end_date = "2021-08-31", #end of first season
  season6_start_date = "2021-09-01", #start of first season
  season6_end_date = "2022-08-31", #end of first season
  season7_start_date = "2022-09-01", #start of first season
  season7_end_date = "2023-08-31", #end of first season 
)

jsonlite::write_json(study_dates, path = here::here("analysis", "design", "study-dates.json"), auto_unbox=TRUE, pretty =TRUE)
