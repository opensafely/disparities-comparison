# # # # # # # # # # # # # # # # # # # # #
# Purpose: Define the study dates that are used
# throughout the rest of the project
# Notes:
# This script is separate from the design.R script as the dates
# are used by the study definition as well as analysis R scripts.
# # # # # # # # # # # # # # # # # # # # #

library(lubridate)

## create output directories ----
fs::dir_create(here::here("analysis", "design"))

# define key dates ----

study_dates <- tibble::lst(
  study_start_date = ymd("2016-03-01"), # first possible study entry date
  study_end_date = ymd("2024-03-31"), # last study entry dates
  season1_start_date = ymd("2016-09-01"), #start of first season
  season1_end_date = ymd("2017-08-31"), #end of first season
  season2_start_date = ymd("2017-09-01"), #start of second season
  season2_end_date = ymd("2018-08-31"), #end of second season
  season3_start_date = ymd("2018-09-01"), #start of third season
  season3_end_date = ymd("2019-08-31"), #end of third season
  season4_start_date = ymd("2019-09-01"), #start of fourth season
  season4_end_date = ymd("2020-08-31"), #end of fourth season
  season5_start_date = ymd("2020-09-01"), #start of fifth season
  season5_end_date = ymd("2021-08-31"), #end of fifth season
  season6_start_date = ymd("2021-09-01"), #start of sixth season
  season6_end_date = ymd("2022-08-31"), #end of sixth season
  season7_start_date = ymd("2022-09-01"), #start of seventh season
  season7_end_date = ymd("2023-08-31"), #end of seventh season
  season8_start_date = ymd("2023-09-01"), #start of eighth season
  season8_end_date = ymd("2024-03-31") #end of eighth season
)

jsonlite::write_json(study_dates, path = here::here("analysis",
                     "design", "study-dates.json"), auto_unbox = TRUE,
                     pretty = TRUE)
