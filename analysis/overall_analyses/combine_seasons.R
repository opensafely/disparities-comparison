library(plyr)
library(tidyverse)
library(here)
library(arrow)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "adults"
} else {
  cohort <- args[[1]]
}

source(here::here("analysis", "functions", "redaction.R"))

## create output directories ----
fs::dir_create(here::here("analysis", "overall_analyses"))

#define the files to read
files <- c("2016_2017", "2017_2018", "2018_2019", "2019_2020",
           "2020_2021", "2021_2022", "2022_2023", "2023_2024")
collated_input_processed <- list()

#initialize a vector to track all columns
all_columns <- NULL

#read each file
for (file in files) {
  
  file_path <- here::here("output", "data", paste0("input_processed_",
                          cohort, "_", file, "_specific_primary.arrow"))
  
  if (file.exists(file_path)) {
    
    df <- read_feather(file_path) %>%
      #add subset column
      mutate(subset = gsub("_(\\d{4})_(\\d{2})\\d{2}", "_\\2", file)) %>%
      #relocate the column
      relocate(subset, .before = patient_id)
    collated_input_processed[[file]] <- df
    
    #update the list of all columns
    all_columns <- union(all_columns, colnames(df))
    
  } else {
    
    message("File not found: ", file_path)
    
  }
  
}

#make sure all data frames have the same columns
for (i in 1:length(collated_input_processed)) {
  
  df <- collated_input_processed[[i]]
  
  #add missing columns with NA values
  missing_cols <- setdiff(all_columns, colnames(df))
  
  #add missing column as NA
  for (col in missing_cols) df[[col]] <- NA
  
  #reorder the columns to match the full column set
  df <- df[, all_columns]
  
  #update the list with the modified data frame
  collated_input_processed[[i]] <- df %>%
    select(subset, patient_id, sex, age_band, latest_ethnicity_group,
           imd_quintile, rurality_classification, prior_flu_vaccination,
           flu_vaccination_mild, flu_vaccination_severe,
           time_since_last_covid_vaccination, covid_vaccination_mild,
           covid_vaccination_severe, rsv_primary_inf, time_rsv_primary,
           rsv_secondary_inf, time_rsv_secondary, flu_primary_inf,
           time_flu_primary, flu_secondary_inf, time_flu_secondary,
           covid_primary_inf, time_covid_primary, covid_secondary_inf,
           time_covid_secondary)
  
}

#combine all data frames into one
collated_input_processed <- do.call(rbind, collated_input_processed)

#remove row names
row.names(collated_input_processed) <- NULL

## create output directories ----
fs::dir_create(here::here("output", "data", "overall_analyses"))

#save the combined data frame
write_feather(collated_input_processed,
              here::here("output", "data", "overall_analyses",
              paste0("input_processed_combined_", cohort,
              "_specific_primary.arrow")))
