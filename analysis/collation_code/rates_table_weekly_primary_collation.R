library(plyr)
library(tidyverse)
library(here)
library(arrow)

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

#define outcomes 
pathogens <- c("rsv", "flu", "covid", "overall_resp", "all_cause")

#create function to collate rates for a specific season
collate_rates <- function(season) {
  
  subset <- sub("_(\\d{2})\\d{2}$", "_\\1", season)
  
  # Create an empty list to store dataframes for each pathogen
  rates_over_time_collated <- list()
  
  for (pathogen in pathogens) {
    
    # Skip "covid" if the season is not in the allowed range
    if (pathogen == "covid" && !season %in% c("2019_2020", "2021_2022",
                                              "2022_2023", "2023_2024")) {
      next
    }
    
    # Initialize an empty tibble for this pathogen
    rates_over_time_collated[[pathogen]] <- tibble()
    
    # Define file patterns and metadata
    file_patterns <- list(
      list(suffix = "age_band", characteristic = "age_band"),
      list(suffix = "sex", characteristic = "sex"),
      list(suffix = "latest_ethnicity_group", characteristic = "latest_ethnicity_group")
    )
    
    codelist_types <- if (pathogen == "overall_resp") c("sensitive") else c("specific", "sensitive")
    
    # Loop through each combination of file pattern and codelist type
    for (pattern in file_patterns) {
      
      for (codelist_type in codelist_types) {
        
        # Construct the file path
        file_path <- here::here(
          "output", "results", "rates", "weekly",
          paste0(
            "rates_over_time_weekly_", pathogen, "_",
            pattern$suffix, "_", cohort, "_", season, "_",
            codelist_type, "_primary.csv"
          )
        )
        
        # # Debug: print the file path and existence check
        # print(paste("Checking file:", file_path))
        # print(paste("File exists:", file.exists(file_path)))
        
        # Read the file if it exists, and append to the tibble
        if (file.exists(file_path)) {
          rates_over_time_collated[[pathogen]] <- bind_rows(
            rates_over_time_collated[[pathogen]],
            read_csv(file_path) %>%
              mutate(
                interval_beginning = as.Date(interval_beginning,
                                             origin = "1970-01-01"),
                codelist_type = codelist_type,
                investigation_type = "primary",
                subset = subset,
                characteristic = pattern$characteristic
              )
          )
          
        } else {
          
          print(paste("File not found:", file_path))
          
        }
        
      }
      
    }
    
  }
  
  return(rates_over_time_collated)
  
}

seasons <- c("2016_2017", "2017_2018", "2018_2019", "2019_2020", "2020_2021",
             "2021_2022", "2022_2023", "2023_2024")

for (i in 1:length(seasons)) {
  
  season <- seasons[i]
  assign(paste0("rsv_s", i, "_", cohort, "_", season),
         collate_rates(season)[["rsv"]])
  assign(paste0("flu_s", i, "_", cohort, "_", season),
         collate_rates(season)[["flu"]])
  assign(paste0("overall_resp_s", i, "_", cohort, "_", season),
         collate_rates(season)[["overall_resp"]])
  assign(paste0("all_cause_s", i, "_", cohort, "_", season),
         collate_rates(season)[["all_cause"]])
  
}

for (i in 4:(length(seasons))) {
  
  season <- seasons[i]
  assign(paste0("covid_s", i, "_", cohort, "_", season),
         collate_rates(season)[["covid"]])
  
}

# Function to save collated rates as CSV files
save_collated_rates <- function(seasons, pathogens, cohort) {
  
  for (i in seq_along(seasons)) {
    
    season <- seasons[i]
    
    # Iterate over pathogens and save the respective datasets
    for (pathogen in pathogens) {
      
      # Skip saving for "covid" if the season is not in the allowed range
      if (pathogen == "covid" && !season %in% c("2019_2020", "2021_2022",
                                                "2022_2023", "2023_2024")) {
        next
      }
      
      # Construct variable name and retrieve the dataset
      variable_name <- paste0(pathogen, "_s", i, "_", cohort, "_", season)
      dataset <- get(variable_name, envir = .GlobalEnv)
      
      # Skip if the dataset is empty or NULL
      if (is.null(dataset) || nrow(dataset) == 0) {
        next
      }
      
      # Define output file path
      output_file_path <- here::here(
        "output", "collated", "descriptive", "over_time",
        paste0(pathogen, "_", cohort, "_rates_weekly_primary_collated_",
               season, ".csv")
      )
      
      # Save the dataset as a CSV file
      write_csv(dataset, output_file_path)
      
      # # Optional: print a message indicating the file was saved
      # print(paste("Saved file:", output_file_path))
      
    }
    
  }
  
}

# Call the function to save the datasets
save_collated_rates(
  seasons = seasons,
  pathogens = pathogens,
  cohort = cohort
)
