library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here("post_check", "primary_analyses"))

cohort <- "older_adults"

#import collated rates
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs 
                     paste0(cohort, "rolling_rates_primary_collated.csv")))

# Updated plotting function to use 30-day rolling rates without smoothing
plot_rolling_rates <- function(df, outcome, characteristic) {
  df %>%
    filter(outcome == !!outcome, characteristic == !!characteristic) %>%
    ggplot(aes(x = interval, y = rate_midpoint10_derived, group = group,
               col = group)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date", y = "Rate per 1000 person-years\n(Midpoint 10 Derived)",
         col = "Group")
}

# Define the updated function to create and save plots
create_rolling_plots <- function(df, outcomes, characteristics) {
  plots <- expand.grid(outcome = outcomes, characteristic = characteristics) %>%
    pmap(function(outcome, characteristic) {
      plot <- plot_rolling_rates(df, outcome, characteristic) +
        ggtitle(paste("30-Day Rolling Rate of", str_to_title(gsub("_", " ",
                outcome)), "by", str_to_title(gsub("_", " ", characteristic))))
      print(plot)  # Print or save each plot
      fs::dir_create(here("output", "collated", "plots", paste0(characteristic)))
      ggsave(paste0(here("output", "collated", "plots", paste0(characteristic)),
                    "/", "rolling_rates_", year(study_start_date), "_",
                    year(study_end_date), "_", cohort, "_", codelist_type, "_",
                    investigation_type, "_", characteristic, ".png"), plot)
    })
  return(plots)
}

#run function to create and save plots
create_rolling_plots(rates_over_time, outcomes, characteristics)
plot_rates(df_input, "Flu mild", "Age Group")
