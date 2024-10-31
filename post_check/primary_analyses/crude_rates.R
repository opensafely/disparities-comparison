library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here("post_check", "primary_analyses"))

cohort <- "older_adults"
phenotype <- "specific"

#import collated rates
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs 
                     paste0(cohort, "_rates_", phenotype,
                     "_primary_collated.csv")))

#create function which plots rates by characteristic, for an inputted outcome
plot_rates <- function(df, outcome, characteristic) {
  df %>%
    filter(Outcome == !!outcome, Characteristic == !!characteristic) %>%
    ggplot(aes(x = subset, y = Rate, group = Characteristic)) +
    geom_line(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0("Rates of ", tolower(outcome), " by characteristic"),
         x = "Season",
         y = "Rate per 1000 person-years")
}

plot_rates(df_input, "Flu mild", "Age Group")
