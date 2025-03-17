library(gtsummary)
library(tidyr)

#import functions
source(here::here("post_check", "functions", "episodes.R"))

cohort <- "older_adults"

#import collated reinfection outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                                "descriptive", paste0(cohort,
                                "_reinfections_collated.csv")))

reinfections(df_input, "rsv")
reinfections(df_input, "flu")
reinfections(df_input, "covid")

#import collated multiple episode outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                                "descriptive", paste0(cohort,
                                "_multiple_episodes_collated.csv")))

multiple_episodes(df_input, "mild")
multiple_episodes(df_input, "severe")
