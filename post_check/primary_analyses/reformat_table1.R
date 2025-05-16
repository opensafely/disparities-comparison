library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(stringr)

###older adults

cohort <- "older_adults"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))
names(df_input) <- c("characteristic", "n", "perc", "subset")

df_input_reformat <- df_input %>%
  filter(!is.na(n)) %>%
  mutate(
    characteristic = case_when(
      characteristic == "1 (least deprived)" ~ "5 (least deprived)",
      characteristic == "2" ~ "4",
      characteristic == "4" ~ "2",
      characteristic == "5 (most deprived)" ~ "1 (most deprived)",
      TRUE ~ characteristic),
    n_perc = paste0(n, " (", perc, ")")
  ) %>%
  select(-c(n, perc)) %>%
  pivot_wider(
    names_from = subset,
    values_from = n_perc
  )

#export to csv file
write_csv(
  df_input_reformat,
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reformated_table1_collated.csv"))
)

###infants

cohort <- "infants"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))
names(df_input) <- c("characteristic", "n", "perc", "subset")

df_input_reformat <- df_input %>%
  filter(!is.na(n)) %>%
  mutate(
    characteristic = case_when(
      characteristic == "1 (least deprived)" ~ "5 (least deprived)",
      characteristic == "2" ~ "4",
      characteristic == "4" ~ "2",
      characteristic == "5 (most deprived)" ~ "1 (most deprived)",
      TRUE ~ characteristic),
    n_perc = paste0(n, " (", perc, ")")
  ) %>%
  select(-c(n, perc)) %>%
  pivot_wider(
    names_from = subset,
    values_from = n_perc
  )

#export to csv file
write_csv(
  df_input_reformat,
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reformated_table1_collated.csv"))
)
