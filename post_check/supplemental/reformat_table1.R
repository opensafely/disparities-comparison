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
  ) %>%
  mutate(
    characteristic = factor(characteristic, levels = c(
      "Total", "65-74y", "75-89y", "90y+", "Female", "Male", "White",
      "Mixed", "Asian or Asian British", "Black or Black British", 
      "Other Ethnic Groups", "Unknown", "5 (least deprived)", "4",
      "3", "2", "1 (most deprived)", "Multiple of the Same Generation",
      "Living Alone", "One Other Generation", "Two Other Generations",
      "Three Other Generations", "Rural Town and Fringe",
      "Rural Village and Dispersed", "Urban City and Town", "Urban Major Conurbation",
      "Urban Minor Conurbation", "Prior Flu Vaccine", "0-6m", "6-12m", "12m+"
    ))
  ) %>%
  arrange(characteristic)

#export to csv file
write_csv(
  df_input_reformat,
  here::here("post_check", "plots", "supplemental", "table1",
             paste0(cohort, "_reformated_table1_collated.csv"))
)

###adults

cohort <- "adults"
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
  ) %>%
  mutate(
    characteristic = factor(characteristic, levels = c(
      "Total", "18-39y", "40-64y", "Female", "Male", "White",
      "Mixed", "Asian or Asian British", "Black or Black British", 
      "Other Ethnic Groups", "Unknown", "5 (least deprived)", "4",
      "3", "2", "1 (most deprived)", "Multiple of the Same Generation",
      "Living Alone", "One Other Generation", "Two Other Generations",
      "Three Other Generations", "Rural Town and Fringe",
      "Rural Village and Dispersed", "Urban City and Town", "Urban Major Conurbation",
      "Urban Minor Conurbation", "Prior Flu Vaccine", "0-6m", "6-12m", "12m+"
    ))
  ) %>%
  arrange(characteristic)

#export to csv file
write_csv(
  df_input_reformat,
  here::here("post_check", "plots", "supplemental", "table1",
             paste0(cohort, "_reformated_table1_collated.csv"))
)

###children and adolescents

cohort <- "children_and_adolescents"
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
  ) %>%
  mutate(
    characteristic = factor(characteristic, levels = c(
      "Total", "2-5y", "6-9y", "10-13y", "14-17y", "Female", "Male", "White",
      "Mixed", "Asian or Asian British", "Black or Black British", 
      "Other Ethnic Groups", "Unknown", "5 (least deprived)", "4",
      "3", "2", "1 (most deprived)", "Multiple of the Same Generation",
      "Living Alone", "One Other Generation", "Two Other Generations",
      "Three Other Generations", "Rural Town and Fringe",
      "Rural Village and Dispersed", "Urban City and Town", "Urban Major Conurbation",
      "Urban Minor Conurbation", "Prior Flu Vaccine", "0-6m", "6-12m", "12m+"
    ))
  ) %>%
  arrange(characteristic)

#export to csv file
write_csv(
  df_input_reformat,
  here::here("post_check", "plots", "supplemental", "table1",
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
  )%>%
  mutate(
    characteristic = factor(characteristic, levels = c(
      "Total","0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
      "Mixed", "Asian or Asian British", "Black or Black British", 
      "Other Ethnic Groups", "Unknown", "5 (least deprived)", "4",
      "3", "2", "1 (most deprived)", "Multiple of the Same Generation",
      "Living Alone", "One Other Generation", "Two Other Generations",
      "Three Other Generations", "Rural Town and Fringe",
      "Rural Village and Dispersed", "Urban City and Town", "Urban Major Conurbation",
      "Urban Minor Conurbation", "Prior Flu Vaccine", "0-6m", "6-12m", "12m+"
    ))
  ) %>%
  arrange(characteristic)

#export to csv file
write_csv(
  df_input_reformat,
  here::here("post_check", "plots", "supplemental", "table1",
             paste0(cohort, "_reformated_table1_collated.csv"))
)

###infants subgroup

cohort <- "infants_subgroup"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))
names(df_input) <- c("characteristic", "n", "perc", "subset")

df_input_reformat <- df_input %>%
  mutate(
    characteristic = case_when(
      characteristic == "1 (least deprived)" ~ "5 (least deprived)",
      characteristic == "2" ~ "4",
      characteristic == "4" ~ "2",
      characteristic == "5 (most deprived)" ~ "1 (most deprived)",
      n < 100000 & characteristic == "Unknown" ~ "Unknown Ethnicity",
      n > 100000 & characteristic == "Unknown" ~ "Unknown Smoking Status",
      TRUE ~ characteristic),
    n = case_when(
      !str_detect(perc, "%") ~ perc,
      TRUE ~ as.character(n)
    ),
    perc = case_when(
      !str_detect(perc, "%") ~ NA,
      TRUE ~ perc
    )
  ) %>% 
  filter(!is.na(n)) %>%
  mutate(
    n_perc = if_else(!is.na(perc), paste0(n, " (", perc, ")"), n)
  ) %>%
  select(-c(n, perc)) %>%
  pivot_wider(
    names_from = subset,
    values_from = n_perc
  ) %>%
  mutate(
    characteristic = factor(characteristic, levels = c(
      "Total", "0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
      "Mixed", "Asian or Asian British", "Black or Black British", 
      "Other Ethnic Groups", "Unknown Ethnicity", "5 (least deprived)", "4",
      "3", "2", "1 (most deprived)", "Rural Town and Fringe",
      "Rural Village and Dispersed", "Urban City and Town", "Urban Major Conurbation",
      "Urban Minor Conurbation", "Maternal Age", "Never", "Former", "Current",
      "Unknown Smoking Status", "Maternal Drinking", "Maternal Drug Usage", 
      "Maternal Pertussis Vaccination", "Maternal Flu Vaccination"
    ))
  ) %>%
  arrange(characteristic)

#export to csv file
write_csv(
  df_input_reformat,
  here::here("post_check", "plots", "supplemental", "table1",
             paste0(cohort, "_reformated_table1_collated.csv"))
)

##secondary table 

###older adults

cohort <- "older_adults"
investigation_type <- "secondary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_secondary_table1_collated.csv")))
names(df_input) <- c("characteristic", "n", "perc", "subset")

df_input_reformat <- df_input %>%
  filter(!is.na(n), !str_detect(characteristic, "Vaccine")) %>%
  mutate(
    n_perc = paste0(n, " (", perc, ")")
  ) %>%
  select(-c(n, perc)) %>%
  pivot_wider(
    names_from = subset,
    values_from = n_perc
  )%>%
  mutate(
    characteristic = factor(characteristic, levels = c(
      "Total", "Asthma", "COPD", "Cystic Fibrosis",
      "Other Chronic Respiratory Diseases", "Diabetes",
      "Heart Disease", "Addisons", "Severe Obesity", "Chronic Heart Diseases",
      "Chronic Liver Disease", "Chronic Kidney Disease",
      "Chronic Neurological Disease", "Cancer Within 3 Years",
      "Immunosuppressed", "Sickle Cell Disease", "Never", "Current", "Former",
      "Unknown", "Hazardous Drinking", "Drug Usage"
    ))
  ) %>%
  arrange(characteristic)

#export to csv file
write_csv(
  df_input_reformat,
  here::here("post_check", "plots", "supplemental", "table1",
             paste0(cohort, "_reformated_secondary_table1_collated.csv"))
)
