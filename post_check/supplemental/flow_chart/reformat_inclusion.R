library(tidyverse)
library(here)

##  older adults 
cohort <- "older_adults"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registered_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registered_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]
patients_df$cohort <- cohort
older_adults_df <- rlang::duplicate(patients_df) %>% 
  select(c(total, not_registered, registered, not_eligible, included,
           perc_included, cohort))

##  adults 
cohort <- "adults"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registered_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registered_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]
patients_df$cohort <- cohort
adults_df <- rlang::duplicate(patients_df) %>% 
  select(c(total, not_registered, registered, not_eligible, included,
           perc_included, cohort))

##  children and adolescents
cohort <- "children_and_adolescents"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registered_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registered_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]
patients_df$cohort <- cohort
children_and_adolescents_df <- rlang::duplicate(patients_df) %>% 
  select(c(total, not_registered, registered, not_eligible, included,
           perc_included, cohort))

##  infants
cohort <- "infants"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registered_sex", "registered_imd",
"registered_no_carehome", "registered_no_riskgroup", "registered_no_immune",
"included", "perc_registered", "perc_registered_sex", "perc_registered_imd",
"perc_registered_no_carehome", "perc_registered_no_riskgroup",
"perc_registered_no_immune",  "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]
patients_df$cohort <- cohort
infants_df <- rlang::duplicate(patients_df) %>% 
  select(c(total, not_registered, registered, not_eligible, included,
           perc_included, cohort))

##  infants subgroup
cohort <- "infants_subgroup"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "mother_registered",
"registered_mother_registered", "registered_mother_registered_sex",
"registered_mother_registered_imd", "registered_mother_registered_no_carehome",
"registered_mother_registered_no_riskgroup", 
"registered_mother_registered_no_immune", "included", "perc_registered",
"perc_mother_registered", "perc_registered_mother_registered", 
"perc_registered_mother_registered_sex", "perc_registered_mother_registered_imd",
"perc_registered_mother_registered_no_carehome",
"perc_registered_mother_registered_no_riskgroup", 
"perc_registered_mother_registered_no_immune", "perc_included",  "subset")
patients_df <- patients_df[, not_registered := total - registered_mother_registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered_mother_registered - included, by = .(subset)]
patients_df$cohort <- cohort
infants_subgroup_df <- rlang::duplicate(patients_df) %>% 
  select(c(total, not_registered, registered, not_eligible, included,
           perc_included, cohort))

#combine inclusion tables
inclusion_df <- bind_rows(
  older_adults_df,
  adults_df,
  children_and_adolescents_df,
  infants_df,
  infants_subgroup_df
) %>% 
  mutate(
    subset = rep(c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
                   "2021-22", "2022-23", "2023-24"), 5)
  ) %>%
  pivot_longer(
    cols = c(total, not_registered, registered,
             not_eligible, included, perc_included),
    names_to  = "measure",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols   = c(cohort, measure),
    names_from  = subset,
    values_from = value
  )

#save it 
write_csv(
  inclusion_df,
  here::here("post_check", "plots", "supplemental", "flow_charts",
             "reformated_inclusion_collated.csv")
)
