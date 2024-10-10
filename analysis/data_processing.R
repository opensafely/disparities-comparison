library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(lubridate)
library(magrittr)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2022-09-01")
  study_end_date <- as.Date("2023-08-31")
  cohort <- "adults"
  codelist_type <- "specific"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

df_input <- read_feather(
  here::here("output", "data", paste0("input_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type,".arrow")))

if (study_start_date == as.Date("2020-09-01")) {
  
  df_household <- read_feather(
    here::here("output", "data", paste0("input_household_processed_", 
               year(study_start_date), "_", year(study_end_date), ".arrow")))
  
  household_comp_vars <- tibble("patient_id" = df_household$patient_id,
                                "num_generations"= df_household$num_generations, 
                                "composition_category" = df_household$composition_category)
  
  df_input <- merge(df_input, household_comp_vars, by = "patient_id")
  
}

#create time dependency
if(cohort == "infants" | cohort == "infants_subgroup") {
  print(nrow(df_input))
  print(nrow(df_household))
  print(class(study_start_date))
  print(class(study_end_date))
  df_input <- df_input %>%
    mutate(
      date = map2(study_start_date, study_end_date, ~seq(.x, .y, by = 30.44))
    ) %>%
    unnest(date) %>%
    mutate(
      age = age + as.numeric((date - study_start_date)/30.44)
    ) 
}
  
#calculate age bands
if(cohort == "older_adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age >= 65 & age <= 74 ~ "65-74y",
      age >= 75 & age <= 89 ~ "75-89y",
      age >= 90 ~ "90y+",
      TRUE ~ NA_character_)
    )
} else if(cohort == "adults") {
df_input <- df_input %>%
  mutate(age_band = case_when(
    age >= 18 & age <= 39 ~ "18-29y",
    age >= 40 & age <= 64 ~ "40-64y",
    TRUE ~ NA_character_)
  )
} else if(cohort == "children_and_adolescents") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age >= 2 & age <= 5 ~ "2-5y",
      age >= 6 & age <= 9 ~ "6-9y",
      age >= 10 & age <= 13 ~ "10-13y",
      age >= 14 & age <= 17 ~ "14-17y",
      TRUE ~ NA_character_)
    )
} else {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age >= 0 & age <= 2 ~ "0-2m",
      age >= 3 & age <= 5 ~ "3-5m",
      age >= 6 & age <= 11 ~ "6-11m",
      age >= 12 & age <= 23 ~ "12-23m",
      TRUE ~ NA_character_)
    )
}

df_input$age_band <- factor(df_input$age_band)

#data manipulation
df_input <- df_input %>%
  mutate(
    #assign ethnicity group
    latest_ethnicity_group = factor(case_when(
      latest_ethnicity_code == "1" ~ "White",
      latest_ethnicity_code == "2" ~ "Mixed",
      latest_ethnicity_code == "3" ~ "Asian or Asian British",
      latest_ethnicity_code == "4" ~ "Black or Black British",
      latest_ethnicity_code == "5" ~ "Other Ethnic Groups",
      TRUE ~ "Unknown"), ordered = TRUE),
    #calculate IMD quintile
    imd_quintile = factor(case_when(
      imd_rounded >= 0 & imd_rounded < as.integer(32800 * 1 / 5) ~ "1 (most deprived)",
      imd_rounded < as.integer(32800 * 2 / 5) ~ "2",
      imd_rounded < as.integer(32800 * 3 / 5) ~ "3",
      imd_rounded < as.integer(32800 * 4 / 5) ~ "4",
      imd_rounded < as.integer(32800 * 5 / 5) ~ "5 (least deprived)",
      TRUE ~ NA_character_)),
    #format sex
    sex = factor(case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "intersex" ~ "Intersex",
      sex == "unknown" ~ "Unknown",
      TRUE ~ "Unknown"))
  )

#identify columns with logical values, excluding specified columns
logical_cols <- which(sapply(df_input, is.logical) & !grepl("primary|secondary|mortality|registered", names(df_input)))

#apply mutation to convert logical columns to factors
df_input <- df_input %>%
  mutate(across(
    .cols = all_of(logical_cols), 
    .fns = ~factor(case_when(
      . == FALSE ~ "No",
      . == TRUE ~ "Yes",
      TRUE ~ NA_character_
    ))
  ))

#more data manipulation
df_input <- df_input %>%
  mutate(
    #recode imd quintile 
    imd_quintile = recode(df_input$imd_quintile, "1 (most deprived)" = "5 (most deprived)",
                          "2" = "4", "3" = "3", "4" = "2",
                          "5 (least deprived)" = "1 (least deprived)"),
    #recode rurality to 5 levels
    rurality_code = recode(rural_urban_classification, "1" = "1", "2" = "2", 
                           "3" = "3", "4" = "3", "5" = "4", "6" = "4", 
                           "7" = "5", "8" = "5", .missing = "Unknown"),
    #define household size categories
    household_size_cat = factor(case_when(
      household_size >= 1 & household_size <= 2 ~ "1",
      household_size >= 3 & household_size <= 5 ~ "2",
      household_size >= 6 ~ "3",
      TRUE ~ "Unknown")),
    #assign rurality classification
    rurality_classification = factor(case_when(
      rurality_code == "1" ~ "Urban Major Conurbation",
      rurality_code == "2" ~ "Urban Minor Conurbation",
      rurality_code == "3" ~ "Urban City and Town",
      rurality_code == "4" ~ "Rural Town and Fringe",
      rurality_code == "5" ~ "Rural Village and Dispersed",
      TRUE ~ "Unknown"), ordered = TRUE)
  )

#flu vaccination
if (cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
    #assign flu vaccination status
    flu_vaccination_immunity_date = flu_vaccination_date + days(10),
    #current flu vaccination status including a lag time
    flu_vaccination = factor(if_else(
      is.na(flu_vaccination_immunity_date), "No", "Yes"
    ))
  )
}

#define two vaccinations categories for each outcome type, set vaccination to null
#if immunity date occurs after outcome date 
if (cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      #define flu_vaccination_mild
      flu_vaccination_mild = factor(case_when(
        flu_vaccination_immunity_date > flu_primary_date ~ "No",
        is.na(flu_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes"
      )),
      #define flu_vaccination severe 
      flu_vaccination_severe = factor(case_when(
        flu_vaccination_immunity_date > flu_secondary_date ~ "No",
        is.na(flu_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes"
      ))
    )
}

#covid vaccination 
if (study_start_date >= covid_prior_vacc_min & cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      time_since_last_covid_vaccination = factor(case_when(
      time_length(difftime(study_start_date, last_covid_vaccination_date, 
                             units = "days"), "months") >= 0 &
        time_length(difftime(study_start_date, last_covid_vaccination_date,
                             units = "days"), "months") < 6 ~ "0-6m",
      time_length(difftime(study_start_date, last_covid_vaccination_date,
                           units = "days"), "months") >= 6 &
        time_length(difftime(study_start_date, last_covid_vaccination_date, 
                             units = "days"), "months") < 12 ~ "6-12m",
      time_length(difftime(study_start_date, last_covid_vaccination_date,
                           units = "days"), "months") >= 12 ~ "12m+",
        TRUE ~ "Unknown"))
  )
}
if (study_start_date >= covid_current_vacc_min & cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>% 
    mutate(
      covid_vaccination_immunity_date = covid_vaccination_date + days(10),
      #current covid vaccination status including a lag time
      covid_vaccination = factor(if_else(
        is.na(covid_vaccination_immunity_date), "No", "Yes"
      ))
    )
}

#define two vaccinations categories for each outcome type, set vaccination to null
#if immunity date occurs after outcome date 
if (study_start_date >= covid_current_vacc_min & cohort != "infants" & cohort != "infants_subgroup") {
  df_input <- df_input %>%
    mutate(
      #define covid_vaccination_mild
      covid_vaccination_mild = fct_rev(factor(case_when(
        covid_vaccination_immunity_date > covid_primary_date ~ "No",
        is.na(covid_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes")
      )),
      #define covid_vaccination severe 
      covid_vaccination_severe = factor(case_when(
        covid_vaccination_immunity_date > covid_secondary_date ~ "No",
        is.na(covid_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes"
      ))
    )
}

# #re-level factors so they have reference categories for the regression models
# df_input <- df_input %>% 
#   mutate(
#     latest_ethnicity_group = fct_relevel(latest_ethnicity_group, 
#                              c("White", "Mixed", "Asian or Asian British", 
#                                "Black or Black British", "Other Ethnic Groups"))
#   ) %>% arrange(latest_ethnicity_group)
# df_input <- df_input %>% 
#   mutate(
#     rurality_classification = fct_relevel(rurality_classification, 
#                               c("Urban Major Conurbation", "Urban Minor Conurbation", 
#                               "Urban City and Town", "Rural Town and Fringe", 
#                               "Rural Village and Dispersed", "Unknown"))
#   ) %>% arrange(rurality_classification)
df_input <- df_input %>%
  mutate(
    composition_category = fct_relevel(composition_category,
                                       c("Multiple of the Same Generation", "Living Alone",
                                         "One Other Generation", "Two Other Generations",
                                         "Three Other Generations"))
  ) %>% arrange(composition_category)

#set covid date to missing if existing date occurs before March 1st 2020
if (study_start_date >= covid_season_min) {
  df_input <- df_input %>%
    mutate(
      covid_primary_date = if_else(covid_primary_date < as.Date("2020-03-01"), NA_Date_, covid_primary_date),
      covid_primary_second_date = if_else(covid_primary_date < as.Date("2020-03-01"), NA_Date_, covid_primary_second_date),
      covid_secondary_date = if_else(covid_secondary_date < as.Date("2020-03-01"), NA_Date_, covid_secondary_date),
      covid_secondary_second_date = if_else(covid_secondary_date < as.Date("2020-03-01"), NA_Date_, covid_secondary_second_date),
      covid_mortality_date = if_else(covid_mortality_date < as.Date("2020-03-01"), NA_Date_, covid_mortality_date)
    )
}

#infer outcomes from event dates 
df_input <- df_input %>%
  mutate(
    #infer presence of mild rsv
    rsv_primary = if_else(
      !is.na(rsv_primary_date), TRUE, FALSE),
    #infer presence of second episode of mild rsv
    rsv_primary_second = if_else(
      !is.na(rsv_primary_second_date), TRUE, FALSE),
    #infer presence of severe rsv
    rsv_secondary = if_else(
      !is.na(rsv_secondary_date), TRUE, FALSE),
    #infer presence of second episode of severe rsv
    rsv_secondary_second = if_else(
      !is.na(rsv_secondary_second_date), TRUE, FALSE),
    #infer presence of rsv mortality
    rsv_mortality = if_else(
      !is.na(rsv_mortality_date), TRUE, FALSE),
    #infer presence of mild flu
    flu_primary = if_else(
      !is.na(flu_primary_date), TRUE, FALSE),
    #infer presence of second episode of mild flu
    flu_primary_second = if_else(
      !is.na(flu_primary_second_date), TRUE, FALSE),
    #infer presence of severe flu
    flu_secondary = if_else(
      !is.na(flu_secondary_date), TRUE, FALSE),
    #infer presence of second episode of severe flu
    flu_secondary_second = if_else(
      !is.na(flu_secondary_second_date), TRUE, FALSE),
    #infer presence of flu mortality
    flu_mortality = if_else(
      !is.na(flu_mortality_date), TRUE, FALSE)
  )

if (study_start_date >= covid_season_min) {
  df_input <- df_input %>%
    mutate(
      #infer presence of mild covid
      covid_primary = if_else(
        !is.na(covid_primary_date), TRUE, FALSE),
      #infer presence of second episode of mild covid
      covid_primary_second = if_else(
        !is.na(covid_primary_second_date), TRUE, FALSE),
      #infer presence of severe covid
      covid_secondary = if_else(
        !is.na(covid_secondary_date), TRUE, FALSE),
      #infer presence of second episode of severe covid
      covid_secondary_second = if_else(
        !is.na(covid_secondary_second_date), TRUE, FALSE),
      #infer presence of covid mortality
      covid_mortality = if_else(
        !is.na(covid_mortality_date), TRUE, FALSE)
    )
}

if (codelist_type == "sensitive") {
  df_input <- df_input %>%
    mutate(
      #infer presence of mild overall respiratory
      overall_resp_primary = if_else(
        !is.na(overall_resp_primary_date), TRUE, FALSE),
      #infer presence of second episode of mild overall respiratory
      overall_resp_primary_second = if_else(
        !is.na(overall_resp_primary_second_date), TRUE, FALSE),
      #infer presence of severe overall respiratory
      overall_resp_secondary = if_else(
        !is.na(overall_resp_secondary_date), TRUE, FALSE),
      #infer presence of second episode of severe overall respiratory
      overall_resp_secondary_second = if_else(
        !is.na(overall_resp_secondary_second_date), TRUE, FALSE),
      #infer presence of overall respiratory mortality
      overall_resp_mortality = if_else(
        !is.na(overall_resp_mortality_date), TRUE, FALSE)
    )
}

df_input <- df_input %>%
  mutate(
    #infer presence of all cause mortality
    all_cause_mortality_date = death_date,
    all_cause_mortality = if_else(
      !is.na(all_cause_mortality_date), TRUE, FALSE)
  )

#define event time 
if (study_start_date < covid_season_min) {
  df_input <- df_input %>%
    mutate(
      #infer mild case date for rsv 
      rsv_primary_inf_date = case_when(
        is.na(rsv_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(rsv_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(rsv_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(rsv_primary_date) & !is.na(rsv_secondary_date) ~ rsv_secondary_date,
        TRUE ~ rsv_primary_date),
      #assign censoring indicator
      rsv_primary_censor = if_else(is.na(rsv_primary_date), 1, 0),
      #infer mild rsv outcome 
      rsv_primary_inf = if_else(rsv_primary_censor == 0, 1, 0),
      # #infer mild case date for second episode of rsv
      # rsv_primary_second_inf_date = case_when(
      #   is.na(rsv_primary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(rsv_primary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(rsv_primary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   is.na(rsv_primary_second_date) & !is.na(rsv_secondary_second_date) ~ rsv_secondary_second_date,
      #   TRUE ~ rsv_primary_second_date),
      # #assign censoring indicator
      # rsv_primary_second_censor = if_else(is.na(rsv_primary_second_date), 1, 0),
      # #infer second mild rsv outcome
      # rsv_primary_second_inf = if_else(rsv_primary_second_censor == 0, 1, 0),
      #infer severe case date for rsv
      rsv_secondary_inf_date = case_when(
        is.na(rsv_secondary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(rsv_secondary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(rsv_secondary_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ rsv_secondary_date),
      #assign censoring indicator
      rsv_secondary_censor = if_else(is.na(rsv_secondary_date), 1, 0),
      #infer severe rsv outcome
      rsv_secondary_inf = if_else(rsv_secondary_censor == 0, 1, 0),
      # #infer severe case date for second episode of rsv
      # rsv_secondary_second_inf_date = case_when(
      #   is.na(rsv_secondary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(rsv_secondary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(rsv_secondary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   TRUE ~ rsv_secondary_second_date),
      # #assign censoring indicator
      # rsv_secondary_second_censor = if_else(is.na(rsv_secondary_second_date), 1, 0),
      # #infer second severe rsv outcome
      # rsv_secondary_second_inf = if_else(rsv_secondary_second_censor == 0, 1, 0),
      #infer rsv mortality outcome 
      rsv_mortality_inf_date = case_when(
        is.na(rsv_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(rsv_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(rsv_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ rsv_mortality_date),
      #assign censoring indicator
      rsv_mortality_censor = if_else(is.na(rsv_mortality_date), 1, 0),
      #infer rsv mortality outcome
      rsv_mortality_inf = if_else(rsv_mortality_censor == 0, 1, 0),
      #infer mild case date for flu
      flu_primary_inf_date = case_when(
        is.na(flu_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(flu_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(flu_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(flu_primary_date) & !is.na(flu_secondary_date) ~ flu_secondary_date,
        TRUE ~ flu_primary_date),
      #assign censoring indicator
      flu_primary_censor = if_else(is.na(flu_primary_date), 1, 0),
      #infer mild flu outcome
      flu_primary_inf = if_else(flu_primary_censor == 0, 1, 0),
      # #infer mild case date for second episode of flu
      # flu_primary_second_inf_date = case_when(
      #   is.na(flu_primary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(flu_primary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(flu_primary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   is.na(flu_primary_second_date) & !is.na(flu_secondary_second_date) ~ flu_secondary_second_date,
      #   TRUE ~ flu_primary_second_date),
      # #assign censoring indicator
      # flu_primary_second_censor = if_else(is.na(flu_primary_second_date), 1, 0),
      # #infer second mild flu outcome
      # flu_primary_second_inf = if_else(flu_primary_second_censor == 0, 1, 0),
      #infer severe case date for flu
      flu_secondary_inf_date = case_when(
        is.na(flu_secondary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(flu_secondary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(flu_secondary_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ flu_secondary_date),
      #assign censoring indicator
      flu_secondary_censor = if_else(is.na(flu_secondary_date), 1, 0),
      #infer severe flu outcome
      flu_secondary_inf = if_else(flu_secondary_censor == 0, 1, 0),
      # #infer severe case date for second episode of flu
      # flu_secondary_second_inf_date = case_when(
      #   is.na(flu_secondary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(flu_secondary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(flu_secondary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   TRUE ~ flu_secondary_second_date),
      # #assign censoring indicator
      # flu_secondary_second_censor = if_else(is.na(flu_secondary_second_date), 1, 0),
      # #infer second severe flu outcome
      # flu_secondary_second_inf = if_else(flu_secondary_second_censor == 0, 1, 0),
      #infer flu mortality outcome
      flu_mortality_inf_date = case_when(
        is.na(flu_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(flu_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(flu_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ flu_mortality_date),
      #assign censoring indicator
      flu_mortality_censor = if_else(is.na(flu_mortality_date), 1, 0),
      #infer flu mortality outcome
      flu_mortality_inf = if_else(flu_mortality_censor == 0, 1, 0)
      )
  #for sensitive analyses define overall respiratory outcomes
  if (codelist_type == "sensitive") {
    df_input <- df_input %>%
      mutate(
        #infer mild case date for overall respiratory
        overall_resp_primary_inf_date = case_when(
          is.na(overall_resp_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
          is.na(overall_resp_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
          is.na(overall_resp_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
          is.na(overall_resp_primary_date) & !is.na(overall_resp_secondary_date) ~ overall_resp_secondary_date,
          TRUE ~ overall_resp_primary_date),
        #assign censoring indicator
        overall_resp_primary_censor = if_else(is.na(overall_resp_primary_date), 1, 0),
        #infer mild overall respiratory outcome
        overall_resp_primary_inf = if_else(overall_resp_primary_censor == 0, 1, 0),
        # #infer mild case date for second episode of overall respiratory
        # overall_resp_primary_second_inf_date = case_when(
        #   is.na(overall_resp_primary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        #   is.na(overall_resp_primary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        #   is.na(overall_resp_primary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
        #   is.na(overall_resp_primary_second_date) & !is.na(overall_resp_secondary_second_date) ~ overall_resp_secondary_second_date,
        #   TRUE ~ overall_resp_primary_second_date),
        # #assign censoring indicator
        # overall_resp_primary_second_censor = if_else(is.na(overall_resp_primary_second_date), 1, 0),
        # #infer second mild overall respiratory outcome
        # overall_resp_primary_second_inf = if_else(overall_resp_primary_second_censor == 0, 1, 0),
        #infer severe case date for overall respiratory
        overall_resp_secondary_inf_date = case_when(
          is.na(overall_resp_secondary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
          is.na(overall_resp_secondary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
          is.na(overall_resp_secondary_date) & !is.na(deregistration_date) ~ deregistration_date,
          TRUE ~ overall_resp_secondary_date),
        #assign censoring indicator
        overall_resp_secondary_censor = if_else(is.na(overall_resp_secondary_date), 1, 0),
        #infer severe overall respiratory outcome
        overall_resp_secondary_inf = if_else(overall_resp_secondary_censor == 0, 1, 0),
        # #infer severe case date for second episode of overall respiratory
        # overall_resp_secondary_second_inf_date = case_when(
        #   is.na(overall_resp_secondary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        #   is.na(overall_resp_secondary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        #   is.na(overall_resp_secondary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
        #   TRUE ~ overall_resp_secondary_second_date),
        # #assign censoring indicator
        # overall_resp_secondary_second_censor = if_else(is.na(overall_resp_secondary_second_date), 1, 0),
        # #infer second severe overall respiratory outcome
        # overall_resp_secondary_second_inf = if_else(overall_resp_secondary_second_censor == 0, 1, 0),
        #infer overall respiratory mortality outcome
        overall_resp_mortality_inf_date = case_when(
          is.na(overall_resp_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
          is.na(overall_resp_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
          is.na(overall_resp_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
          TRUE ~ overall_resp_mortality_date),
        #assign censoring indicator
        overall_resp_mortality_censor = if_else(is.na(overall_resp_mortality_date), 1, 0),
        #infer overall respiratory mortality outcome
        overall_resp_mortality_inf = if_else(overall_resp_mortality_censor == 0, 1, 0)
      )
  }
  df_input <- df_input %>%
    mutate(
      #infer all cause mortality outcome
      all_cause_mortality_inf_date = case_when(
        is.na(all_cause_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(all_cause_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(all_cause_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ all_cause_mortality_date),
      #assign censoring indicator
      all_cause_mortality_censor = if_else(is.na(all_cause_mortality_date), 1, 0),
      #infer all cause mortality outcome
      all_cause_mortality_inf = if_else(all_cause_mortality_censor == 0, 1, 0)
    )
} else {
  df_input <- df_input %>%
    mutate(
      #infer mild case date for rsv
      rsv_primary_inf_date = case_when(
        is.na(rsv_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(rsv_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(rsv_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(rsv_primary_date) & !is.na(rsv_secondary_date) ~ rsv_secondary_date,
        TRUE ~ rsv_primary_date),
      #assign censoring indicator
      rsv_primary_censor = if_else(is.na(rsv_primary_date), 1, 0),
      #infer mild rsv outcome 
      rsv_primary_inf = if_else(rsv_primary_censor == 0, 1, 0),
      # #infer mild case date for second episode of rsv
      # rsv_primary_second_inf_date = case_when(
      #   is.na(rsv_primary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(rsv_primary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(rsv_primary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   is.na(rsv_primary_second_date) & !is.na(rsv_secondary_second_date) ~ rsv_secondary_second_date,
      #   TRUE ~ rsv_primary_second_date),
      # #assign censoring indicator
      # rsv_primary_second_censor = if_else(is.na(rsv_primary_second_date), 1, 0),
      # #infer second mild rsv outcome
      # rsv_primary_second_inf = if_else(rsv_primary_second_censor == 0, 1, 0),
      #infer severe case date for rsv
      rsv_secondary_inf_date = case_when(
        is.na(rsv_secondary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(rsv_secondary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(rsv_secondary_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ rsv_secondary_date),
      #assign censoring indicator
      rsv_secondary_censor = if_else(is.na(rsv_secondary_date), 1, 0),
      #infer severe rsv outcome
      rsv_secondary_inf = if_else(rsv_secondary_censor == 0, 1, 0),
      # #infer severe case date for second episode of rsv
      # rsv_secondary_second_inf_date = case_when(
      #   is.na(rsv_secondary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(rsv_secondary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(rsv_secondary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   TRUE ~ rsv_secondary_second_date),
      # #assign censoring indicator
      # rsv_secondary_second_censor = if_else(is.na(rsv_secondary_second_date), 1, 0),
      # #infer second severe rsv outcome
      # rsv_secondary_second_inf = if_else(rsv_secondary_second_censor == 0, 1, 0),
      #infer rsv mortality outcome 
      rsv_mortality_inf_date = case_when(
        is.na(rsv_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(rsv_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(rsv_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ rsv_mortality_date),
      #assign censoring indicator
      rsv_mortality_censor =if_else(is.na(rsv_mortality_date), 1, 0),
      #infer rsv mortality outcome
      rsv_mortality_inf = if_else(rsv_mortality_censor == 0, 1, 0),
      #infer mild case date for flu
      flu_primary_inf_date = case_when(
        is.na(flu_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(flu_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(flu_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(flu_primary_date) & !is.na(flu_secondary_date) ~ flu_secondary_date,
        TRUE ~ flu_primary_date),
      #assign censoring indicator
      flu_primary_censor = if_else(is.na(flu_primary_date), 1, 0),
      #infer mild flu outcome
      flu_primary_inf = if_else(flu_primary_censor == 0, 1, 0),
      # #infer mild case date for second episode of flu
      # flu_primary_second_inf_date = case_when(
      #   is.na(flu_primary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(flu_primary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(flu_primary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   is.na(flu_primary_second_date) & !is.na(flu_secondary_second_date) ~ flu_secondary_second_date,
      #   TRUE ~ flu_primary_second_date),
      # #assign censoring indicator
      # flu_primary_second_censor = if_else(is.na(flu_primary_second_date), 1, 0),
      # #infer second mild flu outcome
      # flu_primary_second_inf = if_else(flu_primary_second_censor == 0, 1, 0),
      #infer severe case date for flu
      flu_secondary_inf_date = case_when(
        is.na(flu_secondary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(flu_secondary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(flu_secondary_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ flu_secondary_date),
      #assign censoring indicator
      flu_secondary_censor = if_else(is.na(flu_secondary_date), 1, 0),
      #infer severe flu outcome
      flu_secondary_inf = if_else(flu_secondary_censor == 0, 1, 0),
      # #infer severe case date for second episode of flu
      # flu_secondary_second_inf_date = case_when(
      #   is.na(flu_secondary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(flu_secondary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(flu_secondary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   TRUE ~ flu_secondary_second_date),
      # #assign censoring indicator
      # flu_secondary_second_censor = if_else(is.na(flu_secondary_second_date), 1, 0),
      # #infer second severe flu outcome
      # flu_secondary_second_inf = if_else(flu_secondary_second_censor == 0, 1, 0),
      #infer flu mortality outcome
      flu_mortality_inf_date = case_when(
        is.na(flu_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(flu_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(flu_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ flu_mortality_date),
      #assign censoring indicator
      flu_mortality_censor = if_else(is.na(flu_mortality_date), 1, 0),
      #infer flu mortality outcome
      flu_mortality_inf = if_else(flu_mortality_censor == 0, 1, 0),
      #infer mild case date for covid
      covid_primary_inf_date = case_when(
        is.na(covid_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(covid_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(covid_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
        is.na(covid_primary_date) & !is.na(covid_secondary_date) ~ covid_secondary_date,
        TRUE ~ covid_primary_date),
      #assign censoring indicator
      covid_primary_censor = if_else(is.na(covid_primary_date), 1, 0),
      #infer mild covid outcome
      covid_primary_inf = if_else(covid_primary_censor == 0, 1, 0),
      # #infer mild case date for second episode of covid
      # covid_primary_second_inf_date = case_when(
      #   is.na(covid_primary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(covid_primary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(covid_primary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   is.na(covid_primary_second_date) & !is.na(covid_secondary_second_date) ~ covid_secondary_second_date,
      #   TRUE ~ covid_primary_second_date),
      # #assign censoring indicator
      # covid_primary_second_censor = if_else(is.na(covid_primary_second_date), 1, 0),
      # #infer second mild covid outcome
      # covid_primary_second_inf = if_else(covid_primary_second_censor == 0, 1, 0),
      #infer severe case date for covid
      covid_secondary_inf_date = case_when(
        is.na(covid_secondary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(covid_secondary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(covid_secondary_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ covid_secondary_date),
      #assign censoring indicator
      covid_secondary_censor = if_else(is.na(covid_secondary_date), 1, 0),
      #infer severe covid outcome
      covid_secondary_inf = if_else(covid_secondary_censor == 0, 1, 0),
      # #infer severe case date for second episode of covid
      # covid_secondary_second_inf_date = case_when(
      #   is.na(covid_secondary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
      #   is.na(covid_secondary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
      #   is.na(covid_secondary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
      #   TRUE ~ covid_secondary_second_date),
      # #assign censoring indicator
      # covid_secondary_second_censor = if_else(is.na(covid_secondary_second_date), 1, 0),
      # #infer second severe covid outcome
      # covid_secondary_second_inf = if_else(covid_secondary_second_censor == 0, 1, 0),
      #infer covid mortality outcomes
      covid_mortality_inf_date = case_when(
        is.na(covid_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(covid_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(covid_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ covid_mortality_date),
      #assign censoring indicator
      covid_mortality_censor = if_else(is.na(covid_mortality_date), 1, 0),
      #infer covid mortality outcome
      covid_mortality_inf = if_else(covid_mortality_censor == 0, 1, 0)
      )
  #for sensitive analyses define overall respiratory outcomes
  if (codelist_type == "sensitive") {
    df_input <- df_input %>%
      mutate(
        #infer mild case date for overall respiratory
        overall_resp_primary_inf_date = case_when(
          is.na(overall_resp_primary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
          is.na(overall_resp_primary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
          is.na(overall_resp_primary_date) & !is.na(deregistration_date) ~ deregistration_date,
          is.na(overall_resp_primary_date) & !is.na(overall_resp_secondary_date) ~ overall_resp_secondary_date,
          TRUE ~ overall_resp_primary_date),
        #assign censoring indicator
        overall_resp_primary_censor = if_else(is.na(overall_resp_primary_date), 1, 0),
        #infer overall respiratory outcome
        overall_resp_primary_inf = if_else(overall_resp_primary_censor == 0, 1, 0),
        # #infer mild case date for second episode of overall respiratory
        # overall_resp_primary_second_inf_date = case_when(
        #   is.na(overall_resp_primary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        #   is.na(overall_resp_primary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        #   is.na(overall_resp_primary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
        #   is.na(overall_resp_primary_second_date) & !is.na(overall_resp_secondary_second_date) ~ overall_resp_secondary_second_date,
        #   TRUE ~ overall_resp_primary_second_date),
        # #assign censoring indicator
        # overall_resp_primary_second_censor = if_else(is.na(overall_resp_primary_second_date), 1, 0),
        # #infer second mild overall respiratory outcome
        # overall_resp_primary_second_inf = if_else(overall_resp_primary_second_censor == 0, 1, 0),
        #infer severe case date for overall respiratory
        overall_resp_secondary_inf_date = case_when(
          is.na(overall_resp_secondary_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
          is.na(overall_resp_secondary_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
          is.na(overall_resp_secondary_date) & !is.na(deregistration_date) ~ deregistration_date,
          TRUE ~ overall_resp_secondary_date),
        #assign censoring indicator
        overall_resp_secondary_censor = if_else(is.na(overall_resp_secondary_date), 1, 0),
        #infer severe overall respiratory outcome
        overall_resp_secondary_inf = if_else(overall_resp_secondary_censor == 0, 1, 0),
        # #infer severe case date for second episode of overall respiratory
        # overall_resp_secondary_second_inf_date = case_when(
        #   is.na(overall_resp_secondary_second_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        #   is.na(overall_resp_secondary_second_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        #   is.na(overall_resp_secondary_second_date) & !is.na(deregistration_date) ~ deregistration_date,
        #   TRUE ~ overall_resp_secondary_second_date),
        # #assign censoring indicator
        # overall_resp_secondary_second_censor = if_else(is.na(overall_resp_secondary_second_date), 1, 0),
        # #infer second severe overall respiratory outcome
        # overall_resp_secondary_second_inf = if_else(overall_resp_secondary_second_censor == 0, 1, 0),
        #infer overall respiratory mortality outcome
        overall_resp_mortality_inf_date = case_when(
          is.na(overall_resp_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
          is.na(overall_resp_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
          is.na(overall_resp_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
          TRUE ~ overall_resp_mortality_date),
        #assign censoring indicator
        overall_resp_mortality_censor = if_else(is.na(overall_resp_mortality_date), 1, 0),
        #infer overall respiratory mortality outcome
        overall_resp_mortality_inf = if_else(overall_resp_mortality_censor == 0, 1, 0)
      )
  }
  df_input <- df_input %>%
    mutate(
      #infer all cause mortality outcome
      all_cause_mortality_inf_date = case_when(
        is.na(all_cause_mortality_date) & is.na(deregistration_date) & is.na(death_date) ~ study_end_date,
        is.na(all_cause_mortality_date) & is.na(deregistration_date) & !is.na(death_date) ~ death_date,
        is.na(all_cause_mortality_date) & !is.na(deregistration_date) ~ deregistration_date,
        TRUE ~ all_cause_mortality_date),
      #assign censoring indicator
      all_cause_mortality_censor = if_else(is.na(all_cause_mortality_date), 1, 0),
      #infer all cause mortality outcome
      all_cause_mortality_inf = if_else(all_cause_mortality_censor == 0, 1, 0)
    )
}

#calculate time to event
if (study_start_date < covid_season_min) {
  df_input <- df_input %>%
    mutate(
      #time until mild rsv outcome
      time_rsv_primary = time_length(difftime(rsv_primary_inf_date, 
                         study_start_date - days(1), "weeks"), "years"),
      # #time until second mild rsv outcome 
      # time_rsv_primary_second = time_length(difftime(rsv_primary_second_inf_date, 
      #                           study_start_date - days(1), "weeks"), "years"),
      #time until severe rsv outcome
      time_rsv_secondary = time_length(difftime(rsv_secondary_inf_date, 
                           study_start_date - days(1), "weeks"), "years"),
      # #time until second severe rsv outcome
      # time_rsv_secondary_second = time_length(difftime(rsv_secondary_second_inf_date, 
      #                             study_start_date - days(1), "weeks"), "years"),
      #time until rsv mortality
      time_rsv_mortality = time_length(difftime(rsv_mortality_inf_date, 
                          study_start_date - days(1), "weeks"), "years"),
      #time until mild flu outcome
      time_flu_primary = time_length(difftime(flu_primary_inf_date, 
                         study_start_date - days(1), "weeks"), "years"),
      # #time until second mild flu outcome
      # time_flu_primary_second = time_length(difftime(flu_primary_second_inf_date, 
      #                           study_start_date - days(1), "weeks"), "years"),
      #time until severe flu outcome
      time_flu_secondary = time_length(difftime(flu_secondary_inf_date, 
                           study_start_date - days(1), "weeks"), "years"),
      # #time until second severe flu outcome
      # time_flu_secondary_second = time_length(difftime(flu_secondary_second_inf_date, 
      #                             study_start_date - days(1), "weeks"), "years"),
      #time until flu mortality
      time_flu_mortality = time_length(difftime(flu_mortality_inf_date, 
                           study_start_date - days(1), "weeks"), "years")
    )
  if (codelist_type == "sensitive") {
    df_input <- df_input %>%
      mutate(
      #time until mild overall respiratory outcome
      time_overall_resp_primary = time_length(difftime(overall_resp_primary_inf_date, 
                                  study_start_date - days(1), "weeks"), "years"),
      # #time until second mild overall respiratory outcome
      # time_overall_resp_primary_second = time_length(difftime(overall_resp_primary_second_inf_date, 
      #                                    study_start_date - days(1), "weeks"), "years"),
      #time until severe overall respiratory outcome
      time_overall_resp_secondary = time_length(difftime(overall_resp_secondary_inf_date, 
                                    study_start_date - days(1), "weeks"), "years"),
      # #time until second severe overall respiratory outcome
      # time_overall_resp_secondary_second = time_length(difftime(overall_resp_secondary_second_inf_date, 
      #                                      study_start_date - days(1), "weeks"), "years"),
      #time until overall respiratory mortality
      time_overall_resp_mortality = time_length(difftime(overall_resp_mortality_inf_date, 
                                    study_start_date - days(1), "weeks"), "years")
      )
  }
  df_input <- df_input %>%
    mutate(
      #time until all cause mortality
      time_all_cause_mortality = time_length(difftime(all_cause_mortality_inf_date, 
                                 study_start_date - days(1), "weeks"), "years")
    )
} else {
  df_input <- df_input %>%
    mutate(
      #time until mild rsv outcome
      time_rsv_primary = time_length(difftime(rsv_primary_inf_date, 
                         study_start_date - days(1), "weeks"), "years"),
      # #time until second mild rsv outcome
      # time_rsv_primary_second = time_length(difftime(rsv_primary_second_inf_date, 
      #                           study_start_date - days(1), "weeks"), "years"),
      #time until severe rsv outcome
      time_rsv_secondary = time_length(difftime(rsv_secondary_inf_date, 
                           study_start_date - days(1), "weeks"), "years"),
      # #time until second severe rsv outcome
      # time_rsv_secondary_second = time_length(difftime(rsv_secondary_second_inf_date, 
      #                             study_start_date - days(1), "weeks"), "years"),
      #time until rsv mortality
      time_rsv_mortality = time_length(difftime(rsv_mortality_inf_date, 
                          study_start_date - days(1), "weeks"), "years"),
      #time until mild flu outcome
      time_flu_primary = time_length(difftime(flu_primary_inf_date, 
                         study_start_date - days(1), "weeks"), "years"),
      # #time until second mild flu outcome
      # time_flu_primary_second = time_length(difftime(flu_primary_second_inf_date, 
      #                           study_start_date - days(1), "weeks"), "years"),
      #time until severe flu outcome
      time_flu_secondary = time_length(difftime(flu_secondary_inf_date, 
                           study_start_date - days(1), "weeks"), "years"),
      # #time until second severe flu outcome
      # time_flu_secondary_second = time_length(difftime(flu_secondary_second_inf_date, 
      #                             study_start_date - days(1), "weeks"), "years"),
      #time until flu mortality
      time_flu_mortality = time_length(difftime(flu_mortality_inf_date, 
                          study_start_date - days(1), "weeks"), "years"),
      #time until mild covid outcome
      time_covid_primary = time_length(difftime(covid_primary_inf_date, 
                           study_start_date - days(1), "weeks"), "years"),
      # #time until second mild covid outcome
      # time_covid_primary_second = time_length(difftime(covid_primary_second_inf_date, 
      #                             study_start_date - days(1), "weeks"), "years"),
      #time until severe covid outcome
      time_covid_secondary = time_length(difftime(covid_secondary_inf_date, 
                             study_start_date - days(1), "weeks"), "years"),
      # #time until second severe covid outcome
      # time_covid_secondary_second = time_length(difftime(covid_secondary_second_inf_date, 
      #                               study_start_date - days(1), "weeks"), "years"),
      #time until covid mortality
      time_covid_mortality = time_length(difftime(covid_mortality_inf_date, 
                             study_start_date - days(1), "weeks"), "years")
    )
  if (codelist_type == "sensitive") {
    df_input <- df_input %>%
      mutate(
        #time until mild overall respiratory outcome
        time_overall_resp_primary = time_length(difftime(overall_resp_primary_inf_date, 
                                    study_start_date - days(1), "weeks"), "years"),
        # #time until second mild overall respiratory outcome
        # time_overall_resp_primary_second = time_length(difftime(overall_resp_primary_second_inf_date, 
        #                                   study_start_date - days(1), "weeks"), "years"),
        #time until severe overall respiratory outcome
        time_overall_resp_secondary = time_length(difftime(overall_resp_secondary_inf_date, 
                                      study_start_date - days(1), "weeks"), "years"),
        # #time until second severe overall respiratory outcome
        # time_overall_resp_secondary_second = time_length(difftime(overall_resp_secondary_second_inf_date, 
        #                                     study_start_date - days(1), "weeks"), "years"),
        #time until overall respiratory mortality
        time_overall_resp_mortality = time_length(difftime(overall_resp_mortality_inf_date, 
                                      study_start_date - days(1), "weeks"), "years")
      )
  }
  df_input <- df_input %>%
    mutate(
      #time until all cause mortality
      time_all_cause_mortality = time_length(difftime(all_cause_mortality_inf_date, 
                                 study_start_date - days(1), "weeks"), "years")
    )
}

## create output directories ----
fs::dir_create(here("output", "data"))

#write the new input file
write_feather(df_input, here::here("output", "data", 
  paste0("input_processed_", cohort, "_", year(study_start_date),
         "_", year(study_end_date), "_", codelist_type, 
         "_", investigation_type, ".arrow")))
