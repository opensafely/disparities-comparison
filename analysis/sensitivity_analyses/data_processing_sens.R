library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(lubridate)
library(magrittr)

## create output directories ----
fs::dir_create(here("analysis", "sensitivity_analyses"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2022-09-01")
  study_end_date <- as.Date("2023-08-31")
  cohort <- "adults"
  codelist_type <- "specific"
  investigation_type_data <- "primary"
  investigation_type <- "sensitivity"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type_data <- args[[5]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

#set the new start and end dates
study_start_date_sens <- as.Date(paste0(year(study_start_date), "-10-01"))
study_end_date_sens <- as.Date(paste0(year(study_end_date), "-03-31"))

df_input <- read_feather(
  here::here("output", "data", paste0("input_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type_data,".arrow")))

if (study_start_date == as.Date("2020-09-01")) {
  
  df_household <- read_feather(
    here::here("output", "data", paste0("input_household_processed_", 
                                        year(study_start_date), "_", year(study_end_date), ".arrow")))
  
  household_comp_vars <- tibble("patient_id" = df_household$patient_id,
                                "num_generations"= df_household$num_generations, 
                                "composition_category" = df_household$composition_category)
  
  df_input <- merge(df_input, household_comp_vars, by = "patient_id")
  
}

if (cohort == "infants_subgroup") {
  df_input_mothers <- read_feather(here::here("output", "data", 
                                   paste0("input_mothers_processed_",
                                   year(study_start_date), "_",
                                   year(study_end_date), "_",
                                   codelist_type, "_", 
                                   investigation_type,".arrow")))
  df_input_mothers <- df_input_mothers %>%
    mutate(mother_id = patient_id)
  df_input <- merge(df_input, df_input_mothers, by = "mother_id")
}

#subset processed data to include only october-march 
cols <- str_detect(names(df_input), "date")
col_names <- colnames(df_input[, cols])
#remove vaccination dates from the list of columns
col_names <- col_names[!col_names %in% c(paste0(colnames(df_input[, str_detect(names(df_input), "vaccination")])))]
df_input_filt <- df_input %>%
  mutate(across(all_of(col_names), ~if_else(.x >= study_start_date_sens & .x <= study_end_date_sens, .x, NA_Date_)))

#create time dependency
if(cohort == "infants" | cohort == "infants_subgroup") {
  df_input_filt <- df_input_filt %>%
    mutate(
      date = map2(study_start_date_sens, study_end_date_sens, ~seq(.x, .y, by = 30.44))
    ) %>%
    unnest(date) %>%
    mutate(
      age = age + as.numeric((date - study_start_date_sens)/30.44)
    ) 
}
  
#calculate age bands
if(cohort == "older_adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age > 64 & age < 75 ~ "65-74y",
      age > 74 & age < 90 ~ "75-89y",
      age > 89 ~ "90y+",
      TRUE ~ "Unknown")
    )
} else if(cohort == "adults") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age > 17 & age < 40 ~ "18-29y",
      age > 39 & age < 65 ~ "40-64y",
      TRUE ~ "Uknown")
    )
} else if(cohort == "children_and_adolescents") {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age > 1 & age < 6 ~ "2-5y",
      age > 5 & age < 10 ~ "6-9y",
      age > 9 & age < 14 ~ "10-13y",
      age > 13 & age < 18 ~ "14-17y",
      TRUE ~ "Unknown")
    )
} else {
  df_input <- df_input %>%
    mutate(age_band = case_when(
      age >= 0 & age < 3 ~ "0-2m",
      age > 2 & age < 6 ~ "3-5m",
      age > 5 & age < 12 ~ "6-11m",
      age > 11 & age < 24 ~ "12-23m",
      TRUE ~ "Unknown")
    )
}

df_input$age_band <- factor(df_input$age_band)

#data manipulation
df_input_filt <- df_input_filt %>%
  mutate(
    #assign ethnicity group
    latest_ethnicity_group = factor(case_when(
      latest_ethnicity_code == "1" ~ "White",
      latest_ethnicity_code == "2" ~ "Mixed",
      latest_ethnicity_code == "3" ~ "Asian or Asian British",
      latest_ethnicity_code == "4" ~ "Black or Black British",
      latest_ethnicity_code == "5" ~ "Other Ethnic Groups",
      TRUE ~ NA_character_), ordered = TRUE),
    #calculate IMD quintile
    imd_quintile = factor(case_when(
      imd_rounded >= 0 & imd_rounded < as.integer(32800 * 1 / 5) ~ "1 (most deprived)",
      imd_rounded < as.integer(32800 * 2 / 5) ~ "2",
      imd_rounded < as.integer(32800 * 3 / 5) ~ "3",
      imd_rounded < as.integer(32800 * 4 / 5) ~ "4",
      imd_rounded <= as.integer(32800 * 5 / 5) ~ "5 (least deprived)",
      TRUE ~ NA_character_), ordered = TRUE),
    #format sex
    sex = factor(case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      sex == "intersex" ~ "Intersex",
      sex == "unknown" ~ "Unknown",
      TRUE ~ NA_character_))
  )

#identify columns with logical values, excluding specified columns
logical_cols <- which(sapply(df_input_filt, is.logical) & !grepl("primary|secondary|mortality|registered", names(df_input_filt)))

#apply mutation to convert logical columns to factors
df_input_filt <- df_input_filt %>%
  mutate(across(
    .cols = all_of(logical_cols), 
    .fns = ~factor(case_when(
      . == FALSE ~ "No",
      . == TRUE ~ "Yes",
      TRUE ~ NA_character_
    ))
  ))

#more data manipulation
df_input_filt <- df_input_filt %>%
  mutate(
    #recode imd quintile 
    imd_quintile = recode(imd_quintile, "1 (most deprived)" = "5 (most deprived)",
                          "2" = "4", "3" = "3", "4" = "2",
                          "5 (least deprived)" = "1 (least deprived)"),
    #recode rurality to 5 levels
    rurality_code = recode(rural_urban_classification, "1" = "1", "2" = "2", 
                           "3" = "3", "4" = "3", "5" = "4", "6" = "4", 
                           "7" = "5", "8" = "5", .missing = NA_character_),
    #assign rurality classification
    rurality_classification = factor(case_when(
      rurality_code == "1" ~ "Urban Major Conurbation",
      rurality_code == "2" ~ "Urban Minor Conurbation",
      rurality_code == "3" ~ "Urban City and Town",
      rurality_code == "4" ~ "Rural Town and Fringe",
      rurality_code == "5" ~ "Rural Village and Dispersed",
      TRUE ~ NA_character_), ordered = TRUE)
  )

#household variables for when they are included (2020-21)
if (study_start_date == as.Date("2020-09-01")) {
  df_input_filt <- df_input_filt %>%
    mutate(
      #define household size categories
      household_size_cat = factor(case_when(
        household_size >= 1 & household_size <= 2 ~ "1",
        household_size >= 3 & household_size <= 5 ~ "2",
        household_size >= 6 ~ "3",
        TRUE ~ "Unknown")),
      composition_category = fct_relevel(composition_category,
                                         c("Multiple of the Same Generation", "Living Alone",
                                           "One Other Generation", "Two Other Generations",
                                           "Three Other Generations"))
    ) %>% arrange(composition_category)
}

#flu vaccination
if (cohort != "infants" & cohort != "infants_subgroup") {
  df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
    mutate(
      time_since_last_covid_vaccination = factor(case_when(
      time_length(difftime(study_start_date_sens, last_covid_vaccination_date, 
                             units = "days"), "months") >= 0 &
        time_length(difftime(study_start_date_sens, last_covid_vaccination_date,
                             units = "days"), "months") < 6 ~ "0-6m",
      time_length(difftime(study_start_date_sens, last_covid_vaccination_date,
                           units = "days"), "months") >= 6 &
        time_length(difftime(study_start_date_sens, last_covid_vaccination_date, 
                             units = "days"), "months") < 12 ~ "6-12m",
      time_length(difftime(study_start_date_sens, last_covid_vaccination_date,
                           units = "days"), "months") >= 12 ~ "12m+",
        TRUE ~ NA_character_), ordered = TRUE)
  )
}
if (study_start_date >= covid_current_vacc_min & cohort != "infants" & cohort != "infants_subgroup") {
  df_input_filt <- df_input_filt %>% 
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
  df_input_filt <- df_input_filt %>%
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

#set covid date to missing if existing date occurs before March 1st 2020
if (study_start_date >= covid_season_min) {
  df_input_filt <- df_input_filt %>%
    mutate(
      covid_primary_date = if_else(covid_primary_date < as.Date("2020-03-01"), NA_Date_, covid_primary_date),
      covid_primary_second_date = if_else(covid_primary_date < as.Date("2020-03-01"), NA_Date_, covid_primary_second_date),
      covid_secondary_date = if_else(covid_secondary_date < as.Date("2020-03-01"), NA_Date_, covid_secondary_date),
      covid_secondary_second_date = if_else(covid_secondary_date < as.Date("2020-03-01"), NA_Date_, covid_secondary_second_date),
      covid_mortality_date = if_else(covid_mortality_date < as.Date("2020-03-01"), NA_Date_, covid_mortality_date)
    )
}

#infer outcomes from event dates 
df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
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

df_input_filt <- df_input_filt %>%
  mutate(
    #infer presence of all cause mortality
    all_cause_mortality_date = death_date,
    all_cause_mortality = if_else(
      !is.na(all_cause_mortality_date), TRUE, FALSE)
  )

#define event time 
if (study_start_date < covid_season_min) {
  df_input_filt <- df_input_filt %>%
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
    df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
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
      # rsv_secondary_inf = if_else(rsv_secondary_censor == 0, 1, 0),
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
    df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
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
    df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
    mutate(
      #time until all cause mortality
      time_all_cause_mortality = time_length(difftime(all_cause_mortality_inf_date, 
                                 study_start_date - days(1), "weeks"), "years")
    )
} else {
  df_input_filt <- df_input_filt %>%
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
    df_input_filt <- df_input_filt %>%
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
  df_input_filt <- df_input_filt %>%
    mutate(
      #time until all cause mortality
      time_all_cause_mortality = time_length(difftime(all_cause_mortality_inf_date, 
                                 study_start_date - days(1), "weeks"), "years")
    )
}

## create output directories ----
fs::dir_create(here("output", "data"))

#write the new input file
write_feather(df_input_filt, here::here("output", "data", 
  paste0("input_processed_", cohort, "_", year(study_start_date),
         "_", year(study_end_date), "_", codelist_type, 
         "_sensitivity", ".arrow")))
