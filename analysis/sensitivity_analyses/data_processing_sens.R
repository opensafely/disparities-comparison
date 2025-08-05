library(tidyverse)
library(here)
library(arrow)
library(data.table)
library(lubridate)
library(magrittr)
library(purrr)

## create output directories ----
fs::dir_create(here::here("analysis", "sensitivity_analyses"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- as.Date("2018-09-01")
  study_end_date <- as.Date("2019-08-31")
  cohort <- "infants"
  codelist_type <- "specific"
  investigation_type_data <- "primary"
  investigation_type <- "sensitivity"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type_data <- args[[5]]
  investigation_type <- args[[6]]
}

#set the new start and end dates
study_start_date_sens <- as.Date(paste0(year(study_start_date), "-10-01"))
study_end_date_sens <- as.Date(paste0(year(study_end_date), "-03-31"))

df_input <- read_feather(
  here::here("output", "data", paste0("input_", cohort, "_",
             year(study_start_date), "_", year(study_end_date), "_",
             codelist_type, "_", investigation_type_data,".arrow")))

if (cohort == "infants_subgroup") {
  df_input_mothers <- read_feather(here::here("output", "data",
                                   paste0("input_maternal_infants_subgroup_",
                                   year(study_start_date), "_",
                                   year(study_end_date), "_", codelist_type,
                                   "_", investigation_type_data,".arrow")))
  df_input_mothers <- df_input_mothers %>%
    mutate(mother_id = patient_id) %>%
    select(-patient_id)
  df_input <- merge(df_input, df_input_mothers, by = "mother_id")
}

#adjust 'patient_end_date' to account for death and deregistration
df_input <- df_input %>%
  mutate(
    patient_end_date = pmin(patient_end_date, death_date, deregistration_date,
                            na.rm = TRUE)
  )

#subset processed data to include only october-march
cols <- str_detect(names(df_input), "date")
col_names <- colnames(df_input[, cols])
#remove vaccination dates from the list of columns
col_names <- col_names[!col_names %in% c(paste0(colnames(df_input[,
                       str_detect(names(df_input), "vaccination")])))]
#remove birth date from list of columns
col_names <- col_names[!col_names %in% c("birth_date", "patient_index_date",
                                         "patient_end_date")]
df_input_filt <- df_input %>%
  mutate(across(all_of(col_names), ~if_else(.x >= study_start_date_sens &
         .x <= study_end_date_sens, .x, NA_Date_)))

#edit patient specific index date
df_input_filt <- df_input_filt %>%
  mutate(
    patient_index_date = case_when(
    patient_index_date < study_start_date_sens ~ study_start_date_sens,
    patient_index_date > study_end_date_sens ~ NA_Date_,
    TRUE ~ patient_index_date),
    patient_end_date = case_when(
    patient_end_date > study_end_date_sens ~ study_end_date_sens,
    patient_end_date < study_start_date_sens ~ NA_Date_,
    TRUE ~ patient_end_date)
  ) %>%
  filter(!is.na(patient_index_date) & !is.na(patient_end_date))

#set index date as one day prior so they have survived to start
df_input_filt <- df_input_filt %>%
  mutate(
    patient_index_date = patient_index_date - days(1)
  )

#create time dependency
if(cohort == "infants" | cohort == "infants_subgroup") {
  df_input_filt <- df_input_filt %>%
    rowwise() %>%
    mutate(
      date = map2(patient_index_date, patient_end_date,
                  ~seq.Date(.x, .y, by = "month"))
    ) %>%
    unnest(date) %>%
    mutate(
      age = age + as.numeric((date - patient_index_date)/30.44)
    ) %>%
    ungroup()
}

#calculate age bands
if(cohort == "older_adults") {
  df_input_filt <- df_input_filt %>%
    mutate(age_band = case_when(
      age > 64 & age < 75 ~ "65-74y",
      age > 74 & age < 90 ~ "75-89y",
      age > 89 ~ "90y+",
      TRUE ~ NA_character_)
    )
} else if(cohort == "adults") {
  df_input_filt <- df_input_filt %>%
    mutate(age_band = case_when(
      age > 17 & age < 40 ~ "18-39y",
      age > 39 & age < 65 ~ "40-64y",
      TRUE ~ NA_character_)
    )
} else if(cohort == "children_and_adolescents") {
  df_input_filt <- df_input_filt %>%
    mutate(age_band = case_when(
      age > 1 & age < 6 ~ "2-5y",
      age > 5 & age < 10 ~ "6-9y",
      age > 9 & age < 14 ~ "10-13y",
      age > 13 & age < 18 ~ "14-17y",
      TRUE ~ NA_character_)
    )
} else {
  df_input_filt <- df_input_filt %>%
    mutate(age_band = case_when(
      age >= 0 & age < 3 ~ "0-2m",
      age > 2 & age < 6 ~ "3-5m",
      age > 5 & age < 12 ~ "6-11m",
      age > 11 & age < 24 ~ "12-23m",
      TRUE ~ NA_character_)
    ) %>%
    filter(!is.na(age_band))
}

df_input_filt$age_band <- factor(df_input_filt$age_band)

#data manipulation
df_input_filt <- df_input_filt %>%
  mutate(
    #assign ethnicity group
    latest_ethnicity_group = fct_relevel(latest_ethnicity_group,
                                         c("1", "2", "3", "4", "5")),
    #calculate IMD quintile
    imd_quintile = factor(case_when(
      imd_rounded >= 0 & imd_rounded < as.integer(32800 * 1 / 5) ~ "1 (most deprived)",
      imd_rounded < as.integer(32800 * 2 / 5) ~ "2",
      imd_rounded < as.integer(32800 * 3 / 5) ~ "3",
      imd_rounded < as.integer(32800 * 4 / 5) ~ "4",
      imd_rounded <= as.integer(32800 * 5 / 5) ~ "5 (least deprived)",
      TRUE ~ NA_character_)),
    #format sex
    sex = relevel(factor(case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      TRUE ~ NA_character_)), ref = "Female")
  )

#identify columns with logical values, excluding specified columns
logical_cols <- which(sapply(df_input_filt, is.logical) &
                      !grepl("primary|secondary|mortality|registered",
                             names(df_input_filt)))

#apply mutation to convert logical columns to factors
df_input_filt <- df_input_filt %>%
  mutate(across(
    .cols = all_of(logical_cols),
    .fns = ~relevel(factor(case_when(
      . == FALSE ~ "No",
      . == TRUE ~ "Yes",
      TRUE ~ NA_character_
    )), ref = "No")
  ))

#more data manipulation
df_input_filt <- df_input_filt %>%
  mutate(
    #add labels to ethnicity
    latest_ethnicity_group = relevel(factor(latest_ethnicity_group,
                                            levels = c("1", "2", "3", "4", "5"),
                                            labels = c("White", "Mixed",
                                                       "Asian or Asian British",
                                                       "Black or Black British",
                                                       "Other Ethnic Groups"),
                                            ordered = FALSE), ref = "White"),
    #recode imd quintile 
    imd_quintile = relevel(recode(imd_quintile,
                                  "1 (most deprived)" = "5 (most deprived)",
                                  "2" = "4", "3" = "3", "4" = "2",
                                  "5 (least deprived)" = "1 (least deprived)"),
                           ref = "1 (least deprived)"),
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
      TRUE ~ NA_character_))
  )

#ethnicity from HES
if (cohort == "infants" | cohort == "infants_subgroup") {
  df_input_filt <- df_input_filt %>%
    mutate(
      latest_ethnicity_group_hes = relevel(factor(recode(
        latest_ethnicity_group_hes, "A" = "White", "B" = "White", "C" = "White",
        "D" = "Mixed", "E" = "Mixed", "F" = "Mixed", "G" = "Mixed",
        "H" = "Asian or Asian British", "J" = "Asian or Asian British",
        "K" = "Asian or Asian British", "L" = "Asian or Asian British",
        "M" = "Black or Black British", "N" = "Black or Black British",
        "P" = "Black or Black British", "R" = "Other Ethnic Groups",
        "S" = "Other Ethnic Groups"), ordered = F), ref = "White")
    ) %>%
    mutate(
      latest_ethnicity_group = if_else(is.na(latest_ethnicity_group),
                                       latest_ethnicity_group_hes,
                                       latest_ethnicity_group)
    )
}

#maternal characteristics
if (cohort == "infants_subgroup") {
  df_input_filt <- df_input_filt %>%
    mutate(
      #create smoking status factor
      maternal_smoking_status = relevel(factor(maternal_smoking_status,
                                        ordered = FALSE), ref = "Never")
    )
}

#flu vaccination
if (cohort != "infants" & cohort != "infants_subgroup") {
  df_input_filt <- df_input_filt %>%
    mutate(
      #assign flu vaccination status
      flu_vaccination_immunity_date = if_else(
        flu_vaccination_date + days(10) < patient_end_date,
        flu_vaccination_date + days(10), NA_Date_)
  )
}

#define two vaccinations categories for each outcome type, set vaccination to null
#if immunity date occurs after outcome date
if (cohort != "infants" & cohort != "infants_subgroup") {
  df_input_filt <- df_input_filt %>%
    mutate(
      #define flu_vaccination_mild
      flu_vaccination_mild = relevel(factor(case_when(
        flu_vaccination_immunity_date > flu_primary_date ~ "No",
        is.na(flu_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes")), ref = "No"),
      #define flu_vaccination severe 
      flu_vaccination_severe = relevel(factor(case_when(
        flu_vaccination_immunity_date > flu_secondary_date ~ "No",
        is.na(flu_vaccination_immunity_date) ~ "No",
        TRUE ~ "Yes")), ref = "No")
    )
}

if (study_start_date == as.Date("2017-09-01")) {
  if (cohort == "infants_subgroup") {
    df_input_filt <- df_input_filt %>%
      select(-contains("flu_"), maternal_flu_vaccination)
  } else {
    df_input_filt <- df_input_filt %>%
      select(-contains("flu_"))
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
      #infer mild case date for rsv 
      rsv_primary_inf_date = pmin(
        rsv_primary_date, rsv_secondary_date, deregistration_date,
        death_date, patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      rsv_primary_censor = if_else(
        rsv_primary_inf_date < rsv_primary_date, 1, 0, missing = 1),
      #infer mild rsv outcome 
      rsv_primary_inf = if_else(rsv_primary_censor == 0, 1, 0),
      #infer severe case date for rsv
      rsv_secondary_inf_date = pmin(
        rsv_secondary_date, deregistration_date, death_date,
        patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      rsv_secondary_censor = if_else(
        rsv_secondary_inf_date < rsv_secondary_date, 1, 0, missing = 1),
      #infer severe rsv outcome
      rsv_secondary_inf = if_else(rsv_secondary_censor == 0, 1, 0)
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  df_input_filt <- df_input_filt %>%
    select(-contains("rsv_"))
  #infer outcomes from event dates 
  df_input_filt <- df_input_filt %>%
    mutate(
      #infer presence of mild rsv
      flu_primary = if_else(
        !is.na(flu_primary_date), TRUE, FALSE),
      #infer presence of second episode of mild rsv
      flu_primary_second = if_else(
        !is.na(flu_primary_second_date), TRUE, FALSE),
      #infer presence of severe rsv
      flu_secondary = if_else(
        !is.na(flu_secondary_date), TRUE, FALSE),
      #infer presence of second episode of severe rsv
      flu_secondary_second = if_else(
        !is.na(flu_secondary_second_date), TRUE, FALSE),
      #infer mild case date for flu 
      flu_primary_inf_date = pmin(
        flu_primary_date, flu_secondary_date, deregistration_date,
        death_date, patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      flu_primary_censor = if_else(
        flu_primary_inf_date < flu_primary_date, 1, 0, missing = 1),
      #infer mild flu outcome 
      flu_primary_inf = if_else(flu_primary_censor == 0, 1, 0),
      #infer severe case date for flu
      flu_secondary_inf_date = pmin(
        flu_secondary_date, deregistration_date, death_date,
        patient_end_date, na.rm = TRUE),
      #assign censoring indicator
      flu_secondary_censor = if_else(
        flu_secondary_inf_date < flu_secondary_date, 1, 0, missing = 1),
      #infer severe flu outcome
      flu_secondary_inf = if_else(flu_secondary_censor == 0, 1, 0)
    )
}

#calculate time to event
if (study_start_date == as.Date("2017-09-01")) {
  df_input_filt <- df_input_filt %>%
    mutate(
      #time until mild rsv outcome
      time_rsv_primary = time_length(difftime(rsv_primary_inf_date, 
                         patient_index_date - days(1), "weeks"), "years"),
      #time until severe rsv outcome
      time_rsv_secondary = time_length(difftime(rsv_secondary_inf_date, 
                           patient_index_date - days(1), "weeks"), "years")
    )
} else if (study_start_date == as.Date("2018-09-01")) {
  df_input_filt <- df_input_filt %>%
    mutate(
      #time until mild flu outcome
      time_flu_primary = time_length(difftime(flu_primary_inf_date, 
                         patient_index_date - days(1), "weeks"), "years"),
      #time until severe flu outcome
      time_flu_secondary = time_length(difftime(flu_secondary_inf_date, 
                           patient_index_date - days(1), "weeks"), "years")
    )
}

## create output directories ----
fs::dir_create(here::here("output", "data"))

#write the new input file
write_feather(df_input_filt, here::here("output", "data",
  paste0("input_processed_", cohort, "_", year(study_start_date),
         "_", year(study_end_date), "_", codelist_type,
         "_sensitivity", ".arrow")))
