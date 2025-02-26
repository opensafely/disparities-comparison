library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(broom)

## create output directories ----
fs::dir_create(here::here("analysis", "exploratory_analyses"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "infants"
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
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#compare those with and without HES ethnicity
df_input <- df_input %>%
  mutate(
    ethnicity_HES = if_else(is.na(latest_ethnicity_group_hes),
                            FALSE, TRUE)
  ) 

##create required contingency tables
contingency <- function(df, var2) {
  
  contingency <- table(df[[var2]], df$ethnicity_HES)
  return(as.data.frame(contingency))
  
}

#create contingency tables to output
if (study_start_date == covid_season_min & codelist_type == "sensitive") {
  
  contingency_tbls <- bind_rows(
    contingency(df_input, "age_band"),
    contingency(df_input, "sex"),
    contingency(df_input, "imd_quintile"),
    contingency(df_input, "rurality_classification"),
    contingency(df_input, "household_composition"),
    contingency(df_input, "rsv_primary_inf"),
    contingency(df_input, "rsv_secondary_inf"),
    contingency(df_input, "flu_primary_inf"),
    contingency(df_input, "flu_secondary_inf"),
    contingency(df_input, "covid_primary_inf"),
    contingency(df_input, "covid_secondary_inf"),
    contingency(df_input, "overall_resp_primary_inf"),
    contingency(df_input, "overall_resp_secondary_inf")
  )
  
} else if (study_start_date > covid_season_min & codelist_type == "sensitive") {
  
  contingency_tbls <- bind_rows(
    contingency(df_input, "age_band"),
    contingency(df_input, "sex"),
    contingency(df_input, "imd_quintile"),
    contingency(df_input, "rurality_classification"),
    contingency(df_input, "rsv_primary_inf"),
    contingency(df_input, "rsv_secondary_inf"),
    contingency(df_input, "flu_primary_inf"),
    contingency(df_input, "flu_secondary_inf"),
    contingency(df_input, "covid_primary_inf"),
    contingency(df_input, "covid_secondary_inf"),
    contingency(df_input, "overall_resp_primary_inf"),
    contingency(df_input, "overall_resp_secondary_inf")
  )
  
} else if (study_start_date == covid_season_min) {
  
  contingency_tbls <- bind_rows(
    contingency(df_input, "age_band"),
    contingency(df_input, "sex"),
    contingency(df_input, "imd_quintile"),
    contingency(df_input, "rurality_classification"),
    contingency(df_input, "household_composition"),
    contingency(df_input, "rsv_primary_inf"),
    contingency(df_input, "rsv_secondary_inf"),
    contingency(df_input, "flu_primary_inf"),
    contingency(df_input, "flu_secondary_inf"),
    contingency(df_input, "covid_primary_inf"),
    contingency(df_input, "covid_secondary_inf")
  )
  
} else if (study_start_date > covid_season_min) {
  
  contingency_tbls <- bind_rows(
    contingency(df_input, "age_band"),
    contingency(df_input, "sex"),
    contingency(df_input, "imd_quintile"),
    contingency(df_input, "rurality_classification"),
    contingency(df_input, "rsv_primary_inf"),
    contingency(df_input, "rsv_secondary_inf"),
    contingency(df_input, "flu_primary_inf"),
    contingency(df_input, "flu_secondary_inf"),
    contingency(df_input, "covid_primary_inf"),
    contingency(df_input, "covid_secondary_inf")
  )
  
} else if (codelist_type == "sensitive") {
  
  contingency_tbls <- bind_rows(
    contingency(df_input, "age_band"),
    contingency(df_input, "sex"),
    contingency(df_input, "imd_quintile"),
    contingency(df_input, "rurality_classification"),
    contingency(df_input, "rsv_primary_inf"),
    contingency(df_input, "rsv_secondary_inf"),
    contingency(df_input, "flu_primary_inf"),
    contingency(df_input, "flu_secondary_inf"),
    contingency(df_input, "overall_resp_primary_inf"),
    contingency(df_input, "overall_resp_secondary_inf")
  )
  
} else {
  
  contingency_tbls <- bind_rows(
    contingency(df_input, "age_band"),
    contingency(df_input, "sex"),
    contingency(df_input, "imd_quintile"),
    contingency(df_input, "rurality_classification"),
    contingency(df_input, "rsv_primary_inf"),
    contingency(df_input, "rsv_secondary_inf"),
    contingency(df_input, "flu_primary_inf"),
    contingency(df_input, "flu_secondary_inf")
  )
  
}

chi2 <- function(df, var2) {
  
  contingency <- table(df[[var2]], df$ethnicity_HES)
  
  chi2 <- chisq.test(contingency)
  
  chi2 <- tidy(chi2)
  chi2_tbl <- tibble(
    variable = var2,
    statistic = chi2$statistic,
    p.value = chi2$p.value,
    parameter = chi2$parameter,
    method = chi2$method,
    codelist_type = codelist_type,
    investigation_type = investigation_type
  )
  
  return(chi2_tbl)
  
}

#age
chi2_age <- chi2(df_input, "age_band")

#sex
chi2_sex <- chi2(df_input, "sex")

#imd
chi2_imd <- chi2(df_input, "imd_quintile")

#rurality
chi2_rurality <- chi2(df_input, "rurality_classification")

#household composition
if (study_start_date == as.Date("2020-09-01") &
    cohort != "infants_subgroup" & cohort != "infants") {
  chi2_household <- chi2(df_input, "household_composition")
}

#rsv mild
chi2_rsv_mild <- chi2(df_input, "rsv_primary_inf")

#rsv severe
chi2_rsv_severe <- chi2(df_input, "rsv_secondary_inf")

#flu mild
chi2_flu_mild <- chi2(df_input, "flu_primary_inf")

#flu severe
chi2_flu_severe <- chi2(df_input, "flu_secondary_inf")

if (study_start_date >= covid_season_min) {
  
  #covid mild
  chi2_covid_mild <- chi2(df_input, "covid_primary_inf")
  
  #covid severe
  chi2_covid_severe <- chi2(df_input, "covid_secondary_inf")
  
}

if (codelist_type == "sensitive") {
  
  #overall resp mild
  chi2_overall_resp_mild <- chi2(df_input, "overall_resp_primary_inf")
  
  #overall resp severe
  chi2_overall_resp_severe <- chi2(df_input, "overall_resp_secondary_inf")
  
}

#output results
if (study_start_date == covid_season_min & codelist_type == "sensitive") {
  
  results <- bind_rows(
    chi2_age, chi2_sex, chi2_imd, chi2_rurality, chi2_household,
    chi2_rsv_mild, chi2_rsv_severe, chi2_flu_mild, chi2_flu_severe,
    chi2_covid_mild, chi2_covid_severe, chi2_overall_resp_mild,
    chi2_overall_resp_severe
  )

} else if (study_start_date > covid_season_min & codelist_type == "sensitive") {
  
  results <- bind_rows(
    chi2_age, chi2_sex, chi2_imd, chi2_rurality,
    chi2_rsv_mild, chi2_rsv_severe, chi2_flu_mild, chi2_flu_severe,
    chi2_covid_mild, chi2_covid_severe, chi2_overall_resp_mild,
    chi2_overall_resp_severe
  )
  
} else if (study_start_date == covid_season_min) {
  
  results <- bind_rows(
    chi2_age, chi2_sex, chi2_imd, chi2_rurality, chi2_household,
    chi2_rsv_mild, chi2_rsv_severe, chi2_flu_mild, chi2_flu_severe,
    chi2_covid_mild, chi2_covid_severe
  )
  
} else if (study_start_date > covid_season_min) {
  
  results <- bind_rows(
    chi2_age, chi2_sex, chi2_imd, chi2_rurality,
    chi2_rsv_mild, chi2_rsv_severe, chi2_flu_mild, chi2_flu_severe,
    chi2_covid_mild, chi2_covid_severe
  )
  
} else if (codelist_type == "sensitive") {
  
  results <- bind_rows(
    chi2_age, chi2_sex, chi2_imd, chi2_rurality,
    chi2_rsv_mild, chi2_rsv_severe, chi2_flu_mild, chi2_flu_severe,
    chi2_overall_resp_mild, chi2_overall_resp_severe
  )
  
} else {
  
  results <- bind_rows(
    chi2_age, chi2_sex, chi2_imd, chi2_rurality,
    chi2_rsv_mild, chi2_rsv_severe, chi2_flu_mild, chi2_flu_severe
  )
  
}

## create output directories ----
fs::dir_create(here::here("output", "exploratory"))

#write to file
write_csv(contingency_tbls, paste0(here::here("output", "exploratory"),
          "/", "ethnicity_HES_comp_tables_", cohort, "_",
          year(study_start_date), "_", year(study_end_date), "_",
          codelist_type, "_", investigation_type, ".csv"))

#write to file
write_csv(results, paste0(here::here("output", "exploratory"),
          "/", "ethnicity_HES_comp_", cohort, "_", year(study_start_date),
          "_", year(study_end_date), "_", codelist_type, "_",
          investigation_type, ".csv"))
