library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here("analysis"))

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
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

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
                                      year(study_start_date), "_", year(study_end_date), "_", 
                                      codelist_type, "_", investigation_type,".arrow"))) 

if (cohort == "infants") {
  ##Use Poisson regression for outcomes looking first at ethnicity 
  
  #rsv primary by ethnicity
  rsv_mild_ethnicity <- glm(rsv_primary_inf ~ latest_ethnicity_group + 
                              age_band + sex + 
                              rurality_classification + 
                              offset(log(time_rsv_primary)), 
                            data = df_input, family = poisson)
  rsv_mild_ethnicity_output <- tidy(rsv_mild_ethnicity)
  
  #rsv secondary by ethnicity
  rsv_severe_ethnicity <- glm(rsv_secondary_inf ~ latest_ethnicity_group +
                                age_band + sex + 
                                rurality_classification + 
                                offset(log(time_rsv_secondary)),
                              data = df_input, family = poisson)
  rsv_severe_ethnicity_output <- tidy(rsv_severe_ethnicity)
  
  #rsv mortality by ethnicity
  rsv_mortality_ethnicity <- glm(rsv_mortality ~ latest_ethnicity_group + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   offset(log(time_rsv_mortality)),
                                 data = df_input, family = poisson)
  rsv_mortality_ethnicity_output <- tidy(rsv_mortality_ethnicity)
  
  #flu primary by ethnicity
  flu_mild_ethnicity <- glm(flu_primary_inf ~ latest_ethnicity_group + 
                              age_band + sex + 
                              rurality_classification + 
                              offset(log(time_flu_primary)),
                            data = df_input, family = poisson)
  flu_mild_ethnicity_output <- tidy(flu_mild_ethnicity)
  
  #flu secondary by ethnicity
  flu_severe_ethnicity <- glm(flu_secondary_inf ~ latest_ethnicity_group + 
                                age_band + sex + 
                                rurality_classification + 
                                offset(log(time_flu_secondary)),
                              data = df_input, family = poisson)
  flu_severe_ethnicity_output <- tidy(flu_severe_ethnicity)
  
  #flu mortality by ethnicity
  flu_mortality_ethnicity <- glm(flu_mortality ~ latest_ethnicity_group + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   offset(log(time_flu_mortality)),
                                 data = df_input, family = poisson)
  flu_mortality_ethnicity_output <- tidy(flu_mortality_ethnicity)
  
  if (study_start_date >= covid_season_min) {
    #covid primary by ethnicity
    covid_mild_ethnicity <- glm(covid_primary_inf ~ latest_ethnicity_group + 
                                  age_band + sex + 
                                  rurality_classification + 
                                  offset(log(time_covid_primary)),
                                data = df_input, family = poisson)
    covid_mild_ethnicity_output <- tidy(covid_mild_ethnicity)
    
    #covid secondary by ethnicity
    covid_severe_ethnicity <- glm(covid_secondary_inf ~ latest_ethnicity_group + 
                                    age_band + sex + 
                                    rurality_classification + 
                                    offset(log(time_covid_secondary)),
                                  data = df_input, family = poisson)
    covid_severe_ethnicity_output <- tidy(covid_severe_ethnicity)
    
    #covid mortality by ethnicity
    covid_mortality_ethnicity <- glm(covid_mortality ~ latest_ethnicity_group + 
                                       age_band + sex + 
                                       rurality_classification + 
                                       offset(log(time_covid_mortality)),
                                     data = df_input, family = poisson)
    covid_mortality_ethnicity_output <- tidy(covid_mortality_ethnicity)
  }
  
  if (codelist_type == "sensitive") {
    #overall_resp primary by ethnicity
    overall_resp_mild_ethnicity <- glm(overall_resp_primary_inf ~ latest_ethnicity_group + 
                                         age_band + sex + 
                                         rurality_classification + 
                                         offset(log(time_overall_resp_primary)),
                                       data = df_input, family = poisson)
    overall_resp_mild_ethnicity_output <- tidy(overall_resp_mild_ethnicity)
    
    #overall_resp secondary by ethnicity
    overall_resp_severe_ethnicity <- glm(overall_resp_secondary_inf ~ latest_ethnicity_group + 
                                           age_band + sex + 
                                           rurality_classification + 
                                           offset(log(time_overall_resp_secondary)),
                                         data = df_input, family = poisson)
    overall_resp_severe_ethnicity_output <- tidy(overall_resp_severe_ethnicity)
    
    #overall_resp mortality by ethnicity
    overall_resp_mortality_ethnicity <- glm(overall_resp_mortality ~ latest_ethnicity_group + 
                                              age_band + sex + 
                                              rurality_classification + 
                                              offset(log(time_overall_resp_mortality)),
                                            data = df_input, family = poisson)
    overall_resp_mortality_ethnicity_output <- tidy(overall_resp_mortality_ethnicity)
  }
  
  #all cause mortality by ethnicity
  all_cause_mortality_ethnicity <- glm(all_cause_mortality ~ latest_ethnicity_group + 
                                         age_band + sex + 
                                         rurality_classification + 
                                         offset(log(time_all_cause_mortality)),
                                       data = df_input, family = poisson)
  all_cause_mortality_ethnicity_output <- tidy(all_cause_mortality_ethnicity)
  
  ##Now looking at disparity 
  
  #rsv primary by ses
  rsv_mild_ses <- glm(rsv_primary_inf ~ imd_quintile + 
                        age_band + sex + 
                        rurality_classification + 
                        offset(log(time_rsv_primary)),
                      data = df_input, family = poisson)
  rsv_mild_ses_output <- tidy(rsv_mild_ses)
  
  #rsv secondary by ses
  rsv_severe_ses <- glm(rsv_secondary_inf ~ imd_quintile + 
                          age_band + sex + 
                          rurality_classification + 
                          offset(log(time_rsv_secondary)),
                        data = df_input, family = poisson)
  rsv_severe_ses_output <- tidy(rsv_severe_ses)
  
  #rsv mortality by ses
  rsv_mortality_ses <- glm(rsv_mortality ~ imd_quintile + 
                             age_band + sex + 
                             rurality_classification + 
                             offset(log(time_rsv_mortality)),
                           data = df_input, family = poisson)
  rsv_mortality_ses_output <- tidy(rsv_mortality_ses)
  
  #flu primary by ses
  flu_mild_ses <- glm(flu_primary_inf ~ imd_quintile + 
                        age_band + sex + 
                        rurality_classification + 
                        offset(log(time_flu_primary)),
                      data = df_input, family = poisson)
  flu_mild_ses_output <- tidy(flu_mild_ses)
  
  #flu secondary by ses
  flu_severe_ses <- glm(flu_secondary_inf ~ imd_quintile + 
                          age_band + sex + 
                          rurality_classification + 
                          offset(log(time_flu_secondary)),
                        data = df_input, family = poisson)
  flu_severe_ses_output <- tidy(flu_severe_ses)
  
  #flu mortality by ses
  flu_mortality_ses <- glm(flu_mortality ~ imd_quintile + 
                             age_band + sex + 
                             rurality_classification + 
                             offset(log(time_flu_mortality)),
                           data = df_input, family = poisson)
  flu_mortality_ses_output <- tidy(flu_mortality_ses)
  
  if (study_start_date >= covid_season_min) {
    #covid primary by ses
    covid_mild_ses <- glm(covid_primary_inf ~ imd_quintile + 
                            age_band + sex + 
                            rurality_classification + 
                            offset(log(time_covid_primary)),
                          data = df_input, family = poisson)
    covid_mild_ses_output <- tidy(covid_mild_ses)
    
    #covid secondary by ses
    covid_severe_ses <- glm(covid_secondary_inf ~ imd_quintile + 
                              age_band + sex + 
                              rurality_classification + 
                              offset(log(time_covid_secondary)),
                            data = df_input, family = poisson)
    covid_severe_ses_output <- tidy(covid_severe_ses)
    
    #covid mortality by ses
    covid_mortality_ses <- glm(covid_mortality ~ imd_quintile + 
                                 age_band + sex + 
                                 rurality_classification + 
                                 offset(log(time_covid_mortality)),
                               data = df_input, family = poisson)
    covid_mortality_ses_output <- tidy(covid_mortality_ses)
  }
  
  if (codelist_type == "sensitive") {
    #overall_resp primary by ses
    overall_resp_mild_ses <- glm(overall_resp_primary_inf ~ imd_quintile + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   offset(log(time_overall_resp_primary)),
                                 data = df_input, family = poisson)
    overall_resp_mild_ses_output <- tidy(overall_resp_mild_ses)
    
    #overall_resp secondary by ses
    overall_resp_severe_ses <- glm(overall_resp_secondary_inf ~ imd_quintile + 
                                     age_band + sex + 
                                     rurality_classification + 
                                     offset(log(time_overall_resp_secondary)),
                                   data = df_input, family = poisson)
    overall_resp_severe_ses_output <- tidy(overall_resp_severe_ses)
    
    #overall_resp mortality by ses
    overall_resp_mortality_ses <- glm(overall_resp_mortality ~ imd_quintile + 
                                        age_band + sex + 
                                        rurality_classification + 
                                        offset(log(time_overall_resp_mortality)),
                                      data = df_input, family = poisson)
    overall_resp_mortality_ses_output <- tidy(overall_resp_mortality_ses)
  }
  
  #all cause mortality by ses
  all_cause_mortality_ses <- glm(all_cause_mortality ~ imd_quintile + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   offset(log(time_all_cause_mortality)),
                                 data = df_input, family = poisson)
  all_cause_mortality_ses_output <- tidy(all_cause_mortality_ses)
  
  ##Finally looking at household size/composition
  
  #rsv primary by household composition
  rsv_mild_hh_comp <- glm(rsv_primary_inf ~ composition_category + 
                              age_band + sex + 
                              rurality_classification + 
                              offset(log(time_rsv_primary)), 
                            data = df_input, family = poisson)
  rsv_mild_hh_comp_output <- tidy(rsv_mild_hh_comp)
  
  #rsv secondary by household composition
  rsv_severe_hh_comp <- glm(rsv_secondary_inf ~ composition_category +
                                age_band + sex + 
                                rurality_classification + 
                                offset(log(time_rsv_secondary)),
                              data = df_input, family = poisson)
  rsv_severe_hh_comp_output <- tidy(rsv_severe_hh_comp)
  
  #rsv mortality by household composition
  rsv_mortality_hh_comp <- glm(rsv_mortality ~ composition_category + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   offset(log(time_rsv_mortality)),
                                 data = df_input, family = poisson)
  rsv_mortality_hh_comp_output <- tidy(rsv_mortality_hh_comp)
  
  #flu primary by household composition
  flu_mild_hh_comp <- glm(flu_primary_inf ~ composition_category + 
                              age_band + sex + 
                              rurality_classification + 
                              offset(log(time_flu_primary)),
                            data = df_input, family = poisson)
  flu_mild_hh_comp_output <- tidy(flu_mild_hh_comp)
  
  #flu secondary by household composition
  flu_severe_hh_comp <- glm(flu_secondary_inf ~ composition_category + 
                                age_band + sex + 
                                rurality_classification + 
                                offset(log(time_flu_secondary)),
                              data = df_input, family = poisson)
  flu_severe_hh_comp_output <- tidy(flu_severe_hh_comp)
  
  #flu mortality by household composition
  flu_mortality_hh_comp <- glm(flu_mortality ~ composition_category + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   offset(log(time_flu_mortality)),
                                 data = df_input, family = poisson)
  flu_mortality_hh_comp_output <- tidy(flu_mortality_hh_comp)
  
  if (study_start_date >= covid_season_min) {
    #covid primary by household composition
    covid_mild_hh_comp <- glm(covid_primary_inf ~ composition_category + 
                                  age_band + sex + 
                                  rurality_classification + 
                                  offset(log(time_covid_primary)),
                                data = df_input, family = poisson)
    covid_mild_hh_comp_output <- tidy(covid_mild_hh_comp)
    
    #covid secondary by household composition
    covid_severe_hh_comp <- glm(covid_secondary_inf ~ composition_category + 
                                    age_band + sex + 
                                    rurality_classification + 
                                    offset(log(time_covid_secondary)),
                                  data = df_input, family = poisson)
    covid_severe_hh_comp_output <- tidy(covid_severe_hh_comp)
    
    #covid mortality by household composition
    covid_mortality_hh_comp <- glm(covid_mortality ~ composition_category + 
                                       age_band + sex + 
                                       rurality_classification + 
                                       offset(log(time_covid_mortality)),
                                     data = df_input, family = poisson)
    covid_mortality_hh_comp_output <- tidy(covid_mortality_hh_comp)
  }
  
  if (codelist_type == "sensitive") {
    #overall_resp primary by household composition
    overall_resp_mild_hh_comp <- glm(overall_resp_primary_inf ~ composition_category + 
                                         age_band + sex + 
                                         rurality_classification + 
                                         offset(log(time_overall_resp_primary)),
                                       data = df_input, family = poisson)
    overall_resp_mild_hh_comp_output <- tidy(overall_resp_mild_hh_comp)
    
    #overall_resp secondary by household composition
    overall_resp_severe_hh_comp <- glm(overall_resp_secondary_inf ~ composition_category + 
                                           age_band + sex + 
                                           rurality_classification + 
                                           offset(log(time_overall_resp_secondary)),
                                         data = df_input, family = poisson)
    overall_resp_severe_hh_comp_output <- tidy(overall_resp_severe_hh_comp)
    
    #overall_resp mortality by household composition
    overall_resp_mortality_hh_comp <- glm(overall_resp_mortality ~ composition_category + 
                                              age_band + sex + 
                                              rurality_classification + 
                                              offset(log(time_overall_resp_mortality)),
                                            data = df_input, family = poisson)
    overall_resp_mortality_hh_comp_output <- tidy(overall_resp_mortality_hh_comp)
  }
  
  #all cause mortality by household composition
  all_cause_mortality_hh_comp <- glm(all_cause_mortality ~ composition_category + 
                                         age_band + sex + 
                                         rurality_classification + 
                                         offset(log(time_all_cause_mortality)),
                                       data = df_input, family = poisson)
  all_cause_mortality_hh_comp_output <- tidy(all_cause_mortality_hh_comp)

#add models for infants subgroup
#} else if (cohort == "infants_subgroup") {
  
} else {
  ##Use Poisson regression for outcomes looking first at ethnicity 
  #rsv primary by ethnicity
  rsv_mild_ethnicity <- glm(rsv_primary_inf ~ latest_ethnicity_group + 
                              age_band + sex + 
                              rurality_classification + 
                              flu_vaccination +
                              offset(log(time_rsv_primary)), 
                            data = df_input, family = poisson)
  rsv_mild_ethnicity_output <- tidy(rsv_mild_ethnicity)
  
  #rsv secondary by ethnicity
  rsv_severe_ethnicity <- glm(rsv_secondary_inf ~ latest_ethnicity_group +
                                age_band + sex + 
                                rurality_classification + 
                                flu_vaccination +
                                offset(log(time_rsv_secondary)),
                              data = df_input, family = poisson)
  rsv_severe_ethnicity_output <- tidy(rsv_severe_ethnicity)
  
  #rsv mortality by ethnicity
  rsv_mortality_ethnicity <- glm(rsv_mortality ~ latest_ethnicity_group + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   flu_vaccination +
                                     offset(log(time_rsv_mortality)),
                                   data = df_input, family = poisson)
  rsv_mortality_ethnicity_output <- tidy(rsv_mortality_ethnicity)
    
  #flu primary by ethnicity
  flu_mild_ethnicity <- glm(flu_primary_inf ~ latest_ethnicity_group + 
                              age_band + sex + 
                              rurality_classification + 
                              flu_vaccination +
                              offset(log(time_flu_primary)),
                            data = df_input, family = poisson)
  flu_mild_ethnicity_output <- tidy(flu_mild_ethnicity)
  
  #flu secondary by ethnicity
  flu_severe_ethnicity <- glm(flu_secondary_inf ~ latest_ethnicity_group + 
                                age_band + sex + 
                                rurality_classification + 
                                flu_vaccination +
                                offset(log(time_flu_secondary)),
                              data = df_input, family = poisson)
  flu_severe_ethnicity_output <- tidy(flu_severe_ethnicity)
  
  #flu mortality by ethnicity
  flu_mortality_ethnicity <- glm(flu_mortality ~ latest_ethnicity_group + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   flu_vaccination +
                                   offset(log(time_flu_mortality)),
                                 data = df_input, family = poisson)
  flu_mortality_ethnicity_output <- tidy(flu_mortality_ethnicity)

  if (codelist_type == "sensitive") {
    #overall_resp primary by ethnicity
    overall_resp_mild_ethnicity <- glm(overall_resp_primary_inf ~ latest_ethnicity_group + 
                                         age_band + sex + 
                                         rurality_classification + 
                                         flu_vaccination +
                                         offset(log(time_overall_resp_primary)),
                                       data = df_input, family = poisson)
    overall_resp_mild_ethnicity_output <- tidy(overall_resp_mild_ethnicity)
    
    #overall_resp secondary by ethnicity
    overall_resp_severe_ethnicity <- glm(overall_resp_secondary_inf ~ latest_ethnicity_group + 
                                           age_band + sex + 
                                           rurality_classification + 
                                           flu_vaccination +
                                           offset(log(time_overall_resp_secondary)),
                                         data = df_input, family = poisson)
    overall_resp_severe_ethnicity_output <- tidy(overall_resp_severe_ethnicity)
    
    #overall_resp mortality by ethnicity
    overall_resp_mortality_ethnicity <- glm(overall_resp_mortality ~ latest_ethnicity_group + 
                                              age_band + sex + 
                                              rurality_classification + 
                                              flu_vaccination +
                                              offset(log(time_overall_resp_mortality)),
                                            data = df_input, family = poisson)
    overall_resp_mortality_ethnicity_output <- tidy(overall_resp_mortality_ethnicity)
  }
  
  #all cause mortality by ethnicity
  all_cause_mortality_ethnicity <- glm(all_cause_mortality ~ latest_ethnicity_group + 
                                         age_band + sex + 
                                         rurality_classification + 
                                         flu_vaccination +
                                         offset(log(time_all_cause_mortality)),
                                       data = df_input, family = poisson)
  all_cause_mortality_ethnicity_output <- tidy(all_cause_mortality_ethnicity)
  
  ##Now looking at disparity 
  
  #rsv primary by ses
  rsv_mild_ses <- glm(rsv_primary_inf ~ imd_quintile + 
                        age_band + sex + 
                        rurality_classification + 
                        flu_vaccination +
                        offset(log(time_rsv_primary)),
                      data = df_input, family = poisson)
  rsv_mild_ses_output <- tidy(rsv_mild_ses)
  
  #rsv secondary by ses
  rsv_severe_ses <- glm(rsv_secondary_inf ~ imd_quintile + 
                          age_band + sex + 
                          rurality_classification + 
                          flu_vaccination +
                          offset(log(time_rsv_secondary)),
                        data = df_input, family = poisson)
  rsv_severe_ses_output <- tidy(rsv_severe_ses)
  
  #rsv mortality by ses
  rsv_mortality_ses <- glm(rsv_mortality ~ imd_quintile + 
                             age_band + sex + 
                             rurality_classification + 
                             flu_vaccination +
                             offset(log(time_rsv_mortality)),
                           data = df_input, family = poisson)
  rsv_mortality_ses_output <- tidy(rsv_mortality_ses)
  
  #flu primary by ses
  flu_mild_ses <- glm(flu_primary_inf ~ imd_quintile + 
                        age_band + sex + 
                        rurality_classification + 
                        flu_vaccination +
                        offset(log(time_flu_primary)),
                      data = df_input, family = poisson)
  flu_mild_ses_output <- tidy(flu_mild_ses)
  
  #flu secondary by ses
  flu_severe_ses <- glm(flu_secondary_inf ~ imd_quintile + 
                          age_band + sex + 
                          rurality_classification + 
                          flu_vaccination +
                          offset(log(time_flu_secondary)),
                        data = df_input, family = poisson)
  flu_severe_ses_output <- tidy(flu_severe_ses)
  
  #flu mortality by ses
  flu_mortality_ses <- glm(flu_mortality ~ imd_quintile + 
                             age_band + sex + 
                             rurality_classification + 
                             flu_vaccination +
                             offset(log(time_flu_mortality)),
                           data = df_input, family = poisson)
  flu_mortality_ses_output <- tidy(flu_mortality_ses)
  
  if (codelist_type == "sensitive") {
    #overall_resp primary by ses
    overall_resp_mild_ses <- glm(overall_resp_primary_inf ~ imd_quintile + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   flu_vaccination +
                                   offset(log(time_overall_resp_primary)),
                                 data = df_input, family = poisson)
    overall_resp_mild_ses_output <- tidy(overall_resp_mild_ses)
    
    #overall_resp secondary by ses
    overall_resp_severe_ses <- glm(overall_resp_secondary_inf ~ imd_quintile + 
                                     age_band + sex + 
                                     rurality_classification + 
                                     flu_vaccination +
                                     offset(log(time_overall_resp_secondary)),
                                   data = df_input, family = poisson)
    overall_resp_severe_ses_output <- tidy(overall_resp_severe_ses)
    
    #overall_resp mortality by ses
    overall_resp_mortality_ses <- glm(overall_resp_mortality ~ imd_quintile + 
                                        age_band + sex + 
                                        rurality_classification + 
                                        flu_vaccination +
                                        offset(log(time_overall_resp_mortality)),
                                      data = df_input, family = poisson)
    overall_resp_mortality_ses_output <- tidy(overall_resp_mortality_ses)
  }
 
  #all cause mortality by ses
  all_cause_mortality_ses <- glm(all_cause_mortality ~ imd_quintile + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   flu_vaccination +
                                   offset(log(time_all_cause_mortality)),
                                 data = df_input, family = poisson)
  all_cause_mortality_ses_output <- tidy(all_cause_mortality_ses)
  
  ##Finally looking at household size/composition
  
  #rsv primary by household composition
  rsv_mild_hh_comp <- glm(rsv_primary_inf ~ composition_category + 
                        age_band + sex + 
                        rurality_classification + 
                        flu_vaccination +
                        offset(log(time_rsv_primary)),
                      data = df_input, family = poisson)
  rsv_mild_hh_comp_output <- tidy(rsv_mild_hh_comp)
  
  #rsv secondary by household composition
  rsv_severe_hh_comp <- glm(rsv_secondary_inf ~ composition_category + 
                          age_band + sex + 
                          rurality_classification + 
                          flu_vaccination +
                          offset(log(time_rsv_secondary)),
                        data = df_input, family = poisson)
  rsv_severe_hh_comp_output <- tidy(rsv_severe_hh_comp)
  
  #rsv mortality by household composition
  rsv_mortality_hh_comp <- glm(rsv_mortality ~ composition_category + 
                             age_band + sex + 
                             rurality_classification + 
                             flu_vaccination +
                             offset(log(time_rsv_mortality)),
                           data = df_input, family = poisson)
  rsv_mortality_hh_comp_output <- tidy(rsv_mortality_hh_comp)
  
  #flu primary by household composition
  flu_mild_hh_comp <- glm(flu_primary_inf ~ composition_category + 
                        age_band + sex + 
                        rurality_classification + 
                        flu_vaccination +
                        offset(log(time_flu_primary)),
                      data = df_input, family = poisson)
  flu_mild_hh_comp_output <- tidy(flu_mild_hh_comp)
  
  #flu secondary by household composition
  flu_severe_hh_comp <- glm(flu_secondary_inf ~ composition_category + 
                          age_band + sex + 
                          rurality_classification + 
                          flu_vaccination +
                          offset(log(time_flu_secondary)),
                        data = df_input, family = poisson)
  flu_severe_hh_comp_output <- tidy(flu_severe_hh_comp)
  
  #flu mortality by household composition
  flu_mortality_hh_comp <- glm(flu_mortality ~ composition_category + 
                             age_band + sex + 
                             rurality_classification + 
                             flu_vaccination +
                             offset(log(time_flu_mortality)),
                           data = df_input, family = poisson)
  flu_mortality_hh_comp_output <- tidy(flu_mortality_hh_comp)
  
  if (codelist_type == "sensitive") {
    #overall_resp primary by household composition
    overall_resp_mild_hh_comp <- glm(overall_resp_primary_inf ~ composition_category + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   flu_vaccination +
                                   offset(log(time_overall_resp_primary)),
                                 data = df_input, family = poisson)
    overall_resp_mild_hh_comp_output <- tidy(overall_resp_mild_hh_comp)
    
    #overall_resp secondary by household composition
    overall_resp_severe_hh_comp <- glm(overall_resp_secondary_inf ~ composition_category + 
                                     age_band + sex + 
                                     rurality_classification + 
                                     flu_vaccination +
                                     offset(log(time_overall_resp_secondary)),
                                   data = df_input, family = poisson)
    overall_resp_severe_hh_comp_output <- tidy(overall_resp_severe_hh_comp)
    
    #overall_resp mortality by household composition
    overall_resp_mortality_hh_comp <- glm(overall_resp_mortality ~ composition_category + 
                                        age_band + sex + 
                                        rurality_classification + 
                                        flu_vaccination +
                                        offset(log(time_overall_resp_mortality)),
                                      data = df_input, family = poisson)
    overall_resp_mortality_hh_comp_output <- tidy(overall_resp_mortality_hh_comp)
  }
  
  #all cause mortality by household composition
  all_cause_mortality_hh_comp <- glm(all_cause_mortality ~ composition_category + 
                                   age_band + sex + 
                                   rurality_classification + 
                                   flu_vaccination +
                                   offset(log(time_all_cause_mortality)),
                                 data = df_input, family = poisson)
  all_cause_mortality_hh_comp_output <- tidy(all_cause_mortality_hh_comp)
  
  if (study_start_date >= covid_season_min) {
    ##Use Poisson regression for outcomes looking first at ethnicity 
    
    #rsv primary by ethnicity
    rsv_mild_ethnicity <- glm(rsv_primary_inf ~ latest_ethnicity_group +
                                age_band + sex + 
                                rurality_classification + 
                                flu_vaccination +
                                covid_vaccination_count +
                                offset(log(time_rsv_primary)), 
                              data = df_input, family = poisson)
    rsv_mild_ethnicity_output <- tidy(rsv_mild_ethnicity)
    
    #rsv secondary by ethnicity
    rsv_severe_ethnicity <- glm(rsv_secondary_inf ~ latest_ethnicity_group +
                                  age_band + sex + 
                                  rurality_classification + 
                                  flu_vaccination +
                                  covid_vaccination_count +
                                  offset(log(time_rsv_secondary)),
                                data = df_input, family = poisson)
    rsv_severe_ethnicity_output <- tidy(rsv_severe_ethnicity)
    
    #rsv mortality by ethnicity
    rsv_mortality_ethnicity <- glm(rsv_mortality ~ latest_ethnicity_group + 
                                     age_band + sex + 
                                     rurality_classification + 
                                     flu_vaccination +
                                     covid_vaccination_count +
                                     offset(log(time_rsv_mortality)),
                                   data = df_input, family = poisson)
    rsv_mortality_ethnicity_output <- tidy(rsv_mortality_ethnicity)
    
    #flu primary by ethnicity
    flu_mild_ethnicity <- glm(flu_primary_inf ~ latest_ethnicity_group + 
                                age_band + sex + 
                                rurality_classification + 
                                flu_vaccination +
                                covid_vaccination_count +
                                offset(log(time_flu_primary)),
                              data = df_input, family = poisson)
    flu_mild_ethnicity_output <- tidy(flu_mild_ethnicity)
    
    #flu secondary by ethnicity
    flu_severe_ethnicity <- glm(flu_secondary_inf ~ latest_ethnicity_group + 
                                  age_band + sex + 
                                  rurality_classification + 
                                  flu_vaccination +
                                  covid_vaccination_count +
                                  offset(log(time_flu_secondary)),
                                data = df_input, family = poisson)
    flu_severe_ethnicity_output <- tidy(flu_severe_ethnicity)
    
    #flu mortality by ethnicity
    flu_mortality_ethnicity <- glm(flu_mortality ~ latest_ethnicity_group + 
                                     age_band + sex + 
                                     rurality_classification + 
                                     flu_vaccination +
                                     covid_vaccination_count +
                                     offset(log(time_flu_mortality)),
                                   data = df_input, family = poisson)
    flu_mortality_ethnicity_output <- tidy(flu_mortality_ethnicity)
    
    covid_mild_ethnicity <- glm(covid_primary_inf ~ latest_ethnicity_group + 
                                  age_band + sex + 
                                  rurality_classification +
                                  flu_vaccination +
                                  covid_vaccination_count +
                                  offset(log(time_covid_primary)),
                                data = df_input, family = poisson)
    covid_mild_ethnicity_output <- tidy(covid_mild_ethnicity)
    
    #covid secondary by ethnicity
    covid_severe_ethnicity <- glm(covid_secondary_inf ~ latest_ethnicity_group + 
                                    age_band + sex + 
                                    rurality_classification + 
                                    flu_vaccination +
                                    covid_vaccination_count +
                                    offset(log(time_covid_secondary)),
                                  data = df_input, family = poisson)
    covid_severe_ethnicity_output <- tidy(covid_severe_ethnicity)
    
    #covid mortality by ethnicity
    covid_mortality_ethnicity <- glm(covid_mortality ~ latest_ethnicity_group + 
                                      age_band + sex + 
                                      rurality_classification + 
                                      flu_vaccination +
                                      covid_vaccination_count +
                                      offset(log(time_covid_mortality)),
                                    data = df_input, family = poisson)
    covid_mortality_ethnicity_output <- tidy(covid_mortality_ethnicity)
    
    if (codelist_type == "sensitive") {
      #overall_resp primary by ethnicity
      overall_resp_mild_ethnicity <- glm(overall_resp_primary_inf ~ latest_ethnicity_group + 
                                           age_band + sex + 
                                           rurality_classification + 
                                           flu_vaccination +
                                           covid_vaccination_count +
                                           offset(log(time_overall_resp_primary)),
                                         data = df_input, family = poisson)
      overall_resp_mild_ethnicity_output <- tidy(overall_resp_mild_ethnicity)
      
      #overall_resp secondary by ethnicity
      overall_resp_severe_ethnicity <- glm(overall_resp_secondary_inf ~ latest_ethnicity_group + 
                                             age_band + sex + 
                                             rurality_classification + 
                                             flu_vaccination +
                                             covid_vaccination_count +
                                             offset(log(time_overall_resp_secondary)),
                                           data = df_input, family = poisson)
      overall_resp_severe_ethnicity_output <- tidy(overall_resp_severe_ethnicity)
      
      #overall_resp mortality by ethnicity
      overall_resp_mortality_ethnicity <- glm(overall_resp_mortality ~ latest_ethnicity_group + 
                                                age_band + sex + 
                                                rurality_classification + 
                                                flu_vaccination +
                                                covid_vaccination_count +
                                                offset(log(time_overall_resp_mortality)),
                                              data = df_input, family = poisson)
      overall_resp_mortality_ethnicity_output <- tidy(overall_resp_mortality_ethnicity)
    }
    
    #all cause mortality by ethnicity
    all_cause_mortality_ethnicity <- glm(all_cause_mortality ~ latest_ethnicity_group + 
                                           age_band + sex + 
                                           rurality_classification + 
                                           flu_vaccination +
                                           covid_vaccination_count +
                                           offset(log(time_all_cause_mortality)),
                                         data = df_input, family = poisson)
    all_cause_mortality_ethnicity_output <- tidy(all_cause_mortality_ethnicity)
    
    ##Now looking at disparity 
    
    #rsv primary by ses
    rsv_mild_ses <- glm(rsv_primary_inf ~ imd_quintile + 
                          age_band + sex + 
                          rurality_classification + 
                          flu_vaccination +
                          covid_vaccination_count +
                          offset(log(time_rsv_primary)),
                        data = df_input, family = poisson)
    rsv_mild_ses_output <- tidy(rsv_mild_ses)
    
    #rsv secondary by ses
    rsv_severe_ses <- glm(rsv_secondary_inf ~ imd_quintile + 
                            age_band + sex + 
                            rurality_classification +
                            flu_vaccination +
                            covid_vaccination_count +
                            offset(log(time_rsv_secondary)),
                          data = df_input, family = poisson)
    rsv_severe_ses_output <- tidy(rsv_severe_ses)
    
    #rsv mortality by ses
    rsv_mortality_ses <- glm(rsv_mortality ~ imd_quintile + 
                               age_band + sex + 
                               rurality_classification + 
                               flu_vaccination +
                               covid_vaccination_count +
                               offset(log(time_rsv_mortality)),
                             data = df_input, family = poisson)
    rsv_severe_ses_output <- tidy(rsv_mortality_ses)
    
    #flu primary by ses
    flu_mild_ses <- glm(flu_primary_inf ~ imd_quintile + 
                          age_band + sex + 
                          rurality_classification + 
                          flu_vaccination +
                          covid_vaccination_count +
                          offset(log(time_flu_primary)),
                        data = df_input, family = poisson)
    flu_mild_ses_output <- tidy(flu_mild_ses)
    
    #flu secondary by ses
    flu_severe_ses <- glm(flu_secondary_inf ~ imd_quintile + 
                            age_band + sex + 
                            rurality_classification + 
                            flu_vaccination +
                            covid_vaccination_count +
                            offset(log(time_flu_secondary)),
                          data = df_input, family = poisson)
    flu_severe_ses_output <- tidy(flu_severe_ses)
    
    #flu mortality by ses
    flu_mortality_ses <- glm(flu_mortality ~ imd_quintile + 
                               age_band + sex + 
                               rurality_classification + 
                               flu_vaccination +
                               covid_vaccination_count +
                               offset(log(time_flu_mortality)),
                             data = df_input, family = poisson)
    flu_severe_mild_ses_output <- tidy(flu_mortality_ses)

    #covid primary by ses
    covid_mild_ses <- glm(covid_primary_inf ~ imd_quintile + 
                            age_band + sex + 
                            rurality_classification + 
                            flu_vaccination +
                            covid_vaccination_count +
                            offset(log(time_covid_primary)),
                          data = df_input, family = poisson)
    covid_mild_ses_output <- tidy(covid_mild_ses)
   
    #covid secondary by ses
    covid_severe_ses <- glm(covid_secondary_inf ~ imd_quintile + 
                              age_band + sex + 
                              rurality_classification + 
                              flu_vaccination +
                              covid_vaccination_count +
                              offset(log(time_covid_secondary)),
                            data = df_input, family = poisson)
    covid_severe_ses_output <- tidy(covid_severe_ses)
   
    #covid mortality by ses
    covid_mortality_ses <- glm(covid_mortality ~ imd_quintile + 
                                  age_band + sex + 
                                  rurality_classification + 
                                  flu_vaccination +
                                  covid_vaccination_count +
                                  offset(log(time_covid_mortality)),
                                data = df_input, family = poisson)
    covid_mortality_ses_output <- tidy(covid_mortality_ses)
    
    if (codelist_type == "sensitive") {
      #overall_resp primary by ses
      overall_resp_mild_ses <- glm(overall_resp_primary_inf ~ imd_quintile + 
                                     age_band + sex + 
                                     rurality_classification +
                                     flu_vaccination +
                                     covid_vaccination_count +
                                     offset(log(time_overall_resp_primary)),
                                   data = df_input, family = poisson)
      overall_resp_mild_ses_output <- tidy(overall_resp_mild_ses)
      
      #overall_resp secondary by ses
      overall_resp_severe_ses <- glm(overall_resp_secondary_inf ~ imd_quintile + 
                                       age_band + sex + 
                                       rurality_classification + 
                                       flu_vaccination +
                                       covid_vaccination_count +
                                       offset(log(time_overall_resp_secondary)),
                                     data = df_input, family = poisson)
      overall_resp_severe_ses_output <- tidy(overall_resp_severe_ses)
      
      #overall_resp mortality by ses
      overall_resp_mortality_ses <- glm(overall_resp_mortality ~ imd_quintile + 
                                          age_band + sex + 
                                          rurality_classification + 
                                          flu_vaccination +
                                          covid_vaccination_count +
                                          offset(log(time_overall_resp_mortality)),
                                        data = df_input, family = poisson)
      overall_resp_mortality_ses_output <- tidy(overall_resp_mortality_ses)
    }
    
    #all cause mortality by ses
    all_cause_mortality_ses <- glm(all_cause_mortality ~ imd_quintile + 
                                     age_band + sex + 
                                     rurality_classification + 
                                     flu_vaccination +
                                     covid_vaccination_count +
                                     offset(log(time_all_cause_mortality)),
                                   data = df_input, family = poisson)
    all_cause_mortality_ses_output <- tidy(all_cause_mortality_ses)
    
    ##Finally looking at household size/composition
    
    #rsv primary by household composition
    rsv_mild_hh_comp <- glm(rsv_primary_inf ~ composition_category + 
                          age_band + sex + 
                          rurality_classification + 
                          flu_vaccination +
                          covid_vaccination_count +
                          offset(log(time_rsv_primary)),
                        data = df_input, family = poisson)
    rsv_mild_hh_comp_output <- tidy(rsv_mild_hh_comp)
    
    #rsv secondary by household composition
    rsv_severe_hh_comp <- glm(rsv_secondary_inf ~ composition_category + 
                            age_band + sex + 
                            rurality_classification +
                            flu_vaccination +
                            covid_vaccination_count +
                            offset(log(time_rsv_secondary)),
                          data = df_input, family = poisson)
    rsv_severe_hh_comp_output <- tidy(rsv_severe_hh_comp)
    
    #rsv mortality by household composition
    rsv_mortality_hh_comp <- glm(rsv_mortality ~ composition_category + 
                               age_band + sex + 
                               rurality_classification + 
                               flu_vaccination +
                               covid_vaccination_count +
                               offset(log(time_rsv_mortality)),
                             data = df_input, family = poisson)
    rsv_severe_hh_comp_output <- tidy(rsv_mortality_hh_comp)
    
    #flu primary by household composition
    flu_mild_hh_comp <- glm(flu_primary_inf ~ composition_category + 
                          age_band + sex + 
                          rurality_classification + 
                          flu_vaccination +
                          covid_vaccination_count +
                          offset(log(time_flu_primary)),
                        data = df_input, family = poisson)
    flu_mild_hh_comp_output <- tidy(flu_mild_hh_comp)
    
    #flu secondary by household composition
    flu_severe_hh_comp <- glm(flu_secondary_inf ~ composition_category + 
                            age_band + sex + 
                            rurality_classification + 
                            flu_vaccination +
                            covid_vaccination_count +
                            offset(log(time_flu_secondary)),
                          data = df_input, family = poisson)
    flu_severe_hh_comp_output <- tidy(flu_severe_hh_comp)
    
    #flu mortality by household composition
    flu_mortality_hh_comp <- glm(flu_mortality ~ composition_category + 
                               age_band + sex + 
                               rurality_classification + 
                               flu_vaccination +
                               covid_vaccination_count +
                               offset(log(time_flu_mortality)),
                             data = df_input, family = poisson)
    flu_severe_mild_hh_comp_output <- tidy(flu_mortality_hh_comp)
    
    #covid primary by household composition
    covid_mild_hh_comp <- glm(covid_primary_inf ~ composition_category + 
                            age_band + sex + 
                            rurality_classification + 
                            flu_vaccination +
                            covid_vaccination_count +
                            offset(log(time_covid_primary)),
                          data = df_input, family = poisson)
    covid_mild_hh_comp_output <- tidy(covid_mild_hh_comp)
    
    #covid secondary by household composition
    covid_severe_hh_comp <- glm(covid_secondary_inf ~ composition_category + 
                              age_band + sex + 
                              rurality_classification + 
                              flu_vaccination +
                              covid_vaccination_count +
                              offset(log(time_covid_secondary)),
                            data = df_input, family = poisson)
    covid_severe_hh_comp_output <- tidy(covid_severe_hh_comp)
    
    #covid mortality by household composition
    covid_mortality_hh_comp <- glm(covid_mortality ~ composition_category + 
                                 age_band + sex + 
                                 rurality_classification + 
                                 flu_vaccination +
                                 covid_vaccination_count +
                                 offset(log(time_covid_mortality)),
                               data = df_input, family = poisson)
    covid_mortality_hh_comp_output <- tidy(covid_mortality_hh_comp)
    
    if (codelist_type == "sensitive") {
      #overall_resp primary by household composition
      overall_resp_mild_hh_comp <- glm(overall_resp_primary_inf ~ composition_category + 
                                     age_band + sex + 
                                     rurality_classification +
                                     flu_vaccination +
                                     covid_vaccination_count +
                                     offset(log(time_overall_resp_primary)),
                                   data = df_input, family = poisson)
      overall_resp_mild_hh_comp_output <- tidy(overall_resp_mild_hh_comp)
      
      #overall_resp secondary by household composition
      overall_resp_severe_hh_comp <- glm(overall_resp_secondary_inf ~ composition_category + 
                                       age_band + sex + 
                                       rurality_classification + 
                                       flu_vaccination +
                                       covid_vaccination_count +
                                       offset(log(time_overall_resp_secondary)),
                                     data = df_input, family = poisson)
      overall_resp_severe_hh_comp_output <- tidy(overall_resp_severe_hh_comp)
      
      #overall_resp mortality by household composition
      overall_resp_mortality_hh_comp <- glm(overall_resp_mortality ~ composition_category + 
                                          age_band + sex + 
                                          rurality_classification + 
                                          flu_vaccination +
                                          covid_vaccination_count +
                                          offset(log(time_overall_resp_mortality)),
                                        data = df_input, family = poisson)
      overall_resp_mortality_hh_comp_output <- tidy(overall_resp_mortality_hh_comp)
    }
    
    #all cause mortality by household composition
    all_cause_mortality_hh_comp <- glm(all_cause_mortality ~ composition_category + 
                                     age_band + sex + 
                                     rurality_classification + 
                                     flu_vaccination +
                                     covid_vaccination_count +
                                     offset(log(time_all_cause_mortality)),
                                   data = df_input, family = poisson)
    all_cause_mortality_hh_comp_output <- tidy(all_cause_mortality_hh_comp)
  }
}

#define a vector of names for the model outputs
if (study_start_date < covid_season_min) {
  model_names <- c("Mild RSV by Ethnicity", "Severe RSV by Ethnicity",
                   "RSV Mortality By Ethnicity", "Mild Influenza by Ethnicity",
                   "Severe Influenza by Ethnicity", "Influenza Mortality by Ethnicity",
                   "All Cause Mortality by Ethnicity", "Mild RSV by IMD Quintile",
                   "Severe RSV by IMD Quintile", "RSV Mortality by IMD Quintile",
                   "Mild Influenza by IMD Quintile", "Severe Influenza by IMD Quintile",
                   "Influenza Mortality by IMD Quintile", "All Cause Mortlality by IMD Quintile",
                   "Mild RSV by Household Composition", "Severe RSV by Household Composition",
                   "RSV Mortality by Household Composition", "Mild Influenza by Household Composition",
                   "Severe Influenza by Household Composition", "Influenza Mortality by Household Composition",
                   "All Cause Mortality by Household Composition")
} else if (codelist_type == "sensitive") {
  model_names <- c("Mild RSV by Ethnicity", "Severe RSV by Ethnicity",
                   "RSV Mortality By Ethnicity", "Mild Influenza by Ethnicity",
                   "Severe Influenza by Ethnicity", "Influenza Mortality by Ethnicity",
                   "Mild Overall Respiratory Virus by Ethnicity", 
                   "Severe Overall Respiratory Virus by Ethnicity",
                   "Overall Respiratory Virus Mortality by Ethnicity",
                   "All Cause Mortality by Ethnicity", "Mild RSV by IMD Quintile",
                   "Severe RSV by IMD Quintile", "RSV Mortality by IMD Quintile",
                   "Mild Influenza by IMD Quintile", "Severe Influenza by IMD Quintile",
                   "Influenza Mortality by IMD Quintile", 
                   "Mild Overall Respiratory Virus by IMD Quintile",
                   "Severe Overall Respiratory Virus by IMD Quintile",
                   "Overall Respiratory Virus Mortality by IMD Quintile",
                   "All Cause Mortlality by IMD Quintile",
                   "Mild RSV by Household Composition", 
                   "Severe RSV by Household Composition", 
                   "RSV Mortality by Household Composition",
                   "Mild Influenza by Household Composition", 
                   "Severe Influenza by Household Composition", 
                   "Influenza Mortality by Household Composition",
                   "Mild Overall Respiratory Virus by Household Composition",
                   "Severe Overall Respiratory Virus by Household Composition",
                   "Overall Respiratory Virus Mortality by Household Composition",
                   "All Cause Mortality by Household Composition")
} else if (study_start_date >= covid_season_min) {
  model_names <- c("Mild RSV by Ethnicity", "Severe RSV by Ethnicity",
                   "RSV Mortality By Ethnicity", "Mild Influenza by Ethnicity",
                   "Severe Influenza by Ethnicity", "Influenza Mortality by Ethnicity",
                   "Mild COVID-19 by Ethnicity", "Severe COVID-19 by Ethnicity",
                   "COVID-19 Mortality by Ethnicity", "All Cause Mortality by Ethnicity", 
                   "Mild RSV by IMD Quintile", "Severe RSV by IMD Quintile", 
                   "RSV Mortality by IMD Quintile", "Mild Influenza by IMD Quintile", 
                   "Severe Influenza by IMD Quintile", "Influenza Mortality by IMD Quintile", 
                   "Mild COVID-19 by IMD Quintile", "Severe COVID-19 by IMD Quintile",
                   "COVID-19 Mortality by IMD Quintile", "All Cause Mortlality by IMD Quintile",
                   "Mild RSV by Household Composition", "Severe RSV by Household Composition",
                   "RSV Mortality by Household Composition", "Mild Influenza by Household Composition",
                   "Severe Influenza by Household Composition", "Influenza Mortality by Household Composition",
                   "Mild COVID-19 by Household Composition", "Severe COVID-19 by Household Composition",
                   "COVID-19 Mortality by Household Composition", "All Cause Mortality by Household Composition")
} else {
  model_names <- c("Mild RSV by Ethnicity", "Severe RSV by Ethnicity",
                   "RSV Mortality By Ethnicity", "Mild Influenza by Ethnicity",
                   "Severe Influenza by Ethnicity", "Influenza Mortality by Ethnicity",
                   "Mild COVID-19 by Ethnicity", "Severe COVID-19 by Ethnicity",
                   "COVID-19 Mortality by Ethnicity", 
                   "Mild Overall Respiratory Virus by Ethnicity",
                   "Severe Overall Respiratory Virus by Ethnicity",
                   "Overall Respiratory Virus Mortality by Ethnicity",
                   "All Cause Mortality by Ethnicity", 
                   "Mild RSV by IMD Quintile", "Severe RSV by IMD Quintile", 
                   "RSV Mortality by IMD Quintile", "Mild Influenza by IMD Quintile", 
                   "Severe Influenza by IMD Quintile", "Influenza Mortality by IMD Quintile", 
                   "Mild COVID-19 by IMD Quintile", "Severe COVID-19 by IMD Quintile",
                   "COVID-19 Mortality by IMD Quintile", 
                   "Mild Overall Respiratory Virus by IMD Quintile",
                   "Severe Overall Respiratory Virus by IMD Quintile",
                   "Overall Respiratory Virus Mortality by IMD Quintile",
                   "All Cause Mortlality by IMD Quintile",
                   "Mild RSV by Household Composition", 
                   "Severe RSV by Household Composition", 
                   "RSV Mortality by Household Composition",
                   "Mild Influenza by Household Composition", 
                   "Severe Influenza by Household Composition", 
                   "Influenza Mortality by Household Composition",
                   "Mild Overall Respiratory Virus by Household Composition",
                   "Severe Overall Respiratory Virus by Household Composition",
                   "Overall Respiratory Virus Mortality by Household Composition",
                   "Mild COVID`-19 by Household Composition", 
                   "Severe COVID-19 by Household Composition",
                   "COVID-19 Mortality by Household Composition",
                   "All Cause Mortality by Household Composition")
}

#create the model outputs list
model_outputs_list <- list(rsv_mild_ethnicity_output, rsv_severe_ethnicity_output,
                           rsv_mortality_ethnicity_output, flu_mild_ethnicity_output,
                           flu_severe_ethnicity_output, flu_mortality_ethnicity_output,
                           all_cause_mortality_ethnicity_output, rsv_mild_ses_output,
                           rsv_severe_ses_output, rsv_mortality_ses_output,
                           flu_mild_ses_output, flu_severe_ses_output,
                           flu_mortality_ses_output, all_cause_mortality_ses_output,
                           rsv_mild_hh_comp_output, rsv_severe_hh_comp_output,
                           rsv_mortality_hh_comp_output, flu_mild_hh_comp_output,
                           flu_severe_hh_comp_output, flu_mortality_hh_comp_output,
                           all_cause_mortality_hh_comp_output)

#adjust the model outputs list based on the conditions
if (study_start_date >= covid_season_min) {
  model_outputs_list <- c(model_outputs_list, list(covid_mild_ethnicity_output,
                                                   covid_severe_ethnicity_output,
                                                   covid_mortality_ethnicity_output,
                                                   covid_mild_ses_output,
                                                   covid_severe_ses_output,
                                                   covid_mortality_ses_output,
                                                   covid_mild_hh_comp_output,
                                                   covid_severe_hh_comp_output,
                                                   covid_mortality_hh_comp_output))
}
if (codelist_type == "sensitive") {
  model_outputs_list <- c(model_outputs_list, list(overall_resp_mild_ethnicity_output,
                                                   overall_resp_severe_ethnicity_output,
                                                   overall_resp_mortality_ethnicity_output,
                                                   overall_resp_mild_ses_output,
                                                   overall_resp_severe_ses_output,
                                                   overall_resp_mortality_ses_output,
                                                   overall_resp_mild_hh_comp_output,
                                                   overall_resp_severe_hh_comp_output,
                                                   overall_resp_mortality_hh_comp_output))
}
if (study_start_date >= covid_season_min & codelist_type == "sensitive") {
  model_outputs_list <- c(model_outputs_list, list(covid_mild_ethnicity_output,
                                                   covid_severe_ethnicity_output,
                                                   covid_mortality_ethnicity_output,
                                                   overall_resp_mild_ethnicity_output,
                                                   overall_resp_severe_ethnicity_output,
                                                   overall_resp_mortality_ethnicity_output,
                                                   covid_mild_ses_output,
                                                   covid_severe_ses_output,
                                                   covid_mortality_ses_output,
                                                   overall_resp_mild_ses_output,
                                                   overall_resp_severe_ses_output,
                                                   overall_resp_mortality_ses_output,
                                                   covid_mild_hh_comp_output,
                                                   covid_severe_hh_comp_output,
                                                   covid_mortality_hh_comp_output,
                                                   overall_resp_mild_hh_comp_output,
                                                   overall_resp_severe_hh_comp_output,
                                                   overall_resp_mortality_hh_comp_output))
}

#bind model outputs together and add a column with the corresponding names
model_outputs <- do.call(rbind, lapply(seq_along(model_outputs_list), function(i) {
  cbind(model_name = model_names[i], model_outputs_list[[i]])
}))

## create output directories ----
fs::dir_create(here("output", "results", "models"))

#save model output 
if (length(args) == 0) {
  model_outputs %>%
    write_csv(file = paste0(here::here("output", "results", "models"), "/", 
                            "model_outputs_", cohort, "_", year(study_start_date), 
                            "_", year(study_end_date), "_", codelist_type, ".csv"))
}  else{
  model_outputs %>%
    write_csv(path = paste0(here::here("output", "results", "models"), "/", 
                            "model_outputs_", cohort, "_", year(study_start_date),
                            "_", year(study_end_date), "_", codelist_type, ".csv"))
}