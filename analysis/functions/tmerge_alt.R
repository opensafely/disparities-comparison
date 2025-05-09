library(tidyverse)
library(survival)
library(here)

## create output directories ----
fs::dir_create(here::here("analysis", "functions"))

tmerge_alt <- function(df, outcome) {
  
  tstop <- case_when(
    outcome == "flu_primary" ~ "flu_primary_inf_date",
    outcome == "flu_secondary" ~ "flu_primary_inf_date",
    outcome == "covid_primary" ~ "covid_primary_inf_date",
    outcome == "covid_secondary" ~ "covid_primary_inf_date"
  )
  
  outcome_event <- case_when(
    outcome == "flu_primary" ~ "flu_primary_date",
    outcome == "flu_secondary" ~ "flu_secondary_date",
    outcome == "covid_primary" ~ "covid_primary_date",
    outcome == "covid_secondary" ~ "covid_secondary_date"
  )
  
  vax_status <- case_when(
    outcome == "flu_primary" ~ "flu_vaccination_immunity_date",
    outcome == "flu_secondary" ~ "flu_vaccination_immunity_date",
    outcome == "covid_primary" ~ "covid_vaccination_immunity_date",
    outcome == "covid_secondary" ~ "covid_vaccination_immunity_date"
  )
  
  df_vacc <- df %>%
    select(patient_id, patient_index_date, tstop = ensym(tstop),
           outcome_event = ensym(outcome_event),
           vax_status = ensym(vax_status))
  
  # reformat to one row per vax_status using survival::tmerge
  # there is a new row created whenever a person's covid or flu vaccination status changes
  # end of follow-up occurs at end of season or occurrence of an outcome, whichever is first
  dat_1rpe_tmerge <-
    tmerge(
      df,
      df_vacc,
      id = patient_id,
      tstart = patient_index_date,
      tstop = tstop,
      vax_status = tdc(vax_status),
      outcome_event = event(outcome_event)
    ) %>%
    mutate(
      persontime = tstop - tstart + 1,
      persontime_years = time_length(persontime, "years")
    )
  
  #return
  return(dat_1rpe_tmerge)
  
}
