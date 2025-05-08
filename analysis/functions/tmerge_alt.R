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
    ) |>
    mutate(
      persontime = tstop - tstart + 1
    )
  
  # #  reformat to one row per vax_status using standard dplyr/tidyverse tools
  # dat_timevarying_info <-
  #   # create dataset with one row per person per day
  #   expand_grid(
  #     patient_id = df$patient_id, 
  #     date = seq(ymd(study_start_date), ymd(study_end_date), 1L)
  #   ) |>
  #   # join on dates that indicate the start of a row
  #   left_join(df |> transmute(patient_id, patient_index_date, start=1L), by=c("patient_id", date="patient_index_date")) |>
  #   full_join(df |> transmute(patient_id, flu_vaccination_immunity_date, flu_vax=1L), by=c("patient_id", date="flu_vaccination_immunity_date")) |>
  #   # join on dates that indicate the end of follow-up
  #   full_join(df |> transmute(patient_id, flu_primary_date, outcome=1L), by=c("patient_id", date="flu_primary_date")) |>
  #   full_join(df |> transmute(patient_id, flu_primary_inf_date, censor=1L), by=c("patient_id", date="flu_primary_inf_date")) |>
  #   filter(
  #     start | flu_vax | outcome | censor
  #   ) |>
  #   arrange(patient_id, date)
  # 
  # # join time-varying info onto non-time-varying info
  # dat_1rpe_dplyr <-
  #   df |>
  #   left_join(dat_timevarying_info, by="patient_id") |>
  #   group_by(patient_id) |>
  #   # create time-stamps and time-varying variables
  #   mutate(
  #     tstart = date,
  #     tstop = lead(date, 1),
  #     persontime = tstop - tstart + 1,
  #     flu_vax_status = 1-cumprod(1-replace_na(flu_vax,0L)),
  #     censor_status = 1-cumprod(1-replace_na(censor,0L)),
  #     outcome_event = replace_na(lead(outcome, 1), 0L),
  #   ) |>
  #   # remove rows after occurrence of censoring and before start date
  #   filter(
  #     censor_status==0,
  #     tstart >= patient_index_date
  #   )
  # 
  # 
  # # check equivalence
  # # should be a row of zeros
  # map2(
  #   dat_1rpe_dplyr |> select(patient_id, tstart, tstop, persontime, age, flu_vax_status, outcome_event),
  #   dat_1rpe_tmerge |> select(patient_id, tstart, tstop, persontime, age, flu_vax_status, outcome_event),
  #   setdiff
  # ) |> 
  #   map_int(length)
  
  return(dat_1rpe_tmerge)
  
}
