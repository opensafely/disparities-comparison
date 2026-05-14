library(here)
library(tidyverse)
library(arrow)
library(rlang)

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "adults"
} else {
  cohort <- args[[1]]
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

#import redaction function
source(here::here("analysis", "functions", "redaction.R"))

if (study_start_date < covid_season_min) {
  vars <- c(
    "patient_id", "rsv_primary_date", "rsv_secondary_date",
    "flu_primary_date", "flu_secondary_date", "bucket_date",
    "broad_bucket_date"
  )
} else {
  vars <- c(
    "patient_id", "rsv_primary_date", "rsv_secondary_date",
    "flu_primary_date", "flu_secondary_date", "covid_primary_date",
    "covid_secondary_date", "bucket_date", "broad_bucket_date"
  )
}

df_input_spec <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             "specific", "_", "primary",".arrow"))) %>% 
  select(all_of(vars))

df_input_sens <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             "sensitive", "_", "primary",".arrow"))) %>% 
  select(all_of(vars))

#join by patient_id
df_input <- merge(
  df_input_spec, df_input_sens, by = "patient_id", suffixes = c("_spec", "_sens")
)

#create function to assess whether an outcome of interest (specific severe)
#occurs within 30 days of a primary care attendance
flag_population <- function(df, secondary_outcome, primary_cols, n_days = 30) {
  sec_q <- enquo(secondary_outcome)
  sec_date <- pull(df, !!sec_q)

  #build one logical vector per primary column:
  checks <- lapply(primary_cols, function(col) {
    d <- as.numeric(difftime(sec_date, df[[col]], units = "days"))
    coalesce(d < n_days, FALSE)  # NA -> FALSE
  })

  #TRUE if any primary date is within n_days
  Reduce(`|`, checks, init = rep(FALSE, nrow(df)))
}

# add a flag columns for the populations of interest
if (study_start_date < covid_season_min) {

  df_input <- df_input %>%
    mutate(
      rsv_pop = flag_population(
        df = .,
        secondary_outcome = rsv_secondary_date_spec,
        primary_cols = c("rsv_primary_date_sens", "flu_primary_date_sens",
                         "bucket_date_sens", "broad_bucket_date_sens"),
        n_days = 30
      ),
      flu_pop = flag_population(
        df = .,
        secondary_outcome = flu_secondary_date_spec,
        primary_cols = c("rsv_primary_date_sens", "flu_primary_date_sens",
                         "bucket_date_sens", "broad_bucket_date_sens"),
        n_days = 30
      ),
      sec_flag = (!is.na(rsv_secondary_date_spec)|!is.na(flu_secondary_date_spec))
    )
  
} else {

  df_input <- df_input %>%
    mutate(
      rsv_pop = flag_population(
        df = .,
        secondary_outcome = rsv_secondary_date_spec,
        primary_cols = c("rsv_primary_date_sens", "flu_primary_date_sens",
                         "covid_primary_date_sens", "bucket_date_sens",
                         "broad_bucket_date_sens"),
        n_days = 30
      ),
      flu_pop = flag_population(
        df = .,
        secondary_outcome = flu_secondary_date_spec,
        primary_cols = c("rsv_primary_date_sens", "flu_primary_date_sens",
                         "covid_primary_date_sens", "bucket_date_sens",
                         "broad_bucket_date_sens"),
        n_days = 30
      ),
      covid_pop = flag_population(
        df = .,
        secondary_outcome = covid_secondary_date_spec,
        primary_cols = c("rsv_primary_date_sens", "flu_primary_date_sens",
                         "covid_primary_date_sens", "bucket_date_sens",
                         "broad_bucket_date_sens"),
        n_days = 30
      ),
      sec_flag = (!is.na(rsv_secondary_date_spec)|!is.na(flu_secondary_date_spec)|!is.na(covid_secondary_date_spec))
    )

}

if (study_start_date < covid_season_min) {

  props <- df_input %>% 
    mutate(
      rsv_pop_sum = roundmid_any(sum(rsv_pop)),
      flu_pop_sum = roundmid_any(sum(flu_pop)),
      total_patients = roundmid_any(nrow(df_input)),
      total_patients_sec = roundmid_any(sum(sec_flag))
    ) %>% 
    select(c(flu_pop_sum, rsv_pop_sum, total_patients, total_patients_sec)) %>% 
    mutate(
      rsv_pop_prop_total = (rsv_pop_sum/total_patients)*100,
      rsv_pop_prop_sec = (rsv_pop_sum/total_patients_sec)*100,
      flu_pop_prop_total = (flu_pop_sum/total_patients)*100,
      flu_pop_prop_sec = (flu_pop_sum/total_patients_sec)*100
    ) %>% 
    select(c(
      rsv_pop_sum, flu_pop_sum, 
      total_patients, rsv_pop_prop_total, flu_pop_prop_total,
      total_patients_sec, rsv_pop_prop_sec, flu_pop_prop_sec
    ))

} else {

  props <- df_input %>% 
    mutate(
      rsv_pop_sum = roundmid_any(sum(rsv_pop)),
      flu_pop_sum = roundmid_any(sum(flu_pop)),
      covid_pop_sum = roundmid_any(sum(covid_pop)),
      total_patients = roundmid_any(nrow(df_input)),
      total_patients_sec = roundmid_any(sum(sec_flag))
    ) %>% 
    select(c(flu_pop_sum, rsv_pop_sum, covid_pop_sum, total_patients, total_patients_sec)) %>% 
    mutate(
      rsv_pop_prop_total = (rsv_pop_sum/total_patients)*100,
      rsv_pop_prop_sec = (rsv_pop_sum/total_patients_sec)*100,
      flu_pop_prop_total = (flu_pop_sum/total_patients)*100,
      flu_pop_prop_sec = (flu_pop_sum/total_patients_sec)*100,
      covid_pop_prop_total = (covid_pop_sum/total_patients)*100,
      covid_pop_prop_sec = (covid_pop_sum/total_patients_sec)*100
    ) %>% 
    select(c(
      rsv_pop_sum, flu_pop_sum, covid_pop_sum,
      total_patients, rsv_pop_prop_total, flu_pop_prop_total, covid_pop_prop_total,
      total_patients_sec, rsv_pop_prop_sec, flu_pop_prop_sec, covid_pop_prop_sec
    ))

}

props <- props %>% 
  unique() %>% 
  pivot_longer(cols = everything(), names_to = "cat", values_to = "count_or_prop")

## create output directories ----
fs::dir_create(here::here("output", "exploratory"))

#save
write_csv(props, file = here::here("output", "exploratory",
          paste0("internal_validation_population_sizes_", cohort,  "_",
          year(study_start_date), "_", year(study_end_date), ".csv")))

make_pop_flags <- function(df, pop_flag, secondary_date, window_days = 30,
                           include_covid = FALSE) {

  pop_q <- enquo(pop_flag)
  sec_q <- enquo(secondary_date)

  # sensitivity primaries
  fp_sens_q <- sym("flu_primary_date_sens")
  rp_sens_q <- sym("rsv_primary_date_sens")
  cp_sens_q <- sym("covid_primary_date_sens")
  bb_sens_q <- sym("broad_bucket_date_sens")

  # specificity primaries
  fp_spec_q <- sym("flu_primary_date_spec")
  rp_spec_q <- sym("rsv_primary_date_spec")
  cp_spec_q <- sym("covid_primary_date_spec")
  b_spec_q  <- sym("bucket_date_spec")

  within <- function(a, b) {
    # TRUE if b - a < window_days, with NA -> FALSE
    coalesce(as.numeric(difftime(b, a, units = "days")) < window_days, FALSE)
  }

  # infer index pathogen from pop flag, e.g. flu_pop -> flu
  pop_name <- as_name(pop_q)
  index_pathogen <- sub("_pop$", "", pop_name)

  # index primaries (used for "mild" split)
  ip_sens_q <- sym(paste0(index_pathogen, "_primary_date_sens"))
  ip_spec_q <- sym(paste0(index_pathogen, "_primary_date_spec"))

  # names for index-mild columns + tractable broad/bucket-only columns
  index_sens_mild_name <- paste0(index_pathogen, "_sens")
  index_spec_mild_name <- paste0(index_pathogen, "_spec")
  broad_only_name <- paste0("broad_sens")
  bucket_only_name <- paste0("bucket_spec")

  out <- df %>%
    filter(!!pop_q) %>%
    mutate(

      # --- SENS ---
      # index-mild depends on which population you're flagging (flu/rsv/covid)
      !!index_sens_mild_name := within(.data[[as_string(ip_sens_q)]], !!sec_q),

      flu_sens = if (include_covid) {
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          !within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          within(.data[[as_string(fp_sens_q)]], !!sec_q)
      } else {
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(fp_sens_q)]], !!sec_q)
      },

      rsv_sens = if (include_covid) {
        !within(.data[[as_string(fp_sens_q)]], !!sec_q) &
          !within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          within(.data[[as_string(rp_sens_q)]], !!sec_q)
      } else {
        !within(.data[[as_string(fp_sens_q)]], !!sec_q) &
          within(.data[[as_string(rp_sens_q)]], !!sec_q)
      },

      rsv_flu_sens = if (include_covid) {
        !within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          .data[[index_sens_mild_name]]
      } else {
        within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          .data[[index_sens_mild_name]]
      },

      !!broad_only_name :=
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
        (if (include_covid) !within(.data[[as_string(cp_sens_q)]], !!sec_q) else TRUE) &
        !.data[[index_sens_mild_name]] &
        within(.data[[as_string(bb_sens_q)]], !!sec_q),

      # covid-season-only SENS combinations
      rsv_covid_sens = if (include_covid)
        within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          !.data[[index_sens_mild_name]] else FALSE,
      flu_covid_sens = if (include_covid)
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          .data[[index_sens_mild_name]] else FALSE,
      covid_sens = if (include_covid)
        !within(.data[[as_string(fp_sens_q)]], !!sec_q) &
          !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) else FALSE,
      rsv_flu_covid_sens = if (include_covid)
        within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          .data[[index_sens_mild_name]] else FALSE,

      # --- SPEC ---
      !!index_spec_mild_name := within(.data[[as_string(ip_spec_q)]], !!sec_q),

      flu_spec = if (include_covid) {
        !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          !within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          within(.data[[as_string(fp_spec_q)]], !!sec_q)
      } else {
        !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(fp_spec_q)]], !!sec_q)
      },

      rsv_spec = if (include_covid) {
        !within(.data[[as_string(fp_spec_q)]], !!sec_q) &
          !within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          within(.data[[as_string(rp_spec_q)]], !!sec_q)
      } else {
        !within(.data[[as_string(fp_spec_q)]], !!sec_q) &
          within(.data[[as_string(rp_spec_q)]], !!sec_q)
      },

      rsv_flu_spec = if (include_covid) {
        !within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          .data[[index_spec_mild_name]]
      } else {
        within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          .data[[index_spec_mild_name]]
      },

      !!bucket_only_name :=
        !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
        (if (include_covid) !within(.data[[as_string(cp_spec_q)]], !!sec_q) else TRUE) &
        !.data[[index_spec_mild_name]] &
        within(.data[[as_string(b_spec_q)]], !!sec_q),

      # covid-season-only SPEC combinations
      rsv_covid_spec = if (include_covid)
        within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          !.data[[index_spec_mild_name]] else FALSE,
      flu_covid_spec = if (include_covid)
        !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          .data[[index_spec_mild_name]] else FALSE,
      covid_spec = if (include_covid)
        !within(.data[[as_string(fp_spec_q)]], !!sec_q) &
          !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(cp_spec_q)]], !!sec_q) else FALSE,
      rsv_flu_covid_spec = if (include_covid)
        within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          .data[[index_spec_mild_name]] else FALSE

    )

  # keep only the population flag + derived flags
  keep <- c(
    "patient_id",
    index_sens_mild_name,
    index_spec_mild_name,
    broad_only_name,
    bucket_only_name,
    "rsv_sens",
    "flu_sens",
    "covid_sens",
    "rsv_flu_sens",
    "rsv_covid_sens",
    "flu_covid_sens",
    "rsv_flu_covid_sens",
    "rsv_spec",
    "flu_spec",
    "covid_spec",
    "rsv_flu_spec",
    "rsv_covid_spec",
    "flu_covid_spec",
    "rsv_flu_covid_spec"
  )

  out <- out %>% select(any_of(keep))

  # filter out covid related flags in non-covid seasons
  if (!include_covid) {
    out <- out %>% select(-contains("covid"))
  }

  out %>%
    pivot_longer(
      cols = -patient_id,
      names_to = c("category", "stage"),
      names_pattern = "^(.*)_(sens|spec)$",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(names_from = stage, values_from = value) %>%
    group_by(patient_id) %>%
    summarise(
      sens_stage = dplyr::coalesce(category[which(sens)][1], "other"),
      spec_stage = dplyr::coalesce(category[which(spec)][1], "other"),
      .groups = "drop"
    ) %>%
    count(sens_stage, spec_stage, name = "n") %>%
    mutate(
      rounded = roundmid_any(n),
      pct = 100 * rounded / sum(rounded)
    ) %>% 
    select(-n) %>% 
    mutate(population = pop_name, .before = sens_stage)

}

#now apply function to get the different populations - rsv first
df_rsv <- make_pop_flags(
  df = df_input,
  pop_flag = rsv_pop,
  secondary_date = rsv_secondary_date_spec,
  window_days = 30,
  include_covid = (study_start_date >= covid_season_min)
) 

#now flu
df_flu <- make_pop_flags(
  df = df_input,
  pop_flag = flu_pop,
  secondary_date = flu_secondary_date_spec,
  window_days = 30,
  include_covid = (study_start_date >= covid_season_min)
)

#now covid
if (study_start_date >= covid_season_min) {

  df_covid <- make_pop_flags(
    df = df_input,
    pop_flag = covid_pop,
    secondary_date = covid_secondary_date_spec,
    window_days = 30,
    include_covid = TRUE
  )

}

sankey_sums <- bind_rows(df_rsv, df_flu)
if (exists("df_covid")) sankey_sums <- bind_rows(sankey_sums, df_covid)

write_csv(sankey_sums, file = here::here("output", "exploratory",
          paste0("internal_validation_combination_counts_", cohort, "_",
          year(study_start_date), "_", year(study_end_date), ".csv")))
