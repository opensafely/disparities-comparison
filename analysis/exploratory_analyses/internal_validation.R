# Internal validation of mild primary-care phenotypes against specific severe outcomes.
# For each virus population (RSV / flu / COVID), classifies patients by sensitive vs
# specific mild phenotype and outputs (1) population size tables and (2) sens/spec
# combination counts for Sankey diagrams.

library(here)
library(tidyverse)
library(arrow)
library(rlang)

## Study parameters ----
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  # defaults for interactive / local testing
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "infants"
} else {
  cohort <- args[[1]]
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
}
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

source(here::here("analysis", "functions", "redaction.R"))

## Load input data (specific + sensitive primaries merged) ----
# Columns needed depend on whether COVID is in scope for this season.
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

# Specific and sensitive phenotype extracts are separate arrow files; both are needed
# because validation compares mild sens vs mild spec classification per patient.
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

# One row per patient with _spec and _sens suffixes on overlapping columns.
df_input <- merge(
  df_input_spec, df_input_sens, by = "patient_id", suffixes = c("_spec", "_sens")
)

# Infant cohorts can have multiple index rows; keep the first episode only.
if (cohort %in% c("infants", "infants_subgroup")) {

  df_input <- df_input %>%
    group_by(patient_id) %>%
    slice_head() %>%
    ungroup()

}

## Define validation populations ----
# A patient is in e.g. rsv_pop if ANY sensitive mild primary lies within 30 days
# before their specific RSV severe (secondary) date. Severe dates are always specific.

flag_population <- function(df, secondary_outcome, primary_cols, n_days = 30) {
  sec_q <- enquo(secondary_outcome)
  sec_date <- pull(df, !!sec_q)

  checks <- lapply(primary_cols, function(col) {
    d <- as.numeric(difftime(sec_date, df[[col]], units = "days"))
    coalesce(d < n_days, FALSE)  # NA -> FALSE
  })

  # TRUE if any listed sensitive primary is within n_days of the severe date
  Reduce(`|`, checks, init = rep(FALSE, nrow(df)))
}

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
      # Denominator: patients with any specific severe outcome this season
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

## Population size table (counts + proportions + denominators) ----
make_population_table <- function(df, include_covid = FALSE) {

  pop_cols <- c("rsv_pop", "flu_pop")
  if (include_covid) pop_cols <- c(pop_cols, "covid_pop")

  totals <- df %>%
    summarise(
      across(all_of(pop_cols), ~ roundmid_any(sum(.x))),
      total_patients = roundmid_any(n()),
      total_patients_sec = roundmid_any(sum(sec_flag))
    )

  # One row per population × denominator, with separate count and pct columns.
  pop_rows <- totals %>%
    pivot_longer(
      cols = all_of(pop_cols),
      names_to = "population",
      values_to = "count"
    ) %>%
    mutate(
      total_patients_dn = totals$total_patients,
      total_patients_sec_dn = totals$total_patients_sec
    ) %>%
    pivot_longer(
      cols = c(total_patients_dn, total_patients_sec_dn),
      names_to = "denominator",
      values_to = "denominator_n",
      names_pattern = "(.*)_dn$"
    ) %>%
    mutate(pct = 100 * count / denominator_n) %>%
    select(population, denominator, denominator_n, count, pct)
}

props <- make_population_table(
  df = df_input,
  include_covid = (study_start_date >= covid_season_min)
)

fs::dir_create(here::here("output", "exploratory"))

write_csv(props, file = here::here("output", "exploratory",
          paste0("internal_validation_population_sizes_", cohort,  "_",
          year(study_start_date), "_", year(study_end_date), ".csv")))

## Classify patients by mild sens / mild spec phenotype ----
# Applied within each validation population (rsv_pop, flu_pop, covid_pop).
# Timing is always relative to that population's specific severe date.

make_pop_flags <- function(df, pop_flag, secondary_date, window_days = 30,
                           include_covid = FALSE) {

  pop_q <- enquo(pop_flag)
  sec_q <- enquo(secondary_date)

  # Sensitive primary dates
  fp_sens_q <- sym("flu_primary_date_sens")
  rp_sens_q <- sym("rsv_primary_date_sens")
  cp_sens_q <- sym("covid_primary_date_sens")
  bb_sens_q <- sym("broad_bucket_date_sens")
  b_sens_q  <- sym("bucket_date_sens")

  # Specific primary dates
  fp_spec_q <- sym("flu_primary_date_spec")
  rp_spec_q <- sym("rsv_primary_date_spec")
  cp_spec_q <- sym("covid_primary_date_spec")
  b_spec_q  <- sym("bucket_date_spec")

  within <- function(a, b) {
    # TRUE if primary a is strictly less than window_days before severe date b
    coalesce(as.numeric(difftime(b, a, units = "days")) < window_days, FALSE)
  }

  # Index virus for this population (e.g. rsv_pop -> rsv); used for mild/combo rules
  pop_name <- as_name(pop_q)
  index_pathogen <- sub("_pop$", "", pop_name)

  ip_sens_q <- sym(paste0(index_pathogen, "_primary_date_sens"))
  ip_spec_q <- sym(paste0(index_pathogen, "_primary_date_spec"))

  # index_mild_* are internal helpers only (not pivoted to sens_stage directly).
  # Exclusive pathogen flags (rsv_sens, flu_sens, ...) are separate columns.
  index_mild_sens <- "index_mild_sens"
  index_mild_spec <- "index_mild_spec"
  broad_only_name <- "broad_sens"
  bucket_sens_name <- "bucket_sens"
  bucket_spec_name <- "bucket_spec"
  other_sens_name <- "other_sens"

  out <- df %>%
    filter(!!pop_q) %>%
    mutate(

      # --- SENSITIVE mild phenotype flags ---
      # Index mild: index virus sensitive primary within window of severe date
      !!index_mild_sens := within(.data[[as_string(ip_sens_q)]], !!sec_q),

      # Single-virus categories (exclusive: other virus sens primaries outside window)
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

      # Multi-virus combos require index virus mild plus other virus(s) in window
      rsv_flu_sens = if (include_covid) {
        !within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          .data[[index_mild_sens]]
      } else {
        within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          .data[[index_mild_sens]]
      },

      rsv_covid_sens = if (include_covid)
        within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          !.data[[index_mild_sens]] else FALSE,
      flu_covid_sens = if (include_covid)
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          .data[[index_mild_sens]] else FALSE,
      covid_sens = if (include_covid)
        !within(.data[[as_string(fp_sens_q)]], !!sec_q) &
          !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) else FALSE,
      rsv_flu_covid_sens = if (include_covid)
        within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          .data[[index_mild_sens]] else FALSE,

      # Bucket / broad: no index-virus or RSV/covid sens primary in window.
      # bucket_sens = bucket in window and broad not in window.
      # broad_sens = broad in window (no !bucket); if both dates are in window,
      # only broad_sens is true.
      !!bucket_sens_name :=
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
        (if (include_covid) !within(.data[[as_string(cp_sens_q)]], !!sec_q) else TRUE) &
        !.data[[index_mild_sens]] &
        within(.data[[as_string(b_sens_q)]], !!sec_q) &
        !within(.data[[as_string(bb_sens_q)]], !!sec_q),

      !!broad_only_name :=
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
        (if (include_covid) !within(.data[[as_string(cp_sens_q)]], !!sec_q) else TRUE) &
        !.data[[index_mild_sens]] &
        within(.data[[as_string(bb_sens_q)]], !!sec_q),

      # --- SPECIFIC mild phenotype flags (same structure, spec primary dates) ---
      !!index_mild_spec := within(.data[[as_string(ip_spec_q)]], !!sec_q),

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
          .data[[index_mild_spec]]
      } else {
        within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          .data[[index_mild_spec]]
      },

      !!bucket_spec_name :=
        !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
        (if (include_covid) !within(.data[[as_string(cp_spec_q)]], !!sec_q) else TRUE) &
        !.data[[index_mild_spec]] &
        within(.data[[as_string(b_spec_q)]], !!sec_q),

      rsv_covid_spec = if (include_covid)
        within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          !.data[[index_mild_spec]] else FALSE,
      flu_covid_spec = if (include_covid)
        !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          .data[[index_mild_spec]] else FALSE,
      covid_spec = if (include_covid)
        !within(.data[[as_string(fp_spec_q)]], !!sec_q) &
          !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(cp_spec_q)]], !!sec_q) else FALSE,
      rsv_flu_covid_spec = if (include_covid)
        within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          .data[[index_mild_spec]] else FALSE,

      # Residual sensitive category: broad attendance in window but not bucket,
      # after pathogen / bucket / broad rules above have been applied
      !!other_sens_name :=
        within(.data[[as_string(bb_sens_q)]], !!sec_q) &
        !within(.data[[as_string(b_sens_q)]], !!sec_q) &
        !rsv_sens & !flu_sens & !covid_sens &
        !rsv_flu_sens & !rsv_covid_sens & !flu_covid_sens & !rsv_flu_covid_sens &
        !.data[[bucket_sens_name]] & !.data[[broad_only_name]]

    )

  # Column order sets priority when multiple sens flags are TRUE (first match wins)
  keep <- c(
    "patient_id",
    "rsv_flu_covid_sens",
    "rsv_flu_sens",
    "rsv_covid_sens",
    "flu_covid_sens",
    "rsv_sens",
    "flu_sens",
    "covid_sens",
    bucket_sens_name,
    broad_only_name,
    other_sens_name,
    "rsv_flu_covid_spec",
    "rsv_flu_spec",
    "rsv_covid_spec",
    "flu_covid_spec",
    "rsv_spec",
    "flu_spec",
    "covid_spec",
    bucket_spec_name
  )

  out <- out %>% select(any_of(keep))

  if (!include_covid) {
    out <- out %>% select(-contains("covid"))
  }

  # Collapse per-patient flags into one sens label and one spec label, then count
  # patients in each (sens_stage, spec_stage) pair for the Sankey diagram.
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
      denom = sum(rounded),
      pct = 100 * rounded / denom
    ) %>%
    select(-n) %>%
    mutate(population = pop_name, .before = sens_stage)

}

## Run classification for each validation population and save combination counts ----
df_rsv <- make_pop_flags(
  df = df_input,
  pop_flag = rsv_pop,
  secondary_date = rsv_secondary_date_spec,
  window_days = 30,
  include_covid = (study_start_date >= covid_season_min)
)

df_flu <- make_pop_flags(
  df = df_input,
  pop_flag = flu_pop,
  secondary_date = flu_secondary_date_spec,
  window_days = 30,
  include_covid = (study_start_date >= covid_season_min)
)

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
