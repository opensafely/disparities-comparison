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

#add a flag columns for the populations of interest
if (study_start_date < covid_season_min) {

  df_input <- df_input %>%
    mutate(
      flu_pop = flag_population(
        df = .,
        secondary_outcome = flu_secondary_date_spec,
        primary_cols = c("flu_primary_date_sens", "rsv_primary_date_sens"),
        n_days = 30
      ),
      rsv_pop = flag_population(
        df = .,
        secondary_outcome = rsv_secondary_date_spec,
        primary_cols = c("flu_primary_date_sens", "rsv_primary_date_sens"),
        n_days = 30
      )
    )
  
} else {

  df_input <- df_input %>%
    mutate(
      flu_pop = flag_population(
        df = .,
        secondary_outcome = flu_secondary_date_spec,
        primary_cols = c("flu_primary_date_sens", "rsv_primary_date_sens", "covid_primary_date_sens"),
        n_days = 30
      ),
      rsv_pop = flag_population(
        df = .,
        secondary_outcome = rsv_secondary_date_spec,
        primary_cols = c("flu_primary_date_sens", "rsv_primary_date_sens", "covid_primary_date_sens"),
        n_days = 30
      ),
      covid_pop = flag_population(
        df = .,
        secondary_outcome = covid_secondary_date_spec,
        primary_cols = c("flu_primary_date_sens", "rsv_primary_date_sens", "covid_primary_date_sens"),
        n_days = 30
      )
    )

}

#create a function to calculate the percetange of secondary outcomes linked to a primary care attendance
# percent(s) among those with non-missing secondary outcome
pop_percent_both <- function(df, specs, include_triples = TRUE) {
  stopifnot(is.list(specs), length(specs) >= 1)

  flag_names <- vapply(specs, \(x) x$flag, character(1))
  sec_names  <- vapply(specs, \(x) x$secondary, character(1))

  combos <- list()
  combos <- c(combos, lapply(seq_along(flag_names), \(i) i))
  if (length(flag_names) >= 2) combos <- c(combos, combn(seq_along(flag_names), 2, simplify = FALSE))
  if (include_triples && length(flag_names) >= 3) combos <- c(combos, combn(seq_along(flag_names), 3, simplify = FALSE))

  calc <- function(denom_df, denom, sec, idx_flag, denom_type) {
    bind_rows(lapply(combos, function(idx) {
      ok <- Reduce(
        `&`,
        lapply(idx, \(i) coalesce(denom_df[[flag_names[[i]]]], FALSE)),
        init = rep(TRUE, denom)
      )

      n_raw <- sum(ok)
      denom_raw <- denom
      n <- roundmid_any(n_raw)
      denom <- roundmid_any(denom_raw)
      tibble(
        index_pop = idx_flag,
        secondary = sec,
        denom_type = denom_type,
        combo = paste(flag_names[idx], collapse = " & "),
        n_denom_raw = denom_raw,
        n_combo_raw = n_raw,
        n_denom_midpoint10 = denom,
        n_combo_midpoint10 = n,
        pct_combo_midpoint10_derived = if (denom == 0) NA_real_ else 100 * n / denom
      )
    }))
  }

  bind_rows(lapply(seq_along(specs), function(s) {
    sec <- sec_names[[s]]
    idx_flag <- flag_names[[s]]

    # Method 1: whole secondary-denominator
    denom_df1 <- df %>% filter(!is.na(.data[[sec]]))
    out1 <- calc(denom_df1, nrow(denom_df1), sec, idx_flag, "secondary")

    # Method 2: within index population
    denom_df2 <- df %>%
      filter(!is.na(.data[[sec]]), coalesce(.data[[idx_flag]], FALSE))
    out2 <- calc(denom_df2, nrow(denom_df2), sec, idx_flag, "secondary_in_index_pop")

    bind_rows(out1, out2)
  }))
}

specs <- list(
  list(flag = "flu_pop", secondary = "flu_secondary_date_spec"),
  list(flag = "rsv_pop", secondary = "rsv_secondary_date_spec")
)
if (study_start_date >= covid_season_min) {
  specs <- c(specs, list(list(flag = "covid_pop", secondary = "covid_secondary_date_spec")))
}

props <- pop_percent_both(df_input, specs) %>%
  filter(
    !(index_pop == "flu_pop" & denom_type == "secondary_in_index_pop" & combo == "flu_pop") &
    !(index_pop == "rsv_pop" & denom_type == "secondary_in_index_pop" & combo == "rsv_pop") &
    !(index_pop == "covid_pop" & denom_type == "secondary_in_index_pop" & combo == "covid_pop")) %>% 
  mutate(
    secondary = sub("_secondary_date_spec", "", secondary)
  ) %>% 
  select(-index_pop, -ends_with("_raw"))

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
  index_sens_mild_name <- paste0(index_pathogen, "_sens_mild")
  index_spec_mild_name <- paste0(index_pathogen, "_spec_mild")
  broad_only_name <- paste0(index_pathogen, "_broad_only")
  bucket_only_name <- paste0(index_pathogen, "_bucket_only")

  out <- df %>%
    filter(!!pop_q) %>%
    mutate(

      # --- SENS ---
      # index-mild depends on which population you're flagging (flu/rsv/covid)
      !!index_sens_mild_name := within(.data[[as_string(ip_sens_q)]], !!sec_q),

      flu_sens_only = if (include_covid) {
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          !within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          within(.data[[as_string(fp_sens_q)]], !!sec_q)
      } else {
        !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(fp_sens_q)]], !!sec_q)
      },

      rsv_sens_only = if (include_covid) {
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
      covid_sens_only = if (include_covid)
        !within(.data[[as_string(fp_sens_q)]], !!sec_q) &
          !within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) else FALSE,
      rsv_flu_covid_sens = if (include_covid)
        within(.data[[as_string(rp_sens_q)]], !!sec_q) &
          within(.data[[as_string(cp_sens_q)]], !!sec_q) &
          .data[[index_sens_mild_name]] else FALSE,

      # --- SPEC ---
      !!index_spec_mild_name := within(.data[[as_string(ip_spec_q)]], !!sec_q),

      flu_spec_only = if (include_covid) {
        !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          !within(.data[[as_string(cp_spec_q)]], !!sec_q) &
          within(.data[[as_string(fp_spec_q)]], !!sec_q)
      } else {
        !within(.data[[as_string(rp_spec_q)]], !!sec_q) &
          within(.data[[as_string(fp_spec_q)]], !!sec_q)
      },

      rsv_spec_only = if (include_covid) {
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
      covid_spec_only = if (include_covid)
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
    pop_name,
    index_sens_mild_name,
    index_spec_mild_name,
    broad_only_name,
    bucket_only_name,
    "rsv_sens_only",
    "flu_sens_only",
    "covid_sens_only",
    "rsv_flu_sens",
    "rsv_covid_sens",
    "flu_covid_sens",
    "rsv_flu_covid_sens",
    "rsv_spec_only",
    "flu_spec_only",
    "covid_spec_only",
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

  out
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

#create function to calculation proportions of each flag type
flag_counts_props <- function(df) {
  flags <- names(df)[vapply(df, is.logical, logical(1))]

  bind_rows(lapply(flags, function(v) {
    x <- coalesce(df[[v]], FALSE)
    n_raw <- sum(x)
    denom_raw <- nrow(df)
    n <- roundmid_any(n_raw)
    denom <- roundmid_any(denom_raw)
    tibble(
      flag = v,
      n_raw = n_raw,
      denom_raw = denom_raw,
      n_midpoint10 = n,
      denom_midpoint10 = denom,
      prop_midpoint10_derived = if (denom == 0) NA_real_ else n / denom
    )
  })) %>%
    arrange(desc(n_midpoint10))
}

rsv_summary  <- flag_counts_props(df_rsv) %>% mutate(pathogen = "rsv", .before = "flag")
flu_summary  <- flag_counts_props(df_flu) %>% mutate(pathogen = "flu", .before = "flag")
if (exists("df_covid")) covid_summary <- flag_counts_props(df_covid) %>% mutate(pathogen = "covid", .before = "flag")

sankey_sums <- bind_rows(rsv_summary, flu_summary)
if (exists("covid_summary")) sankey_sums <- bind_rows(sankey_sums, covid_summary)

#save
sankey_sums <- sankey_sums %>% select(-ends_with("_raw"))

write_csv(sankey_sums, file = here::here("output", "exploratory",
          paste0("internal_validation_combination_counts_", cohort, "_",
          year(study_start_date), "_", year(study_end_date), ".csv")))
