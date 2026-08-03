library(tidyverse)
library(here)
library(data.table)

seasons <- c("2016-17", "2017-18", "2018-19", "2019-20",
             "2020-21", "2021-22", "2022-23", "2023-24")

# Exclusion breakdown mirrors stages in analysis/cohort_criteria.R:
#   stage1  = registered (infants_subgroup: infant & mother registered)
#   stage2a = registered & is_female_or_male
#   stage2b = registered & has_imd
#   stage2c = registered & !care_home
#   stage2d = registered & !risk_group_infants   (infants only)
#   stage2e = registered & !severe_immunodeficiency (infants only)
# For infants / infants_subgroup, stages 2d and 2e are combined
# (as in flow_chart.R palivizumab eligibility).

process_standard_cohort <- function(cohort) {

  df_input <- read_csv(
    here::here("post_check", "output", "collated", "descriptive",
               paste0(cohort, "_flow_chart_collated.csv")),
    show_col_types = FALSE
  )
  patients_df <- as.data.table(df_input)
  names(patients_df) <- c(
    "total", "registered", "registered_sex", "registered_imd",
    "registered_no_carehome", "included", "perc_registered",
    "perc_registered_sex", "perc_registered_imd",
    "perc_registered_no_carehome", "perc_included", "subset"
  )

  patients_df[, `:=`(
    not_registered = total - registered,
    excl_sex = registered - registered_sex,
    excl_imd = registered - registered_imd,
    excl_care_home = registered - registered_no_carehome
  ), by = .(subset)]
  patients_df$cohort <- cohort

  patients_df %>%
    as_tibble() %>%
    select(
      total, not_registered, registered,
      excl_sex, excl_imd, excl_care_home,
      included, perc_included, cohort
    )
}

process_infants <- function() {

  cohort <- "infants"
  df_input <- read_csv(
    here::here("post_check", "output", "collated", "descriptive",
               paste0(cohort, "_flow_chart_collated.csv")),
    show_col_types = FALSE
  )
  patients_df <- as.data.table(df_input)
  names(patients_df) <- c(
    "total", "registered", "registered_sex", "registered_imd",
    "registered_no_carehome", "registered_no_riskgroup",
    "registered_no_immune", "included", "perc_registered",
    "perc_registered_sex", "perc_registered_imd",
    "perc_registered_no_carehome", "perc_registered_no_riskgroup",
    "perc_registered_no_immune", "perc_included", "subset"
  )

  # Combine stage2d (risk group) and stage2e (severe immunodeficiency)
  patients_df[, `:=`(
    not_registered = total - registered,
    excl_sex = registered - registered_sex,
    excl_imd = registered - registered_imd,
    excl_care_home = registered - registered_no_carehome,
    excl_palivizumab = (registered - registered_no_riskgroup) +
      (registered - registered_no_immune)
  ), by = .(subset)]
  patients_df$cohort <- cohort

  patients_df %>%
    as_tibble() %>%
    select(
      total, not_registered, registered,
      excl_sex, excl_imd, excl_care_home, excl_palivizumab,
      included, perc_included, cohort
    )
}

process_infants_subgroup <- function() {

  cohort <- "infants_subgroup"
  df_input <- read_csv(
    here::here("post_check", "output", "collated", "descriptive",
               paste0(cohort, "_flow_chart_collated.csv")),
    show_col_types = FALSE
  )
  patients_df <- as.data.table(df_input)
  names(patients_df) <- c(
    "total", "registered", "mother_registered",
    "registered_mother_registered",
    "registered_mother_registered_sex",
    "registered_mother_registered_imd",
    "registered_mother_registered_no_carehome",
    "registered_mother_registered_no_riskgroup",
    "registered_mother_registered_no_immune", "included",
    "perc_registered", "perc_mother_registered",
    "perc_registered_mother_registered",
    "perc_registered_mother_registered_sex",
    "perc_registered_mother_registered_imd",
    "perc_registered_mother_registered_no_carehome",
    "perc_registered_mother_registered_no_riskgroup",
    "perc_registered_mother_registered_no_immune",
    "perc_included", "subset"
  )

  # stage1 base = infant & mother registered; combine stage2d + stage2e
  patients_df[, `:=`(
    not_registered = total - registered_mother_registered,
    excl_sex = registered_mother_registered -
      registered_mother_registered_sex,
    excl_imd = registered_mother_registered -
      registered_mother_registered_imd,
    excl_care_home = registered_mother_registered -
      registered_mother_registered_no_carehome,
    excl_palivizumab = (registered_mother_registered -
      registered_mother_registered_no_riskgroup) +
      (registered_mother_registered -
        registered_mother_registered_no_immune),
    registered = registered_mother_registered
  ), by = .(subset)]
  patients_df$cohort <- cohort

  patients_df %>%
    as_tibble() %>%
    select(
      total, not_registered, registered,
      excl_sex, excl_imd, excl_care_home, excl_palivizumab,
      included, perc_included, cohort
    )
}

older_adults_df <- process_standard_cohort("older_adults")
adults_df <- process_standard_cohort("adults")
children_and_adolescents_df <- process_standard_cohort("children_and_adolescents")
infants_df <- process_infants()
infants_subgroup_df <- process_infants_subgroup()

# combine inclusion tables
inclusion_df <- bind_rows(
  older_adults_df,
  adults_df,
  children_and_adolescents_df,
  infants_df,
  infants_subgroup_df
) %>%
  mutate(
    subset = rep(seasons, 5)
  ) %>%
  pivot_longer(
    cols = c(
      total, not_registered, registered,
      excl_sex, excl_imd, excl_care_home, excl_palivizumab,
      included, perc_included
    ),
    names_to = "measure",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = c(cohort, measure),
    names_from = subset,
    values_from = value
  ) %>%
  # drop palivizumab rows for non-infant cohorts (all NA)
  filter(!(measure == "excl_palivizumab" &
             !cohort %in% c("infants", "infants_subgroup")))

#save it
write_csv(
  inclusion_df,
  here::here("post_check", "supplemental", "flow_chart",
             "reformatted_inclusion_collated.csv")
)
