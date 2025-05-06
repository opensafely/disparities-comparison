library(tidyverse)
library(here)
library(lmtest)
library(data.table)

#create function to import data
import <- function(pathogen, cohort) {
  
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  df <- read_csv(here::here("post_check",
    "output", "collated", "descriptive", "over_time",
    paste0(cohort, "_", "rates_over_time_all_all_", pathogen, ".csv"))) %>%
    mutate(virus = pathogen_title) %>%
    arrange(interval_start) %>%
    mutate(
      month = ymd(floor_date(interval_start, unit = "month")),
      event = case_when(
        str_detect(event, "primary") ~ "Mild",
        str_detect(event, "secondary") ~ "Severe"
      )
    ) %>%
    select(c(interval_start, month, event, total_events = total_events_midpoint10,
             codelist_type, virus)) %>%
    filter(month >= "2016-09-01") %>%
    slice_min(order_by = interval_start, n = 1, by = c("month", "event")) %>%
    select(-interval_start)
  
  return(df)
}

#import data
df_rsv_older_adults <- import("rsv", "older_adults")
df_rsv_adults <- import("rsv", "adults")
df_rsv_children <- import("rsv", "children_and_adolescents")
df_rsv_infants <- import("rsv", "infants")

df_rsv <- as.data.table(bind_rows(
  df_rsv_older_adults,
  df_rsv_adults,
  df_rsv_children,
  df_rsv_infants
)) %>%
  arrange(month)
df_rsv <- df_rsv[, total_events := sum(total_events, na.rm = T),
       by = .(month, event, codelist_type, virus)] %>%
  unique()

#separate by mild/severe and specifc/sensitive
df_rsv_spec_mild <- df_rsv %>%
  filter(event == "Mild", codelist_type == "specific")
df_rsv_spec_severe <- df_rsv %>%
  filter(event == "Severe", codelist_type == "specific")
df_rsv_sens_mild <- df_rsv %>%
  filter(event == "Mild", codelist_type == "sensitive")
df_rsv_sens_severe <- df_rsv %>%
  filter(event == "Severe", codelist_type == "sensitive")

#import data
df_flu_older_adults <- import("flu", "older_adults")
df_flu_adults <- import("flu", "adults")
df_flu_children <- import("flu", "children_and_adolescents")
df_flu_infants <- import("flu", "infants")

df_flu <- as.data.table(bind_rows(
  df_flu_older_adults,
  df_flu_adults,
  df_flu_children,
  df_flu_infants
)) %>%
  arrange(month)
df_flu <- df_flu[, total_events := sum(total_events, na.rm = T),
                 by = .(month, event, codelist_type, virus)] %>%
  unique()

#separate by mild/severe and specifc/sensitive
df_flu_spec_mild <- df_flu %>%
  filter(event == "Mild", codelist_type == "specific")
df_flu_spec_severe <- df_flu %>%
  filter(event == "Severe", codelist_type == "specific")
df_flu_sens_mild <- df_flu %>%
  filter(event == "Mild", codelist_type == "sensitive")
df_flu_sens_severe <- df_flu %>%
  filter(event == "Severe", codelist_type == "sensitive")

#import data
df_covid_older_adults <- import("covid", "older_adults")
df_covid_adults <- import("covid", "adults")
df_covid_children <- import("covid", "children_and_adolescents")
df_covid_infants <- import("covid", "infants")

df_covid <- as.data.table(bind_rows(
  df_covid_older_adults,
  df_covid_adults,
  df_covid_children,
  df_covid_infants
)) %>%
  arrange(month)
df_covid <- df_covid[, total_events := sum(total_events, na.rm = T),
                 by = .(month, event, codelist_type, virus)] %>%
  unique()

#separate by mild/severe and specifc/sensitive
df_covid_spec_mild <- df_covid %>%
  filter(event == "Mild", codelist_type == "specific")
df_covid_spec_severe <- df_covid %>%
  filter(event == "Severe", codelist_type == "specific")
df_covid_sens_mild <- df_covid %>%
  filter(event == "Mild", codelist_type == "sensitive")
df_covid_sens_severe <- df_covid %>%
  filter(event == "Severe", codelist_type == "sensitive")

#import surveillance data
df_surv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "seasonality.csv")) %>%
  arrange(month) %>%
  select(-covid_scaled) %>%
  pivot_longer(
    cols = c(total_rsv, total_flu, total_covid),
    names_to = "virus",
    values_to = "total_events"
  ) %>%
  mutate(
    virus = case_when(
      virus == "total_rsv" ~ "RSV",
      virus == "total_flu" ~ "Influenza",
      virus == "total_covid" ~ "COVID-19"
    )
  )

df_surv <- df_surv[, c("month", "total_events", "virus")]

df_surv_rsv <- df_surv %>%
  filter(virus == "RSV", month %in% df_rsv$month)
df_surv_flu <- df_surv %>%
  filter(virus == "Influenza", month %in% df_flu$month)
df_surv_flu_spec_severe <- df_surv %>%
  filter(virus == "Influenza", month %in% df_flu_spec_severe$month) 
df_surv_covid <- df_surv %>%
  filter(virus == "COVID-19") %>%
  filter(month >= "2020-03-01", month %in% df_covid$month)

#granger tests
granger_test_rsv_mild_spec <- lmtest::grangertest(
  df_rsv_spec_mild$total_events ~ df_surv_rsv$total_events,
  order = 1
)
granger_test_rsv_mild_sens <- lmtest::grangertest(
  df_rsv_sens_mild$total_events ~ df_surv_rsv$total_events,
  order = 1
)
granger_test_rsv_severe_spec <- lmtest::grangertest(
  df_rsv_spec_severe$total_events ~ df_surv_rsv$total_events,
  order = 1
)
granger_test_rsv_severe_sens <- lmtest::grangertest(
  df_rsv_sens_severe$total_events ~ df_surv_rsv$total_events,
  order = 1
)
granger_test_flu_mild_spec <- lmtest::grangertest(
  df_flu_spec_mild$total_events ~ df_surv_flu$total_events,
  order = 1
)
granger_test_flu_mild_sens <- lmtest::grangertest(
  df_flu_sens_mild$total_events ~ df_surv_flu$total_events,
  order = 1
)
granger_test_flu_severe_spec <- lmtest::grangertest(
  df_flu_spec_severe$total_events ~ df_surv_flu_spec_severe$total_events,
  order = 1
)
granger_test_flu_severe_sens <- lmtest::grangertest(
  df_flu_sens_severe$total_events ~ df_surv_flu$total_events,
  order = 1
)
granger_test_covid_mild_spec <- lmtest::grangertest(
  df_covid_spec_mild$total_events ~ df_surv_covid$total_events,
  order = 1
)
granger_test_covid_mild_sens <- lmtest::grangertest(
  df_covid_sens_mild$total_events ~ df_surv_covid$total_events,
  order = 1
)
granger_test_covid_severe_spec <- lmtest::grangertest(
  df_covid_spec_severe$total_events ~ df_surv_covid$total_events,
  order = 1
)
granger_test_covid_severe_sens <- lmtest::grangertest(
  df_covid_sens_severe$total_events ~ df_surv_covid$total_events,
  order = 1
)

#save granger test results
granger_test_results <- data.frame(
  test = c("RSV Mild Specific", "RSV Mild Sensitive", "RSV Severe Specific",
           "RSV Severe Sensitive", "Flu Mild Specific", "Flu Mild Sensitive",
           "Flu Severe Specific", "Flu Severe Sensitive", "COVID-19 Mild Specific",
           "COVID-19 Mild Sensitive", "COVID-19 Severe Specific",
           "COVID-19 Severe Sensitive"),
  granger_test = c(granger_test_rsv_mild_spec$F[2], granger_test_rsv_mild_sens$F[2],
                   granger_test_rsv_severe_spec$F[2], granger_test_rsv_severe_sens$F[2],
                   granger_test_flu_mild_spec$F[2], granger_test_flu_mild_sens$F[2],
                   granger_test_flu_severe_spec$F[2], granger_test_flu_severe_sens$F[2],
                   granger_test_covid_mild_spec$F[2], granger_test_covid_mild_sens$F[2],
                   granger_test_covid_severe_spec$F[2], granger_test_covid_severe_sens$F[2])
) %>%
  mutate(
    p_value = c(granger_test_rsv_mild_spec$`Pr(>F)`[2],
                granger_test_rsv_mild_sens$`Pr(>F)`[2],
                granger_test_rsv_severe_spec$`Pr(>F)`[2],
                granger_test_rsv_severe_sens$`Pr(>F)`[2],
                granger_test_flu_mild_spec$`Pr(>F)`[2],
                granger_test_flu_mild_sens$`Pr(>F)`[2],
                granger_test_flu_severe_spec$`Pr(>F)`[2],
                granger_test_flu_severe_sens$`Pr(>F)`[2],
                granger_test_covid_mild_spec$`Pr(>F)`[2],
                granger_test_covid_mild_sens$`Pr(>F)`[2],
                granger_test_covid_severe_spec$`Pr(>F)`[2],
                granger_test_covid_severe_sens$`Pr(>F)`[2])
  )
