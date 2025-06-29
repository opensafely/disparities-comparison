library(tidyverse)
library(here)
library(lmtest)
library(data.table)

#create function to import data
import <- function(pathogen) {
  
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  df_older_adults <- read_csv(here::here("post_check",
    "output", "collated", "descriptive", "over_time",
    paste0("older_adults_", "rates_over_time_all_all_", pathogen, ".csv"))) %>%
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
  
  df_adults <- read_csv(here::here("post_check",
    "output", "collated", "descriptive", "over_time",
    paste0("adults_", "rates_over_time_all_all_", pathogen, ".csv"))) %>%
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
  
  df_children_and_adolescents <- read_csv(here::here("post_check",
    "output", "collated", "descriptive", "over_time",
    paste0("children_and_adolescents_", "rates_over_time_all_all_", pathogen,
           ".csv"))) %>%
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
  
  df_infants <- read_csv(here::here("post_check",
    "output", "collated", "descriptive", "over_time",
    paste0("infants_", "rates_over_time_all_all_", pathogen, ".csv"))) %>%
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
  
  df <- as.data.table(bind_rows(
    df_older_adults,
    df_adults,
    df_children_and_adolescents,
    df_infants
  ))
  
  df <- df[, total_events := sum(total_events, na.rm = TRUE),
           by = .(month, event, codelist_type, virus)] %>%
    unique() %>%
    arrange(month) %>%
    mutate(season = case_when(
      month >= as.Date("2016-09-01") & month < as.Date("2017-09-01") ~ "2016-17",
      month >= as.Date("2017-09-01") & month < as.Date("2018-09-01") ~ "2017-18",
      month >= as.Date("2018-09-01") & month < as.Date("2019-09-01") ~ "2018-19",
      month >= as.Date("2019-09-01") & month < as.Date("2020-09-01") ~ "2019-20",
      month >= as.Date("2020-09-01") & month < as.Date("2021-09-01") ~ "2020-21",
      month >= as.Date("2021-09-01") & month < as.Date("2022-09-01") ~ "2021-22",
      month >= as.Date("2022-09-01") & month < as.Date("2023-09-01") ~ "2022-23",
      month >= as.Date("2023-09-01") & month < as.Date("2024-09-01") ~ "2023-24"
    ))
  
  return(df)
  
}

#import data
df_rsv <- import("rsv")

#import data
df_flu <- import("flu")

#import data
df_covid <- import("covid")

#import surveillance data
df_surv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "seasonality_england.csv")) %>%
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
df_surv <- df_surv[, c("month", "total_events", "virus")] %>%
  arrange(month) %>%
  mutate(season = case_when(
    month >= as.Date("2016-09-01") & month < as.Date("2017-09-01") ~ "2016-17",
    month >= as.Date("2017-09-01") & month < as.Date("2018-09-01") ~ "2017-18",
    month >= as.Date("2018-09-01") & month < as.Date("2019-09-01") ~ "2018-19",
    month >= as.Date("2019-09-01") & month < as.Date("2020-09-01") ~ "2019-20",
    month >= as.Date("2020-09-01") & month < as.Date("2021-09-01") ~ "2020-21",
    month >= as.Date("2021-09-01") & month < as.Date("2022-09-01") ~ "2021-22",
    month >= as.Date("2022-09-01") & month < as.Date("2023-09-01") ~ "2022-23",
    month >= as.Date("2023-09-01") & month < as.Date("2024-09-01") ~ "2023-24"
  ))

#write a function for pearson's correlation coefficient test
pearson_test <- function(df1, df2, event, codelist, virus, subset = "no") {
  
  df1 <- df1 %>%
    filter(event == !!event, codelist_type == !!codelist)
  
  df2 <- df2 %>%
    filter(virus == !!virus, month %in% df1$month)
  
  if (subset == "yes") {
    
    season <- season
    
    df1 <- df1 %>%
      filter(season == !!season)
    
    df2 <- df2 %>%
      filter(season == !!season)
    
  }
  
  xp <- df1$total_events
  yp <- df2$total_events
  
  cor.test(xp, yp, method = "pearson")
  
}

#perform test
pearson_rsv_mild_spec <- pearson_test(
  df_rsv, df_surv, "Mild", "specific", "RSV")
pearson_rsv_mild_sens <- pearson_test(
  df_rsv, df_surv, "Mild", "sensitive", "RSV")
pearson_rsv_severe_spec <- pearson_test(
  df_rsv, df_surv, "Severe", "specific", "RSV")
pearson_rsv_severe_sens <- pearson_test(
  df_rsv, df_surv, "Severe", "sensitive", "RSV")
pearson_flu_mild_spec <- pearson_test(
  df_flu, df_surv, "Mild", "specific", "Influenza")
pearson_flu_mild_sens <- pearson_test(
  df_flu, df_surv, "Mild", "sensitive", "Influenza")
pearson_flu_severe_spec <- pearson_test(
  df_flu, df_surv, "Severe", "specific", "Influenza")
pearson_flu_severe_sens <- pearson_test(
  df_flu, df_surv, "Severe", "sensitive", "Influenza")
pearson_covid_mild_spec <- pearson_test(
  df_covid, df_surv, "Mild", "specific", "COVID-19")
pearson_covid_mild_sens <- pearson_test(
  df_covid, df_surv, "Mild", "sensitive", "COVID-19")
pearson_covid_severe_spec <- pearson_test(
  df_covid, df_surv, "Severe", "specific", "COVID-19")
pearson_covid_severe_sens <- pearson_test(
  df_covid, df_surv, "Severe", "sensitive", "COVID-19")

#save test results
pearson_results <- data.frame(
  test = c("RSV Mild Specific", "RSV Mild Sensitive", "RSV Severe Specific",
           "RSV Severe Sensitive", "Flu Mild Specific", "Flu Mild Sensitive",
           "Flu Severe Specific", "Flu Severe Sensitive", "COVID-19 Mild Specific",
           "COVID-19 Mild Sensitive", "COVID-19 Severe Specific",
           "COVID-19 Severe Sensitive"),
  pearson = c(pearson_rsv_mild_spec$estimate,
              pearson_rsv_mild_sens$estimate,
              pearson_rsv_severe_spec$estimate,
              pearson_rsv_severe_sens$estimate,
              pearson_flu_mild_spec$estimate,
              pearson_flu_mild_sens$estimate,
              pearson_flu_severe_spec$estimate,
              pearson_flu_severe_sens$estimate,
              pearson_covid_mild_spec$estimate,
              pearson_covid_mild_sens$estimate,
              pearson_covid_severe_spec$estimate,
              pearson_covid_severe_sens$estimate),
  p_value = c(pearson_rsv_mild_spec$p.value,
              pearson_rsv_mild_sens$p.value,
              pearson_rsv_severe_spec$p.value,
              pearson_rsv_severe_sens$p.value,
              pearson_flu_mild_spec$p.value,
              pearson_flu_mild_sens$p.value,
              pearson_flu_severe_spec$p.value,
              pearson_flu_severe_sens$p.value,
              pearson_covid_mild_spec$p.value,
              pearson_covid_mild_sens$p.value,
              pearson_covid_severe_spec$p.value,
              pearson_covid_severe_sens$p.value)
)

#now do the same by season
seasons_pre <- c("2016-17", "2017-18", "2018-19")
seasons_post <- c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24")
pearson_results_seasons <- NULL

for (season in seasons_pre) {
  
  pearson_rsv_mild_spec <- pearson_test(
    df_rsv, df_surv, "Mild", "specific", "RSV", subset = "yes")
  pearson_rsv_mild_sens <- pearson_test(
    df_rsv, df_surv, "Mild", "sensitive", "RSV", subset = "yes")
  pearson_rsv_severe_spec <- pearson_test(
    df_rsv, df_surv, "Severe", "specific", "RSV", subset = "yes")
  pearson_rsv_severe_sens <- pearson_test(
    df_rsv, df_surv, "Severe", "sensitive", "RSV", subset = "yes")
  pearson_flu_mild_spec <- pearson_test(
    df_flu, df_surv, "Mild", "specific", "Influenza", subset = "yes")
  pearson_flu_mild_sens <- pearson_test(
    df_flu, df_surv, "Mild", "sensitive", "Influenza", subset = "yes")
  pearson_flu_severe_spec <- pearson_test(
    df_flu, df_surv, "Severe", "specific", "Influenza", subset = "yes")
  pearson_flu_severe_sens <- pearson_test(
    df_flu, df_surv, "Severe", "sensitive", "Influenza", subset = "yes")
  
  #save test results
  season_results <- data.frame(
    test = c(paste0("RSV Mild Specific (", season, ")"),
             paste0("RSV Mild Sensitive (", season, ")"),
             paste0("RSV Severe Specific (", season, ")"),
             paste0("RSV Severe Sensitive (", season, ")"),
             paste0("Flu Mild Specific (", season, ")"),
             paste0("Flu Mild Sensitive (", season, ")"),
             paste0("Flu Severe Specific (", season, ")"),
             paste0("Flu Severe Sensitive (", season, ")")),
    pearson = c(pearson_rsv_mild_spec$estimate,
                pearson_rsv_mild_sens$estimate,
                pearson_rsv_severe_spec$estimate,
                pearson_rsv_severe_sens$estimate,
                pearson_flu_mild_spec$estimate,
                pearson_flu_mild_sens$estimate,
                pearson_flu_severe_spec$estimate,
                pearson_flu_severe_sens$estimate),
    p_value = c(pearson_rsv_mild_spec$p.value,
                pearson_rsv_mild_sens$p.value,
                pearson_rsv_severe_spec$p.value,
                pearson_rsv_severe_sens$p.value,
                pearson_flu_mild_spec$p.value,
                pearson_flu_mild_sens$p.value,
                pearson_flu_severe_spec$p.value,
                pearson_flu_severe_sens$p.value)
  )
  
  pearson_results_seasons <- rbind(
    pearson_results_seasons,
    season_results
  )  
  
}

for (season in seasons_post) {
  
  pearson_rsv_mild_spec <- pearson_test(
    df_rsv, df_surv, "Mild", "specific", "RSV", subset = "yes")
  pearson_rsv_mild_sens <- pearson_test(
    df_rsv, df_surv, "Mild", "sensitive", "RSV", subset = "yes")
  pearson_rsv_severe_spec <- pearson_test(
    df_rsv, df_surv, "Severe", "specific", "RSV", subset = "yes")
  pearson_rsv_severe_sens <- pearson_test(
    df_rsv, df_surv, "Severe", "sensitive", "RSV", subset = "yes")
  pearson_flu_mild_spec <- pearson_test(
    df_flu, df_surv, "Mild", "specific", "Influenza", subset = "yes")
  pearson_flu_mild_sens <- pearson_test(
    df_flu, df_surv, "Mild", "sensitive", "Influenza", subset = "yes")
  pearson_flu_severe_spec <- pearson_test(
    df_flu, df_surv, "Severe", "specific", "Influenza", subset = "yes")
  pearson_flu_severe_sens <- pearson_test(
    df_flu, df_surv, "Severe", "sensitive", "Influenza", subset = "yes")
  pearson_covid_mild_spec <- pearson_test(
    df_covid, df_surv, "Mild", "specific", "COVID-19", subset = "yes")
  pearson_covid_mild_sens <- pearson_test(
    df_covid, df_surv, "Mild", "sensitive", "COVID-19", subset = "yes")
  pearson_covid_severe_spec <- pearson_test(
    df_covid, df_surv, "Severe", "specific", "COVID-19", subset = "yes")
  pearson_covid_severe_sens <- pearson_test(
    df_covid, df_surv, "Severe", "sensitive", "COVID-19", subset = "yes")
  
  #save test results
  season_results <- data.frame(
    test = c(paste0("RSV Mild Specific (", season, ")"),
             paste0("RSV Mild Sensitive (", season, ")"),
             paste0("RSV Severe Specific (", season, ")"),
             paste0("RSV Severe Sensitive (", season, ")"),
             paste0("Flu Mild Specific (", season, ")"),
             paste0("Flu Mild Sensitive (", season, ")"),
             paste0("Flu Severe Specific (", season, ")"),
             paste0("Flu Severe Sensitive (", season, ")"),
             paste0("COVID-19 Mild Specific (", season, ")"),
             paste0("COVID-19 Mild Sensitive (", season, ")"),
             paste0("COVID-19 Severe Specific (", season, ")"),
             paste0("COVID-19 Severe Sensitive (", season, ")")),
    pearson = c(pearson_rsv_mild_spec$estimate,
                pearson_rsv_mild_sens$estimate,
                pearson_rsv_severe_spec$estimate,
                pearson_rsv_severe_sens$estimate,
                pearson_flu_mild_spec$estimate,
                pearson_flu_mild_sens$estimate,
                pearson_flu_severe_spec$estimate,
                pearson_flu_severe_sens$estimate,
                pearson_covid_mild_spec$estimate,
                pearson_covid_mild_sens$estimate,
                pearson_covid_severe_spec$estimate,
                pearson_covid_severe_sens$estimate),
    p_value = c(pearson_rsv_mild_spec$p.value,
                pearson_rsv_mild_sens$p.value,
                pearson_rsv_severe_spec$p.value,
                pearson_rsv_severe_sens$p.value,
                pearson_flu_mild_spec$p.value,
                pearson_flu_mild_sens$p.value,
                pearson_flu_severe_spec$p.value,
                pearson_flu_severe_sens$p.value,
                pearson_covid_mild_spec$p.value,
                pearson_covid_mild_sens$p.value,
                pearson_covid_severe_spec$p.value,
                pearson_covid_severe_sens$p.value)
  )
  
  pearson_results_seasons <- rbind(
    pearson_results_seasons,
    season_results
  )  
  
}
