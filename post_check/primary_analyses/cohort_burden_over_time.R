library(tidyverse)
library(here)
library(ggplot2)
library(ggpubr)
library(scales)
library(RColorBrewer)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
options(scipen = 999)

#create function to import data
import <- function(pathogen, cohort) {
  
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  df <- read_csv(here::here(
    "post_check", "output", "collated", "descriptive", "over_time",
    paste0(cohort, "_", "counts_over_time_all_monthly_", pathogen, ".csv"))) %>%
    mutate(virus = pathogen_title) %>%
    arrange(month) %>%
    mutate(
      event = case_when(
        str_detect(event, "primary") ~ "Mild",
        str_detect(event, "secondary") ~ "Severe"
      )
    ) %>%
    select(c(month, event,
             cohort_events = n,
             codelist_type, virus)) %>%
    filter(month >= "2016-09-01") %>%
    tidyr::complete(
      month = seq(ymd("2016-09-01"), ymd("2024-08-31"), by = "1 month"),
      event,
      codelist_type,
      virus,
      fill = list(cohort_events = 0)
    ) %>% 
    mutate(
      subset = case_when(
        month >= ymd("2016-09-01") & month <= ymd("2017-09-01") ~ "2016-17",
        month >= ymd("2017-09-01") & month <= ymd("2018-09-01") ~ "2017-18",
        month >= ymd("2018-09-01") & month <= ymd("2019-09-01") ~ "2018-19",
        month >= ymd("2019-09-01") & month <= ymd("2020-09-01") ~ "2019-20",
        month >= ymd("2020-09-01") & month <= ymd("2021-09-01") ~ "2020-21",
        month >= ymd("2021-09-01") & month <= ymd("2022-09-01") ~ "2021-22",
        month >= ymd("2022-09-01") & month <= ymd("2023-09-01") ~ "2022-23",
        month >= ymd("2023-09-01") & month <= ymd("2024-09-01") ~ "2023-24",
      ),
      cohort = cohort
    )
  
  return(df)
  
}

#import data
df_rsv_older_adults <- import("rsv", "older_adults")
df_rsv_adults <- import("rsv", "adults")
df_rsv_children_and_adolescents <- import("rsv", "children_and_adolescents")
df_rsv_infants <- import("rsv", "infants")
df_rsv <- bind_rows(
  df_rsv_older_adults,
  df_rsv_adults,
  df_rsv_children_and_adolescents,
  df_rsv_infants
)

#import data
df_flu_older_adults <- import("flu", "older_adults")
df_flu_adults <- import("flu", "adults")
df_flu_children_and_adolescents <- import("flu", "children_and_adolescents")
df_flu_infants <- import("flu", "infants")
df_flu <- bind_rows(
  df_flu_older_adults,
  df_flu_adults,
  df_flu_children_and_adolescents,
  df_flu_infants
)

#import data
df_covid_older_adults <- import("covid", "older_adults")
df_covid_adults <- import("covid", "adults")
df_covid_children_and_adolescents <- import("covid", "children_and_adolescents")
df_covid_infants <- import("covid", "infants")
df_covid <- bind_rows(
  df_covid_older_adults,
  df_covid_adults,
  df_covid_children_and_adolescents,
  df_covid_infants
)

#combine all viruses
df_all <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>%
  filter(codelist_type == "specific") %>% 
  group_by(across(c(subset, month, event, virus))) %>% 
  mutate(
    total_events = sum(cohort_events)
  ) %>% 
  group_by(across(c(subset, month, event, virus, cohort))) %>% 
  mutate(
    perc_burden = cohort_events/total_events,
    cohort = factor(cohort, levels = c("older_adults", "adults", "children_and_adolescents", "infants"),
                    labels = c("Older Adults", "Adults", "Children and Young People", "Infants"),
                    ordered = T)
  ) %>% 
  ungroup() %>% 
  select(
    subset, month, event, virus, total_events, cohort, cohort_events, perc_burden
  )

stacked <- function(virus, area = FALSE, show_event_labels = FALSE) {

  si_labels <- function(digits, width) {
    f <- label_number(accuracy = 10^(-digits), scale_cut = cut_si(""))
    function(x) sprintf(paste0("%", width, "s"), f(x))
  }

  x_scale <- scale_x_date(
    limits = c(ymd("2016-01-01"), ymd("2025-06-01")),
    breaks = seq(ymd("2016-01-01"), ymd("2025-01-01"), by = "1 year"),
    date_labels = "%Y",
    expand = c(0, 0)
  )

  if (area == TRUE) {

    df_all %>% 
      filter(virus == !!virus) %>% 
      ggplot(aes(x = month, y = cohort_events, fill = cohort, group = cohort)) +
      geom_area(stat = "identity", position = "stack") +
      facet_wrap(~event, scales = "free_y") + theme_bw() +
      x_scale +
      scale_y_continuous(labels = si_labels(digits = 1, width = 8)) +
      labs(x = "", y = paste0("Monthly ", virus, " Cases"), fill = "Cohort") +
      scale_fill_brewer(type = "seq", palette = "Set2") +
      theme(
        strip.text = if (show_event_labels) element_text(face = "bold") else element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')
      )

  } else {

    df_all %>% 
      filter(virus == !!virus) %>% 
      ggplot(aes(x = month, y = cohort_events, fill = cohort, group = cohort)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~event, scales = "free_y") + theme_bw() +
      x_scale +
      scale_y_continuous(labels = si_labels(digits = 1, width = 8)) +
      labs(x = "", y = paste0("Monthly ", virus, " Cases"), fill = "Cohort") +
      scale_fill_brewer(type = "seq", palette = "Set2") +
      theme(
        strip.text = if (show_event_labels) element_text(face = "bold") else element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')
      )

  }

}

rsv_area <- stacked("RSV", area = TRUE, show_event_labels = TRUE)
flu_area <- stacked("Influenza", area = TRUE)
covid_area <- stacked("COVID-19", area = TRUE)
area <- ggarrange(
  rsv_area,
  flu_area,
  covid_area,
  ncol = 1,
  common.legend = TRUE
)
rsv_bar <- stacked("RSV", show_event_labels = TRUE)
flu_bar <- stacked("Influenza")
covid_bar <- stacked("COVID-19")
bar <- ggarrange(
  rsv_bar,
  flu_bar,
  covid_bar,
  ncol = 1,
  common.legend = TRUE
)
