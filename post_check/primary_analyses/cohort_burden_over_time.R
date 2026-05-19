library(tidyverse)
library(here)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
library(RColorBrewer)

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)
options(scipen = 999)

PATHOGENS <- c("rsv", "flu", "covid")
COHORTS <- c("older_adults", "adults", "children_and_adolescents", "infants")

import <- function(pathogen, cohort) {
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )

  read_csv(
    here::here(
      "post_check", "output", "collated", "descriptive", "over_time",
      paste0(cohort, "_counts_over_time_all_monthly_", pathogen, ".csv")
    ),
    show_col_types = FALSE
  ) %>%
    mutate(virus = pathogen_title) %>%
    arrange(month) %>%
    mutate(
      event = case_when(
        str_detect(event, "primary") ~ "Mild",
        str_detect(event, "secondary") ~ "Severe"
      )
    ) %>%
    select(month, event, cohort_events = n, codelist_type, virus) %>%
    filter(month >= "2016-09-01") %>%
    tidyr::complete(
      month = seq(ymd("2016-09-01"), ymd("2024-08-31"), by = "1 month"),
      event, codelist_type, virus,
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
        month >= ymd("2023-09-01") & month <= ymd("2024-09-01") ~ "2023-24"
      ),
      cohort = cohort
    )
}

df_all <- crossing(pathogen = PATHOGENS, cohort = COHORTS) %>%
  pmap_dfr(import) %>%
  filter(codelist_type == "specific") %>%
  group_by(subset, month, event, virus) %>%
  mutate(total_events = sum(cohort_events)) %>%
  group_by(subset, month, event, virus, cohort) %>%
  mutate(
    perc_burden = cohort_events / total_events,
    cohort = factor(
      cohort,
      levels = c("older_adults", "adults", "children_and_adolescents", "infants"),
      labels = c("Older Adults", "Adults", "Children and Young People", "Infants"),
      ordered = TRUE
    )
  ) %>%
  ungroup()

stacked <- function(virus, use_area = FALSE) {
  si_labels <- function(digits, width) {
    f <- label_number(accuracy = 10^(-digits), scale_cut = cut_si(""))
    function(x) sprintf(paste0("%", width, "s"), f(x))
  }

  p <- df_all %>%
    filter(virus == !!virus) %>%
    ggplot(aes(x = month, y = cohort_events, fill = cohort, group = cohort))

  if (use_area) {
    p <- p + geom_area(stat = "identity", position = "stack")
  } else {
    p <- p + geom_bar(stat = "identity", position = "stack")
  }

  p +
    facet_wrap(~event, scales = "free_y") +
    theme_bw() +
    scale_x_date(
      limits = c(ymd("2016-01-01"), ymd("2025-06-01")),
      breaks = seq(ymd("2016-01-01"), ymd("2025-01-01"), by = "1 year"),
      date_labels = "%Y",
      expand = c(0, 0)
    ) +
    scale_y_continuous(labels = si_labels(digits = 1, width = 8)) +
    labs(x = "", y = paste0("Monthly ", virus, " Cases"), fill = "Cohort") +
    scale_fill_brewer(type = "seq", palette = "Set2") +
    theme(
      strip.text = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )
}

rsv_area <- stacked("RSV", use_area = TRUE)
flu_area <- stacked("Influenza", use_area = TRUE)
covid_area <- stacked("COVID-19", use_area = TRUE)
fig_area <- ggarrange(
  rsv_area, flu_area, covid_area,
  ncol = 1, common.legend = TRUE
)

rsv_bar <- stacked("RSV")
flu_bar <- stacked("Influenza")
covid_bar <- stacked("COVID-19")
fig_bar <- ggarrange(
  rsv_bar, flu_bar, covid_bar,
  ncol = 1, common.legend = TRUE
)
