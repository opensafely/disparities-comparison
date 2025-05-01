library(tidyverse)
library(here)
library(lubridate)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

cohort <- "older_adults"

#import data
df_rsv <- read_csv(here::here(#"post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_rsv.csv"))) %>%
  mutate(virus = "RSV") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))

#import data
df_flu <- read_csv(here::here(#"post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_flu.csv"))) %>%
  mutate(virus = "Influenza") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))

#import data
df_covid <- read_csv(here::here(#"post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_covid.csv"))) %>%
  mutate(virus = "COVID-19") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))

#import data
df_overall <- read_csv(here::here(#"post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_overall_resp.csv"))) %>%
  mutate(virus = "Overall Respiratory Viruses") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))


#collate
df_all <- bind_rows(df_rsv, df_flu, df_covid, df_overall) %>%
  arrange(interval_start) %>%
  mutate(
    week = floor_date(interval_start, unit = "week"),
    type = "EHR",
    event = case_when(
      str_detect(event, "primary") ~ "Mild",
      str_detect(event, "secondary") ~ "Severe"
    )
  )

df_all_rates <- df_all %>%
  select(week, event, rate_1000_py_midpoint10_derived,
         codelist_type, virus, type)

coeff <- 200
df_all_covid <- df_all %>%
  select(-c(interval_start, rate_1000_py_midpoint10_derived)) %>%
  filter(virus == "COVID-19") %>%
  mutate(
    total_events = if_else(is.na(total_events), 0, total_events/coeff),
  )
  
df_all <- rbind(df_all %>%
  select(-c(interval_start, rate_1000_py_midpoint10_derived)) %>%
  filter(virus != "COVID-19"),
  df_all_covid
  ) %>%
  arrange(week)
  
df_all <- df_all[, c("week", "event", "total_events",
                     "codelist_type", "virus", "type")]

#import surveillance data
df_surv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "seasonality_weekly.csv")) %>%
  arrange(week) %>%
  mutate(
    total_overall = total_rsv + total_flu + total_covid,
    type = "Surveillance") %>%
  select(-total_covid) %>%
  pivot_longer(
    cols = c(total_rsv, total_flu, covid_scaled, total_overall),
    names_to = "virus",
    values_to = "total_events"
  ) %>%
  mutate(
    virus = case_when(
      virus == "total_rsv" ~ "RSV",
      virus == "total_flu" ~ "Influenza",
      virus == "covid_scaled" ~ "COVID-19",
      virus == "total_overall" ~ "Overall Respiratory Viruses"
    ),
    event = "Surveillance"
  )

df_surv <- df_surv[, c("week", "event", "total_events", "virus", "type")]
  
#combine
df_combined <- full_join(
  df_all, df_surv, by = c("week", "event", "total_events", "virus", "type")) %>%
  arrange(week) %>%
  mutate(week = ymd(week))

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

df_combined %>%
  filter(virus != "Overall Respiratory Viruses") %>%
  ggplot() +
  # Primary axis viruses
  geom_line(data = ~filter(.x, virus != "COVID-19"),
            aes(x = week, y = total_events, color = virus)) +
  # Secondary axis virus (COVID-19)
  geom_line(data = ~filter(.x, virus == "COVID-19"),
            aes(x = week, y = total_events, color = virus)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  annotate(label = "Usual Transmission Period (Nov-Mar)",
           x = as.Date("2019-12-15"), 
           y = 5000, geom = "text", col = "black", size = 4) +
  facet_wrap(~event, nrow = 1, scales = "free") +
  scale_y_continuous(
    name = "RSV/Influenza Weekly Events",
    sec.axis = sec_axis(~.*coeff, name = "COVID-19 Weekly Events")
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
                                "COVID-19" = "#E41A1C" )) +
  labs(
    title = paste0("Weekly counts of RSV, Flu and COVID-19 in ",
                   str_to_title(gsub("_", " ", cohort))),
    x = "Year", colour = "Disease"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "seasonality_comparisons.png"), width = 12, height = 8)
