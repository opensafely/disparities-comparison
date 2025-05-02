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

all_intervals <- df_rsv %>% select(interval_start)

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

df_covid <- bind_rows(
  df_covid,
  all_intervals %>% filter(!(interval_start %in% df_covid$interval_start)) %>%
    mutate(event = "covid_primary_date",
           total_events = 0,
           rate_1000_py_midpoint10_derived = 0,
           codelist_type = "specific",
           virus = "COVID-19"),
  all_intervals %>% filter(!(interval_start %in% df_covid$interval_start)) %>%
    mutate(event = "covid_secondary_date",
           total_events = 0,
           rate_1000_py_midpoint10_derived = 0,
           codelist_type = "specific",
           virus = "COVID-19"),
  all_intervals %>% filter(!(interval_start %in% df_covid$interval_start)) %>%
    mutate(
      event = "covid_primary_date",
      total_events = 0,
      rate_1000_py_midpoint10_derived = 0,
      codelist_type = "sensitive",
      virus = "COVID-19"),
  all_intervals %>% filter(!(interval_start %in% df_covid$interval_start)) %>%
    mutate(
      event = "covid_secondary_date",
      total_events = 0,
      rate_1000_py_midpoint10_derived = 0,
      codelist_type = "sensitive",
      virus = "COVID-19")) %>%
  unique() %>%
  arrange(interval_start)

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
    event = "Surveillance",
    codelist_type = "surveillance",
  )

df_surv <- df_surv[, c("week", "event", "total_events", "virus", "type")]
  
#combine
df_combined <- full_join(
  df_all, df_surv, by = c("week", "event", "total_events", "virus", "type")) %>%
  arrange(week) %>%
  mutate(week = ymd(week),
         total_events = if_else(is.na(total_events), 0 , total_events)
  )

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
            aes(x = week, y = total_events, color = virus, alpha = codelist_type)) +
  # Secondary axis virus (COVID-19)
  geom_line(data = ~filter(.x, virus == "COVID-19"),
            aes(x = week, y = total_events, color = virus, alpha = codelist_type)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  annotate(label = "Usual Transmission Period (Nov-Mar)",
           x = as.Date("2020-12-15"), 
           y = 5000, geom = "text", col = "black", size = 4) +
  facet_wrap(~event, nrow = 1, scales = "free") +
  scale_y_continuous(
    name = "RSV/Influenza Weekly Events",
    sec.axis = sec_axis(~.*coeff, name = "COVID-19 Weekly Events")
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
                                "COVID-19" = "#E41A1C" )) +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
  labs(
    title = paste0("Weekly counts of RSV, Flu and COVID-19 in ",
                   str_to_title(gsub("_", " ", cohort))),
    x = "Year", colour = "Disease", alpha = "Phenotype Used"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  paste0(cohort, "_seasonality_comparisons.png")),
                  width = 18, height = 6)

#visual comparisons of surveillance and EHR data per virus
# Define the 4 combinations you want for Surveillance
event_combinations <- crossing(
  event = c("Mild", "Severe"),
  codelist_type = c("specific", "sensitive")
)

# Separate Surveillance and EHR rows
surv_expanded <- df_combined %>%
  filter(type == "Surveillance") %>%
  mutate(event = NA) %>%
  select(-event, -codelist_type) %>%
  crossing(event_combinations)

ehr_rows <- df_combined %>%
  filter(type == "EHR")

# Combine both
df_combined_expanded <- bind_rows(ehr_rows, surv_expanded) %>%
  arrange(week) %>%
  mutate(total_events = if_else(virus == "COVID-19",
                                total_events * coeff, total_events))

df_comp <- df_combined_expanded %>%
  pivot_wider(
    names_from = type,
    values_from = total_events,
    values_fill = 0
  ) %>%
  mutate(week = ymd(week))

rsv <- df_comp %>%
  filter(virus == "RSV", week >= as.Date("2016-09-01")) %>%
  arrange(week) %>%
  ggplot() +
  # Primary axis viruses
  geom_line(aes(x = EHR, y = Surveillance, alpha = codelist_type),
            color = "#377EB8") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  # scale_y_continuous(
  #   name = "RSV/Influenza Weekly Events",
  #   sec.axis = sec_axis(~.*coeff, name = "COVID-19 Weekly Events")
  # ) +
  # scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  # scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
  #                               "COVID-19" = "#E41A1C" )) +
  # scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
  #                    labels = c("sensitive" = "Sensitive",
  #                               "specific" = "Specific"),
  #                    na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

flu <- df_comp %>%
  filter(virus == "Influenza", week >= as.Date("2016-09-01")) %>%
  arrange(week) %>%
  ggplot() +
  # Primary axis viruses
  geom_line(aes(x = EHR, y = Surveillance, alpha = codelist_type),
            color = "#4DAF4A") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  # scale_y_continuous(
  #   name = "RSV/Influenza Weekly Events",
  #   sec.axis = sec_axis(~.*coeff, name = "COVID-19 Weekly Events")
  # ) +
  # scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  # scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
  #                               "COVID-19" = "#E41A1C" )) +
  # scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
  #                    labels = c("sensitive" = "Sensitive",
  #                               "specific" = "Specific"),
  #                    na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

covid <- df_comp %>%
  filter(virus == "COVID-19", week >= as.Date("2020-03-01")) %>%
  arrange(week) %>%
  ggplot() +
  # Primary axis viruses
  geom_line(aes(x = EHR, y = Surveillance, alpha = codelist_type),
            color = "#E41A1C") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  # scale_y_continuous(
  #   name = "RSV/Influenza Weekly Events",
  #   sec.axis = sec_axis(~.*coeff, name = "COVID-19 Weekly Events")
  # ) +
  # scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  # scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
  #                               "COVID-19" = "#E41A1C" )) +
  # scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
  #                    labels = c("sensitive" = "Sensitive",
  #                               "specific" = "Specific"),
  #                    na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  rsv + theme(legend.position = "bottom",
             legend.box = "horizontal",
             legend.title = element_text())
)

plot <- plot_grid(
  rsv,
  flu,
  covid, 
  nrow = 1
) %>% annotate_figure(
  left = text_grob("Surveillance", rot = 90, vjust = 1),
  bottom = text_grob("EHR"),
  top = text_grob(paste0("Weekly counts of RSV, Influenza and COVID-19 in ",
                         str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14))
plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
)

# #save
# ggsave(here::here("post_check", "plots", "exploratory_analyses",
#                   paste0(cohort, "_seasonality_comparisons.png")),
#        width = 18, height = 6)
