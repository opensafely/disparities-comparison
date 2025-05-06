library(tidyverse)
library(here)
library(lubridate)
library(data.table)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

##-- older adults

cohort <- "older_adults"

#import data
df_rsv <- read_csv(here::here("post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_rsv.csv"))) %>%
  mutate(virus = "RSV") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))

all_intervals <- df_rsv %>% select(interval_start)

#import data
df_flu <- read_csv(here::here("post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_flu.csv"))) %>%
  mutate(virus = "Influenza") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))

#import data
df_covid <- read_csv(here::here("post_check",
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
df_overall <- read_csv(here::here("post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_overall_resp.csv"))) %>%
  mutate(virus = "Overall Respiratory Viruses") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))


#collate
df_all <- bind_rows(df_rsv, df_flu, df_covid, df_overall) %>%
  arrange(interval_start) %>%
  mutate(
    month = ymd(floor_date(interval_start, unit = "month")),
    type = "EHR",
    event = case_when(
      str_detect(event, "primary") ~ "Mild",
      str_detect(event, "secondary") ~ "Severe"
    )
  ) %>%
  filter(month >= "2016-09-01") %>%
  slice_min(order_by = interval_start, n = 1, by = c("month", "event"))

df_all_rates <- df_all %>%
  select(month, event, rate_1000_py_midpoint10_derived,
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
  arrange(month)
  
df_all <- df_all[, c("month", "event", "total_events",
                     "codelist_type", "virus", "type")]

#import surveillance data
df_surv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "seasonality.csv")) %>%
  arrange(month) %>%
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

df_surv <- df_surv[, c("month", "event", "total_events", "virus", "type")]

df_surv <- bind_rows(
  df_surv %>%
    mutate(codelist_type = "specific"),
  df_surv %>%
    mutate(codelist_type = "sensitive")
)
  
#combine
df_combined <- full_join(
  df_all, df_surv, by = c("month", "event", "total_events", "virus", "type",
                          "codelist_type")) %>%
  arrange(month) %>%
  mutate(
    total_events = if_else(is.na(total_events), 0 , total_events)
  )

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

spec <- df_combined %>%
  filter(virus != "Overall Respiratory Viruses",
         codelist_type == "specific") %>%
  ggplot() +
  geom_line(data = ~filter(.x, virus != "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_line(data = ~filter(.x, virus == "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    name = "",
    sec.axis = sec_axis(~.*coeff, name = "")
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
                                "COVID-19" = "#E41A1C" )) +
  labs(x = "", colour = "Virus") +
  theme_bw() +
  theme(legend.position = "none")

sens <- df_combined %>%
  filter(virus != "Overall Respiratory Viruses",
         codelist_type == "sensitive") %>%
  ggplot() +
  geom_line(data = ~filter(.x, virus != "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_line(data = ~filter(.x, virus == "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    name = "",
    sec.axis = sec_axis(~.*coeff, name = "")
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
                                "COVID-19" = "#E41A1C" )) +
  labs(x = "", colour = "Virus") +
  theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  spec + theme(legend.position = "bottom",
               legend.box = "horizontal",
               legend.title = element_text())
)

plot <- plot_grid(
  spec,
  sens,
  nrow = 2,
  label_size = 14
) %>% annotate_figure(
  top = text_grob("Specific Phenotype", vjust = 0.5, size = 12,
                  face = "italic"),
  bottom = text_grob("Sensitive Phenotype", vjust = -20, size = 12,
                     face = "italic"),
  left = text_grob("RSV/Influenza Monthly Events", rot = 90, vjust = 1),
  right = text_grob("COVID-19 Monthly Events", rot = 270, vjust = 1)
)

# Dummy data for the grey box
legend_df <- data.frame(
  xmin = 0.5, xmax = 1,
  ymin = 0.25, ymax = 0.75
)

transmission_legend <- ggplot() +
  # Grey square box
  geom_rect(data = legend_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, color = NA) +
  # Label to the right
  annotate("text", x = 1.2, y = 0.5,
           label = "Usual Transmission Period (Nov–Mar)",
           hjust = 0, vjust = 0.5, size = 4) +
  xlim(0, 3) + ylim(0, 1) +  # Give horizontal space
  theme_void() +
  theme(plot.margin = margin(5, 5, 5, 5))

bottom_row <- plot_grid(
  transmission_legend,
  legend,
  ncol = 2,
  rel_widths = c(1, 1)
)

plot_grid(
  plot,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, .1)
) %>% annotate_figure(
    top = text_grob(paste0(
      "Monthly Counts of RSV, Influenza and COVID-19 in ",
      str_to_title(gsub("_", " ", cohort))), face = "bold", size = 14),
    bottom = text_grob("Year", vjust = -6)
  )

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
  arrange(month) %>%
  mutate(total_events = if_else(virus == "COVID-19",
                                total_events * coeff, total_events))

df_comp <- df_combined_expanded %>%
  pivot_wider(
    names_from = type,
    values_from = total_events,
    values_fill = 0
  )

rsv <- df_comp %>%
  filter(virus == "RSV", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#377EB8") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

flu <- df_comp %>%
  filter(virus == "Influenza", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#4DAF4A") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

covid <- df_comp %>%
  filter(virus == "COVID-19", month >= as.Date("2020-03-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#E41A1C") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
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
  top = text_grob(paste0(
    "Surveillance VS EHR: monthly counts of RSV, Influenza and COVID-19 in ",
    str_to_title(gsub("_", " ", cohort))), face = "bold", size = 14))
plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  paste0(cohort, "_seasonality_comparisons_x_vs_y.png")),
       width = 18, height = 6)

##-- infants

cohort <- "infants"

#import data
df_rsv <- read_csv(here::here("post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_rsv.csv"))) %>%
  mutate(virus = "RSV") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))

all_intervals <- df_rsv %>% select(interval_start)

#import data
df_flu <- read_csv(here::here("post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_flu.csv"))) %>%
  mutate(virus = "Influenza") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))

#import data
df_covid <- read_csv(here::here("post_check",
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
df_overall <- read_csv(here::here("post_check",
  "output", "collated", "descriptive", "over_time", paste0(cohort,
  "_", "rates_over_time_all_all_overall_resp.csv"))) %>%
  mutate(virus = "Overall Respiratory Viruses") %>%
  select(c(interval_start, event, total_events = total_events_midpoint10,
           rate_1000_py_midpoint10_derived, codelist_type, virus))


#collate
df_all <- bind_rows(df_rsv, df_flu, df_covid, df_overall) %>%
  arrange(interval_start) %>%
  mutate(
    month = ymd(floor_date(interval_start, unit = "month")),
    type = "EHR",
    event = case_when(
      str_detect(event, "primary") ~ "Mild",
      str_detect(event, "secondary") ~ "Severe"
    )
  ) %>%
  filter(month >= "2016-09-01") %>%
  slice_min(order_by = interval_start, n = 1, by = c("month", "event"))

df_all_rates <- df_all %>%
  select(month, event, rate_1000_py_midpoint10_derived,
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
  arrange(month)
  
df_all <- df_all[, c("month", "event", "total_events",
                     "codelist_type", "virus", "type")]

#import surveillance data
df_surv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "seasonality.csv")) %>%
  arrange(month) %>%
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

df_surv <- df_surv[, c("month", "event", "total_events", "virus", "type")]

df_surv <- bind_rows(
  df_surv %>%
    mutate(codelist_type = "specific"),
  df_surv %>%
    mutate(codelist_type = "sensitive")
)
  
#combine
df_combined <- full_join(
  df_all, df_surv, by = c("month", "event", "total_events", "virus", "type",
                          "codelist_type")) %>%
  arrange(month) %>%
  mutate(
    total_events = if_else(is.na(total_events), 0 , total_events)
  )

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

spec <- df_combined %>%
  filter(virus != "Overall Respiratory Viruses",
         codelist_type == "specific") %>%
  ggplot() +
  geom_line(data = ~filter(.x, virus != "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_line(data = ~filter(.x, virus == "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    name = "",
    sec.axis = sec_axis(~.*coeff, name = "")
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
                                "COVID-19" = "#E41A1C" )) +
  labs(x = "", colour = "Virus") +
  theme_bw() +
  theme(legend.position = "none")

sens <- df_combined %>%
  filter(virus != "Overall Respiratory Viruses",
         codelist_type == "sensitive") %>%
  ggplot() +
  geom_line(data = ~filter(.x, virus != "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_line(data = ~filter(.x, virus == "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    name = "",
    sec.axis = sec_axis(~.*coeff, name = "")
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
                                "COVID-19" = "#E41A1C" )) +
  labs(x = "", colour = "Virus") +
  theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  spec + theme(legend.position = "bottom",
               legend.box = "horizontal",
               legend.title = element_text())
)

plot <- plot_grid(
  spec,
  sens,
  nrow = 2,
  label_size = 14
) %>% annotate_figure(
  top = text_grob("Specific Phenotype", vjust = 0.5, size = 12,
                  face = "italic"),
  bottom = text_grob("Sensitive Phenotype", vjust = -20, size = 12,
                     face = "italic"),
  left = text_grob("RSV/Influenza Monthly Events", rot = 90, vjust = 1),
  right = text_grob("COVID-19 Monthly Events", rot = 270, vjust = 1)
)

# Dummy data for the grey box
legend_df <- data.frame(
  xmin = 0.5, xmax = 1,
  ymin = 0.25, ymax = 0.75
)

transmission_legend <- ggplot() +
  # Grey square box
  geom_rect(data = legend_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, color = NA) +
  # Label to the right
  annotate("text", x = 1.2, y = 0.5,
           label = "Usual Transmission Period (Nov–Mar)",
           hjust = 0, vjust = 0.5, size = 4) +
  xlim(0, 3) + ylim(0, 1) +  # Give horizontal space
  theme_void() +
  theme(plot.margin = margin(5, 5, 5, 5))

bottom_row <- plot_grid(
  transmission_legend,
  legend,
  ncol = 2,
  rel_widths = c(1, 1)
)

plot_grid(
  plot,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, .1)
) %>% annotate_figure(
    top = text_grob(paste0(
      "Monthly Counts of RSV, Influenza and COVID-19 in ",
      str_to_title(gsub("_", " ", cohort))), face = "bold", size = 14),
    bottom = text_grob("Year", vjust = -6)
  )

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
  arrange(month) %>%
  mutate(total_events = if_else(virus == "COVID-19",
                                total_events * coeff, total_events))

df_comp <- df_combined_expanded %>%
  pivot_wider(
    names_from = type,
    values_from = total_events,
    values_fill = 0
  )

rsv <- df_comp %>%
  filter(virus == "RSV", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#377EB8") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

flu <- df_comp %>%
  filter(virus == "Influenza", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#4DAF4A") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

covid <- df_comp %>%
  filter(virus == "COVID-19", month >= as.Date("2020-03-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#E41A1C") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
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
  top = text_grob(paste0(
    "Surveillance VS EHR: monthly counts of RSV, Influenza and COVID-19 in ",
    str_to_title(gsub("_", " ", cohort))), face = "bold", size = 14))
plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  paste0(cohort, "_seasonality_comparisons_x_vs_y.png")),
       width = 18, height = 6)

##-- comparing overall cases

#create function to import data
import <- function(pathogen, cohort) {
  
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  df <- read_csv(here::here(
    "post_check", "output", "collated", "descriptive", "over_time",
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
    select(c(interval_start, month, event,
             total_events = total_events_midpoint10,
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

#import surveillance data
df_surv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "seasonality.csv")) %>%
  arrange(month) %>%
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

df_surv <- df_surv[, c("month", "event", "total_events", "virus")]

df_surv <- bind_rows(
  df_surv %>%
    mutate(codelist_type = "specific"),
  df_surv %>%
    mutate(codelist_type = "sensitive")
)

df_all <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>%
  arrange(month)

coeff <- 200
df_all_covid <- df_all %>%
  filter(virus == "COVID-19") %>%
  mutate(
    total_events = if_else(is.na(total_events), 0, total_events/coeff),
  )

df_all <- rbind(df_all %>%
                  filter(virus != "COVID-19"),
                df_all_covid
) %>%
  arrange(month)

#combine
df_combined <- full_join(
  df_all, df_surv, by = c("month", "event", "total_events", "virus",
                          "codelist_type")) %>%
  arrange(month) %>%
  mutate(
    total_events = if_else(is.na(total_events), 0 , total_events)
  )

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

spec <- df_combined %>%
  filter(virus != "Overall Respiratory Viruses",
         codelist_type == "specific") %>%
  ggplot() +
  geom_line(data = ~filter(.x, virus != "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_line(data = ~filter(.x, virus == "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    name = "",
    sec.axis = sec_axis(~.*coeff, name = "")
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
                                "COVID-19" = "#E41A1C" )) +
  labs(x = "", colour = "Virus") +
  theme_bw() +
  theme(legend.position = "none")

sens <- df_combined %>%
  filter(virus != "Overall Respiratory Viruses",
         codelist_type == "sensitive") %>%
  ggplot() +
  geom_line(data = ~filter(.x, virus != "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_line(data = ~filter(.x, virus == "COVID-19"),
            aes(x = month, y = total_events, color = virus)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    name = "",
    sec.axis = sec_axis(~.*coeff, name = "")
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values = c("RSV" = "#377EB8", "Influenza" = "#4DAF4A",
                                "COVID-19" = "#E41A1C" )) +
  labs(x = "", colour = "Virus") +
  theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  spec + theme(legend.position = "bottom",
               legend.box = "horizontal",
               legend.title = element_text())
)

plot <- plot_grid(
  spec,
  sens,
  nrow = 2,
  label_size = 14
) %>% annotate_figure(
  top = text_grob("Specific Phenotype", vjust = 0.5, size = 12,
                  face = "italic"),
  bottom = text_grob("Sensitive Phenotype", vjust = -20, size = 12,
                     face = "italic"),
  left = text_grob("RSV/Influenza Monthly Events", rot = 90, vjust = 1),
  right = text_grob("COVID-19 Monthly Events", rot = 270, vjust = 1)
)

# Dummy data for the grey box
legend_df <- data.frame(
  xmin = 0.5, xmax = 1,
  ymin = 0.25, ymax = 0.75
)

transmission_legend <- ggplot() +
  # Grey square box
  geom_rect(data = legend_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, color = NA) +
  # Label to the right
  annotate("text", x = 1.2, y = 0.5,
           label = "Usual Transmission Period (Nov–Mar)",
           hjust = 0, vjust = 0.5, size = 4) +
  xlim(0, 3) + ylim(0, 1) +  # Give horizontal space
  theme_void() +
  theme(plot.margin = margin(5, 5, 5, 5))

bottom_row <- plot_grid(
  transmission_legend,
  legend,
  ncol = 2,
  rel_widths = c(1, 1)
)

plot_grid(
  plot,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, .1)
) %>% annotate_figure(
  top = text_grob(
    "Monthly Counts of RSV, Influenza and COVID-19 in ",
    face = "bold", size = 14),
  bottom = text_grob("Year", vjust = -6)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons.png"),
       width = 18, height = 6)

df_all_cohorts <- bind_rows(
  df_rsv %>% mutate(type = "EHR"),
  df_flu %>% mutate(type = "EHR"),
  df_covid %>% mutate(type = "EHR"),
  df_surv %>% mutate(type = "Surveillance")
) %>%
  arrange(month)

# Define the 4 combinations you want for Surveillance
event_combinations <- crossing(
  event = c("Mild", "Severe"),
  codelist_type = c("specific", "sensitive")
)

# Separate Surveillance and EHR rows
surv_expanded_all_cohorts <- df_all_cohorts %>%
  filter(type == "Surveillance") %>%
  mutate(event = NA) %>%
  select(-event, -codelist_type) %>%
  crossing(event_combinations)

ehr_rows_all_cohorts <- df_all_cohorts %>%
  filter(type == "EHR")

# Combine both
df_combined_expanded_all_cohorts <- bind_rows(
  ehr_rows_all_cohorts, surv_expanded_all_cohorts) %>%
  arrange(month) %>%
  mutate(total_events = if_else(virus == "COVID-19",
                                total_events * coeff, total_events))

df_comp_all_cohorts <- df_combined_expanded_all_cohorts %>%
  pivot_wider(
    names_from = type,
    values_from = total_events,
    values_fill = 0
  )

rsv_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "RSV", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#377EB8") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

flu_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "Influenza", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#4DAF4A") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

covid_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "COVID-19", month >= as.Date("2020-03-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type),
             color = "#E41A1C") +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     na.translate = FALSE) +
  labs(x = "", y = "", alpha = "Phenotype Used") + theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  rsv + theme(legend.position = "bottom",
              legend.box = "horizontal",
              legend.title = element_text())
)

plot <- plot_grid(
  rsv_all_cohorts,
  flu_all_cohorts,
  covid_all_cohorts, 
  nrow = 1
) %>% annotate_figure(
  left = text_grob("Surveillance", rot = 90, vjust = 1),
  bottom = text_grob("EHR"),
  top = text_grob(
    "Surveillance VS EHR: monthly counts of RSV, Influenza and COVID-19 in ",
    face = "bold", size = 14))
plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_x_vs_y.png"),
       width = 18, height = 6)
