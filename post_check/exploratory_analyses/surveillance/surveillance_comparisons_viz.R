library(tidyverse)
library(here)
library(lubridate)
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(cowplot)
library(ggpubr)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

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

f <- function(pal) brewer.pal(3, pal)
cols <- f("Set2")

plot_combined <- function(df, pathogen, phenotype) {
  
  coeff_mild <- df_surv_mute %>%
    filter(virus == !!pathogen, event == "Mild") %>%
    select(coeff) %>%
    pull() %>%
    unique()
  
  coeff_severe <- df_surv_mute %>%
    filter(virus == !!pathogen, event == "Severe") %>%
    select(coeff) %>%
    pull() %>%
    unique()
  
  surv_filt <- df_surv_mute %>%
    filter(virus == !!pathogen) %>%
    mutate(type = "Surveillance")
  
  all_filt <- df %>%
    filter(virus == !!pathogen,
           codelist_type == !!phenotype) %>%
    mutate(type = "EHR")
  
  df_plot <- bind_rows(
    all_filt,
    surv_filt %>%
      select(-coeff) %>%
      mutate(codelist_type = "surveillance")
  ) %>%
    arrange(month)
  
  mild <- df_plot %>%
    filter(event == "Mild") %>%
    ggplot() +
    geom_line(aes(x = month, y = total_events, color = virus,
                  alpha = type), linewidth = 1) +
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                                ymin = ymin, ymax = ymax),
              fill = "grey", alpha = 0.25, col = NA) +
    scale_y_continuous(
      sec.axis = sec_axis(trans = ~./coeff_mild, name = "Surveillance Data")
    ) +
    scale_x_date(date_breaks = "1 years", date_labels = "%y") + 
    scale_color_manual(values = c(
      "RSV" = cols[1], "Influenza" = cols[2], "COVID-19" = cols[3],
      "Overall Respiratory Viruses" = "#4C0227")) +
    scale_alpha_manual(values = c("Surveillance" = 0.5,
                                  "EHR" = 1),
                       na.translate = FALSE) +
    labs(x = "", y = "", colour = "Virus", title = "Mild") + theme_bw() +
    theme(legend.position = "none",
          title = element_text(face = "italic", size = 8)) +
    ggeasy::easy_center_title()
  
  severe <- df_plot %>%
    filter(event == "Severe") %>%
    ggplot() +
    geom_line(aes(x = month, y = total_events, color = virus,
                  alpha = type), linewidth = 1) +
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                                ymin = ymin, ymax = ymax),
              fill = "grey", alpha = 0.25, col = NA) +
    scale_y_continuous(
      sec.axis = sec_axis(trans = ~./coeff_severe, name = "Surveillance Data")
    ) +
    scale_x_date(date_breaks = "1 years", date_labels = "%y") + 
    scale_color_manual(values = c(
      "RSV" = cols[1], "Influenza" = cols[2], "COVID-19" = cols[3],
      "Overall Respiratory Viruses" = "#4C0227")) +
    scale_alpha_manual(values = c("Surveillance" = 0.5,
                                  "EHR" = 1),
                       na.translate = FALSE) +
    labs(x = "", y = "", colour = "Virus", title = "Severe") + theme_bw() +
    theme(legend.position = "none",
          title = element_text(face = "italic", size = 8)) +
    ggeasy::easy_center_title()
  
  plot_grid(mild, severe, nrow = 1) %>%
    annotate_figure(
      top = text_grob(
        paste0(pathogen, " Events (", str_to_title(phenotype),
               " Phenotype)"),
        face = "bold", size = 10)
    )
  
}

get_legend_2 <- function(df1, df2) {
  
  surv_filt <- df1 %>%
    mutate(type = "Surveillance")
  
  all_filt <- df2 %>%
    mutate(type = "EHR")
  
  df_plot <- bind_rows(
    all_filt,
    surv_filt %>%
      select(-coeff)
  ) %>%
    arrange(month)
  
  legend <- get_legend(
    df_plot %>%
      filter(virus != "Overall Respiratory Viruses") %>%
      ggplot() +
      geom_line(aes(x = month, y = total_events, color = factor(
        virus, levels = c("RSV", "Influenza", "COVID-19")),
        alpha = factor(type, levels = c("EHR", "Surveillance"))),
        linewidth = 1) +
      scale_color_manual(values = c(
        "RSV" = cols[1], "Influenza" = cols[2], "COVID-19" = cols[3])) +
      scale_alpha_manual(values = c("Surveillance" = 0.5,
                                    "EHR" = 1),
                         na.translate = FALSE) +
      labs(x = "", y = "", colour = "Virus", alpha = " Data Source") +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.box = "horizontal",
            legend.title = element_text())
  )
  
  return(legend)
  
}

##-- older adults

#import data
df_rsv_older_adults <- import("rsv", "older_adults")

df_rsv <- as.data.table(df_rsv_older_adults) %>%
  arrange(month)
df_rsv <- df_rsv[, total_events := sum(total_events, na.rm = T),
                 by = .(month, event, codelist_type, virus)] %>%
  unique()

#import data
df_flu_older_adults <- import("flu", "older_adults")

df_flu <- as.data.table(df_flu_older_adults) %>%
  arrange(month)
df_flu <- df_flu[, total_events := sum(total_events, na.rm = T),
                 by = .(month, event, codelist_type, virus)] %>%
  unique()

#import data
df_covid_older_adults <- import("covid", "older_adults")

df_covid <- as.data.table(df_covid_older_adults) %>%
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
  select(-covid_scaled) %>%
  pivot_longer(
    cols = c(total_rsv, total_flu, total_covid, total_overall),
    names_to = "virus",
    values_to = "total_events"
  ) %>%
  mutate(
    virus = case_when(
      virus == "total_rsv" ~ "RSV",
      virus == "total_flu" ~ "Influenza",
      virus == "total_covid" ~ "COVID-19",
      virus == "total_overall" ~ "Overall Respiratory Viruses"
    ),
    event = "Surveillance",
    codelist_type = "surveillance",
  )

df_surv <- df_surv[, c("month", "event", "total_events", "virus")]

df_surv_mute <- bind_rows(
  df_surv %>%
    select(-event) %>%
    mutate(event = "Mild"),
  df_surv %>%
    select(-event) %>%
    mutate(event = "Severe")
)

df_surv <- bind_rows(
  df_surv %>%
    mutate(codelist_type = "specific"),
  df_surv %>%
    mutate(codelist_type = "specific")
)

df_surv_mute <- df_surv_mute %>%
  mutate(
    coeff = case_when(
      virus == "RSV" & event == "Mild" ~ 20,
      virus == "RSV" & event == "Severe" ~ 10,
      virus == "Influenza" & event == "Mild" ~ 1,
      virus == "Influenza" & event == "Severe" ~ 1,
      virus == "COVID-19" & event == "Mild" ~ 0.5,
      virus == "COVID-19" & event == "Severe" ~ 0.01,
    ),
    total_events = if_else(is.na(total_events), 0, total_events*coeff)
  ) %>%
  arrange(month)

df_all <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>%
  arrange(month)

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

spec_rsv <- plot_combined(df_all, "RSV", "specific")
spec_flu <- plot_combined(df_all, "Influenza", "specific")
spec_covid <- plot_combined(df_all, "COVID-19", "specific")
sens_rsv <- plot_combined(df_all, "RSV", "sensitive")
sens_flu <- plot_combined(df_all, "Influenza", "sensitive")
sens_covid <- plot_combined(df_all, "COVID-19", "sensitive")

legend <- get_legend_2(df_surv_mute, df_all)

plot <- plot_grid(
  spec_rsv, sens_rsv,
  spec_flu, sens_flu,
  spec_covid, sens_covid,
  ncol = 2,
  label_size = 14
) %>% annotate_figure(
  left = text_grob("Monthly Events Identified", rot = 90, vjust = 2)
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
    "Monthly Counts of RSV, Influenza and COVID-19 in Older Adults",
    face = "bold", size = 14),
  bottom = text_grob("Year (2016-2024)", vjust = -4)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "older_adults_seasonality_comparisons_2.png"),
       width = 18, height = 6)

##x v y comparisons

df_older_adults <- bind_rows(
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
surv_expanded_older_adults <- df_older_adults %>%
  filter(type == "Surveillance") %>%
  mutate(event = NA) %>%
  select(-event, -codelist_type) %>%
  crossing(event_combinations)

ehr_rows_older_adults <- df_older_adults %>%
  filter(type == "EHR")

# Combine both
df_combined_expanded_older_adults <- bind_rows(
  ehr_rows_older_adults, surv_expanded_older_adults) %>%
  arrange(month)

df_comp_older_adults <- df_combined_expanded_older_adults %>%
  pivot_wider(
    names_from = type,
    values_from = total_events,
    values_fill = 0
  )

rsv_older_adults <- df_comp_older_adults %>%
  filter(virus == "RSV", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type,
                 shape = codelist_type), color = cols[1], size = 3.5) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_shape_manual(values = c("sensitive" = 15, "specific" = 17),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  labs(x = "", y = "") + theme_bw() +
  theme(legend.position = "none")

flu_older_adults <- df_comp_older_adults %>%
  filter(virus == "Influenza", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type,
                 shape = codelist_type), color = cols[2], size = 3.5) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_shape_manual(values = c("sensitive" = 15, "specific" = 17),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  labs(x = "", y = "") + theme_bw() +
  theme(legend.position = "none")

covid_older_adults <- df_comp_older_adults %>%
  filter(virus == "COVID-19", month >= as.Date("2020-03-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type,
                 shape = codelist_type), color = cols[3], size = 3.5) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_shape_manual(values = c("sensitive" = 15, "specific" = 17),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  labs(x = "", y = "") + theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  df_comp_older_adults %>%
    filter(month >= as.Date("2016-09-01")) %>%
    ggplot() +
    # Primary axis viruses
    geom_point(aes(x = EHR, y = Surveillance, alpha = factor(
      codelist_type, levels = c("specific", "sensitive")),
      color = factor(
        virus, levels = c("RSV", "Influenza", "COVID-19")),
      shape = factor(
        codelist_type, levels = c("specific", "sensitive"))), size = 3.5) +
    scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                       labels = c("sensitive" = "Sensitive",
                                  "specific" = "Specific"),
                       name = "Phenotype Used",
                       na.translate = FALSE) +
    scale_shape_manual(values = c("sensitive" = 15, "specific" = 17),
                       labels = c("sensitive" = "Sensitive",
                                  "specific" = "Specific"),
                       name = "Phenotype Used",
                       na.translate = FALSE) +
    scale_color_manual(values = c(
      "RSV" = cols[1], "Influenza" = cols[2], "COVID-19" = cols[3]),
      na.translate = F) +
    guides(colour = guide_legend(order = 1),
           alpha = guide_legend("Phenotype Used", order = 2),
           shape = guide_legend("Phenotype Used", order = 2)) +
    labs(color = "Virus") + theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text())
)

plot <- plot_grid(
  rsv_older_adults,
  flu_older_adults,
  covid_older_adults, 
  nrow = 1
) %>% annotate_figure(
  left = text_grob("Surveillance", rot = 90, vjust = 1),
  bottom = text_grob("EHR"),
  top = text_grob(
    "Surveillance VS EHR: Monthly Counts of RSV, Influenza and COVID-19 in Older Adults",
    face = "bold", size = 14))
plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
  )

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "older_adults_seasonality_comparisons_x_vs_y_2.png"),
       width = 18, height = 6)


##-- infants

#import data
df_rsv_infants <- import("rsv", "infants")

df_rsv <- as.data.table(df_rsv_infants) %>%
  arrange(month)
df_rsv <- df_rsv[, total_events := sum(total_events, na.rm = T),
                 by = .(month, event, codelist_type, virus)] %>%
  unique()

#import data
df_flu_infants <- import("flu", "infants")

df_flu <- as.data.table(df_flu_infants) %>%
  arrange(month)
df_flu <- df_flu[, total_events := sum(total_events, na.rm = T),
                 by = .(month, event, codelist_type, virus)] %>%
  unique()

#import data
df_covid_infants <- import("covid", "infants")

df_covid <- as.data.table(df_covid_infants) %>%
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
  select(-covid_scaled) %>%
  pivot_longer(
    cols = c(total_rsv, total_flu, total_covid, total_overall),
    names_to = "virus",
    values_to = "total_events"
  ) %>%
  mutate(
    virus = case_when(
      virus == "total_rsv" ~ "RSV",
      virus == "total_flu" ~ "Influenza",
      virus == "total_covid" ~ "COVID-19",
      virus == "total_overall" ~ "Overall Respiratory Viruses"
    ),
    event = "Surveillance",
    codelist_type = "surveillance",
  )

df_surv <- df_surv[, c("month", "event", "total_events", "virus")]

df_surv_mute <- bind_rows(
  df_surv %>%
    select(-event) %>%
    mutate(event = "Mild"),
  df_surv %>%
    select(-event) %>%
    mutate(event = "Severe")
)

df_surv <- bind_rows(
  df_surv %>%
    mutate(codelist_type = "specific"),
  df_surv %>%
    mutate(codelist_type = "specific")
)

df_surv_mute <- df_surv_mute %>%
  mutate(
    coeff = case_when(
      virus == "RSV" & event == "Mild" ~ 20,
      virus == "RSV" & event == "Severe" ~ 10,
      virus == "Influenza" & event == "Mild" ~ 1,
      virus == "Influenza" & event == "Severe" ~ 1,
      virus == "COVID-19" & event == "Mild" ~ 0.5,
      virus == "COVID-19" & event == "Severe" ~ 0.01,
    ),
    total_events = if_else(is.na(total_events), 0, total_events*coeff)
  ) %>%
  arrange(month)

df_all <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>%
  arrange(month)

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

spec_rsv <- plot_combined(df_all, "RSV", "specific")
spec_flu <- plot_combined(df_all, "Influenza", "specific")
spec_covid <- plot_combined(df_all, "COVID-19", "specific")
sens_rsv <- plot_combined(df_all, "RSV", "sensitive")
sens_flu <- plot_combined(df_all, "Influenza", "sensitive")
sens_covid <- plot_combined(df_all, "COVID-19", "sensitive")

legend <- get_legend_2(df_surv_mute, df_all)

plot <- plot_grid(
  spec_rsv, sens_rsv,
  spec_flu, sens_flu,
  spec_covid, sens_covid,
  ncol = 2,
  label_size = 14
) %>% annotate_figure(
  left = text_grob("Monthly Events Identified", rot = 90, vjust = 2)
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
    "Monthly Counts of RSV, Influenza and COVID-19 in Infants",
    face = "bold", size = 14),
  bottom = text_grob("Year (2016-2024)", vjust = -4)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "infants_seasonality_comparisons_2.png"),
       width = 18, height = 6)

##x v y comparisons

df_infants <- bind_rows(
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
surv_expanded_infants <- df_infants %>%
  filter(type == "Surveillance") %>%
  mutate(event = NA) %>%
  select(-event, -codelist_type) %>%
  crossing(event_combinations)

ehr_rows_infants <- df_infants %>%
  filter(type == "EHR")

# Combine both
df_combined_expanded_infants <- bind_rows(
  ehr_rows_infants, surv_expanded_infants) %>%
  arrange(month)

df_comp_infants <- df_combined_expanded_infants %>%
  pivot_wider(
    names_from = type,
    values_from = total_events,
    values_fill = 0
  )

rsv_infants <- df_comp_infants %>%
  filter(virus == "RSV", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type,
                 shape = codelist_type), color = cols[1], size = 3.5) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_shape_manual(values = c("sensitive" = 15, "specific" = 17),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  labs(x = "", y = "") + theme_bw() +
  theme(legend.position = "none")

flu_infants <- df_comp_infants %>%
  filter(virus == "Influenza", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type,
                 shape = codelist_type), color = cols[2], size = 3.5) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_shape_manual(values = c("sensitive" = 15, "specific" = 17),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  labs(x = "", y = "") + theme_bw() +
  theme(legend.position = "none")

covid_infants <- df_comp_infants %>%
  filter(virus == "COVID-19", month >= as.Date("2020-03-01")) %>%
  arrange(month) %>%
  ggplot() +
  # Primary axis viruses
  geom_point(aes(x = EHR, y = Surveillance, alpha = codelist_type,
                 shape = codelist_type), color = cols[3], size = 3.5) +
  facet_wrap(~event, nrow = 1, scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_shape_manual(values = c("sensitive" = 15, "specific" = 17),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  labs(x = "", y = "") + theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  df_comp_infants %>%
    filter(month >= as.Date("2016-09-01")) %>%
    ggplot() +
    # Primary axis viruses
    geom_point(aes(x = EHR, y = Surveillance, alpha = factor(
      codelist_type, levels = c("specific", "sensitive")),
      color = factor(
        virus, levels = c("RSV", "Influenza", "COVID-19")),
      shape = factor(
        codelist_type, levels = c("specific", "sensitive"))), size = 3.5) +
    scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                       labels = c("sensitive" = "Sensitive",
                                  "specific" = "Specific"),
                       name = "Phenotype Used",
                       na.translate = FALSE) +
    scale_shape_manual(values = c("sensitive" = 15, "specific" = 17),
                       labels = c("sensitive" = "Sensitive",
                                  "specific" = "Specific"),
                       name = "Phenotype Used",
                       na.translate = FALSE) +
    scale_color_manual(values = c(
      "RSV" = cols[1], "Influenza" = cols[2], "COVID-19" = cols[3]),
      na.translate = F) +
    guides(colour = guide_legend(order = 1),
           alpha = guide_legend("Phenotype Used", order = 2),
           shape = guide_legend("Phenotype Used", order = 2)) +
    labs(color = "Virus") + theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text())
)

plot <- plot_grid(
  rsv_infants,
  flu_infants,
  covid_infants, 
  nrow = 1
) %>% annotate_figure(
  left = text_grob("Surveillance", rot = 90, vjust = 1),
  bottom = text_grob("EHR"),
  top = text_grob(
    "Surveillance VS EHR: Monthly Counts of RSV, Influenza and COVID-19 in Infants",
    face = "bold", size = 14))
plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
  )

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "infants_seasonality_comparisons_x_vs_y_2.png"),
       width = 18, height = 6)
