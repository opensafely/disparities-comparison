library(tidyverse)
library(here)
library(lubridate)
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(ggpmisc)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

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

#import surveillance data
df_surv_rsv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance", "UKHSA_reports_RSV.csv")) %>%
  arrange(month)
df_surv_flu <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance", "UKHSA_reports_flu.csv")) %>%
  arrange(month)
df_surv <- merge(
  df_surv_rsv %>% select(c(month, total_rsv = RSV)),
  df_surv_flu %>% select(c(month, total_flu = flu)),
  by = "month"
) %>%
  mutate(type = "Surveillance")
df_surv <- df_surv %>%
  pivot_longer(
    cols = c(total_rsv, total_flu),
    names_to = "virus",
    values_to = "total_events"
  ) %>%
  mutate(
    virus = case_when(
      virus == "total_rsv" ~ "RSV",
      virus == "total_flu" ~ "Influenza"
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
    mutate(codelist_type = "sensitive")
)

df_surv_mute <- bind_rows(
  df_surv_mute %>%
    mutate(codelist_type = "specific"),
  df_surv_mute %>%
    mutate(codelist_type = "sensitive")
)

df_surv_mute <- df_surv_mute %>%
  mutate(
    coeff = case_when(
      virus == "RSV" & event == "Mild" & codelist_type == "specific" ~ 8,
      virus == "RSV" & event == "Severe" & codelist_type == "specific" ~ 5,
      virus == "RSV" & event == "Mild" & codelist_type == "sensitive" ~ 18,
      virus == "RSV" & event == "Severe" & codelist_type == "sensitive" ~ 5,
      virus == "Influenza" & event == "Mild" & codelist_type == "specific" ~ 0.75,
      virus == "Influenza" & event == "Severe" & codelist_type == "specific" ~ 0.5,
      virus == "Influenza" & event == "Mild" & codelist_type == "sensitive" ~ 2,
      virus == "Influenza" & event == "Severe" & codelist_type == "sensitive" ~ 2
    ),
    total_events = total_events*coeff
  ) %>%
  arrange(month)

df_all <- bind_rows(
  df_rsv,
  df_flu
) %>%
  arrange(month) %>%
  filter(month <= as.Date("2021-01-01"))

df_all <- df_all %>%
  mutate(
    ylims = case_when(
      virus == "RSV" & event == "Mild" & codelist_type == "specific" ~ 80000,
      virus == "RSV" & event == "Severe" & codelist_type == "specific" ~ 50000,
      virus == "RSV" & event == "Mild" & codelist_type == "sensitive" ~ 250000,
      virus == "RSV" & event == "Severe" & codelist_type == "sensitive" ~ 60000,
      virus == "Influenza" & event == "Mild" & codelist_type == "specific" ~ 10500,
      virus == "Influenza" & event == "Severe" & codelist_type == "specific" ~ 7500,
      virus == "Influenza" & event == "Mild" & codelist_type == "sensitive" ~ 35000,
      virus == "Influenza" & event == "Severe" & codelist_type == "sensitive" ~ 40000
    )
  )

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2019-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2020-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

f <- function(pal) brewer.pal(3, pal)

cols <- f("Set2")

plot_combined <- function(df, pathogen, phenotype) {
  
  coeff_mild <- df_surv_mute %>%
    filter(virus == !!pathogen, event == "Mild",
           codelist_type == !!phenotype) %>%
    select(coeff) %>%
    pull() %>%
    unique()
  
  limits_mild <- df %>%
    filter(virus == !!pathogen, event == "Mild",
           codelist_type == !!phenotype) %>%
    select(ylims) %>%
    pull() %>%
    unique()
  
  coeff_severe <- df_surv_mute %>%
    filter(virus == !!pathogen, event == "Severe",
           codelist_type == !!phenotype) %>%
    select(coeff) %>%
    pull() %>%
    unique()
  
  limits_severe <- df %>%
    filter(virus == !!pathogen, event == "Severe",
           codelist_type == !!phenotype) %>%
    select(ylims) %>%
    pull() %>%
    unique()
  
  surv_filt <- df_surv_mute %>%
    filter(virus == !!pathogen,
           codelist_type == !!phenotype) %>%
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
    arrange(month) %>%
    mutate(
      col_type = case_when(
        virus == "RSV" & type == "EHR" ~ "RSV - EHR",
        virus == "RSV" & type == "Surveillance" ~ "RSV - Surveillance",
        virus == "Influenza" & type == "EHR" ~ "Influenza - EHR",
        virus == "Influenza" & type == "Surveillance" ~ "Influenza - Surveillance"
      )
    )
  
  mild <- df_plot %>%
    filter(event == "Mild") %>%
    ggplot() +
    geom_line(aes(x = month, y = total_events, color = col_type,
                  alpha = codelist_type), linewidth = 1) +
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                                ymin = ymin, ymax = ymax),
              fill = "grey", alpha = 0.25, col = NA) +
    scale_y_continuous(
      limits = c(0, limits_mild),
      sec.axis = sec_axis(trans = ~./coeff_mild, name = "")
    ) +
    scale_x_date(date_breaks = "1 years", date_labels = "%y") + 
    scale_color_manual(values = c(
      "RSV - EHR" = cols[1], "Influenza - EHR" = cols[2],
      "RSV - Surveillance" = "#519A83", "Influenza - Surveillance" = "#CE704C")) +
    scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                       labels = c("sensitive" = "Sensitive",
                                  "specific" = "Specific"),
                       na.translate = FALSE) +
    labs(x = "", y = "", colour = "Virus & Data Source",
         alpha = "Phenotype Used") + theme_bw() +
    theme(legend.position = "none")
  
  severe <- df_plot %>%
    filter(event == "Severe") %>%
    ggplot() +
    geom_line(aes(x = month, y = total_events, color = col_type,
                  alpha = codelist_type), linewidth = 1) +
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                                ymin = ymin, ymax = ymax),
              fill = "grey", alpha = 0.25, col = NA) +
    scale_y_continuous(
      limits = c(0, limits_severe),
      sec.axis = sec_axis(trans = ~./coeff_severe, name = "")
    ) +
    scale_x_date(date_breaks = "1 years", date_labels = "%y") + 
    scale_color_manual(values = c(
      "RSV - EHR" = cols[1], "Influenza - EHR" = cols[2],
      "RSV - Surveillance" = "#519A83", "Influenza - Surveillance" = "#CE704C")) +
    scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                       labels = c("sensitive" = "Sensitive",
                                  "specific" = "Specific"),
                       na.translate = FALSE) +
    labs(x = "", y = "", colour = "Virus & Data Source",
         alpha = "Phenotype Used") + theme_bw() +
    theme(legend.position = "none")
  
  plot_grid(mild, severe, nrow = 1)
  
}

spec_rsv <- plot_combined(df_all, "RSV", "specific")
spec_flu <- plot_combined(df_all, "Influenza", "specific")
sens_rsv <- plot_combined(df_all, "RSV", "sensitive")
sens_flu <- plot_combined(df_all, "Influenza", "sensitive")

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
    arrange(month) %>%
    mutate(
      col_type = case_when(
        virus == "RSV" & type == "EHR" ~ "RSV - EHR",
        virus == "RSV" & type == "Surveillance" ~ "RSV - Surveillance",
        virus == "Influenza" & type == "EHR" ~ "Influenza - EHR",
        virus == "Influenza" & type == "Surveillance" ~ "Influenza - Surveillance"
      )
    )
  
  legend <- get_legend(
    df_plot %>%
      ggplot() +
      geom_line(aes(x = month, y = total_events, color = factor(
        col_type, levels = c(
          "RSV - EHR", "RSV - Surveillance", "Influenza - EHR",
          "Influenza - Surveillance")),
        alpha = factor(codelist_type, levels = c("specific", "sensitive"))),
        linewidth = 1) +
      scale_color_manual(values = c(
        "RSV - EHR" = cols[1], "Influenza - EHR" = cols[2],
        "RSV - Surveillance" = "#519A83", "Influenza - Surveillance" = "#CE704C")) +
      scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                         labels = c("sensitive" = "Sensitive",
                                    "specific" = "Specific"),
                         na.translate = FALSE) +
      guides(colour = guide_legend("Virus & Data Source", order = 1),
             alpha = guide_legend("Phenotype Used", order = 3)) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.box = "horizontal",
            legend.title = element_text())
  )
  
  return(legend)
  
}

legend <- get_legend_2(df_surv_mute, df_all)

plot <- plot_grid(
  spec_rsv, sens_rsv,
  spec_flu, sens_flu,
  ncol = 1,
  label_size = 14
) %>% annotate_figure(
  left = text_grob("Monthly Events Identified", rot = 90, vjust = 1),
  right = text_grob("Monthly Surveillance Data", rot = 270, vjust = 1),
)

# Dummy data for the grey box
legend_df <- data.frame(
  xmin = 0.2, xmax = 0.4,
  ymin = 0.45, ymax = 0.55
)

transmission_legend <- ggplot() +
  # Grey square box
  geom_rect(data = legend_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, color = NA) +
  # Label to the right
  annotate("text", x = 0.5, y = 0.5,
           label = "Usual Transmission Period (Nov–Mar)",
           hjust = 0, vjust = 0.5, size = 4) +
  xlim(0, 3) + ylim(0, 0.75) +  # Give horizontal space
  theme_void() +
  theme(plot.margin = margin(3, -10, -75, -10))

bottom_row <- plot_grid(
  plot_grid(NULL, transmission_legend, ncol = 2, rel_widths = c(0.32, 0.8)),
  legend,
  nrow = 2,
  rel_heights = c(0.5, 0.25)
) %>% annotate_figure(
  left = text_grob("Mild", size = 14, hjust = -7.25, vjust = -118.5),
  right = text_grob("Severe", size = 14, hjust = 4.75, vjust = -118.5)
)

plot_grid(
  NULL,
  plot,
  bottom_row,
  ncol = 1,
  rel_heights = c(0.01, 1, 0.1)
) %>% annotate_figure(
  top = text_grob(
    "Monthly Counts of RSV and Influenza in All Cohorts ",
    face = "bold", size = 14),
  bottom = text_grob("Year (2016-2024)", vjust = -12)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_ukhsa.png"),
       width = 12, height = 18)

df_all_cohorts <- bind_rows(
  df_rsv %>% mutate(type = "EHR"),
  df_flu %>% mutate(type = "EHR"),
  df_surv %>% mutate(type = "Surveillance")
) %>%
  arrange(month) %>%
  filter(month <= as.Date("2021-01-01"))

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
  arrange(month)

df_comp_all_cohorts <- df_combined_expanded_all_cohorts %>%
  pivot_wider(
    names_from = type,
    values_from = total_events,
    values_fill = 0
  ) %>%
  mutate(season = case_when(
    month >= as.Date("2016-09-01") & month < as.Date("2017-09-01") ~ "2016-17",
    month >= as.Date("2017-09-01") & month < as.Date("2018-09-01") ~ "2017-18",
    month >= as.Date("2018-09-01") & month < as.Date("2019-09-01") ~ "2018-19",
    month >= as.Date("2019-09-01") & month < as.Date("2020-09-01") ~ "2019-20",
    month >= as.Date("2020-09-01") & month < as.Date("2021-09-01") ~ "2020-21"
  ))

rsv_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "RSV", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot(aes(x = EHR, y = Surveillance, shape = codelist_type)) +
  # Primary axis viruses
  geom_point(aes(alpha = codelist_type), color = cols[1], size = 3.5) +
  stat_poly_line(color = "#5A5652", se = F) + stat_poly_eq() +
  facet_grid(factor(codelist_type, levels = c("specific", "sensitive"),
                    labels = c("Specific", "Sensitive"))~event,
             scales = "free_x") +
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
  theme(legend.position = "none",
        strip.text = element_text(size = 10))

flu_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "Influenza", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot(aes(x = EHR, y = Surveillance, shape = codelist_type)) +
  # Primary axis viruses
  geom_point(aes(alpha = codelist_type), color = cols[2], size = 3.5) +
  stat_poly_line(color = "#5A5652", se = F) + stat_poly_eq() +
  facet_grid(factor(codelist_type, levels = c("specific", "sensitive"),
                    labels = c("Specific", "Sensitive"))~event,
             scales = "free_x") +
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
  theme(legend.position = "none",
        strip.text = element_text(size = 10))

legend <- get_legend(
  df_comp_all_cohorts %>%
    filter(month >= as.Date("2016-09-01")) %>%
    ggplot() +
    # Primary axis viruses
    geom_point(aes(x = EHR, y = Surveillance, alpha = factor(
      codelist_type, levels = c("specific", "sensitive")),
      color = factor(
        virus, levels = c("RSV", "Influenza")),
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
    scale_color_manual(values = c("RSV" = cols[1], "Influenza" = cols[2]),
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
  rsv_all_cohorts,
  flu_all_cohorts,
  ncol = 1
) %>% annotate_figure(
  left = text_grob("Surveillance Data", rot = 90, vjust = 1),
  bottom = text_grob("EHR Data", hjust = 0.2, vjust = 0),
  top = text_grob(
    "Surveillance VS EHR: Monthly Counts of RSV and Influenza in All Cohorts",
    face = "bold", size = 14))
plot1 <- plot_grid(plot, legend, NULL, ncol = 1,
                   rel_heights = c(2, 0.05, 0.001)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
)

plot_grid(
  plot1, NULL, ncol = 2, rel_widths = c(0.99, 0.01)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_x_vs_y_viruses_ukhsa.png"),
       width = 14, height = 18)

##colour code by season
cols2 <- scales::seq_gradient_pal(
  "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))

rsv_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "RSV", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot(aes(x = EHR, y = Surveillance, shape = codelist_type)) +
  # Primary axis viruses
  geom_point(aes(alpha = codelist_type, color = season), size = 3.5) +
  stat_poly_line(color = "#5A5652", se = F) + stat_poly_eq() +
  facet_grid(factor(codelist_type, levels = c("specific", "sensitive"),
                    labels = c("Specific", "Sensitive"))~event,
             scales = "free_x") +
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
  scale_color_manual(values = cols2[1:5], name = "Season") +
  labs(x = "", y = "", title = "RSV") + theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10))

flu_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "Influenza", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot(aes(x = EHR, y = Surveillance, shape = codelist_type)) +
  # Primary axis viruses
  geom_point(aes(alpha = codelist_type, color = season), size = 3.5) +
  stat_poly_line(color = "#5A5652", se = F) + stat_poly_eq() +
  facet_grid(factor(codelist_type, levels = c("specific", "sensitive"),
                    labels = c("Specific", "Sensitive"))~event,
             scales = "free_x") +
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
  scale_color_manual(values = cols2[1:5], name = "Season") +
  labs(x = "", y = "", title = "Influenza") + theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10))

legend <- get_legend(
  df_comp_all_cohorts %>%
    filter(month >= as.Date("2016-09-01")) %>%
    ggplot() +
    # Primary axis viruses
    geom_point(aes(x = EHR, y = Surveillance, color = season), size = 3.5) +
    scale_color_manual(values = cols2[1:5], name = "Season") +
    guides(colour = guide_legend("Season", order = 1)) +
    theme_bw() +
    theme(legend.position = "right",
          legend.box = "verticle",
          legend.title = element_text())
)

plot <- plot_grid(
  NULL,
  rsv_all_cohorts,
  flu_all_cohorts,
  ncol = 1,
  rel_heights = c(0.05, 1, 1, 1)
) %>% annotate_figure(
  left = text_grob("Surveillance Data", rot = 90, vjust = 1),
  bottom = text_grob("EHR Data", hjust = 0.2, vjust = 0),
  top = text_grob(
    "Surveillance VS EHR: Monthly Counts of RSV and Influenza in All Cohorts",
    face = "bold", size = 14, vjust = 1.75))
plot1 <- plot_grid(plot, legend, ncol = 2, rel_widths = c(2, 0.25)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
  )

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_x_vs_y_seasons_ukhsa.png"),
       width = 14, height = 18)

##phase plot

# #mutate 0 events for log scaling
# df_comp_all_cohorts_scale <- df_comp_all_cohorts %>%
#   mutate(
#     EHR = if_else(EHR == 0, 0.1, EHR),
#     Surveillance = if_else(Surveillance == 0, 0.1, Surveillance))

rsv_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "RSV", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot(aes(x = EHR, y = Surveillance)) +
  # Primary axis viruses
  geom_path(aes(alpha = codelist_type, color = season)) +
  # scale_x_log10() + scale_y_log10() +
  stat_poly_line(color = "#5A5652", se = F) +
  facet_grid(factor(codelist_type, levels = c("specific", "sensitive"),
                    labels = c("Specific", "Sensitive"))~event,
             scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_color_manual(values = cols2[1:5], name = "Season") +
  labs(x = "", y = "", title = "RSV") + theme_bw() +
  theme(legend.position = "none")

flu_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "Influenza", month >= as.Date("2016-09-01")) %>%
  arrange(month) %>%
  ggplot(aes(x = EHR, y = Surveillance, shape)) +
  # Primary axis viruses
  geom_path(aes(alpha = codelist_type, color = season)) +
  # scale_x_log10() + scale_y_log10() +
  stat_poly_line(color = "#5A5652", se = F) +
  facet_grid(factor(codelist_type, levels = c("specific", "sensitive"),
                    labels = c("Specific", "Sensitive"))~event,
             scales = "free_x") +
  scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                     labels = c("sensitive" = "Sensitive",
                                "specific" = "Specific"),
                     name = "Phenotype Used",
                     na.translate = FALSE) +
  scale_color_manual(values = cols2[1:5], name = "Season") +
  labs(x = "", y = "", title = "Influenza") + theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  df_comp_all_cohorts %>%
    filter(month >= as.Date("2016-09-01")) %>%
    ggplot() +
    # Primary axis viruses
    geom_path(aes(x = EHR, y = Surveillance, color = season)) +
    scale_color_manual(values = cols2[1:5], name = "Season") +
    guides(colour = guide_legend("Season", order = 1)) +
    theme_bw() +
    theme(legend.position = "right",
          legend.box = "verticle",
          legend.title = element_text())
)

plot <- plot_grid(
  NULL,
  rsv_all_cohorts,
  flu_all_cohorts,
  ncol = 1,
  rel_heights = c(0.05, 1, 1, 1)
) %>% annotate_figure(
  left = text_grob("Surveillance Data", rot = 90, vjust = 1),
  bottom = text_grob("EHR Data", hjust = 0.2, vjust = 0),
  top = text_grob(
    "Surveillance VS EHR: Monthly Counts of RSV and Influenza in All Cohorts",
    face = "bold", size = 14, vjust = 1.75))
plot1 <- plot_grid(plot, legend, ncol = 2, rel_widths = c(2, 0.25)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
  )

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_phase_seasons_ukhsa.png"),
       width = 12, height = 18)
