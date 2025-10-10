library(tidyverse)
library(here)
library(lubridate)
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(ggpmisc)
library(egg)

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
    paste0(cohort, "_", "counts_over_time_all_weekly_", pathogen, ".csv"))) %>%
    mutate(
      virus = pathogen_title,
      week = as.Date(week - days(1))
    ) %>%
    arrange(week) %>%
    mutate(
      event = case_when(
        str_detect(event, "primary") ~ "Mild",
        str_detect(event, "secondary") ~ "Severe"
      )
    ) %>%
    select(c(week, event,
             total_events = n,
             codelist_type, virus)) %>%
    filter(week >= "2016-09-01")
  
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
  arrange(week)
df_rsv <- df_rsv[, total_events := sum(total_events, na.rm = T),
                 by = .(week, event, codelist_type, virus)] %>%
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
  arrange(week)
df_flu <- df_flu[, total_events := sum(total_events, na.rm = T),
                 by = .(week, event, codelist_type, virus)] %>%
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
  arrange(week)
df_covid <- df_covid[, total_events := sum(total_events, na.rm = T),
                     by = .(week, event, codelist_type, virus)] %>%
  unique()

#import surveillance data
df_surv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "seasonality_weekly_england.csv")) %>%
  arrange(week) %>%
  mutate(
    total_overall = sum(total_rsv, total_flu, total_covid, na.rm = TRUE),
    type = "Surveillance") %>%
  select(-covid_scaled) %>%
  mutate(
    total_covid = if_else(week < ymd("2020-03-01"), NA, total_covid)
  ) %>%
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

df_surv <- df_surv[, c("week", "event", "total_events", "virus")]

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
      virus == "RSV" & event == "Mild" & codelist_type == "specific" ~ 10,
      virus == "RSV" & event == "Severe" & codelist_type == "specific" ~ 5,
      virus == "RSV" & event == "Mild" & codelist_type == "sensitive" ~ 30,
      virus == "RSV" & event == "Severe" & codelist_type == "sensitive" ~ 6,
      virus == "Influenza" & event == "Mild" & codelist_type == "specific" ~ 0.25,
      virus == "Influenza" & event == "Severe" & codelist_type == "specific" ~ 0.25,
      virus == "Influenza" & event == "Mild" & codelist_type == "sensitive" ~ 0.5,
      virus == "Influenza" & event == "Severe" & codelist_type == "sensitive" ~ 1,
      virus == "COVID-19" & event == "Mild" & codelist_type == "specific" ~ 0.25,
      virus == "COVID-19" & event == "Severe" & codelist_type == "specific" ~ 0.01,
      virus == "COVID-19" & event == "Mild" & codelist_type == "sensitive" ~ 0.25,
      virus == "COVID-19" & event == "Severe" & codelist_type == "sensitive" ~ 0.01
    ),
    total_events = case_when(
      is.na(total_events) & virus != "COVID-19" ~ 0,
      is.na(total_events) & virus == "COVID-19" & week >= ymd("2020-03-01") ~ 0,
      is.na(total_events) & virus == "COVID-19" & week < ymd("2020-03-01") ~ NA,
      TRUE ~ total_events*coeff
    )
  ) %>%
  arrange(week)

df_all <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>%
  arrange(week)

df_all <- df_all %>%
  mutate(
    ylims = case_when(
      virus == "RSV" & event == "Mild" & codelist_type == "specific" ~ 30000,
      virus == "RSV" & event == "Severe" & codelist_type == "specific" ~ 15000,
      virus == "RSV" & event == "Mild" & codelist_type == "sensitive" ~ 80000,
      virus == "RSV" & event == "Severe" & codelist_type == "sensitive" ~ 20000,
      virus == "Influenza" & event == "Mild" & codelist_type == "specific" ~ 5000,
      virus == "Influenza" & event == "Severe" & codelist_type == "specific" ~ 5000,
      virus == "Influenza" & event == "Mild" & codelist_type == "sensitive" ~ 12500,
      virus == "Influenza" & event == "Severe" & codelist_type == "sensitive" ~ 15000,
      virus == "COVID-19" & event == "Mild" & codelist_type == "specific" ~ 400000,
      virus == "COVID-19" & event == "Severe" & codelist_type == "specific" ~ 15000,
      virus == "COVID-19" & event == "Mild" & codelist_type == "sensitive" ~ 400000,
      virus == "COVID-19" & event == "Severe" & codelist_type == "sensitive" ~ 15000
    )
  )

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
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
    arrange(week) %>%
    mutate(
      col_type = case_when(
        virus == "RSV" & type == "EHR" ~ "RSV - EHR",
        virus == "RSV" & type == "Surveillance" ~ "RSV - Surveillance",
        virus == "Influenza" & type == "EHR" ~ "Influenza - EHR",
        virus == "Influenza" & type == "Surveillance" ~ "Influenza - Surveillance",
        virus == "COVID-19" & type == "EHR" ~ "COVID-19 - EHR",
        virus == "COVID-19" & type == "Surveillance" ~ "COVID-19 - Surveillance"
      )
    )
  
  my_tag <- function(outcome_type, pathogen, phenotype) {

    tag <- paste0(outcome_type, " ", pathogen, " (",
                  str_to_sentence(phenotype), ")")
    return(tag)

  }
  
  mild <- df_plot %>%
    filter(event == "Mild") %>%
    ggplot() +
    geom_line(aes(x = week, y = total_events, color = col_type,
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
      "COVID-19 - EHR" = cols[3], "RSV - Surveillance" = "#519A83",
      "Influenza - Surveillance" = "#CE704C",
      "COVID-19 - Surveillance" = "#6F7EA0")) +
    scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                       labels = c("sensitive" = "Sensitive",
                                  "specific" = "Specific"),
                       na.translate = FALSE) +
    labs(x = "", y = "", colour = "Virus & Data Source",
         alpha = "Phenotype Used") + theme_bw() +
    theme(legend.position = "none")

  mild <- tag_facet(mild,
                    x = df_plot$week[[1]], y = limits_mild,
                    hjust = 0, vjust = 0.5,
                    open = "", close = "",
                    fontface = 4,
                    size = 3.5,
                    family = "sans",
                    tag_pool = my_tag("Mild", pathogen, phenotype))
  
  severe <- df_plot %>%
    filter(event == "Severe") %>%
    ggplot() +
    geom_line(aes(x = week, y = total_events, color = col_type,
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
      "COVID-19 - EHR" = cols[3], "RSV - Surveillance" = "#519A83",
      "Influenza - Surveillance" = "#CE704C",
      "COVID-19 - Surveillance" = "#6F7EA0")) +
    scale_alpha_manual(values = c("sensitive" = 0.5, "specific" = 1),
                       labels = c("sensitive" = "Sensitive",
                                  "specific" = "Specific"),
                       na.translate = FALSE) +
    labs(x = "", y = "", colour = "Virus & Data Source",
         alpha = "Phenotype Used") + theme_bw() +
    theme(legend.position = "none")

    severe <- tag_facet(severe,
                        x = df_plot$week[[1]], y = limits_severe,
                        hjust = 0, vjust = 0.5,
                        open = "", close = "",
                        fontface = 4,
                        size = 3.5,
                        family = "sans",
                        tag_pool = my_tag("Severe", pathogen, phenotype))
  
  # # Create a text label grob with the phenotype name
  # label <- ggdraw() + 
  #   draw_label(
  #     str_to_sentence(phenotype),
  #     fontface = "bold", 
  #     size = 8
  #   )
  
  # Combine plots with text label in the middle
  plot_grid(
    mild, 
    #label,  # the text label
    severe, 
    nrow = 1, 
    rel_widths = c(1, 1)
  )
  
}

spec_rsv <- plot_combined(df_all, "RSV", "specific")
spec_flu <- plot_combined(df_all, "Influenza", "specific")
spec_covid <- plot_combined(df_all, "COVID-19", "specific")
sens_rsv <- plot_combined(df_all, "RSV", "sensitive")
sens_flu <- plot_combined(df_all, "Influenza", "sensitive")
sens_covid <- plot_combined(df_all, "COVID-19", "sensitive")

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
    arrange(week) %>%
    mutate(
      col_type = case_when(
        virus == "RSV" & type == "EHR" ~ "RSV - EHR",
        virus == "RSV" & type == "Surveillance" ~ "RSV - Surveillance",
        virus == "Influenza" & type == "EHR" ~ "Influenza - EHR",
        virus == "Influenza" & type == "Surveillance" ~ "Influenza - Surveillance",
        virus == "COVID-19" & type == "EHR" ~ "COVID-19 - EHR",
        virus == "COVID-19" & type == "Surveillance" ~ "COVID-19 - Surveillance"
      )
    )
  
  legend <- get_legend(
    df_plot %>%
      filter(virus != "Overall Respiratory Viruses") %>%
      ggplot() +
      geom_line(aes(x = week, y = total_events, color = factor(
        col_type, levels = c(
          "RSV - EHR", "RSV - Surveillance", "Influenza - EHR",
          "Influenza - Surveillance", "COVID-19 - EHR",
          "COVID-19 - Surveillance")),
        alpha = factor(codelist_type, levels = c("specific", "sensitive"))),
        linewidth = 1) +
      scale_color_manual(values = c(
        "RSV - EHR" = cols[1], "Influenza - EHR" = cols[2],
        "COVID-19 - EHR" = cols[3], "RSV - Surveillance" = "#519A83",
        "Influenza - Surveillance" = "#CE704C",
        "COVID-19 - Surveillance" = "#6F7EA0")) +
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
  spec_covid, sens_covid,
  ncol = 1,
  label_size = 14
) %>% annotate_figure(
  left = text_grob("Weekly Events Identified", rot = 90, vjust = 1),
  right = text_grob("Weekly Surveillance Data", rot = 270, vjust = 1),
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
  NULL, transmission_legend, ncol = 2, rel_widths = c(0.32, 0.8)
)

plot_grid(
  NULL,
  plot,
  bottom_row,
  NULL,
  ncol = 1,
  rel_heights = c(0.02, 1, 0.05, 0.025)
) %>% annotate_figure(
  # top = text_grob(
  #   "weekly Counts of RSV, Influenza and COVID-19 in All Cohorts ",
  #   face = "bold", size = 14),
  bottom = text_grob("Year (2016-2024)", vjust = -9.5)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_england_weekly.png"),
       width = 12, height = 18)

df_all_cohorts <- bind_rows(
  df_rsv %>% mutate(type = "EHR"),
  df_flu %>% mutate(type = "EHR"),
  df_covid %>% mutate(type = "EHR"),
  df_surv %>% mutate(type = "Surveillance")
) %>%
  arrange(week)

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
  arrange(week)

df_comp_all_cohorts <- df_combined_expanded_all_cohorts %>%
  pivot_wider(
    names_from = type,
    values_from = total_events,
    values_fill = 0
  ) %>%
  mutate(season = case_when(
    week >= as.Date("2016-09-01") & week < as.Date("2017-09-01") ~ "2016-17",
    week >= as.Date("2017-09-01") & week < as.Date("2018-09-01") ~ "2017-18",
    week >= as.Date("2018-09-01") & week < as.Date("2019-09-01") ~ "2018-19",
    week >= as.Date("2019-09-01") & week < as.Date("2020-09-01") ~ "2019-20",
    week >= as.Date("2020-09-01") & week < as.Date("2021-09-01") ~ "2020-21",
    week >= as.Date("2021-09-01") & week < as.Date("2022-09-01") ~ "2021-22",
    week >= as.Date("2022-09-01") & week < as.Date("2023-09-01") ~ "2022-23",
    week >= as.Date("2023-09-01") & week < as.Date("2024-09-01") ~ "2023-24"
  ))

rsv_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "RSV", week >= as.Date("2016-09-01")) %>%
  arrange(week) %>%
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
  theme(legend.position = "none", strip.text.x = element_blank())

flu_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "Influenza", week >= as.Date("2016-09-01")) %>%
  arrange(week) %>%
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
  theme(legend.position = "none", strip.text.x = element_blank())

covid_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "COVID-19", week >= as.Date("2020-03-01")) %>%
  arrange(week) %>%
  ggplot(aes(x = EHR, y = Surveillance, shape = codelist_type)) +
  # Primary axis viruses
  geom_point(aes(alpha = codelist_type), color = cols[3], size = 3.5) +
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
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  labs(x = "", y = "") + theme_bw() +
  theme(legend.position = "none", strip.text.x = element_blank())

legend <- get_legend(
  df_comp_all_cohorts %>%
    filter(week >= as.Date("2016-09-01")) %>%
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
          legend.title = element_text(),
          strip.text.x = element_blank())
)

plot <- plot_grid(
  rsv_all_cohorts,
  flu_all_cohorts,
  covid_all_cohorts,
  ncol = 1
) %>% annotate_figure(
  left = text_grob("Surveillance Data", rot = 90, vjust = 1),
  bottom = text_grob("EHR Data", hjust = 0.2, vjust = 0),
  # top = text_grob(
  #   "Surveillance VS EHR: weekly Counts of RSV, Influenza and COVID-19 in All Cohorts",
  #   face = "bold", size = 14)
  )
plot1 <- plot_grid(NULL, plot, legend, NULL, ncol = 1,
                   rel_heights = c(0.035, 2, 0.05, 0.001)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
)

plot_grid(
  plot1, NULL, ncol = 2, rel_widths = c(0.99, 0.01)
) %>% annotate_figure(
  left = text_grob("Mild", size = 12, hjust = -10, vjust = -72.75,
                   face = "bold"),
  right = text_grob("Severe", size = 12, hjust = 6, vjust = -72.75,
                    face = "bold")
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_x_vs_y_viruses_england_weekly.png"),
       width = 12, height = 18)

##colour code by season
cols2 <- scales::seq_gradient_pal(
  "#F05039", "#1F449c", "Lab")(seq(0,1,length.out=8))

rsv_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "RSV", week >= as.Date("2016-09-01")) %>%
  arrange(week) %>%
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
  scale_color_manual(values = cols2, name = "Season") +
  labs(x = "", y = "", title = "RSV") + theme_bw() +
  theme(legend.position = "none", strip.text.x = element_blank())

flu_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "Influenza", week >= as.Date("2016-09-01")) %>%
  arrange(week) %>%
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
  scale_color_manual(values = cols2, name = "Season") +
  labs(x = "", y = "", title = "Influenza") + theme_bw() +
  theme(legend.position = "none", strip.text.x = element_blank())

covid_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "COVID-19", week >= as.Date("2020-03-01")) %>%
  arrange(week) %>%
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
  scale_color_manual(values = cols2[4:8], name = "Season") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  labs(x = "", y = "", title = "COVID-19") + theme_bw() +
  theme(legend.position = "none", strip.text.x = element_blank())

legend <- get_legend(
  df_comp_all_cohorts %>%
    filter(week >= as.Date("2016-09-01")) %>%
    ggplot() +
    # Primary axis viruses
    geom_point(aes(x = EHR, y = Surveillance, color = season), size = 3.5) +
    scale_color_manual(values = cols2, name = "Season") +
    guides(colour = guide_legend("Season", order = 1)) +
    theme_bw() +
    theme(legend.position = "right",
          legend.box = "verticle",
          legend.title = element_text(),
          strip.text.x = element_blank())
)

plot <- plot_grid(
  NULL,
  rsv_all_cohorts,
  flu_all_cohorts,
  covid_all_cohorts,
  ncol = 1,
  rel_heights = c(0.05, 1, 1, 1)
) %>% annotate_figure(
  left = text_grob("Surveillance Data", rot = 90, vjust = 1),
  bottom = text_grob("EHR Data", hjust = 0.2, vjust = 0),
  # top = text_grob(
  #   "Surveillance VS EHR: weekly Counts of RSV, Influenza and COVID-19 in All Cohorts",
  #   face = "bold", size = 14, vjust = 1.75)
  )
plot1 <- plot_grid(plot, legend, ncol = 2, rel_widths = c(2, 0.25)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
  )

plot_grid(
  NULL,
  plot_grid(plot1, NULL, ncol = 2, rel_widths = c(0.99, 0.01)),
  nrow = 2,
  rel_heights = c(0.01, 0.99)
) %>% annotate_figure(
  left = text_grob("Mild", size = 14, hjust = -8, vjust = -62,
                   face = "bold"),
  right = text_grob("Severe", size = 14, hjust = 7, vjust = -62,
                    face = "bold")
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_x_vs_y_seasons_england_weekly.png"),
       width = 12, height = 18)

##phase plot

# #mutate 0 events for log scaling
# df_comp_all_cohorts_scale <- df_comp_all_cohorts %>%
#   mutate(
#     EHR = if_else(EHR == 0, 0.1, EHR),
#     Surveillance = if_else(Surveillance == 0, 0.1, Surveillance))

rsv_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "RSV", week >= as.Date("2016-09-01")) %>%
  arrange(week) %>%
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
  scale_color_manual(values = cols2, name = "Season") +
  labs(x = "", y = "", title = "RSV") + theme_bw() +
  theme(legend.position = "none")

flu_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "Influenza", week >= as.Date("2016-09-01")) %>%
  arrange(week) %>%
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
  scale_color_manual(values = cols2, name = "Season") +
  labs(x = "", y = "", title = "Influenza") + theme_bw() +
  theme(legend.position = "none")

covid_all_cohorts <- df_comp_all_cohorts %>%
  filter(virus == "COVID-19", week >= as.Date("2020-03-01")) %>%
  arrange(week) %>%
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
  scale_color_manual(values = cols2[4:8], name = "Season") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  labs(x = "", y = "", title = "COVID-19") + theme_bw() +
  theme(legend.position = "none")

legend <- get_legend(
  df_comp_all_cohorts %>%
    filter(week >= as.Date("2016-09-01")) %>%
    ggplot() +
    # Primary axis viruses
    geom_path(aes(x = EHR, y = Surveillance, color = season)) +
    scale_color_manual(values = cols2, name = "Season") +
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
  covid_all_cohorts,
  ncol = 1,
  rel_heights = c(0.05, 1, 1, 1)
) %>% annotate_figure(
  left = text_grob("Surveillance Data", rot = 90, vjust = 1),
  bottom = text_grob("EHR Data", hjust = 0.2, vjust = 0),
  # top = text_grob(
  #   "Surveillance VS EHR: weekly Counts of RSV, Influenza and COVID-19 in All Cohorts",
  #   face = "bold", size = 14, vjust = 1.75)
  )
plot1 <- plot_grid(plot, legend, ncol = 2, rel_widths = c(2, 0.25)) +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5)
  )

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "all_cohorts_seasonality_comparisons_phase_seasons_england_weekly.png"),
       width = 12, height = 18)
