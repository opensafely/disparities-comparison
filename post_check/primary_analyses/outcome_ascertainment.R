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
library(scales)

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
             total_events = n,
             codelist_type, virus)) %>%
    filter(month >= "2016-09-01") %>% 
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
      )
    )
  
  return(df)
  
}

#import data
df_rsv_older_adults <- import("rsv", "older_adults")
df_rsv_infants <- import("rsv", "infants")

#import data
df_flu_older_adults <- import("flu", "older_adults")
df_flu_infants <- import("flu", "infants")

#import data
df_covid_older_adults <- import("covid", "older_adults")
df_covid_infants <- import("covid", "infants")

sizes <- function(input, cohort) {

  f <- function(pal) brewer.pal(3, pal)
  cols <- f("Set2")

  plot_totals <- function(severity, pathogen) {

    df <- input %>%
      filter(event == !!severity, virus == !!pathogen) %>% 
      select(-event) %>%
      unique() %>% 
      mutate(
        codelist_type = factor(str_to_title(codelist_type), levels = c("Specific", "Sensitive")),
        virus = factor(virus, levels = c("RSV", "Influenza", "COVID-19"))
      )
    
    if (pathogen == "COVID-19") {

      df <- df %>% 
        filter(subset %in% c("2019-20", "2020-21", "2021-22",
                             "2022-23", "2023-24"))

    }

    plot <- df %>%
      ggplot() + geom_bar(aes(y = yearly_events, x = codelist_type,
         fill = virus, alpha = codelist_type),
         stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "RSV" = cols[1], "Influenza" = cols[2],
        "COVID-19" = cols[3])) +
      scale_alpha_manual(values = c("Sensitive" = 0.5, "Specific" = 1),
                         na.translate = FALSE,
                         guide = guide_legend(position = NULL)) +
      scale_y_continuous(labels = label_number(scale_cut = cut_si(""))) +
      #scale_y_log10() +
      coord_cartesian(clip = "off") +
      geom_hline(yintercept = 1, colour = "black", linewidth = 0.6) +
      facet_wrap(~subset, scales = "fixed", nrow = 1) +
      labs(
        x = "",
        y = "",
        fill = "Virus"
      ) + theme_bw(base_size = 18) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            plot.margin = margin(t = 10, unit = "pt"),
            panel.border = element_blank(),
            axis.line.y = element_line(color = 'black'),
            axis.line.x = element_blank(),
            legend.box = "horizontal")
    
    if (pathogen == "COVID-19") {
      my_tag <- c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24")
    } else {
      my_tag <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
                  "2021-22", "2022-23", "2023-24")
    }

    plot <- tag_facet(plot,
                      x = 0.075, y = Inf,
                      hjust = -0.2, vjust = 0.4,
                      open = "", close = "",
                      fontface = 4,
                      size = 5,
                      family = "serif",
                      tag_pool = my_tag)
    
    return(plot)

  }

  mild_rsv <- plot_totals("Mild", "RSV")
  mild_flu <- plot_totals("Mild", "Influenza")
  mild_covid <- plot_totals("Mild", "COVID-19")
  severe_rsv <- plot_totals("Severe", "RSV")
  severe_flu <- plot_totals("Severe", "Influenza")
  severe_covid <- plot_totals("Severe", "COVID-19")

  legend_plot <- get_legend(input %>%
      mutate(
        codelist_type = factor(str_to_title(codelist_type), levels = c("Specific", "Sensitive")),
        virus = factor(virus, levels = c("RSV", "Influenza", "COVID-19"))
      ) %>% 
      ggplot() + geom_bar(aes(y = yearly_events, x = codelist_type, fill = virus, alpha = codelist_type),
                        stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "RSV" = cols[1], "Influenza" = cols[2],
        "COVID-19" = cols[3])) +
      scale_alpha_manual(values = c("Sensitive" = 0.5, "Specific" = 1),
                         na.translate = FALSE,
                         guide = guide_legend(position = NULL)) +
      facet_wrap(~subset, scales = "fixed", nrow = 1) + 
      labs(
        x = "",
        y = "",
        fill = "Virus",
        alpha = "Phenotype Used"
      ) + theme_bw(base_size = 16) + theme(legend.box = "horizontal"))

  if (cohort == "older_adults") {

    bottom_row_mild <- plot_grid(
      legend_plot,
      mild_covid,
      ncol = 2,
      rel_widths = c(0.345, 0.655)
    )

    mild <- plot_grid(
      plot_grid(
        NULL, mild_rsv, nrow = 1, rel_widths = c(0.005, 0.995)
      ),
      mild_flu,
      bottom_row_mild,
      nrow = 3
    ) %>% 
      annotate_figure(
        top = text_grob("Mild Outcomes", size = 16, face = "bold", vjust = 0.3)
      )

    bottom_row_severe <- plot_grid(
      legend_plot,
      severe_covid,
      ncol = 2,
      rel_widths = c(0.3545, 0.6455)
    )

    severe <- plot_grid(
      plot_grid(
        NULL, severe_rsv, nrow = 1, rel_widths = c(0.01, 0.99)
      ),
      severe_flu,
      bottom_row_severe,
      nrow = 3
    ) %>% 
      annotate_figure(
        top = text_grob("Severe Outcomes", size = 16, face = "bold", vjust = 0.3)
      )

    plot_grid(
      mild,
      severe,
      nrow = 2
    ) %>%
    annotate_figure(
      top = text_grob("Older Adults", hjust = 0, size = 18, face = "bold",
                      vjust = 0.5, x = unit(10, "pt"))
    )

  } else if (cohort == "infants") {

    bottom_row_mild <- plot_grid(
      legend_plot,
      mild_covid,
      ncol = 2,
      rel_widths = c(0.345, 0.655)
    )

    mild <- plot_grid(
      mild_rsv,
      plot_grid(
        NULL, mild_flu, nrow = 1, rel_widths = c(0.008, 0.992)
      ),
      bottom_row_mild,
      nrow = 3
    ) %>% 
      annotate_figure(
        top = text_grob("Mild Outcomes", size = 16, face = "bold", vjust = 0.3)
      )

    bottom_row_severe <- plot_grid(
      legend_plot,
      severe_covid,
      ncol = 2,
      rel_widths = c(0.3545, 0.6455)
    )

    severe <- plot_grid(
      severe_rsv,
      plot_grid(
        NULL, severe_flu, nrow = 1, rel_widths = c(0.002, 0.998)
      ),
      bottom_row_severe,
      nrow = 3
    ) %>% 
      annotate_figure(
        top = text_grob("Severe Outcomes", size = 16, face = "bold", vjust = 0.3)
      )

    plot_grid(
      mild,
      severe,
      nrow = 2
    ) %>%
    annotate_figure(
      top = text_grob("Infants", hjust = 0, size = 18, face = "bold",
                      vjust = 0.5, x = unit(10, "pt"))
    )

  }

}

###older adults

#import data
df_rsv_older_adults <- as.data.table(df_rsv_older_adults) %>%
  arrange(month)
df_rsv_older_adults <- df_rsv_older_adults[,
    total_events := sum(total_events, na.rm = T),
    by = .(month, event, codelist_type, virus)] %>%
  unique()

df_flu_older_adults <- as.data.table(df_flu_older_adults) %>%
  arrange(month)
df_flu_older_adults <- df_flu_older_adults[,
    total_events := sum(total_events, na.rm = T),
    by = .(month, event, codelist_type, virus)] %>%
  unique()

df_covid_older_adults <- as.data.table(df_covid_older_adults) %>%
  arrange(month)
df_covid_older_adults <- df_covid_older_adults[,
    total_events := sum(total_events, na.rm = T),
    by = .(month, event, codelist_type, virus)] %>%
  unique()

df_older_adults <- bind_rows(
  df_rsv_older_adults,
  df_flu_older_adults,
  df_covid_older_adults
) %>%
  arrange(month)

df_older_adults_yearly <- rlang::duplicate(df_older_adults)
df_older_adults_yearly <- df_older_adults_yearly %>% 
  mutate(year = year(month)) %>%
  select(-month)
df_older_adults_yearly <- df_older_adults_yearly[,
    yearly_events := sum(total_events, na.rm = T),
    by = .(subset, event, codelist_type, virus)] %>% 
  unique()
df_older_adults_yearly <- df_older_adults_yearly[,
    c("virus", "event", "codelist_type", "yearly_events", "year", "subset")]

older_adults_sizes <- sizes(df_older_adults_yearly, "older_adults")

#get some summary info
older_adults_summary <- df_older_adults_yearly %>% 
  group_by(virus, event, codelist_type, subset) %>% 
  summarise(
    minimum_events = min(yearly_events)
  )
min(older_adults_summary$minimum_events)

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "older_adults_outcome_ascertainment_overtime.png"),
       older_adults_sizes, width = 14, height = 9)

###infants

#import data
df_rsv_infants <- as.data.table(df_rsv_infants) %>%
  arrange(month)
df_rsv_infants <- df_rsv_infants[,
    total_events := sum(total_events, na.rm = T),
    by = .(month, event, codelist_type, virus)] %>%
  unique()

df_flu_infants <- as.data.table(df_flu_infants) %>%
  arrange(month)
df_flu_infants <- df_flu_infants[,
    total_events := sum(total_events, na.rm = T),
    by = .(month, event, codelist_type, virus)] %>%
  unique()

df_covid_infants <- as.data.table(df_covid_infants) %>%
  arrange(month)
df_covid_infants <- df_covid_infants[,
    total_events := sum(total_events, na.rm = T),
    by = .(month, event, codelist_type, virus)] %>%
  unique()

df_infants <- bind_rows(
  df_rsv_infants,
  df_flu_infants,
  df_covid_infants
) %>%
  arrange(month)

df_infants_yearly <- rlang::duplicate(df_infants)
df_infants_yearly <- df_infants_yearly %>% 
  mutate(year = year(month)) %>%
  select(-month)
df_infants_yearly <- df_infants_yearly[,
    yearly_events := sum(total_events, na.rm = T),
    by = .(subset, event, codelist_type, virus)] %>% 
  unique()
df_infants_yearly <- df_infants_yearly[,
    c("virus", "event", "codelist_type", "yearly_events", "year", "subset")]

infants_sizes <- sizes(df_infants_yearly, "infants")

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "infants_outcome_ascertainment_overtime.png"),
       infants_sizes, width = 14, height = 9)

#combine older adults and infants
plot_grid(
  older_adults_sizes,
  infants_sizes,
  nrow = 2
)

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "cohorts_outcome_ascertainment_overtime.png"),
       width = 14, height = 20)

#get some summary info
infants_summary <- df_infants_yearly %>% 
  group_by(virus, event, codelist_type, subset) %>% 
  summarise(
    minimum_events = min(yearly_events)
  )
min(infants_summary$minimum_events)
