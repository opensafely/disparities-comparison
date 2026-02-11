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
    filter(month >= "2016-09-01")
  
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
  "seasonality_england.csv")) %>%
  arrange(month) %>%
  mutate(
    type = "Surveillance") %>%
  select(-covid_scaled) %>%
  mutate(
    total_covid = if_else(month < ymd("2020-03-01"), NA, total_covid)
  ) %>%
  # Generate a complete monthly sequence from 2016-09-01 to 2024-08-31
  complete(
    month = seq(ymd("2016-09-01"), ymd("2024-08-31"), by = "1 month")
  ) %>%
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

df_surv_yearly <- as.data.table(rlang::duplicate(df_surv))
df_surv_yearly <- df_surv_yearly %>% 
  mutate(year = year(month)) %>%
  select(-month)
df_surv_yearly <- df_surv_yearly[, yearly_events := sum(total_events, na.rm = T),
                                 by = .(year, event, codelist_type, virus)] %>% 
  unique()
df_surv_yearly <- df_surv_yearly[, c("virus", "event", "codelist_type", "yearly_events", "year")]

df_all <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>%
  arrange(month)

df_all_yearly <- rlang::duplicate(df_all)
df_all_yearly <- df_all_yearly %>% 
  mutate(year = year(month)) %>%
  select(-month)
df_all_yearly <- df_all_yearly[, yearly_events := sum(total_events, na.rm = T),
                               by = .(year, event, codelist_type, virus)] %>% 
  unique()
df_all_yearly <- df_all_yearly[, c("virus", "event", "codelist_type", "yearly_events", "year")]

sizes <- function(input) {

  f <- function(pal) brewer.pal(3, pal)
  cols <- f("Set2")

  plot_totals <- function(severity, pathogen) {

    surv <- df_surv_yearly %>%
      filter(virus == !!pathogen) %>%
      select(-event) %>%
      rename("surveillance" = yearly_events)

    df <- input %>%
      filter(event == !!severity, virus == !!pathogen) %>% 
      select(-event) %>%
      rename("ehr" = yearly_events)

    df <- full_join(df, surv, by = c("year", "codelist_type", "virus"),
                    relationship = "many-to-many") %>% 
      unique() %>% 
      mutate(
        fraction = ehr/surveillance,
        codelist_type = factor(str_to_title(codelist_type), levels = c("Specific", "Sensitive")),
        virus = factor(virus, levels = c("RSV", "Influenza", "COVID-19"))
      )
    
    if (pathogen == "COVID-19") {

      df <- df %>% 
        filter(year %in% c("2020", "2021", "2022", "2023", "2024"))

    }

    lims <- case_when(
      pathogen == "RSV" ~ c(0.1, 100),
      pathogen == "Influenza" ~ c(0.1, 100),
      pathogen == "COVID-19" ~ c(0.01, 10)
    )

    plot <- df %>%
      ggplot() + geom_bar(aes(y = fraction, x = codelist_type, fill = virus, alpha = codelist_type),
                        stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "RSV" = cols[1], "Influenza" = cols[2],
        "COVID-19" = cols[3])) +
      scale_alpha_manual(values = c("Sensitive" = 0.5, "Specific" = 1),
                        na.translate = FALSE,
                        guide = guide_legend(position = NULL)) +
      scale_y_log10() +
      # geom_text(aes(y = fraction, x = codelist_type,
      #               label = round(fraction, digits = 2)), size = 5) + 
      coord_cartesian(clip = "off", ylim = lims) +
      geom_hline(yintercept = 1, colour = "black", linewidth = 0.6) +
      facet_wrap(~year, scales = "fixed", nrow = 1) +
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
            axis.line.x = element_blank())
    
    if (pathogen == "COVID-19") {
      my_tag <- c("2020", "2021", "2022", "2023", "2024")
    } else {
      my_tag <- c("2016", "2017", "2018", "2019", "2020", "2021", "2022",
       "2023", "2024")
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
      facet_wrap(~year, scales = "fixed", nrow = 1) + 
      labs(
        x = "",
        y = "",
        fill = "Virus",
        alpha = "Phenotype Used"
      ) + theme_bw(base_size = 16))

  bottom_row_mild <- plot_grid(
    legend_plot,
    mild_covid,
    ncol = 2,
    rel_widths = c(0.43, 0.57)
  )

  mild <- plot_grid(
    mild_rsv,
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
    rel_widths = c(0.43, 0.57)
  )

  severe <- plot_grid(
    severe_rsv,
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
    left = text_grob("Ratio of EHR Data/Surveillance Data",
    rot = 90, vjust = 1.5, size = 18)
  )

}

sizes(df_all_yearly)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "size_scales_overtime.png"),
       width = 14, height = 18)
