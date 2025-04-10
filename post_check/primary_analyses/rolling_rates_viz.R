library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)
library(patchwork)
library(ggpubr)

#import plot function
source(here::here("post_check", "functions", "rolling_rates.R"))
scaleFUN <- function(x) sprintf("%.4f", x)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

investigation_type <- "primary"

seasons <- function(pathogen) {
  case_when(
    pathogen == "rsv" ~ "season2",
    pathogen == "flu" ~ "season3",
    pathogen == "covid" ~ "season5"
  )
}

###older adults

cohort <- "older_adults"

##plot rates for older_adults

#rsv
pathogen <- "rsv"
season <- seasons(pathogen)
rsv_spec_sex <- create_rolling_plots(season, "specific")$Sex
rsv_sens_sex <- create_rolling_plots(season, "sensitive")$Sex
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

#flu
pathogen <- "flu"
season <- seasons(pathogen)
flu_spec_sex <- create_rolling_plots(season, "specific")$Sex
flu_sens_sex <- create_rolling_plots(season, "sensitive")$Sex
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

#covid
pathogen <- "covid"
season <- seasons(pathogen)
covid_spec_sex <- create_rolling_plots(season, "specific")$Sex
covid_sens_sex <- create_rolling_plots(season, "sensitive")$Sex
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

##plot together - spec
plot <- plot_grid(
  rsv_spec_sex + scale_y_continuous(labels = scaleFUN),
  flu_spec_sex + scale_y_continuous(labels = scaleFUN),
  covid_spec_sex + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rate by Sex",
                  face = "bold", size = 14),
  bottom = text_grob("Date", vjust = -1), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
)

ggsave(here("post_check", "plots", "primary_analyses", "rates", "condensed",
            paste0(cohort, "_rates_over_time_spec", ".png")),
       plot_an, height = 10, width = 15)

##plot together - sens
plot <- plot_grid(
  rsv_sens_sex + scale_y_continuous(labels = scaleFUN),
  flu_sens_sex + scale_y_continuous(labels = scaleFUN),
  covid_sens_sex + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rate by Sex",
                  face = "bold", size = 14),
  bottom = text_grob("Date", vjust = -1), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
)

ggsave(here("post_check", "plots", "primary_analyses", "rates", "condensed",
            paste0(cohort, "_rates_over_time_sens", ".png")),
       plot_an, height = 10, width = 15)

###infants 

cohort <- "infants"

##plot rates for infants

#rsv
pathogen <- "rsv"
season <- seasons(pathogen)
rsv_spec_sex <- create_rolling_plots(season, "specific")$Sex
rsv_sens_sex <- create_rolling_plots(season, "sensitive")$Sex
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

#flu
pathogen <- "flu"
season <- seasons(pathogen)
flu_spec_sex <- create_rolling_plots(season, "specific")$Sex
flu_sens_sex <- create_rolling_plots(season, "sensitive")$Sex
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

#covid
pathogen <- "covid"
season <- seasons(pathogen)
covid_spec_sex <- create_rolling_plots(season, "specific")$Sex
covid_sens_sex <- create_rolling_plots(season, "sensitive")$Sex
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

##plot together
plot <- plot_grid(
  rsv_spec_sex + scale_y_continuous(labels = scaleFUN),
  flu_spec_sex + scale_y_continuous(labels = scaleFUN),
  covid_spec_sex + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rate by Sex",
                  face = "bold", size = 14),
  bottom = text_grob("Date", vjust = -1), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
)

ggsave(here("post_check", "plots", "primary_analyses", "rates", "condensed",
            paste0(cohort, "_rates_over_time_spec", ".png")),
       plot_an, height = 10, width = 15)

##plot together
plot <- plot_grid(
  rsv_sens_sex + scale_y_continuous(labels = scaleFUN),
  flu_sens_sex + scale_y_continuous(labels = scaleFUN),
  covid_sens_sex + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rate by Sex",
                  face = "bold", size = 14),
  bottom = text_grob("Date", vjust = -1), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
)

ggsave(here("post_check", "plots", "primary_analyses", "rates", "condensed",
            paste0(cohort, "_rates_over_time_sens", ".png")),
       plot_an, height = 10, width = 15)
