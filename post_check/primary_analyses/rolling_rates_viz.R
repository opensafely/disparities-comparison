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
rsv_spec_ethnicity <- create_rolling_plots(season, "specific")$Ethnicity
rsv_sens_ethnicity <- create_rolling_plots(season, "sensitive")$Ethnicity
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

#flu
pathogen <- "flu"
season <- seasons(pathogen)
flu_spec_ethnicity <- create_rolling_plots(season, "specific")$Ethnicity
flu_sens_ethnicity <- create_rolling_plots(season, "sensitive")$Ethnicity
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

#covid
pathogen <- "covid"
season <- seasons(pathogen)
covid_spec_ethnicity <- create_rolling_plots(season, "specific")$Ethnicity
covid_sens_ethnicity <- create_rolling_plots(season, "sensitive")$Ethnicity
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

##plot together - spec
plot <- plot_grid(
  rsv_spec_ethnicity + scale_y_continuous(labels = scaleFUN),
  flu_spec_ethnicity + scale_y_continuous(labels = scaleFUN),
  covid_spec_ethnicity + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rates in Older Adults, by Ethnicity (Specific Phenotype)",
                  face = "bold", size = 14),
  bottom = text_grob("Date", vjust = -1), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
)

ggsave(here("post_check", "plots", "primary_analyses", "rates", "condensed",
            paste0(cohort, "_rates_over_time_spec_by_ethnicity", ".png")),
       plot_an, height = 10, width = 15)

##plot together - sens
plot <- plot_grid(
  rsv_sens_ethnicity + scale_y_continuous(labels = scaleFUN),
  flu_sens_ethnicity + scale_y_continuous(labels = scaleFUN),
  covid_sens_ethnicity + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rates, in Older Adults, by Ethnicity (Sensitive Phenotype)",
                  face = "bold", size = 14),
  bottom = text_grob("Date", vjust = -1), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
)

ggsave(here("post_check", "plots", "primary_analyses", "rates", "condensed",
            paste0(cohort, "_rates_over_time_sens_ethnicity", ".png")),
       plot_an, height = 10, width = 15)

###infants 

cohort <- "infants"

##plot rates for infants

#rsv
pathogen <- "rsv"
season <- seasons(pathogen)
rsv_spec_imd <- create_rolling_plots(season, "specific")$IMD
rsv_sens_imd <- create_rolling_plots(season, "sensitive")$IMD
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

#flu
pathogen <- "flu"
season <- seasons(pathogen)
flu_spec_imd <- create_rolling_plots(season, "specific")$IMD
flu_sens_imd <- create_rolling_plots(season, "sensitive")$IMD
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

#covid
pathogen <- "covid"
season <- seasons(pathogen)
covid_spec_imd <- create_rolling_plots(season, "specific")$IMD
covid_sens_imd <- create_rolling_plots(season, "sensitive")$IMD
# ##run once new releases out
# create_rolling_plots_overall(season, "specific")
# create_rolling_plots_overall(season, "sensitive")

##plot together
plot <- plot_grid(
  rsv_spec_imd + scale_y_continuous(labels = scaleFUN),
  flu_spec_imd + scale_y_continuous(labels = scaleFUN),
  covid_spec_imd + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rates, in Infants, by IMD (Specific Phenotype)",
                  face = "bold", size = 14),
  bottom = text_grob("Date", vjust = -1), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
)

ggsave(here("post_check", "plots", "primary_analyses", "rates", "condensed",
            paste0(cohort, "_rates_over_time_spec_imd", ".png")),
       plot_an, height = 10, width = 15)

##plot together
plot <- plot_grid(
  rsv_sens_imd + scale_y_continuous(labels = scaleFUN),
  flu_sens_imd + scale_y_continuous(labels = scaleFUN),
  covid_sens_imd + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rates, in Infants, by IMD (Sensitive Phenotype)",
                  face = "bold", size = 14),
  bottom = text_grob("Date", vjust = -1), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
)

ggsave(here("post_check", "plots", "primary_analyses", "rates", "condensed",
            paste0(cohort, "_rates_over_time_sens_imd", ".png")),
       plot_an, height = 10, width = 15)
