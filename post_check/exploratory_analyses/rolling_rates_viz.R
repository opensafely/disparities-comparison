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

#import data
df_input <- read_csv(here::here(#"post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
rsv <- create_rolling_plots_overall(df_input)

#flu
pathogen <- "flu"

#import data
df_input <- read_csv(here::here(#"post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
flu <- create_rolling_plots_overall(df_input)

#covid
pathogen <- "covid"

#import data
df_input <- read_csv(here::here(#"post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
covid <- create_rolling_plots_overall(df_input)

##plot together 

#pull legend
legend <- get_legend(rsv)

plot_col <- plot_grid(
  rsv + scale_y_continuous(labels = scaleFUN) +
    theme(legend.position = "none"),
  flu + scale_y_continuous(labels = scaleFUN) +
    theme(legend.position = "none"),
  covid + scale_y_continuous(labels = scaleFUN) +
    theme(legend.position = "none"),
  nrow = 3
)

plot <- plot_grid(
  plot_col,
  legend,
  ncol = 2,
  rel_widths = c(1, 0.1)
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rate", face = "bold",
                  size = 14, hjust = 0.65),
  bottom = text_grob("Month", vjust = -0.75, hjust = 1.25), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 1.75)
)

ggsave(here("post_check", "plots", "exploratory_analyses", "condensed",
            paste0(cohort, "_rates_over_time_all", ".png")),
       plot_an, height = 10, width = 15)

###infants 

cohort <- "infants"

##plot rates for infants

#rsv
pathogen <- "rsv"

#import data
df_input <- read_csv(here::here(#"post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
rsv <- create_rolling_plots_overall(df_input)

#flu
pathogen <- "flu"

#import data
df_input <- read_csv(here::here(#"post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
flu <- create_rolling_plots_overall(df_input)

#covid
pathogen <- "covid"

#import data
df_input <- read_csv(here::here(#"post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
covid <- create_rolling_plots_overall(df_input)

##plot together 

#pull legend
legend <- get_legend(rsv)

plot_col <- plot_grid(
  rsv + scale_y_continuous(labels = scaleFUN) +
    theme(legend.position = "none"),
  flu + scale_y_continuous(labels = scaleFUN) +
    theme(legend.position = "none"),
  covid + scale_y_continuous(labels = scaleFUN) +
    theme(legend.position = "none"),
  nrow = 3
)

plot <- plot_grid(
  plot_col,
  legend,
  ncol = 2,
  rel_widths = c(1, 0.1)
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob("30-Date Rolling Rate", face = "bold",
                  size = 14, hjust = 0.65),
  bottom = text_grob("Month", vjust = -0.75, hjust = 1.25), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 1.75)
)

ggsave(here("post_check", "plots", "exploratory_analyses", "condensed",
            paste0(cohort, "_rates_over_time_all", ".png")),
       plot_an, height = 10, width = 15)
