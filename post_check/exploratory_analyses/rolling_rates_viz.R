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
df_input <- read_csv(here::here("post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
rsv <- create_rolling_plots_overall(df_input)$plot

#pull legend
legend <- create_rolling_plots_overall(df_input)$legend

#flu
pathogen <- "flu"

#import data
df_input <- read_csv(here::here("post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
flu <- create_rolling_plots_overall(df_input)$plot

#covid
pathogen <- "covid"

#import data
df_input <- read_csv(here::here("post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
covid <- create_rolling_plots_overall(df_input)$plot

##plot together 

plot_col <- plot_grid(
  rsv,
  flu,
  covid,
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
  top = text_grob(paste0("30-Day Rolling Rates in ",
                         str_to_title(gsub("_", " ", cohort))), face = "bold",
                  size = 16, hjust = 0.65),
  bottom = text_grob("Month", vjust = -0.75, hjust = 1.25, size = 14), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 1.75, size = 14)
)

ggsave(here::here("post_check", "plots", "exploratory_analyses",
      "condensed", paste0(cohort, "_rates_over_time_all", ".png")),
       plot_an, height = 10, width = 16)

##overall resp
pathogen <- "overall_resp"

#import data
df_input <- read_csv(here::here("post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#plot
overall_resp <- create_rolling_plots_overall(df_input)$plot
legend2 <- create_rolling_plots_overall(df_input)$legend

plot <- plot_grid(
  overall_resp,
  legend2,
  ncol = 2,
  rel_widths = c(1, 0.1)
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob(paste0("30-Day Rolling Rates in ",
                         str_to_title(gsub("_", " ", cohort))), face = "bold",
                  size = 14, hjust = 0.65),
  bottom = text_grob("Month", vjust = -0.75, hjust = 1.25), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 1.75)
)

ggsave(here::here("post_check", "plots", "exploratory_analyses",
      "condensed", paste0(cohort, "_overall_resp_rates_over_time_all", ".png")),
       plot_an, height = 10, width = 16)

##plot all four together 

overall_resp_grid <- plot_grid(
  NULL,
  create_rolling_plots_overall(df_input)$plot_mild,
  NULL,
  create_rolling_plots_overall(df_input)$plot_severe,
  ncol = 4,
  rel_widths = c(0.92, 1, 0.92, 1)
)

plot_col <- plot_grid(
  rsv,
  flu,
  covid,
  overall_resp_grid,
  nrow = 4
)

plot <- plot_grid(
  plot_col,
  legend,
  ncol = 2,
  rel_widths = c(1, 0.1)
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob(paste0("30-Day Rolling Rates in ",
                         str_to_title(gsub("_", " ", cohort))), face = "bold",
                  size = 16, hjust = 0.65),
  bottom = text_grob("Month", vjust = -0.75, hjust = 1.25, size = 14), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 1.75, size = 14)
)

ggsave(here::here("post_check", "plots", "exploratory_analyses",
       "condensed", paste0(cohort, "_rates_over_time_all_plus", ".png")),
       plot_an, height = 10, width = 16)


###infants 

cohort <- "infants"

##plot rates for infants

#rsv
pathogen <- "rsv"

#import data
df_input <- read_csv(here::here("post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
rsv <- create_rolling_plots_overall(df_input)$plot

#pull legend
legend <- create_rolling_plots_overall(df_input)$legend

#flu
pathogen <- "flu"

#import data
df_input <- read_csv(here::here("post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
flu <- create_rolling_plots_overall(df_input)$plot

#covid
pathogen <- "covid"

#import data
df_input <- read_csv(here::here("post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#create plot
covid <- create_rolling_plots_overall(df_input)$plot

##plot together 

plot_col <- plot_grid(
  rsv,
  flu,
  covid,
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
  top = text_grob(paste0("30-Day Rolling Rates in ",
                         str_to_title(gsub("_", " ", cohort))), face = "bold",
                  size = 14, hjust = 0.65),
  bottom = text_grob("Month", vjust = -0.75, hjust = 1.25), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 1.75)
)

ggsave(here::here("post_check", "plots", "exploratory_analyses",
      "condensed", paste0(cohort, "_rates_over_time_all", ".png")),
       plot_an, height = 10, width = 16)

##overall resp
pathogen <- "overall_resp"

#import data
df_input <- read_csv(here::here("post_check",
                     "output", "collated", "descriptive",
                     "over_time", paste0(cohort, "_",
                     "rates_over_time_all_all_", pathogen, ".csv")))

#plot
overall_resp <- create_rolling_plots_overall(df_input)$plot
legend2 <- create_rolling_plots_overall(df_input)$legend

plot <- plot_grid(
  overall_resp,
  legend2,
  ncol = 2,
  rel_widths = c(1, 0.1)
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob(paste0("30-Day Rolling Rates in ",
                         str_to_title(gsub("_", " ", cohort))), face = "bold",
                  size = 16, hjust = 0.65),
  bottom = text_grob("Month", vjust = -0.75, hjust = 1.25, size = 14), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 1.75, size = 14)
)

ggsave(here::here("post_check", "plots", "exploratory_analyses",
      "condensed", paste0(cohort, "_overall_resp_rates_over_time_all", ".png")),
       plot_an, height = 10, width = 16)

##plot all four together 

overall_resp_grid <- plot_grid(
  NULL,
  create_rolling_plots_overall(df_input)$plot_mild,
  NULL,
  create_rolling_plots_overall(df_input)$plot_severe,
  ncol = 4,
  rel_widths = c(0.92, 1, 0.92, 1)
)

plot_col <- plot_grid(
  rsv,
  flu,
  covid,
  overall_resp_grid,
  nrow = 4
)

plot <- plot_grid(
  plot_col,
  legend,
  ncol = 2,
  rel_widths = c(1, 0.1)
)

plot_an <- annotate_figure(
  plot, 
  top = text_grob(paste0("30-Day Rolling Rates in ",
                         str_to_title(gsub("_", " ", cohort))), face = "bold",
                  size = 16, hjust = 0.65),
  bottom = text_grob("Month", vjust = -0.75, hjust = 1.25, size = 14), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 1.75, size = 14)
)

ggsave(here::here("post_check", "plots", "exploratory_analyses",
       "condensed", paste0(cohort, "_rates_over_time_all_plus", ".png")),
       plot_an, height = 10, width = 16)
