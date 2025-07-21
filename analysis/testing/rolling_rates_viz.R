library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)
library(patchwork)
library(ggpubr)

#import plot function
source(here::here("testing", "functions", "rolling_rates.R"))
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

args <- commandArgs(trailingOnly = TRUE)
cohort <- args[[1]]

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

plot_spec <- plot_grid(
  plot, 
  get_legend(rsv_spec_ethnicity, position = "right"),
  ncol = 2, rel_widths = c(1, 0.15)
)

plot_an <- annotate_figure(
  plot_spec, 
  top = text_grob(
    expression(bold("30-Date Rolling Rates in Older Adults, by ") * 
                 bold(phantom("Ethnicity")) * 
                 bold(" (Specific Phenotype)")),
    size = 14
  ),
  bottom = text_grob("Date", vjust = 0, hjust = 1.7), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
) %>% annotate_figure(
  top = text_grob(
    expression(bold(phantom("30-Date Rolling Rates in Older Adults, by ")) * 
                 bold("Ethnicity") * 
                 bold(phantom(" (Specific Phenotype)"))),
    size = 14, col = '#2ca02c', vjust = 2.05, hjust = 0.482
  )
)

ggsave(here::here("output", "testing", "plots",
       paste0(cohort, "_rates_over_time_spec_by_ethnicity",
       ".png")), plot_an, height = 10, width = 15)

##plot together - sens
plot <- plot_grid(
  rsv_sens_ethnicity + scale_y_continuous(labels = scaleFUN),
  flu_sens_ethnicity + scale_y_continuous(labels = scaleFUN),
  covid_sens_ethnicity + scale_y_continuous(labels = scaleFUN),
  nrow = 3
)

plot_sens <- plot_grid(
  plot, 
  get_legend(rsv_sens_ethnicity, position = "right"),
  ncol = 2, rel_widths = c(1, 0.15)
)

plot_an <- annotate_figure(
  plot_sens, 
  top = text_grob(
    expression(bold("30-Date Rolling Rates in Older Adults, by ") * 
                 bold(phantom("Ethnicity")) * 
                 bold(" (Sensitive Phenotype)")),
    size = 14
  ),
  bottom = text_grob("Date", vjust = 0, hjust = 1.7), 
  left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                   rot = 90, vjust = 2)
) %>% annotate_figure(
  top = text_grob(
    expression(bold(phantom("30-Date Rolling Rates in Older Adults, by ")) * 
                 bold("Ethnicity") * 
                 bold(phantom(" (Sensitive Phenotype)"))),
    size = 14, col = '#2ca02c', vjust = 2.05, hjust = 0.482
  )
)

ggsave(here::here("output", "testing", "plots",
       paste0(cohort, "_rates_over_time_sens_ethnicity", ".png")),
       plot_an, height = 10, width = 15)

#plot spec and sens together
plot_an <- plot_grid(
  annotate_figure(plot_spec, top = text_grob("Specific Phenotype",
                  hjust = 0.8)),
  annotate_figure(plot_sens, top = text_grob("Sensitive Phenotype",
                  hjust = 0.8)),
  ncol = 1
) %>%
  annotate_figure(
    top = text_grob(
      expression(bold("30-Date Rolling Rates in Older Adults, by ") * 
                   bold(phantom("Ethnicity"))),
      size = 14, hjust = 0.55
    ),
    bottom = text_grob("Date", vjust = 0, hjust = 1.8), 
    left = text_grob("Rate Per 1000 Person Years (Midpoint 10 Derived)",
                     rot = 90, vjust = 2)
  ) %>% annotate_figure(
    top = text_grob(
      expression(bold(phantom("30-Date Rolling Rates in Older Adults, by ")) * 
                   bold("Ethnicity")),
      size = 14, col = '#2ca02c', vjust = 2.05, hjust = 0.52
    )
  )

ggsave(here::here("output", "testing", "plots",
       paste0(cohort, "_rates_over_time_ethnicity", ".png")),
       plot_an, height = 15, width = 12)
