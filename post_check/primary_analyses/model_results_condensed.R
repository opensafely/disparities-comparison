library(tidyverse)
library(here)
library(arrow)
library(broom)
library(broom.helpers)
library(lubridate)
library(cowplot)
library(stringr)
library(patchwork)
library(ggpubr)

#import plot function
source(here::here("post_check", "functions", "forest.R"))

investigation_type <- "primary"

###older adults

cohort <- "older_adults"

##rsv
pathogen <- "rsv"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                paste0(cohort, "_", pathogen,
                                       "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow")))

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
rsv_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
rsv_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
rsv_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#plot both phenotypes together
rsv_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#plot both phenotypes together
rsv_ethnicity_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#plot both phenotypes together
rsv_ses_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#plot both phenotypes together
rsv_full_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
rsv_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
rsv_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
rsv_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#plot both phenotypes together
rsv_ethnicity_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#plot both phenotypes together
rsv_ses_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#plot both phenotypes together
rsv_full_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Severe"
)

##flu
pathogen <- "flu"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                paste0(cohort, "_", pathogen,
                                       "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
flu_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
flu_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
flu_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#plot both phenotypes together
flu_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#plot both phenotypes together
flu_ethnicity_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#plot both phenotypes together
flu_ses_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#plot both phenotypes together
flu_full_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
flu_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
flu_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
flu_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#plot both phenotypes together
flu_ethnicity_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#plot both phenotypes together
flu_ses_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#plot both phenotypes together
flu_full_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Severe"
)

##covid
pathogen <- "covid"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                paste0(cohort, "_", pathogen,
                                       "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow")))

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
covid_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
covid_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
covid_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Mild"
)

#plot both phenotypes together
covid_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

#plot both phenotypes together
covid_ethnicity_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
)

#plot both phenotypes together
covid_ses_composition_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Mild"
)

#plot both phenotypes together
covid_full_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
covid_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
covid_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
covid_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "composition", "Severe"
)

#plot both phenotypes together
covid_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#plot both phenotypes together
covid_ethnicity_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
)

#plot both phenotypes together
covid_ses_composition_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses_composition", "Severe"
)

#plot both phenotypes together
covid_full_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "full", "Severe"
)

#--- patch pathogens together
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

##mild

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none")
flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none")
covid_plot <- covid_ethnicity_ses_mild + theme(legend.position = "none")

# Extract the legend from the original plot
legend <- get_legend(rsv_ethnicity_ses_mild)

# Create the bottom row with legend and COVID plot
bottom_row <- plot_grid(
  legend, 
  covid_plot, 
  rel_widths = c(1, 1.725), 
  nrow = 1
)

# Combine all plots
combined_plot <- plot_grid(
  rsv_plot,
  flu_plot,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1, 1)
)

# Add annotations
ethnicity_ses <- annotate_figure(
  combined_plot, 
  top = text_grob("Rate Ratios of Mild Disease", face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -1), 
  left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                   x = 1.5, hjust = 0, vjust = -13,
                   y = c(0.83, 0.51, 0.195), 
                   just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
       paste0(cohort, "_mild_ethnicity_ses", ".png")),
       ethnicity_ses, height = 12, width = 15)

##severe

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none")
flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none")
covid_plot <- covid_ethnicity_ses_severe + theme(legend.position = "none")

# Extract the legend from the original plot
legend <- get_legend(rsv_ethnicity_ses_severe)

# Create the bottom row with legend and COVID plot
bottom_row <- plot_grid(
  legend, 
  covid_plot, 
  rel_widths = c(1, 1.725), 
  nrow = 1
)

# Combine all plots
combined_plot <- plot_grid(
  rsv_plot,
  flu_plot,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1, 1)
)

# Add annotations
ethnicity_ses <- annotate_figure(
  combined_plot, 
  top = text_grob("Rate Ratios of Severe Disease", face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -1), 
  left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                   x = 1.5, hjust = 0, vjust = -13,
                   y = c(0.83, 0.51, 0.195), 
                   just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_severe_ethnicity_ses", ".png")),
       ethnicity_ses, height = 12, width = 15)

###infants

cohort <- "infants"

##rsv
pathogen <- "rsv"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                paste0(cohort, "_", pathogen,
                                       "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
rsv_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
rsv_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
rsv_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
rsv_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
rsv_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##flu
pathogen <- "flu"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                paste0(cohort, "_", pathogen,
                                       "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
flu_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
flu_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
flu_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
flu_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
flu_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##covid
pathogen <- "covid"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                                paste0(cohort, "_", pathogen,
                                       "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
covid_ethnicity_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#plot both phenotypes together
covid_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#plot both phenotypes together
covid_ethnicity_ses_mild <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
covid_ethnicity_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#plot both phenotypes together
covid_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#plot both phenotypes together
covid_ethnicity_ses_severe <- forest_year_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#--- patch pathogens together
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

##mild

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none")
flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none")
covid_plot <- covid_ethnicity_ses_mild + theme(legend.position = "none")

# Extract the legend from the original plot
legend <- get_legend(rsv_ethnicity_ses_mild)

# Create the bottom row with legend and COVID plot
bottom_row <- plot_grid(
  legend, 
  covid_plot, 
  rel_widths = c(1, 1.725), 
  nrow = 1
)

# Combine all plots
combined_plot <- plot_grid(
  rsv_plot,
  flu_plot,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1, 1)
)

# Add annotations
ethnicity_ses <- annotate_figure(
  combined_plot, 
  top = text_grob("Rate Ratios of Mild Disease", face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -1), 
  left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                   x = 1.5, hjust = 0, vjust = -13,
                   y = c(0.83, 0.51, 0.195), 
                   just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_mild_ethnicity_ses", ".png")),
       ethnicity_ses, height = 12, width = 15)

##severe

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none")
flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none")
covid_plot <- covid_ethnicity_ses_severe + theme(legend.position = "none")

# Extract the legend from the original plot
legend <- get_legend(rsv_ethnicity_ses_severe)

# Create the bottom row with legend and COVID plot
bottom_row <- plot_grid(
  legend, 
  covid_plot, 
  rel_widths = c(1, 1.725), 
  nrow = 1
)

# Combine all plots
combined_plot <- plot_grid(
  rsv_plot,
  flu_plot,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1, 1)
)

# Add annotations
ethnicity_ses <- annotate_figure(
  combined_plot, 
  top = text_grob("Rate Ratios of Severe Disease", face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -1), 
  left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                   x = 1.5, hjust = 0, vjust = -13,
                   y = c(0.83, 0.51, 0.195), 
                   just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_severe_ethnicity_ses", ".png")),
       ethnicity_ses, height = 12, width = 15)
