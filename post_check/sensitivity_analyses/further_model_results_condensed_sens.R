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
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
investigation_type <- "sensitivity"

###--- older adults
cohort <- "older_adults"

##rsv
pathogen <- "rsv"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     "sensitivity", paste0(cohort, "_further_", pathogen,
                     "_model_outputs_collated_sensitivity.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2017_2018_specific_sensitivity.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
rsv_ethnicity_ses_mild <- forest_year_further_mult_sens(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_further_mult_sens(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##flu
pathogen <- "flu"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     "sensitivity", paste0(cohort, "_further_", pathogen,
                     "_model_outputs_collated_sensitivity.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2018_2019_specific_sensitivity.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
flu_ethnicity_ses_mild <- forest_year_further_mult_sens(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_further_mult_sens(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##mild

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none")
flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none")
top_row <- plot_grid(
  rsv_plot,
  flu_plot,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1)
)

# Extract the legend from the original plot
legend <- get_legend(rsv_ethnicity_ses_mild)

# Combine all plots
combined_plot_mild <- plot_grid(
  top_row,
  legend,
  ncol = 2,
  align = 'v',
  axis = 'lr',
  rel_widths = c(1, 0.2)
)

# Add annotations
ethnicity_ses_mild <- annotate_figure(
  combined_plot_mild,
  top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                         str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -0.5, hjust = 0.75), 
  left = text_grob(c("RSV", "Influenza"), 
                   x = 1.5, #hjust = 0, vjust = -16,
                   y = c(0.97, 0.485), 
                   just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "sensitivity_analyses", "condensed_models",
            paste0(cohort, "_mild_ethnicity_ses_further", ".png")),
       ethnicity_ses_mild, height = 15, width = 15)

##severe

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none")
flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none")
top_row <- plot_grid(
  rsv_plot,
  flu_plot,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1)
)

# Extract the legend from the original plot
legend <- get_legend(rsv_ethnicity_ses_severe)

# Combine all plots
combined_plot_severe <- plot_grid(
  top_row,
  legend,
  ncol = 2,
  align = 'v',
  axis = 'lr',
  rel_widths = c(1, 0.2)
)

# Add annotations
ethnicity_ses_severe <- annotate_figure(
  combined_plot_severe, 
  top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                         str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -0.5, hjust = 0.75), 
  left = text_grob(c("RSV", "Influenza"), 
                   x = 1.5, #hjust = 0, vjust = -16,
                   y = c(0.97, 0.485), 
                   just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "sensitivity_analyses", "condensed_models",
            paste0(cohort, "_severe_ethnicity_ses_further", ".png")),
       ethnicity_ses_severe, height = 15, width = 15)

#plot mild and severe together
final_combined <- plot_grid(
  annotate_figure(combined_plot_mild, top = text_grob(
    "Mild Outcomes", hjust = 0.65, size = 12.5)),
  annotate_figure(combined_plot_severe, top = text_grob(
    "Severe Outcomes", hjust = 0.65, size = 12.5)),
  nrow = 2
) %>%
  annotate_figure(
    top = text_grob(paste0("Rate Ratios of Outcomes in ",
                           str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14, hjust = 0.55),
    bottom = text_grob("Rate Ratio", vjust = -1, hjust = 0.8), 
    left = text_grob(c("RSV", "Influenza", "RSV", "Influenza"), 
                     x = 1.5, hjust = 0, vjust = -1,
                     y = c(0.96, 0.72,
                           0.468, 0.232), 
                     just = "left", face = "bold")
  )

ggsave(here("post_check", "plots", "sensitivity_analyses", "condensed_models",
            paste0(cohort, "_all_ethnicity_ses_further", ".png")),
       final_combined, height = 20, width = 14)

###--- infants
cohort <- "infants"

##rsv
pathogen <- "rsv"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     "sensitivity", paste0(cohort, "_further_", pathogen,
                     "_model_outputs_collated_sensitivity.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2017_2018_specific_sensitivity.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
rsv_ethnicity_ses_mild <- forest_year_further_mult_sens(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_further_mult_sens(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##flu
pathogen <- "flu"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                     "sensitivity", paste0(cohort, "_further_", pathogen,
                     "_model_outputs_collated_sensitivity.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2018_2019_specific_sensitivity.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
flu_ethnicity_ses_mild <- forest_year_further_mult_sens(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_further_mult_sens(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##mild

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none")
flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none")
top_row <- plot_grid(
  rsv_plot,
  flu_plot,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1)
)

# Extract the legend from the original plot
legend <- get_legend(rsv_ethnicity_ses_mild)

# Combine all plots
combined_plot_mild <- plot_grid(
  top_row,
  legend,
  ncol = 2,
  align = 'v',
  axis = 'lr',
  rel_widths = c(1, 0.2)
)

# Add annotations
ethnicity_ses_mild <- annotate_figure(
  combined_plot_mild,
  top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                         str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -0.5, hjust = 0.75), 
  left = text_grob(c("RSV", "Influenza"), 
                   x = 1.5, #hjust = 0, vjust = -16,
                   y = c(0.97, 0.485), 
                   just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "sensitivity_analyses", "condensed_models",
            paste0(cohort, "_mild_ethnicity_ses_further", ".png")),
       ethnicity_ses_mild, height = 15, width = 15)

##severe

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none")
flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none")
top_row <- plot_grid(
  rsv_plot,
  flu_plot,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1)
)

# Extract the legend from the original plot
legend <- get_legend(rsv_ethnicity_ses_severe)

# Combine all plots
combined_plot_severe <- plot_grid(
  top_row,
  legend,
  ncol = 2,
  align = 'v',
  axis = 'lr',
  rel_widths = c(1, 0.2)
)

# Add annotations
ethnicity_ses_severe <- annotate_figure(
  combined_plot_severe, 
  top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                         str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -0.5, hjust = 0.75), 
  left = text_grob(c("RSV", "Influenza"), 
                   x = 1.5, #hjust = 0, vjust = -16,
                   y = c(0.97, 0.485), 
                   just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "sensitivity_analyses", "condensed_models",
            paste0(cohort, "_severe_ethnicity_ses_further", ".png")),
       ethnicity_ses_severe, height = 15, width = 15)

#plot mild and severe together
final_combined <- plot_grid(
  annotate_figure(combined_plot_mild, top = text_grob(
    "Mild Outcomes", hjust = 0.65, size = 12.5)),
  annotate_figure(combined_plot_severe, top = text_grob(
    "Severe Outcomes", hjust = 0.65, size = 12.5)),
  nrow = 2
) %>%
  annotate_figure(
    top = text_grob(paste0("Rate Ratios of Outcomes in ",
                           str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14, hjust = 0.55),
    bottom = text_grob("Rate Ratio", vjust = -1, hjust = 0.8), 
    left = text_grob(c("RSV", "Influenza", "RSV", "Influenza"), 
                     x = 1.5, hjust = 0, vjust = -1,
                     y = c(0.96, 0.72,
                           0.468, 0.232), 
                     just = "left", face = "bold")
  )

ggsave(here("post_check", "plots", "sensitivity_analyses", "condensed_models",
            paste0(cohort, "_all_ethnicity_ses_further", ".png")),
       final_combined, height = 20, width = 14)
