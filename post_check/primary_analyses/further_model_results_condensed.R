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
investigation_type <- "primary"

###--- older adults
cohort <- "older_adults"

##rsv
pathogen <- "rsv"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                    paste0(cohort, "_further_", pathogen,
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
rsv_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##flu
pathogen <- "flu"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                    paste0(cohort, "_further_", pathogen,
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
flu_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##covid
pathogen <- "covid"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                    paste0(cohort, "_further_", pathogen,
                    "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
            "_2021_2022_specific_primary.arrow"))) %>%
  mutate(
    subset = "2021_22",
    time_since_last_covid_vaccination = if_else(
      is.na(covid_vaccination_immunity_date), "6-12m",
      time_since_last_covid_vaccination)
  )

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
covid_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
            "_2021_2022_specific_primary.arrow"))) %>%
  mutate(
    subset = "2021_22",
    time_since_last_covid_vaccination = if_else(
      is.na(covid_vaccination_immunity_date), "6-12m",
      time_since_last_covid_vaccination)
  )

#plot both phenotypes together
covid_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##mild

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none",
                                           axis.text.x = element_blank(),
                                           axis.ticks = element_blank(),
                                           axis.title.x = element_blank())
flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none",
                                           axis.title.x = element_text(size = 12))
covid_plot <- covid_ethnicity_ses_mild + theme(legend.position = "none",
                                               axis.text.x = element_blank(),
                                               axis.ticks = element_blank(),
                                               axis.title.x = element_blank())

# Extract the legend from the original plot
legend <- get_legend(flu_ethnicity_ses_mild)

# Create the bottom row with legend and COVID plot
bottom_row <- plot_grid(
  legend, 
  covid_plot, 
  NULL,
  rel_widths = c(1, 3.15, -0.05), 
  nrow = 1
)

# Combine all plots
combined_plot_mild <- plot_grid(
  rsv_plot,
  NULL,
  flu_plot,
  NULL,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, -0.0475, 1.25, -0.0475, 1.1)
)

# Add annotations
ethnicity_ses_mild <- annotate_figure(
  combined_plot_mild, 
  top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                        str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -1), 
  left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                  x = 1.5, hjust = 0, vjust = -16,
                  y = c(0.836, 0.515, 0.195), 
                  just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
      paste0(cohort, "_mild_ethnicity_ses_further", ".png")),
      ethnicity_ses_mild, height = 15, width = 15)

##severe

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none",
                                             axis.text.x = element_blank(),
                                             axis.ticks = element_blank(),
                                             axis.title.x = element_blank())
flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none",
                                             axis.title.x = element_text(size = 12))
covid_plot <- covid_ethnicity_ses_severe + theme(legend.position = "none",
                                                 axis.text.x = element_blank(),
                                                 axis.ticks = element_blank(),
                                                 axis.title.x = element_blank())

# Extract the legend from the original plot
legend <- get_legend(flu_ethnicity_ses_severe)

# Create the bottom row with legend and COVID plot
bottom_row <- plot_grid(
  legend, 
  covid_plot,
  NULL, 
  rel_widths = c(1, 3.15, -0.05), 
  nrow = 1
)

# Combine all plots
combined_plot_severe <- plot_grid(
  rsv_plot,
  NULL,
  flu_plot,
  NULL,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, -0.0475, 1.25, -0.0475, 1.1)
)

# Add annotations
ethnicity_ses_severe <- annotate_figure(
  combined_plot_severe, 
  top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                        str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -1), 
  left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                  x = 1.5, hjust = 0, vjust = -16,
                  y = c(0.836, 0.515, 0.195), 
                  just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_severe_ethnicity_ses_further", ".png")),
      ethnicity_ses_severe, height = 15, width = 15)

#plot mild and severe together
final_combined <- plot_grid(
  NULL,
  plot_grid(
    annotate_figure(combined_plot_mild, top = text_grob(
      "Mild Outcomes", hjust = -0.7, vjust = 1.45, size = 12.5, face = "bold")),
    NULL,
    annotate_figure(combined_plot_severe, top = text_grob(
      "Severe Outcomes", hjust = -0.5, vjust = 1.45, size = 12.5, face = "bold")),
    nrow = 3,
    rel_heights = c(1, -0.01, 1)
  ),
  NULL,
  ncol = 3,
  rel_widths = c(-0.075, 1, -0.065)
) %>%
  annotate_figure(
    left = text_grob(c("RSV", "Influenza",
                       "RSV", "Influenza"), 
                     x = 1,
                     y = c(0.885, 0.74235,
                           0.388, 0.2448), 
                     hjust = 0, vjust = -16,
                     just = "left", face = "bold"),
    right = text_grob(c("COVID-19", "COVID-19"),
                      x = 1, y = c(0.559, 0.06234),
                      hjust = 10, vjust = -16,
                      just = "left", face = "bold")
  )

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_all_ethnicity_ses_further", ".png")),
      final_combined, height = 21, width = 14)

##overall respiratory
pathogen <- "overall_and_all_cause"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                    paste0(cohort, "_further_", pathogen,
                    "_model_outputs_collated.csv")))
df_dummy <- read_feather(
here::here("output", "data", paste0("input_processed_", cohort, 
            "_2020_2021_sensitive_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
filter(term == "too few events")

df_input <- df_input %>%
filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
overall_resp_ethnicity_ses_mild <- forest_year_further_mult(
df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
overall_resp_ethnicity_ses_severe <- forest_year_further_mult(
df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#plot mild and severe together
final_combined <- plot_grid(
overall_resp_ethnicity_ses_mild %>%
  annotate_figure(
    top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                            str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
    left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                      just = "left", face = "bold")
  ),
overall_resp_ethnicity_ses_severe %>%
  annotate_figure(
    top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                            str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
    left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                      just = "left", face = "bold")
  ),
nrow = 2
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
          paste0(cohort, "_overall_resp_ethnicity_ses_further", ".png")),
      final_combined, height = 10, width = 18)

###--- infants
cohort <- "infants"

##rsv
pathogen <- "rsv"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                    paste0(cohort, "_further_", pathogen,
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
rsv_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##flu
pathogen <- "flu"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                    paste0(cohort, "_further_", pathogen,
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
flu_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##covid
pathogen <- "covid"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                    paste0(cohort, "_further_", pathogen,
                    "_model_outputs_collated.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
            "_2021_2022_specific_primary.arrow")))

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
covid_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
covid_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

##mild

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_mild + theme(legend.position = "none",
                                           axis.text.x = element_blank(),
                                           axis.ticks = element_blank(),
                                           axis.title.x = element_blank())
flu_plot <- flu_ethnicity_ses_mild + theme(legend.position = "none",
                                           axis.title.x = element_text(size = 12))
covid_plot <- covid_ethnicity_ses_mild + theme(legend.position = "none",
                                               axis.text.x = element_blank(),
                                               axis.ticks = element_blank(),
                                               axis.title.x = element_blank())

# Extract the legend from the original plot
legend <- get_legend(flu_ethnicity_ses_mild)

# Create the bottom row with legend and COVID plot
bottom_row <- plot_grid(
  legend, 
  covid_plot, 
  NULL,
  rel_widths = c(1, 2.25, -0.025), 
  nrow = 1
)

# Combine all plots
combined_plot_mild <- plot_grid(
  rsv_plot,
  NULL,
  flu_plot,
  NULL,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, -0.0475, 1.25, -0.0475, 1.1)
)

# Add annotations
ethnicity_ses_mild <- annotate_figure(
  combined_plot_mild, 
  top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                        str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -1), 
  left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                  x = 1.5, hjust = 0, vjust = -16,
                  y = c(0.836, 0.515, 0.195), 
                  just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
      paste0(cohort, "_mild_ethnicity_ses_further", ".png")),
      ethnicity_ses_mild, height = 15, width = 15)

##severe

# Create versions of your plots without legends
rsv_plot <- rsv_ethnicity_ses_severe + theme(legend.position = "none",
                                             axis.text.x = element_blank(),
                                             axis.ticks = element_blank(),
                                             axis.title.x = element_blank())
flu_plot <- flu_ethnicity_ses_severe + theme(legend.position = "none",
                                             axis.title.x = element_text(size = 12))
covid_plot <- covid_ethnicity_ses_severe + theme(legend.position = "none",
                                                 axis.text.x = element_blank(),
                                                 axis.ticks = element_blank(),
                                                 axis.title.x = element_blank())

# Extract the legend from the original plot
legend <- get_legend(flu_ethnicity_ses_severe)

# Create the bottom row with legend and COVID plot
bottom_row <- plot_grid(
  legend, 
  covid_plot, 
  NULL,
  rel_widths = c(1, 2.25, -0.025), 
  nrow = 1
)

# Combine all plots
combined_plot_severe <- plot_grid(
  rsv_plot,
  NULL,
  flu_plot,
  NULL,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, -0.0475, 1.25, -0.0475, 1.1)
)

# Add annotations
ethnicity_ses_severe <- annotate_figure(
  combined_plot_severe, 
  top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                        str_to_title(gsub("_", " ", cohort))),
                  face = "bold", size = 14),
  bottom = text_grob("Rate Ratio", vjust = -1), 
  left = text_grob(c("RSV", "Influenza", "COVID-19"), 
                  x = 1.5, hjust = 0, vjust = -16,
                  y = c(0.836, 0.515, 0.195), 
                  just = "left", face = "bold")
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_severe_ethnicity_ses_further", ".png")),
      ethnicity_ses_severe, height = 15, width = 15)

#plot mild and severe together
final_combined <- plot_grid(
  NULL,
  plot_grid(
    annotate_figure(combined_plot_mild, top = text_grob(
      "Mild Outcomes", hjust = -0.5, vjust = 1.45, size = 12.5, face = "bold")),
    NULL,
    annotate_figure(combined_plot_severe, top = text_grob(
      "Severe Outcomes", hjust = -0.325, vjust = 1.45, size = 12.5, face = "bold")),
    nrow = 3,
    rel_heights = c(1, -0.01, 1)
  ),
  NULL,
  ncol = 3,
  rel_widths = c(-0.075, 1, -0.065)
) %>%
  annotate_figure(
    left = text_grob(c("RSV", "Influenza",
                       "RSV", "Influenza"), 
                     x = 0.5,
                     y = c(0.885, 0.74235,
                           0.388, 0.2448), 
                     hjust = 0, vjust = -16,
                     just = "left", face = "bold"),
    right = text_grob(c("COVID-19", "COVID-19"),
                      x = 1, y = c(0.559, 0.06234),
                      hjust = 10, vjust = -16,
                      just = "left", face = "bold")
  )

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
            paste0(cohort, "_all_ethnicity_ses_further", ".png")),
      final_combined, height = 21, width = 14)

##overall respiratory
pathogen <- "overall_and_all_cause"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated", "analytic",
                    paste0(cohort, "_further_", pathogen,
                    "_model_outputs_collated.csv")))
df_dummy <- read_feather(
here::here("output", "data", paste0("input_processed_", cohort, 
            "_2020_2021_sensitive_primary.arrow"))) 

#extract models for which there were too few events
df_few <- df_input %>%
filter(term == "too few events")

df_input <- df_input %>%
filter(term != "too few events")

##create relevant forest plots - mild

#plot both phenotypes together
overall_resp_ethnicity_ses_mild <- forest_year_further_mult(
df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#plot both phenotypes together
overall_resp_ethnicity_ses_severe <- forest_year_further_mult(
df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#plot mild and severe together
final_combined <- plot_grid(
overall_resp_ethnicity_ses_mild %>%
  annotate_figure(
    top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                            str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
    left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                      just = "left", face = "bold")
  ),
overall_resp_ethnicity_ses_severe %>%
  annotate_figure(
    top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                            str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14),
    bottom = text_grob("Rate Ratio", vjust = -1, hjust = -0.5),
    left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                      just = "left", face = "bold")
  ),
nrow = 2
)

ggsave(here("post_check", "plots", "primary_analyses", "condensed_models",
          paste0(cohort, "_overall_resp_ethnicity_ses_further", ".png")),
      final_combined, height = 10, width = 18)
