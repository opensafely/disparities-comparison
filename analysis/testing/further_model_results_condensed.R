library(tidyr)
library(dplyr)
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
source(here::here("analysis", "testing", "functions", "forest.R"))
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
investigation_type <- "primary"
args <- commandArgs(trailingOnly = TRUE)
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cohort <- "children_and_adolescents"
  } else {
    cohort <- args[[1]]
  }
}

## create output directories ----
fs::dir_create(here::here("output", "testing", "plots"))

##rsv
pathogen <- "rsv"

#import collated model outputs
df_input <- read_csv(here::here("output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                     "_model_outputs_collated.csv")))
df_dummy <- sample_n(read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))), 10000) 

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

# #plot both phenotypes together
# rsv_ethnicity_mild <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ethnicity", "Mild"
# )
# 
# #plot both phenotypes together
# rsv_ses_mild <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ses", "Mild"
# )

#plot both phenotypes together
rsv_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

# if (cohort != "infants" & cohort != "infants_subgroup") {
#   
#   #plot both phenotypes together
#   rsv_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   rsv_ethnicity_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   rsv_ses_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ses_composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   rsv_full_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "full", "Mild"
#   )
#   
# }

##create relevant forest plots - severe

# #plot both phenotypes together
# rsv_ethnicity_severe <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ethnicity", "Severe"
# )
# 
# #plot both phenotypes together
# rsv_ses_severe <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ses", "Severe"
# )

#plot both phenotypes together
rsv_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

# if (cohort != "infants" & cohort != "infants_subgroup") {
#   
#   #plot both phenotypes together
#   rsv_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   rsv_ethnicity_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   rsv_ses_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ses_composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   rsv_full_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "full", "Severe"
#   )
#   
# }

##flu
pathogen <- "flu"

#import collated model outputs
df_input <- read_csv(here::here("output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                     "_model_outputs_collated.csv")))
df_dummy <- sample_n(read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_specific_primary.arrow"))), 10000)

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

# #plot both phenotypes together
# flu_ethnicity_mild <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ethnicity", "Mild"
# )
# 
# #plot both phenotypes together
# flu_ses_mild <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ses", "Mild"
# )

#plot both phenotypes together
flu_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

# if (cohort != "infants" & cohort != "infants_subgroup") {
#   
#   #plot both phenotypes together
#   flu_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   flu_ethnicity_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   flu_ses_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ses_composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   flu_full_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "full", "Mild"
#   )
#   
# }

##create relevant forest plots - severe

# #plot both phenotypes together
# flu_ethnicity_severe <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ethnicity", "Severe"
# )
# 
# #plot both phenotypes together
# flu_ses_severe <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ses", "Severe"
# )

#plot both phenotypes together
flu_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

# if (cohort != "infants" & cohort != "infants_subgroup") {
#   
#   #plot both phenotypes together
#   flu_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   flu_ethnicity_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   flu_ses_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ses_composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   flu_full_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "full", "Severe"
#   )
#   
# }

##covid
pathogen <- "covid"

#import collated model outputs
df_input <- read_csv(here::here("output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                     "_model_outputs_collated.csv")))
df_dummy <- sample_n(read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2021_2022_specific_primary.arrow"))), 10000)

if (cohort != "infants" & cohort != "infants_subgroup") {
  
  df_dummy <- df_dummy %>%
    mutate(
      subset = "2021_22",
      time_since_last_covid_vaccination = factor(if_else(
        is.na(covid_vaccination_immunity_date), "6-12m",
        as.character(time_since_last_covid_vaccination)),
        levels = c("0-6m", "6-12m", "12m+"))
    )
  
}

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

# #plot both phenotypes together
# covid_ethnicity_mild <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ethnicity", "Mild"
# )
# 
# #plot both phenotypes together
# covid_ses_mild <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ses", "Mild"
# )

#plot both phenotypes together
covid_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

# if (cohort != "infants" & cohort != "infants_subgroup") {
#   
#   df_dummy <- read_feather(
#     here::here("output", "data", paste0("input_processed_", cohort, 
#                "_2020_2021_specific_primary.arrow"))) %>%
#     mutate(
#       subset = "2020_21",
#       time_since_last_covid_vaccination = factor(case_when(
#         is.na(covid_vaccination_immunity_date) ~ "6-12m",
#         TRUE ~ "12m+"), levels = c("0-6m", "6-12m", "12m+"))
#     )
#   
#   #plot both phenotypes together
#   covid_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   covid_ethnicity_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   covid_ses_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ses_composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   covid_full_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "full", "Mild"
#   )
#   
# }

##create relevant forest plots - severe

# df_dummy <- read_feather(
#   here::here("output", "data", paste0("input_processed_", cohort, 
#              "_2021_2022_specific_primary.arrow")))
# 
# if (cohort != "infants" & cohort != "infants_subgroup") {
# 
#   df_dummy <- df_dummy %>%
#     mutate(
#       subset = "2021_22",
#       time_since_last_covid_vaccination = factor(if_else(
#         is.na(covid_vaccination_immunity_date), "6-12m",
#         as.character(time_since_last_covid_vaccination)),
#         levels = c("0-6m", "6-12m", "12m+"))
#     )
#   
# }

# #plot both phenotypes together
# covid_ethnicity_severe <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ethnicity", "Severe"
# )
# 
# #plot both phenotypes together
# covid_ses_severe <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ses", "Severe"
# )

#plot both phenotypes together
covid_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

# if (cohort != "infants" & cohort != "infants_subgroup") {
# 
#   df_dummy <- read_feather(
#     here::here("output", "data", paste0("input_processed_", cohort, 
#                "_2020_2021_specific_primary.arrow"))) %>%
#     mutate(
#       subset = "2020_21",
#       time_since_last_covid_vaccination = factor(case_when(
#         is.na(covid_vaccination_immunity_date) ~ "6-12m",
#         TRUE ~ "12m+"), levels = c("0-6m", "6-12m", "12m+"))
#     )
# 
#   #plot both phenotypes together
#   covid_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   covid_ethnicity_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   covid_ses_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ses_composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   covid_full_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "full", "Severe"
#   )
#     
# }

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
  rel_widths = c(1, 2.525), 
  nrow = 1
)

# Combine all plots
combined_plot_mild <- plot_grid(
  rsv_plot,
  flu_plot,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1, 1)
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

ggsave(here("output", "testing", "plots",
       paste0(cohort, "_mild_ethnicity_ses_further", ".png")),
       ethnicity_ses_mild, height = 15, width = 15)

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
  rel_widths = c(1, 2.525), 
  nrow = 1
)

# Combine all plots
combined_plot_severe <- plot_grid(
  rsv_plot,
  flu_plot,
  bottom_row,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1, 1)
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

ggsave(here("output", "testing", "plots",
            paste0(cohort, "_severe_ethnicity_ses_further", ".png")),
       ethnicity_ses_severe, height = 15, width = 15)

#plot mild and severe together
final_combined <- plot_grid(
  annotate_figure(combined_plot_mild, top = text_grob(
    "Mild Outcomes", hjust = -0.35, size = 12.5)),
  annotate_figure(combined_plot_severe, top = text_grob(
    "Severe Outcomes", hjust = -0.2, size = 12.5)),
  nrow = 2
) %>%
  annotate_figure(
    top = text_grob(paste0("Rate Ratios of Outcomes in ",
                           str_to_title(gsub("_", " ", cohort))),
                    face = "bold", size = 14, hjust = 0.25),
    bottom = text_grob("Rate Ratio", vjust = -1), 
    left = text_grob(c("RSV", "Influenza", "COVID-19",
                       "RSV", "Influenza", "COVID-19"), 
                     x = 1.5, hjust = 0, vjust = -16,
                     y = c(0.87, 0.71, 0.55,
                           0.38, 0.22, 0.06), 
                     just = "left", face = "bold")
  )

ggsave(here("output", "testing", "plots",
            paste0(cohort, "_all_ethnicity_ses_further", ".png")),
       final_combined, height = 20, width = 14)

##overall respiratory
pathogen <- "overall_and_all_cause"

#import collated model outputs
df_input <- read_csv(here::here("output", "collated", "analytic",
                     paste0(cohort, "_further_", pathogen,
                     "_model_outputs_collated.csv")))
df_dummy <- sample_n(read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, 
             "_2020_2021_sensitive_primary.arrow"))), 10000)

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild

# #plot both phenotypes together
# overall_resp_ethnicity_mild <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ethnicity", "Mild"
# )
# 
# #plot both phenotypes together
# overall_resp_ses_mild <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ses", "Mild"
# )

#plot both phenotypes together
overall_resp_ethnicity_ses_mild <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

# if (cohort != "infants" & cohort != "infants_subgroup") {
#   
#   #plot both phenotypes together
#   overall_resp_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   overall_resp_ethnicity_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ethnicity_composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   overall_resp_ses_composition_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ses_composition", "Mild"
#   )
#   
#   #plot both phenotypes together
#   overall_resp_full_mild <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "full", "Mild"
#   )
#   
# }

##create relevant forest plots - severe

# #plot both phenotypes together
# overall_resp_ethnicity_severe <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ethnicity", "Severe"
# )
# 
# #plot both phenotypes together
# overall_resp_ses_severe <- forest_year_further_mult(
#   df_input, df_dummy, pathogen, "ses", "Severe"
# )

#plot both phenotypes together
overall_resp_ethnicity_ses_severe <- forest_year_further_mult(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

# if (cohort != "infants" & cohort != "infants_subgroup") {
#   
#   #plot both phenotypes together
#   overall_resp_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   overall_resp_ethnicity_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ethnicity_composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   overall_resp_ses_composition_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "ses_composition", "Severe"
#   )
#   
#   #plot both phenotypes together
#   overall_resp_full_severe <- forest_year_further_mult(
#     df_input, df_dummy, pathogen, "full", "Severe"
#   )
#   
# }

#plot mild and severe together
final_combined <- plot_grid(
  overall_resp_ethnicity_ses_mild %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Mild Disease in ",
                             str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                       just = "left", face = "bold")
    ),
  overall_resp_ethnicity_ses_severe %>%
    annotate_figure(
      top = text_grob(paste0("Rate Ratios of Severe Disease in ",
                             str_to_title(gsub("_", " ", cohort))),
                      face = "bold", size = 14),
      bottom = text_grob("Rate Ratio", vjust = -1),
      left = text_grob("Overall Respiratory", vjust = -16.75, hjust = -0.75,
                       just = "left", face = "bold")
    ),
  nrow = 2
)

ggsave(here("output", "testing", "plots",
            paste0(cohort, "_overall_resp_ethnicity_ses_further", ".png")),
       final_combined, height = 10, width = 18)
