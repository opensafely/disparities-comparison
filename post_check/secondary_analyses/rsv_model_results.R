library(tidyverse)
library(here)
library(arrow)
library(broom)
library(broom.helpers)
library(lubridate)
library(cowplot)
library(stringr)

#import plot function
source(here::here("post_check", "functions", "forest.R"))
#define parameters for plots
pathogen <- "rsv"
investigation_type <- "secondary"

###older adults

cohort <- "older_adults"

#import collated model outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "analytic", "secondary", paste0(cohort, "_", pathogen,
                     "_model_outputs_collated_secondary.csv")))
df_dummy <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort,
             "_2017_2018_specific_secondary.arrow")))

#extract models for which there were too few events
df_few <- df_input %>%
  filter(term == "too few events")

df_input <- df_input %>%
  filter(term != "too few events")

##create relevant forest plots - mild
## (empty collated inputs still yield reference-only panels)

#ethnicity
rsv_ethnicity_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Mild"
)

#ses
rsv_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ses", "Mild"
)

#ethnicity & ses
rsv_ethnicity_ses_mild <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Mild"
)

##create relevant forest plots - severe

#ethnicity
rsv_ethnicity_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity", "Severe"
)

#ses
rsv_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ses", "Severe"
)

#ethnicity & ses
rsv_ethnicity_ses_severe <- forest(
  df_input, df_dummy, pathogen, "ethnicity_ses", "Severe"
)

#create list of plots
plotlist <- list(
  rsv_ethnicity_mild,
  rsv_ethnicity_severe,
  rsv_ses_mild,
  rsv_ses_severe,
  rsv_ethnicity_ses_mild,
  rsv_ethnicity_ses_severe
)
plot_names <- c(
  "rsv_ethnicity_mild",
  "rsv_ethnicity_severe",
  "rsv_ses_mild",
  "rsv_ses_severe",
  "rsv_ethnicity_ses_mild",
  "rsv_ethnicity_ses_severe"
)

dir.create(
  here("post_check", "plots", "supplemental", "models", cohort, "secondary"),
  recursive = TRUE, showWarnings = FALSE
)

for (i in seq_along(plotlist)) {
  p <- plotlist[[i]]
  name <- plot_names[i]

  print(p)

  ggsave(
    here("post_check", "plots", "supplemental", "models", cohort, "secondary",
         paste0(cohort, "_", name, ".png")),
    p, height = 10, width = 8
  )
}

#assign plot names to list
names(plotlist) <- plot_names

#save Rdata
save_dashboard_plotlist(plotlist, file = here("post_check", "supplemental", "dashboard",
                           paste0(cohort, "_rsv_model_results_secondary.RData")))

## ethnicity & SES — key exposure variables only
key_vars_plotlist <- list(
  forest(df_input, df_dummy, pathogen, "ethnicity_ses", "Mild",
         key_vars_only = TRUE),
  forest(df_input, df_dummy, pathogen, "ethnicity_ses", "Severe",
         key_vars_only = TRUE)
)
key_vars_plot_names <- c(
  "rsv_ethnicity_ses_mild_key_vars",
  "rsv_ethnicity_ses_severe_key_vars"
)

dir.create(
  here("post_check", "plots", "secondary_analyses"),
  recursive = TRUE, showWarnings = FALSE
)

for (i in seq_along(key_vars_plotlist)) {
  p <- key_vars_plotlist[[i]]
  name <- key_vars_plot_names[i]

  print(p)

  ggsave(
    here("post_check", "plots", "secondary_analyses",
         paste0(cohort, "_", name, ".png")),
    p, height = 10, width = 8
  )
}
