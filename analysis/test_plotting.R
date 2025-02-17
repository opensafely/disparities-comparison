library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(survival)
library(broom)
library(readr)

## create output directories ----
fs::dir_create(here::here("analysis"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cohort <- "adults"
  } else {
    cohort <- args[[1]]
  }
}

df_input <- read_csv(
  paste0(here::here("output", "collated", "analytic"),
          "/", cohort, "_rsv_model_outputs_collated.csv"))


#create function to filter collated results to models wanted and then plot
forest <- function(df, model) {

    df_model <- df %>%
        filter(model_type == !!sym(model)) %>%
        group_by(season)

    tidy_forest <- df_model %>%
        # adding in the reference row for categorical variables
        tidy_add_reference_rows() %>%
        # adding a reference value to appear in plot
        tidy_add_estimate_to_reference_rows() %>%
        # adding the variable labels
        tidy_add_term_labels() %>%
        # removing intercept estimate from model
        tidy_remove_intercept()

    tidy_forest %>%
        mutate(
            plot_label = paste(var_label, label, sep = ":") %>%
            forcats::fct_inorder() %>%
            forcats::fct_rev()) %>%
        ggplot(aes(x = plot_label, y = estimate, ymin = conf.low,
               ymax = conf.high, color = variable)) +
            geom_hline(yintercept = 1, linetype = 2) +
            geom_pointrange() + coord_flip() +
            theme(legend.position = "none") +
        labs(y = "Rate Ratio",
             x = " ",
             title = "Forest Plot using broom.helpers")

}