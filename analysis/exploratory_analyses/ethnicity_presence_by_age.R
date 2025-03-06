library(tidyverse)
library(here)
library(arrow)
library(lubridate)

## create output directories ----
fs::dir_create(here::here("analysis", "exploratory_analyses"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  study_start_date <- "2016-09-01"
  study_end_date <- "2017-08-31"
  cohort <- "children_and_adolescents"
  codelist_type <- "specific"
  investigation_type <- "primary"
} else {
  study_start_date <- study_dates[[args[[2]]]]
  study_end_date <- study_dates[[args[[3]]]]
  cohort <- args[[1]]
  codelist_type <- args[[4]]
  investigation_type <- args[[5]]
}

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#get rounding function
source(here::here("analysis", "functions", "redaction.R"))

#look at the presence of ethnicity by age 
df_ethnicity <- df_input %>%
  mutate(
    ethnicity_present = if_else(is.na(latest_ethnicity_group),
                                FALSE, TRUE)
  ) %>%
  group_by(ethnicity_present, age) %>%
  summarise(
    n_midpoint10 = roundmid_any(n())
  ) %>%
  group_by(age) %>%
  mutate(
    proportion_midpoint10_derived = n_midpoint10 / sum(n_midpoint10) 
  )

#plot
plot <- df_ethnicity %>%
  ggplot(aes(x = age, y = proportion_midpoint10_derived,
             fill = ethnicity_present)) + geom_bar(stat = "identity") +
  labs(x = "Age", y = "Proportion of Patients (Midpoint 10 Derived)",
       subtitle = paste0(year(study_start_date), "-", year(study_end_date))) +
  theme_bw()

## create output directories ----
fs::dir_create(here::here("output", "exploratory"))

#write to file
write_csv(df_ethnicity, paste0(here::here("output", "exploratory"),
          "/", "ethnicity_by_age_", cohort, "_", year(study_start_date),
          "_", year(study_end_date), "_", codelist_type, "_",
          investigation_type, ".csv"))
#and the plot
ggsave(paste0(here::here("output", "exploratory"), "/",
      "ethnicity_by_age_", cohort, "_", year(study_start_date),
      "_", year(study_end_date), "_", codelist_type, "_",
      investigation_type, ".png"), plot)
