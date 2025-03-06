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
covid_season_min <- as.Date("2019-09-01")
covid_current_vacc_min = as.Date("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = as.Date("2021-09-01", "%Y-%m-%d")

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
    n = n()
  ) %>%
  group_by(age) %>%
  mutate(
    proportion = roundmid_any(n) / roundmid_any(sum(n))
  )

#plot
plot <- df_ethnicity %>%
  ggplot(aes(x = age, y = proportion, fill = ethnicity_present)) +
  geom_bar(stat = "identity") + theme_bw() + 
  labs(subtitle = paste0(year(study_start_date), "-", year(study_end_date)))

## create output directories ----
fs::dir_create(here::here("output", "exploratory"))

#write to file
write_csv(df_ethnicity, paste0(here::here("output", "exploratory"),
          "/", "ethnicity_by_age_", cohort, "_", year(study_start_date),
          "_", year(study_end_date), "_", codelist_type, "_",
          investigation_type, ".csv"))
ggsave(paste0(here::here("output", "exploratory"), "/",
      "ethnicity_by_age_", cohort, "_", year(study_start_date),
      "_", year(study_end_date), "_", codelist_type, "_",
      investigation_type, ".png"), plot)
