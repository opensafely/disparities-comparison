library(dplyr)
library(here)
library(arrow)
library(lubridate)
library(readr)
library(survival)
library(cmprsk)
library(ggplot2)

## create output directories ----
fs::dir_create(here::here("output", "testing"))

#define study start date and study end date
source(here::here("analysis", "design", "design.R"))
is_being_sourced <- sys.nframe() > 0
if (is_being_sourced == FALSE) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    study_start_date <- "2020-09-01"
    study_end_date <- "2021-08-31"
    cohort <- "older_adults"
    codelist_type <- "specific"
    investigation_type <- "primary"
  } else {
    study_start_date <- study_dates[[args[[2]]]]
    study_end_date <- study_dates[[args[[3]]]]
    cohort <- args[[1]]
    codelist_type <- args[[4]]
    investigation_type <- args[[5]]
  }
}

df_input <- read_feather(
  here::here("output", "data", paste0("input_processed_", cohort, "_", 
             year(study_start_date), "_", year(study_end_date), "_", 
             codelist_type, "_", investigation_type,".arrow")))

#get relevant columns
df_filt <- df_input %>%
  select(patient_id, patient_index_date, patient_end_date,
         latest_ethnicity_group, covid_vaccination_immunity_date,
         covid_vaccination_mild, covid_vaccination_severe, covid_primary_date,
         covid_primary_inf, time_covid_primary,covid_secondary_date,
         covid_secondary_inf, time_covid_secondary) %>%
  mutate(
    covid_vaccination_primary = if_else(covid_vaccination_mild == "Yes", 1, 0),
    covid_vaccination_secondary = if_else(covid_vaccination_severe == "Yes", 1, 0),
    time_covid_vaccination = time_length(
      difftime(covid_vaccination_immunity_date, patient_index_date - days(1),
               "weeks"), "years")
  )

# Sort data by time_to_event
data_primary <- df_filt[order(df_filt$time_covid_primary), ]

# Calculate cumulative incidence for a specific event type (e.g., event_type == 1)
cum_incidence_primary <- cuminc(data_primary$time_covid_primary,
                                data_primary$covid_primary_inf == 1,
                                group = data_primary$latest_ethnicity_group)

# Keep only entries that actually have 'time' and 'est'
# Usually, they are named like '1 group=Control', '1 group=Treatment'
ci_list_primary <- cum_incidence_primary[sapply(
  cum_incidence_primary,
  function(x) is.list(x) && all(c("time", "est") %in% names(x)))]

# Now build the tidy dataframe
ci_df_primary <- bind_rows(
  lapply(names(ci_list_primary), function(name) {
    data.frame(
      time = ci_list_primary[[name]]$time,
      est = ci_list_primary[[name]]$est,
      group = gsub(".*=", "", name)  # Extract group name after '='
    )
  })
)

plt1 <- ggplot(ci_df_primary, aes(x = time, y = est, color = group)) +
  geom_step(linewidth = 1.2) +
  labs(
    x = "Time", 
    y = "Cumulative Incidence", 
    title = "Cumulative Incidence by Group",
    color = "Group"
  ) +
  theme_bw()

ggsave(
  here::here("output", "exploratory", paste0("cumulative_incidence_primary_", 
  cohort, "_", year(study_start_date), "_", year(study_end_date), "_", 
  codelist_type, "_", investigation_type,".png")),
  width = 8, height = 6, dpi = 300, plt1)

# Sort data by time_to_event
data_primary_vacc <- df_filt[order(df_filt$time_covid_vaccination), ]

# Calculate cumulative incidence for a specific event type (e.g., event_type == 1)
cum_incidence_primary_vacc <- cuminc(
  data_primary_vacc$time_covid_vaccination,
  data_primary_vacc$covid_vaccination_primary == 1,
  group = data_primary_vacc$latest_ethnicity_group)

# Keep only entries that actually have 'time' and 'est'
# Usually, they are named like '1 group=Control', '1 group=Treatment'
ci_list_primary_vacc <- cum_incidence_primary_vacc[sapply(
  cum_incidence_primary_vacc,
  function(x) is.list(x) && all(c("time", "est") %in% names(x)))]

# Now build the tidy dataframe
ci_df_primary_vacc <- bind_rows(
  lapply(names(ci_list_primary_vacc), function(name) {
    data.frame(
      time = ci_list_primary_vacc[[name]]$time,
      est = ci_list_primary_vacc[[name]]$est,
      group = gsub(".*=", "", name)  # Extract group name after '='
    )
  })
)

plt2 <- ggplot(ci_df_primary_vacc, aes(x = time, y = est, color = group)) +
  geom_step(linewidth = 1.2) +
  labs(
    x = "Time", 
    y = "Cumulative Incidence", 
    title = "Cumulative Incidence by Group",
    color = "Group"
  ) +
  theme_bw()

ggsave(
  here::here("output", "exploratory", paste0("cumulative_incidence_primary_vacc_", 
  cohort, "_", year(study_start_date), "_", year(study_end_date), "_", 
  codelist_type, "_", investigation_type,".png")),
  width = 8, height = 6, dpi = 300, plt2)

# Sort data by time_to_event
data_secondary <- df_filt[order(df_filt$time_covid_secondary), ]

# Calculate cumulative incidence for a specific event type (e.g., event_type == 1)
cum_incidence_secondary <- cuminc(data_secondary$time_covid_secondary,
                                  data_secondary$covid_secondary_inf == 1,
                                  group = data_secondary$latest_ethnicity_group)

# Keep only entries that actually have 'time' and 'est'
# Usually, they are named like '1 group=Control', '1 group=Treatment'
ci_list_secondary <- cum_incidence_secondary[sapply(
  cum_incidence_secondary,
  function(x) is.list(x) && all(c("time", "est") %in% names(x)))]

# Now build the tidy dataframe
ci_df_secondary <- bind_rows(
  lapply(names(ci_list_secondary), function(name) {
    data.frame(
      time = ci_list_secondary[[name]]$time,
      est = ci_list_secondary[[name]]$est,
      group = gsub(".*=", "", name)  # Extract group name after '='
    )
  })
)

plt3 <- ggplot(ci_df_secondary, aes(x = time, y = est, color = group)) +
  geom_step(linewidth = 1.2) +
  labs(
    x = "Time", 
    y = "Cumulative Incidence", 
    title = "Cumulative Incidence by Group",
    color = "Group"
  ) +
  theme_bw()

ggsave(
  here::here("output", "exploratory", paste0("cumulative_incidence_secondary_", 
  cohort, "_", year(study_start_date), "_", year(study_end_date), "_", 
  codelist_type, "_", investigation_type,".png")),
  width = 8, height = 6, dpi = 300, plt3)

# Sort data by time_to_event
data_secondary_vacc <- df_filt[order(df_filt$time_covid_vaccination), ]

# Calculate cumulative incidence for a specific event type (e.g., event_type == 1)
cum_incidence_secondary_vacc <- cuminc(
  data_secondary_vacc$time_covid_vaccination,
  data_secondary_vacc$covid_vaccination_secondary == 1,
  group = data_secondary_vacc$latest_ethnicity_group)

# Keep only entries that actually have 'time' and 'est'
# Usually, they are named like '1 group=Control', '1 group=Treatment'
ci_list_secondary_vacc <- cum_incidence_secondary_vacc[sapply(
  cum_incidence_secondary_vacc,
  function(x) is.list(x) && all(c("time", "est") %in% names(x)))]

# Now build the tidy dataframe
ci_df_secondary_vacc <- bind_rows(
  lapply(names(ci_list_secondary_vacc), function(name) {
    data.frame(
      time = ci_list_secondary_vacc[[name]]$time,
      est = ci_list_secondary_vacc[[name]]$est,
      group = gsub(".*=", "", name)  # Extract group name after '='
    )
  })
)

plt4 <- ggplot(ci_df_secondary_vacc, aes(x = time, y = est, color = group)) +
  geom_step(linewidth = 1.2) +
  labs(
    x = "Time", 
    y = "Cumulative Incidence", 
    title = "Cumulative Incidence by Group",
    color = "Group"
  ) +
  theme_bw()

ggsave(
  here::here("output", "testing", paste0("cumulative_incidence_secondary_vacc_", 
  cohort, "_", year(study_start_date), "_", year(study_end_date), "_", 
  codelist_type, "_", investigation_type,".png")),
  width = 8, height = 6, dpi = 300, plt4)
