library(gtsummary)
library(tidyr)

#create function to summarise reinfections
reinfections <- function(df, pathogen, severity) {
  
  pathogen_label <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  tbl_sum <- df %>%
    filter(infection_type == pathogen) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    select(-c(infection_type, subset)) %>%
    tbl_summary(
      by = outcome_type,
      type = list(
        number_infected_midpoint10 = "continuous",
        number_reinfected_midpoint10 = "continuous",
        proportion_reinfected_midpoint10_derived = "continuous",
        median_time_to_reinfection = "continuous",
        number_reinfected_28_days_midpoint10 = "continuous",
        proportion_reinfected_in_28_days_midpoint10_derived = "continuous"
      ),
      statistic = list(all_continuous() ~ "{mean} ({sd})"),
      label = list(
        number_infected_midpoint10 = paste0(
          "Number of ", pathogen_label, " infections"),
        number_reinfected_midpoint10 = paste0(
          "Number of ", pathogen_label, " reinfections"),
        proportion_reinfected_midpoint10_derived = paste0(
          "Proportion of ", pathogen_label, " reinfections"),
        median_time_to_reinfection = "Median time to reinfection (days)",
        number_reinfected_28_days_midpoint10 = paste0(
          "Number of ", pathogen_label, " reinfections within 28 days"),
        proportion_reinfected_in_28_days_midpoint10_derived = paste0(
          "Proportion of ", pathogen_label, " reinfections within 28 days")
      )
    )
  
  return(tbl_sum)  
  
}

#create function to summarise multiple episodes
multiple_episodes <- function(df, severity) {
  
  tbl_sum <- df %>%
    filter(!str_detect(combo, "Total")) %>%
    mutate_if(is.factor, forcats::fct_explicit_na, na_level = "Unknown") %>%
    mutate(
      combo = case_when(
        combo == "0_0" ~ "No RSV or flu",
        combo == "0_0_0" ~ "No RSV, flu, or COVID-19",
        combo == "0_0_COVID_Mild" ~ "Only COVID-19",
        combo == "0_0_COVID_Severe" ~ "Only COVID-19",
        combo == "0_Flu_Mild" ~ "Only flu",
        combo == "0_Flu_Mild_0" ~ "Only flu (post COVID-19)",
        combo == "0_Flu_Severe" ~ "Only flu",
        combo == "0_Flu_Severe_0" ~ "Only flu (post COVID-19)",
        combo == "0_Flu_Mild_COVID_Mild" ~ "Flu and COVID-19",
        combo == "0_Flu_Severe_COVID_Severe" ~ "Flu and COVID-19",
        combo == "RSV_Mild_0" ~ "Only RSV",
        combo == "RSV_Mild_0_0" ~ "Only RSV (post COVID-19)",
        combo == "RSV_Severe_0" ~ "Only RSV",
        combo == "RSV_Severe_0_0" ~ "Only RSV (post COVID-19)",
        combo == "RSV_Mild_0_COVID_Mild" ~ "RSV and COVID-19",
        combo == "RSV_Severe_0_COVID_Severe" ~ "RSV and COVID-19",
        combo == "RSV_Mild_Flu_Mild" ~ "RSV and flu",
        combo == "RSV_Mild_Flu_Mild_0" ~ "RSV and flu (post COVID-19)",
        combo == "RSV_Severe_Flu_Severe" ~ "RSV and flu",
        combo == "RSV_Severe_Flu_Severe_0" ~ "RSV and flu (post COVID-19)"
      )
    ) %>%
    filter(outcome_type == severity) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    select(-c(outcome_type, subset)) %>%
    pivot_wider(cols = c(characteristic), names_from = combo,
                values_from = `n (midpoint 10 rounded)`) %>%
    tbl_summary(
      by = combo,
      type = list(value = "continuous"),
      statistic = list(all_continuous() ~ "{mean} ({sd})")
      )
  
  return(tbl_sum)  
  
}
