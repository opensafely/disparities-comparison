library(gtsummary)
library(tidyr)
library(stringr)
library(purrr)

#create function to summarise reinfections
reinfections <- function(df, pathogen, seasons) {
  
  pathogen_label <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  if (pathogen == "covid") {
    
    seasons <- seasons[
      seasons %in% c("2019_20", "2020_21", "2021_22", "2022_23", "2023_24")]
    
  }
  
  tbl_sum <- list()
  
  for (season in seasons) {
    
    tbl_sum[[season]] <- df %>%
      filter(infection_type == pathogen, subset == !!season) %>%
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
        statistic = list(all_continuous() ~ "{mean}"),
        label = list(
          number_infected_midpoint10 = paste0(
            "Number of infections"),
          number_reinfected_midpoint10 = paste0(
            "Number of reinfections"),
          proportion_reinfected_midpoint10_derived = paste0(
            "Proportion of reinfections"),
          median_time_to_reinfection = "Median time to reinfection (days)",
          number_reinfected_28_days_midpoint10 = paste0(
            "Number of reinfections within 28 days"),
          proportion_reinfected_in_28_days_midpoint10_derived = paste0(
            "Proportion of reinfections within 28 days")
        )
      ) %>%
      modify_header(
        label = "**Characteristic**",
        stat_1 = "**Mild**", stat_2 = "**Severe**"
      ) %>%
      modify_footnote_header(
        footnote = "Across full season",
        columns = all_stat_cols(),
        replace = TRUE
      )
    
  }
  
  tbl_sum_all <- tbl_merge(
    tbls = tbl_sum,
    tab_spanner = gsub("_", "-", seasons)
    ) %>%
    as_gt() %>%
    gt::tab_header(title = paste0(pathogen_label, " Infections by Season"))
  
  return(tbl_sum_all)
  
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
