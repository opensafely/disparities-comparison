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
      select(-c(infection_type, subset, codelist_type)) %>%
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
    rename("n" = `n (midpoint 10 rounded)`) %>% 
    filter(!str_detect(combo, "Total")) %>%
    mutate(
      combo = case_when(
        combo == "0_0" ~ "None",
        combo == "0_0_0" ~ "None",
        combo == "0_0_COVID_Mild" ~ "Only COVID-19",
        combo == "0_0_COVID_Severe" ~ "Only COVID-19",
        combo == "0_Flu_Mild" ~ "Only flu",
        combo == "0_Flu_Mild_0" ~ "Only flu",
        combo == "0_Flu_Severe" ~ "Only flu",
        combo == "0_Flu_Severe_0" ~ "Only flu",
        combo == "0_Flu_Mild_COVID_Mild" ~ "Flu and COVID-19",
        combo == "0_Flu_Severe_COVID_Severe" ~ "Flu and COVID-19",
        combo == "RSV_Mild_0" ~ "Only RSV",
        combo == "RSV_Mild_0_0" ~ "Only RSV",
        combo == "RSV_Severe_0" ~ "Only RSV",
        combo == "RSV_Severe_0_0" ~ "Only RSV",
        combo == "RSV_Mild_0_COVID_Mild" ~ "RSV and COVID-19",
        combo == "RSV_Severe_0_COVID_Severe" ~ "RSV and COVID-19",
        combo == "RSV_Mild_Flu_Mild" ~ "RSV and flu",
        combo == "RSV_Mild_Flu_Mild_0" ~ "RSV and flu",
        combo == "RSV_Severe_Flu_Severe" ~ "RSV and flu",
        combo == "RSV_Severe_Flu_Severe_0" ~ "RSV and flu",
        combo == "RSV_Mild_Flu_Mild_COVID_Mild" ~ "RSV, flu, and COVID-19",
        combo == "RSV_Severe_Flu_Severe_COVID_Severe" ~ "RSV, flu, and COVID-19"
      )
    ) %>%
    filter(outcome_type == !!severity) %>%
    select(-c(outcome_type)) %>%
    mutate(
      characteristic = case_when(
        characteristic == "1 (least deprived)" ~ "5 (least deprived)",
        characteristic == "2" ~ "4",
        characteristic == "4" ~ "2",
        characteristic == "5 (most deprived)" ~ "1 (most deprived)",
        is.na(characteristic) ~ "Unknown",
        TRUE ~ characteristic
      )
    ) %>% 
    mutate(
      group = case_when(
        characteristic %in% c("0-2m", "3-5m", "6-11m", "12-23m",
                              "2-5y", "6-9y", "10-13y", "14-17y",
                              "18-39y", "40-64y",
                              "65-74y", "75-89y", "90y+") ~ "Age",
        characteristic %in% c("Male", "Female") ~ "Sex",
        characteristic %in% c("White", "Mixed", "Asian or Asian British",
                              "Black or Black British", "Other Ethnic Groups",
                              "Unknown") ~ "Ethnicity",
        characteristic %in% c("5 (least deprived)", "4", "3", "2",
                              "1 (most deprived)") ~ "IMD",
        characteristic %in% c("Rural Town and Fringe", "Rural Village and Dispersed",
                              "Urban City and Town", "Urban Major Conurbation",
                              "Urban Minor Conurbation") ~ "Rurality",
        TRUE ~ "Other"
      )
    ) %>% 
    group_by(subset, group, combo) %>% 
    mutate(
      prop = n/sum(n)
    )
    # pivot_wider(id_cols = c(characteristic, subset), names_from = combo,
    #             values_from = `n (midpoint 10 rounded)`) %>%
    # tbl_summary(
    #   by = combo,
    #   type = list(value = "continuous"),
    #   statistic = list(all_continuous() ~ "{mean} ({sd})")
    #   )
  
  return(tbl_sum)  
  
}
