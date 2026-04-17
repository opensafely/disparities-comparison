library(tidyverse)
library(here)
library(readr)
library(ggplot2)
library(scales)

#write a function to calculate percentage of overlapping outcomes
perc_overlap <- function(cohort) {

  #import collated phenotype sensitivity data
  df_input <- read_csv(
    here::here("post_check", "output", "collated", "descriptive", paste0(cohort,
               "_phenotype_sensitivity_collated.csv"))
  )
  names(df_input) <- c("combo", "n", "outcome_type", "codelist_type", "subset")

  #calculate percentage of outcomes which are overlapping
  df_perc <- df_input %>% 
    filter(
      combo %in% c("RSV_Mild_Total", "RSV_Severe_Total",
                   "Flu_Mild_Total", "Flu_Severe_Total",
                   "COVID_Mild_Total", "COVID_Severe_Total",
                   "RSV_Mild_Flu_Mild", "RSV_Severe_Flu_Severe",
                   "RSV_Mild_Flu_Mild_0", "RSV_Severe_Flu_Severe_0",
                   "RSV_Mild_0_COVID_Mild", "RSV_Severe_0_COVID_Severe",
                   "0_Flu_Mild_COVID_Mild", "0_Flu_Severe_COVID_Severe") &
      outcome_type %in% c("mild", "severe")
  ) %>%
    group_by(outcome_type, codelist_type, subset) %>% 
    mutate(
      total_cases = if_else(
        grepl("_Total", combo),
        sum(n[grepl("_Total", combo)], na.rm = TRUE),
        NA_integer_
      ),
      total_overlapping = if_else(
        grepl("_Total", combo),
        sum(n[!grepl("_Total", combo)], na.rm = TRUE),
        NA_integer_
      )
    ) %>% 
    select(-c(combo, n)) %>% 
    filter(!is.na(total_cases)) %>% 
    mutate(
      perc = signif((total_overlapping/total_cases)*100, digits = 2),
      outcome_type = str_to_title(outcome_type),
      codelist_type = str_to_title(codelist_type),
      subset = gsub("_", "-", subset)
    ) %>% 
    unique()

  #reorder the columns
  df_perc <- df_perc[, c("subset", "outcome_type", "codelist_type",
                         "total_cases", "total_overlapping", "perc")]
  
  #create a plot df
  df_plot <- df_perc %>% 
    mutate(
      non_overlapping = total_cases - total_overlapping
    ) %>% 
    pivot_longer(
      cols = c(total_overlapping, non_overlapping),
      names_to = "type",
      values_to = "value"
    )
  
  names(df_perc) <- c("Season", "Severity", "Phenotype Used", "Total Cases",
                      "Total Overlapping", "Percentage Overlapping")
  
  #save the file
  write_csv(df_perc, here::here("post_check", "supplemental", "outcome_overlap",
                                paste0(cohort, "_outcome_overlap.csv")))

  df_plot <- df_plot %>% 
    mutate(codelist_type = factor(codelist_type, 
                                  levels = c("Specific", "Sensitive")))

  #create a plot
  ggplot(
    df_plot,
    aes(x = subset, y = value, fill = type)
  ) +
    geom_col(position = position_stack(reverse = TRUE)) +  # stacked bars [web:3][web:13]
    geom_text(
      data = df_plot %>% filter(type == "total_overlapping"),
      aes(x = subset, y = total_cases, label = sprintf("%.1f%%", perc)),
      vjust = -0.2, 
      color = "steelblue",
      size = 5
    ) + # label in middle of overlapping segment [web:5][web:10]
    facet_grid(outcome_type ~ codelist_type, scales = "free_y") +  # facet by two variables [web:6][web:9][web:12]
    scale_fill_manual(
      values = c(non_overlapping = "#555555", total_overlapping = "steelblue"),
      name = "Count type",
      labels = c("Total cases", "Total overlapping")
    ) +
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_si(""))) +
    labs(
      x = "Season",
      y = "Number of cases"
    ) +
    theme_bw(base_size = 20) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          strip.background = element_blank(),
          axis.line = element_line(color = 'black')) +
    guides(fill = guide_legend(reverse = TRUE))

  #save the plot
  ggsave(here::here("post_check", "plots", "supplemental",
                    paste0(cohort, "_outcome_overlap.png")),
         height = 12, width = 18)

}

##  older adults 
perc_overlap("older_adults")

##  adults 
perc_overlap("adults")

##  children and adolescents
perc_overlap("children_and_adolescents")

##  infants
perc_overlap("infants")

##  infants subgroup
perc_overlap("infants_subgroup")

#write a function to calculate percentage of bucket-only outcomes
perc_bucket <- function(cohort) {

  #import collated phenotype sensitivity data
  df_input <- read_csv(
    here::here("post_check", "output", "collated", "descriptive", paste0(cohort,
               "_phenotype_sensitivity_collated.csv"))
  )
  names(df_input) <- c("combo", "n", "outcome_type", "codelist_type", "subset")

  #calculate percentage of outcomes which are overlapping
  df_perc <- df_input %>% 
    filter(
      !(combo %in% c("0_0", "0_0_0",
                  "RSV_Mild_Flu_Mild", "RSV_Severe_Flu_Severe",
                  "RSV_Mild_Flu_Mild_0", "RSV_Severe_Flu_Severe_0",
                  "RSV_Mild_0_COVID_Mild", "RSV_Severe_0_COVID_Severe",
                  "0_Flu_Mild_COVID_Mild", "0_Flu_Severe_COVID_Severe")) &
      outcome_type %in% c("mild_overall", "severe_overall")
  ) %>%
    group_by(outcome_type, codelist_type, subset) %>% 
    mutate(
      total_cases = if_else(
        grepl("_Total", combo) & !grepl("Overall_", combo),
        sum(n[grepl("_Total", combo) & !grepl("Overall_", combo)],
            na.rm = TRUE),
        NA_integer_
      ),
      total_bucket = if_else(
        grepl("_Total", combo)& !grepl("Overall_", combo),
        sum(n[grepl("_Total", combo)& grepl("Overall_", combo)], na.rm = TRUE),
        NA_integer_
      )
    ) %>% 
    select(-c(combo, n)) %>% 
    filter(!is.na(total_cases)) %>% 
    mutate(
      perc = signif((total_cases/total_bucket)*100, digits = 2),
      outcome_type = str_to_title(gsub("_overall", "", outcome_type)),
      subset = gsub("_", "-", subset)
    ) %>% 
    unique()

  #reorder the columns
  df_perc <- df_perc[, c("subset", "outcome_type", "total_bucket",
                         "total_cases", "perc")]
  
  #create a plot df
  df_plot <- df_perc %>% 
    mutate(
      bucket_only = total_bucket - total_cases
    ) %>% 
    pivot_longer(
      cols = c(total_cases, bucket_only),
      names_to = "type",
      values_to = "value"
    )
  
  names(df_perc) <- c("Season", "Severity", "Total Bucket",
                      "Total Cases", "Percentage Bucket Only")
  
  #save the file
  write_csv(df_perc, here::here("post_check", "supplemental", "outcome_overlap",
                                paste0(cohort, "_bucket_overlap.csv")))

  cohort_title <- if_else(cohort == "infants_subgroup",
                          "maternally_linked_infants", cohort)
  
  #create a plot
  ggplot(
    df_plot,
    aes(x = subset, y = value, fill = type)
  ) +
    geom_col() +  # stacked bars [web:3][web:13]
    geom_text(
      data = df_plot %>% filter(type == "total_cases"),
      aes(label = sprintf("%.1f%%", perc)),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 5
    ) + # label in middle of overlapping segment [web:5][web:10]
    facet_wrap(~outcome_type, scales = "free") +  # facet by two variables [web:6][web:9][web:12]
    scale_fill_manual(
      values = c(bucket_only = "#bd592c", total_cases = "#555555"),
      name = "Count type",
      labels = c("Overall Respiratory Virus", "RSV/Influenza/COVID-19")
    ) +
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_si(""))) +
    labs(
      x = "Season",
      y = "Number of cases",
      title = str_to_title(gsub("_", " ", cohort_title))
    ) +
    theme_bw(base_size = 20) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          strip.background = element_blank(),
          axis.line = element_line(color = 'black'))

  #save the plot
  ggsave(here::here("post_check", "plots", "supplemental",
                    paste0(cohort, "_bucket_overlap.png")),
         height = 12, width = 18)

}

##  older adults 
perc_bucket("older_adults")

##  adults 
perc_bucket("adults")

##  children and adolescents
perc_bucket("children_and_adolescents")

##  infants
perc_bucket("infants")

##  infants subgroup
perc_bucket("infants_subgroup")
