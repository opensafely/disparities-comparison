library(here)
library(tidyverse)
library(ggplot2)
library(ggalluvial)

##  older adults 
cohort <- "older_adults"

node_levels <- c(
  "rsv", "flu", "covid", "rsv_flu", "rsv_covid",
  "flu_covid", "rsv_flu_covid", "broad", "bucket", "other"
)

#import collated phenotype sensitivity data
df_input <- read_csv(here::here(#"post_check", "output", "collated",
                     "output", "collated",
                     "descriptive", paste0(cohort,
                     "_validation_counts_collated.csv")))

df_pops <- read_csv(here::here(#"post_check", "output", "collated",
                    "output", "collated",
                    "descriptive", paste0(cohort,
                    "_validation_pops_collated.csv")))

sankey_prep <- function(df, pop, season) {

  pop_flag <- gsub("_pop", "", pop)

  df <- df %>% 
    filter(population == pop, subset == season) %>% 
    select(-c(population, pct, subset)) %>% 
    mutate(
      mild_group_sens = if_else(grepl(pop_flag, sens_stage), "mild", "no_mild"),
      mild_group_spec = if_else(grepl(pop_flag, spec_stage), "mild", "no_mild"),
      mild_group_sens = factor(mild_group_sens, levels = c("mild", "no_mild")),
      mild_group_spec = factor(mild_group_spec, levels = c("mild", "no_mild")),
      spec_stage = factor(spec_stage, levels = node_levels),
      sens_stage = factor(sens_stage, levels = node_levels),
      flow_id = row_number()
    )
  
  df_axes <- df %>% select(rounded, mild_group_sens, sens_stage, spec_stage, mild_group_spec, flow_id)

  df_long <- to_lodes_form(
    df_axes,
    key = "stage",
    value = "stratum",
    id = "flow_id",
    axes = c("mild_group_sens", "sens_stage", "spec_stage", "mild_group_spec"),
    discern = TRUE
  ) %>%
    left_join(
      df_axes %>% select(flow_id, sens_stage, spec_stage),
      by = "flow_id"
    ) %>%
    mutate(
      stage = factor(stage, levels = c("mild_group_sens", "sens_stage", "spec_stage", "mild_group_spec")),
      flow_fill = factor(
        case_when(
          stage == "mild_group_sens" ~ as.character(sens_stage),
          stage == "sens_stage" ~ as.character(spec_stage),
          stage == "spec_stage" ~ as.character(spec_stage),
          stage == "mild_group_spec" ~ as.character(spec_stage),
          TRUE ~ NA_character_
        ),
        levels = node_levels
      )
    )
  
  flow_palette <- setNames(
    scales::hue_pal()(length(node_levels)),
    node_levels
  )
  used_levels <- intersect(node_levels, levels(droplevels(df_long$flow_fill)))

  df_long <- df_long %>% 
    mutate(
      stratum_lab = case_when(
        grepl("no_mild", stratum) ~ "Irrelevant Mild Outcome",
        grepl("mild",  stratum)~ "Relevant Mild Outcome",
        grepl("broad", stratum) ~ "Primary Care Respiratory Attendance",
        grepl("bucket", stratum) ~ "Overall Respiratory Virus",
        stratum == "other" ~ "Other",
        stratum == "other.1" ~ "Other",
        stratum == "rsv_flu" ~ "RSV and Influenza",
        stratum == "rsv_covid" ~ "RSV and COVID-19",
        stratum == "flu_covid" ~ "Influenza and COVID-19",
        stratum == "rsv_flu_covid" ~ "RSV, Influenza, and COVID-19",
        grepl("rsv", stratum) ~ "RSV",
        grepl("flu", stratum) ~ "Influenza",
        grepl("covid", stratum) ~ "COVID-19"
      )
    )
  
  pop_title <- case_when(
    pop_flag == "rsv" ~ "RSV",
    pop_flag == "flu" ~ "Influenza",
    pop_flag == "covid" ~ "COVID-19"
  )

  ggplot(
    data = df_long,
    aes(x = stage, stratum = stratum, alluvium = flow_id, y = rounded)
  ) +
    geom_flow(aes(fill = flow_fill)) +
    geom_stratum(fill = "white", colour = "grey30", linewidth = 0.3) +
    geom_text(
      stat = "stratum",
      aes(label = stringr::str_wrap(stratum_lab, 10))
    ) +
    scale_fill_manual(
      values = flow_palette,
      breaks = used_levels,
      name = "Phenotype"
    ) +
    scale_x_discrete(
      labels = c("Severe Sensitive", "Mild Sensitive", "Mild Specific", "Severe Specific")
    ) +
    labs(x = "", y = "", title = paste("Severe", pop_title, "Outcomes with Mild Outcome up to 30 Days Prior")) +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line(color = 'black'),
          legend.position = "none")
  
}

sankey_prep(df_input, "rsv_pop", "2023_24")


