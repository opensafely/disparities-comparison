library(tidyverse)
library(here)
library(lubridate)
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(ggpmisc)
library(egg)
library(scales)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
options(scipen = 999)

#create function to import data
import <- function(pathogen, cohort) {
  
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  df <- read_csv(here::here(
    "post_check", "output", "collated", "descriptive", "over_time",
    paste0(cohort, "_", "counts_over_time_all_monthly_", pathogen, ".csv"))) %>%
    mutate(virus = pathogen_title) %>%
    arrange(month) %>%
    mutate(
      event = case_when(
        str_detect(event, "primary") ~ "Mild",
        str_detect(event, "secondary") ~ "Severe"
      )
    ) %>%
    select(c(month, event,
             total_events = n,
             codelist_type, virus)) %>%
    filter(month >= "2016-09-01") %>%
    tidyr::complete(
      month = seq(ymd("2016-09-01"), ymd("2024-08-31"), by = "1 month"),
      event,
      codelist_type,
      virus,
      fill = list(total_events = 0)
    ) %>% 
    mutate(
      subset = case_when(
        month >= ymd("2016-09-01") & month <= ymd("2017-09-01") ~ "2016-17",
        month >= ymd("2017-09-01") & month <= ymd("2018-09-01") ~ "2017-18",
        month >= ymd("2018-09-01") & month <= ymd("2019-09-01") ~ "2018-19",
        month >= ymd("2019-09-01") & month <= ymd("2020-09-01") ~ "2019-20",
        month >= ymd("2020-09-01") & month <= ymd("2021-09-01") ~ "2020-21",
        month >= ymd("2021-09-01") & month <= ymd("2022-09-01") ~ "2021-22",
        month >= ymd("2022-09-01") & month <= ymd("2023-09-01") ~ "2022-23",
        month >= ymd("2023-09-01") & month <= ymd("2024-09-01") ~ "2023-24",
      )
    )
  
  return(df)
  
}

#import data
df_rsv_older_adults <- import("rsv", "older_adults")
df_rsv_adults <- import("rsv", "adults")
df_rsv_children_and_adolescents <- import("rsv", "children_and_adolescents")
df_rsv_infants <- import("rsv", "infants")
df_rsv <- bind_rows(
  df_rsv_older_adults,
  df_rsv_adults,
  df_rsv_children_and_adolescents,
  df_rsv_infants
) %>%
  group_by(across(c(month, event, codelist_type, virus))) %>% 
  mutate(total_events = sum(total_events)) %>% 
  unique() %>% 
  #calculate proportion of burden attributable to each cohort
  bind_cols(
    events_older_adults = df_rsv_older_adults$total_events,
    events_adults = df_rsv_adults$total_events,
    events_children_and_adolescents = df_rsv_children_and_adolescents$total_events,
    events_infants = df_rsv_infants$total_events
  ) %>% 
  select(
    month, event, events_older_adults, events_adults, events_children_and_adolescents,
    events_infants, total_events, codelist_type, virus, subset
  )

#import data
df_flu_older_adults <- import("flu", "older_adults")
df_flu_adults <- import("flu", "adults")
df_flu_children_and_adolescents <- import("flu", "children_and_adolescents")
df_flu_infants <- import("flu", "infants")
df_flu <- bind_rows(
  df_flu_older_adults,
  df_flu_adults,
  df_flu_children_and_adolescents,
  df_flu_infants
) %>%
  group_by(across(c(month, event, codelist_type, virus))) %>% 
  mutate(total_events = sum(total_events)) %>% 
  unique() %>% 
  #calculate proportion of burden attributable to each cohort
  bind_cols(
    events_older_adults = df_flu_older_adults$total_events,
    events_adults = df_flu_adults$total_events,
    events_children_and_adolescents = df_flu_children_and_adolescents$total_events,
    events_infants = df_flu_infants$total_events
  ) %>% 
  select(
    month, event, events_older_adults, events_adults, events_children_and_adolescents,
    events_infants, total_events, codelist_type, virus, subset
  )

#import data
df_covid_older_adults <- import("covid", "older_adults")
df_covid_adults <- import("covid", "adults")
df_covid_children_and_adolescents <- import("covid", "children_and_adolescents")
df_covid_infants <- import("covid", "infants")
df_covid <- bind_rows(
  df_covid_older_adults,
  df_covid_adults,
  df_covid_children_and_adolescents,
  df_covid_infants
) %>%
  group_by(across(c(month, event, codelist_type, virus))) %>% 
  mutate(total_events = sum(total_events)) %>% 
  unique() %>% 
  #calculate proportion of burden attributable to each cohort
  bind_cols(
    events_older_adults = df_covid_older_adults$total_events,
    events_adults = df_covid_adults$total_events,
    events_children_and_adolescents = df_covid_children_and_adolescents$total_events,
    events_infants = df_covid_infants$total_events
  ) %>% 
  select(
    month, event, events_older_adults, events_adults, events_children_and_adolescents,
    events_infants, total_events, codelist_type, virus, subset
  )

#combine all viruses
df_all <- bind_rows(
  df_rsv,
  df_flu,
  df_covid
) %>% 
  pivot_longer(
    cols = c(events_older_adults, events_adults, events_children_and_adolescents, events_infants),
    names_to = "cohort",
    values_to = "cohort_events",
    names_prefix = "events_"
  ) %>%
  group_by(across(c(subset, event, codelist_type, virus, cohort))) %>% 
  mutate(
    total_events = sum(total_events),
    cohort_events = sum(cohort_events)
  ) %>% 
  select(-month) %>% 
  unique() %>% 
  mutate(
    perc_burden = cohort_events/total_events
  ) %>% 
  select(
    subset, event, virus, total_events, cohort, cohort_events, perc_burden,
    codelist_type
  )

#define colour palette
f <- function(pal) brewer.pal(3, pal)
cols <- f("Set2")

#plot
plot_burden <- function(df, virus) {

  plot <- df %>%
    filter(virus == !!virus) %>%
    mutate(
      codelist_type = factor(str_to_title(codelist_type), levels = c("Specific", "Sensitive")),
      cohort = str_to_title(gsub("_", " ", cohort))
    ) %>% 
    ggplot(aes(x = subset, y = perc_burden, col = cohort, alpha = codelist_type,
               group = cohort)) +
    geom_line() + geom_point() + facet_grid(codelist_type~event) +
    theme_bw(base_size = 18) +
    # scale_colour_manual(values = c(
    #   "RSV" = cols[1], "Influenza" = cols[2],
    #   "COVID-19" = cols[3])) +
    scale_alpha_manual(values = c("Sensitive" = 0.5, "Specific" = 1),
                       na.translate = FALSE,
                       guide = guide_legend(position = NULL)) +
    labs(x = "", y = "", col = "Virus", alpha = "Phenotype Used",
         title = virus) +
    scale_y_continuous(limits = c(0, 1)) +
    theme(legend.position = "none",
          plot.margin = margin(t = 10, unit = "pt"),
          panel.border = element_blank(),
          axis.line.y = element_line(color = 'black'),
          axis.line.x = element_line(color = 'black'),
          legend.box = "horizontal",
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          # strip.text.x = element_blank()
        )

  return(plot)

}

rsv <- plot_burden(df_all, "RSV")
flu <- plot_burden(df_all, "Influenza")
covid <- plot_burden(df_all, "COVID-19")
legend <- get_legend(
  df_all %>%
    filter(virus == "RSV") %>% 
    mutate(
      codelist_type = factor(str_to_title(codelist_type), levels = c("Specific", "Sensitive")),
      cohort = str_to_title(gsub("_", " ", cohort))
    ) %>% 
    ggplot(aes(x = subset, y = perc_burden, col = cohort, alpha = codelist_type,
               group = cohort)) +
    geom_line() + geom_point() + facet_grid(cohort~event) + theme_bw() +
    scale_alpha_manual(values = c("Sensitive" = 0.5, "Specific" = 1),
                       na.translate = FALSE,
                       guide = guide_legend(position = NULL)) +
    labs(x = "", y = "", col = "Virus", alpha = "Phenotype Used") +
    scale_y_continuous(limits = c(0, 1)) +
      labs(
        x = "",
        y = "",
        col = "Age Cohort",
        alpha = "Phenotype Used"
      ) + theme_bw(base_size = 16) + theme(legend.box = "horizontal")
  )

viruses <- plot_grid(
  rsv + theme(
    axis.text.x = element_blank(), axis.ticks.x = element_blank()),
  flu,
  covid + theme(
    axis.text.x = element_blank(), axis.ticks.x = element_blank()),
  nrow = 3,
  rel_heights = c(0.85, 1, 0.85)
) %>% 
  annotate_figure(
    bottom = text_grob("Annual Cohort", size = 18, hjust = 0.4, vjust = -0.5)
  )

plot_grid(
  legend,
  viruses,
  nrow = 2,
  rel_heights = c(0.065, 0.935)
)

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "viruses_burden_proportions_overtime.png"),
       width = 14, height = 20)

plot_cases_specific <- function(df) {
  
  df_plot <- df %>%
    filter(str_to_title(codelist_type) == "Specific") %>%
    mutate(
      cohort = str_to_title(gsub("_", " ", cohort)),
      cohort = factor(
        cohort,
        levels = c("Infants",
                   "Children and Adolescents",
                   "Adults",
                   "Older Adults")
      ),
      cohort_events = if_else(cohort_events == 0, NA, cohort_events),
      virus = factor(
        virus,
        levels = c("RSV", "Influenza", "COVID-19")
      )
    )
  
  ggplot(
    df_plot,
    aes(
      x = subset,
      y = cohort_events,      # ✔ your variable name
      fill = virus            # ✔ your virus variable
    )
  ) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    
    facet_wrap(~ cohort, ncol = 1, scales = "free_y") +
    
    scale_fill_manual(
      values = c(
        "RSV"       = cols[1],
        "Influenza" = cols[2],
        "COVID-19"  = cols[3]
      )
    ) +
    
    labs(
      x = "Season",
      y = "N cases (log10)",
      fill = "Virus"
    ) +
    
    scale_y_log10() +
    
    coord_cartesian(ylim = c(10, NA)) +
    
    theme_bw(base_size = 18) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "right"
    )
}

burden_specific <- plot_cases_specific(df_all)

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "viruses_burden_proportions_overtime_specific.png"),
       width = 14, height = 20)

plot_burden_options <- function(df, option = c("1", "2"), older) {
  
  option <- match.arg(option)
  
  if (older == "yes") {

    df_plot <- df %>%
      filter(
        str_to_title(codelist_type) == "Specific" & 
          cohort %in% c("adults", "older_adults")
      ) %>%
      mutate(
        cohort = str_to_title(gsub("_", " ", cohort)),
        cohort = factor(
          cohort,
          levels = c("Adults",
                     "Older Adults")
        ),
        virus = factor(
          virus,
          levels = c("RSV", "Influenza", "COVID-19")
        )
      )
    
  } else {

    df_plot <- df %>%
      filter(
        str_to_title(codelist_type) == "Specific" & 
          cohort %in% c("infants", "children_and_adolescents")
      ) %>%
      mutate(
        cohort = str_to_title(gsub("_", " ", cohort)),
        cohort = factor(
          str_wrap(cohort, 20),
          levels = c("Infants",
                     "Children And\nAdolescents")
        ),
        virus = factor(
          virus,
          levels = c("RSV", "Influenza", "COVID-19")
        )
      )

  }
  
  if (option == "1") {
    # ------------------------------------------------------------
    # OPTION 1
    # % of cases for a given pathogen accounted for by each cohort
    # ------------------------------------------------------------
    
    df_option <- df_plot %>%
      group_by(virus, subset, event) %>%
      mutate(
        denom = sum(cohort_events),
        value = cohort_events / denom
      ) %>%
      ungroup()
    
    p <- ggplot(
      df_option,
      aes(
        x = subset,
        y = value,
        fill = cohort
      )
    ) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = ifelse(cohort %in% c("Infants", "Adults"),
                           ifelse(denom == 0, "", denom), ""), y = 0.8), 
            size = 4, angle = 45
      ) +
      facet_grid(virus ~ event) +
      #scale_fill_manual(values = cols) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "Season",
        y = "% of pathogen cases",
        fill = "Cohort",
        #title = "Option 1: % of pathogen's cases accounted for by each cohort"
      ) +
      theme_bw(base_size = 18) +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key.height=unit(1, "cm")
      )
    
  } else if (option == "2") {
    # ------------------------------------------------------------
    # OPTION 2
    # % of cases in a given cohort accounted for by each pathogen
    # ------------------------------------------------------------
    
    df_option <- df_plot %>%
      group_by(cohort, subset, event) %>%
      mutate(
        denom = sum(cohort_events),
        value = cohort_events / denom
      ) %>%
      ungroup()
    
    p <- ggplot(
      df_option,
      aes(
        x = subset,
        y = value,
        fill = virus
      )
    ) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = ifelse(virus == "Influenza", ifelse(denom == 0, "", denom), ""),
            y = 0.8), size = 4, angle = 45
      ) +
      facet_grid(cohort ~ event) +
      scale_fill_manual(
        values = c(
          "RSV"       = cols[1],
          "Influenza" = cols[2],
          "COVID-19"  = cols[3]
        )
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "Season",
        y = "% of cohort cases",
        fill = "Virus",
        #title = "Option 2: % of cohort's cases accounted for by each pathogen"
      ) +
      theme_bw(base_size = 18) +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key.height=unit(1, "cm")
      )
  }
  
  return(p)

}

p1 <- plot_burden_options(df_all, option = "1", older = "yes")

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "viruses_burden_proportions_overtime_pathogen_cases_older.png"),
       width = 14, height = 20)

p2 <- plot_burden_options(df_all, option = "2", older = "yes")

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "viruses_burden_proportions_overtime_cohort_cases_older.png"),
       width = 14, height = 20)

plot_grid(p1, p2, nrow = 2)

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "viruses_burden_proportions_overtime_older.png"),
       width = 14, height = 20)

p1 <- plot_burden_options(df_all, option = "1", older = "no")

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "viruses_burden_proportions_overtime_pathogen_cases_younger.png"),
       width = 14, height = 20)

p2 <- plot_burden_options(df_all, option = "2", older = "no")

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "viruses_burden_proportions_overtime_cohort_cases_younger.png"),
       width = 14, height = 20)

plot_grid(p1, p2, nrow = 2)

#save
ggsave(here::here("post_check", "plots", "primary_analyses",
                  "viruses_burden_proportions_overtime_younger.png"),
       width = 14, height = 20)

# plot_burden_allviruses <- function(df, ctype) {

#   df %>% 
#     filter(codelist_type == !!ctype) %>%
#     mutate(
#       cohort = factor(str_to_title(gsub("_", " ", cohort)),
#                       levels = c("Older Adults", "Adults", "Children", "Infants")),
#       virus  = factor(virus, levels = c("RSV", "Influenza", "COVID-19"))
#     ) %>% 
#     ggplot(aes(x = subset, y = perc_burden, col = virus, shape = cohort,
#            group = interaction(cohort, virus))) +
#     geom_line() + 
#     geom_point(size = 5) +
#     facet_grid(. ~ event) +     # only Mild vs Severe
#     theme_bw(base_size = 20) +
#     labs(
#       title = paste("Phenotype:", str_to_title(ctype)),
#       x = "Annual Cohort",
#       y = "Proportion of Burden",
#       col = "Virus",
#       shape = "Age Cohort"
#     ) +
#     scale_y_continuous(limits = c(0, 1)) +
#     scale_colour_manual(values = c(
#       "RSV" = cols[1], "Influenza" = cols[2],
#       "COVID-19" = cols[3])) +
#     theme(
#       legend.position = "bottom",
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       strip.background = element_blank(),
#       panel.border = element_blank(),
#       axis.line.y = element_line(color = 'black'),
#       axis.line.x = element_line(color = 'black'),
#       axis.title.x = element_text(vjust = -1)
#     )
# }

# plot_specific  <- plot_burden_allviruses(df_all, "specific")
# #save
# ggsave(here::here("post_check", "plots", "primary_analyses",
#                   "viruses_burden_proportions_overtime_specific.png"),
#        plot_specific, width = 16, height = 14)

# plot_sensitive <- plot_burden_allviruses(df_all, "sensitive")
# #save
# ggsave(here::here("post_check", "plots", "primary_analyses",
#                   "viruses_burden_proportions_overtime_sensitive.png"),
#        plot_sensitive, width = 16, height = 14)

# plot_virus_composition <- function(df, ctype) {
  
#   df %>%
#     filter(codelist_type == !!ctype) %>%
#     mutate(
#       cohort = factor(str_to_title(gsub("_", " ", cohort)),
#                       levels = c("Older Adults", "Adults", "Children", "Infants")),
#       virus  = factor(virus, levels = c("RSV", "Influenza", "COVID-19"))
#     ) %>%
#     group_by(subset, cohort, event, codelist_type) %>%
#     mutate(
#       total_in_cohort = sum(cohort_events),
#       perc_within_cohort = cohort_events / total_in_cohort
#     ) %>%
#     ungroup() %>%
#     ggplot(aes(
#       x = subset,
#       y = perc_within_cohort,
#       fill = virus
#     )) +
#     geom_col(position = "stack") +
#     facet_grid(event ~ cohort) +
#     scale_fill_manual(values = c(
#       "RSV" = cols[1], "Influenza" = cols[2], "COVID-19" = cols[3]
#     )) +
#     scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
#     labs(
#       title = paste("Virus Composition by Age Cohort — Phenotype:", str_to_title(ctype)),
#       x = "Annual Cohort",
#       y = "Percentage of Cases",
#       fill = "Virus"
#     ) +
#     theme_bw(base_size = 18) +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       strip.background = element_blank(),
#       panel.border = element_blank(),
#       axis.line.y = element_line(color = "black"),
#       axis.line.x = element_line(color = "black")
#     )
# }

# plot_comp_specific <- plot_virus_composition(df_all, "specific")
# ggsave(here::here("post_check", "plots", "primary_analyses",
#                   "virus_composition_by_age_specific.png"),
#        plot_comp_specific, width = 18, height = 10)

# plot_comp_sensitive <- plot_virus_composition(df_all, "sensitive")
# ggsave(here::here("post_check", "plots", "primary_analyses",
#                   "virus_composition_by_age_sensitive.png"),
#        plot_comp_sensitive, width = 18, height = 10)

# plot_virus_composition_by_subset <- function(df, ctype) {
  
#   df_plot <- df %>%
#     filter(codelist_type == !!ctype) %>%
#     mutate(
#       cohort = str_to_title(gsub("_", " ", cohort)),
#       cohort = factor(cohort, 
#                       levels = c("Infants", 
#                                  "Children And Adolescents",
#                                  "Adults", 
#                                  "Older Adults")),
#       virus = factor(virus, levels = c("RSV", "Influenza", "COVID-19"))
#     ) %>%
#     group_by(subset, cohort, event) %>%
#     mutate(
#       total_in_cohort = sum(cohort_events),
#       perc_within_cohort = cohort_events / total_in_cohort
#     ) %>%
#     ungroup()
  
#   ggplot(
#     df_plot,
#     aes(
#       x = subset,
#       y = perc_within_cohort,
#       fill = virus,
#       group = cohort
#     )
#   ) +
#     geom_col(
#       position = position_dodge(width = 0.9)
#     ) +
#     facet_wrap(~ event, ncol = 2) +
    
#     scale_fill_manual(values = c(
#       "RSV" = cols[1],
#       "Influenza" = cols[2],
#       "COVID-19" = cols[3]
#     )) +
    
#     scale_y_continuous(
#       labels = scales::percent_format(),
#       limits = c(0, 1)
#     ) +
    
#     labs(
#       title = paste("Virus Composition Within Age Cohorts Across Seasons —", 
#                     str_to_title(ctype)),
#       x = "Season",
#       y = "Percentage of Cases",
#       fill = "Virus"
#     ) +
    
#     theme_bw(base_size = 18) +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       strip.background = element_blank(),
#       panel.border = element_blank(),
#       axis.line.y = element_line(colour = "black"),
#       axis.line.x = element_line(colour = "black")
#     )
# }

# plot_sub_specific <- plot_virus_composition_by_subset(df_all, "specific")
# ggsave(
#   here::here("post_check", "plots", "primary_analyses",
#              "virus_composition_dodgedbars_specific.png"),
#   plot_sub_specific,
#   width = 18, height = 12
# )

# plot_sub_sensitive <- plot_virus_composition_by_subset(df_all, "sensitive")
# ggsave(
#   here::here("post_check", "plots", "primary_analyses",
#              "virus_composition_dodgedbars_sensitive.png"),
#   plot_sub_sensitive,
#   width = 18, height = 12
# )