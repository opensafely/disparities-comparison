library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(egg)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

#create function to plot reinfections over time
reinfections <- function(cohort) {

  df_input <- bind_rows(
    read_csv(
      here::here("post_check", "output", "collated", "descriptive",
                paste0(cohort, "_reinfections_specific_collated.csv"))),
    read_csv(
      here::here("post_check", "output", "collated", "descriptive",
                paste0(cohort, "_reinfections_sensitive_collated.csv")))
    ) %>% 
    mutate(
      codelist_type = factor(
        str_to_title(codelist_type),
        levels = c("Specific", "Sensitive")
      ),
      outcome_type = factor(
        str_to_title(outcome_type),
        levels = c("Mild", "Severe")
      ),
      infection_type = factor(case_when(
        infection_type == "covid" ~ "COVID-19",
        infection_type == "flu" ~ "Influenza",
        infection_type == "rsv" ~ "RSV",
      ), levels = c("RSV", "Influenza", "COVID-19")),
      subset = gsub("_", "-", subset)
    )

  rsv <- df_input %>% 
    filter(infection_type == "RSV") %>% 
    ggplot() + 
    geom_line(aes(x = subset, y = proportion_reinfected_midpoint10_derived,
                  group = codelist_type, col = codelist_type)) +
    scale_colour_manual(values = c("#1E88E5", "#D81B60"),
                        name = "Phenotype Used") +
    labs(title = str_to_title(gsub("_", " ", cohort)),x = "", y = "") +
    theme_bw(base_size  = 18) +
    facet_wrap(~outcome_type) +
    theme(legend.position = "none",
          title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  if (cohort != "infants_subgroup") {
    rsv <- rsv + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  rsv <- tag_facet(
    rsv, tag_pool = c("Mild", "Severe"),
    open = "", close = "",
    fontface = 4,
    size = 4.5,
    family = "sans"
  )

  flu <- df_input %>% 
    filter(infection_type == "Influenza") %>% 
    ggplot() + 
    geom_line(aes(x = subset, y = proportion_reinfected_midpoint10_derived,
                  group = codelist_type, col = codelist_type)) +
    scale_colour_manual(values = c("#1E88E5", "#D81B60"),
                        name = "Phenotype Used") +
    labs(title = " ", x = "", y = "") + theme_bw(base_size  = 18) +
    facet_wrap(~outcome_type) +
    theme(legend.position = "none",
          title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  if (cohort != "infants_subgroup") {
    flu <- flu + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  flu <- tag_facet(
    flu, tag_pool = c("Mild", "Severe"),
    open = "", close = "",
    fontface = 4,
    size = 4.5,
    family = "sans"
  )

  covid <- df_input %>% 
    filter(infection_type == "COVID-19") %>% 
    ggplot() + 
    geom_line(aes(x = subset, y = proportion_reinfected_midpoint10_derived,
                  group = codelist_type, col = codelist_type)) +
    scale_colour_manual(values = c("#1E88E5", "#D81B60"),
                        name = "Phenotype Used") +
    labs(title = " ", x = "", y = "") + theme_bw(base_size = 18) +
    facet_wrap(~outcome_type) +
    theme(legend.position = "none",
          title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  if (cohort != "infants_subgroup") {
    covid <- covid + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  covid <- tag_facet(
    covid, tag_pool = c("Mild", "Severe"),
    open = "", close = "",
    fontface = 4,
    size = 4.5,
    family = "sans"
  )

  plot <- plot_grid(
    rsv,
    flu,
    covid,
    ncol = 3
  )

  return(plot)
    
}

older_adults <- reinfections("older_adults")
adults <- reinfections("adults")
children_and_adolescents <- reinfections("children_and_adolescents")
infants <- reinfections("infants")
infants_subgroup <- reinfections("infants_subgroup")
leg_df <- tibble(
  x = c("2016-17", "2016-17"),
  y = 1,
  codelist_type = factor(c("Specific", "Sensitive"),
                         levels = c("Specific", "Sensitive"))
)
legend <- get_legend(
  ggplot(leg_df) + 
    geom_line(aes(x, y, group = codelist_type,
                  col = codelist_type)) +
    scale_colour_manual(values = c("#1E88E5", "#D81B60"),
                        name = "Phenotype Used") +
    labs(x = "", y = "") + theme_bw(base_size = 18) +
    guides(col = guide_legend(label.position = "left")),
  position = "bottom"
)

label_plot <- ggplot() +
  annotate("text",
           x = c(0.96, 2.05, 3.165),
           y = 1,
           label = c("RSV", "Influenza", "COVID-19"),
           size = 6
          ) +
  xlim(0.5, 3.5) +
  ylim(0.5, 1.5) +
  theme_void()

plot_grid(
  label_plot,
  older_adults,
  adults,
  children_and_adolescents,
  infants,
  infants_subgroup,
  legend,
  nrow = 7,
  rel_heights = c(0.15, 1, 1, 1, 1, 1.4, 0.1)
) %>% 
  annotate_figure(
    bottom = text_grob("Season", vjust = -3, hjust = -0.05, size = 14),
    left = text_grob("Proportion Reinfected", rot = 90, size = 14, vjust = 1)
  )

#save
ggsave(here::here("post_check", "plots", "supplemental",
            paste0("reinfections", ".png")),
       height = 12, width = 18)

#create function to plot reinfections within 28 days over time
reinfections_28 <- function(cohort) {

  df_input <- bind_rows(
    read_csv(
      here::here("post_check", "output", "collated", "descriptive",
                paste0(cohort, "_reinfections_specific_collated.csv"))),
    read_csv(
      here::here("post_check", "output", "collated", "descriptive",
                paste0(cohort, "_reinfections_sensitive_collated.csv")))
    ) %>% 
    mutate(
      codelist_type = factor(
        str_to_title(codelist_type),
        levels = c("Specific", "Sensitive")
      ),
      outcome_type = factor(
        str_to_title(outcome_type),
        levels = c("Mild", "Severe")
      ),
      infection_type = factor(case_when(
        infection_type == "covid" ~ "COVID-19",
        infection_type == "flu" ~ "Influenza",
        infection_type == "rsv" ~ "RSV",
      ), levels = c("RSV", "Influenza", "COVID-19")),
      subset = gsub("_", "-", subset)
    )

  rsv <- df_input %>% 
    filter(infection_type == "RSV") %>% 
    ggplot() + 
    geom_line(aes(x = subset, y = proportion_reinfected_in_28_days_midpoint10_derived,
                  group = codelist_type, col = codelist_type)) +
    scale_colour_manual(values = c("#1E88E5", "#D81B60"),
                        name = "Phenotype Used") +
    labs(title = str_to_title(gsub("_", " ", cohort)),x = "", y = "") +
    theme_bw(base_size  = 18) +
    facet_wrap(~outcome_type) +
    theme(legend.position = "none",
          title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  if (cohort != "infants_subgroup") {
    rsv <- rsv + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  rsv <- tag_facet(
    rsv, tag_pool = c("Mild", "Severe"),
    open = "", close = "",
    fontface = 4,
    size = 4.5,
    family = "sans"
  )

  flu <- df_input %>% 
    filter(infection_type == "Influenza") %>% 
    ggplot() + 
    geom_line(aes(x = subset, y = proportion_reinfected_midpoint10_derived,
                  group = codelist_type, col = codelist_type)) +
    scale_colour_manual(values = c("#1E88E5", "#D81B60"),
                        name = "Phenotype Used") +
    labs(title = " ",x = "", y = "") +
    theme_bw(base_size  = 18) +
    facet_wrap(~outcome_type) +
    theme(legend.position = "none",
          title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  if (cohort != "infants_subgroup") {
    flu <- flu + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  flu <- tag_facet(
    flu, tag_pool = c("Mild", "Severe"),
    open = "", close = "",
    fontface = 4,
    size = 4.5,
    family = "sans"
  )

  covid <- df_input %>% 
    filter(infection_type == "COVID-19") %>% 
    ggplot() + 
    geom_line(aes(x = subset, y = proportion_reinfected_in_28_days_midpoint10_derived,
                  group = codelist_type, col = codelist_type)) +
    scale_colour_manual(values = c("#1E88E5", "#D81B60"),
                        name = "Phenotype Used") +
    labs(title = " ",x = "", y = "") +
    theme_bw(base_size  = 18) +
    facet_wrap(~outcome_type) +
    theme(legend.position = "none",
          title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
  if (cohort != "infants_subgroup") {
    covid <- covid + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  covid <- tag_facet(
    covid, tag_pool = c("Mild", "Severe"),
    open = "", close = "",
    fontface = 4,
    size = 4.5,
    family = "sans"
  )

  plot <- plot_grid(
    rsv,
    flu,
    covid,
    ncol = 3
  )

  return(plot)
    
}

older_adults <- reinfections_28("older_adults")
adults <- reinfections_28("adults")
children_and_adolescents <- reinfections_28("children_and_adolescents")
infants <- reinfections_28("infants")
infants_subgroup <- reinfections_28("infants_subgroup")
leg_df <- tibble(
  x = c("2016-17", "2016-17"),
  y = 1,
  codelist_type = factor(c("Specific", "Sensitive"),
                         levels = c("Specific", "Sensitive"))
)
legend <- get_legend(
  ggplot(leg_df) + 
    geom_line(aes(x, y, group = codelist_type,
                  col = codelist_type)) +
    scale_colour_manual(values = c("#1E88E5", "#D81B60"),
                        name = "Phenotype Used") +
    labs(x = "", y = "") + theme_bw(base_size  = 18) +
    guides(col = guide_legend(label.position = "left")),
  position = "bottom"
)

label_plot <- ggplot() +
  annotate("text",
           x = c(0.96, 2.05, 3.165),
           y = 1,
           label = c("RSV", "Influenza", "COVID-19"),
           size = 6
          ) +
  xlim(0.5, 3.5) +
  ylim(0.5, 1.5) +
  theme_void()

plot_grid(
  label_plot,
  older_adults,
  adults,
  children_and_adolescents,
  infants,
  infants_subgroup,
  legend,
  nrow = 7,
  rel_heights = c(0.15, 1, 1, 1, 1, 1.4, 0.1)
) %>% 
  annotate_figure(
    bottom = text_grob("Season", vjust = -3, hjust = -0.05, size = 14),
    left = text_grob("Proportion Reinfected in 28 Days", rot = 90, size = 14, vjust = 1)
  )

#save
ggsave(here::here("post_check", "plots", "supplemental",
            paste0("reinfections_28_days", ".png")),
       height = 12, width = 18)
