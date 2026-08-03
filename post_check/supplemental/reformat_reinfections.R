library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(egg)
library(scales)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

phenotype_cols <- c("#1E88E5", "#D81B60")

show_x_axis <- function(cohort, descriptive) {
  (descriptive == "no" & cohort == "infants_subgroup") |
    (descriptive == "yes" & cohort == "adults")
}

# helper: line plot (rate) above bar plot (counts) for one pathogen
pathogen_rate_count_plot <- function(df, pathogen, title, show_x,
                                     y_rate, y_count,
                                     y_limits = c(0, 1),
                                     y_breaks = seq(0, 1, by = 0.25)) {

  df_p <- df %>% filter(infection_type == pathogen)
  has_title <- !(is.null(title) || !nzchar(trimws(title)))

  p_rate <- ggplot(df_p) +
    geom_line(aes(x = subset, y = .data[[y_rate]],
                  group = codelist_type, col = codelist_type)) +
    scale_colour_manual(values = phenotype_cols, name = "Phenotype Used") +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      expand = expansion(mult = c(0.02, 0.12))
    ) +
    labs(title = if (has_title) title else " ", x = "", y = "") +
    theme_bw(base_size = 18) +
    facet_wrap(~outcome_type) +
    theme(legend.position = "none",
          # keep title band for alignment, but minimise vertical footprint
          plot.title = element_text(
            size = 12,
            colour = if (has_title) "black" else NA,
            margin = margin(t = 0, b = 1)
          ),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "black"),
          strip.text = element_blank(),
          plot.margin = margin(t = 0, r = 5, b = 0, l = 5))

  p_rate <- tag_facet(
    p_rate, tag_pool = c("Mild", "Severe"),
    open = "", close = "",
    fontface = 4,
    size = 4.5,
    family = "sans"
  )

  p_n <- ggplot(df_p) +
    geom_col(aes(x = subset, y = .data[[y_count]], fill = codelist_type),
             position = position_dodge(width = 0.7), width = 0.65,
             alpha = 0.85) +
    scale_fill_manual(values = phenotype_cols, name = "Phenotype Used") +
    scale_y_continuous(labels = label_number(scale_cut = cut_si(""))) +
    labs(x = "", y = "") +
    theme_bw(base_size = 14) +
    facet_wrap(~outcome_type, scales = "free_y") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = "black"),
          strip.text = element_blank(),
          axis.title.y = element_text(size = 11),
          # tighter bottom on non-final rows reduces space before the next cohort
          plot.margin = margin(t = 0, r = 5,
                               b = if (show_x) 5 else 0, l = 5))

  if (!show_x) {
    p_n <- p_n +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }

  plot_grid(p_rate, p_n, ncol = 1, rel_heights = c(1, 1.5),
            align = "v", axis = "lr")
}

#create function to plot reinfections over time
reinfections <- function(cohort, descriptive = "no") {

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
    ) %>%
    mutate_if(is.numeric, replace_na, replace = 0)

  show_x <- show_x_axis(cohort, descriptive)
  cohort_title <- str_to_title(gsub("_", " ", cohort))

  plot_grid(
    pathogen_rate_count_plot(
      df_input, "RSV", cohort_title, show_x,
      "proportion_reinfected_midpoint10_derived",
      "number_reinfected_midpoint10",
      y_limits = c(0, 0.2),
      y_breaks = seq(0, 0.2, by = 0.05)
    ),
    pathogen_rate_count_plot(
      df_input, "Influenza", " ", show_x,
      "proportion_reinfected_midpoint10_derived",
      "number_reinfected_midpoint10",
      y_limits = c(0, 0.2),
      y_breaks = seq(0, 0.2, by = 0.05)
    ),
    pathogen_rate_count_plot(
      df_input, "COVID-19", " ", show_x,
      "proportion_reinfected_midpoint10_derived",
      "number_reinfected_midpoint10",
      y_limits = c(0, 0.2),
      y_breaks = seq(0, 0.2, by = 0.05)
    ),
    ncol = 3
  )
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
    scale_colour_manual(values = phenotype_cols,
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
  NULL,
  legend,
  nrow = 8,
  rel_heights = c(0.12, 1, 1, 1, 1, 1.35, -0.055, 0.1)
) #%>%
  # annotate_figure(
  #   #bottom = text_grob("Season", vjust = -3, hjust = -0.05, size = 14),
  #   left = text_grob("Proportion Reinfected (lines) / Number Reinfected (bars)",
  #                    rot = 90, size = 12, vjust = 1.75)
  # )

#save
ggsave(here::here("post_check", "plots", "supplemental",
            paste0("reinfections", ".png")),
       height = 18, width = 14)

# now split ages
adults <- reinfections("adults", "yes")
plot_grid(
  label_plot,
  older_adults,
  adults,
  legend,
  nrow = 4,
  rel_heights = c(0.12, 1, 1, 0.1)
) %>%
  annotate_figure(
    bottom = text_grob("Season", vjust = -4, hjust = -0.05, size = 14),
    left = text_grob("Proportion Reinfected (lines) / Number Reinfected (bars)",
                     rot = 90, size = 12, vjust = 1)
  )

#save
ggsave(here::here("post_check", "plots", "supplemental",
            paste0("reinfections_older_pop", ".png")),
       height = 14, width = 18)

plot_grid(
  label_plot,
  children_and_adolescents,
  infants,
  infants_subgroup,
  legend,
  nrow = 5,
  rel_heights = c(0.12, 1, 1, 1, 0.1)
) %>%
  annotate_figure(
    bottom = text_grob("Season", vjust = -3.5, hjust = -0.05, size = 14),
    left = text_grob("Proportion Reinfected (lines) / Number Reinfected (bars)",
                     rot = 90, size = 12, vjust = 1)
  )

#save
ggsave(here::here("post_check", "plots", "supplemental",
            paste0("reinfections_younger_pop", ".png")),
       height = 15, width = 18)

#create function to plot reinfections within 28 days over time
reinfections_28 <- function(cohort, descriptive = "no") {

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
    ) %>%
    mutate_if(is.numeric, replace_na, replace = 0)

  show_x <- show_x_axis(cohort, descriptive)
  cohort_title <- str_to_title(gsub("_", " ", cohort))

  plot_grid(
    pathogen_rate_count_plot(
      df_input, "RSV", cohort_title, show_x,
      "proportion_reinfected_in_28_days_midpoint10_derived",
      "number_reinfected_28_days_midpoint10"
    ),
    pathogen_rate_count_plot(
      df_input, "Influenza", " ", show_x,
      "proportion_reinfected_in_28_days_midpoint10_derived",
      "number_reinfected_28_days_midpoint10"
    ),
    pathogen_rate_count_plot(
      df_input, "COVID-19", " ", show_x,
      "proportion_reinfected_in_28_days_midpoint10_derived",
      "number_reinfected_28_days_midpoint10"
    ),
    ncol = 3
  )
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
    scale_colour_manual(values = phenotype_cols,
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
  NULL,
  legend,
  nrow = 8,
  rel_heights = c(0.12, 1, 1, 1, 1, 1.35, -0.055, 0.1)
) #%>%
  # annotate_figure(
  #   #bottom = text_grob("Season", vjust = -3, hjust = -0.05, size = 14),
  #   left = text_grob(
  #     "Proportion of Reinfections Within 28 Days (lines) / Number (bars)",
  #     rot = 90, size = 12, vjust = 1.75
  #   )
  # )

#save
ggsave(here::here("post_check", "plots", "supplemental",
            paste0("reinfections_28_days", ".png")),
       height = 18, width = 14)

#separate ages
adults <- reinfections_28("adults", "yes")
plot_grid(
  label_plot,
  older_adults,
  adults,
  legend,
  nrow = 4,
  rel_heights = c(0.12, 1, 1, 0.1)
) %>%
  annotate_figure(
    bottom = text_grob("Season", vjust = -4, hjust = -0.05, size = 14),
    left = text_grob(
      "Proportion of Reinfections Within 28 Days (lines) / Number (bars)",
      rot = 90, size = 12, vjust = 1
    )
  )

#save
ggsave(here::here("post_check", "plots", "supplemental",
            paste0("reinfections_28_days_older_pop", ".png")),
       height = 14, width = 18)

plot_grid(
  label_plot,
  children_and_adolescents,
  infants,
  infants_subgroup,
  legend,
  nrow = 5,
  rel_heights = c(0.12, 1, 1, 1, 0.1)
) %>%
  annotate_figure(
    bottom = text_grob("Season", vjust = -3.5, hjust = -0.05, size = 14),
    left = text_grob(
      "Proportion of Reinfections Within 28 Days (lines) / Number (bars)",
      rot = 90, size = 12, vjust = 1
    )
  )

#save
ggsave(here::here("post_check", "plots", "supplemental",
            paste0("reinfections_28_days_younger_pop", ".png")),
       height = 15, width = 18)
