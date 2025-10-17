library(tidyverse)
library(here)
library(ggplot2)
library(egg)
library(scales)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

seasons <- c("2017_18", "2018_19", "2020_21", "2023_24")

sizes <- function(cohort) {

  #import collated phenotype sensitivity data
  df_input <- read_csv(here::here("post_check", "output", "collated",
                      "descriptive", paste0(cohort,
                      "_phenotype_sensitivity_collated.csv")))

  df_totals <- df_input %>% 
    filter(str_detect(combo, "Total")) %>% 
    filter(!(str_detect(outcome_type, "overall"))) %>% 
    mutate(
      pathogen = case_when(
        str_detect(combo, "RSV") ~ "RSV",
        str_detect(combo, "Flu") ~ "Influenza",
        str_detect(combo, "COVID") ~ "COVID-19"
      )
    ) %>% 
    mutate(
      pathogen = factor(pathogen, levels = c("RSV", "Influenza", "COVID-19")),
      codelist_type = factor(str_to_title(codelist_type), levels = c("Specific", "Sensitive"))
    ) %>% 
    rename("count" = `n (midpoint 10 rounded)`)

  f <- function(pal) brewer.pal(3, pal)
  cols <- f("Set2")

  plot_totals <- function(df, severity, pathogen) {

    lims <- case_when(
      pathogen == "RSV" & severity == "mild" & cohort == "older_adults" ~ 250000,
      pathogen == "Influenza" & severity == "mild" & cohort == "older_adults" ~ 10000,
      pathogen == "COVID-19" & severity == "mild" & cohort == "older_adults" ~ 300000,
      pathogen == "RSV" & severity == "severe" & cohort == "older_adults" ~ 30000,
      pathogen == "Influenza" & severity == "severe" & cohort == "older_adults" ~ 10000,
      pathogen == "COVID-19" & severity == "severe" & cohort == "older_adults" ~ 45000,
      pathogen == "RSV" & severity == "mild" & cohort == "infants" ~ 250000,
      pathogen == "Influenza" & severity == "mild" & cohort == "infants" ~ 100000,
      pathogen == "COVID-19" & severity == "mild" & cohort == "infants" ~ 200000,
      pathogen == "RSV" & severity == "severe" & cohort == "infants" ~ 250000,
      pathogen == "Influenza" & severity == "severe" & cohort == "infants" ~ 250000,
      pathogen == "COVID-19" & severity == "severe" & cohort == "infants" ~ 10000
    )

    plot <- df_totals %>%
      filter(outcome_type == !!severity, pathogen == !!pathogen,
            subset %in% seasons) %>%
      ggplot() + geom_bar(aes(y = count, x = codelist_type, fill = pathogen, alpha = codelist_type),
                        stat = "identity", position = "dodge") +
      geom_text(aes(codelist_type, count, label = if_else(count == 0, NA, count)),
                    vjust=-.5, color="black", size = 5) +
      scale_fill_manual(values = c(
        "RSV" = cols[1], "Influenza" = cols[2],
        "COVID-19" = cols[3])) +
      scale_alpha_manual(values = c("Sensitive" = 0.5, "Specific" = 1),
                        na.translate = FALSE,
                        guide = guide_legend(position = NULL)) +
      facet_wrap(~subset, scales = "fixed", nrow = 1) +
    scale_y_continuous(limits = c(0, lims),
                       labels = scales::label_number(scale_cut = cut_si(""))) + 
      labs(
        x = "",
        y = "",
        fill = "Virus"
      ) + theme_bw(base_size = 18) +
      theme(legend.position = "none",
            plot.margin = margin(t = 10, unit = "pt"),
            panel.border = element_blank(),
            axis.line = element_line(color = 'black')) + ## pad "t"op region of the plot
      coord_cartesian(clip = "off")
    
    if (pathogen == "COVID-18") {
      my_tag <- c("2020-21", "2023-24")
    } else {
      my_tag <- c("2017-18", "2018-19", "2020-21", "2023-24")
    }

    plot <- tag_facet(plot,
                      x = 0.075, y = Inf,
                      hjust = -0.2, vjust = 0.4,
                      open = "", close = "",
                      fontface = 4,
                      size = 5,
                      family = "serif",
                      tag_pool = my_tag)
    
    return(plot)

  }

  mild_rsv <- plot_totals(df_totals, "mild", "RSV")
  mild_flu <- plot_totals(df_totals, "mild", "Influenza")
  mild_covid <- plot_totals(df_totals, "mild", "COVID-19")
  severe_rsv <- plot_totals(df_totals, "severe", "RSV")
  severe_flu <- plot_totals(df_totals, "severe", "Influenza")
  severe_covid <- plot_totals(df_totals, "severe", "COVID-19")

  legend_plot <- get_legend(df_totals %>%
      filter(subset %in% seasons) %>%
      ggplot() + geom_bar(aes(y = count, x = codelist_type, fill = pathogen),
                        stat = "identity", position = "dodge") +
      geom_text(aes(codelist_type, count, label = if_else(count == 0, NA, count)),
                    vjust=-.5, color="black") +
      scale_fill_manual(values = c(
        "RSV" = cols[1], "Influenza" = cols[2],
        "COVID-19" = cols[3])) +
      facet_wrap(~subset, scales = "fixed", nrow = 1) + 
      labs(
        x = "",
        y = "",
        fill = "Virus"
      ) + theme_bw(base_size = 18))

  bottom_row_mild <- plot_grid(
    legend_plot,
    mild_covid,
    ncol = 2,
    rel_widths = c(0.48, 0.52)
  )

  mild <- plot_grid(
    mild_rsv,
    mild_flu,
    bottom_row_mild,
    nrow = 3
  )

  bottom_row_severe <- plot_grid(
    legend_plot,
    severe_covid,
    ncol = 2,
    rel_widths = c(0.48, 0.52)
  )

  severe <- plot_grid(
    severe_rsv,
    severe_flu,
    bottom_row_severe,
    nrow = 3
  )

  plot_grid(
    mild,
    severe,
    nrow = 2
  ) %>%
  annotate_figure(
    left = text_grob("Number of Outcomes Identified",
    rot = 90, vjust = 1.5, size = 18)
  )

}

##  older adults 
cohort <- "older_adults"
sizes(cohort)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "size_scales_overtime_older_adults.png"),
       width = 14, height = 18)

##  infants
cohort <- "infants"
sizes(cohort)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "size_scales_overtime_infants.png"),
       width = 14, height = 18)