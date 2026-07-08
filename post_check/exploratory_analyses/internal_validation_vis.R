library(here)
library(tidyverse)
library(ggplot2)
library(ggsankey)
library(cowplot)

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

NODE_LEVELS <- c(
  "no_mild", "rsv", "flu", "covid",
  "rsv_flu", "rsv_covid", "flu_covid", "rsv_flu_covid",
  "broad", "bucket", "other"
)

PHENOTYPE_LABELS <- c(
  no_mild = "Mild Outcome Not Detected",
  rsv = "Mild RSV",
  flu = "Mild Influenza",
  covid = "Mild COVID-19",
  rsv_flu = "Mild RSV and Influenza",
  rsv_covid = "Mild RSV and COVID-19",
  flu_covid = "Mild Influenza and COVID-19",
  rsv_flu_covid = "Mild RSV, Influenza, and COVID-19",
  bucket = "Mild Overall Respiratory Virus",
  broad = "Mild Respiratory Attendance"
)

OUTCOME_ORDER <- names(PHENOTYPE_LABELS)
STACK_ORDER <- rev(OUTCOME_ORDER)

import_validation_counts <- function(cohort) {

  readr::read_csv(here::here(
    "output", "collated", "descriptive",
    paste0(cohort, "_validation_counts_collated.csv")
  ))

}

import_validation_pops <- function(cohort) {

  readr::read_csv(here::here(
    "output", "collated", "descriptive",
    paste0(cohort, "_validation_pops_collated.csv")
  ))

}

season_has_covid <- function(season) {

  as.integer(substr(season, 1, 4)) >= 2019L

}

secondary_hospitalisation_total <- function(df_pops, season) {

  pop_rows <- df_pops %>%
    filter(
      subset == .env$season,
      denominator %in% c("total_patients_rsv", "total_patients_flu", "total_patients_covid")
    )

  pop_nums <- pop_rows %>% 
    select(c(population, outcome = denominator, denom = denominator_n, rounded = count, pct, subset)) %>% 
    mutate(
      population = gsub("_pop$", "", population),
      phenotype = "sens_stage"
    )
  
  return(pop_nums)
  
}

prep_flow_counts <- function(df_counts, df_pops, population, season) {

  total_patients_sec <- secondary_hospitalisation_total(df_pops, season)
  df_counts <- df_counts %>% 
    filter(subset == .env$season)

  flow_counts_list <- list()

  for (pathogen in c("rsv", "flu", "covid")) {
    
    if (!season_has_covid(season)) {
      next 
    }

    df_pops_filt <- total_patients_sec %>% 
      filter(
        population == pathogen,
        outcome == paste0("total_patients_", pathogen)
      )

    df_counts_filt <- df_counts %>% 
      mutate(
        population = gsub("_pop$", "", population)
      ) %>%
      filter(population == pathogen) %>% 
      pivot_longer(
        cols = ends_with("_stage"),
        names_to = "phenotype",
        values_to = "outcome"
      ) %>% 
      select(c(population, outcome, denom, rounded, pct, subset, phenotype)) %>% 
      mutate(
        denom = df_pops_filt$denom,
        pct = 100* rounded/denom
      )

    flow_counts <- bind_rows(
      df_pops_filt, df_counts_filt
    ) %>% 
      mutate(
        outcome = if_else(str_detect(outcome, "total"), "mild", outcome)
      )

    flow_counts <-  bind_rows(
        tibble(
          population = pathogen,
          outcome = "no_mild",
          denom = df_pops_filt$denom,
          rounded = df_pops_filt$denom - df_pops_filt$rounded,
          pct = (df_pops_filt$denom - df_pops_filt$rounded) / df_pops_filt$denom * 100,
          subset = season,
          phenotype = "spec_stage"
        ),
        tibble(
          population = pathogen,
          outcome = "no_mild",
          denom = df_pops_filt$denom,
          rounded = df_pops_filt$denom - df_pops_filt$rounded,
          pct = (df_pops_filt$denom - df_pops_filt$rounded) / df_pops_filt$denom * 100,
          subset = season,
          phenotype = "sens_stage"
        ),
        tibble(
          population = pathogen,
          outcome = "mild",
          denom = df_pops_filt$denom,
          rounded = df_pops_filt$rounded,
          pct = df_pops_filt$pct,
          subset = season,
          phenotype = "spec_stage"
        ),
        flow_counts
      )

    # return pathogen specific counts to bind out of loop
    flow_counts_list[[pathogen]] <- flow_counts

  }

  # bind pathogen specific counts together
  flow_counts_all <- bind_rows(flow_counts_list)

  return(flow_counts_all)

}

source_label_for_outcome <- function(outcome) {
  paste0(outcome, " Hospitalisation")
}

flow_base_theme <- function(plot_margin = margin(12, -2, -1, 5)) {
  theme_bw() +
    theme(
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text = element_blank(),
      panel.grid = element_blank(),
      strip.background = element_blank(),
      legend.position = "none",
      plot.margin = plot_margin
    )
}

prep_sankey_ggsankey_long <- function(df, label_with_pct = FALSE, use_pct = TRUE) {

  edges <- df %>%
    filter(outcome != "mild", rounded > 0) %>%
    filter(outcome %in% OUTCOME_ORDER) %>%
    mutate(
      outcome_code = factor(outcome, levels = OUTCOME_ORDER),
      phenotype = factor(
        phenotype,
        levels = c("spec_stage", "sens_stage"),
        labels = c("Specific", "Sensitive")
      ),
      population = factor(
        .data$population,
        levels = c("rsv", "flu", "covid"),
        labels = c("RSV", "Influenza", "COVID-19")
      ),
      source = source_label_for_outcome(population),
      target = unname(PHENOTYPE_LABELS[as.character(outcome_code)]),
      value = if (isTRUE(use_pct)) pct else rounded
    ) %>%
    group_by(population, phenotype) %>%
    arrange(desc(pct), outcome_code, .by_group = TRUE) %>%
    ungroup()

  if (isTRUE(label_with_pct)) {
    edges <- edges %>%
      mutate(target = sprintf("%s\n(%.1f%%)", target, pct))
  }

  # ggsankey stacks bottom-up; use reversed order so the visual top-to-bottom
  # matches PHENOTYPE_LABELS.
  edges <- edges %>%
    group_by(population, phenotype) %>%
    arrange(match(as.character(outcome_code), STACK_ORDER), .by_group = TRUE) %>%
    ungroup()

  # ggsankey colours each flow by its *source* node. To colour flows by the
  # right-hand outcome while keeping a single hospitalisation box, we reverse
  # the direction: outcomes are the source nodes (left in data, drawn on the
  # right) and the hospitalisation denominator is the single target node.
  x_levels <- c("Hospitalisation", "Outcome")

  flow_rows <- edges %>%
    transmute(
      population,
      phenotype,
      x = factor("Outcome", levels = x_levels),
      next_x = factor("Hospitalisation", levels = x_levels),
      node = target,
      next_node = source,
      value,
      outcome_code = as.character(outcome_code),
      fillcode = as.character(outcome_code)
    )

  node_rows <- edges %>%
    group_by(population, phenotype, source) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    transmute(
      population,
      phenotype,
      x = factor("Hospitalisation", levels = x_levels),
      next_x = factor(NA_character_, levels = x_levels),
      node = source,
      next_node = NA_character_,
      value = if (isTRUE(use_pct)) 100 else value,
      outcome_code = NA_character_,
      fillcode = "hosp"
    )

  long_df <- bind_rows(flow_rows, node_rows)

  # Explicitly order rows so ggsankey stacks nodes in the desired order.
  long_df <- long_df %>%
    arrange(
      population,
      phenotype,
      x,
      match(outcome_code, STACK_ORDER)
    )

  # ggsankey stacks using factor level order after group_by; per-panel levels
  # get merged into an inconsistent global level set when faceting. Use one
  # shared level order for all panels instead.
  stack_labels <- unname(PHENOTYPE_LABELS[STACK_ORDER])
  hosp_labels <- long_df %>%
    filter(x == "Hospitalisation") %>%
    distinct(node) %>%
    pull(node) %>%
    as.character()
  all_node_levels <- c(hosp_labels, stack_labels)

  long_df <- long_df %>%
    mutate(
      node = factor(as.character(node), levels = all_node_levels),
      next_node = factor(as.character(next_node), levels = all_node_levels)
    )

  long_df
}

plot_sankey_ggsankey <- function(
  df,
  space = 4,
  smooth = 8,
  width = 0.4,
  label_with_pct = FALSE,
  use_pct = TRUE
) {

  sankey_long <- prep_sankey_ggsankey_long(
    df,
    label_with_pct = label_with_pct,
    use_pct = use_pct
  )
  flow_palette <- c(
    setNames(scales::hue_pal()(length(OUTCOME_ORDER)), OUTCOME_ORDER),
    hosp = "white"
  )

  # In ggsankey, `space` is in y-axis units (percent if `use_pct = TRUE`,
  # otherwise counts), not a proportion.
  # If a small fractional value is supplied, treat it as a proportion of the
  # panel total as a convenience.
  if (is.numeric(space) && length(space) == 1 && space > 0 && space < 1) {
    panel_total <- sankey_long %>%
      filter(x == "Hospitalisation") %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    space <- max(0.1, space * panel_total)
  }

  plot <- ggplot(
    sankey_long,
    aes(
      x = x,
      next_x = next_x,
      node = node,
      next_node = next_node,
      value = value,
      label = if_else(x == "Hospitalisation", as.character(node), NA_character_),
      fill = factor(fillcode, levels = c(OUTCOME_ORDER, "hosp")),
      colour = if_else(x == "Hospitalisation", "grey30", NA_character_)
    )
  ) +
    ggsankey::geom_sankey(
      space = space,
      smooth = smooth,
      width = width,
      flow.alpha = 0.85,
      flow.colour = NA,
      node.colour = NA,
      # Right-hand nodes: no fill (transparent). We'll draw the left node separately.
      node.fill = NA,
      node.alpha = 1
    ) +
    # Left hospitalisation node: opaque white with outline; not in legend.
    ggplot2::geom_rect(
      data = sankey_long %>%
        dplyr::filter(x == "Hospitalisation") %>%
        dplyr::distinct(population, phenotype, x, value) %>%
        dplyr::mutate(
          n_x = as.numeric(x),
          xmin = n_x - width / 2,
          xmax = n_x + width / 2,
          ymin = -value / 2,
          ymax = value / 2
        ),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "white",
      colour = "grey30",
      linewidth = 0.5,
      alpha = 1
    ) +
    ggsankey::geom_sankey_text(
      data = dplyr::filter(sankey_long, x == "Hospitalisation"),
      size = 3.2,
      colour = "black",
      lineheight = 0.9,
      na.rm = TRUE
    ) +
    facet_grid(population ~ phenotype) +
    scale_fill_manual(
      values = flow_palette,
      breaks = OUTCOME_ORDER,
      labels = unname(PHENOTYPE_LABELS[OUTCOME_ORDER]),
      name = NULL,
      guide = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
    ) +
    scale_colour_identity(guide = "none") +
    scale_x_discrete(expand = ggplot2::expansion(add = 0.15)) +
    scale_y_continuous(expand = ggplot2::expansion(mult = 0.002)) +
    labs(
      x = NULL,
      y = if (isTRUE(use_pct)) "% of Hospitalisations" else "Hospitalisations (count)"
    ) +
    coord_cartesian(clip = "off") +
    flow_base_theme(plot_margin = margin(2, 4, 1, 14)) +
    theme(
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      panel.spacing.x = unit(2, "pt"),
      panel.spacing.y = unit(3, "pt"),
      strip.text.x = element_text(size = 12, face = "bold"),
      strip.text.y = element_blank()
    )

  plot
}

plot_sankey_between_legend <- function(
  df,
  space = 4,
  smooth = 8,
  width = 0.4,
  label_with_pct = FALSE,
  use_pct = TRUE,
  legend_rel_width = 0.45
) {
  df_spec <- df %>% filter(phenotype == "spec_stage")
  df_sens <- df %>% filter(phenotype == "sens_stage")

  left <- plot_sankey_ggsankey(
    df_spec,
    space = space,
    smooth = smooth,
    width = width,
    label_with_pct = label_with_pct,
    use_pct = use_pct
  ) +
    ggtitle("Specific") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      strip.text.x = element_blank(),
      strip.background = element_blank()
    )

  right <- plot_sankey_ggsankey(
    df_sens,
    space = space,
    smooth = smooth,
    width = width,
    label_with_pct = label_with_pct,
    use_pct = use_pct
  ) +
    ggtitle("Sensitive") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      strip.text.x = element_blank(),
      strip.background = element_blank()
    )

  # Extract a single vertical legend.
  legend_plot <- plot_sankey_ggsankey(
    df_spec,
    space = space,
    smooth = smooth,
    width = width,
    label_with_pct = label_with_pct,
    use_pct = use_pct
  ) +
    guides(fill = guide_legend(ncol = 1)) +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box = "vertical",
      strip.text.x = element_blank(),
      strip.background = element_blank()
    )

  legend <- cowplot::get_legend(legend_plot)

  cowplot::plot_grid(
    left,
    legend,
    right,
    nrow = 1,
    rel_widths = c(1, legend_rel_width, 1),
    align = "h",
    axis = "tb"
  )
}

cohort <- "older_adults"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 4, legend_rel_width = 0.6)
