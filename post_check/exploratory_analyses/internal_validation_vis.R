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

  read_csv(here::here(
    "post_check", "output", "collated", "descriptive",
    paste0(cohort, "_validation_counts_collated.csv")
  ))

}

import_validation_pops <- function(cohort) {

  read_csv(here::here(
    "post_check", "output", "collated", "descriptive",
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
      # Marginal totals per phenotype: source rows are a sens × spec cross-tab
      group_by(population, phenotype, outcome, subset) %>%
      summarise(rounded = sum(rounded, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        denom = df_pops_filt$denom,
        pct = 100 * rounded / denom
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

# Compute outcome % label positions per facet so they match geom_sankey
# stacking (ggsankey's own geom_sankey_text defaults different space/width and
# can reorder across panels).
sankey_outcome_label_data <- function(sankey_long, space, width, flow_palette) {
  sankey_long %>%
    filter(x == "Outcome") %>%
    group_by(population, phenotype) %>%
    arrange(match(outcome_code, STACK_ORDER), .by_group = TRUE) %>%
    mutate(
      n_x = as.numeric(x),
      freq = value,
      ymax = cumsum(freq) + (row_number() - 1) * space,
      ymin = ymax - freq
    ) %>%
    mutate(
      ymin = ymin - max(ymax) / 2,
      ymax = ymax - max(ymax) / 2,
      y = (ymin + ymax) / 2,
      # Place just to the right of the (transparent) outcome node band
      x_pos = n_x + width / 2,
      label = sprintf("%.2f%%", pct),
      pct_colour = unname(flow_palette[fillcode])
    ) %>%
    ungroup() %>%
    select(population, phenotype, x_pos, y, label, pct_colour)
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
      pct,
      node_label = NA_character_,
      outcome_code = as.character(outcome_code),
      fillcode = as.character(outcome_code)
    )

  node_rows <- edges %>%
    group_by(population, phenotype, source) %>%
    summarise(
      value = sum(value, na.rm = TRUE),
      hosp_n = dplyr::first(denom),
      .groups = "drop"
    ) %>%
    transmute(
      population,
      phenotype,
      x = factor("Hospitalisation", levels = x_levels),
      next_x = factor(NA_character_, levels = x_levels),
      node = source,
      next_node = NA_character_,
      # Match the left-node height to the sum of flowing outcomes (not a
      # hardcoded 100), so the white box aligns with where flows attach.
      value,
      pct = NA_real_,
      node_label = paste0(
        source,
        "\nn = ",
        format(hosp_n, big.mark = ",", trim = TRUE, scientific = FALSE)
      ),
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
  # --- Left hospitalisation box (white outline) — edit these to resize ---
  hosp_width = 0.55,      # x-axis units; slightly wider than flow `width` (0.4)
  hosp_height_pad = 0,    # extra height in y-units, split top/bottom (0 = flush)
  # ---------------------------------------------------------------------
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

  # Geometry for the opaque left-node rectangle (see hosp_width / hosp_height_pad)
  hosp_box <- sankey_long %>%
    filter(x == "Hospitalisation") %>%
    distinct(population, phenotype, x, value) %>%
    mutate(
      n_x = as.numeric(x),
      xmin = n_x - hosp_width / 2,
      xmax = n_x + hosp_width / 2,
      ymin = -value / 2 - hosp_height_pad / 2,
      ymax = value / 2 + hosp_height_pad / 2
    )

  plot <- ggplot(
    sankey_long,
    aes(
      x = x,
      next_x = next_x,
      node = node,
      next_node = next_node,
      value = value,
      label = node_label,
      fill = factor(fillcode, levels = c(OUTCOME_ORDER, "hosp")),
      colour = if_else(x == "Hospitalisation", "grey30", NA_character_)
    )
  ) +
    geom_sankey(
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
    # Left hospitalisation node: opaque white with outline; sized by hosp_box.
    geom_rect(
      data = hosp_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "white",
      colour = "grey30",
      linewidth = 0.5,
      alpha = 1
    ) +
    geom_sankey_text(
      data = filter(sankey_long, x == "Hospitalisation"),
      space = space,
      width = hosp_width,
      size = 3.2,
      colour = "black",
      lineheight = 0.9,
      na.rm = TRUE
    ) +
    geom_text(
      data = sankey_outcome_label_data(sankey_long, space, width, flow_palette),
      aes(
        x = x_pos,
        y = y,
        label = label,
        colour = pct_colour
      ),
      inherit.aes = FALSE,
      size = 2.8,
      hjust = -0.1,
      lineheight = 0.9,
      na.rm = TRUE
    ) +
    # Free y-ranges stop one pathogen row with large/redacted percentages from
    # setting the scale for every row. Row heights stay fixed because
    # `space = "free_y"` is not used.
    facet_grid(population ~ phenotype, scales = "free_y") +
    scale_fill_manual(
      values = flow_palette,
      breaks = OUTCOME_ORDER,
      labels = unname(PHENOTYPE_LABELS[OUTCOME_ORDER]),
      limits = c(OUTCOME_ORDER, "hosp"),
      drop = FALSE,
      name = NULL,
      guide = guide_legend(nrow = 1, byrow = TRUE)
    ) +
    scale_colour_identity(guide = "none") +
    scale_x_discrete(expand = expansion(add = c(0.15, 0.55))) +
    scale_y_continuous(expand = expansion(mult = 0.002)) +
    labs(
      x = NULL,
      y = if (isTRUE(use_pct)) "% of Hospitalisations" else "Hospitalisations (count)"
    ) +
    coord_cartesian(clip = "off") +
    flow_base_theme(plot_margin = margin(2, 28, 1, 14)) +
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
  # Left hospitalisation box — passed through to plot_sankey_ggsankey
  hosp_width = 0.55,
  hosp_height_pad = 0,
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
    hosp_width = hosp_width,
    hosp_height_pad = hosp_height_pad,
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
    hosp_width = hosp_width,
    hosp_height_pad = hosp_height_pad,
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

  # Extract a single vertical legend from the full data so outcomes that
  # appear in only one phenotype (e.g. broad = Sensitive-only) still get a
  # colour key, not just a label.
  legend_plot <- plot_sankey_ggsankey(
    df,
    space = space,
    smooth = smooth,
    width = width,
    hosp_width = hosp_width,
    hosp_height_pad = hosp_height_pad,
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

  legend <- get_legend(legend_plot)

  plot_row <- plot_grid(
    left,
    NULL,
    legend,
    right,
    nrow = 1,
    rel_widths = c(1, -0.1, legend_rel_width, 1),
    align = "h",
    axis = "tb"
  )

  cohort_label <- case_when(
    cohort == "older_adults" ~ "Older Adults",
    cohort == "adults" ~ "Adults", 
    cohort == "children_and_adolescents" ~ "Children and Young People",
    cohort == "infants" ~ "Infants",
    cohort == "infants_subgroup" ~ "Maternally Linked Infants"
  )

  title <- ggdraw() + 
    draw_label(
      paste(cohort_label, gsub("_", "-", season)),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )

  plot_grid(
    title, plot_row,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
}

cohort <- "older_adults"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 4, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

cohort <- "adults"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 4, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

cohort <- "children_and_adolescents"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 4, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

cohort <- "infants"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 4, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

cohort <- "infants_subgroup"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 4, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

