library(here)
library(tidyverse)
library(ggplot2)
library(ggsankey)
library(cowplot)

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

NODE_LEVELS <- c(
  "no_mild", "rsv", "flu", "covid",
  "rsv_flu", "rsv_covid", "flu_covid", "rsv_flu_covid",
  "broad", "bucket"
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

# Spec-stage "other" = sensitive mild in window but no specific mild.
# Fold into broad if sens was broad, otherwise into bucket.
fold_spec_other <- function(df) {
  df %>%
    mutate(
      spec_stage = case_when(
        spec_stage != "other" ~ spec_stage,
        sens_stage == "broad" ~ "broad",
        TRUE ~ "bucket"
      )
    )
}

OUTCOME_ORDER <- names(PHENOTYPE_LABELS)
STACK_ORDER <- rev(OUTCOME_ORDER)

# Outcomes excluded from the curly brace (no mild / overall / broad attendance)
BRACE_EXCLUDE <- c("no_mild", "bucket", "broad")

# Stack so brace-eligible (virus-coded) outcomes are contiguous.
# Within the brace block, pathogen-matching outcomes are grouped together.
# Display top-to-bottom: no_mild, bucket, broad, other viruses..., pathogen-specific...
stack_order_for_pathogen <- function(pathogen_code) {
  brace_outcomes <- setdiff(OUTCOME_ORDER, BRACE_EXCLUDE)
  is_match <- str_detect(brace_outcomes, fixed(pathogen_code))
  pathogen_specific <- brace_outcomes[is_match]
  other_virus <- brace_outcomes[!is_match]
  rev(c(BRACE_EXCLUDE, other_virus, pathogen_specific))
}

validation_flow_palette <- function() {
  c(
    setNames(scales::hue_pal()(length(OUTCOME_ORDER)), OUTCOME_ORDER),
    hosp = "white"
  )
}

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
      fold_spec_other() %>%
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
sankey_outcome_label_data <- function(sankey_long, space, width, flow_palette,
                                      stack_order = STACK_ORDER) {
  sankey_long %>%
    filter(x == "Outcome") %>%
    group_by(population, phenotype) %>%
    arrange(match(outcome_code, stack_order), .by_group = TRUE) %>%
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

pathogen_code_for_population <- function(population) {
  recode(
    as.character(population),
    RSV = "rsv",
    Influenza = "flu",
    `COVID-19` = "covid",
    .default = NA_character_
  )
}

pathogen_label_for_population <- function(population) {
  recode(
    as.character(population),
    RSV = "RSV",
    Influenza = "Influenza",
    `COVID-19` = "COVID-19",
    .default = as.character(population)
  )
}

# Cubic Bezier helper for curly-brace paths
bezier_cubic <- function(p1, p2, p3, p4, n = 40) {
  t <- seq(0, 1, length.out = n)
  tibble(
    x = (1 - t)^3 * p1[1] + 3 * (1 - t)^2 * t * p2[1] +
      3 * (1 - t) * t^2 * p3[1] + t^3 * p4[1],
    y = (1 - t)^3 * p1[2] + 3 * (1 - t)^2 * t * p2[2] +
      3 * (1 - t) * t^2 * p3[2] + t^3 * p4[2]
  )
}

# Right-pointing curly brace spanning [ymin, ymax], tip at x + width
make_curly_brace <- function(x, ymin, ymax, width = 0.12, n_per = 40) {
  ymid <- (ymin + ymax) / 2
  w <- width
  bind_rows(
    bezier_cubic(
      c(x, ymax), c(x + w * 0.55, ymax),
      c(x + w * 0.55, ymid + (ymax - ymid) * 0.55),
      c(x + w * 0.55, ymid + (ymax - ymid) * 0.12),
      n_per
    ),
    bezier_cubic(
      c(x + w * 0.55, ymid + (ymax - ymid) * 0.12),
      c(x + w * 0.55, ymid),
      c(x + w, ymid), c(x + w, ymid),
      n_per
    ),
    bezier_cubic(
      c(x + w, ymid), c(x + w, ymid),
      c(x + w * 0.55, ymid),
      c(x + w * 0.55, ymid - (ymid - ymin) * 0.12),
      n_per
    ),
    bezier_cubic(
      c(x + w * 0.55, ymid - (ymid - ymin) * 0.12),
      c(x + w * 0.55, ymid - (ymid - ymin) * 0.55),
      c(x + w * 0.55, ymin), c(x, ymin),
      n_per
    )
  )
}

sankey_pathogen_brace_data <- function(sankey_long, space, width,
                                       stack_order = STACK_ORDER,
                                       brace_gap = 0.5,
                                       brace_width = 0.14,
                                       label_gap = 0.06) {
  outcome_positions <- sankey_long %>%
    filter(x == "Outcome") %>%
    group_by(population, phenotype) %>%
    arrange(match(outcome_code, stack_order), .by_group = TRUE) %>%
    mutate(
      n_x = as.numeric(x),
      freq = value,
      ymax = cumsum(freq) + (row_number() - 1) * space,
      ymin = ymax - freq
    ) %>%
    mutate(
      ymin = ymin - max(ymax) / 2,
      ymax = ymax - max(ymax) / 2,
      pathogen_code = pathogen_code_for_population(population),
      # Brace: all virus-coded mild outcomes (exclude no_mild / bucket / broad)
      in_brace = !outcome_code %in% BRACE_EXCLUDE,
      is_pathogen_specific = !is.na(pathogen_code) &
        str_detect(outcome_code, fixed(pathogen_code))
    ) %>%
    ungroup()

  # Brace spans virus-coded outcomes (excl. no_mild / bucket / broad).
  # Label % = pathogen-matching share of all detected mild outcomes
  # (everything except no_mild).
  brace_meta <- outcome_positions %>%
    group_by(population, phenotype, pathogen_code) %>%
    summarise(
      ymin = min(ymin[in_brace], na.rm = TRUE),
      ymax = max(ymax[in_brace], na.rm = TRUE),
      n_x = first(n_x),
      detected_pct = sum(pct[!outcome_code %in% "no_mild"], na.rm = TRUE),
      pathogen_pct = sum(pct[is_pathogen_specific], na.rm = TRUE),
      has_brace = any(in_brace),
      .groups = "drop"
    ) %>%
    filter(has_brace, is.finite(ymin), is.finite(ymax)) %>%
    mutate(
      pathogen_share = if_else(
        detected_pct > 0,
        100 * pathogen_pct / detected_pct,
        NA_real_
      ),
      pathogen_label = pathogen_label_for_population(population),
      brace_x = n_x + width / 2 + brace_gap,
      y = (ymin + ymax) / 2,
      label_x = brace_x + brace_width + label_gap,
      label = sprintf("%% %s\n%.1f%%", pathogen_label, pathogen_share)
    )

  if (nrow(brace_meta) == 0) {
    return(list(
      braces = tibble(
        population = factor(), phenotype = factor(),
        x = numeric(), y = numeric(), group = character()
      ),
      labels = brace_meta
    ))
  }

  braces <- brace_meta %>%
    mutate(group = paste(population, phenotype, sep = "__")) %>%
    rowwise() %>%
    reframe(
      population,
      phenotype,
      group,
      make_curly_brace(brace_x, ymin, ymax, width = brace_width)
    )

  list(braces = braces, labels = brace_meta)
}

prep_sankey_ggsankey_long <- function(df, label_with_pct = FALSE, use_pct = TRUE,
                                      stack_order = STACK_ORDER) {

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
  # matches the intended display order.
  edges <- edges %>%
    group_by(population, phenotype) %>%
    arrange(match(as.character(outcome_code), stack_order), .by_group = TRUE) %>%
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
        as.character(population),
        "\nHospitalisation\nn = ",
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
      match(outcome_code, stack_order)
    )

  # ggsankey stacks using factor level order after group_by; per-panel levels
  # get merged into an inconsistent global level set when faceting. Use one
  # shared level order for all panels instead.
  stack_labels <- unname(PHENOTYPE_LABELS[stack_order])
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

plot_sankey_one_population <- function(
  df,
  pathogen,
  space = 4,
  smooth = 8,
  width = 0.4,
  hosp_width = 0.85,
  hosp_height_pad = 0,
  label_with_pct = FALSE,
  use_pct = TRUE
) {

  stack_order <- stack_order_for_pathogen(pathogen)

  sankey_long <- prep_sankey_ggsankey_long(
    df,
    label_with_pct = label_with_pct,
    use_pct = use_pct,
    stack_order = stack_order
  )
  flow_palette <- validation_flow_palette()

  space_use <- space
  if (is.numeric(space) && length(space) == 1 && space > 0 && space < 1) {
    panel_total <- sankey_long %>%
      filter(x == "Hospitalisation") %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    space_use <- max(0.1, space * panel_total)
  }

  pathogen_braces <- sankey_pathogen_brace_data(
    sankey_long, space_use, width, stack_order = stack_order
  )

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

  ggplot(
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
      space = space_use,
      smooth = smooth,
      width = width,
      flow.alpha = 0.85,
      flow.colour = NA,
      node.colour = NA,
      node.fill = NA,
      node.alpha = 1
    ) +
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
      space = space_use,
      width = hosp_width,
      size = 4,
      colour = "black",
      lineheight = 0.9,
      na.rm = TRUE
    ) +
    geom_text(
      data = sankey_outcome_label_data(
        sankey_long, space_use, width, flow_palette, stack_order = stack_order
      ),
      aes(
        x = x_pos,
        y = y,
        label = label,
        colour = pct_colour
      ),
      inherit.aes = FALSE,
      size = 3.4,
      hjust = -0.1,
      lineheight = 0.9,
      na.rm = TRUE
    ) +
    geom_path(
      data = pathogen_braces$braces,
      aes(x = x, y = y, group = group),
      inherit.aes = FALSE,
      colour = "grey25",
      linewidth = 0.55,
      lineend = "round",
      na.rm = TRUE
    ) +
    geom_text(
      data = pathogen_braces$labels,
      aes(x = label_x, y = y, label = label),
      inherit.aes = FALSE,
      size = 3.4,
      hjust = 0,
      lineheight = 0.9,
      colour = "grey25",
      na.rm = TRUE
    ) +
    facet_grid(population ~ phenotype, scales = "free_y") +
    scale_fill_manual(
      values = flow_palette,
      breaks = OUTCOME_ORDER,
      labels = unname(PHENOTYPE_LABELS[OUTCOME_ORDER]),
      limits = c(OUTCOME_ORDER, "hosp"),
      drop = FALSE,
      name = NULL,
      guide = "none"
    ) +
    scale_colour_identity(guide = "none") +
    scale_x_discrete(expand = expansion(add = c(0.15, 1.55))) +
    scale_y_continuous(expand = expansion(mult = 0.002)) +
    labs(
      x = NULL,
      y = if (isTRUE(use_pct)) "% of Hospitalisations" else "Hospitalisations (count)"
    ) +
    coord_cartesian(clip = "off") +
    flow_base_theme(plot_margin = margin(2, 78, 12, 14)) +
    theme(
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      legend.position = "none",
      panel.spacing.x = unit(2, "pt"),
      panel.spacing.y = unit(3, "pt"),
      strip.text.x = element_blank(),
      strip.text.y = element_blank()
    )
}

plot_sankey_ggsankey <- function(
  df,
  space = 4,
  smooth = 8,
  width = 0.4,
  # --- Left hospitalisation box (white outline) — edit these to resize ---
  hosp_width = 0.85,      # x-axis units; slightly wider than flow `width` (0.4)
  hosp_height_pad = 0,    # extra height in y-units, split top/bottom (0 = flush)
  # ---------------------------------------------------------------------
  label_with_pct = FALSE,
  use_pct = TRUE,
  facet_gap = 0.12
) {

  # One panel per pathogen so pathogen-specific outcomes can stack contiguously
  # for the curly brace (shared facet factor levels cannot do this).
  pathogens <- c("rsv", "flu", "covid")
  pathogens <- pathogens[pathogens %in% unique(df$population)]

  panels <- lapply(pathogens, function(pathogen) {
    plot_sankey_one_population(
      df = df %>% filter(population == pathogen),
      pathogen = pathogen,
      space = space,
      smooth = smooth,
      width = width,
      hosp_width = hosp_width,
      hosp_height_pad = hosp_height_pad,
      label_with_pct = label_with_pct,
      use_pct = use_pct
    )
  })

  # Insert vertical gaps between pathogen panels (the three "facets")
  if (length(panels) <= 1) {
    return(panels[[1]])
  }

  plotlist <- vector("list", length(panels) * 2 - 1)
  rel_heights <- numeric(length(panels) * 2 - 1)
  for (i in seq_along(panels)) {
    idx <- 2 * i - 1
    plotlist[[idx]] <- panels[[i]]
    rel_heights[idx] <- 1
    if (i < length(panels)) {
      plotlist[[idx + 1]] <- NULL
      rel_heights[idx + 1] <- facet_gap
    }
  }

  plot_grid(
    plotlist = plotlist, ncol = 1,
    rel_heights = rel_heights,
    align = "v", axis = "lr"
  )
}

plot_sankey_between_legend <- function(
  df,
  space = 4,
  smooth = 8,
  width = 0.4,
  # Left hospitalisation box — passed through to plot_sankey_ggsankey
  hosp_width = 1.15,
  hosp_height_pad = 0,
  label_with_pct = FALSE,
  use_pct = TRUE,
  legend_rel_width = 0.45,
  facet_gap = 0.12
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
    use_pct = use_pct,
    facet_gap = facet_gap
  ) +
    ggtitle("Specific") +
    theme(
      plot.title = element_text(vjust = 2.5, face = "bold", size = 16),
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
    use_pct = use_pct,
    facet_gap = facet_gap
  ) +
    ggtitle("Sensitive") +
    theme(
      plot.title = element_text(vjust = 2.5, face = "bold", size = 16),
      legend.position = "none",
      strip.text.x = element_blank(),
      strip.background = element_blank()
    )

  # Build the legend from all possible outcomes, not only those present in
  # this cohort/season, so rare or absent categories still have colour keys.
  legend_plot <- tibble(
    outcome = factor(OUTCOME_ORDER, levels = OUTCOME_ORDER),
    x = 1,
    y = seq_along(OUTCOME_ORDER)
  ) %>%
    ggplot(aes(x = x, y = y, fill = outcome)) +
    geom_tile() +
    scale_fill_manual(
      values = validation_flow_palette()[OUTCOME_ORDER],
      breaks = OUTCOME_ORDER,
      labels = unname(PHENOTYPE_LABELS[OUTCOME_ORDER]),
      limits = OUTCOME_ORDER,
      drop = FALSE,
      name = NULL,
      guide = guide_legend(ncol = 1)
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.text = element_text(size = 11),
      legend.margin = margin(t = 0, r = 24, b = 0, l = 0)
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
      size = 16,
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
plot_sankey_between_legend(flow_counts, space = 8, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

cohort <- "adults"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 8, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

cohort <- "children_and_adolescents"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 8, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

cohort <- "infants"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 8, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

cohort <- "infants_subgroup"
season <- "2023_24"

df_counts <- import_validation_counts(cohort)
df_pops <- import_validation_pops(cohort)

flow_counts <- prep_flow_counts(df_counts, df_pops, cohort, season)
plot_sankey_between_legend(flow_counts, space = 8, legend_rel_width = 0.4)

ggsave(here::here("post_check", "plots", "supplemental",
            paste0(cohort, "_internal_validation_", season, ".png")),
       height = 10, width = 14)

