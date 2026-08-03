library(UpSetR)
library(tidyverse)
library(ggplot2)
library(grid)
library(stringr)
library(data.table)
library(RColorBrewer)
library(tibble)
library(scales)
library(forcats)
library(cowplot)

# UpSetR requires integer inputs via fromExpression(), so counts are passed to
# upset() for the matrix layout. The main intersection bar and set-size bar are
# replaced with custom ggplots showing rates per `per_n` person-seasons.
#
# Denominators are pulled from rows in the CSV:
#   pre-COVID seasons  -> combo == "0_0"   (RSV + Flu only)
#   post-COVID seasons -> combo == "0_0_0" (RSV + Flu + COVID)
# matched on codelist_type, outcome_type (mild/severe), and subset (season).


# ---- helpers -----------------------------------------------------------------

post_covid_seasons <- c("2019_20", "2020_21", "2021_22", "2022_23", "2023_24")

safe_pull_n <- function(df, codelist, combo_spec) {
  # codelist_type in CSV is lowercase ("specific"/"sensitive")
  count <- df %>%
    filter(codelist_type == tolower(codelist), combo == !!combo_spec) %>%
    pull(`n (midpoint 10 rounded)`)
  if (length(count) == 0 || is.na(count[1])) return(0L)
  as.integer(round(as.numeric(count[1])))
}

safe_pull_denom <- function(df, codelist, outcome, season) {
  denom_combo <- if (season %in% post_covid_seasons) "0_0_0" else "0_0"
  # codelist_type and outcome_type in CSV are both lowercase
  denom <- df %>%
    filter(
      codelist_type == tolower(codelist),
      outcome_type  == tolower(outcome),
      combo         == denom_combo
    ) %>%
    pull(`n (midpoint 10 rounded)`)
  if (length(denom) == 0 || is.na(denom[1])) return(NA_real_)
  as.numeric(denom[1])
}

to_rate <- function(count, denom, per_n = 100000) {
  if (is.na(denom) || denom == 0) return(NA_real_)
  round(count / denom * per_n, 1)
}

build_combo_df <- function(filtered_input, season, outcome_label,
                           codelist_types, full_combos, per_n) {

  is_post <- season %in% post_covid_seasons

  rows <- lapply(codelist_types, function(cl) {

    oc_lower <- tolower(outcome_label)

    if (is_post) {
      rsv_n        <- safe_pull_n(filtered_input, cl, paste0("RSV_", outcome_label, "_0_0"))
      flu_n        <- safe_pull_n(filtered_input, cl, paste0("0_Flu_", outcome_label, "_0"))
      covid_n      <- safe_pull_n(filtered_input, cl, paste0("0_0_COVID_", outcome_label))
      rsv_flu_n    <- safe_pull_n(filtered_input, cl, paste0("RSV_", outcome_label, "_Flu_", outcome_label, "_0"))
      rsv_covid_n  <- safe_pull_n(filtered_input, cl, paste0("RSV_", outcome_label, "_0_COVID_", outcome_label))
      flu_covid_n  <- safe_pull_n(filtered_input, cl, paste0("0_Flu_", outcome_label, "_COVID_", outcome_label))
      all_n        <- safe_pull_n(filtered_input, cl, paste0("RSV_", outcome_label, "_Flu_", outcome_label, "_COVID_", outcome_label))
    } else {
      rsv_n        <- safe_pull_n(filtered_input, cl, paste0("RSV_", outcome_label, "_0"))
      flu_n        <- safe_pull_n(filtered_input, cl, paste0("0_Flu_", outcome_label))
      covid_n      <- 0L
      rsv_flu_n    <- safe_pull_n(filtered_input, cl, paste0("RSV_", outcome_label, "_Flu_", outcome_label))
      rsv_covid_n  <- 0L
      flu_covid_n  <- 0L
      all_n        <- 0L
    }

    denom <- safe_pull_denom(filtered_input, cl, oc_lower, season)

    tibble(
      combo        = factor(full_combos, levels = full_combos),
      n            = c(rsv_n, flu_n, covid_n, rsv_flu_n,
                       rsv_covid_n, flu_covid_n, all_n),
      rate         = sapply(c(rsv_n, flu_n, covid_n, rsv_flu_n,
                              rsv_covid_n, flu_covid_n, all_n),
                            to_rate, denom = denom, per_n = per_n),
      denom        = denom,
      codelist_type = cl,
      outcome_type  = outcome_label,
      subset        = season
    )
  })

  bind_rows(rows)
}

# Build the custom ggplot intersection bar (rates) to replace uu$Main_bar.
# `input_expr` is the named integer vector passed to upset(); `rate_vec` is the
# corresponding named numeric vector of rates in the same order as full_combos
# (7 elements, COVID combos may be 0/NA for pre-covid).
build_main_bar <- function(rate_vec, combo_order, y_label, cols_vec) {

  df <- tibble(
    combo = factor(names(rate_vec), levels = combo_order),
    rate  = as.numeric(rate_vec)
  ) %>%
    filter(!is.na(rate))

  # degree-ordered x positions (single -> doubles -> triple) matching UpSetR
  # default order.by = "degree", decreasing = FALSE
  degree_order <- combo_order  # already in degree order

  ggplot(df, aes(x = combo, y = rate)) +
    geom_col(fill = "#595959") +
    scale_x_discrete(limits = degree_order, drop = FALSE) +
    scale_y_continuous(
      name   = y_label,
      expand = expansion(mult = c(0, 0.12))
    ) +
    theme_bw() +
    theme(
      axis.title.x    = element_blank(),
      axis.text.x     = element_blank(),
      axis.ticks.x    = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border    = element_blank(),
      panel.background = element_blank(),
      axis.line       = element_line(colour = "black"),
      plot.margin     = margin(t = 5, r = 5, b = 0, l = 5)
    )
}


# ---- upset_plot_rates --------------------------------------------------------
# Condensed version (4 seasons, side-by-side mild+severe per row)

upset_plot_rates <- function(input, seasons, per_n = 100000) {

  filtered_input_all <- input %>%
    filter(subset %in% seasons) %>%
    mutate(`n (midpoint 10 rounded)` = as.numeric(`n (midpoint 10 rounded)`))

  full_combos    <- c("RSV", "Flu", "COVID", "RSV & Flu", "RSV & COVID",
                      "Flu & COVID", "RSV & Flu & COVID")
  codelist_types <- c("Specific", "Sensitive")

  y_label <- paste0("Rate per ", format(per_n, big.mark = ",", scientific = FALSE))

  plot_list_all <- list()

  for (season in seasons) {

    filtered_input <- filtered_input_all %>% filter(subset == season)

    df_mild   <- build_combo_df(filtered_input, season, "Mild",
                                codelist_types, full_combos, per_n)
    df_severe <- build_combo_df(filtered_input, season, "Severe",
                                codelist_types, full_combos, per_n)
    df_season <- bind_rows(df_mild, df_severe)

    plot_list_all[[season]] <- df_season
  }

  final_df <- bind_rows(plot_list_all) %>%
    mutate(
      n            = if_else(is.na(n), 0L, n),
      codelist_type = factor(codelist_type, levels = codelist_types),
      subset        = gsub("_", "-", subset),
      combo = factor(case_when(
        combo == "RSV & COVID"       ~ "RSV&COVID",
        combo == "RSV & Flu"         ~ "RSV&Flu",
        combo == "Flu & COVID"       ~ "Flu&COVID",
        combo == "RSV & Flu & COVID" ~ "RSV&Flu&COVID",
        TRUE ~ as.character(combo)
      ), levels = c("RSV", "Flu", "COVID", "RSV&COVID", "RSV&Flu",
                    "Flu&COVID", "RSV&Flu&COVID"))
    ) %>%
    mutate(combo = gsub("COVID", "COVID-19", gsub("Flu", "Influenza", combo)))

  combo_order <- c("RSV", "Influenza", "COVID-19",
                   "RSV&Influenza", "RSV&COVID-19",
                   "Influenza&COVID-19", "RSV&Influenza&COVID-19")

  outcomes   <- c("Mild", "Severe")
  subsets    <- unique(final_df$subset)
  f_cols     <- brewer.pal(3, "Set2")

  plots_by_outcome <- list()

  for (outcome in outcomes) {

    season_plots <- list()

    for (season in subsets) {

      panel <- list()

      for (phenotype in codelist_types) {

        sub_df <- final_df %>%
          filter(outcome_type == outcome, subset == season,
                 codelist_type == phenotype)

        # Integer counts for UpSetR matrix layout
        input2     <- as.data.table(sub_df %>% select(combo, n))
        input_expr <- tibble::deframe(input2)
        # ensure no NAs
        input_expr[is.na(input_expr)] <- 0L

        uu <- upset(fromExpression(input_expr),
                    nsets             = 3,
                    keep.order        = TRUE,
                    order.by          = "degree",
                    decreasing        = FALSE,
                    mb.ratio          = c(0.8, 0.2),
                    text.scale        = c(1.25, 1.25, 1.25, 1.25, 1.25, 1.25),
                    point.size        = 2,
                    line.size         = 1,
                    mainbar.y.label   = y_label,
                    empty.intersections = TRUE,
                    sets.bar.color    = rev(f_cols),
                    sets              = c("COVID-19", "Influenza", "RSV")
        )

        # Rate vector in combo_order
        rate_vec <- setNames(
          sub_df$rate[match(combo_order, sub_df$combo)],
          combo_order
        )
        rate_vec[is.na(rate_vec)] <- 0

        main_bar_plot <- build_main_bar(rate_vec, combo_order, y_label, f_cols)

        # Desired: RSV=Set2[1], Influenza=Set2[2], COVID-19=Set2[3]
        # sets arg is c("COVID-19","Influenza","RSV") so bar colours must be reversed
        set_cols <- setNames(f_cols, c("RSV", "Influenza", "COVID-19"))

        # Set-size bar using rates (total per pathogen = sum of all combos containing it)
        sizes_data <- sub_df %>%
          summarise(
            RSV       = sum(rate[str_detect(combo, "RSV")],       na.rm = TRUE),
            Influenza = sum(rate[str_detect(combo, "Influenza")],  na.rm = TRUE),
            `COVID-19`= sum(rate[str_detect(combo, "COVID-19")],  na.rm = TRUE)
          ) %>%
          pivot_longer(everything(), names_to = "set", values_to = "rate") %>%
          mutate(set = factor(set, levels = c("RSV", "Influenza", "COVID-19")))

        sizes_plot <- ggplot(sizes_data, aes(x = set, y = rate)) +
          geom_col(aes(fill = set), show.legend = TRUE) +
          scale_fill_manual(
            values = set_cols,
            name   = "Virus"
          ) +
          ggrepel::geom_text_repel(
            aes(label = round(rate, 1)),
            size = 3, direction = "y",
            point.padding = NA, segment.color = NA
          ) +
          labs(x = NULL) +
          scale_y_continuous(
            name   = y_label,
            expand = expansion(mult = c(0, 0.1))
          ) +
          theme_bw() +
          theme(
            axis.line        = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border     = element_blank(),
            panel.background = element_blank(),
            axis.text.x      = element_blank(),
            axis.ticks.x     = element_blank(),
            legend.direction = "horizontal",
            legend.position  = "bottom",
            legend.title     = element_text(size = 14)
          ) +
          guides(fill = guide_legend(label.position = "left"))

        legend <- get_legend(sizes_plot)

        panel[[phenotype]] <- plot_grid(
          plot_grid(
            sizes_plot + theme(legend.position = "none"),
            NULL, nrow = 2, rel_heights = c(0.95, 0.05)
          ),
          plot_grid(
            uu$Matrix,
            main_bar_plot,
            NULL,
            nrow = 3,
            rel_heights = c(1, 2.75, 0.05),
            align = "hv"
          ),
          ncol = 2, align = "hv", rel_widths = c(0.25, 0.75)
        )
      }

      season_plots[[season]] <- plot_grid(
        panel[["Specific"]],
        NULL,
        panel[["Sensitive"]],
        ncol = 3, rel_widths = c(0.8, 0.01, 0.8)
      )
    }

    plot_label <- list()

    plot_label[["Mild"]] <- ggdraw() +
      draw_label("A. Specific Mild",   x = 0.5, y = 0.5,
                 hjust = 1.35, vjust = 0.5, fontface = "bold", size = 14) +
      draw_label("B. Sensitive Mild",  x = 1,   y = 0.5,
                 hjust = 1.55, vjust = 0.5, fontface = "bold", size = 14) +
      theme(plot.background = element_rect(fill = "white", colour = "white"))

    plot_label[["Severe"]] <- ggdraw() +
      draw_label("C. Specific Severe",  x = 0.5, y = 0.5,
                 hjust = 1.35, vjust = 0.5, fontface = "bold", size = 14) +
      draw_label("D. Sensitive Severe", x = 1,   y = 0.5,
                 hjust = 1.55, vjust = 0.5, fontface = "bold", size = 14) +
      theme(plot.background = element_rect(fill = "white", colour = "white"))

    other_plot_label <- ggdraw() +
      draw_label("Total Rate", x = 1, y = 0, hjust = 12.68, vjust = -1.2, size = 12) +
      draw_label("Total Rate", x = 1, y = 0, hjust = 5.9,   vjust = -1.2, size = 12) +
      theme(plot.background = element_rect(fill = "white", colour = "white"))

    plots_by_outcome[[outcome]] <- plot_grid(
      other_plot_label,
      plot_grid(
        plotlist  = season_plots,
        ncol      = 1, align = "v", axis = "tb",
        labels    = gsub("_", "-", seasons),
        vjust     = 0.2, hjust = 0.5, label_size = 12
      ),
      ncol = 1, rel_heights = c(0.01, 0.99)
    )
  }

  plot_final2 <- plot_grid(
    plot_label[["Mild"]],
    plot_grid(NULL, plots_by_outcome[["Mild"]],   ncol = 2, rel_widths = c(0.05, 1)),
    plot_label[["Severe"]],
    plot_grid(NULL, plots_by_outcome[["Severe"]], ncol = 2, rel_widths = c(0.05, 1)),
    nrow = 5,
    rel_heights   = c(0.025, 0.5, 0.025, 0.5),
    labels        = c("", "", "", ""),
    label_size    = 14,
    label_fontface = "bold",
    hjust         = c(0, -0.1, 0, -0.1),
    vjust         = -2
  )

  plot_grid(
    legend,
    plot_final2,
    nrow = 2, rel_heights = c(0.025, 0.925)
  )
}


# ---- upset_plot_supplement_rates ---------------------------------------------
# Supplemental version (8 seasons), returns list(mild_plot, severe_plot)

upset_plot_supplement_rates <- function(input, seasons, per_n = 100000) {

  filtered_input_all <- input %>%
    filter(subset %in% seasons) %>%
    mutate(`n (midpoint 10 rounded)` = as.numeric(`n (midpoint 10 rounded)`))

  full_combos    <- c("RSV", "Flu", "COVID", "RSV & Flu", "RSV & COVID",
                      "Flu & COVID", "RSV & Flu & COVID")
  codelist_types <- c("Specific", "Sensitive")

  y_label <- paste0("Rate per ", format(per_n, big.mark = ",", scientific = FALSE))

  plot_list_all <- list()

  for (season in seasons) {
    filtered_input <- filtered_input_all %>% filter(subset == season)
    df_mild   <- build_combo_df(filtered_input, season, "Mild",
                                codelist_types, full_combos, per_n)
    df_severe <- build_combo_df(filtered_input, season, "Severe",
                                codelist_types, full_combos, per_n)
    plot_list_all[[season]] <- bind_rows(df_mild, df_severe)
  }

  final_df <- bind_rows(plot_list_all) %>%
    mutate(
      n            = if_else(is.na(n), 0L, n),
      codelist_type = factor(codelist_type, levels = codelist_types),
      subset        = gsub("_", "-", subset),
      combo = factor(case_when(
        combo == "RSV & COVID"       ~ "RSV&COVID",
        combo == "RSV & Flu"         ~ "RSV&Flu",
        combo == "Flu & COVID"       ~ "Flu&COVID",
        combo == "RSV & Flu & COVID" ~ "RSV&Flu&COVID",
        TRUE ~ as.character(combo)
      ), levels = c("RSV", "Flu", "COVID", "RSV&COVID", "RSV&Flu",
                    "Flu&COVID", "RSV&Flu&COVID"))
    ) %>%
    mutate(combo = gsub("COVID", "COVID-19", gsub("Flu", "Influenza", combo)))

  combo_order <- c("RSV", "Influenza", "COVID-19",
                   "RSV&Influenza", "RSV&COVID-19",
                   "Influenza&COVID-19", "RSV&Influenza&COVID-19")

  outcomes   <- c("Mild", "Severe")
  subsets    <- unique(final_df$subset)
  f_cols     <- brewer.pal(3, "Set2")

  result_plots <- list()

  for (outcome in outcomes) {

    season_plots <- list()

    for (season in subsets) {

      panel <- list()

      for (phenotype in codelist_types) {

        sub_df <- final_df %>%
          filter(outcome_type == outcome, subset == season,
                 codelist_type == phenotype)

        input2     <- as.data.table(sub_df %>% select(combo, n))
        input_expr <- tibble::deframe(input2)
        input_expr[is.na(input_expr)] <- 0L

        uu <- upset(fromExpression(input_expr),
                    nsets             = 3,
                    keep.order        = TRUE,
                    order.by          = "degree",
                    decreasing        = FALSE,
                    mb.ratio          = c(0.8, 0.2),
                    text.scale        = c(1.25, 1.25, 1.25, 1.25, 1.25, 1.25),
                    point.size        = 2,
                    line.size         = 1,
                    mainbar.y.label   = y_label,
                    empty.intersections = TRUE,
                    sets.bar.color    = rev(f_cols),
                    sets              = c("COVID-19", "Influenza", "RSV")
        )

        rate_vec <- setNames(
          sub_df$rate[match(combo_order, sub_df$combo)],
          combo_order
        )
        rate_vec[is.na(rate_vec)] <- 0

        main_bar_plot <- build_main_bar(rate_vec, combo_order, y_label, f_cols)

        sizes_data <- sub_df %>%
          summarise(
            RSV        = sum(rate[str_detect(combo, "RSV")],      na.rm = TRUE),
            Influenza  = sum(rate[str_detect(combo, "Influenza")], na.rm = TRUE),
            `COVID-19` = sum(rate[str_detect(combo, "COVID-19")], na.rm = TRUE)
          ) %>%
          pivot_longer(everything(), names_to = "set", values_to = "rate") %>%
          mutate(set = factor(set, levels = c("RSV", "Influenza", "COVID-19")))

        # Desired: RSV=Set2[1], Influenza=Set2[2], COVID-19=Set2[3]
        # sets arg is c("COVID-19","Influenza","RSV") so bar colours must be reversed
        set_cols <- setNames(f_cols, c("RSV", "Influenza", "COVID-19"))

        sizes_plot <- ggplot(sizes_data, aes(x = set, y = rate)) +
          geom_col(aes(fill = set), show.legend = TRUE) +
          scale_fill_manual(
            values = set_cols,
            name   = "Virus"
          ) +
          ggrepel::geom_text_repel(
            aes(label = round(rate, 1)), size = 3, direction = "y",
            point.padding = NA, segment.color = NA
          ) +
          labs(x = NULL) +
          scale_y_continuous(name = y_label, expand = expansion(mult = c(0, 0.1))) +
          theme_bw() +
          theme(
            axis.line        = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border     = element_blank(),
            panel.background = element_blank(),
            axis.text.x      = element_blank(),
            axis.ticks.x     = element_blank(),
            legend.direction = "horizontal",
            legend.position  = "bottom",
            legend.title     = element_text(size = 14)
          ) +
          guides(fill = guide_legend(label.position = "left"))

        legend <- get_legend(sizes_plot)

        panel[[phenotype]] <- plot_grid(
          plot_grid(
            sizes_plot + theme(legend.position = "none"),
            NULL, nrow = 2, rel_heights = c(0.95, 0.05)
          ),
          plot_grid(
            uu$Matrix,
            main_bar_plot,
            NULL,
            nrow = 3, rel_heights = c(1, 2.75, 0.05), align = "hv"
          ),
          ncol = 2, align = "hv", rel_widths = c(0.25, 0.75)
        )
      }

      season_plots[[season]] <- plot_grid(
        panel[["Specific"]], NULL, panel[["Sensitive"]],
        ncol = 3, rel_widths = c(0.8, 0.01, 0.8)
      )
    }

    plot_label <- ggdraw() +
      draw_label("A. Specific Phenotype",  x = 0.5, y = 0,
                 hjust = 1.35, vjust = -0.75, fontface = "bold", size = 14) +
      draw_label("B. Sensitive Phenotype", x = 1,   y = 0,
                 hjust = 1.55, vjust = -0.75, fontface = "bold", size = 14) +
      theme(plot.background = element_rect(fill = "white", colour = "white"))

    other_plot_label <- ggdraw() +
      draw_label("Total Rate", x = 1, y = 0, hjust = 12.68, vjust = -1.2, size = 12) +
      draw_label("Total Rate", x = 1, y = 0, hjust = 5.9,   vjust = -1.2, size = 12) +
      theme(plot.background = element_rect(fill = "white", colour = "white"))

    outcome_plot <- plot_grid(
      other_plot_label,
      plot_grid(
        plotlist  = season_plots,
        ncol      = 1, align = "v", axis = "tb",
        labels    = gsub("_", "-", seasons),
        vjust     = 0.2, hjust = 0.5, label_size = 12
      ),
      ncol = 1, rel_heights = c(0.01, 0.99)
    )

    result_plots[[outcome]] <- plot_grid(
      legend,
      plot_label,
      plot_grid(NULL, outcome_plot, ncol = 2, rel_widths = c(0.05, 1)),
      nrow = 3, rel_heights = c(0.02, 0.02, 0.925)
    )
  }

  list(result_plots[["Mild"]], result_plots[["Severe"]])
}
