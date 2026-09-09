# 6-panel Table 1 figure for key sociodemographic variables
# Layout:
#   rows: Age Group / Ethnicity / IMD
#   columns: Adults / Older adults
#   x = season, y = number of people (N), colour = category

library(tidyverse)
library(here)
library(cowplot)
library(stringr)
library(scales)

ggsave <- function(..., bg = "white") ggplot2::ggsave(..., bg = bg)

groups_keep <- c("Age Group", "Ethnicity", "IMD")
cohorts <- c("adults", "older_adults")

group_headers <- c(
  "Total", "Age Group", "Sex", "Ethnicity", "IMD", "Household Composition",
  "Rurality", "Prior Flu Vaccine", "Time Since Last Covid Vaccine",
  "Maternal Smoking Status", "Maternal Drinking", "Maternal Drug Usage",
  "Maternal Flu Vaccination", "Maternal Pertussis Vaccination",
  "Smoking Status", "Hazardous Drinking", "Drug Usage"
)

age_levels <- c("18-39y", "40-64y", "65-74y", "75-89y", "90y+")
ethnicity_levels <- c(
  "White", "Mixed", "Asian or Asian British", "Black or Black British",
  "Other Ethnic Groups", "Unknown"
)
# Display uses UK convention (1 = most deprived, 5 = least deprived).
# Processing was fixed in cecf9e4 (Jun 2026) to emit these labels directly.
# Older collated Table 1 outputs still have inverted labels; remap only then.
imd_levels <- c(
  "1 (most deprived)", "2", "3", "4", "5 (least deprived)"
)

category_levels <- c(age_levels, ethnicity_levels, imd_levels)

age_cols <- c(
  "18-39y" = "#4E79A7",
  "40-64y" = "#A0CBE8",
  "65-74y" = "#F28E2B",
  "75-89y" = "#FFBE7D",
  "90y+" = "#E15759"
)
ethnicity_cols <- c(
  "White" = "#4E79A7",
  "Mixed" = "#F28E2B",
  "Asian or Asian British" = "#59A14F",
  "Black or Black British" = "#E15759",
  "Other Ethnic Groups" = "#B07AA1",
  "Unknown" = "#BAB0AC"
)
imd_cols <- c(
  "1 (most deprived)" = "#a50f15",
  "2" = "#fb6a4a",
  "3" = "#9e9ac8",
  "4" = "#6baed6",
  "5 (least deprived)" = "#08519c"
)
category_cols <- c(age_cols, ethnicity_cols, imd_cols)

# Only needed for pre-fix Table 1 outputs (1 = least, 5 = most).
relabel_imd_uk <- function(x) {
  dplyr::case_when(
    x == "1 (least deprived)" ~ "5 (least deprived)",
    x == "2" ~ "4",
    x == "3" ~ "3",
    x == "4" ~ "2",
    x == "5 (most deprived)" ~ "1 (most deprived)",
    TRUE ~ x
  )
}

cohort_labels <- c(
  adults = "Adults",
  older_adults = "Older adults"
)

assign_groups <- function(df) {
  df %>%
    mutate(
      Characteristic = if_else(
        Characteristic == "18-29y", "18-39y", Characteristic
      ),
      Characteristic = if_else(
        Characteristic %in% c("Chinese or Other", "Chinese or other"),
        "Other Ethnic Groups",
        Characteristic
      ),
      is_header = Characteristic %in% group_headers | is.na(N),
      group = if_else(
        is_header & Characteristic != "Total",
        Characteristic,
        NA_character_
      )
    ) %>%
    fill(group, .direction = "down") %>%
    filter(!is_header, !is.na(N), group %in% groups_keep) %>%
    {
      imd_inverted <- any(
        .$group == "IMD" &
          .$Characteristic %in% c("1 (least deprived)", "5 (most deprived)")
      )
      if (imd_inverted) {
        mutate(
          .,
          Characteristic = if_else(
            group == "IMD",
            relabel_imd_uk(Characteristic),
            Characteristic
          )
        )
      } else {
        .
      }
    } %>%
    select(characteristic = Characteristic, n = N, subset, group)
}

import_table1 <- function(cohort_name) {
  cohort_lab <- cohort_labels[[cohort_name]]

  read_csv(
    here::here(
      "post_check", "output", "collated", "descriptive",
      paste0(cohort_name, "_table1_collated.csv")
    ),
    show_col_types = FALSE
  ) %>%
    rename(N = `N (midpoint 10 rounded)`) %>%
    assign_groups() %>%
    mutate(
      cohort = cohort_name,
      cohort_label = factor(cohort_lab, levels = unname(cohort_labels)),
      season = factor(
        str_to_title(gsub("_", "-", subset)),
        levels = str_to_title(gsub(
          "_", "-",
          c(
            "2016_17", "2017_18", "2018_19", "2019_20",
            "2020_21", "2021_22", "2022_23", "2023_24"
          )
        ))
      ),
      group = factor(group, levels = groups_keep),
      characteristic = factor(
        characteristic,
        levels = category_levels[category_levels %in% unique(characteristic)]
      )
    )
}

df_plot <- bind_rows(lapply(cohorts, import_table1))

make_panel <- function(dat, show_x = FALSE, show_y = TRUE, show_legend = TRUE) {
  group_name <- as.character(unique(dat$group))
  cols <- category_cols[names(category_cols) %in% levels(droplevels(dat$characteristic))]

  p <- ggplot(
    dat,
    aes(x = season, y = n, colour = characteristic, group = characteristic)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_colour_manual(values = cols, drop = FALSE) +
    scale_y_continuous(labels = label_comma()) +
    labs(x = NULL, y = if (show_y) "Number of people" else NULL, colour = NULL) +
    theme_bw() +
    theme(
      text = element_text(size = 11),
      axis.text.x = if (show_x) {
        element_text(angle = 45, hjust = 1, vjust = 1)
      } else {
        element_blank()
      },
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      axis.title.y = if (show_y) element_text() else element_blank(),
      legend.position = if (show_legend) "right" else "none",
      plot.margin = margin(4, 4, 4, 4)
    )

  p
}

panels <- list()
for (g in groups_keep) {
  for (c in cohorts) {
    dat <- df_plot %>%
      filter(group == g, cohort == c) %>%
      mutate(characteristic = droplevels(characteristic))

    show_x <- g == "IMD"
    show_y <- c == "adults"
    # Age categories differ by cohort; ethnicity/IMD share levels so legend
    # only on the older-adults column.
    show_legend <- (g == "Age Group") || (c == "older_adults")

    panels[[paste(g, c, sep = "__")]] <- make_panel(
      dat, show_x = show_x, show_y = show_y, show_legend = show_legend
    )
  }
}

col_adults <- ggdraw() +
  draw_label("Adults", fontface = "bold", size = 13)
col_older <- ggdraw() +
  draw_label("Older adults", fontface = "bold", size = 13)

row_labs <- lapply(groups_keep, function(g) {
  ggdraw() +
    draw_label(g, fontface = "bold", size = 12, angle = 90)
})

header <- plot_grid(NULL, col_adults, col_older, ncol = 3,
                    rel_widths = c(0.06, 1.12, 1.12))

rows <- lapply(seq_along(groups_keep), function(i) {
  g <- groups_keep[[i]]
  plot_grid(
    row_labs[[i]],
    panels[[paste(g, "adults", sep = "__")]],
    panels[[paste(g, "older_adults", sep = "__")]],
    ncol = 3,
    rel_widths = c(0.06, 1.12, 1.12),
    align = "h",
    axis = "tb"
  )
})

fig <- plot_grid(
  header,
  rows[[1]],
  rows[[2]],
  rows[[3]],
  ncol = 1,
  rel_heights = c(0.06, 1, 1, 1.15)
)

out_dir <- here::here("post_check", "plots", "primary_analyses", "table1_key_vars")
fs::dir_create(out_dir)

ggsave(
  file.path(out_dir, "Adults_Older_Adults_Age_Ethnicity_IMD_over_time.png"),
  fig,
  height = 10,
  width = 14
)

print(fig)
message("Saved: ", file.path(out_dir, "Adults_Older_Adults_Age_Ethnicity_IMD_over_time.png"))
