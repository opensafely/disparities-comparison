library(tidyverse)
library(here)
library(lubridate)
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(ggpmisc)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

##-- comparing overall cases

#import surveillance data - UKHSA
df_surv_rsv <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance", "UKHSA_reports_RSV.csv")) %>%
  arrange(month)
df_surv_flu <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance", "UKHSA_reports_flu.csv")) %>%
  arrange(month)
df_surv_ukhsa <- merge(
  df_surv_rsv %>% select(c(month, total_rsv = RSV)),
  df_surv_flu %>% select(c(month, total_flu = flu)),
  by = "month"
) %>%
  mutate(type = "Surveillance")
df_surv_ukhsa <- df_surv_ukhsa %>%
  pivot_longer(
    cols = c(total_rsv, total_flu),
    names_to = "virus",
    values_to = "total_events"
  ) %>%
  mutate(
    virus = case_when(
      virus == "total_rsv" ~ "RSV",
      virus == "total_flu" ~ "Influenza"
    ),
    event = "UKHSA"
  ) %>%
  mutate(
    total_events = if_else(is.na(total_events), 0, total_events)
  )
df_surv_ukhsa <- df_surv_ukhsa[, c("month", "event", "total_events", "virus")]

#import surveillance data - WHO
df_surv_who <- read_csv(here::here(
  "post_check", "exploratory_analyses", "surveillance",
  "seasonality_england.csv")) %>%
  arrange(month) %>%
  select(-covid_scaled) %>%
  mutate(
    total_covid = if_else(month < ymd("2020-03-01"), NA, total_covid)
  ) %>%
  pivot_longer(
    cols = c(total_rsv, total_flu, total_covid),
    names_to = "virus",
    values_to = "total_events"
  ) %>%
  mutate(
    virus = case_when(
      virus == "total_rsv" ~ "RSV",
      virus == "total_flu" ~ "Influenza",
      virus == "total_covid" ~ "COVID-19"
    ),
    event = "WHO",
  ) %>%
  mutate(
    total_events = if_else(is.na(total_events) & virus != "COVID-19", 0,
                           total_events)
  )

df_surv_who <- df_surv_who[, c("month", "event", "total_events", "virus")]

df_plot <- bind_rows(
  df_surv_who,
  df_surv_ukhsa
) %>%
  arrange(month)

#plot together 
rects <- tibble(
  xmin = seq(as.Date("2016-11-01"), as.Date("2023-11-01"), by = "year"),
  xmax = seq(as.Date("2017-03-01"), as.Date("2024-03-01"), by = "year"),
  ymin = 0,
  ymax = Inf
)

f <- function(pal) brewer.pal(3, pal)

cols <- f("Set2")

plot <- df_plot %>%
  filter(virus != "COVID-19") %>%
  ggplot() +
  geom_line(aes(x = month, y = total_events, color = virus),
            linewidth = 1) +
  facet_wrap(~event, nrow = 2, scales = "free_y") +
  geom_line(data = df_plot %>% filter(virus == "COVID-19"),
            aes(x = month, y = total_events/100, color = virus),
            linewidth = 1) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, col = NA) +
  scale_x_date(date_breaks = "1 years", date_labels = "%y") +
  scale_y_continuous(n.breaks = 6, sec.axis = sec_axis(
    trans = ~.*100, name = "COVID-19 Cases")) +
  scale_color_manual(values = c(
    "RSV" = cols[1], "Influenza" = cols[2], "COVID-19" = cols[3])) +
  labs(y = "RSV/Influenza Cases", x = "", colour = "Virus") +
  theme_bw() + theme(legend.position = "bottom", text = element_text(size = 22))

#get the legend
legend <- get_legend(plot)

# Dummy data for the grey box
legend_df <- data.frame(
  xmin = 0.2, xmax = 0.4,
  ymin = 0.45, ymax = 0.55
)

transmission_legend <- ggplot() +
  # Grey square box
  geom_rect(data = legend_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.25, color = NA) +
  # Label to the right
  annotate("text", x = 0.5, y = 0.5,
           label = "Usual Transmission Period (Nov–Mar)",
           hjust = 0, vjust = 0.5, size = 8) +
  xlim(0, 3) + ylim(0, 0.75) +  # Give horizontal space
  theme_void() +
  theme(plot.margin = margin(3, -10, -75, -10))

bottom_row <- plot_grid(
  plot_grid(NULL, transmission_legend, ncol = 2, rel_widths = c(0.25, 0.8)),
  NULL,
  legend,
  nrow = 3,
  rel_heights = c(0.5, 0.5, 0.25)
)

plot_grid(
  NULL,
  plot + theme(legend.position = "none"),
  bottom_row,
  ncol = 1,
  rel_heights = c(0.01, 1, 0.1)
) %>% annotate_figure(
  top = text_grob(
    "Monthly Surveillance Counts of RSV, Influenza and COVID-19",
    face = "bold", size = 20, vjust = 1),
  bottom = text_grob("Year (2016-2024)", size = 18, vjust = -7.5)
)

#save
ggsave(here::here("post_check", "plots", "exploratory_analyses",
                  "surveillance_source_comparisons.png"),
       width = 20, height = 14)
