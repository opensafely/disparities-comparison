library(here)
library(magick)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "children_and_adolescents"
} else {
  cohort <- args[[1]]
}

## create output directories ----
fs::dir_create(here::here("output", "collated", "exploratory"))

#import plots
s1 <- image_read(paste0(here::here("output", "exploratory"), "/",
                      "ethnicity_by_age_", cohort,
                      "_2016_2017_specific_primary.png"))
s2 <- image_read(paste0(here::here("output", "exploratory"), "/",
                      "ethnicity_by_age_", cohort,
                      "_2017_2018_specific_primary.png"))
s3 <- image_read(paste0(here::here("output", "exploratory"), "/",
                      "ethnicity_by_age_", cohort,
                      "_2018_2019_specific_primary.png"))
s4 <- image_read(paste0(here::here("output", "exploratory"), "/",
                      "ethnicity_by_age_", cohort,
                      "_2019_2020_specific_primary.png"))
s5 <- image_read(paste0(here::here("output", "exploratory"), "/",
                      "ethnicity_by_age_", cohort,
                      "_2020_2021_specific_primary.png"))
s6 <- image_read(paste0(here::here("output", "exploratory"), "/",
                      "ethnicity_by_age_", cohort,
                      "_2021_2022_specific_primary.png"))
s7 <- image_read(paste0(here::here("output", "exploratory"), "/",
                      "ethnicity_by_age_", cohort,
                      "_2022_2023_specific_primary.png"))
s8 <- image_read(paste0(here::here("output", "exploratory"), "/",
                      "ethnicity_by_age_", cohort,
                      "_2023_2024_specific_primary.png"))

col1 <- image_append(c(s1, s3, s5, s7), stack = TRUE)
col2 <- image_append(c(s2, s4, s6, s8), stack = TRUE)

patch_plot <- image_append(c(col1, col2), stack = FALSE)

image_write(patch_plot, paste0(here::here("output", "collated", "exploratory"),
                        "/ethnicity_by_age_", cohort, "_specific_primary.png"))
