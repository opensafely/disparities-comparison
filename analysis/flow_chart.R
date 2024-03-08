library("tidyverse")
library("here")
library("arrow")
library("ggplot2")
library("data.table")
library("gtsummary")

#define study start date and study end date
source(here("analysis", "design", "design.R"))
args <- commandArgs(trailingOnly = TRUE)
study_start_date <- study_dates[[args[[2]]]]
study_end_date <- study_dates[[args[[3]]]]
cohort <- args[[1]]

patients_df <- read_feather(
  here::here("output", paste0(cohort, "_", year(study_start_date), "_", 
                              year(study_end_date), "_flow_chart", ".arrow")))


library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)

# Define counts based on inclusion and exclusion criteria
registered_count <- sum(patients_df$registered)
gender_count <- sum(patients_df$is_female_or_male)
age_count <- sum(patients_df$is_appropriate_age & registered_count)
imd_count <- sum(patients_df$has_imd)
included_count <- sum(patients_df$registered & patients_df$is_female_or_male & patients_df$is_appropriate_age & patients_df$has_imd)
care_home_count <- sum(patients_df$registered & patients_df$is_appropriate_age & patients_df$care_home)
demographic_excl_count <- sum(patients_df$registered & patients_df$is_appropriate_age & !patients_df$care_home &!patients_df$is_female_or_male |!patients_df$has_imd)

org_cohort <- boxGrob(glue("Total Patients",
                           "n = {pop}",
                           pop = txtInt(nrow(patients_df)),
                           .sep = "\n"))

follow_up <- boxGrob(glue("At least one year of follow-up before {start}",
                           "n = {follow}",
                           start = study_start_date,
                           follow = txtInt(registered_count),
                           .sep = "\n"))

follow_up_excl <- boxGrob(glue("Less than one year of prior follow-up",
                               "n = {follow_e}",
                               follow_e = txtInt(nrow(patients_df) - registered_count),
                               .sep = "\n"))

age <- boxGrob(glue("Aged 65 or over",
                    "n = {age}",
                    age = txtInt(age_count),
                    .sep = "\n"))

age_excl <- boxGrob(glue("Aged under 65 on {start}",
                         "n = {age_e}",
                         start = study_start_date,
                         age_e = txtInt(nrow(patients_df) - age_count),
                         .sep = "\n"))

not_care_home <- boxGrob(glue("Not in long-term care facility",
                              "n = {nocare}",
                              nocare = txtInt(age_count - care_home_count),
                              .sep = "\n"))

care_home <- boxGrob(glue("In long-term care facility",
                          "n = {care}",
                          care = txtInt(care_home_count),
                          .sep = "\n"))

demographic_excl <- boxGrob(glue("Missing deomgraphic information",
                                 "n = {excl}",
                                 excl = txtInt(demographic_excl_count),
                                 .sep = "\n"))

included <- boxGrob(glue("Included",
                         "n = {incl}",
                         incl = txtInt(included_count),
                         .sep = "\n"))

grid.newpage()
vert <- spreadVertical(org_cohort = org_cohort, follow_up = follow_up, 
                       age = age, not_care_home = not_care_home, 
                       included = included, .from = org_cohort,
                       .to = included, .type = "center")
vert$excluded <- NULL

follow_up_excl <- moveBox(follow_up_excl,
                          x = 0.82,
                          y = coords(vert$follow_up)$top + distance(vert$age, vert$follow_up, half = TRUE, center = FALSE))

age_excl <- moveBox(age_excl,
                    x = 0.82,
                    y = coords(vert$age)$top + distance(vert$not_care_home, vert$age, half = TRUE, center = FALSE))

care_home <- moveBox(care_home,
                     x = 0.82,
                     y = coords(vert$not_care_home)$top + distance(vert$included, vert$not_care_home, half = TRUE, center = FALSE))

demographic_excl <- moveBox(demographic_excl,
                            x = 0.82,
                            y = coords(vert$included)$top + distance(vert$included, vert$not_care_home, half = TRUE, center = FALSE))


for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}

connectGrob(vert$org_cohort, follow_up_excl, type = "L")
connectGrob(vert$follow_up, age_excl, type = "L")
connectGrob(vert$age, care_home, type = "L")
connectGrob(vert$not_care_home,  demographic_excl, type = "L")

# Print boxes
vert
follow_up_excl
age_excl
care_home
demographic_excl
