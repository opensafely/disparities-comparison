library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)

#import plot function
source(here::here("post_check", "functions",
                  "characteristic_visualisation.R"))

###older adults

cohort <- "older_adults"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))

##create relevant plots

#season
older_adults_season <- character_viz(df_input, "none")
older_adults_season_scaled <- character_viz(df_input, "yes")

#characteristic
older_adults_characters <- character_viz_mult(df_input, "none")
older_adults_characters_scaled <- character_viz_mult(df_input, "yes")

#create list of plots
plotlist1 <- list(older_adults_characters_scaled[[1]],
                  older_adults_characters_scaled[[2]])
plotlist2 <- list(older_adults_characters[[1]], older_adults_characters[[2]])

#plot all
print(older_adults_season)
title_name <- older_adults_season$labels$title
saveas <- paste0(gsub(" ", "_", title_name))
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       older_adults_season, height = 8, width = 15)
print(older_adults_season_scaled)
title_name <- older_adults_season_scaled$labels$title
saveas <- paste0(gsub(" ", "_", title_name), "_scaled")
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       older_adults_season, height = 8, width = 15)

t <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist1) {
  
  print(p)
  t <- t+1
  title_name <- older_adults_characters_scaled[[t]]
  saveas <- paste0(title_name, "_scaled")
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

u <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist2) {
  
  print(p)
  u <- u+1
  title_name <- older_adults_characters[[u]]
  saveas <- title_name
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

###adults

cohort <- "adults"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))
df_input <- df_input %>%
  mutate(
    Characteristic = if_else(Characteristic == "18-29y", "18-39y",
                             Characteristic)
  )

##create relevant plots

#season
adults_season <- character_viz(df_input, "none")
adults_season_scaled <- character_viz(df_input, "yes")

#characteristic
adults_characters <- character_viz_mult(df_input, "none")
adults_characters_scaled <- character_viz_mult(df_input, "yes")

#create list of plots
plotlist1 <- list(adults_characters_scaled[[1]],
                  adults_characters_scaled[[2]])
plotlist2 <- list(adults_characters[[1]], adults_characters[[2]])

#plot all
print(adults_season)
title_name <- adults_season$labels$title
saveas <- paste0(gsub(" ", "_", title_name))
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       adults_season, height = 8, width = 15)
print(adults_season_scaled)
title_name <- adults_season_scaled$labels$title
saveas <- paste0(gsub(" ", "_", title_name), "_scaled")
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       adults_season, height = 8, width = 15)

t <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist1) {
  
  print(p)
  t <- t+1
  title_name <- adults_characters_scaled[[t]]
  saveas <- paste0(title_name, "_scaled")
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

u <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist2) {
  
  print(p)
  u <- u+1
  title_name <- adults_characters[[u]]
  saveas <- title_name
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

###children and adolescents

cohort <- "children_and_adolescents"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))

##create relevant plots

#season
children_and_adolescents_season <- character_viz(df_input, "none")
children_and_adolescents_season_scaled <- character_viz(df_input, "yes")

#characteristic
children_and_adolescents_characters <- character_viz_mult(df_input, "none")
children_and_adolescents_characters_scaled <- character_viz_mult(df_input, "yes")

#create list of plots
plotlist1 <- list(children_and_adolescents_characters_scaled[[1]],
                  children_and_adolescents_characters_scaled[[2]])
plotlist2 <- list(children_and_adolescents_characters[[1]],
                  children_and_adolescents_characters[[2]])

#plot all
print(children_and_adolescents_season)
title_name <- children_and_adolescents_season$labels$title
saveas <- paste0(gsub(" ", "_", title_name))
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       children_and_adolescents_season, height = 8, width = 15)
print(children_and_adolescents_season_scaled)
title_name <- children_and_adolescents_season_scaled$labels$title
saveas <- paste0(gsub(" ", "_", title_name), "_scaled")
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       children_and_adolescents_season, height = 8, width = 15)

t <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist1) {
  
  print(p)
  t <- t+1
  title_name <- children_and_adolescents_characters_scaled[[t]]
  saveas <- paste0(title_name, "_scaled")
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

u <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist2) {
  
  print(p)
  u <- u+1
  title_name <- children_and_adolescents_characters[[u]]
  saveas <- title_name
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

###infants

cohort <- "infants"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))

##create relevant plots

#season
infants_season <- character_viz(df_input, "none")
infants_season_scaled <- character_viz(df_input, "yes")

#characteristic
infants_characters <- character_viz_mult(df_input, "none")
infants_characters_scaled <- character_viz_mult(df_input, "yes")

#create list of plots
plotlist1 <- list(infants_characters_scaled[[1]],
                  infants_characters_scaled[[2]])
plotlist2 <- list(infants_characters[[1]], infants_characters[[2]])

#plot all
print(infants_season)
title_name <- infants_season$labels$title
saveas <- paste0(gsub(" ", "_", title_name))
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       infants_season, height = 8, width = 15)
print(infants_season_scaled)
title_name <- infants_season_scaled$labels$title
saveas <- paste0(gsub(" ", "_", title_name), "_scaled")
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       infants_season, height = 8, width = 15)

t <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist1) {
  
  print(p)
  t <- t+1
  title_name <- infants_characters_scaled[[t]]
  saveas <- paste0(title_name, "_scaled")
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

u <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist2) {
  
  print(p)
  u <- u+1
  title_name <- infants_characters[[u]]
  saveas <- title_name
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

###infants subgroup

cohort <- "infants_subgroup"
investigation_type <- "primary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort, "_table1_collated.csv")))
df_input <- df_input %>%
  filter(Characteristic != "Maternal Age")

##create relevant plots

#season
infants_subgroup_season <- character_viz(df_input, "none")
infants_subgroup_season_scaled <- character_viz(df_input, "yes")

#characteristic
infants_subgroup_characters <- character_viz_mult(df_input, "none")
infants_subgroup_characters_scaled <- character_viz_mult(df_input, "yes")

#create list of plots
plotlist1 <- list(infants_subgroup_characters_scaled[[1]],
                  infants_subgroup_characters_scaled[[2]])
plotlist2 <- list(infants_subgroup_characters[[1]],
                  infants_subgroup_characters[[2]])

#plot all
print(infants_subgroup_season)
title_name <- infants_subgroup_season$labels$title
saveas <- paste0(gsub(" ", "_", title_name))
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       infants_subgroup_season, height = 8, width = 15)
print(infants_subgroup_season_scaled)
title_name <- infants_subgroup_season_scaled$labels$title
saveas <- paste0(gsub(" ", "_", title_name), "_scaled")
ggsave(here("post_check", "plots", "primary_analyses",
            paste0(str_to_title(cohort), "_", saveas, ".png")),
       infants_subgroup_season, height = 8, width = 15)

t <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist1) {
  
  print(p)
  t <- t+1
  title_name <- infants_subgroup_characters_scaled[[t]]
  saveas <- paste0(title_name, "_scaled")
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}

u <- 2
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
for(p in plotlist2) {
  
  print(p)
  u <- u+1
  title_name <- infants_subgroup_characters[[u]]
  saveas <- title_name
  ggsave(here("post_check", "plots", "primary_analyses",
              paste0(str_to_title(cohort), "_", saveas, ".png")), p,
         height = 8, width = 15)
  
}
