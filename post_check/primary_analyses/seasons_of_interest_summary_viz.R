library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)

#import plot function
source(here::here("post_check", "functions", "rates_visualisation.R"))

investigation_type <- "primary"

###older adults

cohort <- "older_adults"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_rates_primary_collated.csv")))

##plot rates for older_adults

#rsv
older_adults_rsv_mild <- rate_viz(
  df_input, "RSV", "Mild", "yes")
older_adults_season_rsv_mild <- rate_viz_season(
  df_input, "RSV", "Mild", "yes")
older_adults_characters_rsv_mild <- rate_viz_mult(
  df_input, "RSV", "Mild", "yes")
older_adults_rsv_severe <- rate_viz(
  df_input, "RSV", "Severe", "yes")
older_adults_season_rsv_severe <- rate_viz_season(
  df_input, "RSV", "Severe", "yes")
older_adults_characters_rsv_severe <- rate_viz_mult(
  df_input, "RSV", "Severe", "yes")

#flu
older_adults_flu_mild <- rate_viz(
  df_input, "Flu", "Mild", "yes")
older_adults_season_flu_mild <- rate_viz_season(
  df_input, "Flu", "Mild", "yes")
older_adults_characters_flu_mild <- rate_viz_mult(
  df_input, "Flu", "Mild", "yes")
older_adults_flu_severe <- rate_viz(
  df_input, "Flu", "Severe", "yes")
older_adults_season_flu_severe <- rate_viz_season(
  df_input, "Flu", "Severe", "yes")
older_adults_characters_flu_severe <- rate_viz_mult(
  df_input, "Flu", "Severe", "yes")

#covid
older_adults_covid_mild <- rate_viz(
  df_input, "COVID", "Mild", "yes")
older_adults_season_covid_mild <- rate_viz_season(
  df_input, "COVID", "Mild", "yes")
older_adults_characters_covid_mild <- rate_viz_mult(
  df_input, "COVID", "Mild", "yes")
older_adults_covid_severe <- rate_viz(
  df_input, "COVID", "Severe", "yes")
older_adults_season_covid_severe <- rate_viz_season(
  df_input, "COVID", "Severe", "yes")
older_adults_characters_covid_severe <- rate_viz_mult(
  df_input, "COVID", "Severe", "yes")

#create list of plots
plotlist1 <- list(
  older_adults_rsv_mild,
  older_adults_rsv_severe,
  older_adults_flu_mild,
  older_adults_flu_severe,
  older_adults_covid_mild,
  older_adults_covid_severe
)

plotlist2 <- list(
  older_adults_season_rsv_mild,
  older_adults_season_rsv_severe,
  older_adults_season_flu_mild,
  older_adults_season_flu_severe,
  older_adults_season_covid_mild,
  older_adults_season_covid_severe
)
plotlist3 <- list(
  older_adults_characters_rsv_mild,
  older_adults_characters_rsv_severe,
  older_adults_characters_flu_mild,
  older_adults_characters_flu_severe,
  older_adults_characters_covid_mild,
  older_adults_characters_covid_severe
)

#plot all
for(p in plotlist1) {
  
  print(p)
  title_name <- p$labels$title
  saveas <- paste0(gsub(" ", "_", title_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
         "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

for(p in 1:length(plotlist2)) {
  
  plots <- plotlist2[[p]]
  
  for (i in 1:length(plots)) {
    
    p1 <- plots[[i]]
    print(p1)
    title_name <- p1$labels$title
    saveas <- paste0(gsub(" ", "_", title_name))
    ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
           "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
           p1, height = 8, width = 15)
    
  }
  
}

for(p in 1:length(plotlist3)) {
  
  plots <- plotlist3[[p]]
  
  for (i in 1:length(plots)) {
    
    p1 <- plots[[i]]
    print(p1)
    title_name <- p1$labels$title
    saveas <- paste0(gsub(" ", "_", title_name))
    ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
           "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
           p1, height = 8, width = 15)
    
  }
  
}

###infants 

cohort <- "infants"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_rates_primary_collated.csv")))

##plot rates for infants

#rsv
infants_rsv_mild <- rate_viz(
  df_input, "RSV", "Mild", "yes")
infants_season_rsv_mild <- rate_viz_season(
  df_input, "RSV", "Mild", "yes")
infants_characters_rsv_mild <- rate_viz_mult(
  df_input, "RSV", "Mild", "yes")
infants_rsv_severe <- rate_viz(
  df_input, "RSV", "Severe", "yes")
infants_season_rsv_severe <- rate_viz_season(
  df_input, "RSV", "Severe", "yes")
infants_characters_rsv_severe <- rate_viz_mult(
  df_input, "RSV", "Severe", "yes")

#flu
infants_flu_mild <- rate_viz(
  df_input, "Flu", "Mild", "yes")
infants_season_flu_mild <- rate_viz_season(
  df_input, "Flu", "Mild", "yes")
infants_characters_flu_mild <- rate_viz_mult(
  df_input, "Flu", "Mild", "yes")
infants_flu_severe <- rate_viz(
  df_input, "Flu", "Severe", "yes")
infants_season_flu_severe <- rate_viz_season(
  df_input, "Flu", "Severe", "yes")
infants_characters_flu_severe <- rate_viz_mult(
  df_input, "Flu", "Severe", "yes")

#covid
infants_covid_mild <- rate_viz(
  df_input, "COVID", "Mild", "yes")
infants_season_covid_mild <- rate_viz_season(
  df_input, "COVID", "Mild", "yes")
infants_characters_covid_mild <- rate_viz_mult(
  df_input, "COVID", "Mild", "yes")
infants_covid_severe <- rate_viz(
  df_input, "COVID", "Severe", "yes")
infants_season_covid_severe <- rate_viz_season(
  df_input, "COVID", "Severe", "yes")
infants_characters_covid_severe <- rate_viz_mult(
  df_input, "COVID", "Severe", "yes")

#create list of plots
plotlist1 <- list(
  infants_rsv_mild,
  infants_rsv_severe,
  infants_flu_mild,
  infants_flu_severe,
  infants_covid_mild,
  infants_covid_severe
)

plotlist2 <- list(
  infants_season_rsv_mild,
  infants_season_rsv_severe,
  infants_season_flu_mild,
  infants_season_flu_severe,
  infants_season_covid_mild,
  infants_season_covid_severe
)
plotlist3 <- list(
  infants_characters_rsv_mild,
  infants_characters_rsv_severe,
  infants_characters_flu_mild,
  infants_characters_flu_severe,
  infants_characters_covid_mild,
  infants_characters_covid_severe
)

#plot all
for(p in plotlist1) {
  
  print(p)
  title_name <- p$labels$title
  saveas <- paste0(gsub(" ", "_", title_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
         "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

for(p in 1:length(plotlist2)) {
  
  plots <- plotlist2[[p]]
  
  for (i in 1:length(plots)) {
    
    p1 <- plots[[i]]
    print(p1)
    title_name <- p1$labels$title
    saveas <- paste0(gsub(" ", "_", title_name))
    ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
           "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
           p1, height = 8, width = 15)
    
  }
  
}

for(p in 1:length(plotlist3)) {
  
  plots <- plotlist3[[p]]
  
  for (i in 1:length(plots)) {
    
    p1 <- plots[[i]]
    print(p1)
    title_name <- p1$labels$title
    saveas <- paste0(gsub(" ", "_", title_name))
    ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
           "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
           p1, height = 8, width = 15)
    
  }
  
}

###infants_subgroup 

cohort <- "infants_subgroup"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                     "descriptive", paste0(cohort,
                     "_rates_primary_collated.csv")))
df_input <- df_input %>%
  filter(Characteristic != "Average Maternal Age")

##plot rates for infants_subgroup

#rsv
infants_subgroup_rsv_mild <- rate_viz(
  df_input, "RSV", "Mild", "yes")
infants_subgroup_season_rsv_mild <- rate_viz_season(
  df_input, "RSV", "Mild", "yes")
infants_subgroup_characters_rsv_mild <- rate_viz_mult(
  df_input, "RSV", "Mild", "yes")
infants_subgroup_rsv_severe <- rate_viz(
  df_input, "RSV", "Severe", "yes")
infants_subgroup_season_rsv_severe <- rate_viz_season(
  df_input, "RSV", "Severe", "yes")
infants_subgroup_characters_rsv_severe <- rate_viz_mult(
  df_input, "RSV", "Severe", "yes")

#flu
infants_subgroup_flu_mild <- rate_viz(
  df_input, "Flu", "Mild", "yes")
infants_subgroup_season_flu_mild <- rate_viz_season(
  df_input, "Flu", "Mild", "yes")
infants_subgroup_characters_flu_mild <- rate_viz_mult(
  df_input, "Flu", "Mild", "yes")
infants_subgroup_flu_severe <- rate_viz(
  df_input, "Flu", "Severe", "yes")
infants_subgroup_season_flu_severe <- rate_viz_season(
  df_input, "Flu", "Severe", "yes")
infants_subgroup_characters_flu_severe <- rate_viz_mult(
  df_input, "Flu", "Severe", "yes")

#covid
infants_subgroup_covid_mild <- rate_viz(
  df_input, "COVID", "Mild", "yes")
infants_subgroup_season_covid_mild <- rate_viz_season(
  df_input, "COVID", "Mild", "yes")
infants_subgroup_characters_covid_mild <- rate_viz_mult(
  df_input, "COVID", "Mild", "yes")
infants_subgroup_covid_severe <- rate_viz(
  df_input, "COVID", "Severe", "yes")
infants_subgroup_season_covid_severe <- rate_viz_season(
  df_input, "COVID", "Severe", "yes")
infants_subgroup_characters_covid_severe <- rate_viz_mult(
  df_input, "COVID", "Severe", "yes")

#create list of plots
plotlist1 <- list(
  infants_subgroup_rsv_mild,
  infants_subgroup_rsv_severe,
  infants_subgroup_flu_mild,
  infants_subgroup_flu_severe,
  infants_subgroup_covid_mild,
  infants_subgroup_covid_severe
)

plotlist2 <- list(
  infants_subgroup_season_rsv_mild,
  infants_subgroup_season_rsv_severe,
  infants_subgroup_season_flu_mild,
  infants_subgroup_season_flu_severe,
  infants_subgroup_season_covid_mild,
  infants_subgroup_season_covid_severe
)
plotlist3 <- list(
  infants_subgroup_characters_rsv_mild,
  infants_subgroup_characters_rsv_severe,
  infants_subgroup_characters_flu_mild,
  infants_subgroup_characters_flu_severe,
  infants_subgroup_characters_covid_mild,
  infants_subgroup_characters_covid_severe
)

#plot all
for(p in plotlist1) {
  
  print(p)
  title_name <- p$labels$title
  saveas <- paste0(gsub(" ", "_", title_name))
  ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
         "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
         p, height = 8, width = 15)
  
}

for(p in 1:length(plotlist2)) {
  
  plots <- plotlist2[[p]]
  
  for (i in 1:length(plots)) {
    
    p1 <- plots[[i]]
    print(p1)
    title_name <- p1$labels$title
    saveas <- paste0(gsub(" ", "_", title_name))
    ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
           "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
           p1, height = 8, width = 15)
    
  }
  
}

for(p in 1:length(plotlist3)) {
  
  plots <- plotlist3[[p]]
  
  for (i in 1:length(plots)) {
    
    p1 <- plots[[i]]
    print(p1)
    title_name <- p1$labels$title
    saveas <- paste0(gsub(" ", "_", title_name))
    ggsave(here("post_check", "plots", "primary_analyses", "seasons_of_interest",
           "rates", cohort, paste0(str_to_title(cohort), "_", saveas, ".png")),
           p1, height = 8, width = 15)
    
  }
  
}
