library(tidyverse)
library(here)
library(arrow)
library(lubridate)
library(cowplot)
library(stringr)

cohort <- "older_adults"
investigation_type <- "secondary"

#import collated table 1 outputs
df_input <- read_csv(here::here("post_check", "output", "collated",
                                "descriptive",
                     paste0(cohort, "_rates_secondary_collated.csv")))

#import plot function
source(here::here("post_check", "functions", "rates_visualisation.R"))

##plot rates for older adults

#rsv
older_adults_rsv_mild <- rate_viz(df_input, "RSV", "Mild")
older_adults_season_rsv_mild <- rate_viz_season(df_input, "RSV", "Mild")
older_adults_rsv_severe <- rate_viz(df_input, "RSV", "Severe")
older_adults_season_rsv_severe <- rate_viz_season(df_input, "RSV", "Severe")

#flu
older_adults_flu_mild <- rate_viz(df_input, "Flu", "Mild")
older_adults_season_flu_mild <- rate_viz_season(df_input, "Flu", "Mild")
older_adults_flu_severe <- rate_viz(df_input, "Flu", "Severe")
older_adults_season_flu_severe <- rate_viz_season(df_input, "Flu", "Severe")

#covid
older_adults_covid_mild <- rate_viz(df_input, "COVID", "Mild")
older_adults_season_covid_mild <- rate_viz_season(df_input, "COVID", "Mild")
older_adults_covid_severe <- rate_viz(df_input, "COVID", "Severe")
older_adults_season_covid_severe <- rate_viz_season(df_input, "COVID", "Severe")


#create list of plots
plotlist1 <- list(
  older_adults_rsv_mild, older_adults_rsv_severe, older_adults_flu_mild,
  older_adults_flu_severe, older_adults_covid_mild, older_adults_covid_severe
)

plotlist2 <- list(
  older_adults_season_rsv_mild, older_adults_season_rsv_severe,
  older_adults_season_flu_mild, older_adults_season_flu_severe,
  older_adults_season_covid_mild, older_adults_season_covid_severe
)

#plot all
for(p in plotlist1) {
  
  print(p)
  title_name <- p$labels$title
  saveas <- paste0(gsub(" ", "_", title_name))
  ggsave(here("post_check", "plots", "secondary_analyses", "rates",
              paste0(saveas, ".png")), p, height = 8, width = 15)
  
}

for(p in 1:length(plotlist2)) {
  
  plots <- plotlist2[[p]]
  
  for (i in 1:length(plots)) {
    
    p1 <- plots[[i]]
    print(p1)
    title_name <- p1$labels$title
    saveas <- paste0(gsub(" ", "_", title_name))
    ggsave(here("post_check", "plots", "secondary_analyses", "rates",
                paste0(saveas, ".png")), p1, height = 8, width = 15)
    
  }
  
}
