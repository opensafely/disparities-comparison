library(gtsummary)
library(tidyr)
library(readr)
library(dplyr)

#import functions
source(here::here("post_check", "functions", "episodes.R"))

seasons <- c("2017_18", "2018_19", "2020_21", "2023_24")

##older adults

cohort <- "older_adults"

#specific phenotype
phenotype <- "specific"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_older_adults <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_older_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_older_adults <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_older_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_older_adults <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_older_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

#sensitive phenotype
phenotype <- "sensitive"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_older_adults <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_older_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_older_adults <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_older_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_older_adults <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_older_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

# #import collated multiple episode outputs
# df_input <- read_csv(
#   here::here("post_check", "output", "collated", "descriptive",
#              paste0(cohort, "_multiple_episodes_collated.csv"))) %>% 
#   unique()

# mild_older_adults <- multiple_episodes(df_input, "mild")
# severe_older_adults <- multiple_episodes(df_input, "severe")

# cc <- scales::seq_gradient_pal(
#       "#F05039", "#1F449c", "Lab")(seq(0, 1, length.out = 8))

# #define levels
# levels <- list()

# if (cohort == "infants") {
  
#   levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
#               "Mixed", "Asian or Asian British", "Black or Black British",
#               "Other Ethnic Groups", "Unknown", "1 (most deprived)",
#               "2", "3", "4", "5 (least deprived)", "Urban Major Conurbation",
#               "Urban Minor Conurbation", "Urban City and Town",
#               "Rural Town and Fringe", "Rural Village and Dispersed")
  
# } else if (cohort == "infants_subgroup") {
  
#   levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male", "White",
#               "Mixed", "Asian or Asian British", "Black or Black British",
#               "Other Ethnic Groups", "Unknown", "1 (most deprived)",
#               "2", "3", "4", "5 (least deprived)", "Urban Major Conurbation",
#               "Urban Minor Conurbation", "Urban City and Town",
#               "Rural Town and Fringe", "Rural Village and Dispersed",
#               "Yes", "Never", "Former", "Current")
  
# } else if (cohort == "children_and_adolescents") {
  
#   levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female",
#               "Male", "White", "Mixed", "Asian or Asian British",
#               "Black or Black British", "Other Ethnic Groups",
#               "Unknown", "1 (most deprived)", "2", "3", "4",
#               "5 (least deprived)", "Multiple of the Same Generation",
#               "Living Alone", "One Other Generation",
#               "Two Other Generations", "Three Other Generations",
#               "Urban Major Conurbation", "Urban Minor Conurbation",
#               "Urban City and Town", "Rural Town and Fringe",
#               "Rural Village and Dispersed")
  
# } else if (cohort == "adults") {
  
#   levels <- c("18-39y", "40-64y", "Female", "Male", "White",
#               "Mixed", "Asian or Asian British", "Black or Black British",
#               "Other Ethnic Groups", "Unknown", "1 (most deprived)",
#               "2", "3", "4", "5 (least deprived)",
#               "Multiple of the Same Generation", "Living Alone",
#               "One Other Generation", "Two Other Generations",
#               "Three Other Generations", "Urban Major Conurbation",
#               "Urban Minor Conurbation", "Urban City and Town",
#               "Rural Town and Fringe", "Rural Village and Dispersed")
  
# } else {
  
#   levels <- c("65-74y", "75-89y", "90y+", "Female", "Male", "White",
#               "Mixed", "Asian or Asian British", "Black or Black British",
#               "Other Ethnic Groups", "Unknown", "1 (most deprived)", "2", "3",
#               "4", "5 (least deprived)", "Multiple of the Same Generation",
#               "Living Alone", "One Other Generation", "Two Other Generations",
#               "Three Other Generations", "Urban Major Conurbation",
#               "Urban Minor Conurbation", "Urban City and Town",
#               "Rural Town and Fringe", "Rural Village and Dispersed")
  
# }

# group_order <- c("Sex", "Age", "Ethnicity", "IMD",
#                  "Household Composition", "Rurality")

# plot <- mild_older_adults %>% 
#   group_by(group) %>% 
#   complete(combo, subset, characteristic, fill = list(n = 0, prop = 0)) %>% 
#   filter(combo %in% c("RSV and flu", "RSV and COVID-19", "Flu and COVID-19", 
#                       "RSV, flu, and COVID-19")) %>%
#   mutate(
#   characteristic = factor(characteristic, levels = levels,
#                           labels = str_wrap(levels, width = 12)),
#   subset = str_to_title(gsub("_", "-", subset)),
#   group = factor(group, levels = group_order, 
#                   labels = str_wrap(group_order, width = 20))
#   ) %>%
#   ggplot() +
#   geom_bar(aes(x = characteristic, y = prop, fill = subset),
#            stat = "identity", position = position_dodge(1)) +
#   facet_grid(combo~group, scales = "free_x") +
#   labs(x = "", y = "") +
#   scale_fill_manual(values = cc, name = "Season")

##adults

cohort <- "adults"

#specific phenotype
phenotype <- "specific"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_adults <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_adults <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_adults <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

#sensitive phenotype
phenotype <- "sensitive"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_adults <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_adults <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_adults <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_adults, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

##children and adolescents

cohort <- "children_and_adolescents"

#specific phenotype
phenotype <- "specific"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_children_and_adolescents <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_children_and_adolescents, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_children_and_adolescents <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_children_and_adolescents, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_children_and_adolescents <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_children_and_adolescents, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

#sensitive phenotype
phenotype <- "sensitive"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_children_and_adolescents <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_children_and_adolescents, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_children_and_adolescents <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_children_and_adolescents, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_children_and_adolescents <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_children_and_adolescents, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

##infants

cohort <- "infants"

#specific phenotype
phenotype <- "specific"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_infants <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_infants, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_infants <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_infants, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_infants <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_infants, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

#sensitive phenotype
phenotype <- "sensitive"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_infants <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_infants, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_infants <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_infants, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_infants <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_infants, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

##infants subgroup

cohort <- "infants_subgroup"

#specific phenotype
phenotype <- "specific"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_infants_subgroup <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_infants_subgroup, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_infants_subgroup <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_infants_subgroup, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_infants_subgroup <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_infants_subgroup, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))

#sensitive phenotype
phenotype <- "sensitive"

#import collated reinfection outputs
df_input <- read_csv(
  here::here("post_check", "output", "collated", "descriptive",
             paste0(cohort, "_reinfections_", phenotype, "_collated.csv")))

rsv_infants_subgroup <- reinfections(df_input, "rsv", seasons)
gt::gtsave(
  rsv_infants_subgroup, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_rsv.png")))
flu_infants_subgroup <- reinfections(df_input, "flu", seasons)
gt::gtsave(
  flu_infants_subgroup, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed", paste0(cohort, "_reinfections_", phenotype, "_flu.png")))
covid_infants_subgroup <- reinfections(df_input, "covid", seasons)
gt::gtsave(
  covid_infants_subgroup, here::here("post_check", "plots", "sensitivity_analyses",
  "condensed",  paste0(cohort, "_reinfections_", phenotype, "_covid.png")))
