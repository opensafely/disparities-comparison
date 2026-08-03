library(tidyverse)
library(here)
library(arrow)
library(ggplot2)
library(data.table)
library(gtsummary)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

options(scipen = 999)

## create output directories ----
fs::dir_create(here("post_check", "plots", "supplemental", "flow_charts"))

fmt_n <- function(x) format(x, big.mark = ",")

##  older adults 
cohort <- "older_adults"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registered_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registered_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]
patients_df <- patients_df[, excl_sex := registered - registered_sex, by = .(subset)]
patients_df <- patients_df[, excl_imd := registered - registered_imd, by = .(subset)]
patients_df <- patients_df[, excl_care_home := registered - registered_no_carehome, by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
excl_sex_label <- vector("character", nrow(patients_df))
excl_imd_label <- vector("character", nrow(patients_df))
excl_care_home_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age (65y+)", 
                                "\nin practices using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", fmt_n(
                                patients_df[i, ]$total),")")
  
  follow_up_excl_label[i] <- paste0("Less than three months of prior follow-up","\n(n = ", 
                                    fmt_n(patients_df[i, ]$not_registered), ")")
  
  follow_up_label[i] <- paste0("At least three months of registration during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\nafter which follow-up begins (n = ",
                               fmt_n(patients_df[i,]$registered), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", fmt_n(
    patients_df[i, ]$not_eligible), ")")
  
  excl_sex_label[i] <- paste0("Missing or unknown sex", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_sex), ")")
  
  excl_imd_label[i] <- paste0("Missing IMD", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_imd), ")")
  
  excl_care_home_label[i] <- paste0("Care home resident", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_care_home), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", fmt_n(
    patients_df[i, ]$included), ")")
  
  #render graph and store in the flow_chart list
  flow_chart[[i]] <- 
    grViz(
      sprintf('
      digraph my_flowchart {
        graph[splines = ortho]
        node [fontname = Helvetica, shape = box, width = 4, height = 1]
    
        org_cohort[label = "%s"]
        follow_up_excl[label = "%s"]
        follow_up[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        excl_sex[label = "%s"]
        excl_imd[label = "%s"]
        excl_care_home[label = "%s"]
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
        {rank = same; excl_sex; excl_imd; excl_care_home}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
        excluded -> excl_sex
        excluded -> excl_imd
        excluded -> excl_care_home
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i], excl_sex_label[i],
              excl_imd_label[i], excl_care_home_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "plots", "supplemental", "flow_charts"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}

##  adults
cohort <- "adults"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registered_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registered_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]
patients_df <- patients_df[, excl_sex := registered - registered_sex, by = .(subset)]
patients_df <- patients_df[, excl_imd := registered - registered_imd, by = .(subset)]
patients_df <- patients_df[, excl_care_home := registered - registered_no_carehome, by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
excl_sex_label <- vector("character", nrow(patients_df))
excl_imd_label <- vector("character", nrow(patients_df))
excl_care_home_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age (18-64y)", 
                                "\nin practices using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", fmt_n(
                                patients_df[i, ]$total),")")
  
  follow_up_excl_label[i] <- paste0("Less than three months of prior follow-up","\n(n = ", 
                                    fmt_n(patients_df[i, ]$not_registered), ")")
  
  follow_up_label[i] <- paste0("At least three months of follow-up during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\nafter which follow-up begins (n = ",
                               fmt_n(patients_df[i,]$registered), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", fmt_n(
    patients_df[i, ]$not_eligible), ")")
  
  excl_sex_label[i] <- paste0("Missing or unknown sex", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_sex), ")")
  
  excl_imd_label[i] <- paste0("Missing IMD", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_imd), ")")
  
  excl_care_home_label[i] <- paste0("Care home resident", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_care_home), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", fmt_n(
    patients_df[i, ]$included), ")")
  
  #render graph and store in the flow_chart list
  flow_chart[[i]] <- 
    grViz(
      sprintf('
      digraph my_flowchart {
        graph[splines = ortho]
        node [fontname = Helvetica, shape = box, width = 4, height = 1]
    
        org_cohort[label = "%s"]
        follow_up_excl[label = "%s"]
        follow_up[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        excl_sex[label = "%s"]
        excl_imd[label = "%s"]
        excl_care_home[label = "%s"]
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
        {rank = same; excl_sex; excl_imd; excl_care_home}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
        excluded -> excl_sex
        excluded -> excl_imd
        excluded -> excl_care_home
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i], excl_sex_label[i],
              excl_imd_label[i], excl_care_home_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "plots", "supplemental", "flow_charts"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}

## children and adolescents
cohort <- "children_and_adolescents"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registered_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registered_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]
patients_df <- patients_df[, excl_sex := registered - registered_sex, by = .(subset)]
patients_df <- patients_df[, excl_imd := registered - registered_imd, by = .(subset)]
patients_df <- patients_df[, excl_care_home := registered - registered_no_carehome, by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
excl_sex_label <- vector("character", nrow(patients_df))
excl_imd_label <- vector("character", nrow(patients_df))
excl_care_home_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age (2-17y)", 
                                "\nin practices using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", fmt_n(
                                patients_df[i, ]$total),")")
  
  follow_up_excl_label[i] <- paste0("Less than three months of prior follow-up","\n(n = ", 
                                    fmt_n(patients_df[i, ]$not_registered), ")")
  
  follow_up_label[i] <- paste0("At least three months of follow-up during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\nafter which follow-up begins (n = ",
                               fmt_n(patients_df[i,]$registered), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", fmt_n(
    patients_df[i, ]$not_eligible), ")")
  
  excl_sex_label[i] <- paste0("Missing or unknown sex", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_sex), ")")
  
  excl_imd_label[i] <- paste0("Missing IMD", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_imd), ")")
  
  excl_care_home_label[i] <- paste0("Care home resident", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_care_home), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", fmt_n(
    patients_df[i, ]$included), ")")
  
  #render graph and store in the flow_chart list
  flow_chart[[i]] <- 
    grViz(
      sprintf('
      digraph my_flowchart {
        graph[splines = ortho]
        node [fontname = Helvetica, shape = box, width = 4, height = 1]
    
        org_cohort[label = "%s"]
        follow_up_excl[label = "%s"]
        follow_up[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        excl_sex[label = "%s"]
        excl_imd[label = "%s"]
        excl_care_home[label = "%s"]
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
        {rank = same; excl_sex; excl_imd; excl_care_home}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
        excluded -> excl_sex
        excluded -> excl_imd
        excluded -> excl_care_home
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i], excl_sex_label[i],
              excl_imd_label[i], excl_care_home_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "plots", "supplemental", "flow_charts"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}

## infants
cohort <- "infants"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registered_sex", "registered_imd",
"registered_no_carehome", "registered_no_riskgroup", "registered_no_immune",
"included", "perc_registered", "perc_registered_sex", "perc_registered_imd",
"perc_registered_no_carehome", "perc_registered_no_riskgroup",
"perc_registered_no_immune",  "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]
patients_df <- patients_df[, excl_sex := registered - registered_sex, by = .(subset)]
patients_df <- patients_df[, excl_imd := registered - registered_imd, by = .(subset)]
patients_df <- patients_df[, excl_care_home := registered - registered_no_carehome, by = .(subset)]
patients_df <- patients_df[, palivizumab := (registered - registered_no_riskgroup) +
  (registered - registered_no_immune), by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
excl_sex_label <- vector("character", nrow(patients_df))
excl_imd_label <- vector("character", nrow(patients_df))
excl_care_home_label <- vector("character", nrow(patients_df))
palivizumab_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age (under 2y)", 
                                "\nin practices using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", fmt_n(
                                patients_df[i, ]$total),")")
  
  follow_up_excl_label[i] <- paste0("Registation does not exist during follow-up","\n(n = ", 
                                    fmt_n(patients_df[i, ]$not_registered), ")")
  
  follow_up_label[i] <- paste0("Current registration exists during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\nafter which follow-up begins (n = ",
                               fmt_n(patients_df[i,]$registered), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", fmt_n(
    patients_df[i, ]$not_eligible), ")")
  
  excl_sex_label[i] <- paste0("Missing or unknown sex", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_sex), ")")
  
  excl_imd_label[i] <- paste0("Missing IMD", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_imd), ")")
  
  excl_care_home_label[i] <- paste0("Care home resident", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_care_home), ")")
  
  palivizumab_label[i] <- paste0("Excluded due to Palivizumab eligibility", "\n(n = ",
    fmt_n(patients_df[i, ]$palivizumab), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", fmt_n(
    patients_df[i, ]$included), ")")
  
  #render graph and store in the flow_chart list
  flow_chart[[i]] <- 
    grViz(
      sprintf('
      digraph my_flowchart {
        graph[splines = ortho]
        node [fontname = Helvetica, shape = box, width = 4, height = 1]
    
        org_cohort[label = "%s"]
        follow_up_excl[label = "%s"]
        follow_up[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        excl_sex[label = "%s"]
        excl_imd[label = "%s"]
        excl_care_home[label = "%s"]
        palivizumab[label = "%s"]
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
        {rank = same; excl_sex; excl_imd; excl_care_home; palivizumab}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
        excluded -> excl_sex
        excluded -> excl_imd
        excluded -> excl_care_home
        excluded -> palivizumab
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i], excl_sex_label[i],
              excl_imd_label[i], excl_care_home_label[i], palivizumab_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "plots", "supplemental", "flow_charts"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}

## infants subgroup
cohort <- "infants_subgroup"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "mother_registered",
"registered_mother_registered", "registered_mother_registered_sex",
"registered_mother_registered_imd", "registered_mother_registered_no_carehome",
"registered_mother_registered_no_riskgroup", 
"registered_mother_registered_no_immune", "included", "perc_registered",
"perc_mother_registered", "perc_registered_mother_registered", 
"perc_registered_mother_registered_sex", "perc_registered_mother_registered_imd",
"perc_registered_mother_registered_no_carehome",
"perc_registered_mother_registered_no_riskgroup", 
"perc_registered_mother_registered_no_immune", "perc_included",  "subset")
patients_df <- patients_df[, not_registered := total - registered_mother_registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered_mother_registered - included, by = .(subset)]
patients_df <- patients_df[, excl_sex := registered_mother_registered -
  registered_mother_registered_sex, by = .(subset)]
patients_df <- patients_df[, excl_imd := registered_mother_registered -
  registered_mother_registered_imd, by = .(subset)]
patients_df <- patients_df[, excl_care_home := registered_mother_registered -
  registered_mother_registered_no_carehome, by = .(subset)]
patients_df <- patients_df[, palivizumab := (registered_mother_registered -
  registered_mother_registered_no_riskgroup) + (registered_mother_registered -
  registered_mother_registered_no_immune), by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
excl_sex_label <- vector("character", nrow(patients_df))
excl_imd_label <- vector("character", nrow(patients_df))
excl_care_home_label <- vector("character", nrow(patients_df))
palivizumab_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age (under 2y) with maternal linkage", 
                                "\nin practices using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", fmt_n(
                                patients_df[i, ]$total),")")
  
  follow_up_excl_label[i] <- paste0("Registation does not exist during follow-up",
                                    "\n or maternal registration not available", "\n(n = ", 
                                    fmt_n(patients_df[i, ]$not_registered), ")")
  
  follow_up_label[i] <- paste0("Current registration exists during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\n and one year of maternal registration exists prior to follow-up",
                               "\nafter which follow-up begins (n = ",
                               fmt_n(patients_df[i,]$registered_mother_registered), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", fmt_n(
    patients_df[i, ]$not_eligible), ")")
  
  excl_sex_label[i] <- paste0("Missing or unknown sex", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_sex), ")")
  
  excl_imd_label[i] <- paste0("Missing IMD", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_imd), ")")
  
  excl_care_home_label[i] <- paste0("Care home resident", "\n(n = ", fmt_n(
    patients_df[i, ]$excl_care_home), ")")
  
  palivizumab_label[i] <- paste0("Excluded due to Palivizumab eligibility", "\n(n = ",
    fmt_n(patients_df[i, ]$palivizumab), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", fmt_n(
    patients_df[i, ]$included), ")")
  
  #render graph and store in the flow_chart list
  flow_chart[[i]] <- 
    grViz(
      sprintf('
      digraph my_flowchart {
        graph[splines = ortho]
        node [fontname = Helvetica, shape = box, width = 4, height = 1]
    
        org_cohort[label = "%s"]
        follow_up_excl[label = "%s"]
        follow_up[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        excl_sex[label = "%s"]
        excl_imd[label = "%s"]
        excl_care_home[label = "%s"]
        palivizumab[label = "%s"]
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
        {rank = same; excl_sex; excl_imd; excl_care_home; palivizumab}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
        excluded -> excl_sex
        excluded -> excl_imd
        excluded -> excl_care_home
        excluded -> palivizumab
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i], excl_sex_label[i],
              excl_imd_label[i], excl_care_home_label[i], palivizumab_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "plots", "supplemental", "flow_charts"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}
