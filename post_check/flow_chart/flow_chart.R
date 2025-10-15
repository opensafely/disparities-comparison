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
fs::dir_create(here("post_check", "flow_chart", "plots"))

##  older adults 
cohort <- "older_adults"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registed_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registed_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
# age_cut_excl <- vector("character", nrow(patients_df))
# age_excl_label <- vector("character", nrow(patients_df))
# age_cut_incl <- vector("character", nrow(patients_df))
# age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age and sex with records", 
                                "\nin pracitces using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  follow_up_excl_label[i] <- paste0("Less than three months of prior follow-up","\n(n = ", 
                                    format(patients_df[i, ]$not_registered, big.mark = ","), ")")
  
  follow_up_label[i] <- paste0("At least three months of registration during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\nafter which follow-up begins (n = ",
                               format(patients_df[i,]$registered, big.mark = ","), ")")
  
  # #use case_when to assign the correct exclusion labels
  # age_cut_excl[i] <- case_when(
  #   cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
  #   cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
  #   cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
  #   cohort == "infants" ~ "Over 2 years old \nthroughout season",
  #   cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  # )
  
  # age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
  #   patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  # #use case_when to assign the correct inclusion labels
  # age_cut_incl[i] <- case_when(
  #   cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
  #   cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
  #   cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
  #   cohort == "infants" ~ "Under 2 years old \nwithin season",
  #   cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  # )
  
  # age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
  #   patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", format(
    patients_df[i, ]$not_eligible, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", format(
    patients_df[i, ]$included, big.mark = ","), ")")
  
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
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "flow_chart", "plots"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}

##  adults
cohort <- "adults"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registed_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registed_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
# age_cut_excl <- vector("character", nrow(patients_df))
# age_excl_label <- vector("character", nrow(patients_df))
# age_cut_incl <- vector("character", nrow(patients_df))
# age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age and sex with records", 
                                "\nin pracitces using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  follow_up_excl_label[i] <- paste0("Less than three months of prior follow-up","\n(n = ", 
                                    format(patients_df[i, ]$not_registered, big.mark = ","), ")")
  
  follow_up_label[i] <- paste0("At least three months of follow-up during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\nafter which follow-up begins (n = ",
                               format(patients_df[i,]$registered, big.mark = ","), ")")
  
  # #use case_when to assign the correct exclusion labels
  # age_cut_excl[i] <- case_when(
  #   cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
  #   cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
  #   cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
  #   cohort == "infants" ~ "Over 2 years old \nthroughout season",
  #   cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  # )
  
  # age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
  #   patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  # #use case_when to assign the correct inclusion labels
  # age_cut_incl[i] <- case_when(
  #   cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
  #   cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
  #   cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
  #   cohort == "infants" ~ "Under 2 years old \nwithin season",
  #   cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  # )
  
  # age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
  #   patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", format(
    patients_df[i, ]$not_eligible, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", format(
    patients_df[i, ]$included, big.mark = ","), ")")
  
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
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "flow_chart", "plots"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}

## children and adolescents
cohort <- "children_and_adolescents"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registed_sex", "registered_imd",
"registered_no_carehome", "included", "perc_registered", "perc_registed_sex",
"perc_registered_imd", "perc_registered_no_carehome", "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
# age_cut_excl <- vector("character", nrow(patients_df))
# age_excl_label <- vector("character", nrow(patients_df))
# age_cut_incl <- vector("character", nrow(patients_df))
# age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age and sex with records", 
                                "\nin pracitces using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  follow_up_excl_label[i] <- paste0("Less than three months of prior follow-up","\n(n = ", 
                                    format(patients_df[i, ]$not_registered, big.mark = ","), ")")
  
  follow_up_label[i] <- paste0("At least three months of follow-up during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\nafter which follow-up begins (n = ",
                               format(patients_df[i,]$registered, big.mark = ","), ")")
  
  # #use case_when to assign the correct exclusion labels
  # age_cut_excl[i] <- case_when(
  #   cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
  #   cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
  #   cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
  #   cohort == "infants" ~ "Over 2 years old \nthroughout season",
  #   cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  # )
  
  # age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
  #   patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  # #use case_when to assign the correct inclusion labels
  # age_cut_incl[i] <- case_when(
  #   cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
  #   cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
  #   cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
  #   cohort == "infants" ~ "Under 2 years old \nwithin season",
  #   cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  # )
  
  # age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
  #   patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", format(
    patients_df[i, ]$not_eligible, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", format(
    patients_df[i, ]$included, big.mark = ","), ")")
  
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
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "flow_chart", "plots"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}

## infants
cohort <- "infants"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "registed_sex", "registered_imd",
"registered_no_carehome", "registered_no_riskgroup", "registered_no_immune",
"included", "perc_registered", "perc_registed_sex", "perc_registered_imd",
"perc_registered_no_carehome", "perc_registered_no_riskgroup",
"perc_registered_no_immune",  "perc_included", "subset")
patients_df <- patients_df[, not_registered := total - registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered - included, by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
# age_cut_excl <- vector("character", nrow(patients_df))
# age_excl_label <- vector("character", nrow(patients_df))
# age_cut_incl <- vector("character", nrow(patients_df))
# age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age and sex with records", 
                                "\nin pracitces using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  follow_up_excl_label[i] <- paste0("Registation does not exist at during follow-up","\n(n = ", 
                                    format(patients_df[i, ]$not_registered, big.mark = ","), ")")
  
  follow_up_label[i] <- paste0("Current registration exists during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\nafter which follow-up begins (n = ",
                               format(patients_df[i,]$registered, big.mark = ","), ")")
  
  # #use case_when to assign the correct exclusion labels
  # age_cut_excl[i] <- case_when(
  #   cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
  #   cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
  #   cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
  #   cohort == "infants" ~ "Over 2 years old \nthroughout season",
  #   cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  # )
  
  # age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
  #   patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  # #use case_when to assign the correct inclusion labels
  # age_cut_incl[i] <- case_when(
  #   cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
  #   cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
  #   cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
  #   cohort == "infants" ~ "Under 2 years old \nwithin season",
  #   cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  # )
  
  # age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
  #   patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", format(
    patients_df[i, ]$not_eligible, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", format(
    patients_df[i, ]$included, big.mark = ","), ")")
  
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
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "flow_chart", "plots"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}

## infants subgroup
cohort <- "infants_subgroup"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "output", "collated", "descriptive", 
                                paste0(cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)
names(patients_df) <- c("total", "registered", "mother_registed",
"registered_mother_registered", "registed_mother_registered_sex",
"registered_mother_registered_imd", "registered_mother_registered_no_carehome",
"registered_mother_registered_no_riskgroup", 
"registered_mother_registered_no_immune", "included", "perc_registered",
"perc_mother_registed", "perc_registered_mother_registered", 
"perc_registed_mother_registered_sex", "perc_registered_mother_registered_imd",
"perc_registered_mother_registered_no_carehome",
"perc_registered_mother_registered_no_riskgroup", 
"perc_registered_mother_registered_no_immune", "perc_included",  "subset")
patients_df <- patients_df[, not_registered := total - registered_mother_registered, by = .(subset)]
patients_df <- patients_df[, not_eligible := registered_mother_registered - included, by = .(subset)]

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
# age_cut_excl <- vector("character", nrow(patients_df))
# age_excl_label <- vector("character", nrow(patients_df))
# age_cut_incl <- vector("character", nrow(patients_df))
# age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Living population of correct age and sex with records", 
                                "\nin pracitces using TPP software on \n", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  follow_up_excl_label[i] <- paste0("Registation does not exist at during follow-up",
                                    "\n or maternal registration not available", "\n(n = ", 
                                    format(patients_df[i, ]$not_registered, big.mark = ","), ")")
  
  follow_up_label[i] <- paste0("Current registration exists during study period \n 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " to 31-08-20", substr(patients_df[i, ]$subset, start = 6, stop = 7),
                               "\n and one year of maternal registration exists prior to follow-up",
                               "\nafter which follow-up begins (n = ",
                               format(patients_df[i,]$registered, big.mark = ","), ")")
  
  # #use case_when to assign the correct exclusion labels
  # age_cut_excl[i] <- case_when(
  #   cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
  #   cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
  #   cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
  #   cohort == "infants" ~ "Over 2 years old \nthroughout season",
  #   cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  # )
  
  # age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
  #   patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  # #use case_when to assign the correct inclusion labels
  # age_cut_incl[i] <- case_when(
  #   cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
  #   cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
  #   cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
  #   cohort == "infants" ~ "Under 2 years old \nwithin season",
  #   cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  # )
  
  # age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
  #   patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria", "\n(n = ", format(
    patients_df[i, ]$not_eligible, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population", "\n(n = ", format(
    patients_df[i, ]$included, big.mark = ","), ")")
  
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
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; excluded}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> included
        follow_up -> excluded
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              included_label[i], excluded_label[i])
    )
}

#check the flow_chart list
flow_chart

#save flow charts
for (i in 1:length(flow_chart)) {
  flow_chart[[i]] %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(paste0(here::here("post_check", "flow_chart", "plots"), "/", 
             "cohort_inclusion_", cohort, "_", patients_df[i, ]$subset, ".png"))
}
