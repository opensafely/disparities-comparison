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
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs  
                                paste0("/", cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
age_cut_excl <- vector("character", nrow(patients_df))
age_excl_label <- vector("character", nrow(patients_df))
age_cut_incl <- vector("character", nrow(patients_df))
age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Population registered with a general practice ", 
                                "\nusing TPP software on ", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  follow_up_excl_label[i] <- paste0("Less than one year of prior follow-up (n = ", 
                                    format(patients_df[i, ]$non_registered_count, big.mark = ","), ")")
  
  follow_up_label[i] <- paste0("At least one year of follow-up \nprior to 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " (n = ", format(patients_df[i,]$registered_count, big.mark = ","), ")")
  
  #use case_when to assign the correct exclusion labels
  age_cut_excl[i] <- case_when(
    cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
    cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
    cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
    cohort == "infants" ~ "Over 2 years old \nthroughout season",
    cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  )
  
  age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
    patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  #use case_when to assign the correct inclusion labels
  age_cut_incl[i] <- case_when(
    cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
    cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
    cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
    cohort == "infants" ~ "Under 2 years old \nwithin season",
    cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  )
  
  age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
    patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria (n = ", format(
    patients_df[i, ]$excluded_count, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population (n = ", format(
    patients_df[i, ]$included_count, big.mark = ","), ")")
  
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
        age_excl[label = "%s"]
        age[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; age_excl} 
        {rank = same; age; excluded}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> age_excl
        follow_up -> age
        age -> included
        age -> excluded
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              age_excl_label[i], age_label[i], included_label[i], excluded_label[i])
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
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs
                                paste0("/", cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
age_cut_excl <- vector("character", nrow(patients_df))
age_excl_label <- vector("character", nrow(patients_df))
age_cut_incl <- vector("character", nrow(patients_df))
age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Population registered with a general practice ", 
                                "\nusing TPP software on ", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  follow_up_excl_label[i] <- paste0("Less than one year of prior follow-up (n = ", 
                                    format(patients_df[i, ]$non_registered_count, big.mark = ","), ")")
  
  follow_up_label[i] <- paste0("At least one year of follow-up \nprior to 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " (n = ", format(patients_df[i,]$registered_count, big.mark = ","), ")")
  
  #use case_when to assign the correct exclusion labels
  age_cut_excl[i] <- case_when(
    cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
    cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
    cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
    cohort == "infants" ~ "Over 2 years old \nthroughout season",
    cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  )
  
  age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
    patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  #use case_when to assign the correct inclusion labels
  age_cut_incl[i] <- case_when(
    cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
    cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
    cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
    cohort == "infants" ~ "Under 2 years old \nwithin season",
    cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  )
  
  age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
    patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria (n = ", format(
    patients_df[i, ]$excluded_count, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population (n = ", format(
    patients_df[i, ]$included_count, big.mark = ","), ")")
  
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
        age_excl[label = "%s"]
        age[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; age_excl} 
        {rank = same; age; excluded}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> age_excl
        follow_up -> age
        age -> included
        age -> excluded
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              age_excl_label[i], age_label[i], included_label[i], excluded_label[i])
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
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs 
                                paste0("/", cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
follow_up_excl_label <- vector("character", nrow(patients_df))
follow_up_label <- vector("character", nrow(patients_df))
age_cut_excl <- vector("character", nrow(patients_df))
age_excl_label <- vector("character", nrow(patients_df))
age_cut_incl <- vector("character", nrow(patients_df))
age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Population registered with a general practice ", 
                                "\nusing TPP software on ", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  follow_up_excl_label[i] <- paste0("Less than one year of prior follow-up (n = ", 
                                    format(patients_df[i, ]$non_registered_count, big.mark = ","), ")")
  
  follow_up_label[i] <- paste0("At least one year of follow-up \nprior to 01-09-", 
                               substr(patients_df[i, ]$subset, start = 1, stop = 4),
                               " (n = ", format(patients_df[i,]$registered_count, big.mark = ","), ")")
  
  #use case_when to assign the correct exclusion labels
  age_cut_excl[i] <- case_when(
    cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
    cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
    cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
    cohort == "infants" ~ "Over 2 years old \nthroughout season",
    cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  )
  
  age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
    patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  #use case_when to assign the correct inclusion labels
  age_cut_incl[i] <- case_when(
    cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
    cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
    cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
    cohort == "infants" ~ "Under 2 years old \nwithin season",
    cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  )
  
  age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
    patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria (n = ", format(
    patients_df[i, ]$excluded_count, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population (n = ", format(
    patients_df[i, ]$included_count, big.mark = ","), ")")
  
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
        age_excl[label = "%s"]
        age[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        
        {rank = same; org_cohort; follow_up_excl}
        {rank = same; follow_up; age_excl} 
        {rank = same; age; excluded}
    
        org_cohort -> follow_up_excl
        org_cohort -> follow_up
        follow_up -> age_excl
        follow_up -> age
        age -> included
        age -> excluded
      }
    ', org_cohort_label[i], follow_up_excl_label[i], follow_up_label[i], 
              age_excl_label[i], age_label[i], included_label[i], excluded_label[i])
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
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs 
                                paste0("/", cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
age_cut_excl <- vector("character", nrow(patients_df))
age_excl_label <- vector("character", nrow(patients_df))
age_cut_incl <- vector("character", nrow(patients_df))
age_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Population registered with a general practice ", 
                                "\nusing TPP software on ", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  #use case_when to assign the correct exclusion labels
  age_cut_excl[i] <- case_when(
    cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
    cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
    cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
    cohort == "infants" ~ "Over 2 years old \nthroughout season",
    cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  )
  
  age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
    patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  #use case_when to assign the correct inclusion labels
  age_cut_incl[i] <- case_when(
    cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
    cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
    cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
    cohort == "infants" ~ "Under 2 years old \nwithin season",
    cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  )
  
  age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
    patients_df[i, ]$age_count, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria (n = ", format(
    patients_df[i, ]$excluded_count, big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population (n = ", format(
    patients_df[i, ]$included_count, big.mark = ","), ")")
  
  #render graph and store in the flow_chart list
  flow_chart[[i]] <- 
    grViz(
      sprintf('
      digraph my_flowchart {
        graph[splines = ortho]
        node [fontname = Helvetica, shape = box, width = 4, height = 1]
    
        org_cohort[label = "%s"]
        age_excl[label = "%s"]
        age[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        
        {rank = same; org_cohort; age_excl}
        {rank = same; age; excluded}
    
        org_cohort -> age_excl
        org_cohort -> age
        age -> included
        age -> excluded
      }
    ', org_cohort_label[i], age_excl_label[i], age_label[i], 
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
cohort <- "infants_subgroup"

#import collated flow chart data
df_input <- read_csv(here::here("post_check", "outputs", "test", #remove test folder once you have real outputs 
                                paste0("/", cohort, "_flow_chart_collated.csv")))
patients_df <- as.data.table(df_input)

#pre-allocate vectors and lists outside the loop
org_cohort_label <- vector("character", nrow(patients_df))
age_cut_excl <- vector("character", nrow(patients_df))
age_excl_label <- vector("character", nrow(patients_df))
age_cut_incl <- vector("character", nrow(patients_df))
age_label <- vector("character", nrow(patients_df))
maternal_linkage_label <- vector("character", nrow(patients_df))
maternal_linkage_exclude_label <- vector("character", nrow(patients_df))
maternal_registration_label <- vector("character", nrow(patients_df))
maternal_registration_exclude_label <- vector("character", nrow(patients_df))
excluded_label <- vector("character", nrow(patients_df))
included_label <- vector("character", nrow(patients_df))
flow_chart <- vector("list", nrow(patients_df))

#loop over each row of the dataframe
for (i in 1:nrow(patients_df)) {
  
  #construct label strings with variable values
  org_cohort_label[i] <- paste0("Population registered with a general practice ", 
                                "\nusing TPP software on ", substr(patients_df[i, ]$subset,
                                start = 1, stop = 4), "-09-01 (n = ", format(
                                patients_df[i, ]$total, big.mark = ","),")")
  
  #use case_when to assign the correct exclusion labels
  age_cut_excl[i] <- case_when(
    cohort == "older_adults" ~ "Under 65 years of age \nthroughout season",
    cohort == "adults" ~ "Not between 18 and 64 years of age \nthroughout season",
    cohort == "children_and_adolescents"~ "Not between 2 and 17 years of age \nthroughout season",
    cohort == "infants" ~ "Over 2 years old \nthroughout season",
    cohort == "infants_subgroup" ~ "Over 2 years old \nthroughout season"
  )
  
  age_excl_label[i] <- paste0(age_cut_excl[i], " (n = ", format(
    patients_df[i, ]$not_age_count, big.mark = ","), ")")
  
  #use case_when to assign the correct inclusion labels
  age_cut_incl[i] <- case_when(
    cohort == "older_adults" ~ "Aged 65 and over \nwithin season",
    cohort == "adults" ~ "Between 18 and 64 years of age \nwithin season",
    cohort == "children_and_adolescents"~ "Between 2 and 17 years of age \nwithin season",
    cohort == "infants" ~ "Under 2 years old \nwithin season",
    cohort == "infants_subgroup" ~ "Under 2 years old \nwithin season"
  )
  
  age_label[i] <- paste0(age_cut_incl[i], " (n = ", format(
    patients_df[i, ]$age_count, big.mark = ","), ")")
  
  maternal_linkage_label[i] <- paste0("Maternal linkage available (n = ", format(
    patients_df[i, ]$mother_linkage_available, big.mark = ","), ")")
  
  maternal_linkage_exclude_label[i] <- paste0("Maternal linkage not available (n = ", format(
    patients_df[i, ]$age_count - patients_df[i, ]$mother_linkage_available, big.mark = ","), ")")
  
  # maternal_registration_label[i] <- paste0("Maternal registration available ",
  #                                          "\nfor year prior to birth (n = ", format(
  #   patients_df[i, ]$mother_registered_spanning, big.mark = ","), ")")
  
  # maternal_registration_exclude_label[i] <- paste0("Maternal registration not available ",
  #                                                  "\nfor year prior to birth (n = ", format(
  #   patients_df[i, ]$mother_linkage_available - patients_df[i, ]$mother_registered_spanning, big.mark = ","), ")")
  
  excluded_label[i] <- paste0("Fits exclusion criteria (n = ", format(
    patients_df[i, ]$excluded_count - (patients_df[i, ]$age_count - 
    patients_df[i, ]$mother_linkage_available), big.mark = ","), ")")
  
  included_label[i] <- paste0("Final included in study population (n = ", format(
    patients_df[i, ]$included_count, big.mark = ","), ")")
  
  #render graph and store in the flow_chart list
  flow_chart[[i]] <- 
    grViz(
      sprintf('
      digraph my_flowchart {
        graph[splines = ortho]
        node [fontname = Helvetica, shape = box, width = 4, height = 1]
    
        org_cohort[label = "%s"]
        age_excl[label = "%s"]
        age[label = "%s"]
        maternal_linkage_excl[label = "%s"]
        maternal_linkage[label = "%s"]
        included[label = "%s"]
        excluded[label = "%s"]
        
        {rank = same; org_cohort; age_excl}
        {rank = same; age; maternal_linkage_excl}
        {rank = same; maternal_linkage; excluded}
    
        org_cohort -> age_excl
        org_cohort -> age
        age -> maternal_linkage_excl
        age -> maternal_linkage
        maternal_linkage -> included
        maternal_linkage -> excluded
        
      }
    ', org_cohort_label[i], age_excl_label[i], age_label[i],
              maternal_linkage_exclude_label[i], maternal_linkage_label[i],
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
