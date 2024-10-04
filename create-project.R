library('tidyverse')
library('yaml')
library('here')
library('glue')

# create action functions ----

## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}

#splice <- function(...) {list_flatten(lst(...))}

## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


## generic action function ----
action <- function(
  name,
  run,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL,
  ... # other arguments / options for special action types
){

  outputs <- list(
    highly_sensitive = highly_sensitive,
    moderately_sensitive = moderately_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL

  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    needs = needs,
    outputs = outputs,
    ... = ...
  )
  action[sapply(action, is.null)] <- NULL

  action_list <- list(name = action)
  names(action_list) <- name

  action_list
}


##actions for inclusion/exclusion

action_inclusion <- function(cohort, season, dates, season_start_date, 
                             season_end_date) {
  
  splice (
    
    action(
      name = glue("generate_flow_chart_data_{cohort}_{season}"),
      run = glue("ehrql:v1 generate-dataset analysis/dataset_definition_flow_chart.py
      --output output/flow_chart/{cohort}_{dates}_flow_chart.csv
      --dummy-data-file analysis/dummydata/dummyextract_{cohort}_{dates}.arrow
      -- {cohort} {season_start_date} {season_end_date}"),
      arguments = c(cohort, season, dates, season_start_date, season_end_date),
      needs = NULL,
      highly_sensitive = lst(
        dataset = glue("output/flow_chart/{cohort}_{dates}_flow_chart.csv"),
      )
    ),
    
    action(
      name = glue("process_flow_chart_data_{cohort}_{season}"),
      run = glue("r:latest analysis/cohort_criteria.R {cohort} {season_start_date} {season_end_date}"),
      arguments = c(cohort, season, dates, season_start_date, season_end_date),
      needs = list(glue("generate_flow_chart_data_{cohort}_{season}")),
      moderately_sensitive = lst(
        cohort = glue("output/flow_chart/flow_chart_processed_{cohort}_{dates}.csv"),
      )
    )
    
  )
}

##household actions 

action_household <- function(season, dates, season_start_date, season_end_date) {
  
  splice (
    
    action(
      name = glue("extract_household_information_{season}"),
      run = glue("ehrql:v1 generate-dataset analysis/dataset_definition_household.py
      --output output/data/input_household_{dates}.arrow
      --dummy-data-file analysis/dummydata/dummyextract_household_{dates}.arrow
      -- {season_start_date} {season_end_date}"),
      arguments = c(season, dates, season_start_date, season_end_date),
      needs = NULL,
      highly_sensitive = lst(
        dataset = glue("output/data/input_household_{dates}.arrow"),
      )
    ),
    
    action(
      name = glue("process_household_information_{season}"),
      run = glue("r:latest analysis/data_processing_household.R {season_start_date} {season_end_date}"),
      arguments = c(season, dates, season_start_date, season_end_date),
      needs = list(glue("extract_household_information_{season}")),
      highly_sensitive = lst(
        arrow = glue("output/data/input_household_processed_{dates}.arrow"),
      )
    )
    
  )
}

##actions for all cohorts/seasons 

action_specified <- function(cohort, season, dates, codelist_type, 
                             investigation_type, season_start_date, 
                             season_end_date) {

  splice(
    
    action(
      name = glue("generate_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}"),
      run = glue("ehrql:v1 generate-dataset analysis/dataset_definition.py
      --output output/data/input_{cohort}_{dates}_{codelist_type}_{investigation_type}.arrow
      --dummy-data-file analysis/dummydata/dummyextract_{cohort}_{dates}.arrow
      -- {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = NULL,
      highly_sensitive = lst(
        dataset = glue("output/data/input_{cohort}_{dates}_{codelist_type}_{investigation_type}.arrow"),
      )
    ),
    
    action(
      name = glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/data_processing.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}"),
                   glue("process_household_information_{season}")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_{dates}_{codelist_type}_{investigation_type}.arrow"),
      )
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_rsv/rsv_models_master.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type,
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv"))
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/flu_models_master.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv"))
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_further_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/further_flu_models_master.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv"))
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_models_master.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type,
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv"))
    )
  
  )

}

action_specified_infants <- function(cohort, season, dates, codelist_type,
                                     investigation_type, season_start_date,
                                     season_end_date) {
  
  splice(
    
    action(
      name = glue("generate_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}"),
      run = glue("ehrql:v1 generate-dataset analysis/dataset_definition.py
      --output output/data/input_{cohort}_{dates}_{codelist_type}_{investigation_type}.arrow
      --dummy-data-file analysis/dummydata/dummyextract_{cohort}_{dates}.arrow
      -- {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = NULL,
      highly_sensitive = lst(
        dataset = glue("output/data/input_{cohort}_{dates}_{codelist_type}_{investigation_type}.arrow"),
      )
    ),
    
    action(
      name = glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/data_processing.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}"),
                   glue("process_household_information_{season}")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_{dates}_{codelist_type}_{investigation_type}.arrow"),
      )
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ses_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ses_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ses_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/flu_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ses_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_rsv/rsv_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/flu_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_ses_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_ses_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_ses_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_ses_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ses_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ses_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/flu_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ses_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_full_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_rsv/rsv_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_full_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_full_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_flu/flu_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_full_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_full_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_full_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    )
    
  )
  
}

##covid actions

action_covid <- function(cohort, season, dates, codelist_type,
                         investigation_type, season_start_date, season_end_date) {
  
  splice(
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/covid_models_master.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_further_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/further_covid_models_master.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/further_covid_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    )
    
  )
  
}

action_covid_infants <- function(cohort, season, dates, codelist_type,
                                 investigation_type, season_start_date, 
                                 season_end_date) {
  
  splice(
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ses_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/covid_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ses_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/covid_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_ses_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_ses_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ses_hh_comp_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/covid_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ses_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_full_{season}_{codelist_type}_{investigation_type}"),
      run = glue("r:latest analysis/outcome_covid/covid_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_{investigation_type}")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_full_model_outputs_{cohort}_{dates}_{codelist_type}_{investigation_type}.csv")),
    )
    
  )
  
}

##secondary analysis actions

action_secondary_rsv <- function(cohort, season, dates, codelist_type,
                                 investigation_type, season_start_date,
                                 season_end_date) {
  
  splice(
    
    action(
      name = glue("process_dataset_{cohort}_s2_specific_secondary"),
      run = glue("r:latest analysis/secondary_analyses/data_processing_sec.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_s2_specific_secondary"),
                   glue("process_household_information_s2")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_s2_specific_secondary.arrow"),
      )
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_s2_specific_secondary"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_s2_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_s2_specific_secondary.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_rsv/rsv_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv"))
    ),
    
    # action(
    #   name = glue("analyse_dataset_{cohort}_rsv_further_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/rsv_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv"))
    # ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv"))
    )#,
    
    # action(
    #   name = glue("analyse_dataset_{cohort}_overall_and_all_cause_further_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/overall_and_all_cause_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv"))
    #   
    # )
    
  )
  
}

action_secondary_flu <- function(cohort, season, dates, codelist_type,
                                 investigation_type, season_start_date,
                                 season_end_date) {
  
  splice(
    
    action(
      name = glue("process_dataset_{cohort}_s3_specific_secondary"),
      run = glue("r:latest analysis/secondary_analyses/data_processing_sec.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_s3_specific_secondary"),
                   glue("process_household_information_s3")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_s3_specific_secondary.arrow"),
      )
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_s3_specific_secondary"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_s3_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_s3_specific_secondary.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/flu_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv"))
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_further_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/further_flu_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv"))
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv"))
    )#,
    
    # action(
    #   name = glue("analyse_dataset_{cohort}_overall_and_all_cause_further_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/overall_and_all_cause_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv"))
    #   
    # )
    
  )
  
}

action_secondary_covid <- function(cohort, season, dates, codelist_type,
                         investigation_type, season_start_date, season_end_date) {
  
  splice(
    
    action(
      name = glue("process_dataset_{cohort}_s5_specific_secondary"),
      run = glue("r:latest analysis/secondary_analyses/data_processing_sec.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_s5_specific_secondary"),
                   glue("process_household_information_s5")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_s5_specific_secondary.arrow"),
      )
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_s5_specific_secondary"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_s5_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_s5_specific_secondary.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/covid_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_further_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/further_covid_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/further_covid_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    )#,
    
    # action(
    #   name = glue("analyse_dataset_{cohort}_overall_and_all_cause_further_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # )
    
  )
  
}

action_secondary_rsv_infants <- function(cohort, season, dates, codelist_type,
                                         investigation_type, season_start_date,
                                         season_end_date) {
  
  splice(
    
    action(
      name = glue("process_dataset_{cohort}_s2_specific_secondary"),
      run = glue("r:latest analysis/secondary_analyses/data_processing_sec.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_s2_specific_secondary"),
                   glue("process_household_information_s2")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_s2_specific_secondary.arrow"),
      )
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_s2_specific_secondary"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_s2_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_s2_specific_secondary.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_rsv/rsv_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_full_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_rsv/rsv_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_full_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    )#,
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_rsv_ethnicity_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_rsv_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_rsv_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_rsv_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_rsv_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_rsv_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_rsv_ethnicity_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_rsv_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_rsv_ethnicity_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_rsv_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    #
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_rsv_ses_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_rsv_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ses_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_rsv_full_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_rsv_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_full_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # )
    
  )
  
}

action_secondary_flu_infants <- function(cohort, season, dates, codelist_type,
                                         investigation_type, season_start_date,
                                         season_end_date) {
  
  splice(
    
    action(
      name = glue("process_dataset_{cohort}_s3_specific_secondary"),
      run = glue("r:latest analysis/secondary_analyses/data_processing_sec.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_s3_specific_secondary"),
                   glue("process_household_information_s3")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_s3_specific_secondary.arrow"),
      )
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_s3_specific_secondary"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_s3_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_s3_specific_secondary.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/flu_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/flu_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ses_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/flu_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_full_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_flu/flu_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_full_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    )#,
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_flu_ethnicity_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_flu/further_flu_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_flu_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_flu_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_flu/further_flu_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_flu_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_flu_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_flu/further_flu_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_flu_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_flu_ethnicity_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_flu/further_flu_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_flu_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_flu_ethnicity_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_flu/further_flu_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_flu_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_flu_ses_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_flu/further_flu_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_flu_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ses_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_flu_full_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_flu/further_flu_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_flu_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_full_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # )
    
  )
  
}

action_secondary_covid_infants <- function(cohort, season, dates, codelist_type,
                                           investigation_type, season_start_date,
                                           season_end_date) {
  
  splice(
    
    action(
      name = glue("process_dataset_{cohort}_s5_specific_secondary"),
      run = glue("r:latest analysis/secondary_analyses/data_processing_sec.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_s5_specific_secondary"),
                   glue("process_household_information_s5")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_s5_specific_secondary.arrow"),
      )
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_s5_specific_secondary"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_s5_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_s5_specific_secondary.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/covid_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/covid_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ses_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/covid_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_full_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_covid/covid_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_full_{season}_specific_secondary"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    )#,
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_covid_ethnicity_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_covid/further_covid_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_covid_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}further__covid_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_covid/further_covid_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_covid_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_covid_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_covid/further_covid_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_covid_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_covid_ethnicity_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_covid/further_covid_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_covid_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_covid_ethnicity_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_covid/further_covid_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_covid_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_covid_ses_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_covid/further_covid_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_covid_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_covid_full_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_covid/further_covid_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_covid_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}further__overall_and_all_cause_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_ses_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_ses_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ethnicity_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_ses_hh_comp_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_ses_hh_comp_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # ),
    # 
    # action(
    #   name = glue("analyse_dataset_{cohort}_further_overall_and_all_cause_full_{season}_specific_secondary"),
    #   run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_specific_secondary")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/further_overall_and_all_cause_full_model_outputs_{cohort}_{dates}_specific_secondary.csv")),
    # )
    
  )
  
}

##descriptive actions

action_descriptive <- function(cohort, season, dates, codelist_type, 
                               investigation_type, season_start_date, 
                               season_end_date) {
  
  splice(
    
    action(
      name = glue("generate_dataset_{cohort}_{season}_specific_secondary"),
      run = glue("ehrql:v1 generate-dataset analysis/dataset_definition.py
      --output output/data/input_{cohort}_{dates}_specific_secondary.arrow
      --dummy-data-file analysis/dummydata/dummyextract_{cohort}_{dates}.arrow
      -- {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = NULL,
      highly_sensitive = lst(
        dataset = glue("output/data/input_{cohort}_{dates}_specific_secondary.arrow"),
      )
    ),
    
    action(
      name = glue("process_cohort_{cohort}_{season}_specific_secondary"),
      run = glue("r:latest analysis/data_processing.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("generate_dataset_{cohort}_{season}_specific_secondary"),
                   glue("process_household_information_{season}")),
      highly_sensitive = lst(
        cohort = glue("output/data/input_processed_{cohort}_{dates}_specific_secondary.arrow"),
      )
    ),
    
    action(
      name = glue("describe_cohort_{cohort}_{season}"),
      run = glue("r:latest analysis/cohort_description.R {cohort} {season_start_date} {season_end_date} specific secondary"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_cohort_{cohort}_{season}_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/table1/table1_{cohort}_{dates}.csv")),
    )
    
  )
  
}

action_exploratory <- function(cohort, season, dates, season_start_date, 
                               season_end_date) {
  
  splice(
    
    action(
      name = glue("phenotype_sensitivity_{cohort}_{season}"),
      run = glue("r:latest analysis/exploratory_analyses/phenotype_sensitivity.R {cohort} {season_start_date} {season_end_date}"),
      arguments = c(cohort, season, dates, season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_specific_primary"),
                   glue("process_dataset_{cohort}_{season}_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/exploratory/phenotype_sensitivity_{cohort}_{dates}.csv")),
    )
    
  )

}

action_sensitivity <- function(cohort, season, dates, season_start_date,
                               season_end_date, codelist_type, 
                               investigation_type_data, investigation_type) {
  
  splice(
    
    action(
      name = glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/sensitivity_analyses/data_processing_sens.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type_data} {investigation_type}"),
      arguments = c(cohort, season, dates, season_start_date, season_end_date, 
                    codelist_type, investigation_type_data, investigation_type),
      needs = list(glue("generate_dataset_{cohort}_{season}_{codelist_type}_primary"),
                   glue("process_household_information_{season}")),
      highly_sensitive = lst(
        csv = glue("output/data/input_processed_{cohort}_{dates}_{codelist_type}_sensitivity.arrow")),
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_{dates}_{codelist_type}_sensitivity.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_rsv/rsv_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv")),
    ),
    
    # action(
    #   name = glue("analyse_dataset_{cohort}_rsv_further_{season}_{codelist_type}_sensitivity"),
    #   run = glue("r:latest analysis/outcome_rsv/further_rsv_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
    #   arguments = c(cohort, season, dates, codelist_type, investigation_type, 
    #                 season_start_date, season_end_date),
    #   needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
    #   moderately_sensitive = lst(
    #     csv = glue("output/results/models/rsv_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv")),
    # ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/flu_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_further_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/further_flu_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_model_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_further_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/further_overall_and_all_cause_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv")),
    )
    
  )
  
}

action_sensitivity_infants <- function(cohort, season, dates, season_start_date, 
                                       season_end_date, codelist_type, 
                                       investigation_type_data, investigation_type) {
  
  splice(
    
    action(
      name = glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/sensitivity_analyses/data_processing_sens.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type_data} {investigation_type}"),
      arguments = c(cohort, season, dates, season_start_date, season_end_date, 
                    codelist_type, investigation_type_data, investigation_type),
      needs = list(glue("generate_dataset_{cohort}_{season}_{codelist_type}_primary"),
                   glue("process_household_information_{season}")),
      highly_sensitive = lst(
        csv = glue("output/data/input_processed_{cohort}_{dates}_{codelist_type}_sensitivity.arrow")),
    ),
    
    action(
      name = glue("describe_dataset_{cohort}_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/report.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/rates/rates_{cohort}_{dates}_{codelist_type}_sensitivity.csv"),
      )
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ses_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ses_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ses_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/flu_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ses_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_rsv/rsv_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/flu_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_ses_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_ses_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_ses_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_ses_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/flu_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_rsv/rsv_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_ses_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_ses_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/flu_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_ses_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_ses_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_rsv_full_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_rsv/rsv_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/rsv_full_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_flu_full_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_flu/flu_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/flu_full_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_overall_and_all_cause_full_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_overall_and_all_cause/overall_and_all_cause_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/overall_and_all_cause_full_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    )
    
  )

}

action_covid_sensitivity <- function(cohort, season, dates, 
                                     season_start_date, season_end_date, 
                                     codelist_type, investigation_type) {
  
  splice(
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/covid_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_{investigation_type}/*_{cohort}_{dates}_{codelist_type}_*.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_further_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/further_covid_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_{investigation_type}/further_*_{cohort}_{dates}_{codelist_type}_*.csv")),
    )
    
  )
  
}

action_covid_sensitivity_infants <- function(cohort, season, dates, 
                                             season_start_date, season_end_date, 
                                             codelist_type, investigation_type) {
  
  splice(
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ses_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/covid_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ses_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/covid_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_ses_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_ses_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_ses_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/covid_ethnicity_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ethnicity_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_ses_hh_comp_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/covid_ses_hh_comp_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_ses_hh_comp_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    ),
    
    action(
      name = glue("analyse_dataset_{cohort}_covid_full_{season}_{codelist_type}_sensitivity"),
      run = glue("r:latest analysis/outcome_covid/covid_full_models.R {cohort} {season_start_date} {season_end_date} {codelist_type} {investigation_type}"),
      arguments = c(cohort, season, dates, codelist_type, investigation_type, 
                    season_start_date, season_end_date),
      needs = list(glue("process_dataset_{cohort}_{season}_{codelist_type}_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/results/models/covid_full_model_outputs_{cohort}_{dates}_{codelist_type}_sensitivity.csv")),
    )
    
  )
  
}

action_finalise <- function(cohort) {
  splice(
    
    action(
      name = glue("collate_flow_chart_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/flow_chart_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("process_flow_chart_data_{cohort}_s1"),
                   glue("process_flow_chart_data_{cohort}_s2"),
                   glue("process_flow_chart_data_{cohort}_s3"),
                   glue("process_flow_chart_data_{cohort}_s4"),
                   glue("process_flow_chart_data_{cohort}_s5"),
                   glue("process_flow_chart_data_{cohort}_s6"),
                   glue("process_flow_chart_data_{cohort}_s7")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_flow_chart_collated.csv"))
    ),
    
    action(
      name = glue("collate_table1_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/table1_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_cohort_{cohort}_s1"),
                   glue("describe_cohort_{cohort}_s2"),
                   glue("describe_cohort_{cohort}_s3"),
                   glue("describe_cohort_{cohort}_s4"),
                   glue("describe_cohort_{cohort}_s5"),
                   glue("describe_cohort_{cohort}_s6"),
                   glue("describe_cohort_{cohort}_s7")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_table1_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_specific_primary_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_specific_primary_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s1_specific_primary"),
                   glue("describe_dataset_{cohort}_s2_specific_primary"),
                   glue("describe_dataset_{cohort}_s3_specific_primary"),
                   glue("describe_dataset_{cohort}_s4_specific_primary"),
                   glue("describe_dataset_{cohort}_s5_specific_primary"),
                   glue("describe_dataset_{cohort}_s6_specific_primary"),
                   glue("describe_dataset_{cohort}_s7_specific_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_specific_primary_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_sensitive_primary_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_sensitive_primary_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s1_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s2_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s3_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s4_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s5_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s6_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_sensitive_primary_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_specific_secondary_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_specific_secondary_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s2_specific_secondary"),
                   glue("describe_dataset_{cohort}_s3_specific_secondary"),
                   glue("describe_dataset_{cohort}_s5_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_specific_secondary_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_specific_sensitivity_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_specific_sensitivity_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s1_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s2_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s3_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s4_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s5_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s6_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s7_specific_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_specific_sensitivity_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_sensitive_sensitivity_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_sensitive_sensitivity_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s1_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s2_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s3_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s4_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s5_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s6_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_sensitive_sensitivity_collated.csv"))
    ),
    
    action(
      name = glue("collate_rsv_model_outputs_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/rsv_model_outputs_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_rsv_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_rsv_model_outputs_collated.csv"))
      
    ),
    
    action(
      name = glue("collate_flu_model_outputs_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/flu_model_outputs_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_flu_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_flu_model_outputs_collated.csv"))
      
    ),
    
    action(
      name = glue("collate_covid_model_outputs_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/covid_model_outputs_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_covid_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_covid_model_outputs_collated.csv"))
      
    ),
    
    action(
      name = glue("collate_overall_and_all_cause_outputs_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/overall_and_all_cause_model_outputs_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_overall_and_all_cause_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_overall_and_all_cause_model_outputs_collated.csv"))
    ),
    
    action(
      name = glue("collate_rsv_outputs_tables_{cohort}_secondary"),
      run = glue("r:latest analysis/collation_code/rsv_outputs_table_collation_secondary.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_rsv_s2_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/secondary/{cohort}_rsv_model_outputs_collated_secondary.csv"))
    ),
    
    action(
      name = glue("collate_flu_outputs_tables_{cohort}_secondary"),
      run = glue("r:latest analysis/collation_code/flu_outputs_table_collation_secondary.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_flu_s3_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/secondary/{cohort}_flu_model_outputs_collated_secondary.csv"))
    ),
    
    action(
      name = glue("collate_covid_outputs_tables_{cohort}_secondary"),
      run = glue("r:latest analysis/collation_code/covid_outputs_table_collation_secondary.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_covid_s5_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/secondary/{cohort}_covid_model_outputs_collated_secondary.csv"))
    ),
    
    action(
      name = glue("collate_overall_and_all_cause_outputs_tables_{cohort}_secondary"),
      run = glue("r:latest analysis/collation_code/overall_and_all_cause_outputs_table_collation_secondary.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_overall_and_all_cause_s5_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/secondary/{cohort}_overall_and_all_cause_model_outputs_collated_secondary.csv"))
    ),
    
    action(
      name = glue("collate_phenotype_sensitivity_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/phenotype_sensitivity_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("phenotype_sensitivity_{cohort}_s1"),
                   glue("phenotype_sensitivity_{cohort}_s2"),
                   glue("phenotype_sensitivity_{cohort}_s3"),
                   glue("phenotype_sensitivity_{cohort}_s4"),
                   glue("phenotype_sensitivity_{cohort}_s5"),
                   glue("phenotype_sensitivity_{cohort}_s6"),
                   glue("phenotype_sensitivity_{cohort}_s7")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_phenotype_sensitivity_collated.csv"))
    ),
    
    action(
      name = glue("collate_rsv_outputs_tables_{cohort}_sensitivity"),
      run = glue("r:latest analysis/collation_code/rsv_outputs_table_collation_sensitivity.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_rsv_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/sensitivity/{cohort}_rsv_model_outputs_collated_sensitivity.csv"))
    ),
    
    action(
      name = glue("collate_flu_outputs_tables_{cohort}_sensitivity"),
      run = glue("r:latest analysis/collation_code/flu_outputs_table_collation_sensitivity.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_flu_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/sensitivity/{cohort}_flu_model_outputs_collated_sensitivity.csv"))
    ),
    
    action(
      name = glue("collate_covid_outputs_tables_{cohort}_sensitivity"),
      run = glue("r:latest analysis/collation_code/covid_outputs_table_collation_sensitivity.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_covid_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/sensitivity/{cohort}_covid_model_outputs_collated_sensitivity.csv"))
    ),
    
    action(
      name = glue("collate_overall_and_all_cause_outputs_tables_{cohort}_sensitivity"),
      run = glue("r:latest analysis/collation_code/overall_and_all_cause_outputs_table_collation_sensitivity.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_overall_and_all_cause_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/sensitivity/{cohort}_overall_and_all_cause_model_outputs_collated_sensitivity.csv"))
    )
    
  )
  
}

action_finalise_infants <- function(cohort) {
  
  splice(
    
    action(
      name = glue("collate_flow_chart_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/flow_chart_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("process_flow_chart_data_{cohort}_s1"),
                   glue("process_flow_chart_data_{cohort}_s2"),
                   glue("process_flow_chart_data_{cohort}_s3"),
                   glue("process_flow_chart_data_{cohort}_s4"),
                   glue("process_flow_chart_data_{cohort}_s5"),
                   glue("process_flow_chart_data_{cohort}_s6"),
                   glue("process_flow_chart_data_{cohort}_s7")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_flow_chart_collated.csv"))
    ),
    
    action(
      name = glue("collate_table1_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/table1_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_cohort_{cohort}_s1"),
                   glue("describe_cohort_{cohort}_s2"),
                   glue("describe_cohort_{cohort}_s3"),
                   glue("describe_cohort_{cohort}_s4"),
                   glue("describe_cohort_{cohort}_s5"),
                   glue("describe_cohort_{cohort}_s6"),
                   glue("describe_cohort_{cohort}_s7")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_table1_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_specific_primary_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_specific_primary_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s1_specific_primary"),
                   glue("describe_dataset_{cohort}_s2_specific_primary"),
                   glue("describe_dataset_{cohort}_s3_specific_primary"),
                   glue("describe_dataset_{cohort}_s4_specific_primary"),
                   glue("describe_dataset_{cohort}_s5_specific_primary"),
                   glue("describe_dataset_{cohort}_s6_specific_primary"),
                   glue("describe_dataset_{cohort}_s7_specific_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_specific_primary_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_sensitive_primary_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_sensitive_primary_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s1_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s2_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s3_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s4_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s5_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s6_sensitive_primary"),
                   glue("describe_dataset_{cohort}_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_sensitive_primary_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_specific_secondary_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_specific_secondary_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s2_specific_secondary"),
                   glue("describe_dataset_{cohort}_s3_specific_secondary"),
                   glue("describe_dataset_{cohort}_s5_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_specific_secondary_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_specific_sensitivity_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_specific_sensitivity_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s1_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s2_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s3_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s4_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s5_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s6_specific_sensitivity"),
                   glue("describe_dataset_{cohort}_s7_specific_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_specific_sensitivity_collated.csv"))
    ),
    
    action(
      name = glue("collate_rates_tables_sensitive_sensitivity_{cohort}"),
      run = glue("r:latest analysis/collation_code/rates_table_sensitive_sensitivity_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("describe_dataset_{cohort}_s1_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s2_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s3_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s4_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s5_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s6_sensitive_sensitivity"),
                   glue("describe_dataset_{cohort}_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_rates_sensitive_sensitivity_collated.csv"))
    ),
    
    action(
      name = glue("collate_rsv_model_outputs_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/rsv_model_outputs_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_rsv_ethnicity_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_rsv_model_outputs_collated.csv"))
    ),
    
    action(
      name = glue("collate_flu_model_outputs_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/flu_model_outputs_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_flu_ethnicity_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_flu_full_s7_sensitive_primary")),
      moderately_sensitive = lst(
        glue("output/collated/analytic/{cohort}_flu_model_outputs_collated.csv"))
    ),
    
    action(
      name = glue("collate_covid_model_outputs_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/covid_model_outputs_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_covid_ethnicity_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_full_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_full_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_full_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_full_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_full_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_full_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_full_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_covid_full_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_covid_model_outputs_collated.csv"))
    ),
    
    action(
      name = glue("collate_overall_and_all_cause_model_outputs_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/overall_and_all_cause_model_outputs_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s1_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s2_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s3_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s4_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s5_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s6_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s7_specific_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s7_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s1_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s2_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s3_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s4_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s5_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s6_sensitive_primary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s7_sensitive_primary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_overall_and_all_cause_model_outputs_collated.csv"))
    ),
    
    action(
      name = glue("collate_rsv_model_outputs_tables_{cohort}_secondary"),
      run = glue("r:latest analysis/collation_code/rsv_model_outputs_table_collation_secondary.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_rsv_ethnicity_s2_specific_secondary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s2_specific_secondary"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s2_specific_secondary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s2_specific_secondary"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s2_specific_secondary"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s2_specific_secondary"),
                   glue("analyse_dataset_{cohort}_rsv_full_s2_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_rsv_model_outputs_secondary_collated.csv"))
    ),
    
    action(
      name = glue("collate_flu_model_outputs_tables_{cohort}_secondary"),
      run = glue("r:latest analysis/collation_code/flu_model_outputs_table_collation_secondary.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_flu_ethnicity_s3_specific_secondary"),
                   glue("analyse_dataset_{cohort}_flu_ses_s3_specific_secondary"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s3_specific_secondary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s3_specific_secondary"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s3_specific_secondary"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s3_specific_secondary"),
                   glue("analyse_dataset_{cohort}_flu_full_s3_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_flu_model_outputs_secondary_collated.csv"))
    ),
    
    action(
      name = glue("collate_covid_model_outputs_tables_{cohort}_secondary"),
      run = glue("r:latest analysis/collation_code/covid_model_outputs_table_collation_secondary.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_covid_ethnicity_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_covid_ses_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_covid_full_s5_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_covid_model_outputs_secondary_collated.csv"))
    ),
    
    action(
      name = glue("collate_overall_and_all_cause_model_outputs_tables_{cohort}_secondary"),
      run = glue("r:latest analysis/collation_code/overall_and_all_cause_model_outputs_table_collation_secondary.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s5_specific_secondary"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s5_specific_secondary")),
      moderately_sensitive = lst(
        csv = glue("output/collated/analytic/{cohort}_overall_and_all_cause_model_outputs_secondary_collated.csv"))
    ),
    
    action(
      name = glue("collate_phenotype_sensitivity_tables_{cohort}"),
      run = glue("r:latest analysis/collation_code/phenotype_sensitivity_table_collation.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("phenotype_sensitivity_{cohort}_s1"),
                   glue("phenotype_sensitivity_{cohort}_s2"),
                   glue("phenotype_sensitivity_{cohort}_s3"),
                   glue("phenotype_sensitivity_{cohort}_s4"),
                   glue("phenotype_sensitivity_{cohort}_s5"),
                   glue("phenotype_sensitivity_{cohort}_s6"),
                   glue("phenotype_sensitivity_{cohort}_s7")),
      moderately_sensitive = lst(
        csv = glue("output/collated/descriptive/{cohort}_phenotype_sensitivity_collated.csv"))
    ),
    
    action(
      name = glue("collate_rsv_model_outputs_tables_{cohort}_sensitivity"),
      run = glue("r:latest analysis/collation_code/rsv_model_outputs_table_collation_sensitivity.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_rsv_ethnicity_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_ses_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ethnicity_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_ses_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_rsv_full_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/sensitivity/{cohort}_rsv_model_outputs_collated_sensitivity.csv"))
    ),

    action(
      name = glue("collate_flu_model_outputs_tables_{cohort}_sensitivity"),
      run = glue("r:latest analysis/collation_code/flu_model_outputs_table_collation_sensitivity.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_flu_ethnicity_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_ses_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ethnicity_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_ses_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_flu_full_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        glue("output/collated/sensitivity/{cohort}_flu_model_outputs_collated_sensitivity.csv"))
    ),

    action(
      name = glue("collate_covid_model_outputs_tables_{cohort}_sensitivity"),
      run = glue("r:latest analysis/collation_code/covid_model_outputs_table_collation_sensitivity.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_covid_ethnicity_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_full_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_full_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_full_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_full_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_ses_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ethnicity_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_ses_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_full_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_full_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_full_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_covid_full_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/sensitivity/{cohort}_covid_model_outputs_collated_sensitivity.csv"))
    ),

    action(
      name = glue("collate_overall_and_all_cause_model_outputs_tables_{cohort}_sensitivity"),
      run = glue("r:latest analysis/collation_code/overall_and_all_cause_model_outputs_table_collation_sensitivity.R {cohort}"),
      arguments = c(cohort),
      needs = list(glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s1_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s2_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s3_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s4_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s5_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s6_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s7_specific_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_ses_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ethnicity_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_ses_hh_comp_s7_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s1_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s2_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s3_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s4_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s5_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s6_sensitive_sensitivity"),
                   glue("analyse_dataset_{cohort}_overall_and_all_cause_full_s7_sensitive_sensitivity")),
      moderately_sensitive = lst(
        csv = glue("output/collated/sensitivity/{cohort}_overall_and_all_cause_model_outputs_collated_sensitivity.csv"))
    )
    
  )
  
}

# specify project ----

## defaults ----
defaults_list <- lst(
  version = "3.0",
  expectations= lst(population_size=100000L)
)

## actions ----
actions_list <- splice(

  comment("# # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create-project.R",
          "Edit and run create-project.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # #"
          ),


  comment("# # # # # # # # # # # # # # # # # # #", "Pre-server scripts", "# # # # # # # # # # # # # # # # # # #"),

  # doesn't currently work as "project.yaml is not an allowed file type"
  # action(
  #   name = "checkyaml",
  #   run = "r:latest create-project.R",
  #   moderately_sensitive = lst(
  #     project = "project.yaml"
  #   )
  # ),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Inclusion/Exclusion Flow Chart Actions: Older Adults", "# # # # # # # # # # # # # # # # # # #"),

  action_inclusion("older_adults", "s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_inclusion("older_adults", "s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_inclusion("older_adults", "s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_inclusion("older_adults", "s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_inclusion("older_adults", "s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_inclusion("older_adults", "s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_inclusion("older_adults", "s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Inclusion/Exclusion Flow Chart Actions: Adults", "# # # # # # # # # # # # # # # # # # #"),

  action_inclusion("adults", "s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_inclusion("adults", "s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_inclusion("adults", "s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_inclusion("adults", "s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_inclusion("adults", "s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_inclusion("adults", "s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_inclusion("adults", "s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Inclusion/Exclusion Flow Chart Actions: Children and Adolescents", "# # # # # # # # # # # # # # # # # # #"),
  
  action_inclusion("children_and_adolescents", "s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_inclusion("children_and_adolescents", "s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_inclusion("children_and_adolescents", "s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_inclusion("children_and_adolescents", "s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_inclusion("children_and_adolescents", "s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_inclusion("children_and_adolescents", "s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_inclusion("children_and_adolescents", "s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Inclusion/Exclusion Flow Chart Actions: Infants", "# # # # # # # # # # # # # # # # # # #"),
  
  action_inclusion("infants", "s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_inclusion("infants", "s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_inclusion("infants", "s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_inclusion("infants", "s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_inclusion("infants", "s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_inclusion("infants", "s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_inclusion("infants", "s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Household Information Actions", "# # # # # # # # # # # # # # # # # # #"),
  
  action_household("s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_household("s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_household("s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_household("s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_household("s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_household("s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_household("s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "PRIMARY ANALYSES", "# # # # # # # # # # # # # # # # # # #"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Older Adults, Codelist Type: Specific, Investigation Type: Primary", "# # # # # # # # # # # # # # # # # # #"),

  action_specified("older_adults", "s1", "2016_2017", "specific", "primary", "season1_start_date", "season1_end_date"),
  action_specified("older_adults", "s2", "2017_2018", "specific", "primary", "season2_start_date", "season2_end_date"),
  action_specified("older_adults", "s3", "2018_2019", "specific", "primary", "season3_start_date", "season3_end_date"),
  action_specified("older_adults", "s4", "2019_2020", "specific", "primary", "season4_start_date", "season4_end_date"),
  action_covid("older_adults", "s4", "2019_2020", "specific", "primary", "season4_start_date", "season4_end_date"),
  action_specified("older_adults", "s5", "2020_2021", "specific", "primary", "season5_start_date", "season5_end_date"),
  action_covid("older_adults", "s5", "2020_2021", "specific", "primary", "season5_start_date", "season5_end_date"),
  action_specified("older_adults", "s6", "2021_2022", "specific", "primary", "season6_start_date", "season6_end_date"),
  action_covid("older_adults", "s6", "2021_2022", "specific", "primary", "season6_start_date", "season6_end_date"),
  action_specified("older_adults", "s7", "2022_2023", "specific", "primary", "season7_start_date", "season7_end_date"),
  action_covid("older_adults", "s7", "2022_2023", "specific", "primary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Adults, Season: Codelist Type: Specific, Investigation Type: Primary", "# # # # # # # # # # # # # # # # # # #"),

  action_specified("adults", "s1", "2016_2017", "specific", "primary", "season1_start_date", "season1_end_date"),
  action_specified("adults", "s2", "2017_2018", "specific", "primary", "season2_start_date", "season2_end_date"),
  action_specified("adults", "s3", "2018_2019", "specific", "primary", "season3_start_date", "season3_end_date"),
  action_specified("adults", "s4", "2019_2020", "specific", "primary", "season4_start_date", "season4_end_date"),
  action_covid("adults", "s4", "2019_2020", "specific", "primary", "season4_start_date", "season4_end_date"),
  action_specified("adults", "s5", "2020_2021", "specific", "primary", "season5_start_date", "season5_end_date"),
  action_covid("adults", "s5", "2020_2021", "specific", "primary", "season5_start_date", "season5_end_date"),
  action_specified("adults", "s6", "2021_2022", "specific", "primary", "season6_start_date", "season6_end_date"),
  action_covid("adults", "s6", "2021_2022", "specific", "primary", "season6_start_date", "season6_end_date"),
  action_specified("adults", "s7", "2022_2023", "specific", "primary", "season7_start_date", "season7_end_date"),
  action_covid("adults", "s7", "2022_2023", "specific", "primary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Children and Adolescents, Codelist Type: Specific, Investigation Type: Primary", "# # # # # # # # # # # # # # # # # # #"),
  
  action_specified("children_and_adolescents", "s1", "2016_2017", "specific", "primary", "season1_start_date", "season1_end_date"),
  action_specified("children_and_adolescents", "s2", "2017_2018", "specific", "primary", "season2_start_date", "season2_end_date"),
  action_specified("children_and_adolescents", "s3", "2018_2019", "specific", "primary", "season3_start_date", "season3_end_date"),
  action_specified("children_and_adolescents", "s4", "2019_2020", "specific", "primary", "season4_start_date", "season4_end_date"),
  action_covid("children_and_adolescents", "s4", "2019_2020", "specific", "primary", "season4_start_date", "season4_end_date"),
  action_specified("children_and_adolescents", "s5", "2020_2021", "specific", "primary", "season5_start_date", "season5_end_date"),
  action_covid("children_and_adolescents", "s5", "2020_2021", "specific", "primary", "season5_start_date", "season5_end_date"),
  action_specified("children_and_adolescents", "s6", "2021_2022", "specific", "primary", "season6_start_date", "season6_end_date"),
  action_covid("children_and_adolescents", "s6", "2021_2022", "specific", "primary", "season6_start_date", "season6_end_date"),
  action_specified("children_and_adolescents", "s7", "2022_2023", "specific", "primary", "season7_start_date", "season7_end_date"),
  action_covid("children_and_adolescents", "s7", "2022_2023", "specific", "primary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Infants, Codelist Type: Specific, Investigation Type: Primary", "# # # # # # # # # # # # # # # # # # #"),
  
  action_specified_infants("infants", "s1", "2016_2017", "specific", "primary", "season1_start_date", "season1_end_date"),
  action_specified_infants("infants", "s2", "2017_2018", "specific", "primary", "season2_start_date", "season2_end_date"),
  action_specified_infants("infants", "s3", "2018_2019", "specific", "primary", "season3_start_date", "season3_end_date"),
  action_specified_infants("infants", "s4", "2019_2020", "specific", "primary", "season4_start_date", "season4_end_date"),
  action_covid_infants("infants", "s4", "2019_2020", "specific", "primary", "season4_start_date", "season4_end_date"),
  action_specified_infants("infants", "s5", "2020_2021", "specific", "primary", "season5_start_date", "season5_end_date"),
  action_covid_infants("infants", "s5", "2020_2021", "specific", "primary", "season5_start_date", "season5_end_date"),
  action_specified_infants("infants", "s6", "2021_2022", "specific", "primary", "season6_start_date", "season6_end_date"),
  action_covid_infants("infants", "s6", "2021_2022", "specific", "primary", "season6_start_date", "season6_end_date"),
  action_specified_infants("infants", "s7", "2022_2023", "specific", "primary", "season7_start_date", "season7_end_date"),
  action_covid_infants("infants", "s7", "2022_2023", "specific", "primary", "season7_start_date", "season7_end_date"),

  comment("# # # # # # # # # # # # # # # # # # #", "SENSITIVE PHENOTYPES", "# # # # # # # # # # # # # # # # # # #"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Older Adults, Codelist Type: Sensitive, Investigation Type: Primary", "# # # # # # # # # # # # # # # # # # #"),
  
  action_specified("older_adults", "s1", "2016_2017", "sensitive", "primary", "season1_start_date", "season1_end_date"),
  action_specified("older_adults", "s2", "2017_2018", "sensitive", "primary", "season2_start_date", "season2_end_date"),
  action_specified("older_adults", "s3", "2018_2019", "sensitive", "primary", "season3_start_date", "season3_end_date"),
  action_specified("older_adults", "s4", "2019_2020", "sensitive", "primary", "season4_start_date", "season4_end_date"),
  action_covid("older_adults", "s4", "2019_2020", "sensitive", "primary", "season4_start_date", "season4_end_date"),
  action_specified("older_adults", "s5", "2020_2021", "sensitive", "primary", "season5_start_date", "season5_end_date"),
  action_covid("older_adults", "s5", "2020_2021", "sensitive", "primary", "season5_start_date", "season5_end_date"),
  action_specified("older_adults", "s6", "2021_2022", "sensitive", "primary", "season6_start_date", "season6_end_date"),
  action_covid("older_adults", "s6", "2021_2022", "sensitive", "primary", "season6_start_date", "season6_end_date"),
  action_specified("older_adults", "s7", "2022_2023", "sensitive", "primary", "season7_start_date", "season7_end_date"),
  action_covid("older_adults", "s7", "2022_2023", "sensitive", "primary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Adults, Codelist Type: Sensitive, Investigation Type: Primary", "# # # # # # # # # # # # # # # # # # #"),
  
  action_specified("adults", "s1", "2016_2017", "sensitive", "primary", "season1_start_date", "season1_end_date"),
  action_specified("adults", "s2", "2017_2018", "sensitive", "primary", "season2_start_date", "season2_end_date"),
  action_specified("adults", "s3", "2018_2019", "sensitive", "primary", "season3_start_date", "season3_end_date"),
  action_specified("adults", "s4", "2019_2020", "sensitive", "primary", "season4_start_date", "season4_end_date"),
  action_covid("adults", "s4", "2019_2020", "sensitive", "primary", "season4_start_date", "season4_end_date"),
  action_specified("adults", "s5", "2020_2021", "sensitive", "primary", "season5_start_date", "season5_end_date"),
  action_covid("adults", "s5", "2020_2021", "sensitive", "primary", "season5_start_date", "season5_end_date"),
  action_specified("adults", "s6", "2021_2022", "sensitive", "primary", "season6_start_date", "season6_end_date"),
  action_covid("adults", "s6", "2021_2022", "sensitive", "primary", "season6_start_date", "season6_end_date"),
  action_specified("adults", "s7", "2022_2023", "sensitive", "primary", "season7_start_date", "season7_end_date"),
  action_covid("adults", "s7", "2022_2023", "sensitive", "primary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Children and Adolescents, Codelist Type: Sensitive, Investigation Type: Primary", "# # # # # # # # # # # # # # # # # # #"),
  
  action_specified("children_and_adolescents", "s1", "2016_2017", "sensitive", "primary", "season1_start_date", "season1_end_date"),
  action_specified("children_and_adolescents", "s2", "2017_2018", "sensitive", "primary", "season2_start_date", "season2_end_date"),
  action_specified("children_and_adolescents", "s3", "2018_2019", "sensitive", "primary", "season3_start_date", "season3_end_date"),
  action_specified("children_and_adolescents", "s4", "2019_2020", "sensitive", "primary", "season4_start_date", "season4_end_date"),
  action_covid("children_and_adolescents", "s4", "2019_2020", "sensitive", "primary", "season4_start_date", "season4_end_date"),
  action_specified("children_and_adolescents", "s5", "2020_2021", "sensitive", "primary", "season5_start_date", "season5_end_date"),
  action_covid("children_and_adolescents", "s5", "2020_2021", "sensitive", "primary", "season5_start_date", "season5_end_date"),
  action_specified("children_and_adolescents", "s6", "2021_2022", "sensitive", "primary", "season6_start_date", "season6_end_date"),
  action_covid("children_and_adolescents", "s6", "2021_2022", "sensitive", "primary", "season6_start_date", "season6_end_date"),
  action_specified("children_and_adolescents", "s7", "2022_2023", "sensitive", "primary", "season7_start_date", "season7_end_date"),
  action_covid("children_and_adolescents", "s7", "2022_2023", "sensitive", "primary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Infants, Codelist Type: Sensitive, Investigation Type: Primary", "# # # # # # # # # # # # # # # # # # #"),
  
  action_specified_infants("infants", "s1", "2016_2017", "sensitive", "primary", "season1_start_date", "season1_end_date"),
  action_specified_infants("infants", "s2", "2017_2018", "sensitive", "primary", "season2_start_date", "season2_end_date"),
  action_specified_infants("infants", "s3", "2018_2019", "sensitive", "primary", "season3_start_date", "season3_end_date"),
  action_specified_infants("infants", "s4", "2019_2020", "sensitive", "primary", "season4_start_date", "season4_end_date"),
  action_covid_infants("infants", "s4", "2019_2020", "sensitive", "primary", "season4_start_date", "season4_end_date"),
  action_specified_infants("infants", "s5", "2020_2021", "sensitive", "primary", "season5_start_date", "season5_end_date"),
  action_covid_infants("infants", "s5", "2020_2021", "sensitive", "primary", "season5_start_date", "season5_end_date"),
  action_specified_infants("infants", "s6", "2021_2022", "sensitive", "primary", "season6_start_date", "season6_end_date"),
  action_covid_infants("infants", "s6", "2021_2022", "sensitive", "primary", "season6_start_date", "season6_end_date"),
  action_specified_infants("infants", "s7", "2022_2023", "sensitive", "primary", "season7_start_date", "season7_end_date"),
  action_covid_infants("infants", "s7", "2022_2023", "sensitive", "primary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "DESCRIPTIVE ANALYSES", "# # # # # # # # # # # # # # # # # # #"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Older Adults", "# # # # # # # # # # # # # # # # # # #"),
  
  action_descriptive("older_adults", "s1", "2016_2017", "specific", "secondary", "season1_start_date", "season1_end_date"),
  action_descriptive("older_adults", "s2", "2017_2018", "specific", "secondary", "season2_start_date", "season2_end_date"),
  action_descriptive("older_adults", "s3", "2018_2019", "specific", "secondary", "season3_start_date", "season3_end_date"),
  action_descriptive("older_adults", "s4", "2019_2020", "specific", "secondary", "season4_start_date", "season4_end_date"),
  action_descriptive("older_adults", "s5", "2020_2021", "specific", "secondary", "season5_start_date", "season5_end_date"),
  action_descriptive("older_adults", "s6", "2021_2022", "specific", "secondary", "season6_start_date", "season6_end_date"),
  action_descriptive("older_adults", "s7", "2022_2023", "specific", "secondary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Adults", "# # # # # # # # # # # # # # # # # # #"),
  
  action_descriptive("adults", "s1", "2016_2017", "specific", "secondary", "season1_start_date", "season1_end_date"),
  action_descriptive("adults", "s2", "2017_2018", "specific", "secondary", "season2_start_date", "season2_end_date"),
  action_descriptive("adults", "s3", "2018_2019", "specific", "secondary", "season3_start_date", "season3_end_date"),
  action_descriptive("adults", "s4", "2019_2020", "specific", "secondary", "season4_start_date", "season4_end_date"),
  action_descriptive("adults", "s5", "2020_2021", "specific", "secondary", "season5_start_date", "season5_end_date"),
  action_descriptive("adults", "s6", "2021_2022", "specific", "secondary", "season6_start_date", "season6_end_date"),
  action_descriptive("adults", "s7", "2022_2023", "specific", "secondary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Children and Adolescents", "# # # # # # # # # # # # # # # # # # #"),
  
  action_descriptive("children_and_adolescents", "s1", "2016_2017", "specific", "secondary", "season1_start_date", "season1_end_date"),
  action_descriptive("children_and_adolescents", "s2", "2017_2018", "specific", "secondary", "season2_start_date", "season2_end_date"),
  action_descriptive("children_and_adolescents", "s3", "2018_2019", "specific", "secondary", "season3_start_date", "season3_end_date"),
  action_descriptive("children_and_adolescents", "s4", "2019_2020", "specific", "secondary", "season4_start_date", "season4_end_date"),
  action_descriptive("children_and_adolescents", "s5", "2020_2021", "specific", "secondary", "season5_start_date", "season5_end_date"),
  action_descriptive("children_and_adolescents", "s6", "2021_2022", "specific", "secondary", "season6_start_date", "season6_end_date"),
  action_descriptive("children_and_adolescents", "s7", "2022_2023", "specific", "secondary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Infants", "# # # # # # # # # # # # # # # # # # #"),
  
  action_descriptive("infants", "s1", "2016_2017", "specific", "secondary", "season1_start_date", "season1_end_date"),
  action_descriptive("infants", "s2", "2017_2018", "specific", "secondary", "season2_start_date", "season2_end_date"),
  action_descriptive("infants", "s3", "2018_2019", "specific", "secondary", "season3_start_date", "season3_end_date"),
  action_descriptive("infants", "s4", "2019_2020", "specific", "secondary", "season4_start_date", "season4_end_date"),
  action_descriptive("infants", "s5", "2020_2021", "specific", "secondary", "season5_start_date", "season5_end_date"),
  action_descriptive("infants", "s6", "2021_2022", "specific", "secondary", "season6_start_date", "season6_end_date"),
  action_descriptive("infants", "s7", "2022_2023", "specific", "secondary", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "SECONDARY ANALYSES", "# # # # # # # # # # # # # # # # # # #"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Older Adults, Codelist Type: Specific, Investigation Type: Secondary", "# # # # # # # # # # # # # # # # # # #"),

  action_secondary_rsv("older_adults", "s2", "2017_2018", "specific", "secondary", "season2_start_date", "season2_end_date"),
  action_secondary_flu("older_adults", "s3", "2018_2019", "specific", "secondary", "season3_start_date", "season3_end_date"),
  action_secondary_covid("older_adults", "s5", "2020_2021", "specific", "secondary", "season5_start_date", "season5_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Adults, Codelist Type: Specific, Investigation Type: Secondary", "# # # # # # # # # # # # # # # # # # #"),

  action_secondary_rsv("adults", "s2", "2017_2018", "specific", "secondary", "season2_start_date", "season2_end_date"),
  action_secondary_flu("adults", "s3", "2018_2019", "specific", "secondary", "season3_start_date", "season3_end_date"),
  action_secondary_covid("adults", "s5", "2020_2021", "specific", "secondary", "season5_start_date", "season5_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Children and Adolescents, Codelist Type: Specific, Investigation Type: Secondary", "# # # # # # # # # # # # # # # # # # #"),

  action_secondary_rsv("children_and_adolescents", "s2", "2017_2018", "specific", "secondary", "season2_start_date", "season2_end_date"),
  action_secondary_flu("children_and_adolescents", "s3", "2018_2019", "specific", "secondary", "season3_start_date", "season3_end_date"),
  action_secondary_covid("children_and_adolescents", "s5", "2020_2021", "specific", "secondary", "season5_start_date", "season5_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Infants, Codelist Type: Specific, Investigation Type: Secondary", "# # # # # # # # # # # # # # # # # # #"),

  action_secondary_rsv_infants("infants", "s2", "2017_2018", "specific", "secondary", "season2_start_date", "season2_end_date"),
  action_secondary_flu_infants("infants", "s3", "2018_2019", "specific", "secondary", "season3_start_date", "season3_end_date"),
  action_secondary_covid_infants("infants", "s5", "2020_2021", "specific", "secondary", "season5_start_date", "season5_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "EXPLORATORY ANALYSES", "# # # # # # # # # # # # # # # # # # #"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Older Adults", "# # # # # # # # # # # # # # # # # # #"),
  
  action_exploratory("older_adults", "s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_exploratory("older_adults", "s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_exploratory("older_adults", "s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_exploratory("older_adults", "s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_exploratory("older_adults", "s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_exploratory("older_adults", "s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_exploratory("older_adults", "s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Adults", "# # # # # # # # # # # # # # # # # # #"),
  
  action_exploratory("adults", "s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_exploratory("adults", "s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_exploratory("adults", "s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_exploratory("adults", "s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_exploratory("adults", "s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_exploratory("adults", "s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_exploratory("adults", "s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Children and Adolescents", "# # # # # # # # # # # # # # # # # # #"),
  
  action_exploratory("children_and_adolescents", "s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_exploratory("children_and_adolescents", "s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_exploratory("children_and_adolescents", "s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_exploratory("children_and_adolescents", "s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_exploratory("children_and_adolescents", "s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_exploratory("children_and_adolescents", "s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_exploratory("children_and_adolescents", "s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Infants", "# # # # # # # # # # # # # # # # # # #"),
  
  action_exploratory("infants", "s1", "2016_2017", "season1_start_date", "season1_end_date"),
  action_exploratory("infants", "s2", "2017_2018", "season2_start_date", "season2_end_date"),
  action_exploratory("infants", "s3", "2018_2019", "season3_start_date", "season3_end_date"),
  action_exploratory("infants", "s4", "2019_2020", "season4_start_date", "season4_end_date"),
  action_exploratory("infants", "s5", "2020_2021", "season5_start_date", "season5_end_date"),
  action_exploratory("infants", "s6", "2021_2022", "season6_start_date", "season6_end_date"),
  action_exploratory("infants", "s7", "2022_2023", "season7_start_date", "season7_end_date"),
  
  comment("# # # # # # # # # # # # # # # # # # #", "SENSITIVITY ANALYSES: REDUCED SEASONS", "# # # # # # # # # # # # # # # # # # #"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Older Adults, Codelist Type: Specific", "# # # # # # # # # # # # # # # # # # #"),

  action_sensitivity("older_adults", "s1", "2016_2017", "season1_start_date", "season1_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("older_adults", "s2", "2017_2018", "season2_start_date", "season2_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("older_adults", "s3", "2018_2019", "season3_start_date", "season3_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("older_adults", "s4", "2019_2020", "season4_start_date", "season4_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("older_adults", "s4", "2019_2020", "season4_start_date", "season4_end_date", "specific", "sensitivity"),
  action_sensitivity("older_adults", "s5", "2020_2021", "season5_start_date", "season5_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("older_adults", "s5", "2020_2021", "season5_start_date", "season5_end_date", "specific", "sensitivity"),
  action_sensitivity("older_adults", "s6", "2021_2022", "season6_start_date", "season6_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("older_adults", "s6", "2021_2022", "season6_start_date", "season6_end_date", "specific", "sensitivity"),
  action_sensitivity("older_adults", "s7", "2022_2023", "season7_start_date", "season7_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("older_adults", "s7", "2022_2023", "season7_start_date", "season7_end_date", "specific", "sensitivity"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Adults, Codelist Type: Specific", "# # # # # # # # # # # # # # # # # # #"),

  action_sensitivity("adults", "s1", "2016_2017", "season1_start_date", "season1_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("adults", "s2", "2017_2018", "season2_start_date", "season2_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("adults", "s3", "2018_2019", "season3_start_date", "season3_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("adults", "s4", "2019_2020", "season4_start_date", "season4_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("adults", "s4", "2019_2020", "season4_start_date", "season4_end_date", "specific", "sensitivity"),
  action_sensitivity("adults", "s5", "2020_2021", "season5_start_date", "season5_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("adults", "s5", "2020_2021", "season5_start_date", "season5_end_date", "specific", "sensitivity"),
  action_sensitivity("adults", "s6", "2021_2022", "season6_start_date", "season6_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("adults", "s6", "2021_2022", "season6_start_date", "season6_end_date", "specific", "sensitivity"),
  action_sensitivity("adults", "s7", "2022_2023", "season7_start_date", "season7_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("adults", "s7", "2022_2023", "season7_start_date", "season7_end_date", "specific", "sensitivity"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Children and Adolscents, Codelist Type: Specific", "# # # # # # # # # # # # # # # # # # #"),

  action_sensitivity("children_and_adolescents", "s1", "2016_2017", "season1_start_date", "season1_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s2", "2017_2018", "season2_start_date", "season2_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s3", "2018_2019", "season3_start_date", "season3_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s4", "2019_2020", "season4_start_date", "season4_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("children_and_adolescents", "s4", "2019_2020", "season4_start_date", "season4_end_date", "specific", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s5", "2020_2021", "season5_start_date", "season5_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("children_and_adolescents", "s5", "2020_2021", "season5_start_date", "season5_end_date", "specific", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s6", "2021_2022", "season6_start_date", "season6_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("children_and_adolescents", "s6", "2021_2022", "season6_start_date", "season6_end_date", "specific", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s7", "2022_2023", "season7_start_date", "season7_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity("children_and_adolescents", "s7", "2022_2023", "season7_start_date", "season7_end_date", "specific", "sensitivity"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Infants, Codelist Type: Specific", "# # # # # # # # # # # # # # # # # # #"),

  action_sensitivity_infants("infants", "s1", "2016_2017", "season1_start_date", "season1_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity_infants("infants", "s2", "2017_2018", "season2_start_date", "season2_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity_infants("infants", "s3", "2018_2019", "season3_start_date", "season3_end_date", "specific", "primary", "sensitivity"),
  action_sensitivity_infants("infants", "s4", "2019_2020", "season4_start_date", "season4_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity_infants("infants", "s4", "2019_2020", "season4_start_date", "season4_end_date", "specific", "sensitivity"),
  action_sensitivity_infants("infants", "s5", "2020_2021", "season5_start_date", "season5_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity_infants("infants", "s5", "2020_2021", "season5_start_date", "season5_end_date", "specific", "sensitivity"),
  action_sensitivity_infants("infants", "s6", "2021_2022", "season6_start_date", "season6_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity_infants("infants", "s6", "2021_2022", "season6_start_date", "season6_end_date", "specific", "sensitivity"),
  action_sensitivity_infants("infants", "s7", "2022_2023", "season7_start_date", "season7_end_date", "specific", "primary", "sensitivity"),
  action_covid_sensitivity_infants("infants", "s7", "2022_2023", "season7_start_date", "season7_end_date", "specific", "sensitivity"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Older Adults, Codelist Type: Sensitive", "# # # # # # # # # # # # # # # # # # #"),

  action_sensitivity("older_adults", "s1", "2016_2017", "season1_start_date", "season1_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("older_adults", "s2", "2017_2018", "season2_start_date", "season2_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("older_adults", "s3", "2018_2019", "season3_start_date", "season3_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("older_adults", "s4", "2019_2020", "season4_start_date", "season4_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("older_adults", "s4", "2019_2020", "season4_start_date", "season4_end_date", "sensitive", "sensitivity"),
  action_sensitivity("older_adults", "s5", "2020_2021", "season5_start_date", "season5_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("older_adults", "s5", "2020_2021", "season5_start_date", "season5_end_date", "sensitive", "sensitivity"),
  action_sensitivity("older_adults", "s6", "2021_2022", "season6_start_date", "season6_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("older_adults", "s6", "2021_2022", "season6_start_date", "season6_end_date", "sensitive", "sensitivity"),
  action_sensitivity("older_adults", "s7", "2022_2023", "season7_start_date", "season7_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("older_adults", "s7", "2022_2023", "season7_start_date", "season7_end_date", "sensitive", "sensitivity"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Adults, Codelist Type: Sensitive", "# # # # # # # # # # # # # # # # # # #"),

  action_sensitivity("adults", "s1", "2016_2017", "season1_start_date", "season1_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("adults", "s2", "2017_2018", "season2_start_date", "season2_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("adults", "s3", "2018_2019", "season3_start_date", "season3_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("adults", "s4", "2019_2020", "season4_start_date", "season4_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("adults", "s4", "2019_2020", "season4_start_date", "season4_end_date", "sensitive", "sensitivity"),
  action_sensitivity("adults", "s5", "2020_2021", "season5_start_date", "season5_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("adults", "s5", "2020_2021", "season5_start_date", "season5_end_date", "sensitive", "sensitivity"),
  action_sensitivity("adults", "s6", "2021_2022", "season6_start_date", "season6_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("adults", "s6", "2021_2022", "season6_start_date", "season6_end_date", "sensitive", "sensitivity"),
  action_sensitivity("adults", "s7", "2022_2023", "season7_start_date", "season7_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("adults", "s7", "2022_2023", "season7_start_date", "season7_end_date", "sensitive", "sensitivity"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Children and Adolescents, Codelist Type: Sensitive", "# # # # # # # # # # # # # # # # # # #"),

  action_sensitivity("children_and_adolescents", "s1", "2016_2017", "season1_start_date", "season1_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s2", "2017_2018", "season2_start_date", "season2_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s3", "2018_2019", "season3_start_date", "season3_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s4", "2019_2020", "season4_start_date", "season4_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("children_and_adolescents", "s4", "2019_2020", "season4_start_date", "season4_end_date", "sensitive", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s5", "2020_2021", "season5_start_date", "season5_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("children_and_adolescents", "s5", "2020_2021", "season5_start_date", "season5_end_date", "sensitive", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s6", "2021_2022", "season6_start_date", "season6_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("children_and_adolescents", "s6", "2021_2022", "season6_start_date", "season6_end_date", "sensitive", "sensitivity"),
  action_sensitivity("children_and_adolescents", "s7", "2022_2023", "season7_start_date", "season7_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity("children_and_adolescents", "s7", "2022_2023", "season7_start_date", "season7_end_date", "sensitive", "sensitivity"),

  comment("# # # # # # # # # # # # # # # # # # #", "Cohort: Infants, Codelist Type: Sensitive", "# # # # # # # # # # # # # # # # # # #"),

  action_sensitivity_infants("infants", "s1", "2016_2017", "season1_start_date", "season1_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity_infants("infants", "s2", "2017_2018", "season2_start_date", "season2_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity_infants("infants", "s3", "2018_2019", "season3_start_date", "season3_end_date", "sensitive", "primary", "sensitivity"),
  action_sensitivity_infants("infants", "s4", "2019_2020", "season4_start_date", "season4_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity_infants("infants", "s4", "2019_2020", "season4_start_date", "season4_end_date", "sensitive", "sensitivity"),
  action_sensitivity_infants("infants", "s5", "2020_2021", "season5_start_date", "season5_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity_infants("infants", "s5", "2020_2021", "season5_start_date", "season5_end_date", "sensitive", "sensitivity"),
  action_sensitivity_infants("infants", "s6", "2021_2022", "season6_start_date", "season6_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity_infants("infants", "s6", "2021_2022", "season6_start_date", "season6_end_date", "sensitive", "sensitivity"),
  action_sensitivity_infants("infants", "s7", "2022_2023", "season7_start_date", "season7_end_date", "sensitive", "primary", "sensitivity"),
  action_covid_sensitivity_infants("infants", "s7", "2022_2023", "season7_start_date", "season7_end_date", "sensitive", "sensitivity"),

  comment("# # # # # # # # # # # # # # # # # # #", "Files for release", "# # # # # # # # # # # # # # # # # # #"),
  
  action_finalise("older_adults"),
  action_finalise("adults"),
  action_finalise("children_and_adolescents"),
  action_finalise_infants("infants"),

  comment("# # # # # # # # # # # # # # # # # # #", "End", "# # # # # # # # # # # # # # # # # # #")

)


project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

## convert list to yaml, reformat comments and whitespace ----
thisproject <- as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1")


# if running via opensafely, check that the project on disk is the same as the project created here:
if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("expectations", "tpp")){

  thisprojectsplit <- str_split(thisproject, "\n")[[1]]
  currentproject <- readLines(here("project.yaml"))

  stopifnot("project.yaml is not up-to-date with create-project.R.  Run create-project.R before running further actions." = identical(thisprojectsplit, currentproject))

# if running manually, output new project as normal
} else if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("")){

## output to file ----
  writeLines(thisproject, here("project.yaml"))
#yaml::write_yaml(project_list, file =here("project.yaml"))

## grab all action names and send to a txt file

names(actions_list) %>% tibble(action=.) %>%
  mutate(
    model = action==""  & lag(action!="", 1, TRUE),
    model_number = cumsum(model),
  ) %>%
  group_by(model_number) %>%
  summarise(
    sets = str_trim(paste(action, collapse=" "))
  ) %>% pull(sets) %>%
  paste(collapse="\n") %>%
  writeLines(here("actions.txt"))

# fail if backend not recognised
} else {
  stop("Backend not recognised")
}

##filter actions based on prefix

actions_list %>% 
  names() %>% 
  str_subset("^generate.+secondary$") %>% 
  tibble(action=.) %>% 
  mutate(
    model = action==""  & lag(action!="", 1, TRUE),
    model_number = cumsum(model),
  ) %>% 
  group_by(model_number) %>% 
  summarise(
    sets = str_trim(paste(action, collapse=" "))
  ) %>% pull(sets) %>% 
  paste(collapse="\n") %>% 
  writeLines(here("filtered_actions.txt"))
