library(tidyverse)
library(here)
library(broom)
library(rlang)
library(broom.helpers)
library(cowplot)
library(stringr)
library(egg)

#import model functions
source(here::here("post_check", "functions", "model.R"))
options(scipen = 999)

#create function to filter collated results to models wanted and then plot
forest <- function(df, df_dummy, pathogen, model_type, outcome_type,
                   further = "no") {

  pathogen <- if_else(pathogen == "overall_and_all_cause", "overall_resp",
                      pathogen)
  
  df_model <- df %>%
    filter(model_type == !!model_type,
           str_detect(model_name, outcome_type)) %>%
    group_by(codelist_type, subset)
  
  outcome <- if_else(outcome_type == "Mild",
                     paste0(pathogen, "_primary_inf"),
                     paste0(pathogen, "_secondary_inf"))
  
  exposure <- case_when(
    model_type == "ethnicity" ~ list("latest_ethnicity_group"),
    model_type == "ses" ~ list("imd_quintile"),
    model_type == "composition" ~ list("composition_category"),
    model_type == "ethnicity_ses" ~ list(c("latest_ethnicity_group",
                                           "imd_quintile")),
    model_type == "ethnicity_composition" ~ list(c("latest_ethnicity_group",
                                                   "composition_category")),
    model_type == "ses_composition" ~ list(c("imd_quintile",
                                             "composition_category")),
    model_type == "full" ~ list(c("latest_ethnicity_group",
                                  "imd_quintile",
                                  "composition_category"))
  )[[1]]
  
  offset <- case_when(
    outcome_type == "Mild" ~ paste0("time_", pathogen, "_primary"),
    outcome_type == "Severe" ~ paste0("time_", pathogen, "_secondary")
  )

  if (further == "no") {

    dummy_model <- glm_poisson(df_dummy, exposure, outcome, offset)

  } else {

      vacc_prev <- case_when(
        pathogen == "flu" ~ "prior_flu_vaccination",
        pathogen == "covid" ~ "time_since_last_covid_vaccination"
    )
    
    if (cohort == "infants" | cohort == "infants_subgroup") {
      
      dummy_model <- glm_poisson_further(
        df_dummy, exposure, outcome, offset_var = offset
      )
      
    } else {
      
      dummy_model <- glm_poisson_further(
        df_dummy, exposure, outcome, vacc_prev, offset_var = offset
      )
      
    }

  }
  
  #define levels
  levels <- list()
  
  if (cohort == "infants" | cohort == "infants_subgroup") {
    
    levels <- c("12-23m", "6-11m", "3-5m", "0-2m", "Male", "Female")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Male", "Female")
    
  } else if (cohort == "adults") {
    
    levels <- c("40-64y", "18-39y", "Male", "Female")
    
  } else {
    
    levels <- c("90y+", "75-89y", "65-74y", "Male", "Female")
    
  }
  
  if (model_type == "ethnicity") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed",  "Other Ethnic Groups",
                  "Black or Black British", "Asian or Asian British",
                  "Mixed", "White"),
                levels)
    
  } else if (model_type == "ses") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "1 (most deprived)",
                  "2", "3", "4", "5 (least deprived)"), levels)
    
  } else if (model_type == "composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation"), levels)
    
  } else if (model_type == "ethnicity_ses") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "1 (most deprived)",
                  "2", "3", "4", "5 (least deprived)",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
    
  } else if (model_type == "ethnicity_composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
    
  } else if (model_type == "ses_composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "1 (most deprived)", "2", "3", "4",
                  "5 (least deprived)"), levels)
    
  } else if (model_type == "full" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
    
  }
  
  if (cohort == "infants_subgroup") {
    
    levels <- c("Maternal Pertussis Vaccination",
                "Maternal Flu Vaccination", "Maternal Drug Usage",
                "Maternal Drinking", "Binary Variables (Reference)",
                "Maternal Current Smoking", "Maternal Former Smoking",
                "Maternal Never Smoking", "Maternal Age",
                "Maternal Age (Average)", levels)
    
  } else if (cohort != "infants" & pathogen == "flu" & investigation_type == "primary") {
    
    levels <- c("Flu Vaccination (Yes)", "Flu Vaccination (No)",
                "Eligible and Vaccinated Last Autumn",
                "Not Vaccinated in Past Year", levels)
    
  } else if (cohort != "infants" & pathogen == "covid" & investigation_type == "primary") {
    
    levels <- c("Covid Vaccination (Yes)", "Covid Vaccination (No)",
                "Not Vaccinated in Past Year",
                "Eligible and Vaccinated Last Autumn",
                "Eligible and Vaccinated Last Spring", levels)
    
  } else if (investigation_type == "secondary") {

    levels <- c("Drug Usage", "Hazardous Drinking", "Sickle Cell",
                "Immunosuppressed", "Cancer Within 3 Yrs", "CND", "CKD",
                "CLD", "CHD", "Severely Obese", "Addisons", "Diabetes",
                "Other Resp. Cond.", "Cystic Fibrosis", "COPD", "Asthma",
                "Binary Variables (Reference)", "Current", "Former",
                "Never", levels)

  }
  
  process_forest_plot <- function(df_model) {
    
    if (nrow(df_model) != 0) {
      
      tidy_forest <- df_model %>%
        tidy_attach_model(dummy_model) %>%
        tidy_add_reference_rows() %>%
        tidy_add_estimate_to_reference_rows(exponentiate = TRUE,
                                            conf.level = 95) %>%
        tidy_add_term_labels() %>%
        tidy_remove_intercept() %>%
        mutate(
          conf.low = if_else(conf.low < 1e-100, NA_real_, conf.low),
          conf.high = if_else(conf.high < 1e-100, NA_real_, conf.high)
        ) %>%
        mutate(
          reference_row = if_else(var_type == "continuous", FALSE, reference_row)
        )
      
      if (cohort == "infants_subgroup" & further == "yes") {
        
        continuous_vars <- tibble(
          term = "maternal_ageAverage",
          variable = "maternal_age",
          var_label = "maternal_age",
          var_class = "integer",
          var_type = "continuous",
          var_nlevels = 1,
          contrasts = "contr.treatment",
          contrasts_type = "treatment",
          reference_row = TRUE,
          label = "Maternal Age (Average)",
          model_name = NA,
          estimate = 1,
          std.error = 0,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = NA
        )
        
        tidy_forest <- rbind(tidy_forest, continuous_vars)
        
      } else if (cohort != "infants" & cohort != "infants_subgroup" & pathogen == "flu" & further == "yes") {
        
        continuous_vars <- tibble(
          term = "flu_vaccinationNo",
          variable = "flu_vaccination",
          var_label = "flu_vaccination",
          var_class = "integer",
          var_type = "continuous",
          var_nlevels = 1,
          contrasts = "contr.treatment",
          contrasts_type = "treatment",
          reference_row = TRUE,
          label = "Flu Vaccination (No)",
          model_name = NA,
          estimate = 1,
          std.error = 0,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = NA
        )
        
        tidy_forest <- rbind(tidy_forest, continuous_vars)
        
      } else if (cohort != "infants" & cohort != "infants_subgroup" &
                 pathogen == "covid" & further == "yes") {
        
        continuous_vars <- tibble(
          term = "covid_vaccinationNo",
          variable = "covid_vaccination",
          var_label = "covid_vaccination",
          var_class = "integer",
          var_type = "continuous",
          var_nlevels = 1,
          contrasts = "contr.treatment",
          contrasts_type = "treatment",
          reference_row = TRUE,
          label = "Covid Vaccination (No)",
          model_name = NA,
          estimate = 1,
          std.error = 0,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = NA
        )
        
        tidy_forest <- rbind(tidy_forest, continuous_vars)
        
      }
      
    } else if (nrow(df_model) == 0 & model_type %in% c(
      "composition", "ethnicity_composition", "ses_composition", "full")) {
      
      age <- case_when(
        cohort == "older_adults" ~ "65-74y",
        cohort == "adults" ~ "18-39y",
        cohort == "children_and_adolescents" ~ "2-5y",
        cohort == "infants" ~ "0-2m",
        cohort == "infants_subgroup" ~ "0-2m"
      )
      
      vars <- case_when(
        model_type == "ethnicity" ~ list(c("sex", "age_band", "latest_ethnicity_group",
                                           "rurality_classification")),
        model_type == "ses" ~ list(c("sex", "age_band", "imd_quintile",
                                     "rurality_classification")),
        model_type == "composition" ~ list(c(
          "sex", "age_band", "composition_category", "rurality_classification")),
        model_type == "ethnicity_ses" ~ list(c(
          "sex", "age_band", "latest_ethnicity_group", "imd_quintile",
          "rurality_classification")),
        model_type == "ethnicity_composition" ~ list(c(
          "sex", "age_band", "ethnicity", "composition_category",
          "rurality_classification")),
        model_type == "ses_composition" ~ list(c(
          "sex", "age_band", "imd_quintile", "composition_category",
          "rurality_classification")),
        model_type == "full" ~ list(c(
          "sex", "age_band", "ethnicity", "imd_quintile", "composition_category",
          "rurality_classification"))
      )[[1]]
      
      var_labels <- case_when(
        model_type == "ethnicity" ~ list(c("Female", age, "White",
                                           "Rural Town and Fringe")),
        model_type == "ses" ~ list(c("Female", age, "5 (least deprived)",
                                     "Rural Town and Fringe")),
        model_type == "composition" ~ list(c(
          "Female", age, "Multiple of the Same Generation", "Rural Town and Fringe")),
        model_type == "ethnicity_ses" ~ list(c(
          "Female", age, "White", "5 (least deprived)", "Rural Town and Fringe")),
        model_type == "ethnicity_composition" ~ list(c(
          "Female", age, "White", "Multiple of the Same Generation",
          "Rural Town and Fringe")),
        model_type == "ses_composition" ~ list(c(
          "Female", age, "5 (least deprived)", "Multiple of the Same Generation",
          "Rural Town and Fringe")),
        model_type == "full" ~ list(c(
          "Female", age, "White", "5 (least deprived)", "Multiple of the Same Generation",
          "Rural Town and Fringe"))
      )[[1]]
      
      tidy_forest <- bind_rows(
        tibble(
          term = NA,
          variable = vars,
          var_label = var_labels,
          var_class = NA,
          var_type = NA,
          var_nlevels = NA_real_,
          contrasts = NA,
          contrasts_type = NA,
          reference_row = TRUE,
          label = var_labels,
          estimate = NA_real_,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          model_type = !!model_type,
          codelist_type = "specific",
          investigation_type = investigation_type,
          subset = "2020_21"
        ),
        tibble(
          term = NA,
          variable = vars,
          var_label = var_labels,
          var_class = NA,
          var_type = NA,
          var_nlevels = NA_real_,
          contrasts = NA,
          contrasts_type = NA,
          reference_row = TRUE,
          label = var_labels,
          estimate = NA_real_,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          model_type = !!model_type,
          codelist_type = "sensitive",
          investigation_type = investigation_type,
          subset = "2020_21"
        ),
        tibble(
          term = NA,
          variable = vars,
          var_label = var_labels,
          var_class = NA,
          var_type = NA,
          var_nlevels = NA_real_,
          contrasts = NA,
          contrasts_type = NA,
          reference_row = TRUE,
          label = var_labels,
          estimate = 1,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = "2020_21"
        )
      )
      
    } else if (model_type %in% c("composition", "ethnicity_composition",
                                 "ses_composition", "full") &
               length(unique(df_model$codelist_type)) != 3) {

      tidies <- tidy_forest %>% 
        filter(codelist_type == "sensitive")
      
      tidy_forest <- bind_rows(
        tibble(
          term = tidies$term,
          variable = tidies$variable,
          var_label = tidies$var_label,
          var_class = NA,
          var_type = NA,
          var_nlevels = NA_real_,
          contrasts = NA,
          contrasts_type = NA,
          reference_row = FALSE,
          label = tidies$var_label,
          estimate = NA_real_,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          model_type = !!model_type,
          codelist_type = "specific",
          investigation_type = investigation_type,
          subset = "2020_21"
        ),
        tidy_forest
      )

    }
    
    if (cohort == "infants_subgroup" & further == "yes") {
      
      binaries <- tidy_forest %>%
        filter(str_detect(term, "Yes")) %>%
        rowwise() %>%
        mutate(
          label = str_to_title(label)
        ) %>%
        rbind(tibble(
          term = "are_binary_variablesYes",
          variable = "binary_variables",
          var_label = "binary_variables",
          var_class = "factor",
          var_type = "dichotomous",
          var_nlevels = 2,
          contrasts = "contr.treatment",
          contrasts_type = "treatment",
          reference_row = TRUE,
          label = "Binary Variables (Reference)",
          model_name = NA,
          estimate = 1,
          std.error = 0,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = NA)
        ) 
      
      tidy_forest <- tidy_forest %>%
        filter(!(str_detect(term, "Yes"))) %>%
        filter(!(str_detect(term, "No"))) %>%
        bind_rows(binaries)
      
    } else if (investigation_type == "secondary") {
    
      binaries <- tidy_forest %>%
        filter(str_detect(term, "Yes")) %>%
        rowwise() %>%
        mutate(
          label = case_when(
            str_detect(label, "Chd") ~ gsub("Chd", "CHD", label),
            str_detect(label, "Ckd") ~ gsub("Ckd", "CKD", label),
            str_detect(label, "Cld") ~ gsub("Cld", "CLD", label),
            str_detect(label, "Cnd") ~ gsub("Cnd", "CND", label),
            str_detect(label, "Copd") ~ gsub("Copd", "COPD", label),
            str_detect(label, "Cancer") ~ gsub("Cancer",
                                               "Cancer Within 3 Yrs", label),
            TRUE ~ str_to_title(label))
        ) %>%
        rbind(tibble(
            term = "are_binary_variablesYes",
            variable = "binary_variables",
            var_label = "binary_variables",
            var_class = "factor",
            var_type = "dichotomous",
            var_nlevels = 2,
            contrasts = "contr.treatment",
            contrasts_type = "treatment",
            reference_row = TRUE,
            label = "Binary Variables (Reference)",
            model_name = NA,
            estimate = 1,
            std.error = 0,
            statistic = NA,
            p.value = NA,
            conf.low = 1,
            conf.high = 1,
            model_type = !!model_type,
            codelist_type = "reference",
            investigation_type = "secondary",
            subset = NA)
        ) 
      
      tidy_forest <- tidy_forest %>%
        filter(!(str_detect(term, "Yes"))) %>%
        filter(!(str_detect(term, "No"))) %>%
        bind_rows(binaries)
      
    }
    
    if (model_type %in% c(
      "composition", "ethnicity_composition", "ses_composition", "full")) {
      
      # Expand reference rows
      reference_rows <- tidy_forest %>%
        filter(reference_row) %>%
        mutate(subset = c("2020_21"),
               conf.low = 1,
               conf.high = 1
        ) %>%
        mutate(codelist_type = "reference")
      
    } else {
      
      # Expand reference rows
      reference_rows <- tidy_forest %>%
        filter(reference_row) %>%
        mutate(rn = row_number()) %>%
        slice(rep(1:n(), each = 8)) %>%
        group_by(rn) %>%
        mutate(
          subset = c("2016_17", "2017_18", "2018_19", "2019_20", "2020_21",
                     "2021_22", "2022_23", "2023_24"),
          conf.low = 1,
          conf.high = 1
        ) %>%
        ungroup() %>%
        select(-rn) %>%
        mutate(codelist_type = "reference")
      
    }
      
    tidy_forest <- tidy_forest %>%
      filter(!reference_row) %>%
      bind_rows(reference_rows)
    
    legend_labels <- unique(str_to_title(gsub("_", " ", tidy_forest$variable)))

    if (further == "yes") {
      vacc_prev <- vacc_prev
    } else {
      vacc_prev <- "NONE"
    }
    
    cols2 <- tibble(
      variable = c("sex", "age_band", "latest_ethnicity_group", "imd_quintile",
                   "composition_category", "rurality_classification",
                   vacc_prev, "vax_status", "flu_vaccination",
                   "covid_vaccination", "maternal_age", "maternal_smoking_status",
                   "smoking_status", "binary_variables", "maternal_drinking",
                   "maternal_drug_usage", "maternal_flu_vaccination",
                   "maternal_pertussis_vaccination",
                   "has_asthma", "has_copd", "has_cystic_fibrosis",
                   "has_other_resp", "has_diabetes", "has_addisons",
                   "severe_obesity", "has_chd", "has_ckd", "has_cld", "has_cnd",
                   "has_cancer", "immunosuppressed", "has_sickle_cell",
                   "hazardous_drinking", "drug_usage"),
      col = c('#1f77b4', '#ffbb78', '#2ca02c', '#ff9896',
              '#aec7e8', '#ff7f0e', '#98df8a',
              '#d62728', '#d62728',
              '#d62728', '#9467bd',
              '#c49c94', '#9467bd', '#4e3f2c',
              '#e377c2', '#c5b0d5',
              '#8c564b', '#f7b6d2',
              '#d177f4', '#7b98f4', '#37aabe',
              '#bcbd22', '#c5b0d5',
              '#dbdb8d', '#17becf', '#9edae5',
              '#b884f4', '#ec62f4',
              '#38a8cb', '#9d8ff4',
              '#98df8a', '#43a1f4',
              '#8c564b', '#e377c2'),
      labels = c("Sex", "Age Group", "Ethnicity", "IMD Quintile",
                 "Household Composition", "Rurality", "Prior Vaccination",
                 "Current Vaccination", "Current Vaccination",
                 "Current Vaccination", "Age",
                 "Maternal Smoking Status", "Smoking Status", "Binary Variables",
                 "Maternal Drinking", "Maternal Drug Usage",
                 "Maternal Flu Vaccination", "Maternal Pertussis Vaccination",
                 "Asthma", "COPD", "Cystic Fibrosis",
                 "Other Chronic Respiratory Disease", "Diabetes",
                 "Addison's Disease", "Severely Obese", "Chronic Heart Disease",
                 "Chronic Kidney Disease", "Chronic Liver Disease",
                 "Chronic Neurological Disease", "Cancer Within 3 Years",
                 "Immunosuppressed", "Sickle Cell Disease",
                 "Hazardous Drinking", "Drug Usage")
    )
    
    cols_final <- cols2 %>%
      filter(variable %in% unique(tidy_forest$variable))
    
    cols_final2 <- full_join(cols_final, tidy_forest, by = "variable",
                             relationship = "one-to-many") %>%
      select(variable, subset, col, labels)
    
    tidy_forest <- merge(tidy_forest, cols_final2,
                         by = c("variable", "subset"))
    
    tidy_forest <- tidy_forest %>%
      mutate(
        shape = case_when(
          codelist_type == "reference" ~ 16,
          codelist_type == "specific" ~ 17,
          codelist_type == "sensitive" ~ 15,
        )
      )
    
    plot_labelling <- cols_final2$col
    names(plot_labelling) <- cols_final2$labels

    pathogen_title <- case_when(
      pathogen == "rsv" ~ "RSV",
      pathogen == "flu" ~ "Influenza",
      pathogen == "covid" ~ "COVID-19",
      pathogen == "overall_resp" ~ "Overall Respiratory Virus"
    )

    model_title <- case_when(
      model_type == "ethnicity" ~ "Ethnicity",
      model_type == "ses" ~ "IMD Quintile",
      model_type == "composition" ~ "Household Composition",
      model_type == "ethnicity_ses" ~ "Ethnicity and IMD Quintile",
      model_type == "ethnicity_composition" ~ "Ethnicity and Household Composition",
      model_type == "ses_composition" ~ "IMD Quintile and Household Composition",
      model_type == "full" ~ "Ethnicity, IMD Quintile, and Household Composition" 
    )

    title <- paste0(outcome_type, " ", pathogen_title, " by ", model_title)
    
    if (cohort == "infants_subgroup") {
      
      if (pathogen == "covid") {
        
        tidy_forest %>%
          mutate(
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              label == "Current" ~ "Maternal Current Smoking",
              label == "Former" ~ "Maternal Former Smoking",
              label == "Never" ~ "Maternal Never Smoking",
              str_detect(label, "Age") ~ "Maternal Age (Average)",
              label == "maternal_age" ~ "Maternal Age",
              TRUE ~ label
            )
          ) %>% 
          mutate(
            plot_label = str_to_title(
              gsub("has ", "", gsub("_", " ", variable))),
            subset = gsub("_", "-", subset),
            faceting = case_when(
              variable %in% c("age_band", "sex", "latest_ethnicity_group",
                              "imd_quintile", "rurality_classification") ~
                "Infant", TRUE ~ "Maternal"
            )
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Comp") ~ "Household Composition",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              TRUE ~ plot_label)
          ) %>%
          mutate(
            label = if_else(label == "Yes", plot_label, label)
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          filter(subset %in% c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24")) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_wrap( ~ subset, ncol = 4, nrow = 2) + 
          labs(title = title, x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      } else {
        
        tidy_forest %>%
          mutate(
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              label == "Current" ~ "Maternal Current Smoking",
              label == "Former" ~ "Maternal Former Smoking",
              label == "Never" ~ "Maternal Never Smoking",
              str_detect(label, "Age") ~ "Maternal Age (Average)",
              label == "maternal_age" ~ "Maternal Age",
              TRUE ~ label
            )
          ) %>%
          mutate(
            plot_label = str_to_title(
              gsub("has ", "", gsub("_", " ", variable))),
            subset = gsub("_", "-", subset),
            faceting = case_when(
              variable %in% c("age_band", "sex", "latest_ethnicity_group",
                              "imd_quintile", "rurality_classification") ~
                "Infant", TRUE ~ "Maternal"
            )
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              TRUE ~ plot_label)
          ) %>%
          mutate(
            label = if_else(label == "Yes", plot_label, label)
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels)
          ) %>%
          mutate(
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_wrap(~ subset, ncol = 4, nrow = 2) + 
          labs(title = title, x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      }
      
    } else {
      
      if (pathogen == "covid") {

        if (investigation_type == "secondary") {

          tidy_forest <- tidy_forest %>%
            filter(subset %in% c("2017_18", "2018_19", "2020_21"))

        } else if (investigation_type == "sensitivity") {

          tidy_forest <- tidy_forest %>%
            filter(subset %in% c("2017_18", "2018_19"))

        }
        
        tidy_forest %>%
          mutate(
            label = if_else(
              str_detect(term, "Yes"),
              str_to_title(gsub("has ", "", gsub("_", " ", variable))),
              label
            ),
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              str_detect(label, "Cancer") ~ "Cancer Within 3 Yrs",
              str_detect(label, "Chd") ~ "CHD",
              str_detect(label, "Ckd") ~ "CKD",
              str_detect(label, "Cld") ~ "CLD",
              str_detect(label, "Cnd") ~ "CND",
              str_detect(label, "Copd") ~ "COPD",
              str_detect(label, "Resp") ~ "Other Resp. Cond.",
              str_detect(label, "Obesity") ~ "Severely Obese",
              str_detect(label, "Binary") ~ "Binary Variables (Reference)",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = case_when(
              variable == "vax_status" ~ "Covid Vaccination (Yes)",
              variable == "time_since_last_covid_vaccination" ~ paste0(
                label, " Since Last Covid Vaccination"),
              TRUE ~ label
            ),
            subset = gsub("_", "-", subset)
          ) %>%
          mutate(
            variable = case_when(
              variable == "vax_status" ~ "covid_vaccination",
              TRUE ~ variable
            ),
            plot_label = str_to_title(gsub("_", " ", variable))
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Comp") ~ "Household Composition",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              str_detect(plot_label, "Time Since") ~ "Prior Vaccination",
              str_detect(plot_label, "Covid Vaccination") ~ "Current Vaccination",
              TRUE ~ plot_label)
          ) %>%
          mutate(
            label = case_when(
              str_detect(label, regex("^6-12m Since Last Covid Vaccination$")) ~ "Eligible and Vaccinated Last Autumn",
              str_detect(label, regex("^0-6m Since Last Covid Vaccination$")) ~ "Eligible and Vaccinated Last Spring",
              str_detect(label, "12m") ~ "Not Vaccinated in Past Year",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          filter(subset %in% c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24")) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_wrap(~ subset, nrow = 2, ncol = 4) + 
          labs(title = title, x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      } else if (cohort != "infants" & model_type %in% c(
        "composition", "ethnicity_composition", "ses_composition", "full")) {
        
        tidy_forest %>%
          mutate(
            label = if_else(
              str_detect(term, "Yes"),
              str_to_title(gsub("has ", "", gsub("_", " ", variable))),
              label
            ),
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              str_detect(label, "Cancer") ~ "Cancer Within 3 Yrs",
              str_detect(label, "Chd") ~ "CHD",
              str_detect(label, "Ckd") ~ "CKD",
              str_detect(label, "Cld") ~ "CLD",
              str_detect(label, "Cnd") ~ "CND",
              str_detect(label, "Copd") ~ "COPD",
              str_detect(label, "Resp") ~ "Other Resp. Cond.",
              str_detect(label, "Obesity") ~ "Severely Obese",
              str_detect(label, "Binary") ~ "Binary Variables (Reference)",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = case_when(
              variable == "vax_status" & pathogen == "covid" ~ "Covid Vaccination (Yes)",
              variable == "time_since_last_covid_vaccination" ~ paste0(
                label, " Since Last Covid Vaccination"),
              variable == "prior_flu_vaccination" ~ paste0(
                "Prior Flu Vaccination (", str_to_title(label),")"),
              variable == "vax_status" & pathogen == "flu" ~ "Flu Vaccination (Yes)",
              TRUE ~ label
            ),
            subset = gsub("_", "-", subset)
          ) %>%
          mutate(
            variable = case_when(
              variable == "vax_status" & pathogen == "covid" ~ "covid_vaccination",
              variable == "vax_status" & pathogen == "flu" ~ "flu_vaccination",
              TRUE ~ variable
            ),
            plot_label = str_to_title(gsub("_", " ", variable))
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Comp") ~ "Household Composition",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              str_detect(plot_label, "Time Since") ~ "Prior Vaccination",
              str_detect(plot_label, "Covid Vaccination") ~ "Current Vaccination",
              TRUE ~ plot_label)
          ) %>%
          mutate(
            label = case_when(
              str_detect(label, regex("^6-12m Since Last Covid Vaccination$")) ~ "Eligible and Vaccinated Last Autumn",
              str_detect(label, regex("^0-6m Since Last Covid Vaccination$")) ~ "Eligible and Vaccinated Last Spring",
              str_detect(label, "12m") ~ "Not Vaccinated in Past Year",
              str_detect(label, "Prior") & str_detect(label, "(Yes)") ~ "Eligible and Vaccinated Last Autumn",
              str_detect(label, "Prior") & str_detect(label, "(No)") ~ "Not Vaccinated in Past Year",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          filter(subset %in% c("2020-21")) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_wrap(~ subset, nrow = 2, ncol = 4) + 
          labs(title = title, x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      } else {

        if (investigation_type == "secondary") {

          tidy_forest <- tidy_forest %>%
            filter(subset %in% c("2017_18", "2018_19", "2020_21"))

        } else if (investigation_type == "sensitivity") {

          if (pathogen == "rsv") {

            tidy_forest <- tidy_forest %>%
              filter(subset == "2017_18")

          } else if (pathogen == "flu") {

            tidy_forest <- tidy_forest %>%
              filter(subset == "2018_19")

          }

        }
        
        tidy_forest %>%
          mutate(
            label = if_else(
              str_detect(term, "Yes"),
              str_to_title(gsub("has ", "", gsub("_", " ", variable))),
              label
            ),
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              str_detect(label, "Cancer") ~ "Cancer Within 3 Yrs",
              str_detect(label, "Chd") ~ "CHD",
              str_detect(label, "Ckd") ~ "CKD",
              str_detect(label, "Cld") ~ "CLD",
              str_detect(label, "Cnd") ~ "CND",
              str_detect(label, "Copd") ~ "COPD",
              str_detect(label, "Resp") ~ "Other Resp. Cond.",
              str_detect(label, "Obesity") ~ "Severely Obese",
              str_detect(label, "Binary") ~ "Binary Variables (Reference)",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = case_when(
              variable == "vax_status" ~ "Flu Vaccination (Yes)",
              variable == "prior_flu_vaccination" ~ paste0(
                "Prior Flu Vaccination (", str_to_title(label),")"),
              TRUE ~ label),
            subset = gsub("_", "-", subset)
          ) %>%
          mutate(
            variable = case_when(
              variable == "vax_status" ~ "flu_vaccination",
              TRUE ~ variable
            ),
            plot_label = str_to_title(gsub("_", " ", variable))
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Comp") ~ "Household Composition",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              str_detect(plot_label, "Prior") ~ "Prior Vaccination",
              str_detect(plot_label, "Flu Vaccination") ~ "Current Vaccination",
              TRUE ~ plot_label),
            label = case_when(
              str_detect(label, "Prior") & str_detect(label, "(Yes)") ~ "Eligible and Vaccinated Last Autumn",
              str_detect(label, "Prior") & str_detect(label, "(No)") ~ "Not Vaccinated in Past Year",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_wrap(~ subset, nrow = 2, ncol = 4) + 
          labs(title = title, x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      }
      
    }
    
  }
  
  if (cohort != "infants" & cohort != "infants_subgroup" & 
    model_type %in% c("composition", "ethnicity_composition",
                      "ses_composition", "full")) {
    my_tag <- "2020-21"
  } else if (investigation_type %in% c("secondary", "sensitivity")) {
    my_tag <- case_when(
      pathogen == "rsv" ~ "2017-18",
      pathogen == "flu" ~ "2018-19",
      pathogen == "covid" ~ "2020-21"
    )
  } else if (pathogen == "covid") {
    my_tag <- c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24")
  } else {
    my_tag <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
                "2021-22", "2022-23", "2023-24")
  }
  
  plot <- process_forest_plot(df_model)
  
  dat_ribbon <- plot$data %>%
    select(variable, label, var_nlevels) %>%
    unique() %>%
    mutate(
      xmin = 0.055,
      xmax = 10,
      label = factor(label, levels = levels)
    ) %>%
    arrange(label) %>%
    mutate(
      yposition = seq_len(n()),
      ymin = yposition - 0.5,
      ymax = yposition + 0.5
    ) %>%
    # assign a group order based on where each variable first appears in the arranged data
    mutate(group_id = match(variable, unique(variable))) %>%
    mutate(fill = if_else(group_id %% 2 == 1, "a", "b"))
  
  dat_ribbon_long <- dat_ribbon %>%
    pivot_longer(cols = c(xmin, xmax), values_to = "x",
                 names_to = "xmin_xmax") %>%
    select(-xmin_xmax)

  plot <- plot +
    geom_ribbon(
      data = dat_ribbon_long,
      aes(x = x, ymin = ymin, ymax = ymax, group = yposition, fill = fill),
      inherit.aes = FALSE,
      alpha = 0.2   # sets transparency for filled areas
    ) +
    scale_fill_manual(
      values = c("a" = "transparent", "b" = "grey50"),
      guide = "none"   # hide legend if not needed
    )

  plot <- tag_facet(plot,
                    x = 0.045, y = Inf,
                    hjust = -0.4,
                    open = "", close = "",
                    fontface = 4,
                    size = 5,
                    family = "serif",
                    tag_pool = my_tag)
  
  return(plot)
  
  }

#forest plot combined model results
forest_year_further_mult <- function(df, df_dummy, pathogen, model_type,
                                     outcome_type) {
  
  pathogen <- if_else(pathogen == "overall_and_all_cause", "overall_resp",
                      pathogen)
  
  df_model <- df %>%
    filter(model_type == !!model_type,
           str_detect(model_name, outcome_type)) %>%
    group_by(codelist_type, subset)
  
  outcome <- if_else(outcome_type == "Mild",
                     paste0(pathogen, "_primary_inf"),
                     paste0(pathogen, "_secondary_inf"))
  
  exposure <- case_when(
    model_type == "ethnicity" ~ list("latest_ethnicity_group"),
    model_type == "ses" ~ list("imd_quintile"),
    model_type == "composition" ~ list("composition_category"),
    model_type == "ethnicity_ses" ~ list(c("latest_ethnicity_group",
                                           "imd_quintile")),
    model_type == "ethnicity_composition" ~ list(c("latest_ethnicity_group",
                                                   "composition_category")),
    model_type == "ses_composition" ~ list(c("imd_quintile",
                                             "composition_category")),
    model_type == "full" ~ list(c("latest_ethnicity_group",
                                  "imd_quintile",
                                  "composition_category"))
  )[[1]]
  
  offset <- case_when(
    outcome_type == "Mild" ~ paste0("time_", pathogen, "_primary"),
    outcome_type == "Severe" ~ paste0("time_", pathogen, "_secondary")
  )
  
  vacc_prev <- case_when(
    pathogen == "flu" ~ "prior_flu_vaccination",
    pathogen == "covid" ~ "time_since_last_covid_vaccination"
  )
  
  if (cohort == "infants" | cohort == "infants_subgroup") {
    
    dummy_model <- glm_poisson_further(
      df_dummy, exposure, outcome, offset_var = offset
    )
    
  } else {
    
    dummy_model <- glm_poisson_further(
      df_dummy, exposure, outcome, vacc_prev, offset_var = offset
    )
    
  }
  
  #define levels
  levels <- list()
  
  if (cohort == "infants" | cohort == "infants_subgroup") {
    
    levels <- c("12-23m", "6-11m", "3-5m", "0-2m", "Male", "Female")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female", "Male")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male")
    
  } else {
    
    levels <- c("90y+", "75-89y", "65-74y", "Male", "Female")
    
  }
  
  if (model_type == "ethnicity") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed",  "Other Ethnic Groups",
                  "Black or Black British", "Asian or Asian British",
                  "Mixed", "White"),
                levels)
    
  } else if (model_type == "ses") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "1 (most deprived)",
                  "2", "3", "4", "5 (least deprived)"), levels)
    
  } else if (model_type == "composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation"), levels)
    
  } else if (model_type == "ethnicity_ses") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "1 (most deprived)",
                  "2", "3", "4", "5 (least deprived)",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
    
  } else if (model_type == "ethnicity_composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
    
  } else if (model_type == "ses_composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "1 (most deprived)", "2", "3", "4",
                  "5 (least deprived)"), levels)
    
  } else if (model_type == "full" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Village and Dispersed",
                  "Rural Town and Fringe", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
    
  }
  
  if (cohort == "infants_subgroup") {
    
    levels <- c(levels, "Maternal Pertussis Vaccination",
                "Maternal Flu Vaccination", "Maternal Drug Usage",
                "Maternal Drinking", "Binary Variables (Reference)",
                "Current Smoker", "Former Smoker", "Never Smoker",
                "Maternal Age", "Maternal Age (Average)")
    
  } else if (cohort != "infants" & pathogen == "flu") {
    
    levels <- c("Flu Vaccination (Yes)", "Flu Vaccination (No)",
                "Eligible and Vaccinated Last Autumn",
                "Not Vaccinated in Past Year", levels)
    
  } else if (cohort != "infants" & pathogen == "covid") {
    
    levels <- c("Covid Vaccination (Yes)", "Covid Vaccination (No)",
                "Not Vaccinated in Past Year",
                "Eligible and Vaccinated Last Autumn",
                "Eligible and Vaccinated Last Spring", levels)
    
  }
  
  process_forest_plot <- function(df_model) {
    
    if (nrow(df_model) != 0) {
      
      tidy_forest <- df_model %>%
        filter(subset %in% c("2017_18", "2018_19", "2020_21", "2023_24")) %>%
        tidy_attach_model(dummy_model) %>%
        tidy_add_reference_rows() %>%
        tidy_add_estimate_to_reference_rows(exponentiate = TRUE,
                                            conf.level = 95) %>%
        tidy_add_term_labels() %>%
        tidy_remove_intercept() %>%
        mutate(
          conf.low = if_else(conf.low < 1e-100, NA_real_, conf.low),
          conf.high = if_else(conf.high < 1e-100, NA_real_, conf.high)
        ) %>%
        mutate(
          reference_row = if_else(var_type == "continuous", FALSE, reference_row)
        )
      
      if (cohort == "infants_subgroup") {
        
        continuous_vars <- tibble(
          term = "maternal_ageAverage",
          variable = "maternal_age",
          var_label = "maternal_age",
          var_class = "integer",
          var_type = "continuous",
          var_nlevels = 1,
          contrasts = "contr.treatment",
          contrasts_type = "treatment",
          reference_row = TRUE,
          label = "Maternal Age (Reference)",
          model_name = NA,
          estimate = 1,
          std.error = 0,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = NA
        )
        
        tidy_forest <- rbind(tidy_forest, continuous_vars)
        
      } else if (cohort != "infants" & cohort != "infants_subgroup" & pathogen == "flu") {
        
        continuous_vars <- tibble(
          term = "flu_vaccinationNo",
          variable = "flu_vaccination",
          var_label = "flu_vaccination",
          var_class = "integer",
          var_type = "continuous",
          var_nlevels = 1,
          contrasts = "contr.treatment",
          contrasts_type = "treatment",
          reference_row = TRUE,
          label = "Flu Vaccination (No)",
          model_name = NA,
          estimate = 1,
          std.error = 0,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = NA
        )
        
        tidy_forest <- rbind(tidy_forest, continuous_vars)
        
      } else if (cohort != "infants" & cohort != "infants_subgroup" &
                 pathogen == "covid") {
        
        continuous_vars <- tibble(
          term = "covid_vaccinationNo",
          variable = "covid_vaccination",
          var_label = "covid_vaccination",
          var_class = "integer",
          var_type = "continuous",
          var_nlevels = 1,
          contrasts = "contr.treatment",
          contrasts_type = "treatment",
          reference_row = TRUE,
          label = "Covid Vaccination (No)",
          model_name = NA,
          estimate = 1,
          std.error = 0,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = NA
        )
        
        tidy_forest <- rbind(tidy_forest, continuous_vars)
        
      }
      
    } else if (nrow(df_model) == 0 & model_type %in% c(
      "composition", "ethnicity_composition", "ses_composition", "full")) {
      
      age <- case_when(
        cohort == "older_adults" ~ "65-74y",
        cohort == "adults" ~ "18-39y",
        cohort == "children_and_adolescents" ~ "2-5y",
        cohort == "infants" ~ "0-2m",
        cohort == "infants_subgroup" ~ "0-2m"
      )
      
      vars <- case_when(
        model_type == "ethnicity" ~ list(c("sex", "age_band", "latest_ethnicity_group",
                                           "rurality_classification")),
        model_type == "ses" ~ list(c("sex", "age_band", "imd_quintile",
                                     "rurality_classification")),
        model_type == "composition" ~ list(c(
          "sex", "age_band", "composition_category", "rurality_classification")),
        model_type == "ethnicity_ses" ~ list(c(
          "sex", "age_band", "latest_ethnicity_group", "imd_quintile",
          "rurality_classification")),
        model_type == "ethnicity_composition" ~ list(c(
          "sex", "age_band", "ethnicity", "composition_category",
          "rurality_classification")),
        model_type == "ses_composition" ~ list(c(
          "sex", "age_band", "imd_quintile", "composition_category",
          "rurality_classification")),
        model_type == "full" ~ list(c(
          "sex", "age_band", "ethnicity", "imd_quintile", "composition_category",
          "rurality_classification"))
      )[[1]]
      
      var_labels <- case_when(
        model_type == "ethnicity" ~ list(c("Female", age, "White",
                                           "Rural Town and Fringe")),
        model_type == "ses" ~ list(c("Female", age, "5 (least deprived)",
                                     "Rural Town and Fringe")),
        model_type == "composition" ~ list(c(
          "Female", age, "Multiple of the Same Generation", "Rural Town and Fringe")),
        model_type == "ethnicity_ses" ~ list(c(
          "Female", age, "White", "5 (least deprived)", "Rural Town and Fringe")),
        model_type == "ethnicity_composition" ~ list(c(
          "Female", age, "White", "Multiple of the Same Generation",
          "Rural Town and Fringe")),
        model_type == "ses_composition" ~ list(c(
          "Female", age, "5 (least deprived)", "Multiple of the Same Generation",
          "Rural Town and Fringe")),
        model_type == "full" ~ list(c(
          "Female", age, "White", "5 (least deprived)", "Multiple of the Same Generation",
          "Rural Town and Fringe"))
      )[[1]]
      
      tidy_forest <- bind_rows(
        tibble(
          term = NA,
          variable = vars,
          var_label = var_labels,
          var_class = NA,
          var_type = NA,
          var_nlevels = NA_real_,
          contrasts = NA,
          contrasts_type = NA,
          reference_row = TRUE,
          label = var_labels,
          estimate = 1,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "specific",
          investigation_type = investigation_type,
          subset = "2020_21"
        ),
        tibble(
          term = NA,
          variable = vars,
          var_label = var_labels,
          var_class = NA,
          var_type = NA,
          var_nlevels = NA_real_,
          contrasts = NA,
          contrasts_type = NA,
          reference_row = TRUE,
          label = var_labels,
          estimate = 1,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "sensitive",
          investigation_type = investigation_type,
          subset = "2020_21"
        ),
        tibble(
          term = NA,
          variable = vars,
          var_label = var_labels,
          var_class = NA,
          var_type = NA,
          var_nlevels = NA_real_,
          contrasts = NA,
          contrasts_type = NA,
          reference_row = TRUE,
          label = var_labels,
          estimate = NA_real_,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = NA_real_,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = "2020_21"
        )
      )
      
    }
    
    if (cohort == "infants_subgroup") {
      
      binaries <- tidy_forest %>%
        filter(str_detect(term, "Yes")) %>%
        rowwise() %>%
        mutate(
          label = str_to_title(label)
        ) %>%
        rbind(tibble(
          term = "are_binary_variablesYes",
          variable = "binary_variables",
          var_label = "binary_variables",
          var_class = "factor",
          var_type = "dichotomous",
          var_nlevels = 2,
          contrasts = "contr.treatment",
          contrasts_type = "treatment",
          reference_row = TRUE,
          label = "Binary Variables (Reference)",
          model_name = NA,
          estimate = 1,
          std.error = 0,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = 1,
          conf.high = 1,
          model_type = !!model_type,
          codelist_type = "reference",
          investigation_type = investigation_type,
          subset = NA)
        ) 
      
      tidy_forest <- tidy_forest %>%
        filter(!(str_detect(term, "Yes"))) %>%
        filter(!(str_detect(term, "No"))) %>%
        bind_rows(binaries)
      
    }
    
    if (model_type %in% c(
      "composition", "ethnicity_composition", "ses_composition", "full")) {
      
      # Expand reference rows
      reference_rows <- tidy_forest %>%
        filter(reference_row) %>%
        mutate(subset = c("2020_21"),
               conf.low = 1,
               conf.high = 1
        ) %>%
        mutate(codelist_type = "reference")
      
    } else {
      
      # Expand reference rows
      reference_rows <- tidy_forest %>%
        filter(reference_row) %>%
        mutate(rn = row_number()) %>%
        slice(rep(1:n(), each = 4)) %>%
        group_by(rn) %>%
        mutate(
          subset = c("2017_18", "2018_19", "2020_21", "2023_24"),
          conf.low = 1,
          conf.high = 1
        ) %>%
        ungroup() %>%
        select(-rn) %>%
        mutate(codelist_type = "reference")
      
    }
    
    if (nrow(df_few) != 0 & model_type %in% c("composition", "ethnicity_composition",
                                              "ses_composition", "full")) {
      
      tidy_forest <- tidy_forest
      
    } else {
      
      tidy_forest <- tidy_forest %>%
        filter(!reference_row) %>%
        bind_rows(reference_rows)
      
    }
    
    legend_labels <- unique(str_to_title(gsub("_", " ", tidy_forest$variable)))
    
    cols2 <- tibble(
      variable = c("sex", "age_band", "latest_ethnicity_group", "imd_quintile",
                   "composition_category", "rurality_classification",
                   vacc_prev, "vax_status", "flu_vaccination",
                   "covid_vaccination", "maternal_age",
                   "maternal_smoking_status", "maternal_drinking",
                   "maternal_drug_usage", "maternal_flu_vaccination",
                   "maternal_pertussis_vaccination", "binary_variables"),
      col = c('#1f77b4', '#ffbb78', '#2ca02c', '#ff9896',
              '#aec7e8', '#ff7f0e',
              '#98df8a', '#d62728', '#d62728',
              '#d62728', '#9467bd',
              '#c49c94', '#e377c2',
              '#c5b0d5', '#8c564b',
              '#f7b6d2', "#4e3f2c"),
      labels = c("Sex", "Age Group", "Ethnicity", "IMD Quintile",
                 "Household Composition", "Rurality", "Prior Vaccination",
                 "Current Vaccination", "Current Vaccination",
                 "Current Vaccination", "Age", "Smoking Status",
                 "Drinking", "Drug Usage", "Flu Vaccination",
                 "Pertussis Vaccination", "Binary Variables")
    )
    
    cols_final <- cols2 %>%
      filter(variable %in% unique(tidy_forest$variable))
    
    cols_final2 <- full_join(cols_final, tidy_forest, by = "variable",
                             relationship = "one-to-many") %>%
      select(variable, subset, col, labels)
    
    tidy_forest <- merge(tidy_forest, cols_final2,
                         by = c("variable", "subset"))
    
    tidy_forest <- tidy_forest %>%
      mutate(
        shape = case_when(
          codelist_type == "reference" ~ 16,
          codelist_type == "specific" ~ 17,
          codelist_type == "sensitive" ~ 15,
        )
      )
    
    plot_labelling <- cols_final2$col
    names(plot_labelling) <- cols_final2$labels
    
    if (cohort == "infants_subgroup") {
      
      if (pathogen == "covid") {
        
        tidy_forest %>%
          mutate(
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              label == "Current" ~ "Current Smoker",
              label == "Former" ~ "Former Smoker",
              label == "Never" ~ "Never Smoker",
              TRUE ~ label
            )
          ) %>%
          mutate(
            plot_label = str_to_title(
              gsub("has ", "", gsub("_", " ", variable))),
            subset = gsub("_", "-", subset),
            faceting = case_when(
              variable %in% c("age_band", "sex", "latest_ethnicity_group",
                              "imd_quintile", "rurality_classification") ~
                "Infant", TRUE ~ "Maternal"
            )
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Comp") ~ "Household Composition",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              TRUE ~ plot_label)
          ) %>%
          mutate(
            label = if_else(label == "Yes", plot_label, label)
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          filter(subset %in% c("2020-21", "2023-24")) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_grid(faceting ~ subset, scales = "free_y", space = "free_y") + 
          labs(x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      } else {
        
        tidy_forest %>%
          mutate(
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              label == "Current" ~ "Current Smoker",
              label == "Former" ~ "Former Smoker",
              label == "Never" ~ "Never Smoker",
              TRUE ~ label
            )
          ) %>%
          mutate(
            plot_label = str_to_title(
              gsub("has ", "", gsub("_", " ", variable))),
            subset = gsub("_", "-", subset),
            faceting = case_when(
              variable %in% c("age_band", "sex", "latest_ethnicity_group",
                              "imd_quintile", "rurality_classification") ~
                "Infant", TRUE ~ "Maternal"
            )
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              TRUE ~ plot_label)
          ) %>%
          mutate(
            label = if_else(label == "Yes", plot_label, label)
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_grid(faceting ~ subset, scales = "free_y", space = "free_y") + 
          labs(x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      }
      
    } else {
      
      if (pathogen == "covid") {
        
        tidy_forest %>%
          mutate(
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = case_when(
              variable == "vax_status" ~ "Covid Vaccination (Yes)",
              variable == "time_since_last_covid_vaccination" ~ paste0(
                label, " Since Last Covid Vaccination"),
              TRUE ~ label
            ),
            subset = gsub("_", "-", subset)
          ) %>%
          mutate(
            variable = case_when(
              variable == "vax_status" ~ "covid_vaccination",
              TRUE ~ variable
            ),
            plot_label = str_to_title(gsub("_", " ", variable))
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Comp") ~ "Household Composition",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              str_detect(plot_label, "Time Since") ~ "Prior Vaccination",
              str_detect(plot_label, "Covid Vaccination") ~ "Current Vaccination",
              TRUE ~ plot_label)
          ) %>%
          mutate(
            label = case_when(
              str_detect(label, regex("^6-12m Since Last Covid Vaccination$")) ~ "Eligible and Vaccinated Last Autumn",
              str_detect(label, regex("^0-6m Since Last Covid Vaccination$")) ~ "Eligible and Vaccinated Last Spring",
              str_detect(label, "12m") ~ "Not Vaccinated in Past Year",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          filter(subset %in% c("2020-21", "2023-24")) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_wrap(~ subset, nrow = 1, ncol = 4) + 
          labs(x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      } else {
        
        tidy_forest %>%
          mutate(
            label = case_when(
              str_detect(term, "imd_quintile5") ~ "1 (most deprived)",
              str_detect(term, "imd_quintile4") ~ "2",
              str_detect(term, "imd_quintile2") ~ "4",
              str_detect(term, "imd_quintile1") ~ "5 (least deprived)",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = case_when(
              variable == "vax_status" ~ "Flu Vaccination (Yes)",
              variable == "prior_flu_vaccination" ~ paste0(
                "Prior Flu Vaccination (", str_to_title(label),")"),
              TRUE ~ label),
            subset = gsub("_", "-", subset)
          ) %>%
          mutate(
            variable = case_when(
              variable == "vax_status" ~ "flu_vaccination",
              TRUE ~ variable
            ),
            plot_label = str_to_title(gsub("_", " ", variable))
          ) %>%
          mutate(
            plot_label = case_when(
              str_detect(plot_label, "Age Band") ~ "Age Group",
              str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
              str_detect(plot_label, "Latest") ~ "Ethnicity",
              str_detect(plot_label, "Comp") ~ "Household Composition",
              str_detect(plot_label, "Rurality" ) ~ "Rurality",
              str_detect(plot_label, "Prior") ~ "Prior Vaccination",
              str_detect(plot_label, "Flu Vaccination") ~ "Current Vaccination",
              TRUE ~ plot_label),
            label = case_when(
              str_detect(label, "Prior") & str_detect(label, "(Yes)") ~ "Eligible and Vaccinated Last Autumn",
              str_detect(label, "Prior") & str_detect(label, "(No)") ~ "Not Vaccinated in Past Year",
              TRUE ~ label
            )
          ) %>%
          mutate(
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive")),
            labels = factor(labels, levels = c(unique(cols_final$labels)))
          ) %>%
          unique() %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = labels, shape = shape)) +
          scale_color_manual(name = "Characteristic", guide = "legend",
                             values = plot_labelling) +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10(breaks = c(0.1, 0.5, 2, 10)) +
          coord_cartesian(xlim = c(0.06, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = guide_legend("Characteristic", order = 2),
                 shape = guide_legend("Est. Type"), order = 1) +
          facet_wrap(~ subset, nrow = 1, ncol = 4) + 
          labs(x = "Rate Ratio", y = "") + theme_bw(base_size = 18) +
          theme(text = element_text(size = 14),
                strip.text.x = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black'),
                legend.spacing.y = unit(-0.25, "cm"))
        
      }
      
    }
    
  }
  
  if (pathogen == "covid") {
    my_tag <- c("2020-21", "2023-24")
  } else {
    my_tag <- c("2017-18", "2018-19", "2020-21", "2023-24")
  }
  
  plot <- process_forest_plot(df_model)
  
  dat_ribbon <- plot$data %>%
    select(variable, label, var_nlevels) %>%
    unique() %>%
    mutate(
      xmin = 0.055,
      xmax = 10,
      label = factor(label, levels = levels)
    ) %>%
    arrange(label) %>%
    mutate(
      yposition = seq_len(n()),
      ymin = yposition - 0.5,
      ymax = yposition + 0.5
    ) %>%
    # assign a group order based on where each variable first appears in the arranged data
    mutate(group_id = match(variable, unique(variable))) %>%
    mutate(fill = if_else(group_id %% 2 == 1, "a", "b"))

  dat_ribbon_long <- dat_ribbon %>%
    pivot_longer(cols = c(xmin, xmax), values_to = "x",
                 names_to = "xmin_xmax") %>%
    select(-xmin_xmax)

  plot <- plot +
    geom_ribbon(
      data = dat_ribbon_long,
      aes(x = x, ymin = ymin, ymax = ymax, group = yposition, fill = fill),
      inherit.aes = FALSE,
      alpha = 0.2   # sets transparency for filled areas
    ) +
    scale_fill_manual(
      values = c("a" = "transparent", "b" = "grey50"),
      guide = "none"   # hide legend if not needed
    )

  plot <- tag_facet(plot,
                    x = 0.045, y = Inf,
                    hjust = -0.4,
                    open = "", close = "",
                    fontface = 4,
                    size = 5,
                    family = "serif",
                    tag_pool = my_tag)
  
  return(plot)
  
}
