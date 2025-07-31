library(tidyverse)
library(here)
library(broom)
library(rlang)
library(broom.helpers)
library(cowplot)
library(stringr)

#import model functions
source(here::here("post_check", "functions", "model.R"))

options(scipen = 999)

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
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed", "1 (most deprived)",
                  "2", "3", "4", "5 (least deprived)"), levels)
    
  } else if (model_type == "composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation"), levels)
    
  } else if (model_type == "ethnicity_ses") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed", "1 (most deprived)",
                  "2", "3", "4", "5 (least deprived)",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
    
  } else if (model_type == "ethnicity_composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "Other Ethnic Groups", "Black or Black British",
                  "Asian or Asian British", "Mixed", "White"), levels)
    
  } else if (model_type == "ses_composition" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed", "Three Other Generations",
                  "Two Other Generations", "One Other Generation",
                  "Living Alone", "Multiple of the Same Generation",
                  "1 (most deprived)", "2", "3", "4",
                  "5 (least deprived)"), levels)
    
  } else if (model_type == "full" & cohort != "infants" &
             cohort != "infants_subgroup") {
    
    levels <- c(c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed", "Three Other Generations",
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
                "Maternal Age")
    
  } else if (cohort != "infants" & pathogen == "flu") {
    
    levels <- c("Prior Flu Vaccination (Yes)", "Prior Flu Vaccination (No)",
                "Flu Vaccination (Yes)", levels)
      
  } else if (cohort != "infants" & pathogen == "covid") {
      
    levels <- c("Covid Vaccination (Yes)", "12m+ Since Last Covid Vaccination",
                "6-12m Since Last Covid Vaccination",
                "0-6m Since Last Covid Vaccination", levels)
      
  }
  
  group_order <- case_when(
    
    model_type == "ethnicity" & cohort == "infants" ~ list(c(
      "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
      "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
      "Ethnicity (Reference)", "Ethnicity (Specific)", "Ethnicity (Sensitive)",
      "Rurality (Reference)", "Rurality (Specific)", "Rurality (Sensitive)")),
    
    model_type == "ses" & cohort == "infants" ~ list(c(
      "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
      "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
      "IMD Quintile (Reference)", "IMD Quintile (Specific)",
      "IMD Quintile (Sensitive)", "Rurality (Reference)",
      "Rurality (Specific)", "Rurality (Sensitive)")),
    
    model_type == "ethnicity_ses" & cohort == "infants" ~ list(c(
      "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
      "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
      "Ethnicity (Reference)", "Ethnicity (Specific)", "Ethnicity (Sensitive)",
      "IMD Quintile (Reference)", "IMD Quintile (Specific)",
      "IMD Quintile (Sensitive)", "Rurality (Reference)",
      "Rurality (Specific)", "Rurality (Sensitive)")),
    
    model_type == "ethnicity" & cohort == "infants_subgroup" ~ list(c(
      "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
      "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
      "Ethnicity (Reference)", "Ethnicity (Specific)", "Ethnicity (Sensitive)",
      "Rurality (Reference)", "Rurality (Specific)", "Rurality (Sensitive)",
      "Maternal Age (Specific)", "Maternal Age (Sensitive)",
      "Maternal Smoking Status (Reference)", "Maternal Smoking Status (Specific)",
      "Maternal Smoking Status (Sensitive)", "Maternal Drinking (Specific)",
      "Maternal Drinking (Sensitive)", "Maternal Drug Usage (Specific)",
      "Maternal Drug Usage (Sensitive)", "Maternal Flu Vaccination (Specific)",
      "Maternal Flu Vaccination (Sensitive)",
      "Maternal Pertussis Vaccination (Specific)",
      "Maternal Pertussis Vaccination (Sensitive)",
      "Binary Variables (Reference)")),
    
    model_type == "ses" & cohort == "infants_subgroup" ~ list(c(
      "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
      "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
      "IMD Quintile (Reference)", "IMD Quintile (Specific)",
      "IMD Quintile (Sensitive)", "Rurality (Reference)", "Rurality (Specific)",
      "Rurality (Sensitive)", "Maternal Age (Specific)",
      "Maternal Age (Sensitive)", "Maternal Smoking Status (Reference)",
      "Maternal Smoking Status (Specific)",
      "Maternal Smoking Status (Sensitive)", "Maternal Drinking (Specific)",
      "Maternal Drinking (Sensitive)", "Maternal Drug Usage (Specific)",
      "Maternal Drug Usage (Sensitive)", "Maternal Flu Vaccination (Specific)",
      "Maternal Flu Vaccination (Sensitive)",
      "Maternal Pertussis Vaccination (Specific)",
      "Maternal Pertussis Vaccination (Sensitive)",
      "Binary Variables (Reference)")),
    
    model_type == "ethnicity_ses" & cohort == "infants_subgroup" ~ list(c(
      "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
      "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
      "Ethnicity (Reference)", "Ethnicity (Specific)", "Ethnicity (Sensitive)",
      "IMD Quintile (Reference)", "IMD Quintile (Specific)",
      "IMD Quintile (Sensitive)", "Rurality (Reference)", "Rurality (Specific)",
      "Rurality (Sensitive)", "Maternal Age (Specific)",
      "Maternal Age (Sensitive)", "Maternal Smoking Status (Reference)",
      "Maternal Smoking Status (Specific)",
      "Maternal Smoking Status (Sensitive)", "Maternal Drinking (Specific)",
      "Maternal Drinking (Sensitive)", "Maternal Drug Usage (Specific)",
      "Maternal Drug Usage (Sensitive)", "Maternal Flu Vaccination (Specific)",
      "Maternal Flu Vaccination (Sensitive)",
      "Maternal Pertussis Vaccination (Specific)",
      "Maternal Pertussis Vaccination (Sensitive)",
      "Binary Variables (Reference)")),
    
    model_type %in% c("ethnicity", "ses", "composition", "ethnicity_ses",
                      "ethnicity_composition", "ses_composition", "full") & 
      cohort != "infants" & cohort != "infants_subgroup" &
      pathogen == "rsv" ~ list(c(
        "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
        "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
        "Ethnicity (Reference)", "Ethnicity (Specific)", "Ethnicity (Sensitive)",
        "IMD Quintile (Reference)", "IMD Quintile (Specific)",
        "IMD Quintile (Sensitive)", "Household Composition (Reference)",
        "Household Composition (Specific)", "Household Composition (Sensitive)",
        "Rurality (Reference)", "Rurality (Specific)", "Rurality (Sensitive)")),
    
    model_type %in% c("ethnicity", "ses", "composition", "ethnicity_ses",
                      "ethnicity_composition", "ses_composition", "full") & 
      cohort != "infants" & cohort != "infants_subgroup" &
      pathogen == "flu" ~ list(c(
        "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
        "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
        "Ethnicity (Reference)", "Ethnicity (Specific)", "Ethnicity (Sensitive)",
        "IMD Quintile (Reference)", "IMD Quintile (Specific)",
        "IMD Quintile (Sensitive)", "Household Composition (Reference)",
        "Household Composition (Specific)", "Household Composition (Sensitive)",
        "Rurality (Reference)", "Rurality (Specific)", "Rurality (Sensitive)",
        "Prior Flu Vaccination (Reference)", "Prior Flu Vaccination (Specific)",
        "Prior Flu Vaccination (Sensitive)", "Flu Vaccination (Reference)",
        "Flu Vaccination (Specific)", "Flu Vaccination (Sensitive)")),
    
    model_type %in% c("ethnicity", "ses", "composition", "ethnicity_ses",
                      "ethnicity_composition", "ses_composition", "full") & 
      cohort != "infants" & cohort != "infants_subgroup" &
      pathogen == "covid" ~ list(c(
        "Sex (Reference)", "Sex (Specific)", "Sex (Sensitive)",
        "Age Group (Reference)", "Age Group (Specific)", "Age Group (Sensitive)",
        "Ethnicity (Reference)", "Ethnicity (Specific)", "Ethnicity (Sensitive)",
        "IMD Quintile (Reference)", "IMD Quintile (Specific)",
        "IMD Quintile (Sensitive)", "Household Composition (Reference)",
        "Household Composition (Specific)", "Household Composition (Sensitive)",
        "Rurality (Reference)", "Rurality (Specific)", "Rurality (Sensitive)",
        "Time Since Last Covid Vaccination (Reference)",
        "Time Since Last Covid Vaccination (Specific)",
        "Time Since Last Covid Vaccination (Sensitive)",
        "Covid Vaccination (Reference)", "Covid Vaccination (Specific)",
        "Covid Vaccination (Sensitive)"))
    
  )[[1]]
  
  process_forest_plot <- function(df_model) {
    
    if (nrow(df_model) != 0) {
    
      tidy_forest <- df_model %>%
        filter(subset %in% c("2017_18", "2018_19", "2020_21", "2023_24")) %>%
        tidy_attach_model(dummy_model) %>%
        tidy_add_reference_rows() %>%
        tidy_add_estimate_to_reference_rows(exponentiate = TRUE) %>%
        tidy_add_term_labels() %>%
        tidy_remove_intercept() %>%
        mutate(
          conf.low = if_else(reference_row, 1, conf.low),
          conf.high = if_else(reference_row, 1, conf.high),
          label = case_when(
            label == "maternal_age" ~ "Maternal Age",
            label == "vax_status" ~ "Vaccination Status",
            TRUE ~ label),
          reference_row = case_when(
            label == "Maternal Age" ~ FALSE,
            label == "Vaccination Status" ~ FALSE,
            TRUE ~ reference_row)
        ) %>%
        mutate(
          conf.low = if_else(conf.low < 1e-100, NA_real_, conf.low),
          conf.high = if_else(conf.high < 1e-100, NA_real_, conf.high)
        )
      
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
        model_type == "ethnicity" ~ list(c("sex", "age_band", "latest_ethnicity_group")),
        model_type == "ses" ~ list(c("sex", "age_band", "imd_quintile")),
        model_type == "composition" ~ list(c(
          "sex", "age_band", "composition_category")),
        model_type == "ethnicity_ses" ~ list(c(
          "sex", "age_band", "latest_ethnicity_group", "imd_quintile")),
        model_type == "ethnicity_composition" ~ list(c(
          "sex", "age_band", "ethnicity", "composition_category")),
        model_type == "ses_composition" ~ list(c(
          "sex", "age_band", "imd_quintile", "composition_category")),
        model_type == "full" ~ list(c(
          "sex", "age_band", "ethnicity", "imd_quintile", "composition_category"))
      )[[1]]
      
      var_labels <- case_when(
        model_type == "ethnicity" ~ list(c("Female", age, "White")),
        model_type == "ses" ~ list(c("Female", age, "5 (least deprived)")),
        model_type == "composition" ~ list(c(
          "Female", age, "Multiple of the Same Generation")),
        model_type == "ethnicity_ses" ~ list(c(
          "Female", age, "White", "5 (least deprived)")),
        model_type == "ethnicity_composition" ~ list(c(
          "Female", age, "White", "Multiple of the Same Generation")),
        model_type == "ses_composition" ~ list(c(
          "Female", age, "5 (least deprived)",
          "Multiple of the Same Generation")),
        model_type == "full" ~ list(c(
          "Female", age, "White", "5 (least deprived)",
          "Multiple of the Same Generation"))
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
        mutate(subset = c("2020_21")) %>%
        mutate(codelist_type = "reference")
      
    } else {
      
      # Expand reference rows
      reference_rows <- tidy_forest %>%
        filter(reference_row) %>%
        mutate(rn = row_number()) %>%
        slice(rep(1:n(), each = 4)) %>%
        group_by(rn) %>%
        mutate(subset = c("2017_18", "2018_19", "2020_21", "2023_24")) %>%
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
                   vacc_prev, "vax_status", "maternal_age",
                   "maternal_smoking_status", "maternal_drinking",
                   "maternal_drug_usage", "maternal_flu_vaccination",
                   "maternal_pertussis_vaccination", "binary_variables"),
      col = c('#1f77b4', '#ffbb78', '#2ca02c', '#ff9896',
              '#aec7e8', '#ff7f0e',
              '#98df8a', '#d62728', '#9467bd',
              '#c49c94', '#e377c2',
              '#c5b0d5', '#8c564b',
              '#f7b6d2', "#4e3f2c")
    )
    
    cols_final <- cols2 %>%
      filter(variable %in% unique(tidy_forest$variable))
    
    cols_final2 <- full_join(cols_final, tidy_forest, by = "variable",
                             relationship = "one-to-many") %>%
      select(variable, subset, col)
    
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
            label = forcats::fct_relevel(label, levels)
          ) %>%
          mutate(
            plot_label2 = paste0(
              plot_label, " (", str_to_title(codelist_type), ")")
          ) %>%
          mutate(
            plot_label2 = factor(plot_label2, levels = group_order),
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive"))
          ) %>%
          filter(subset %in% c("2020-21", "2023-24")) %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = col, shape = shape)) +
          scale_color_identity() +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10() + coord_cartesian(xlim = c(0.01, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = "none", shape = guide_legend("Est. Type")) +
          facet_grid(faceting ~ subset, scales = "free_y", space = "free_y") + 
          labs(x = "", y = "") +
          theme_bw() + theme(text = element_text(size = 11))
        
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
            label = forcats::fct_relevel(label, levels)
          ) %>%
          mutate(
            plot_label2 = paste0(
                plot_label, " (", str_to_title(codelist_type), ")")
          ) %>%
          mutate(
            plot_label2 = factor(plot_label2, levels = group_order),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive"))
          ) %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = col, shape = shape)) +
          scale_color_identity() +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10() + coord_cartesian(xlim = c(0.01, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = "none", shape = guide_legend("Est. Type")) +
          facet_grid(faceting ~ subset, scales = "free_y", space = "free_y") + 
          labs(x = "", y = "") +
          theme_bw() + theme(text = element_text(size = 11))
        
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
            variable = case_when(
              variable == "vax_status" ~ "covid_vaccination",
              TRUE ~ variable
            ),
            plot_label = str_to_title(gsub("_", " ", variable)),
            label = case_when(
              variable == "covid_vaccination" ~ paste0(
                plot_label, " (Yes)"),
              variable == "time_since_last_covid_vaccination" ~ paste0(
                label, " Since Last Covid Vaccination"),
              TRUE ~ label
              ),
            subset = gsub("_", "-", subset)
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
            plot_label2 = paste0(
              plot_label, " (", str_to_title(codelist_type), ")")
          ) %>%
          mutate(
            plot_label2 = factor(plot_label2, levels = group_order),
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive"))
          ) %>%
          filter(subset %in% c("2020-21", "2023-24")) %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = col, shape = shape)) +
          scale_color_identity() +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10() + coord_cartesian(xlim = c(0.01, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = "none", shape = guide_legend("Est. Type")) +
          facet_wrap(~ subset, nrow = 1, ncol = 4) + 
          labs(x = "", y = "") +
          theme_bw() + theme(text = element_text(size = 11))
        
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
            variable = case_when(
              variable == "vax_status" ~ "flu_vaccination",
              TRUE ~ variable
            ),
            plot_label = str_to_title(gsub("_", " ", variable)),
            label = case_when(
              variable == "flu_vaccination" ~ paste0(plot_label, " (Yes)"),
              variable == "prior_flu_vaccination" ~ paste0(
                plot_label, "(", str_to_title(label),")"),
              TRUE ~ label),
            subset = gsub("_", "-", subset)
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
            plot_label2 = paste0(
              plot_label, " (", str_to_title(codelist_type), ")")
          ) %>%
          mutate(
            plot_label2 = factor(plot_label2, levels = group_order),
            label = forcats::fct_relevel(label, levels),
            shape_order = factor(str_to_title(codelist_type), levels = c(
              "Reference", "Specific", "Sensitive"))
          ) %>%
          ggplot(aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high,
                     color = col, shape = shape)) +
          scale_color_identity() +
          scale_shape_identity(name = "Est. Type", guide = "legend",
                               breaks = c(16, 17, 15), labels = c(
                                 "Reference", "Specific", "Sensitive")) +
          geom_vline(xintercept = 1, linetype = 2) +
          scale_x_log10() + coord_cartesian(xlim = c(0.01, 10)) +
          geom_pointrange(position = position_dodge(width = 0.75), size = 0.5) +
          guides(color = "none", shape = guide_legend("Est. Type")) +
          facet_wrap(~ subset, nrow = 1, ncol = 4) + 
          labs(x = "", y = "") +
          theme_bw() + theme(text = element_text(size = 11))
        
      }
      
    }
    
  }
  
  plot <- process_forest_plot(df_model)
  
  return(plot)
  
}
