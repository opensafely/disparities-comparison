library(here)
library(broom)
library(rlang)
library(broom.helpers)
library(cowplot)
library(stringr)
library(RColorBrewer)
library(khroma)
library(paletteer)

## create output directories ----
fs::dir_create(here::here("post_check", "functions"))

#import model functions
source(here::here("post_check", "functions", "model.R"))

#create function to filter collated results to models wanted and then plot
forest <- function(df, df_dummy, pathogen, model_type, outcome_type) {
  
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
  
  dummy_model <- glm_poisson(
    df_dummy, exposure, outcome, offset
  )
  
  process_forest <- function(df_model, codelist_filter) {
    df_model %>%
      filter(codelist_type == codelist_filter) %>%
      tidy_attach_model(dummy_model) %>%
      tidy_add_reference_rows() %>%
      tidy_add_estimate_to_reference_rows(exponentiate = TRUE,
                                          conf.level = 95) %>%
      tidy_add_term_labels() %>%
      tidy_remove_intercept() %>%
      mutate(conf.low = if_else(reference_row, 1, conf.low), 
             conf.high = if_else(reference_row, 1, conf.high))
    
  }
  
  tidy_forest_spec <- process_forest(df_model, "specific")
  
  if (investigation_type == "primary") {
    
    tidy_forest_sens <- process_forest(df_model, "sensitive")
    conf_low <- min(tidy_forest_spec$conf.low, tidy_forest_sens$conf.low)
    conf_high <- max(tidy_forest_spec$conf.high, tidy_forest_sens$conf.high)
    
  } else {
    
    conf_low <- min(tidy_forest_spec$conf.low)
    conf_high <- max(tidy_forest_spec$conf.high)
    
  }
  
  #define levels
  levels <- list()
  
  if (cohort == "infants" | cohort == "infants_subgroup") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female", "Male")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male")
    
  } else if (cohort == "older_adults") {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male")
    
  }
  
  if (str_detect(model_type, "ethnicity")) {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups"),
                levels)
    
  }
  
  if (str_detect(model_type, "ses")) {
    
    levels <- c(c("1 (least deprived)", "2", "3", "4",
                  "5 (most deprived)"), levels)
    
  }
  
  if (str_detect(model_type, "composition") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("Multiple of the Same Generation",
                  "Living Alone", "One Other Generation",
                  "Two Other Generations", "Three Other Generations"),
                levels)
    
  }
  
  if (str_detect(model_type, "full") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups",
                  "1 (least deprived)", "2", "3", "4",
                  "5 (most deprived)", "Multiple of the Same Generation",
                  "Living Alone", "One Other Generation",
                  "Two Other Generations", "Three Other Generations"),
                levels)
    
  }
  
  if (investigation_type == "secondary") {
    
    levels <- c(levels, "No", "Yes", "Never", "Current", "Former")
    
  }
  
  make_forest_plot <- function(tidy_forest, shape_value, title_suffix) {
    
    pathogen_title <- case_when(
      pathogen == "rsv" ~ "RSV",
      pathogen == "flu" ~ "Influenza",
      pathogen == "covid" ~ "COVID-19"
    )
    
    plot_label_order <- case_when(
      investigation_type == "secondary" ~ list(
        c("Sex", "Age Group", "Ethnicity", "IMD Quintile",
          "Household Composition", "Has Asthma", "Has COPD",
          "Has Cystic Fibrosis", "Has Other Resp", "Has Diabetes",
          "Has Heart Disease", "Has Addisons", "Severe Obesity", "Has CHD",
          "Has CLD", "Has CKD", "Has CND", "Cancer Within 3 Yrs",
          "Immunosuppressed", "Has Sickle Cell", "Smoking Status",
          "Hazardous Drinking", "Drug Usage")),
      TRUE ~ list(
        c("Sex", "Age Group", "Ethnicity", "IMD Quintile",
          "Household Composition"))
    )[[1]]
    
    plot_label_order <- factor(plot_label_order, levels = plot_label_order)
    
    references <- tidy_forest %>%
      filter(reference_row) %>%
      select(variable, label, estimate, conf.low, conf.high) %>%
      mutate(
        plot_label = str_to_title(gsub("_", " ", variable))
      ) %>%
      mutate(
        plot_label = case_when(
          str_detect(plot_label, "Age") ~ "Age Group",
          str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
          str_detect(plot_label, "Latest") ~ "Ethnicity",
          str_detect(plot_label, "Comp") ~ "Household Composition",
          str_detect(plot_label, "Cancer") ~ "Cancer Within 3 Yrs",
          str_detect(plot_label, "Chd") ~ "Has CHD",
          str_detect(plot_label, "Ckd") ~ "Has CKD",
          str_detect(plot_label, "Cld") ~ "Has CLD",
          str_detect(plot_label, "Cnd") ~ "Has CND",
          str_detect(plot_label, "Copd") ~ "Has COPD",
          TRUE ~ plot_label)
      ) %>%
      mutate(across(plot_label, ~factor(., levels = plot_label_order)))
    
    if (investigation_type == "primary") {
      
      cc <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0, 1, length.out = 8))
      
    } else {
      
      cc_full <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0, 1, length.out = 8))
      
      cc <- case_when(
        pathogen == "rsv" ~ cc_full[2],
        pathogen == "flu" ~ cc_full[3],
        pathogen == "covid" ~ cc_full[5]
      )
      
    }
    
    tidy_forest %>%
      mutate(
        plot_label = str_to_title(gsub("_", " ", variable)),
        label = forcats::fct_relevel(label, levels),
        subset = str_to_title(gsub("_", "-", subset))
      ) %>%
      mutate(
        plot_label = case_when(
          str_detect(plot_label, "Age") ~ "Age Group",
          str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
          str_detect(plot_label, "Latest") ~ "Ethnicity",
          str_detect(plot_label, "Comp") ~ "Household Composition",
          str_detect(plot_label, "Cancer") ~ "Cancer Within 3 Yrs",
          str_detect(plot_label, "Chd") ~ "Has CHD",
          str_detect(plot_label, "Ckd") ~ "Has CKD",
          str_detect(plot_label, "Cld") ~ "Has CLD",
          str_detect(plot_label, "Cnd") ~ "Has CND",
          str_detect(plot_label, "Copd") ~ "Has COPD",
          TRUE ~ plot_label)
      ) %>%
      mutate(across(plot_label, ~factor(., levels = plot_label_order))) %>%
      mutate(reference_row = NA) %>%
      ggplot(aes(y = label, x = estimate, xmin = conf.low,
                 xmax = conf.high, color = subset)) +
      scale_color_manual(values = cc, na.translate = F) +
      geom_vline(xintercept = 1, linetype = 2) + 
      geom_pointrange(position = position_dodge(width = 0.5), size = 0.45,
                      shape = shape_value) +
      geom_point(data = references, aes(y = label, x = estimate,
                                        shape = as.factor(estimate)),
                 size = 1, stroke = 1, color = "#4e3f2c") +
      scale_shape_manual(name = "", values = c(8),
                         labels = "Reference Category") +
      guides(color = guide_legend("Season", order = 1,
                                  override.aes = list(shape = shape_value))) +
      facet_wrap(plot_label~., scales = "free_y") + 
      labs(x = "Rate Ratio", y = " ",
           title = paste0("Rate Ratios of ", outcome_type, " ", 
                          pathogen_title, " by Group (", title_suffix, ")"),
           subtitle = paste0(str_to_title(gsub("_", " ", model_type)))) + 
      scale_x_log10(limits = c(conf_low, conf_high)) +
      theme_bw() + theme(title = element_text(size = 12),
                         axis.text = element_text(size = 10),
                         axis.title = element_text(size = 10),
                         legend.text = element_text(size = 10),
                         plot.tag.position = "topright",
                         plot.tag = element_text(hjust = 0, size = 9))
    
  }
  
  spec_plot <- make_forest_plot(tidy_forest_spec, shape_value = 16, 
                                title_suffix = "Specific Phenotype")

  if (investigation_type == "primary") {
    
    sens_plot <- make_forest_plot(tidy_forest_sens, shape_value = 17, 
                                  title_suffix = "Sensitive Phenotype")
    
    return(list(spec_plot = spec_plot, sens_plot = sens_plot))
    
  } else {
    
    return(list(spec_plot = spec_plot))
    
  }
  
}

forest_year <- function(df, df_dummy, pathogen, model_type, outcome_type) {
  
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
  
  dummy_model <- glm_poisson(
    df_dummy, exposure, outcome, offset
  )
  
  #define levels
  levels <- list()
  
  if (cohort == "infants" | cohort == "infants_subgroup") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female", "Male")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male")
    
  } else if (cohort == "older_adults") {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male")
    
  }
  
  if (str_detect(model_type, "ethnicity")) {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups"),
                levels)
    
  }
  
  if (str_detect(model_type, "ses")) {
    
    levels <- c(c("1 (least deprived)", "2", "3", "4",
                  "5 (most deprived)"), levels)
    
  }
  
  if (str_detect(model_type, "composition") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("Multiple of the Same Generation",
                  "Living Alone", "One Other Generation",
                  "Two Other Generations", "Three Other Generations"),
                levels)
    
  }
  
  if (str_detect(model_type, "full") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups",
                  "1 (least deprived)", "2", "3", "4",
                  "5 (most deprived)", "Multiple of the Same Generation",
                  "Living Alone", "One Other Generation",
                  "Two Other Generations", "Three Other Generations"),
                levels)
    
  }
  
  if (investigation_type == "secondary") {
    
    levels <- c(levels, "Asthma (No)", "Asthma (Yes)", "COPD (No)",
                "COPD (Yes)", "Cystic Fibrosis (No)", "Cystic Fibrosis (Yes)",
                "Other Resp (No)", "Other Resp (Yes)", "Diabetes (No)",
                "Diabetes (Yes)", "Addisons (No)", "Addisons (Yes)",
                "Severe Obesity (No)", "Severe Obesity (Yes)", "CHD (No)",
                "CHD (Yes)", "CLD (No)", "CLD (Yes)", "CKD (No)", "CKD (Yes)",
                "CND (No)", "CND (Yes)", "Cancer Within 3 Yrs (No)",
                "Cancer Within 3 Yrs (Yes)", "Immunosuppressed (No)",
                "Immunosuppressed (Yes)", "Sickle Cell (No)",
                "Sickle Cell (Yes)", "Never", "Current", "Former",
                "Hazardous Drinking (No)", "Hazardous Drinking (Yes)",
                "Drug Usage (No)", "Drug Usage (Yes)")
    
  }
  
  group_order <- case_when(
    model_type == "ethnicity" ~ list(c("Sex (False)", "Sex (True)",
                                       "Age Group (False)", "Age Group (True)",
                                       "Ethnicity (False)", "Ethnicity (True)")),
    model_type == "ses" ~ list(c("Sex (False)", "Sex (True)",
                                 "Age Group (False)", "Age Group (True)",
                                 "IMD Quintile (False)", "IMD Quintile (True)")),
    model_type == "composition" ~ list(c("Sex (False)", "Sex (True)",
                                         "Age Group (False)", "Age Group (True)",
                                         "Household Composition (False)",
                                         "Household Composition (True)")),
    model_type == "ethnicity_ses" ~ list(c("Sex (False)", "Sex (True)",
                                           "Age Group (False)",
                                           "Age Group (True)",
                                           "Ethnicity (False)",
                                           "Ethnicity (True)",
                                           "IMD Quintile (False)",
                                           "IMD Quintile (True)")),
    model_type == "ethnicity_composition" ~ list(c(
      "Sex (False)", "Age Group (False)", "Ethnicity (False)",
      "Household Composition (False)", "Sex (True)", "Age Group (True)",
      "Ethnicity (True)", "Household Composition (True)")),
    model_type == "ses_composition" ~ list(c("Sex (False)", "Sex (True)",
                                             "Age Group (False)",
                                             "Age Group (True)",
                                             "IMD Quintile (False)",
                                             "IMD Quintile (True)",
                                             "Household Composition (False)",
                                             "Household Composition (True)")),
    model_type == "full" ~ list(c("Sex (False)", "Sex (True)",
                                  "Age Group (False)", "Age Group (True)",
                                  "Ethnicity (False)", "Ethnicity (True)",
                                  "IMD Quintile (False)", "IMD Quintile (True)",
                                  "Household Composition (False)",
                                  "Household Composition (True)"))
  )[[1]]
  
  if (investigation_type == "secondary") {
    
    group_order <- c(group_order,
                     "Has Asthma (False)", "Has Asthma (True)",
                     "Has COPD (False)", "Has COPD (True)",
                     "Has Cystic Fibrosis (False)", "Has Cystic Fibrosis (True)",
                     "Has Other Resp (False)", "Has Other Resp (True)",
                     "Has Diabetes (False)", "Has Diabetes (True)",
                     "Has Addisons (False)", "Has Addisons (True)",
                     "Severe Obesity (False)", "Severe Obesity (True)",
                     "Has CHD (False)", "Has CHD (True)", "Has CLD (False)",
                     "Has CLD (True)", "Has CKD (False)", "Has CKD (True)",
                     "Has CND (False)", "Has CND (True)",
                     "Cancer Within 3 Yrs (False)",
                     "Cancer Within 3 Yrs (True)", "Immunosuppressed (False)",
                     "Immunosuppressed (True)", "Has Sickle Cell (False)",
                     "Has Sickle Cell (True)", "Smoking Status (False)",
                     "Smoking Status (True)", "Hazardous Drinking (False)",
                     "Hazardous Drinking (True)", "Drug Usage (False)",
                     "Drug Usage (True)")
    
  }
  
  process_forest_plot <- function(df_model, codelist_filter, shape_value,
                                  title_label) {
    
    tidy_forest <- df_model %>%
      filter(codelist_type == codelist_filter) %>%
      tidy_attach_model(dummy_model) %>%
      tidy_add_reference_rows() %>%
      tidy_add_estimate_to_reference_rows(exponentiate = TRUE,
                                          conf.level = 95) %>%
      tidy_add_term_labels() %>%
      tidy_remove_intercept() %>%
      mutate(
        conf.low = if_else(reference_row, 1, conf.low),
        conf.high = if_else(reference_row, 1, conf.high)
      )
    
    binaries <- tidy_forest %>%
      filter(label %in% c("Yes", "No")) %>%
      rowwise() %>%
      mutate(
        label = paste0(unique(str_to_title(gsub("_", " ",
                       gsub("has_", "", variable)))),
                       " (", label, ")")
      ) %>%
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
      )
    
    tidy_forest <- tidy_forest %>%
      filter(!(label %in% c("Yes", "No"))) %>%
      bind_rows(binaries)
    
    if (investigation_type == "primary") {
      
      # Expand reference rows
      reference_rows <- tidy_forest %>%
        filter(reference_row) %>%
        mutate(rn = row_number()) %>%
        slice(rep(1:n(), each = 8)) %>%
        group_by(rn) %>%
        mutate(subset = c("2016_17", "2017_18", "2018_19", "2019_20",
                          "2020_21", "2021_22", "2022_23", "2023_24")) %>%
        ungroup() %>%
        select(-rn)
      
    } else {
      
      if (pathogen == "rsv") {
        
        # Expand reference rows
        reference_rows <- tidy_forest %>%
          filter(reference_row) %>%
          mutate(subset = c("2017_18"))
        
      } else if (pathogen == "flu") {
        
        # Expand reference rows
        reference_rows <- tidy_forest %>%
          filter(reference_row) %>%
          mutate(subset = c("2018_19"))
        
      } else {
        
        # Expand reference rows
        reference_rows <- tidy_forest %>%
          filter(reference_row) %>%
          mutate(subset = c("2020_21"))
        
      }
      
    }
    
    tidy_forest <- tidy_forest %>%
      filter(!reference_row) %>%
      bind_rows(reference_rows)
    
    legend_labels <- unique(str_to_title(gsub("_", " ", tidy_forest$variable)))
    
    cols2 <- tibble(
      var = c("sex", "age_band", "latest_ethnicity_group", "imd_quintile",
              "composition_category", "has_asthma", "has_copd",
              "has_cystic_fibrosis", "has_other_resp", "has_diabetes",
              "has_addisons", "severe_obesity", "has_chd", "has_ckd", "has_cld",
              "has_cnd", "has_cancer", "immunosuppressed", "has_sickle_cell",
              "smoking_status", "hazardous_drinking", "drug_usage"),
      col = c("#f64883", "#50edb2", "#43006f", "#b1e466",
              "#8e0077", "#d66dbe", "#50873c",
              "#b97fd4", "#cc8331", "#628bd5",
              "#d45e46", "#38dbda", "#e1556e", "#a1863d", "#952a5e",
              "#9b4729", "#dd6a9c", "#ad4248", "#ac4258",
              "#6a70d7", "#81307a", "#45ba8a")
    )
    
    cols_final <- cols2 %>%
      filter(var %in% unique(tidy_forest$variable)) %>%
      select(col) %>%
      slice(rep(1:n(), each = 2))
    
    if (investigation_type == "secondary") {
      
      tidy_forest %>%
        mutate(
          plot_label = str_to_title(gsub("_", " ", variable)),
          label = forcats::fct_relevel(label, levels),
          faceting = case_when(
            variable %in% c("age_band", "sex", "latest_ethnicity_group",
                            "imd_quintile", "composition_category") ~ "Baseline",
            TRUE ~ "Secondary"
          )
        ) %>%
        mutate(
          plot_label = case_when(
            str_detect(plot_label, "Age") ~ "Age Group",
            str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
            str_detect(plot_label, "Latest") ~ "Ethnicity",
            str_detect(plot_label, "Cancer") ~ "Cancer Within 3 Yrs",
            str_detect(plot_label, "Comp") ~ "Household Composition",
            str_detect(plot_label, "Chd") ~ "Has CHD",
            str_detect(plot_label, "Ckd") ~ "Has CKD",
            str_detect(plot_label, "Cld") ~ "Has CLD",
            str_detect(plot_label, "Cnd") ~ "Has CND",
            str_detect(plot_label, "Copd") ~ "Has COPD",
            TRUE ~ plot_label)
        ) %>%
        mutate(
          plot_label2 = paste0(plot_label, " (", str_to_title(reference_row), ")")
        ) %>%
        mutate(
          plot_label2 = factor(plot_label2, levels = group_order)
        ) %>%
        ggplot(aes(y = label, x = estimate, xmin = conf.low,
                   xmax = conf.high, color = plot_label2, shape = plot_label2)) +
        scale_color_manual(values = cols_final$col,
                           name = "Characteristic (Reference)") +
        scale_shape_manual(name = "Characteristic (Reference)",
                           values = c(rep(c(shape_value, 8),
                                          length(legend_labels)))) +
        geom_vline(xintercept = 1, linetype = 2) + scale_x_log10() +
        geom_pointrange(position = position_dodge(width = 0.5), size = 0.2) +
        guides(color = guide_legend("Characteristic (Reference)", nrow = 22),
               shape = guide_legend("Characteristic (Reference)", nrow = 22)) +
        facet_wrap(~ faceting, scales = "free_y", ncol = 2) + 
        labs(x = "Rate Ratio", y = " ", title = title_label,
             subtitle = paste0(str_to_title(gsub("_", " ", model_type)))) +
        theme_bw() + theme(title = element_text(size = 12),
                           axis.text = element_text(size = 10),
                           axis.title = element_text(size = 10),
                           legend.text = element_text(size = 10))
      
    } else {
      
      tidy_forest %>%
        mutate(
          plot_label = str_to_title(gsub("_", " ", variable)),
          label = forcats::fct_relevel(label, levels),
          subset = str_to_title(gsub("_", "-", subset))
        ) %>%
        mutate(
          plot_label = case_when(
            str_detect(plot_label, "Age") ~ "Age Group",
            str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
            str_detect(plot_label, "Latest") ~ "Ethnicity",
            str_detect(plot_label, "Cancer") ~ "Cancer Within 3 Yrs",
            str_detect(plot_label, "Comp") ~ "Household Composition",
            str_detect(plot_label, "Chd") ~ "Has CHD",
            str_detect(plot_label, "Ckd") ~ "Has CKD",
            str_detect(plot_label, "Cld") ~ "Has CLD",
            str_detect(plot_label, "Cnd") ~ "Has CND",
            str_detect(plot_label, "Copd") ~ "Has COPD",
            TRUE ~ plot_label)
        ) %>%
        mutate(
          plot_label2 = paste0(plot_label, " (", str_to_title(reference_row), ")")
        ) %>%
        mutate(
          plot_label2 = factor(plot_label2, levels = group_order)
        ) %>%
        ggplot(aes(y = label, x = estimate, xmin = conf.low,
                   xmax = conf.high, color = plot_label2, shape = plot_label2)) +
        scale_color_manual(values = cols_final$col,
                           name = "Characteristic (Reference)") +
        scale_shape_manual(name = "Characteristic (Reference)",
                           values = c(rep(c(shape_value, 8),
                                          length(legend_labels)))) +
        geom_vline(xintercept = 1, linetype = 2) + scale_x_log10() +
        geom_pointrange(position = position_dodge(width = 0.5), size = 0.2) +
        guides(color = guide_legend("Characteristic (Reference)"),
               shape = guide_legend("Characteristic (Reference)")) +
        facet_wrap(~ subset, scales = "free_y", nrow = 2) + 
        labs(x = "Rate Ratio", y = " ", title = title_label,
             subtitle = paste0(str_to_title(gsub("_", " ", model_type)))) +
        theme_bw() + theme(title = element_text(size = 12),
                           axis.text = element_text(size = 10),
                           axis.title = element_text(size = 10),
                           legend.text = element_text(size = 10))
      
    }
    
  }
  
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  # Generate plots separately
  spec_plot <- process_forest_plot(
    df_model, "specific", shape_value = 16,
    title_label = paste0("Rate Ratios of ", outcome_type, " ", pathogen_title,
                         " (Specific Phenotype)"))

  if (investigation_type == "primary") {
    
    sens_plot <- process_forest_plot(
    df_model, "sensitive", shape_value = 17,
    title_label = paste0("Rate Ratios of ", outcome_type, " ", pathogen_title,
                         " (Sensitive Phenotype)"))  
    
    return(list(spec = spec_plot, sens = sens_plot))
    
  } else {
    
    return(list(spec = spec_plot))
    
  }
  
}

#create function to filter collated results to models wanted and then plot
forest_further <- function(df, df_dummy, pathogen, model_type, outcome_type) {
  
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
  
  vacc_current <- case_when(
    pathogen == "flu" & outcome_type == "Mild" ~ "flu_vaccination_mild",
    pathogen == "flu" & outcome_type == "Severe" ~ "flu_vaccination_severe",
    pathogen == "covid" & outcome_type == "Mild" ~ "covid_vaccination_mild",
    pathogen == "covid" & outcome_type == "Severe" ~ "covid_vaccination_severe"
  )
  
  dummy_model <- glm_poisson_further(
    df_dummy, exposure, outcome, offset, vacc_prev, vacc_current
  )
  
  process_forest <- function(df_model, codelist_filter) {
    df_model %>%
      filter(codelist_type == codelist_filter) %>%
      tidy_attach_model(dummy_model) %>%
      tidy_add_reference_rows() %>%
      tidy_add_estimate_to_reference_rows(exponentiate = TRUE,
                                          conf.level = 95) %>%
      tidy_add_term_labels() %>%
      tidy_remove_intercept() %>%
      mutate(conf.low = if_else(reference_row, 1, conf.low), 
             conf.high = if_else(reference_row, 1, conf.high))
    
  }
  
  tidy_forest_spec <- process_forest(df_model, "specific")
  tidy_forest_sens <- process_forest(df_model, "sensitive")
  
  conf_low <- min(tidy_forest_spec$conf.low, tidy_forest_sens$conf.low)
  conf_high <- max(tidy_forest_spec$conf.high, tidy_forest_sens$conf.high)
  
  #define levels
  levels <- list()
  
  if (cohort == "infants" | cohort == "infants_subgroup") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female", "Male")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male")
    
  } else if (cohort == "older_adults") {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male")
    
  }
  
  if (str_detect(model_type, "ethnicity")) {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups"),
                levels, c("Urban Major Conurbation", "Urban Minor Conurbation",
                          "Urban City and Town", "Rural Town and Fringe",
                          "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "ses")) {
    
    levels <- c(c("1 (least deprived)", "2", "3", "4",
                  "5 (most deprived)"), levels,
                c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "composition") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("Multiple of the Same Generation",
                  "Living Alone", "One Other Generation",
                  "Two Other Generations"), levels,
                c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "full") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups",
                  "1 (least deprived)", "2", "3", "4",
                  "5 (most deprived)", "Multiple of the Same Generation",
                  "Living Alone", "One Other Generation",
                  "Two Other Generations", "Three Other Generations"),
                levels, c("Urban Major Conurbation", "Urban Minor Conurbation",
                          "Urban City and Town", "Rural Town and Fringe",
                          "Rural Village and Dispersed"))
    
  }
  
  if (cohort == "infants_subgroup") {
    
    levels <- c(levels, "Maternal Age", "Never", "Former", "Current",
                "No", "Yes")
    
  } else if (cohort != "infants" & pathogen == "flu") {
    
    levels <- c(levels, "No", "Yes")
    
  } else if (cohort != "infants" & pathogen == "covid") {
    
    levels <- c(levels, "No", "Yes", "0-6m", "6-12m", "12m+")
    
  }
  
  make_forest_plot <- function(tidy_forest, shape_value, title_suffix) {
    
    pathogen_title <- case_when(
      pathogen == "rsv" ~ "RSV",
      pathogen == "flu" ~ "Influenza",
      pathogen == "covid" ~ "COVID-19"
    )
    
    references <- tidy_forest %>%
      filter(reference_row) %>%
      select(variable, label, estimate, conf.low, conf.high) %>%
      mutate(
        plot_label = str_to_title(gsub("_", " ", variable))
      ) %>%
      mutate(
        plot_label = case_when(
          str_detect(plot_label, "Age") ~ "Age Group",
          str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
          str_detect(plot_label, "Latest") ~ "Ethnicity",
          str_detect(plot_label, "Comp") ~ "Household Composition",
          TRUE ~ plot_label)
      ) %>%
      mutate(across(plot_label, ~factor(., levels = c(
        "Sex", "Age Group", "Ethnicity", "IMD Quintile",
        "Household Composition"))))
    
    if (investigation_type == "primary") {
      
      cc <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0, 1, length.out = 8))
      
    } else {
      
      cc_full <- scales::seq_gradient_pal(
        "#F05039", "#1F449c", "Lab")(seq(0, 1, length.out = 8))
      
      cc <- case_when(
        pathogen == "rsv" ~ cc_full[2],
        pathogen == "flu" ~ cc_full[3],
        pathogen == "covid" ~ cc_full[5]
      )
      
    }
    
    tidy_forest %>%
      mutate(
        plot_label = str_to_title(gsub("_", " ", variable)),
        label = forcats::fct_relevel(label, levels),
        subset = str_to_title(gsub("_", "-", subset))
      ) %>%
      mutate(
        plot_label = case_when(
          str_detect(plot_label, "Age") ~ "Age Group",
          str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
          str_detect(plot_label, "Latest") ~ "Ethnicity",
          str_detect(plot_label, "Comp") ~ "Household Composition",
          TRUE ~ plot_label)
      ) %>%
      mutate(across(plot_label, ~factor(., levels = c(
        "Sex", "Age Group", "Ethnicity", "IMD Quintile",
        "Household Composition")))) %>%
      mutate(reference_row = NA) %>%
      ggplot(aes(y = label, x = estimate, xmin = conf.low,
                 xmax = conf.high, color = subset)) +
      scale_color_manual(values = cc, na.translate = F) +
      geom_vline(xintercept = 1, linetype = 2) + 
      geom_pointrange(position = position_dodge(width = 0.5), size = 0.45,
                      shape = shape_value) +
      geom_point(data = references, aes(y = label, x = estimate,
                                        shape = as.factor(estimate)),
                 size = 1, stroke = 1, color = "deeppink") +
      scale_shape_manual(name = "", values = c(8),
                         labels = "Reference Category") +
      guides(color = guide_legend("Season", order = 1,
                                  override.aes = list(shape = shape_value))) +
      facet_wrap(~ plot_label, scales = "free_y") + 
      labs(x = "Rate Ratio", y = " ",
           title = paste0("Rate Ratios of ", outcome_type, " ", 
                          pathogen_title, " by Group (", title_suffix, ")"),
           subtitle = paste0(str_to_title(gsub("_", " ", model_type)))) + 
      scale_x_log10(limits = c(conf_low, conf_high)) +
      theme_bw() + theme(title = element_text(size = 12),
                         axis.text = element_text(size = 10),
                         axis.title = element_text(size = 10),
                         legend.text = element_text(size = 10),
                         plot.tag.position = "topright",
                         plot.tag = element_text(hjust = 0, size = 9))
    
  }
  
  spec_plot <- make_forest_plot(tidy_forest_spec, shape_value = 16, 
                                title_suffix = "Specific Phenotype")
  sens_plot <- make_forest_plot(tidy_forest_sens, shape_value = 17, 
                                title_suffix = "Sensitive Phenotype")
  
  return(list(spec_plot = spec_plot, sens_plot = sens_plot))
  
}

forest_year_further <- function(df, df_dummy, pathogen, model_type,
                                outcome_type) {
  
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
  
  vacc_current <- case_when(
    pathogen == "flu" & outcome_type == "Mild" ~ "flu_vaccination_mild",
    pathogen == "flu" & outcome_type == "Severe" ~ "flu_vaccination_severe",
    pathogen == "covid" & outcome_type == "Mild" ~ "covid_vaccination_mild",
    pathogen == "covid" & outcome_type == "Severe" ~ "covid_vaccination_severe"
  )
  
  dummy_model <- glm_poisson_further(
    df_dummy, exposure, outcome, offset, vacc_prev, vacc_current
  )
  
  #define levels
  levels <- list()
  
  if (cohort == "infants" | cohort == "infants_subgroup") {
    
    levels <- c("0-2m", "3-5m", "6-11m", "12-23m", "Female", "Male")
    
  } else if (cohort == "children_and_adolescents") {
    
    levels <- c("2-5y", "6-9y", "10-13y", "14-17y", "Female", "Male")
    
  } else if (cohort == "adults") {
    
    levels <- c("18-39y", "40-64y", "Female", "Male")
    
  } else if (cohort == "older_adults") {
    
    levels <- c("65-74y", "75-89y", "90y+", "Female", "Male")
    
  }
  
  if (str_detect(model_type, "ethnicity")) {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups"),
                levels, c("Urban Major Conurbation", "Urban Minor Conurbation",
                          "Urban City and Town", "Rural Town and Fringe",
                          "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "ses")) {
    
    levels <- c(c("1 (least deprived)", "2", "3", "4",
                  "5 (most deprived)"), levels,
                c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "composition") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("Multiple of the Same Generation",
                  "Living Alone", "One Other Generation",
                  "Two Other Generations"), levels,
                c("Urban Major Conurbation", "Urban Minor Conurbation",
                  "Urban City and Town", "Rural Town and Fringe",
                  "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "full") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups",
                  "1 (least deprived)", "2", "3", "4",
                  "5 (most deprived)", "Multiple of the Same Generation",
                  "Living Alone", "One Other Generation",
                  "Two Other Generations", "Three Other Generations"),
                levels, c("Urban Major Conurbation", "Urban Minor Conurbation",
                          "Urban City and Town", "Rural Town and Fringe",
                          "Rural Village and Dispersed"))
    
  }
  
  if (cohort == "infants_subgroup") {
    
    levels <- c(levels, "Maternal Age", "Never", "Former", "Current",
                "No", "Yes")
    
  } else if (cohort != "infants" & pathogen == "flu") {
    
    levels <- c(levels, "No", "Yes")
    
  } else if (cohort != "infants" & pathogen == "covid") {
    
    levels <- c(levels, "No", "Yes", "0-6m", "6-12m", "12m+")
    
  }
  
  group_order <- case_when(
    model_type == "ethnicity" & cohort == "infants" ~ list(c(
      "Sex (False)", "Sex (True)",
      "Age Group (False)", "Age Group (True)",
      "Ethnicity (False)", "Ethnicity (True)",
      "Rurality Classification (False)", "Rurality Classification (True)")),
    
    model_type == "ses" & cohort == "infants" ~ list(c(
      "Sex (False)", "Sex (True)",
      "Age Group (False)", "Age Group (True)",
      "IMD Quintile (False)", "IMD Quintile (True)",
      "Rurality Classification (False)", "Rurality Classification (True)")),
    
    model_type == "ethnicity_ses" & cohort == "infants" ~ list(c(
      "Sex (False)", "Sex (True)",
      "Age Group (False)", "Age Group (True)",
      "Ethnicity (False)", "Ethnicity (True)",
      "IMD Quintile (False)", "IMD Quintile (True)",
      "Rurality Classification (False)", "Rurality Classification (True)")),
    
    model_type == "ethnicity" & cohort == "infants_subgroup" ~ list(c(
      "Sex (False)", "Sex (True)",
      "Age Group (False)", "Age Group (True)",
      "Ethnicity (False)", "Ethnicity (True)",
      "Rurality Classification (False)", "Rurality Classification (True)",
      "Maternal Age (False)", "Maternal Age (True)",
      "Maternal Smoking Status (False)", "Maternal Smoking Status (True)",
      "Maternal Drinking (False)", "Maternal Drinking (True)",
      "Maternal Drug Usage (False)", "Maternal Drug Usage (True)",
      "Maternal Flu Vaccination (False)", "Maternal Flu Vaccination (True)",
      "Maternal Pertussis Vaccination (False)",
      "Maternal Pertussis Vaccination (True)")),
    
    model_type == "ses" & cohort == "infants_subgroup" ~ list(c(
      "Sex (False)", "Sex (True)",
      "Age Group (False)", "Age Group (True)",
      "IMD Quintile (False)", "IMD Quintile (True)",
      "Rurality Classification (False)", "Rurality Classification (True)",
      "Maternal Age (False)", "Maternal Age (True)",
      "Maternal Smoking Status (False)", "Maternal Smoking Status (True)",
      "Maternal Drinking (False)", "Maternal Drinking (True)",
      "Maternal Drug Usage (False)", "Maternal Drug Usage (True)",
      "Maternal Flu Vaccination (False)", "Maternal Flu Vaccination (True)",
      "Maternal Pertussis Vaccination (False)",
      "Maternal Pertussis Vaccination (True)")),
    
    model_type == "ethnicity_ses" & cohort == "infants_subgroup" ~ list(c(
      "Sex (False)", "Sex (True)",
      "Age Group (False)", "Age Group (True)",
      "Ethnicity (False)", "Ethnicity (True)",
      "IMD Quintile (False)", "IMD Quintile (True)",
      "Rurality Classification (False)", "Rurality Classification (True)",
      "Maternal Age (False)", "Maternal Age (True)",
      "Maternal Smoking Status (False)", "Maternal Smoking Status (True)",
      "Maternal Drinking (False)", "Maternal Drinking (True)",
      "Maternal Drug Usage (False)", "Maternal Drug Usage (True)",
      "Maternal Flu Vaccination (False)", "Maternal Flu Vaccination (True)",
      "Maternal Pertussis Vaccination (False)",
      "Maternal Pertussis Vaccination (True)")),
    
    # RSV  
    model_type %in% c("ethnicity", "ses", "composition", "ethnicity_ses",
                      "ethnicity_composition", "ses_composition", "full") & 
      cohort != "infants" & cohort != "infants_subgroup" &
      pathogen == "rsv" ~ list(c(
        "Sex (False)", "Sex (True)",
        "Age Group (False)", "Age Group (True)",
        "Ethnicity (False)", "Ethnicity (True)",
        "IMD Quintile (False)", "IMD Quintile (True)",
        "Household Composition (False)", "Household Composition (True)",
        "Rurality Classification (False)", "Rurality Classification (True)")),
    
    # Flu
    model_type %in% c("ethnicity", "ses", "composition", "ethnicity_ses",
                      "ethnicity_composition", "ses_composition", "full") & 
      cohort != "infants" & cohort != "infants_subgroup" &
      pathogen == "flu" ~ list(c(
        "Sex (False)", "Sex (True)",
        "Age Group (False)", "Age Group (True)",
        "Ethnicity (False)", "Ethnicity (True)",
        "IMD Quintile (False)", "IMD Quintile (True)",
        "Household Composition (False)", "Household Composition (True)",
        "Rurality Classification (False)", "Rurality Classification (True)",
        "Prior Flu Vaccine (False)", "Prior Flu Vaccine (True)",
        "Current Flu Vaccine (False)", "Current Flu Vaccine (True)")),
    
    # COVID
    model_type %in% c("ethnicity", "ses", "composition", "ethnicity_ses",
                      "ethnicity_composition", "ses_composition", "full") & 
      cohort != "infants" & cohort != "infants_subgroup" &
      pathogen == "covid" ~ list(c(
        "Sex (False)", "Sex (True)",
        "Age Group (False)", "Age Group (True)",
        "Ethnicity (False)", "Ethnicity (True)",
        "IMD Quintile (False)", "IMD Quintile (True)",
        "Household Composition (False)", "Household Composition (True)",
        "Rurality Classification (False)", "Rurality Classification (True)",
        "Time Since Last Covid Vaccine (False)",
        "Time Since Last Covid Vaccine (True)",
        "Current Covid Vaccine (False)", "Current Covid Vaccine (True)"))
  )[[1]]
  
  process_forest_plot <- function(df_model, codelist_filter, shape_value,
                                  title_label) {
    
    tidy_forest <- df_model %>%
      filter(codelist_type == codelist_filter) %>%
      tidy_attach_model(dummy_model) %>%
      tidy_add_reference_rows() %>%
      tidy_add_estimate_to_reference_rows(exponentiate = TRUE,
                                          conf.level = 95) %>%
      tidy_add_term_labels() %>%
      tidy_remove_intercept() %>%
      mutate(
        conf.low = if_else(reference_row, 1, conf.low),
        conf.high = if_else(reference_row, 1, conf.high)
      )
    
    if (investigation_type == "primary") {
      
      # Expand reference rows
      reference_rows <- tidy_forest %>%
        filter(reference_row) %>%
        mutate(rn = row_number()) %>%
        slice(rep(1:n(), each = 8)) %>%
        group_by(rn) %>%
        mutate(subset = c("2016_17", "2017_18", "2018_19", "2019_20",
                          "2020_21", "2021_22", "2022_23", "2023_24")) %>%
        ungroup() %>%
        select(-rn)
      
    } else {
      
      if (pathogen == "rsv") {
        
        # Expand reference rows
        reference_rows <- tidy_forest %>%
          filter(reference_row) %>%
          mutate(subset = c("2017_18"))
        
      } else if (pathogen == "flu") {
        
        # Expand reference rows
        reference_rows <- tidy_forest %>%
          filter(reference_row) %>%
          mutate(subset = c("2018_19"))
        
      } else {
        
        # Expand reference rows
        reference_rows <- tidy_forest %>%
          filter(reference_row) %>%
          mutate(subset = c("2020_21"))
        
      }
      
    }
    
    tidy_forest <- tidy_forest %>%
      filter(!reference_row) %>%
      bind_rows(reference_rows)
    
    legend_labels <- unique(str_to_title(gsub("_", " ", tidy_forest$variable)))
    
    cols2 <- tibble(
      var = c("sex", "age_band", "latest_ethnicity_group", "imd_quintile",
              "composition_category", "rurality_classification",
              vacc_prev, vacc_current, "maternal_age",
              "maternal_smoking_status", "maternal_drinking",
              "maternal_drug_usage", "maternal_flu_vaccination",
              "maternal_pertussis_vaccination"),
      col = c("#f64883", "#50edb2", "#43006f", "#b1e466",
              "#8e0077", "#227e00",
              "#bb1782", "#009142", "#ff62ab",
              "#004e13", "#e1b1ff",
              "#ae9500", "#004194",
              "#d98116")
    )
    cols_final <- cols2 %>%
      filter(var %in% unique(tidy_forest$variable)) %>%
      select(col) %>%
      slice(rep(1:n(), each = 2))
    
    tidy_forest %>%
      mutate(
        plot_label = str_to_title(gsub("_", " ", variable)),
        label = forcats::fct_relevel(label, levels),
        subset = str_to_title(gsub("_", "-", subset))
      ) %>%
      mutate(
        plot_label = case_when(
          str_detect(plot_label, "Age") ~ "Age Group",
          str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
          str_detect(plot_label, "Latest") ~ "Ethnicity",
          str_detect(plot_label, "Comp") ~ "Household Composition",
          TRUE ~ plot_label)
      ) %>%
      mutate(
        plot_label2 = paste0(plot_label, " (", str_to_title(reference_row), ")")
      ) %>%
      mutate(
        plot_label2 = factor(plot_label2, levels = group_order)
      ) %>%
      ggplot(aes(y = label, x = estimate, xmin = conf.low,
                 xmax = conf.high, color = plot_label2, shape = plot_label2)) +
      scale_color_manual(values = rep(cols, 2),
                         name = "Characteristic (Reference)") +
      scale_shape_manual(name = "Characteristic (Reference)",
                         values = c(rep(shape_value, length(legend_labels)),
                                    rep(8, length(legend_labels)))) +
      geom_vline(xintercept = 1, linetype = 2) + scale_x_log10() +
      geom_pointrange(position = position_dodge(width = 0.5), size = 0.2) +
      guides(color = guide_legend("Characteristic (Reference)"),
             shape = guide_legend("Characteristic (Reference)")) +
      facet_wrap(~ subset, scales = "free_y", nrow = 2) + 
      labs(x = "Rate Ratio", y = " ", title = title_label,
           subtitle = paste0(str_to_title(gsub("_", " ", model_type)))) +
      theme_bw() + theme(title = element_text(size = 12),
                         axis.text = element_text(size = 10),
                         axis.title = element_text(size = 10),
                         legend.text = element_text(size = 10))
    
  }
  
  pathogen_title <- case_when(
    pathogen == "rsv" ~ "RSV",
    pathogen == "flu" ~ "Influenza",
    pathogen == "covid" ~ "COVID-19"
  )
  
  # Generate plots separately
  spec_plot <- process_forest_plot(
    df_model, "specific", shape_value = 16,
    title_label = paste0("Rate Ratios of ", outcome_type, " ", pathogen_title,
                         " (Specific Phenotype)"))
  sens_plot <- process_forest_plot(
    df_model, "sensitive", shape_value = 17,
    title_label = paste0("Rate Ratios of ", outcome_type, " ", pathogen_title,
                         " (Sensitive Phenotype)"))
  
  return(list(spec = spec_plot, sens = sens_plot))
  
}
