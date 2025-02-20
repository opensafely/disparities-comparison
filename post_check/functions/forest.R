library(here)
library(broom)
library(rlang)
library(broom.helpers)
library(cowplot)
library(stringr)

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
                levels)
    
  }
  
  if (str_detect(model_type, "ses")) {
    
    levels <- c(levels, c("1 (least deprived)", "2", "3", "4",
                          "5 (most deprived)"))
    
  }
  
  if (str_detect(model_type, "hh_comp") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(levels, c("Multiple of the Same Generation",
                          "Living Alone", "One Other Generation",
                          "Two Other Generations",
                          "Three Other Generations"))
    
  }
  
  if (str_detect(model_type, "full") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups"),
                levels, c("1 (least deprived)", "2", "3", "4",
                          "5 (most deprived)",
                          "Multiple of the Same Generation",
                          "Living Alone", "One Other Generation",
                          "Two Other Generations",
                          "Three Other Generations"))
    
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
          str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
          str_detect(plot_label, "Comp") ~ gsub("Composition Category",
                                                "Household Composition",
                                                plot_label),
          TRUE ~ plot_label)
      )
    
    tidy_forest %>%
      mutate(
        plot_label = str_to_title(gsub("_", " ", variable)),
        label = forcats::fct_relevel(label, levels),
        subset = str_to_title(gsub("_", "-", subset))
      ) %>%
      mutate(
        plot_label = case_when(
          str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
          str_detect(plot_label, "Comp") ~ gsub("Composition Category",
                                                "Household Composition",
                                                plot_label),
          TRUE ~ plot_label)
      ) %>%
      mutate(reference_row = NA) %>%
      ggplot(aes(x = label, y = estimate, ymin = conf.low,
                 ymax = conf.high, color = subset)) +
      geom_hline(yintercept = 1, linetype = 2) + 
      geom_pointrange(position = position_dodge(width = 0.5), size = 0.2,
                      shape = shape_value) +
      geom_point(data = references, aes(x = label, y = estimate,
                                        shape = as.factor(estimate)),
                 size = 1, stroke = 1, color = "black") +
      scale_shape_manual(name = "", values = c(8),
                         labels = "Reference Category") +
      scale_color_discrete(na.translate = F) +
      guides(color = guide_legend("Season", order = 1,
                                  override.aes = list(shape = shape_value))) +
      coord_flip() + facet_wrap(~ plot_label, scales = "free_y") + 
      labs(y = "Rate Ratio", x = " ",
           title = paste0("Rate Ratios of ", outcome_type, " ", 
                          pathogen_title, " (", title_suffix, ")")) + 
      ylim(conf_low, conf_high) +
      theme_bw() + theme(plot.tag.position = "topright",
                         plot.tag = element_text(hjust = 0, size = 9))
    
  }
  
  spec_plot <- make_forest_plot(tidy_forest_spec, shape_value = 16, 
                                title_suffix = "Specific Phenotype")
  sens_plot <- make_forest_plot(tidy_forest_sens, shape_value = 17, 
                                title_suffix = "Sensitive Phenotype")
  
  return(list(spec_plot = spec_plot, sens_plot = sens_plot))
  
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
    
    levels <- c(levels, c("1 (least deprived)", "2", "3", "4",
                          "5 (most deprived)"))
    
  }
  
  if (str_detect(model_type, "hh_comp") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(levels, c("Multiple of the Same Generation",
                          "Living Alone", "One Other Generation",
                          "Two Other Generations",
                          "Three Other Generations"))
    
  }
  
  if (str_detect(model_type, "full") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups"),
                levels, c("1 (least deprived)", "2", "3", "4",
                          "5 (most deprived)",
                          "Multiple of the Same Generation",
                          "Living Alone", "One Other Generation",
                          "Two Other Generations",
                          "Three Other Generations"))
    
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
    
    tidy_forest <- tidy_forest %>%
      filter(!reference_row) %>%
      bind_rows(reference_rows)
    
    tidy_forest %>%
      mutate(
        plot_label = str_to_title(gsub("_", " ", variable)),
        label = forcats::fct_relevel(label, levels),
        subset = str_to_title(gsub("_", "-", subset))
      ) %>%
      mutate(
        plot_label = case_when(
          str_detect(plot_label, "Imd") ~ gsub("Imd", "IMD", plot_label),
          str_detect(plot_label, "Comp") ~ gsub("Composition Category",
                                                "Household Composition",
                                                plot_label),
          TRUE ~ plot_label)
      ) %>%
      ggplot(aes(x = label, y = estimate, ymin = conf.low,
                 ymax = conf.high, color = plot_label)) +
        geom_hline(yintercept = 1, linetype = 2) + 
        geom_pointrange(aes(shape = reference_row),
                        position = position_dodge(width = 0.5), size = 0.2) +
        scale_shape_manual(values = c("TRUE" = 8, "FALSE" = shape_value)) +
        guides(color = guide_legend("Characteristic", order = 1),
               shape = guide_legend("Reference Category", order = 2)) +
        coord_flip() + facet_wrap(~ subset, scales = "free_y", nrow = 2) + 
        labs(y = "Rate Ratio", x = " ", title = title_label) +
        theme_bw()
    
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
             conf.high = if_else(reference_row, 1, conf.high),
             subset = if_else(reference_row, "reference", subset))
    
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
    
    levels <- c(levels, c("1 (least deprived)", "2", "3", "4",
                          "5 (most deprived)", "Urban Major Conurbation",
                          "Urban Minor Conurbation", "Urban City and Town",
                          "Rural Town and Fringe", "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "hh_comp") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(levels, c("Multiple of the Same Generation",
                          "Living Alone", "One Other Generation",
                          "Two Other Generations",
                          "Three Other Generations", "Urban Major Conurbation",
                          "Urban Minor Conurbation", "Urban City and Town",
                          "Rural Town and Fringe", "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "full") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups"),
                levels, c("1 (least deprived)", "2", "3", "4",
                          "5 (most deprived)",
                          "Multiple of the Same Generation",
                          "Living Alone", "One Other Generation",
                          "Two Other Generations",
                          "Three Other Generations", "Urban Major Conurbation",
                          "Urban Minor Conurbation", "Urban City and Town",
                          "Rural Town and Fringe", "Rural Village and Dispersed"))
    
  }
  
  if (cohort == "infants_subgroup") {
    
    levels <- c(levels, "Never", "Former", "Current")
    
  }
  
  make_forest_plot <- function(tidy_forest, shape_value, title_suffix) {
    
    pathogen_title <- case_when(
      pathogen == "rsv" ~ "RSV",
      pathogen == "flu" ~ "Influenza",
      pathogen == "covid" ~ "COVID-19"
    )
    
    tidy_forest %>%
      # group_by(variable) %>%
      mutate(
        plot_label = str_to_title(gsub("_", " ", variable)),
        label = forcats::fct_relevel(label, levels)
      ) %>%
      ggplot(aes(x = label, y = estimate, ymin = conf.low,
                 ymax = conf.high, color = subset)) +
      geom_hline(yintercept = 1, linetype = 2) + 
      geom_pointrange(aes(shape = subset), position = position_dodge(width = 0.5),
                      size = 0.2) +
      scale_shape_manual(values = c(
        "reference" = 8, "2016_17" = shape_value, "2017_18" = shape_value,
        "2018_19" = shape_value, "2019_20" = shape_value,
        "2020_21" = shape_value, "2021_22" = shape_value,
        "2022_23" = shape_value, "2023_24" = shape_value), guide = "none") +
      coord_flip() + facet_wrap(~ plot_label, scales = "free_y") + 
      labs(y = "Rate Ratio", x = " ",
           title = paste0("Rate Ratios of ", outcome_type, " ", 
                          pathogen_title, " (", title_suffix, ")")) + 
      ylim(conf_low, conf_high) +
      theme_bw()
  }
  
  spec_plot <- make_forest_plot(tidy_forest_spec, shape_value = 16, 
                                title_suffix = "Specific Phenotype")
  sens_plot <- make_forest_plot(tidy_forest_sens, shape_value = 17, 
                                title_suffix = "Sensitive Phenotype")
  
  return(list(spec_plot = spec_plot, sens_plot = sens_plot))
  
}

forest_year_further <- function(df, df_dummy, pathogen, model_type, outcome_type) {
  
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
    
    levels <- c(levels, c("1 (least deprived)", "2", "3", "4",
                          "5 (most deprived)", "Urban Major Conurbation",
                          "Urban Minor Conurbation", "Urban City and Town",
                          "Rural Town and Fringe", "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "hh_comp") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(levels, c("Multiple of the Same Generation",
                          "Living Alone", "One Other Generation",
                          "Two Other Generations",
                          "Three Other Generations", "Urban Major Conurbation",
                          "Urban Minor Conurbation", "Urban City and Town",
                          "Rural Town and Fringe", "Rural Village and Dispersed"))
    
  }
  
  if (str_detect(model_type, "full") & cohort != "infants" &
      cohort != "infants_subgroup") {
    
    levels <- c(c("White", "Mixed", "Asian or Asian British",
                  "Black or Black British", "Other Ethnic Groups"),
                levels, c("1 (least deprived)", "2", "3", "4",
                          "5 (most deprived)",
                          "Multiple of the Same Generation",
                          "Living Alone", "One Other Generation",
                          "Two Other Generations",
                          "Three Other Generations", "Urban Major Conurbation",
                          "Urban Minor Conurbation", "Urban City and Town",
                          "Rural Town and Fringe", "Rural Village and Dispersed"))
    
  }
  
  if (cohort == "infants_subgroup") {
    
    levels <- c(levels, "Never", "Former", "Current")
    
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
    
    tidy_forest <- tidy_forest %>%
      filter(!reference_row) %>%
      bind_rows(reference_rows)
    
    tidy_forest %>%
      #group_by(variable) %>%
      mutate(
        label = forcats::fct_relevel(label, levels)
      ) %>%
      ggplot(aes(x = label, y = estimate, ymin = conf.low,
                 ymax = conf.high, color = variable)) +
        geom_hline(yintercept = 1, linetype = 2) + 
        geom_pointrange(position = position_dodge(width = 0.5), size = 0.2,
                        shape = shape_value) +
        coord_flip() + 
        facet_wrap(~ subset, scales = "free_y", nrow = 2) + 
        labs(y = "Rate Ratio", x = " ", title = title_label) +
        theme_bw()
    
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
