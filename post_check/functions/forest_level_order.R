# Shared factor level order for forest / over-time plots.
# style = "year_mult" matches forest_year_further_mult() ordering (differs for age, smoking labels, flu/covid).
get_forest_level_order <- function(
  cohort_val,
  model_type,
  pathogen,
  investigation_val,
  style = c("default", "year_mult")
) {
  style <- match.arg(style)
  year_mult <- identical(style, "year_mult")
  levels <- list()

  if (cohort_val %in% c("infants", "infants_subgroup")) {
    levels <- c("12-23m", "6-11m", "3-5m", "0-2m", "Male", "Female")
  } else if (cohort_val == "children_and_adolescents") {
    levels <- if (year_mult) {
      c("2-5y", "6-9y", "10-13y", "14-17y", "Female", "Male")
    } else {
      c("14-17y", "10-13y", "6-9y", "2-5y", "Male", "Female")
    }
  } else if (cohort_val == "adults") {
    levels <- if (year_mult) {
      c("18-39y", "40-64y", "Female", "Male")
    } else {
      c("40-64y", "18-39y", "Male", "Female")
    }
  } else {
    levels <- c("90y+", "75-89y", "65-74y", "Male", "Female")
  }

  if (model_type == "ethnicity") {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Town and Fringe",
        "Rural Village and Dispersed", "Other Ethnic Groups",
        "Unknown", "Other Ethnic Groups",
        "Black or Black British", "Asian or Asian British",
        "Mixed", "White"
      ),
      levels
    )
  } else if (model_type == "ses") {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "1 (most deprived)",
        "2", "3", "4", "5 (least deprived)"
      ),
      levels
    )
  } else if (model_type == "composition" &&
             !cohort_val %in% c("infants", "infants_subgroup")) {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "Three Other Generations",
        "Two Other Generations", "One Other Generation",
        "Living Alone", "Multiple of the Same Generation"
      ),
      levels
    )
  } else if (model_type == "ethnicity_ses") {
    eth_ses <- if (year_mult) {
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "1 (most deprived)",
        "2", "3", "4", "5 (least deprived)",
        "Other Ethnic Groups", "Black or Black British",
        "Unknown", "Other Ethnic Groups", "Black or Black British",
        "Asian or Asian British", "Mixed", "White"
      )
    } else {
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "1 (most deprived)",
        "2", "3", "4", "5 (least deprived)",
        "Unknown", "Other Ethnic Groups", "Black or Black British",
        "Asian or Asian British", "Mixed", "White"
      )
    }
    levels <- c(eth_ses, levels)
  } else if (model_type == "ethnicity_composition" &&
             !cohort_val %in% c("infants", "infants_subgroup")) {
    eth_comp <- if (year_mult) {
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "Three Other Generations",
        "Two Other Generations", "One Other Generation",
        "Living Alone", "Multiple of the Same Generation",
        "Other Ethnic Groups", "Black or Black British",
        "Unknown", "Other Ethnic Groups", "Black or Black British",
        "Asian or Asian British", "Mixed", "White"
      )
    } else {
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "Three Other Generations",
        "Two Other Generations", "One Other Generation",
        "Living Alone", "Multiple of the Same Generation",
        "Unknown", "Other Ethnic Groups", "Black or Black British",
        "Asian or Asian British", "Mixed", "White"
      )
    }
    levels <- c(eth_comp, levels)
  } else if (model_type == "ses_composition" &&
             !cohort_val %in% c("infants", "infants_subgroup")) {
    levels <- c(
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "Three Other Generations",
        "Two Other Generations", "One Other Generation",
        "Living Alone", "Multiple of the Same Generation",
        "1 (most deprived)", "2", "3", "4", "5 (least deprived)"
      ),
      levels
    )
  } else if (model_type == "full" &&
             !cohort_val %in% c("infants", "infants_subgroup")) {
    full_levels <- if (year_mult) {
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "Three Other Generations",
        "Two Other Generations", "One Other Generation",
        "Living Alone", "Multiple of the Same Generation",
        "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
        "Other Ethnic Groups", "Black or Black British",
        "Unknown", "Other Ethnic Groups", "Black or Black British",
        "Asian or Asian British", "Mixed", "White"
      )
    } else {
      c(
        "Urban Major Conurbation", "Urban Minor Conurbation",
        "Urban City and Town", "Rural Village and Dispersed",
        "Rural Town and Fringe", "Three Other Generations",
        "Two Other Generations", "One Other Generation",
        "Living Alone", "Multiple of the Same Generation",
        "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
        "Unknown", "Other Ethnic Groups", "Black or Black British",
        "Asian or Asian British", "Mixed", "White"
      )
    }
    levels <- c(full_levels, levels)
  }

  if (cohort_val == "infants_subgroup") {
    smoking <- if (year_mult) {
      c(
        "Current Smoker", "Former Smoker", "Never Smoker"
      )
    } else {
      c(
        "Maternal Current Smoking", "Maternal Former Smoking",
        "Maternal Never Smoking", "Maternal Unknown Smoking Status"
      )
    }
    levels <- c(
      "Maternal Pertussis Vaccination",
      "Maternal Flu Vaccination", "Maternal Drug Usage",
      "Maternal Drinking", "Binary Variables (Reference)",
      smoking,
      "Maternal Age", "Maternal Age (Average)", levels
    )
  } else if (
    cohort_val != "infants" && pathogen == "flu" &&
    (year_mult || investigation_val == "primary")
  ) {
    levels <- c(
      "Flu Vaccination (Yes)", "Flu Vaccination (No)",
      "Eligible and Vaccinated Last Autumn",
      "Not Vaccinated in Past Year", levels
    )
  } else if (
    cohort_val != "infants" && pathogen == "covid" &&
    (year_mult || investigation_val == "primary")
  ) {
    levels <- c(
      "Covid Vaccination (Yes)", "Covid Vaccination (No)",
      "Not Vaccinated in Past Year",
      "Eligible and Vaccinated Last Autumn",
      "Eligible and Vaccinated Last Spring", levels
    )
  } else if (!year_mult && investigation_val == "secondary") {
    levels <- c(
      "Drug Usage", "Hazardous Drinking", "Sickle Cell",
      "Immunosuppressed", "Cancer Within 3 Yrs", "CND", "CKD",
      "CLD", "CHD", "Severely Obese", "Addisons", "Diabetes",
      "Other Resp. Cond.", "Cystic Fibrosis", "COPD", "Asthma",
      "Binary Variables (Reference)", "Current", "Former",
      "Never", levels
    )
  }

  unique(as.character(unlist(levels)))
}
