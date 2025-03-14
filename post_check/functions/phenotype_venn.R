library(VennDiagram)
library(tidyverse)
library(here)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(stringr)

## create output directories ----
fs::dir_create(here::here("IMG"))

#define function to create venn diagram of multiple infection outcomes
venn <- function(input, season, outcome) {
  
  filtered_input <- input %>%
    filter(subset == season, outcome_type == outcome)  %>%
    mutate(n = `n (midpoint 10 rounded)`)
  
  df_diff <- filtered_input %>%
    group_by(combo) %>%
    filter(codelist_type %in% c("sensitive", "specific")) %>%
    arrange(combo, codelist_type) %>%
    mutate(
      n = if_else(is.na(n), 0, as.numeric(n))
    ) %>%
    reframe(n_diff = n[codelist_type == "sensitive"] - n[codelist_type == "specific"]) %>%
    ungroup() 
  
  # Helper function to safely extract count or return 0 if not found
  safe_pull <- function(df, combo_spec) {
    
    df <- df %>% mutate(n_diff = if_else(is.na(n_diff), 0, n_diff))
    count <- df %>% filter(combo == !!combo_spec) %>% select(n_diff) %>% pull()
    if (length(count) == 0) return(0) else return(count)
    
  }
  
  #sets 
  if (season %in% c("2019_20", "2020_21", "2021_22", "2022_23", "2023_24")) {
    
    if (outcome == "mild") {
      
      rsv_total <- safe_pull(df_diff, "RSV_Mild_Total")
      flu_total <- safe_pull(df_diff, "Flu_Mild_Total")
      covid_total <- safe_pull(df_diff, "COVID_Mild_Total")
      
      rsv <- safe_pull(df_diff, "RSV_Mild_0_0")
      flu <- safe_pull(df_diff, "0_Flu_Mild_0")
      covid <- safe_pull(df_diff, "0_0_COVID_Mild")
      rsv_flu <- safe_pull(df_diff, "RSV_Mild_Flu_Mild_0")
      rsv_covid <- safe_pull(df_diff, "RSV_Mild_0_COVID_Mild")
      flu_covid <- safe_pull(df_diff, "0_Flu_Mild_COVID_Mild")
      rsv_flu_covid <- safe_pull(df_diff, "RSV_Mild_Flu_Mild_COVID_Mild")
      
    } else {
      
      rsv_total <- safe_pull(df_diff, "RSV_Severe_Total")
      flu_total <- safe_pull(df_diff, "Flu_Severe_Total")
      covid_total <- safe_pull(df_diff, "COVID_Severe_Total")
      
      rsv <- safe_pull(df_diff, "RSV_Severe_0_0")
      flu <- safe_pull(df_diff, "0_Flu_Severe_0")
      covid <- safe_pull(df_diff, "0_0_COVID_Severe")
      rsv_flu <- safe_pull(df_diff, "RSV_Severe_Flu_Severe_0")
      rsv_covid <- safe_pull(df_diff, "RSV_Severe_0_COVID_Severe")
      flu_covid <- safe_pull(df_diff, "0_Flu_Severe_COVID_Severe")
      rsv_flu_covid <- safe_pull(df_diff, "RSV_Severe_Flu_Severe_COVID_Severe")
      
    }
    
  } else {
    
    if (outcome == "mild") {
      
      rsv_total <- safe_pull(df_diff, "RSV_Mild_Total")
      flu_total <- safe_pull(df_diff, "Flu_Mild_Total")
      
      rsv <- safe_pull(df_diff, "RSV_Mild_0")
      flu <- safe_pull(df_diff, "0_Flu_Mild")
      rsv_flu <- safe_pull(df_diff, "RSV_Mild_Flu_Mild")
      
    } else {
      
      rsv_total <- safe_pull(df_diff, "RSV_Severe_Total")
      flu_total <- safe_pull(df_diff, "Flu_Severe_Total")
      
      rsv <- safe_pull(df_diff, "RSV_Severe_0")
      flu <- safe_pull(df_diff, "0_Flu_Severe")
      rsv_flu <- safe_pull(df_diff, "RSV_Severe_Flu_Severe")
      
    }
    
  }
  
  diag <- if (season %in% c("2019_20", "2020_21", "2021_22", "2022_23") & 
              outcome %in% c("mild", "severe")) {
    
    draw.triple.venn(
      area1 = rsv_total,
      area2 = flu_total,
      area3 = covid_total, 
      n12 = rsv_flu + rsv_flu_covid,
      n13 = rsv_covid + rsv_flu_covid,
      n23 = flu_covid + rsv_flu_covid,
      n123 = rsv_flu_covid,
      category = c("RSV", "Influenza", "COVID-19"),
      fill = c("#D81B60", "#004D40", "#FFC107"),
      alpha = c(0.5, 0.5, 0.5),
      cat.fontfamily = "sans",
      cat.cex = 1,
      cat.dist = c(0.035, 0.05, 0.03),
      fontfamily = "sans",
      cex = 1,
      print.mode = "raw",
      #rotation.degree = if_else(rsv_total <= flu_total, 0, 180),
      #inverted = if_else(rsv_total <= flu_total, FALSE, TRUE),
      ind = FALSE,
      sigdigs = 2
    )
    
  } else {
    
    draw.pairwise.venn(
      area1 = rsv_total,
      area2 = flu_total,
      cross.area = rsv_flu,
      category = c("RSV", "Influenza"),
      fill = c("#D81B60", "#004D40"),
      alpha = c(0.5, 0.5),
      fontfamily = "sans",
      cat.fontfamily = "sans",
      cat.cex = 1,
      cat.pos = case_when(rsv_total < flu_total & rsv_flu != 0 ~ c(140, 220),
                          rsv_total < flu_total & rsv_flu == 0 ~ c(325, 35),
                          rsv_total > flu_total & rsv_flu == 0 ~ c(325, 35),
                           TRUE ~ c(325, 35)),
      cat.dist = c(0.03, 0.04),
      cex = 1,
      print.mode = "raw",
      rotation.degree = case_when(rsv_total < flu_total & rsv_flu != 0 ~ 180,
                                  rsv_total < flu_total & rsv_flu == 0 ~ 180,
                                  rsv_total > flu_total & rsv_flu == 0 ~ 0,
                                  TRUE ~ 0),
      #inverted = if_else(rsv_total <= flu_total, FALSE, TRUE),
      ind = FALSE,
      sigdigs = 2
    )
    
  }
  
  diag2 <- grid.arrange(gTree(children = diag), 
                        top = textGrob("Increase in the Occurrence of Identification of\nMultiple Outcomes Within One Episode\nWhen Using Maximally Sensitive Outcome Phenotype", 
                              gp = gpar(fontsize = 12, fontface = "bold")),
                        bottom = textGrob(paste0("Season: ", season, "\nOutcome: ", str_to_title(outcome)), 
                                          gp = gpar(fontsize = 12, fontface = "bold")))
  
  ggsave(diag2, filename = paste0(here::here("post_check", "plots",
                                  "exploratory_analyses"), "/", cohort,
                                  "_venn_diag_diff_", season, "_", outcome,
                                  ".png"), height = 8, width = 8,
         device = "png")
  
}
