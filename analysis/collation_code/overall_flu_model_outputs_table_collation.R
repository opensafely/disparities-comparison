library(tidyverse)
library(here)
library(arrow)
library(ggplot2)

#define cohort
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort <- "adults"
} else {
  cohort <- args[[1]]
}

## create output directories ----
fs::dir_create(here::here("output", "collated", "analytic"))

##model outputs

#import flu results table by cohort 
#(primary investigation, specific and sensitive phenotypes)
collated_model_outputs_overall_flu = rbind(
  read_csv(here::here("output", "results", "models", "flu_overall",
                      paste0("flu_ethnicity_model_outputs_", 
                             cohort, "_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity", codelist_type = "specific", 
             investigation_type = "primary", subset = "overall"),
  read_csv(here::here("output", "results", "models", "flu_overall",
                      paste0("flu_ses_model_outputs_", 
                             cohort, "_specific_primary.csv"))) 
  %>% mutate(model_type = "ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "overall"),
  read_csv(here::here("output", "results", "models", "flu_overall",
                      paste0("flu_ethnicity_ses_model_outputs_", 
                             cohort, "_specific_primary.csv"))) 
  %>% mutate(model_type = "ethnicity_ses", codelist_type = "specific", 
             investigation_type = "primary", subset = "overall")
)

#save as csv
write_csv(collated_model_outputs_overall_flu, 
          paste0(here::here("output", "collated", "analytic"),
          "/", cohort, "_overall_flu_model_outputs_collated.csv"))
