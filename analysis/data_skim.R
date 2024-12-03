# import libraries
library(tidyverse)
library(here)

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  rds_file <- "output/data/data_processed.arrow"
  output_dir <- "output/data/skim"
} else {
  arrow_file <- args[[1]]
  output_dir <- args[[2]]
}


stopifnot("must pass an .arrow file" = fs::path_ext(arrow_file)=="arrow")

filenamebase <- fs::path_ext_remove(fs::path_file(arrow_file))

# Import processed data ----

source_data <- arrow::read_feather(here(arrow_file))

# Output summary .txt ----

options(width=200) # set output width for capture.output

dir.create(here(output_dir), showWarnings = FALSE, recursive=TRUE)

## high-level variable overview ----
capture.output(
  skimr::skim_without_charts(source_data),
  file = here(output_dir, paste0(filenamebase, "_skim", ".txt")),
  split=FALSE
)

## list of column types ----
capture.output(
  lapply(source_data, class),
  file = here(output_dir, paste0(filenamebase, "_coltypes", ".txt"))
)