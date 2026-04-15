## Run all dummydata generators for all seasons

library(here)

source(here::here("analysis", "design", "design.R"))

season_ids <- 1:8

scripts <- c(
  "analysis/dummydata/dummydata_infants.R",
  "analysis/dummydata/dummydata_infants_subgroup.R",
  "analysis/dummydata/dummydata_children_and_adolescents.R",
  "analysis/dummydata/dummydata_adults.R",
  "analysis/dummydata/dummydata_older_adults.R"
)

for (sid in season_ids) {
  season_id <- sid

  # make sure per-script defaults don't leak across iterations
  if (exists("study_start_date", inherits = FALSE)) rm(study_start_date)
  if (exists("study_end_date", inherits = FALSE)) rm(study_end_date)

  message("Running dummydata for season ", season_id)
  for (script in scripts) {
    message("  - ", script)
    source(here::here(script), local = FALSE)
  }
}

