# Recreate all post_check outputs from released / collated inputs.
#
# Runs every publication table + plot script under post_check/ in dependency
# order (reformat → descriptive → model forests → condensed → exploratory →
# dashboard). Each script runs in its own Rscript process so failures are
# isolated.
#
# Usage (from repo root):
#   Rscript post_check/recreate_all_outputs.R
#   Rscript post_check/recreate_all_outputs.R --only=condensed
#   Rscript post_check/recreate_all_outputs.R --only=reformat,primary,models
#   Rscript post_check/recreate_all_outputs.R --include-scrape
#   Rscript post_check/recreate_all_outputs.R --dry-run
#
# Stages:
#   reformat     supplemental table reformatting + flow chart
#   primary      primary descriptive / ascertainment / counts / table1 plots
#   models       primary, secondary, and sensitivity forest-model scripts
#   condensed    covariate-row key-vars + base-vs-further + sensitivity condensed
#   exploratory  phenotype / surveillance / internal-validation exploratories
#   dashboard    static dashboard asset build (needs model .RData caches)
#   scrape       UKHSA / web scrapers (off by default; network required)
#
# Skips helper modules, Quarto sources, and deprecated prototype wrappers.

library(here)

args <- commandArgs(trailingOnly = TRUE)

dry_run <- "--dry-run" %in% args
include_scrape <- "--include-scrape" %in% args

only_arg <- args[startsWith(args, "--only=")]
if (length(only_arg) == 1L) {
  stages_requested <- strsplit(sub("^--only=", "", only_arg), ",", fixed = TRUE)[[1]]
  stages_requested <- trimws(stages_requested)
} else if (length(only_arg) > 1L) {
  stop("Pass a single --only=stage1,stage2 argument.")
} else {
  stages_requested <- NULL
}

known_stages <- c(
  "reformat", "primary", "models", "condensed",
  "exploratory", "dashboard", "scrape"
)
if (!is.null(stages_requested)) {
  bad <- setdiff(stages_requested, known_stages)
  if (length(bad) > 0) {
    stop(
      "Unknown stage(s): ", paste(bad, collapse = ", "),
      "\nKnown: ", paste(known_stages, collapse = ", ")
    )
  }
}

want_stage <- function(stage) {
  if (is.null(stages_requested)) {
    if (identical(stage, "scrape")) {
      return(isTRUE(include_scrape))
    }
    return(TRUE)
  }
  stage %in% stages_requested
}

# Relative paths from repo root.
post_check_jobs <- list(
  reformat = c(
    "post_check/supplemental/reformat_table1.R",
    "post_check/supplemental/reformat_rates_primary.R",
    "post_check/supplemental/reformat_all_cohort_cases.R",
    "post_check/supplemental/reformat_internal_validation.R",
    "post_check/supplemental/reformat_reinfections.R",
    "post_check/supplemental/reformat_model_estimates.R",
    "post_check/supplemental/outcome_overlap/reformat_outcome_overlap.R",
    "post_check/supplemental/flow_chart/reformat_inclusion.R",
    "post_check/supplemental/flow_chart/flow_chart.R"
  ),
  primary = c(
    "post_check/primary_analyses/table1_viz.R",
    "post_check/primary_analyses/table1_key_vars_panel_viz.R",
    "post_check/primary_analyses/counts_viz_all.R",
    "post_check/primary_analyses/outcome_ascertainment.R"
  ),
  models = c(
    "post_check/primary_analyses/models/rsv_model_results.R",
    "post_check/primary_analyses/models/rsv_further_model_results.R",
    "post_check/primary_analyses/models/flu_model_results.R",
    "post_check/primary_analyses/models/flu_further_model_results.R",
    "post_check/primary_analyses/models/covid_model_results.R",
    "post_check/primary_analyses/models/covid_further_model_results.R",
    "post_check/secondary_analyses/secondary_table1_viz.R",
    "post_check/secondary_analyses/rsv_model_results.R",
    "post_check/secondary_analyses/flu_model_results.R",
    "post_check/secondary_analyses/covid_model_results.R",
    "post_check/sensitivity_analyses/rsv_model_results.R",
    "post_check/sensitivity_analyses/flu_model_results.R",
    "post_check/sensitivity_analyses/overall_and_all_cause_model_results.R",
    "post_check/sensitivity_analyses/overall_and_all_cause_further_model_results.R"
  ),
  condensed = c(
    "post_check/primary_analyses/further_results_condensed_key_vars.R",
    "post_check/primary_analyses/key_exposures_condensed_base_vs_further_stacked_all_seasons.R",
    "post_check/primary_analyses/full_model_condensed_key_vars_2020_21.R",
    "post_check/sensitivity_analyses/further_model_results_condensed_sens.R"
  ),
  exploratory = c(
    "post_check/exploratory_analyses/phenotype_sensitivity_upset.R",
    "post_check/exploratory_analyses/phenotype_sensitivity_upset_rates.R",
    "post_check/exploratory_analyses/size_scales_phenotypes.R",
    "post_check/exploratory_analyses/internal_validation_vis.R",
    "post_check/supplemental/plot_internal_validation_trends.R",
    "post_check/exploratory_analyses/surveillance/size_scales_surveillance.R",
    "post_check/exploratory_analyses/surveillance/surveillance_sources.R",
    "post_check/exploratory_analyses/surveillance/seasonality_england.R",
    "post_check/exploratory_analyses/surveillance/seasonality_weekly_england.R",
    "post_check/exploratory_analyses/surveillance/surviellance_comparisons_england.R",
    "post_check/exploratory_analyses/surveillance/surviellance_comparisons_england_subset.R",
    "post_check/exploratory_analyses/surveillance/surveillance_comparisons_viz_all_cohorts_england.R",
    "post_check/exploratory_analyses/surveillance/surveillance_comparisons_viz_all_cohorts_england_weekly.R",
    "post_check/exploratory_analyses/surveillance/surveillance_comparisons_viz_all_cohorts_ukhsa.R"
  ),
  dashboard = c(
    "post_check/supplemental/dashboard/build_static_dashboard.R"
  ),
  scrape = c(
    "post_check/exploratory_analyses/surveillance/scrape_data.R",
    "post_check/exploratory_analyses/surveillance/scrape_data_flu_subtypes.R"
  )
)

run_script <- function(rel_path) {
  abs_path <- here::here(rel_path)
  if (!file.exists(abs_path)) {
    return(list(
      script = rel_path,
      status = "missing",
      ok = FALSE,
      seconds = NA_real_
    ))
  }

  message("\n>>> ", rel_path)
  if (isTRUE(dry_run)) {
    return(list(
      script = rel_path,
      status = "dry-run",
      ok = TRUE,
      seconds = 0
    ))
  }

  t0 <- proc.time()[["elapsed"]]
  status <- system2(
    "Rscript",
    args = abs_path,
    stdout = "",
    stderr = ""
  )
  elapsed <- proc.time()[["elapsed"]] - t0
  ok <- identical(as.integer(status), 0L)
  list(
    script = rel_path,
    status = if (ok) "ok" else paste0("exit=", status),
    ok = ok,
    seconds = round(elapsed, 1)
  )
}

message("post_check recreate_all_outputs")
message("Repo root: ", here::here())
if (isTRUE(dry_run)) message("Mode: dry-run (no scripts executed)")
if (!is.null(stages_requested)) {
  message("Stages: ", paste(stages_requested, collapse = ", "))
} else {
  message(
    "Stages: ",
    paste(setdiff(known_stages, if (include_scrape) character() else "scrape"),
          collapse = ", ")
  )
}

results <- list()
for (stage in known_stages) {
  if (!want_stage(stage)) {
    next
  }
  message("\n========== STAGE: ", stage, " ==========")
  for (script in post_check_jobs[[stage]]) {
    results[[length(results) + 1L]] <- run_script(script)
  }
}

if (length(results) == 0L) {
  stop("No jobs selected. Check --only=... / --include-scrape.")
}

summary_df <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
failed <- summary_df[!summary_df$ok, , drop = FALSE]
n_ok <- sum(summary_df$ok)
n_all <- nrow(summary_df)

message("\n========== SUMMARY ==========")
message("Completed: ", n_ok, " / ", n_all)
if (nrow(failed) > 0) {
  message("Failed / missing:")
  for (i in seq_len(nrow(failed))) {
    message("  - ", failed$script[[i]], " [", failed$status[[i]], "]")
  }
  quit(status = 1)
}

message("All selected post_check jobs finished successfully.")
