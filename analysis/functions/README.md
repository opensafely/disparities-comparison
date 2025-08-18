Functions which are used for the analysis pipeline are contained within here:

- `model.R` contains two functions which employ the base `glm` function for `family = Poisson` but with variables relevant to analysis
- `event_count.R` provides a method by which to ensure models run are only done so when at least one event occurs in all subgroups
- `expand_with_tmerge.R` uses the `tmerge` function to expand survival time for patients with time updating vaccination variables
- `redaction.R` creates a number of functions which can be used to apply statistical disclosure controls
