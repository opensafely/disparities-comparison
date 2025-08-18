**Analysis Folder**

Within this folder are scripts and folders relating to:

- Data extraction (scripts containing `_dataset_definition_` within the name)
- Data processing (scripts containing `_processing_` within the name)
- Data description
- Sensitivity analyses (scripts within the [sensitivity_analyses](https://github.com/opensafely/disparities-comparison/tree/main/analysis/sensitivity_analyses) folder)
- Secondary analyses (scripts within [secondary_analyses](https://github.com/opensafely/disparities-comparison/tree/main/analysis/secondary_analyses) folder)

In addtion, scripts for statistical analyses of all outcomes are separated by the pathogen of interest. Within the relevant folders are a number of scripts containing statistical models for the following combinations of exposures:

- Ethnicity
- Socioeconomic status
- Household composition (for 2020-21 season)
- Ethnicity + socioeconomic status
- Ethnicity + household composition (for 2020-21 season)
- Socioeconomic status + household composition (for 2020-21 season)
- Ethnicity + socioeconomic status + household composition (for 2020-21 season)

Scripts for `_further_` models are also contained within these folders, these scripts have the same exposure combinations as well as additional covariates on top of the baseline inclusions (age and sex). These are:

- Rurality
- Vaccination status (for relevant cohorts and outcomes)
- Maternal characteristics (for maternally linked infants)

Scripts are separate for the various models due to computational time, where possible these scripts are sourced within a _master_ script which allows for one action to run several model scripts.
