# Comparing disparities in RSV, influenza, and COVID-19

[View on OpenSAFELY](https://jobs.opensafely.org/repo/https%253A%252F%252Fgithub.com%252Fopensafely%252Fdisparities-comparison)

# About this project

This repo has been used to perform analyses on data from OpenSAFELY within the secure research environment. The code within the repo is used across the following variations:

- Season: each season starts on September 1st of the relevant year, and ends on August 31st of the following year. The first season is 2016-17 and the final season is 2023-24
- Cohort: there are five available cohorts:
   - older adults (65+)
   - adults (18-65)
   - children and adolescents (2-18)
   - infants (0-2)
   - infants who have been linked to their mother (0-2)
- Investigation type:
    - primary
    - secondary
    - sensitivity
- Codelist type: these are according to [phenotypes](https://github.com/opensafely/disparities-comparison/issues/1) designed with clinician input and can be:
    - specific
    - sensitive

For almost every combination of these factors the following outcomes are explored:

- Mild RSV
- Severe RSV
- Mild influenza
- Severe influenza
- Mild COVID-19
- Severe COVID-19

For sensitive phenotypes additional outcomes are explored:

- Mild overall respiratory virus
- Severe overall respiratory virus

For more information on the analyses performed, see the [protocol](https://github.com/opensafely/disparities-comparison/blob/main/docs/Finalised%20OpenSAFELY%20Protocol%20(Comparison%20of%20Disparities%20in%20RSV%2C%20influenza%2C%20and%20COVID-19).pdf)

Details of the purpose and any published outputs from this project can be found at the link above.

The contents of this repository MUST NOT be considered an accurate or valid representation of the study or its purpose. 
This repository may reflect an incomplete or incorrect analysis with no further ongoing work.
The content has ONLY been made public to support the OpenSAFELY [open science and transparency principles](https://www.opensafely.org/about/#contributing-to-best-practice-around-open-science) and to support the sharing of re-usable code for other subsequent users.
No clinical, policy or safety conclusions must be drawn from the contents of this repository.

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 

# Repository details

Scripts for code which is run within the secure OpenSAFELY environment can be found within the [analysis](https://github.com/opensafely/disparities-comparison/tree/main/analysis) folder and scripts which are for use on [released outputs](https://jobs.opensafely.org/comparing-disparities-in-rsv-influenza-and-covid-19/disparities-comparison-rsv-flu-c19/outputs/) can be found within the [post_check](https://github.com/opensafely/disparities-comparison/tree/main/post_check) folder. 

Within the [analysis](https://github.com/opensafely/disparities-comparison/tree/main/analysis) folder are scripts and folders relating to:

- Data extraction (scripts containing _dataset_definition_ within the name)
- Data processing (scripts containing __processing_ within the name)
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

Scripts for _further_ models are also contained within these folders, these scripts have the same exposure combinations as well as additional covariates on top of the baseline inclusions (age and sex). These are:

- Rurality
- Vaccination status (for relevant cohorts and outcomes)
- Maternal characteristics (for maternally linked infants)

Scripts are separate for the various models due to computational time, where possible these scripts are sourced within a _master_ script which allows for one action to run several model scripts.
