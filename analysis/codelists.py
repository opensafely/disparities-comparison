# import ehrql function for importing codelists
from ehrql import (
  codelist_from_csv
)

# ethnicity codes
ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    column = "snomedcode",
    category_column = "Grouping_6",
)
ethnicity_codes_16 = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    column = "snomedcode",
    category_column = "Grouping_16",
)

# smoking
clear_smoking_codes = codelist_from_csv(
    "codelists/opensafely-smoking-clear.csv",
    column = "CTV3Code",
    category_column = "Category",
)

unclear_smoking_codes = codelist_from_csv(
    "codelists/opensafely-smoking-unclear.csv",
    column = "CTV3Code",
    category_column = "Category",
)

# drinking
drinking_codelist = codelist_from_csv(
  "codelists/user-angel-wong-hazardous-drinking.csv",
  column = "code",
)

# illicit substances 
drug_usage_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-illsub_cod.csv",
  column = "code",
)
drug_intervention_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-illsubint_cod.csv",
  column = "code",
)
drug_assessment_declination_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-illsubassdec_cod.csv",
  column = "code",
)

# asthma diagnosis
asthma_codelist = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ast_cod.csv",
    column = "code",
)

# asthma medications
asthma_medications = codelist_from_csv(
  "codelists/opensafely-asthma-inhaler-steroid-medication.csv",
  column = "code",
)

# reactive airway disease code
reactive_airway_disease_code = ["266898002"]

# COPD
copd_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-copd_cod.csv",
  column = "code",
)
copd_medications = codelist_from_csv(
  "codelists/bristol-copd-medications-bnf.csv",
  column = "code",
)

# pulmonary fibrosis
pulmonary_fibrosis_codelist = codelist_from_csv(
  "codelists/bristol-ild-snomed.csv",
  column = "code",
)

# # hypertension
# hypertension_codelist = codelist_from_csv(
#   "codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv",
#   column = "code",
# )
# 
# # type 1 diabetes
# type1_diabetes_codelist = codelist_from_csv(
#   "codelists/nhsd-primary-care-domain-refsets-dmtype1_cod.csv",
#   column = "code",
# )
# 
# # non-type 1 diabetes
# non_type1_diabetes_codelist = codelist_from_csv(
#   "codelists/nhsd-primary-care-domain-refsets-dmnontype1_cod.csv",
#   column = "code",
# )
# 
# # heart failure
# heart_failure_codelist = codelist_from_csv(
#   "codelists/nhsd-primary-care-domain-refsets-hf_cod.csv",
#   column = "code",
# )
# 
# # prior MI
# prior_mi_codelist = codelist_from_csv(
#   "codelists/nhsd-primary-care-domain-refsets-mi_cod.csv",
#   column = "code",
# )

# rsv primary - specific 

# rsv secondary - specific

# covid primary - specific
covid_primary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
  column = "CTV3ID",
)

# covid secondary - specific
covid_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-identification.csv",
  column = "icd10_code",
)

# flu primary - specific

# flu secondary - specific

# rsv primary - sensitive 

# rsv secondary - sensitive

# covid primary - sensitive
covid_primary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
  column = "CTV3ID",
)

# covid secondary - sensitive
covid_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-identification.csv",
  column = "icd10_code",
)

# flu primary - sensitive

# flu secondary - sensitive

##exclusion criteria

# care home 
care_home_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-carehome_cod.csv",
  column = "code",
)

# severe combined immunodeficiency syndrome
severe_immunodeficiency_code = ["31323000"]

# requiring ventilation
ventilation_codes = ["Z99.1", "P28.8"]

# cardiac disease
cardiac_disease_codelist = codelist_from_csv(
  "codelists/opensafely-chronic-cardiac-disease.csv",
  column = "CTV3ID",
)

# pulmonary hypertension
pulmonary_hypertension_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-pulmohyp_cod.csv",
  column = "code", 
)
