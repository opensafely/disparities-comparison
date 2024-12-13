# import ehrql function for importing codelists
from ehrql import (
  codelist_from_csv
)

## exposures 

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

##exclusion criteria

# care home 
carehome_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-carehome_cod.csv",
  column = "code",
)

# severe combined immunodeficiency syndrome
severe_immunodeficiency_code = ["31323000"]

# requiring ventilation
ventilation_codes = ["Z991", "P288"]

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


##outcomes

# rsv primary - specific 
rsv_primary_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-primary-care.csv",
  column = "code",
)

# rsv emergency attendances - specific
bronchiolitis_attendance = ["4120002"]

# rsv secondary - specific
rsv_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-secondary-care.csv",
  column = "code",
)

# rsv primary - sensitive 
rsv_sensitive_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-primary-care-maximal-sensitivity.csv",
  column = "code",
)

# rsv emergency attendances - sensitive
wheeze_attendance = ["4120002", "276191000000107"]

# rsv primary - sensitive (prescriptions)
rsv_prescriptions_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-identification-prescriptions-maximal-sensitivity-dmd.csv",
  column = "dmd_id",
)

# rsv primary exclusion
rsv_primary_exclusion_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-exclusion-primary-care-maximal-sensitivity.csv",
  column = "code",
)

# rsv secondary - sensitive
unspecified_lrti = ["J22"]

# rsv secondary exclusion 
rsv_secondary_exclusion_codelist = codelist_from_csv(
  "codelists/opensafely-rsv-exclusion-secondary-care-maximal-sensitivity.csv",
  column = "code",
)

# flu primary - specific
flu_primary_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-primary-care.csv",
  column = "code",
)

# flu secondary - specific
flu_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-secondary-care.csv",
  column = "code",
)

# flu primary - sensitive
flu_sensitive_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-primary-care-maximal-sensitivity.csv",
  column = "code",
)

# acute respiratory illness primary
ari_primary_codelist = codelist_from_csv(
  "codelists/opensafely-acute-respiratory-illness-primary-care.csv",
  column = "code",
)

# fever 
fever_codelist = codelist_from_csv(
  "codelists/opensafely-symptoms-fever.csv",
  column = "code",
)

# cough
cough_codelist = codelist_from_csv(
  "codelists/opensafely-symptoms-cough.csv",
  column = "code",
)

# flu primary - sensitive (prescriptions)
flu_prescriptions_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-identification-prescriptions-maximal-sensitivity-dmd.csv",
  column = "dmd_id",
)

# flu primary exclusion
flu_primary_exclusion_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-exclusion-primary-care-maximal-sensitivity.csv",
  column = "code",
)

# acute respiratoy illness secondary
ari_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-acute-respiratory-illness-secondary-care.csv",
  column = "code",
)

# flu secondary exclusion
flu_secondary_exclusion_codelist = codelist_from_csv(
  "codelists/opensafely-influenza-exclusion-secondary-care-maximal-sensitivity.csv",
  column = "code",
)

# covid primary - specific
covid_primary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-identification-primary-care.csv",
  column = "code",
)

# covid secondary - specific
covid_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-covid-identification.csv",
  column = "icd10_code",
)

# covid primary - sensitive
covid_sensitive_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-identification-primary-care-maximal-sensitivity.csv",
  column = "code",
)

# covid primary - sensitive (prescriptions)
covid_prescriptions_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-identification-prescriptions-dmd.csv",
  column = "dmd_id",
)

# covid primary exclusion
covid_primary_exclusion_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-exclusion-primary-care-maximal-sensitivity.csv",
  column = "code",
)

# covid secondary - sensitive
coronavirus_unspecified = ["B972", "B342"]

# covid secondary exclusion
covid_secondary_exclusion_codelist = codelist_from_csv(
  "codelists/opensafely-covid-19-exclusion-secondary-care-maximal-sensitivity.csv",
  column = "code",
)

# unspecified respiratory virus primary
respiratory_virus_primary_codelist = codelist_from_csv(
  "codelists/opensafely-respiratory-virus-unspecified-identification-primary-care.csv",
  column = "code",
)

#copd exacerbation primary
copd_exacerbation_primary_codelist = codelist_from_csv(
  "codelists/bristol-copd-exacerbations-snomed.csv",
  column = "code",
)

#asthma exacerbation primary
asthma_exacerbation_primary_codelist = codelist_from_csv(
  "codelists/bristol-asthma_exacerbations_snomed.csv",
  column = "code",
)

# respiratory tract infection emergency attendances
rtri_attendance = ["50417007", "54150009"]

# acute COPD exacerbation emergency attendances
copd_exacerbation_attendance = ["195951007"]

# unspecified respiratory virus exlcusion primary 
respiratory_virus_primary_exclusion_codelist = codelist_from_csv(
  "codelists/opensafely-respiratory-virus-unspecified-exclusion-primary-care.csv",
  column = "code",
)

# unspecified respiratory virus secondary
respiratory_virus_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-respiratory-virus-unspecified-identification-secondary-care.csv",
  column = "code",
)

#copd exacerbation secondary
copd_exacerbation_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-copd-exacerbation.csv",
  column = "code",
)

#asthma exacerbation secondary
asthma_exacerbation_secondary_codelist = codelist_from_csv(
  "codelists/opensafely-copd-exacerbation.csv",
  column = "code",
)

# unspecified respiratory virus exclusion secondary
respiratory_virus_secondary_exclusion_codelist = codelist_from_csv(
  "codelists/opensafely-respiratory-virus-unspecified-exclusion-secondary-care.csv",
  column = "code",
)


##comorbidities

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
  "codelists/primis-covid19-vacc-uptake-ast.csv",
  column = "code",
)

# asthma inhaled medications
asthma_inhaled_medications = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astrxm1.csv",
  column = "code",
)

# asthma oral medications
asthma_oral_medications = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astrxm2.csv",
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
  "codelists/opensafely-copd-medications-new-dmd.csv",
  column = "dmd_id",
)
copd_resolved_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-copdres_cod.csv",
  column = "code",
)
copd_qof_codelist = codelist_from_csv(
  "codelists/opensafely-chronic-obstructive-pulmonary-disease-copd-review-qof.csv",
  column = "code",
)

# pulmonary fibrosis
pulmonary_fibrosis_codelist = codelist_from_csv(
  "codelists/bristol-ild-snomed.csv",
  column = "code",
)

# cystic fibrosis
cystic_fibrosis_codelist = codelist_from_csv(
  "codelists/opensafely-cystic-fibrosis.csv",
  column = "CTV3ID",
)
 
# diabetes
diabetes_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-diab.csv",
  column = "code",
)

# diabetes resolved
diabetes_resolved_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-dmres.csv",
  column = "code",
)

# addisons
addisons_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-addis_cod.csv",
  column = "code",
)

# BMI
bmi_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-bmi.csv",
  column = "code",
)

# BMI stage
bmi_stage_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-bmi_stage.csv",
  column = "code",
)

# severe obesity
severe_obesity_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-sev_obesity.csv",
  column = "code",
)

# chronic heart disease
chd_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-chd_cov.csv",
  column = "code",
)

# chronic kidney disease (no stage) 
ckd_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd_cov.csv",
  column = "code",
)

# chronic kidney disease (all stages)
ckd15_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd15.csv",
  column = "code",
)

# chronic kidney disease (late stages)
ckd35_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd35.csv",
  column = "code",
)

# chronic liver disease
cld_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cld.csv",
  column = "code",
)

# chronic respiratory disease
crd_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-resp_cov.csv",
  column = "code",
)

# chronic neurological disease
cnd_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
  column = "code",
)

# cancer
cancer_codelist = codelist_from_csv(
  "codelists/opensafely-cancer-excluding-lung-and-haematological-snomed.csv",
  column = "id",
)
haemotalogical_cancer_codelist = codelist_from_csv(
  "codelists/opensafely-haematological-cancer-snomed.csv",
  column = "id",
)
lung_cancer_codelist = codelist_from_csv(
  "codelists/opensafely-lung-cancer-snomed.csv",
  column = "id",
)

# immunosuppresion
immunosuppression_diagnosis_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immdx_cov.csv",
  column = "code",
)
immunosuppression_medications_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immrx.csv",
  column = "code",
) #dm+d codes
immunosuppression_admin_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immunosuppression-admin-codes.csv",
  column = "code",
)
chemo_codelist = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-dxt_chemo_cod.csv",
  column = "code",
)

# sickle cell disease
sickle_cell_codelist = codelist_from_csv(
  "codelists/opensafely-sickle-cell-disease-snomed.csv",
  column = "id",
)

# heart failure
heart_failure_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-hf_cod.csv",
  column = "code",
)

# coronary heart disease
coronary_heart_disease_codelist = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-chd_cod.csv",
  column = "code",
)
