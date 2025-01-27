import json, sys
from pathlib import Path 

from datetime import date, datetime
from ehrql import Dataset, case, when, maximum_of, minimum_of, years, days
from ehrql.tables.tpp import ( 
  patients, 
  medications,
  ons_deaths,
  addresses, 
  clinical_events,
  practice_registrations,
  household_memberships_2020,
  vaccinations,
  apcs,
  emergency_care_attendances
)

from variable_lib import (
  has_a_continuous_practice_registration_spanning,
  most_recent_bmi,
  practice_registration_as_of,
  emergency_care_diagnosis_matches,
  hospitalisation_diagnosis_matches
)

import codelists

dataset = Dataset()

#######################################################################################
# Import study dates defined in "./analysis/design/study-dates.R" script and then exported
# to JSON
#######################################################################################
study_dates = json.loads(
  Path("analysis/design/study-dates.json").read_text(),
)

args = sys.argv

#define dataset definition settings from command line arguments
cohort = args[1]
codelist_type = args[4]
investigation_type = args[5]

# Change these in ./analysis/design/study-dates.R if necessary
study_start_date = datetime.strptime(study_dates[args[2]], "%Y-%m-%d").date()
study_end_date = datetime.strptime(study_dates[args[3]], "%Y-%m-%d").date()

# #define patients age
# age_at_start = patients.age_on(study_start_date)
# age_at_end = patients.age_on(study_end_date)
# age_at_start_months = (study_start_date - patients.date_of_birth).months

#get date patient ages into cohort 
if cohort == "infants" or cohort == "infants_subgroup" :
  age_date = patients.date_of_birth
elif cohort == "children_and_adolescents" :
  age_date = patients.date_of_birth + years(2) 
elif cohort == "adults" :
  age_date = patients.date_of_birth + years(18)
else :
  age_date = patients.date_of_birth + years(65)

#set index date (and registration date) as last date of either start date or age date
#so that patients are the correct age for the cohort when looking at records
if cohort == "infants" or cohort == "infants_subgroup" :
  index_date = maximum_of(study_start_date, study_start_date)
else : 
  index_date = maximum_of(study_start_date, age_date)

registration_date = index_date - years(1)


#events occurring before index date
prior_events = clinical_events.where(clinical_events.date.is_on_or_before(index_date))

#query prior_events for existence of event-in-codelist
def has_prior_event(codelist, where = True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .exists_for_patient()
    )

#query prior_events for date of most recent event-in-codelist
def last_prior_event(codelist, where = True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )
    
#query prior_events for date of earliest event-in-codelist
def first_prior_event(codelist, where = True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

#meds occurring before booster date
prior_meds = medications.where(medications.date.is_on_or_before(index_date))

#query prior_meds for existence of event-in-codelist
def has_prior_meds(codelist, where = True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .exists_for_patient()
    )

#query prior meds for date of most recent med-in-codelist
def last_prior_meds(codelist, where = True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .last_for_patient()
    )

#query prior_events for date of earliest event-in-codelist
def first_prior_meds(codelist, where = True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .first_for_patient()
    )

##comorbidities for secondary investigation

def filter_codes_by_category(codelist, include):
    return {k:v for k,v in codelist.items() if v in include}

#lifestyle


#smoking 

###############################################################################
# from https://github.com/opensafely/early-inflammatory-arthritis/blob/069e61712fcc9a0c2ec2804ff36a9b773073291c/analysis/dataset_definition.py#L136
###############################################################################
  
most_recent_smoking_code = (
  (clinical_events.where(clinical_events.ctv3_code
  .is_in(codelists.clear_smoking_codes))
  .sort_by(clinical_events.date).last_for_patient()
  .ctv3_code.to_category(codelists.clear_smoking_codes))
)
ever_smoked = (
  clinical_events.where(clinical_events.ctv3_code
  .is_in(filter_codes_by_category(codelists
  .clear_smoking_codes, include = ["S", "E"])))
  .exists_for_patient()
)
smoking_status = (case(
  when(most_recent_smoking_code == "S").then("Current"),
  when((most_recent_smoking_code == "E") 
  | ((most_recent_smoking_code == "N") 
  & (ever_smoked == True))).then("Former"),
  when((most_recent_smoking_code == "N") 
  & (ever_smoked == False)).then("Never"),
  otherwise = None)
)
  
#drinking 
hazardous_drinking = has_prior_event(codelists.drinking_codelist)
  
#drug usage 
drug_usage = (
  (has_prior_event(codelists.drug_usage_codelist + 
  codelists.drug_intervention_codelist +
  codelists.drug_assessment_declination_codelist))
)
  
#medication date
medication_date = index_date - years(1)

#reactive airway disease diagnosis 
has_reactive_airway = has_prior_event(codelists.reactive_airway_disease_code)

#has asthma if there is an asthma diagnosis and a recent medication prescribed 
has_asthma = (
  (has_prior_event(codelists.asthma_codelist))
  & (has_prior_meds(codelists.asthma_oral_medications,
  where = medications.date.is_on_or_between(medication_date, index_date))
  |(has_prior_meds(codelists.asthma_inhaled_medications)))
)
  
#copd diagnosis
copd_res = (case(
  when(last_prior_event(codelists.copd_resolved_codelist).date
  .is_on_or_after(last_prior_event(codelists.copd_codelist).date))
  .then(True), otherwise = False)
)
has_copd = (case(
  when(last_prior_event(codelists.copd_resolved_codelist).date
  .is_on_or_after(last_prior_event(codelists.copd_codelist).date))
  .then(False), when(last_prior_event(codelists
  .copd_qof_codelist).date.is_on_or_after(
  last_prior_event(codelists.copd_codelist).date))
  .then(False), when(((has_prior_event(codelists.copd_codelist)) 
  | (has_prior_meds(codelists.copd_medications)) 
  | (has_prior_event(codelists.copd_qof_codelist)))
  & (~copd_res)).then(True), otherwise = False)
)
  
#cystic fibrosis diagnosis
has_cystic_fibrosis = (
  clinical_events.where(clinical_events.ctv3_code.
  is_in(codelists.cystic_fibrosis_codelist))
  .exists_for_patient()
)

#pulmonary fibrosis diagnosis
has_pulmonary_fibrosis = (
  has_prior_event(codelists
  .pulmonary_fibrosis_codelist)
)

#Chronic Respiratory Disease
has_crd = has_prior_event(codelists.crd_codelist)

#other chronic respiratory disease
has_other_resp = (case(
  when((has_pulmonary_fibrosis | has_crd) & 
  (~has_asthma & ~ has_copd & ~ has_cystic_fibrosis)).then(True),
  otherwise = False)
)

#diabetes diagnosis
diab_date = last_prior_event(codelists.diabetes_codelist).date
dmres_date = last_prior_event(codelists.diabetes_resolved_codelist).date
has_diabetes = (case(
  when(dmres_date < diab_date).then(True),
  when(diab_date.is_not_null() & dmres_date.is_null())
  .then(True), otherwise = False)
)

#addison's disease diagnosis
has_addisons = (
  clinical_events.where(clinical_events.snomedct_code
  .is_in(codelists.addisons_codelist))
  .where(clinical_events.date.is_on_or_before(index_date))
  .exists_for_patient()
)
  
#Calculate BMI
  
###############################################################################
# from https://github.com/opensafely/comparative-booster-spring2023/blob/main/analysis/dataset_definition.py
###############################################################################

# BMI
bmi_measurement = most_recent_bmi(
  where = clinical_events.date.is_after(index_date - years(5)),
  minimum_age_at_measurement = 16,
)
bmi_value = bmi_measurement.numeric_value
bmi = case(
  when(bmi_value < 30).then("Not obese"), # include this here to ensure this value is the 1st level in the factor
  when((bmi_value >= 30.0) & (bmi_value < 35.0)).then("Obese I (30-34.9)"),
  when((bmi_value >= 35.0) & (bmi_value < 40.0)).then("Obese II (35-39.9)"),
  # Set maximum to avoid any impossibly extreme values being classified as obese
  when((bmi_value >= 40.0) & (bmi_value < 100.0)).then("Obese III (40+)"),
  otherwise = "Not obese", # assume missing is non-obese
)

# Severe Obesity
bmi_stage_event = last_prior_event(codelists.bmi_stage_codelist)
sev_obesity_event = last_prior_event(
  codelists.severe_obesity_codelist,
  where = ((clinical_events.date >= bmi_stage_event.date) 
  & (clinical_events.numeric_value != 0.0)),
)
bmi_event = last_prior_event(codelists.bmi_codelist, 
  where = (clinical_events.numeric_value != 0.0))
severe_obesity = case(
  when(sev_obesity_event.date > bmi_event.date).then(True),
  when(bmi_event.numeric_value >= 40.0).then(True),
  otherwise = False
)
  
#Chronic Heart Diseases
has_chd = (
  (has_prior_event(codelists.chd_codelist)) | 
  (has_prior_event(codelists.heart_failure_codelist)) |
  (has_prior_event(codelists.coronary_heart_disease_codelist))
)

#Chronic Kidney Disease
  
###############################################################################
# from https://github.com/opensafely/comparative-booster-spring2023/blob/main/analysis/dataset_definition.py
###############################################################################  
  
#chronic kidney disease diagnostic codes 
ckd = has_prior_event(codelists.ckd_codelist)
#chronic kidney disease codes - all stages
ckd15_date = last_prior_event(codelists.ckd15_codelist).date
#chronic kidney disease codes-stages 3 - 5
ckd35_date = last_prior_event(codelists.ckd35_codelist).date
has_ckd = case(
  when(ckd).then(True),
  when((ckd35_date >= ckd15_date)).then(True),
  otherwise = False
)

#Chronic Liver Disease
has_cld = has_prior_event(codelists.cld_codelist)

#Chronic Neurological Disease including Significant Learning Disorder
has_cnd = has_prior_event(codelists.cnd_codelist)

#Cancer within 3 years 
has_cancer = (
  has_prior_event(codelists.cancer_codelist +
  codelists.haemotalogical_cancer_codelist +
  codelists.lung_cancer_codelist, 
  where = clinical_events.date.is_after(index_date - years(3)))
)
  
#Immunosuppression
  
###############################################################################
# from https://github.com/opensafely/comparative-booster-spring2023/blob/main/analysis/dataset_definition.py
###############################################################################    
  
#Immunosuppression diagnosis 
immdx = has_prior_event(codelists.immunosuppression_diagnosis_codelist)

#Immunosuppression medication 
immrx = has_prior_meds(
  codelists.immunosuppression_medications_codelist,
  where = (medications.date.is_on_or_after(index_date - years(3)))
)

#Immunosuppression admin date
immadm = has_prior_event(
  codelists.immunosuppression_admin_codelist,
  where = (clinical_events.date
  .is_on_or_after(index_date - years(3)))
)

#Chemotherapy medication date
dxt_chemo = has_prior_event(
  codelists.chemo_codelist,
  where = (clinical_events.date
  .is_on_or_after(index_date - years(3)))
)

#Immunosuppression group
immunosuppressed = immdx | immrx | immadm | dxt_chemo
  
#Sickle Cell Disease
has_sickle_cell = has_prior_event(codelists.sickle_cell_codelist)
