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
study_start_date = study_dates[args[2]]
study_end_date = study_dates[args[3]]
index_date = study_start_date
registration_date = index_date - years(1)

#define patients age
age_at_start = patients.age_on(study_start_date)
age_at_end = patients.age_on(study_end_date)
age_months = (index_date - patients.date_of_birth).months
age_at_start_months = (study_start_date - patients.date_of_birth).months

#get patients who are registered, have sex, age, and imd info
registered_patients = case(
  when(cohort == "older_adults").then(practice_registrations
  .for_patient_on(registration_date).exists_for_patient()),
  when(cohort == "adults").then(practice_registrations
  .for_patient_on(registration_date).exists_for_patient()),
  when(cohort == "children_adolescents").then(practice_registrations
  .for_patient_on(registration_date).exists_for_patient()),
  otherwise = practice_registrations.for_patient_on(index_date).exists_for_patient()
)
is_female_or_male = patients.sex.is_in(["female", "male"])
is_appropriate_age = case(
  when(cohort == "older_adults").then((age_at_start <= 110) & (age_at_end >= 65)),
  when(cohort == "adults").then((age_at_start <= 64) & (age_at_end >= 18)),
  when(cohort == "children_adolescents").then((age_at_start <= 17) & (age_at_end >= 2)),
  when(cohort == "infants").then(age_at_start_months <= 23),
  when(cohort == "infants_subgroup").then(age_at_start_months <= 23)
)
has_imd = (addresses.for_patient_on(index_date).imd_rounded.is_not_null())

##exclusion criteria

#combined severe immunodeficiency syndrome
if cohort == "infants" or cohort == "infants_subgroup" :
  severe_immunodeficiency = (
    clinical_events.where(clinical_events.snomedct_code
    .is_in(codelists.severe_immunodeficiency_code))
    .exists_for_patient()
  )

#infant risk group (cardiac disease, pulmonary hypertension)
if cohort == "infants" or cohort == "infants_subgroup" :
  risk_group_infants = (case(
    when(age_months < 12).then(apcs.where(apcs.any_diagnosis
    .is_in(codelists.ventilation_codes))),
    when(age_months >=12 & age_months <= 23).then(apcs.where(apcs.any_diagnosis
    .is_in(codelists.ventilation_codes)) & 
    clinical_events.where(clinical_events.ctv3_code
    .is_in(codelists.cardiac_disease_codelist))
    .exists_for_patient()
    |clinical_events.where(clinical_events.snomedct_code
    .is_in(codelists.pulmonary_hypertension_codelist))
    .exists_for_patient()))
  )

#care home resident
if cohort == "older_adults" :
  care_home_tpp = (addresses.for_patient_on(index_date)
    .care_home_is_potential_match.when_null_then(False))
  care_home_code = (has_prior_event(codelists.carehome_codelist))
  care_home = care_home_tpp | care_home_code

#define population
if cohort == "infants" or cohort == "infants_subgroup" :
  dataset.define_population(
    registered_patients
    & is_female_or_male
    & is_appropriate_age
    & has_imd
    & (~severe_immunodeficiency)
    & (~risk_group_infants)
  )

if cohort == "older_adults" :
  dataset.define_population(
    registered_patients
    & is_female_or_male
    & is_appropriate_age
    & has_imd
    & (~care_home)
  )

if cohort == "adults" or cohort == "children_and_adolescents" :
  dataset.define_population(
    registered_patients
    & is_female_or_male
    & is_appropriate_age
    & has_imd
  )

#registration, sex and age 
dataset.registered = registered_patients
dataset.sex = patients.sex
dataset.age = case(
  when(cohort == "older_adults").then(age_at_start),
  when(cohort == "adults").then(age_at_start),
  when(cohort == "children_adolescents").then(age_at_start),
  when(cohort == "infants").then(age_at_start_months),
  when(cohort == "infants_subgroup").then(age_at_start_months)
)

#date of death
dataset.death_date = ons_deaths.date 

#define latest ethnicity code for patient
dataset.latest_ethnicity_code = (
  clinical_events.where(clinical_events.snomedct_code.is_in(codelists.ethnicity_codes))
  .where(clinical_events.date.is_on_or_before(index_date))
  .sort_by(clinical_events.date)
  .last_for_patient()
  .snomedct_code
)

#get patients IMD rank
dataset.imd_rounded = addresses.for_patient_on(index_date).imd_rounded

#get rural/urban classification
dataset.rural_urban_classification = addresses.for_patient_on(index_date).rural_urban_classification

#get patients household info
dataset.household_pseudo_id = household_memberships_2020.household_pseudo_id
dataset.household_size = household_memberships_2020.household_size

# #get patients practice's pseudonymised identifier
# dataset.practice = practice_registrations.for_patient_on(index_date).practice_pseudo_id

#practice and patient information
dataset.region = (practice_registrations.for_patient_on(index_date)).practice_nuts1_region_name
dataset.stp = (practice_registrations.for_patient_on(index_date)).practice_stp

##define functions for queries

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
        .sort_by(events.date)
        .last_for_patient()
    )
    
#query prior_events for date of earliest event-in-codelist
def first_prior_event(codelist, where=True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(events.date)
        .first_for_patient()
    )

#meds occurring before booster date
prior_meds = medications.where(medications.date.is_on_or_before(index_date))

#query prior_meds for existence of event-in-codelist
def has_prior_meds(codelist, where=True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .exists_for_patient()
    )

#query prior meds for date of most recent med-in-codelist
def last_prior_meds(codelist, where=True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .last_for_patient()
    )

#query prior_events for date of earliest event-in-codelist
def first_prior_meds(codelist, where=True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .first_for_patient()
    )

#infections occuring after index date but before study end date
infection_events = clinical_events.where(clinical_events.date.is_on_or_between(index_date, study_end_date))

#query infection_events for existence of event-in-codelist
def has_infection_event(codelist, where = True):
    return (
        infection_events.where(where)
        .where(infection_events.snomedct_code.is_in(codelist))
        .exists_for_patient()
    )

#query infection_events for date of most recent event-in-codelist
def last_infection_event(codelist, where = True):
    return (
        infection_events.where(where)
        .where(infection_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )
  
#query infection_events for date of earliest event-in-codelist
def first_infection_event(codelist, where=True):
    return (
        infection_events.where(where)
        .where(infection_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

##comorbidities

#define earliest vaccination date 
vaccination_date = study_start_date - years(1)

#define seasons for covid
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d")

#vaccinations
if cohort == "adults" or cohort == "older_adults" or cohort == "children_adolescents" :
  flu_vaccination = (
    vaccinations.where(vaccinations.target_disease.is_in(["Influenza"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(vaccination_date, index_date))
    .exists_for_patient()
  )

if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
  if cohort == "adults" or cohort == "older_adults" or cohort == "children_adolescents" :
    covid_vaccination_count = (
    vaccinations.where(vaccinations.target_disease.is_in(["SARS-COV-2"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_before(index_date))
    .count_for_patient()
  )

##outcomes

#rsv primary care
dataset.rsv_primary = (case(
  #primary analysis
  when(codelist_type == "specific")
  #presence of code in 'specific' primary care codelist
  .then((has_infection_event(codelists.rsv_primary_codelist))
  #presence of code in 'specific' emergency attendances codelist for infants
  |(case(when(cohort == "infants").then(emergency_care_attendances
  .where(emergency_care_attendances.diagnosis_01
  .is_in(codelists.bronchiolitis_attendance)).exists_for_patient()),
  when(cohort == "infants_subgroup").then(emergency_care_attendances
  .where(emergency_care_attendances.diagnosis_01
  .is_in(codelists.bronchiolitis_attendance)).exists_for_patient())))),
  #sensitivity analysis
  when(codelist_type == "sensitive")
  #presence of code in 'specific' primary care codelist
  .then((has_infection_event(codelists.rsv_primary_codelist))
  #presence of at least two codes within two weeks in 'sensitive' primary care codelist
  |(clinical_events.where(clinical_events.where(clinical_events.snomedct_code
  .is_in(codelists.rsv_sensitive_codelist)).where(clinical_events
  .date.is_on_or_between(first_infection_event(codelists
  .rsv_sensitive_codelist).date, first_infection_event(codelists
  .rsv_sensitive_codelist).date + days(14)))).count_distinct_for_patient()>1)
  #presence of code in 'sensitive' emergency care attendances codelist for infants
  |(case(when(cohort == "infants")
  .then(emergency_care_attendances.where(emergency_care_attendances
  .diagnosis_01.is_in(codelists.wheeze_attendance)).exists_for_patient()),
  when(cohort == "infants_subgroup").then(emergency_care_attendances
  .where(emergency_care_attendances.diagnosis_01
  .is_in(codelists.wheeze_attendance)).exists_for_patient())))
  #presence of code in 'sensitive' prescriptions codelist
  |(medications.where(medications.dmd_code
  .is_in(codelists.rsv_prescriptions_codelist)).exists_for_patient()))
))

# #date
# dataset.rsv_primary_date = (case(
#   when(codelist_type == "specific")
#   .then(first_infection_event(codelists.rsv_primary_codelist).date),
#   when(codelist_type == "sensitive")
#   .then(first_prior_event(codelists.rsv_primary_sens_codelist).date))
# )
# 
# #rsv secondary care
# dataset.rsv_secondary = (case(
#   when(codelist_type == "specific")
#   .then(apcs.where(apcs.primary_diagnosis
#   .is_in(codelists.rsv_secondary_codelist))
#   .exists_for_patient()
#   |apcs.where(apcs.secondary_diagnosis
#   .is_in(codelists.rsv_secondary_codelist)) 
#   .exists_for_patient()),
#   when(codelist_type == "sensitive")
#   .then(apcs.where(apcs.any_diagnosis
#   .is_in(codelists.rsv_secondary_codelist))
#   .exists_for_patient()
#   |apcs.where(apcs.any_diagnosis
#   .is_in(codelists.unspecified_lrti)) 
#   .exists_for_patient())
# ))
# 
# #date
# dataset.rsv_secondary_date = (case(
#   when(codelist_type == "specific")
#   .then(apcs.sort_by(apcs.admission_date)
#   .where(apcs.primary_diagnosis
#   .is_in(codelists.rsv_secondary_codelist) 
#   |apcs.secondary_diagnosis
#   .is_in(codelists.rsv_secondary_codelist))
#   .first_for_patient().admission_date),
#   when(codelist_type == "sensitive")
#   .then(apcs.sort_by(apcs.admission_date)
#   .where(apcs.any_diagnosis
#   .is_in(codelists.rsv_secondary_codelist) 
#   |apcs.any_diagnosis
#   .is_in(codelists.unspecified_lrti))
#   .first_for_patient().admission_date)
# ))
# 
# #covid primary care
# if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
#   
#   dataset.covid_primary = (case(
#     when(codelist_type == "specific")
#     .then(has_prior_event(codelists.covid_primary_codelist)),
#     when(codelist_type == "sensitive")
#     .then(has_prior_event(codelists.covid_sensitive_codelist)))
#   )
# 
# #date
#   dataset.covid_primary_date = (case(
#     when(codelist_type == "specific")
#     .then(first_prior_event(codelists.covid_primary_codelist)),
#     when(codelist_type == "sensitive")
#     .then(first_prior_event(codelists.covid_sensitive_codelist)))
#   )
# 
# #covid secondary care
#   dataset.covid_secondary = (case(
#     when(codelist_type == "specific").then(
#     apcs.where(apcs.primary_diagnosis
#     .is_in(codelists.covid_secondary_codelist)) 
#     .exists_for_patient()
#     |apcs.where(apcs.secondary_diagnosis
#     .is_in(codelists.covid_secondary_codelist)) 
#     .exists_for_patient()),
#     when(codelist_type == "sensitive").then(
#     apcs.where(apcs.any_diagnosis
#     .is_in(codelists.covid_secondary_codelist)) 
#     .exists_for_patient())
#   ))
# 
#   #date
#   dataset.covid_secondary_date = (case(
#     when(codelist_type == "specific")
#     .then(apcs.sort_by(apcs.admission_date)
#     .where(apcs.primary_diagnosis
#     .is_in(codelists.covid_secondary_codelist) 
#     |apcs.secondary_diagnosis
#     .is_in(codelists.covid_secondary_codelist))
#     .first_for_patient().admission_date),
#     when(codelist_type == "sensitive")
#     .then(apcs.sort_by(apcs.admission_date)
#     .where(apcs.any_diagnosis
#     .is_in(codelists.covid_secondary_codelist)))
#   ))
# 
# #flu primary care
# dataset.flu_primary = (
#   clinical_events.where(clinical_events.ctv3_code
#   .is_in(codelists.covid_primary_codelist)) #change codelist when available
#   .exists_for_patient()
# )
# 
# #date
# dataset.flu_primary_date = (
#   clinical_events.where(clinical_events.ctv3_code
#   .is_in(codelists.covid_primary_codelist)) #change codelist when available
#   .sort_by(clinical_events.date)
#   .first_for_patient().date
# )
# 
# #flu secondary care
# dataset.flu_secondary = (
#   apcs.where(apcs.primary_diagnosis
#   .is_in(codelists.covid_secondary_codelist)) #change codelist when available
#   .exists_for_patient()
#   |apcs.where(apcs.secondary_diagnosis
#   .is_in(codelists.covid_secondary_codelist)) #change codelist when available
#   .exists_for_patient()
# )
# 
# #date
# dataset.flu_secondary_date = (
#   apcs.sort_by(apcs.admission_date)
#   .where(apcs.primary_diagnosis.is_in(codelists.covid_secondary_codelist) #change codelist when available
#   |apcs.secondary_diagnosis.is_in(codelists.covid_secondary_codelist)) #change codelist when available
#   .first_for_patient().admission_date
# )

# ##comorbidities for secondary investigation
# 
# #medication date
# medication_date = index_date - years(1)
# 
# if investigation_type == secondary :
# 
#   #has asthma if there is an asthma diagnosis and a recent medication prescribed 
#   if cohort != "infants" and cohort != "infants_subgroup" :
#     dataset.has_asthma = (
#       clinical_events.where(clinical_events.snomedct_code.is_in(codelists.asthma_codelist))
#       .exists_for_patient() & medications.where(medications.dmd_code
#       .is_in(codelists.asthma_medications))
#       .where(medications.date.is_on_or_between(medication_date, index_date))
#       .exists_for_patient()
#     )
# 
#   #reactive airway disease diagnosis 
#   if cohort == "children_adolescents" : 
#     dataset.has_reactive_airway = (
#       clinical_events.where(clinical_events.snomedct_code
#       .is_in(reactive_airway_disease_code))
#       .exists_for_patient()
#     )
# 
#   if cohort == "adults" or cohort == "older_adults" :
#     
#     #copd diagnosis
#     dataset.has_copd = (
#       clinical_events.where(clinical_events.snomedct_code.is_in(codelists.copd_codelist))
#       # & medications.where(medications.dmd_code.is_in(codelists.copd_medications))
#       .exists_for_patient()
#     )
# 
#     #pulmonary fibrosis diagnosis
#     dataset.has_pulmonary_fibrosis = (
#       clinical_events.where(clinical_events.snomedct_code
#       .is_in(codelists.pulmonary_fibrosis_codelist))
#       .where(clinical_events.date.is_on_or_before(index_date))
#       .exists_for_patient()
#     )
# 
#     #cystic fibrosis diagnosis
#     dataset.has_cystic_fibrosis = (
#       clinical_events.where(clinical_events.snomedct_code
#       .is_in(codelists.cystic_fibrosis_codelist))
#       .where(clinical_events.date.is_on_or_before(index_date))
#       .exists_for_patient()
#     )  
# 
#     #diabetes diagnosis
#     diab_date = last_prior_event(codelists.diab).date
#     dmres_date = last_prior_event(codelists.dmres).date
#     dataset.has_diabetes = (
#       clinical_events.where(clinical_events.snomedct_code
#       .is_in(codelists.type1_diabetes_codelist))
#       .where(clinical_events.date.is_before(dmres_date))
#       .exists_for_patient() | clinical_events.where(clinical_events.snomedct_code
#       .is_in(codelists.non_type1_diabetes_codelist))
#       .where(clinical_events.date.is_before(dmres_date))
#       .exists_for_patient()
#     )
#   
#     #addison's disease diagnosis
#     dataset.has_addisons = (
#       clinical_events.where(clinical_events.snomedct_code
#       .is_in(codelists.addisons_codelist))
#       .where(clinical_events.date.is_on_or_before(index_date))
#       .exists_for_patient()
#     )  
#     
#     #Chronic Neurological Disease including Significant Learning Disorder
#     dataset.chronic_neuro_disease = has_prior_event(codelists.chronic_neuro_codelist)
#     
#     #Chronic Respiratory Disease
#     dataset.chronic_resp_disease = has_prior_event(codelists.respiratory_codelist)
#     
#     #Calculate BMI
#     # BMI
#     bmi_measurement = most_recent_bmi(
#         where = events.date.is_after(index_date - days(5 * 365)),
#         minimum_age_at_measurement = 16,
#     )
#     bmi_value = bmi_measurement.numeric_value
#     
#     dataset.bmi = case(
#         when(bmi_value < 30).then("Not obese"), # include this here to ensure this value is the 1st level in the factor
#         when((bmi_value >= 30.0) & (bmi_value < 35.0)).then("Obese I (30-34.9)"),
#         when((bmi_value >= 35.0) & (bmi_value < 40.0)).then("Obese II (35-39.9)"),
#         # Set maximum to avoid any impossibly extreme values being classified as obese
#         when((bmi_value >= 40.0) & (bmi_value < 100.0)).then("Obese III (40+)"),
#         default = "Not obese", # assume missing is non-obese
#     )
#     
#     # Severe Obesity
#     bmi_stage_event = last_prior_event(codelists.bmi_stage)
#     sev_obesity_event = last_prior_event(
#       codelists.sev_obesity,
#       where = ((events.date >= bmi_stage_event.date) & (events.numeric_value != 0.0)),
#     )
#     bmi_event = last_prior_event(codelists.bmi, where = (events.numeric_value != 0.0))
#   
#     dataset.sev_obesity = case(
#       when(sev_obesity_event.date > bmi_event.date).then(True),
#       when(bmi_event.numeric_value >= 40.0).then(True),
#       default = False
#     )
