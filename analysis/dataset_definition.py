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
  hospitalisation_diagnosis_matches,
  cause_of_death_matches
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
#age_at_end_months = (study_end_date - patients.date_of_birth).months

#get patients who are registered
if cohort == "infants" or cohort == "infants_subgroup" :
  registered_patients = practice_registrations.for_patient_on(index_date).exists_for_patient()
else :
  registered_patients = practice_registrations.for_patient_on(registration_date).exists_for_patient()

#have sex
is_female_or_male = patients.sex.is_in(["female", "male"])

#have age
if cohort == "infants" or cohort == "infants_subgroup" :
  is_appropriate_age = (age_at_start_months <= 23) & (age_at_start_months >= 0)
elif cohort == "children_and_adolescents" :
  is_appropriate_age = (age_at_start <= 17) & (age_at_end >= 2)
elif cohort == "adults" :
  is_appropriate_age = (age_at_start <= 64) & (age_at_end >= 18)
else :
  is_appropriate_age = (age_at_start <= 110) & (age_at_end >= 65)

#have imd
has_imd = (addresses.for_patient_on(index_date).imd_rounded.is_not_null())

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
def first_prior_event(codelist, where = True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(events.date)
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

#infections occuring after index date but before study end date
infection_events = clinical_events.where(clinical_events.date.is_on_or_between(index_date, study_end_date))

#query infection_events for existence of event-in-codelist 
def is_infection_event(codelist, where = True):
    return (
        infection_events.where(where)
        .where(infection_events.snomedct_code.is_in(codelist))
    )

#query infection_events for existence of event-in-codelist (get first of these)
def has_infection_event(codelist, where = True):
    return (
        infection_events.where(where)
        .where(infection_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
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
def first_infection_event(codelist, where = True):
    return (
        infection_events.where(where)
        .where(infection_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

##exclusion criteria

#combined severe immunodeficiency syndrome
if cohort == "infants" or cohort == "infants_subgroup" :
  severe_immunodeficiency = (
    has_prior_event(codelists
    .severe_immunodeficiency_code)
  )

#infant risk group (cardiac disease, pulmonary hypertension)
if cohort == "infants" or cohort == "infants_subgroup" :
  risk_group_infants = (case(
    when(age_months < 12)
    .then(hospitalisation_diagnosis_matches(codelists.ventilation_codes)
    .exists_for_patient()),
    when((age_months >=12) & (age_months <= 23) & 
    ((clinical_events.where(clinical_events.ctv3_code
    .is_in(codelists.cardiac_disease_codelist))
    .exists_for_patient())
    |(clinical_events.where(clinical_events.snomedct_code
    .is_in(codelists.pulmonary_hypertension_codelist))
    .exists_for_patient())))
    .then(hospitalisation_diagnosis_matches(codelists.ventilation_codes)
    .exists_for_patient()))
  )

#care home resident
#if cohort == "older_adults" :
care_home_tpp = (
  addresses.for_patient_on(index_date)
  .care_home_is_potential_match.when_null_then(False)
)
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
    & (~care_home)
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
    & (~care_home)
  )

#registration and sex  
dataset.registered = registered_patients
dataset.sex = patients.sex

#age
if cohort == "infants" or cohort == "infants_subgroup" :
  dataset.age = age_at_start_months
else:
  dataset.age = age_at_start


#date of death
dataset.death_date = ons_deaths.date 

#define latest ethnicity code for patient
dataset.latest_ethnicity_code = (
  clinical_events.where(clinical_events
  .snomedct_code.is_in(codelists.ethnicity_codes))
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

#get patients practice's pseudonymised identifier
dataset.practice_pseudo_id = (
  (practice_registrations.for_patient_on(index_date))
  .practice_pseudo_id
)

#practice and patient information
dataset.region = (
  (practice_registrations.for_patient_on(index_date))
  .practice_nuts1_region_name
)
dataset.stp = (
  (practice_registrations.for_patient_on(index_date))
  .practice_stp
)

#date deregistered from practice
dataset.deregistration_date = (
  (practice_registrations
  .for_patient_on(index_date)).end_date
)

##comorbidities

#define earliest vaccination date 
vaccination_date = study_start_date - years(1)

#define seasons for covid
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d")

#vaccinations
if cohort == "adults" or cohort == "older_adults" or cohort == "children_and_adolescents" :
  dataset.flu_vaccination = (
    vaccinations.where(vaccinations.target_disease.is_in(["Influenza"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(vaccination_date, index_date))
    .exists_for_patient()
  )

  if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
    if cohort == "adults" or cohort == "older_adults" or cohort == "children_and_adolescents" :
      dataset.covid_vaccination_count = (
      vaccinations.where(vaccinations.target_disease.is_in(["SARS-COV-2"]))
      .sort_by(vaccinations.date)
      .where(vaccinations.date.is_on_or_before(index_date))
      .count_for_patient()
    )

##outcomes - rsv

#count number of clinical codes in RSV sensitive codelist
rsv_code_number = (
  (clinical_events.where(clinical_events
  .date.is_on_or_between(first_infection_event(codelists
  .rsv_sensitive_codelist).date, first_infection_event(codelists
  .rsv_sensitive_codelist).date + days(14)))
  .where(clinical_events.snomedct_code
  .is_in(codelists.rsv_sensitive_codelist)))
  .snomedct_code.count_distinct_for_patient()
)

#date of first occurance of two of the above codes within 2 weeks
rsv_codes_date = (
  case(when(rsv_code_number > 1)
  .then(first_infection_event(codelists
  .rsv_sensitive_codelist).date))
)

#occurance of event in exclusion list within one month of rsv_codes_date
rsv_exclusion_primary = (case(
  when((first_infection_event(codelists.rsv_primary_exclusion_codelist)
  .date.is_on_or_between(rsv_codes_date - days(30), rsv_codes_date + days(30)))
  |(medications.where(medications.dmd_code
  .is_in(codelists.rsv_prescriptions_codelist)).date.minimum_for_patient()
  .is_on_or_between(rsv_codes_date - days(30), rsv_codes_date + days(30))))
  .then(has_infection_event(codelists.rsv_primary_exclusion_codelist))
))

# #rsv primary care
# if codelist_type == "specific" :
#   if cohort == "infants" or cohort == "infants_subgroup" :
#     dataset.rsv_primary = (
#       (has_infection_event(codelists.rsv_primary_codelist))
#       |(emergency_care_attendances.where(emergency_care_attendances
#       .diagnosis_01.is_in(codelists.bronchiolitis_attendance))
#       .exists_for_patient())
#       |(emergency_care_attendances.where(emergency_care_attendances
#       .diagnosis_02.is_in(codelists.bronchiolitis_attendance))
#       .exists_for_patient())
#     )
#   else :
#     dataset.rsv_primary = has_infection_event(codelists.rsv_primary_codelist)
# 
# if codelist_type == "sensitive" :
#   if cohort == "infants" or cohort == "infants_subgroup" :
#     dataset.rsv_primary = (
#       (has_infection_event(codelists.rsv_primary_codelist))
#       |(rsv_code_number >1)|(medications.where(medications.dmd_code
#       .is_in(codelists.rsv_prescriptions_codelist)).exists_for_patient())
#       |(emergency_care_diagnosis_matches(codelists.wheeze_attendance)
#       .exists_for_patient())
#       &(~rsv_exclusion_primary)
#     )
#   else :
#     dataset.rsv_primary = (
#       (has_infection_event(codelists.rsv_primary_codelist))
#       |(rsv_code_number >1)|(medications.where(medications.dmd_code
#       .is_in(codelists.rsv_prescriptions_codelist)).exists_for_patient())
#       &(~rsv_exclusion_primary)
#     )

#rsv primary care date
if codelist_type == "specific" :
  if cohort == "infants" or cohort == "infants_subgroup" :
    dataset.rsv_primary_date = (
      minimum_of(first_infection_event(codelists.rsv_primary_codelist).date,
      (emergency_care_attendances.where((emergency_care_attendances
      .diagnosis_01.is_in(codelists.bronchiolitis_attendance))
      |(emergency_care_attendances.diagnosis_02
      .is_in(codelists.bronchiolitis_attendance))))
      .arrival_date.minimum_for_patient())
    )
    # #get number of RSV mild cases
    # dataset.rsv_primary_cases = (
    #   ((is_infection_event(codelists.rsv_primary_codelist).date)
    #   |(emergency_care_attendances.where(emergency_care_attendances
    #   .diagnosis_01.is_in(codelists.bronchiolitis_attendance))
    #   .arrival_date)|(emergency_care_attendances
    #   .where(emergency_care_attendances.diagnosis_02
    #   .is_in(codelists.bronchiolitis_attendance)).arrival_date))
    #   .count_episodes_for_patient(days(14))
    # )
  else :
    dataset.rsv_primary_date = first_infection_event(codelists.rsv_primary_codelist).date
    # dataset.rsv_primary_cases = (
    #   is_infection_event(codelists.rsv_primary_codelist).date
    #   .count_episodes_for_patient(days(14))
    # )

if codelist_type == "sensitive" :
  if cohort == "infants" or cohort == "infants_subgroup" :
    dataset.rsv_primary_date = (case(
      when(~rsv_exclusion_primary).then(
      minimum_of((first_infection_event(codelists.rsv_primary_codelist).date),
      (rsv_codes_date),(medications.where(medications.dmd_code
      .is_in(codelists.rsv_prescriptions_codelist)).date.minimum_for_patient()),
      ((emergency_care_diagnosis_matches(codelists.wheeze_attendance)
      .arrival_date.minimum_for_patient())))))
    )
    # dataset.rsv_primary_cases = (
    #   (is_infection_event(codelists.rsv_primary_codelist).date)
    #   |(rsv_codes_date)|(medications.where(medications.dmd_code
    #   .is_in(codelists.rsv_prescriptions_codelist)).date)
    #   |(emergency_care_diagnosis_matches(codelists.wheeze_attendance)
    #   .arrival_date).count_episodes_for_patient(days(14))
    # )
  else :
    dataset.rsv_primary_date = (case(
      when(~rsv_exclusion_primary).then(
      minimum_of((first_infection_event(codelists.rsv_primary_codelist).date),
      (rsv_codes_date),(medications.where(medications.dmd_code
      .is_in(codelists.rsv_prescriptions_codelist))
      .date.minimum_for_patient()))))
    )
    # dataset.rsv_primary_cases = (case(
    #   when(~rsv_exclusion_primary).then(
    #   minimum_of((first_infection_event(codelists.rsv_primary_codelist).date),
    #   (rsv_codes_date),(medications.where(medications.dmd_code
    #   .is_in(codelists.rsv_prescriptions_codelist))
    #   .date.minimum_for_patient()))))
    #   .count_episodes_for_patient(days(14))
    # )

#occurance of event in exclusion list within one month of secondary care diagnosis
rsv_secondary_sens_date = (
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .unspecified_lrti).exists_for_patient()))
    .first_for_patient().admission_date
)
rsv_exclusion_secondary = (case(
  when((hospitalisation_diagnosis_matches(codelists
  .rsv_secondary_exclusion_codelist))
  .admission_date.minimum_for_patient()
  .is_on_or_between(rsv_secondary_sens_date - days(30),
  rsv_secondary_sens_date + days(30)))
  .then((hospitalisation_diagnosis_matches(codelists
  .rsv_secondary_exclusion_codelist))
  .exists_for_patient())
))

# #rsv secondary care
# if codelist_type == "specific" :
#   dataset.rsv_secondary = (
#     apcs.where(apcs.primary_diagnosis
#     .is_in(codelists.rsv_secondary_codelist))
#     .exists_for_patient()|apcs.where(apcs.secondary_diagnosis
#     .is_in(codelists.rsv_secondary_codelist)).exists_for_patient()
#   )
# if codelist_type == "sensitive" :
#   dataset.rsv_secondary = (
#     (hospitalisation_diagnosis_matches(codelists.rsv_secondary_codelist)
#     .exists_for_patient())
#     |(hospitalisation_diagnosis_matches(codelists.unspecified_lrti)
#     .exists_for_patient()) & (~rsv_exclusion_secondary)
#   )

#rsv secondary care date
if codelist_type == "specific" :
  dataset.rsv_secondary_date = (
    apcs.sort_by(apcs.admission_date)
    .where(apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist) 
    |apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist))
    .first_for_patient().admission_date
  )
  dataset.rsv_los = (
    (apcs.sort_by(apcs.admission_date)
    .where(apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist) 
    |apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist))
    .first_for_patient().discharge_date - 
    dataset.rsv_secondary_date).days*24
  )
if codelist_type == "sensitive" :
  dataset.rsv_secondary_date = (case(
    when(~rsv_exclusion_secondary)
    .then(rsv_secondary_sens_date))
  )
  dataset.rsv_los = (
    (apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .unspecified_lrti).exists_for_patient()))
    .first_for_patient().discharge_date -
    dataset.rsv_secondary_date).days*24
  )

##outcomes - flu

#occurance of ILI (according to WHO definition)
ILI_case = (
  (has_infection_event(codelists.ari_primary_codelist))
  & (has_infection_event(codelists.fever_codelist))
  & (has_infection_event(codelists.cough_codelist))
)

#date of first occurance of ILI
ILI_date = (case(
  when(ILI_case).then(minimum_of(first_infection_event(codelists
  .ari_primary_codelist).date, first_infection_event(codelists
  .fever_codelist).date, first_infection_event(codelists
  .cough_codelist).date)))
)

#occurance of event in exclusion list within one month of ILI
flu_exclusion_primary = (case(
  when((first_infection_event(codelists.flu_primary_exclusion_codelist)
  .date.is_on_or_between(first_infection_event(codelists.flu_sensitive_codelist)
  .date - days(30), first_infection_event(codelists.flu_sensitive_codelist)
  .date + days(30)))|(first_infection_event(codelists
  .flu_primary_exclusion_codelist).date
  .is_on_or_between(ILI_date - days(30), ILI_date + days(30)))
  |(medications.where(medications.dmd_code.is_in(codelists
  .flu_prescriptions_codelist)).date.minimum_for_patient()
  .is_on_or_between(ILI_date - days(30), ILI_date + days(30))))
  .then(has_infection_event(codelists.flu_primary_exclusion_codelist))
))

# #flu primary care
# if codelist_type == "specific" :
#   dataset.flu_primary = has_infection_event(codelists.flu_primary_codelist)
# 
# if codelist_type == "sensitive" :
#   dataset.flu_primary = (
#     (has_infection_event(codelists.flu_sensitive_codelist))
#     |(ILI_case)|(medications.where(medications.dmd_code
#     .is_in(codelists.flu_prescriptions_codelist))
#     .exists_for_patient()) & (~flu_exclusion_primary)
#   )

#flu primary care date
if codelist_type == "specific" :
  dataset.flu_primary_date = (
    first_infection_event(codelists
    .flu_primary_codelist).date
  )

if codelist_type == "sensitive" :
  dataset.flu_primary_date = (case(
    when(~flu_exclusion_primary).then(
    minimum_of((first_infection_event(codelists
    .flu_sensitive_codelist).date),
    (ILI_date),(medications.where(medications.dmd_code
    .is_in(codelists.flu_prescriptions_codelist))
    .date.minimum_for_patient()))))
  )
 
#occurance of event in exclusion list within one month of secondary care diagnosis
flu_secondary_sens_date = (
    apcs.sort_by(apcs.admission_date)
    .where(((hospitalisation_diagnosis_matches(codelists
    .flu_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .ari_secondary_codelist).exists_for_patient())))
    .admission_date.minimum_for_patient()
)
flu_exclusion_secondary = (case(
  when((hospitalisation_diagnosis_matches(codelists
  .flu_secondary_exclusion_codelist))
  .admission_date.minimum_for_patient()
  .is_on_or_between(flu_secondary_sens_date - days(30),
  flu_secondary_sens_date + days(30)))
  .then((hospitalisation_diagnosis_matches(codelists
  .flu_secondary_exclusion_codelist)).exists_for_patient())
))

# #flu secondary care
# if codelist_type == "specific" :
#   dataset.flu_secondary = (apcs.where(apcs.primary_diagnosis
#     .is_in(codelists.flu_secondary_codelist)).exists_for_patient()
#     |apcs.where(apcs.secondary_diagnosis
#     .is_in(codelists.flu_secondary_codelist))
#     .exists_for_patient()
#   )
# if codelist_type == "sensitive" :
#   dataset.flu_secondary = (
#     hospitalisation_diagnosis_matches(codelists
#     .flu_secondary_codelist).exists_for_patient()
#     |(hospitalisation_diagnosis_matches(codelists
#     .ari_secondary_codelist).exists_for_patient())
#     &(~flu_exclusion_secondary)
#   )

#flu secondary care date
if codelist_type == "specific" :
  dataset.flu_secondary_date = (
    apcs.sort_by(apcs.admission_date)
    .where(apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_codelist) 
    |apcs.secondary_diagnosis
    .is_in(codelists.flu_secondary_codelist))
    .first_for_patient().admission_date
  )
  dataset.flu_los = (
    (apcs.sort_by(apcs.admission_date)
    .where(apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_codelist) 
    |apcs.secondary_diagnosis
    .is_in(codelists.flu_secondary_codelist))
    .first_for_patient().discharge_date -
    dataset.flu_secondary_date).days*24
  )
if codelist_type == "sensitive" :
  dataset.flu_secondary_date = (case(
    when(~flu_exclusion_secondary)
    .then(flu_secondary_sens_date))
  )
  dataset.flu_los = (
    (apcs.sort_by(apcs.admission_date)
    .where(((hospitalisation_diagnosis_matches(codelists
    .flu_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .ari_secondary_codelist).exists_for_patient())))
    .discharge_date.minimum_for_patient() -
    dataset.flu_secondary_date).days*24
  )
  
##outcomes - covid

if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :

  #count number of clinical codes in covid symptom list
  covid_code_number = (
    (clinical_events.where(clinical_events
    .date.is_on_or_between(first_infection_event(codelists
    .covid_sensitive_codelist).date, first_infection_event(codelists
    .covid_sensitive_codelist).date + days(14)))
    .where(clinical_events.snomedct_code
    .is_in(codelists.covid_sensitive_codelist)))
    .snomedct_code.count_distinct_for_patient()
  )
  
  #date of first occurance of two of the above codes within 2 weeks
  covid_codes_date = (
    case(when(covid_code_number > 1)
    .then(first_infection_event(codelists
    .covid_sensitive_codelist).date))
  )
  
  #occurance of event in exclusion list within one month of covid_codes_date
  covid_exclusion_primary = (case(
    when((first_infection_event(codelists.covid_primary_exclusion_codelist)
    .date.is_on_or_between(covid_codes_date - days(30), covid_codes_date + days(30)))
    |(medications.where(medications.dmd_code
    .is_in(codelists.covid_prescriptions_codelist)).date.minimum_for_patient()
    .is_on_or_between(covid_codes_date - days(30), covid_codes_date + days(30))))
    .then(has_infection_event(codelists.covid_primary_exclusion_codelist))
  ))
  
  # #covid primary care
  # if codelist_type == "specific" :
  #   dataset.covid_primary = has_infection_event(codelists.covid_primary_codelist)
  # 
  # if codelist_type == "sensitive" :
  #   dataset.covid_primary = (
  #     (has_infection_event(codelists.covid_primary_codelist))
  #     |(covid_code_number >1)|(medications.where(medications.dmd_code
  #     .is_in(codelists.covid_prescriptions_codelist)).exists_for_patient())
  #     &(~covid_exclusion_primary)
  #   )
  
  #covid primary care date
  if codelist_type == "specific" :
    dataset.covid_primary_date = (
      first_infection_event(codelists
      .covid_primary_codelist).date
    )
  
  if codelist_type == "sensitive" :
    dataset.covid_primary_date = (case(
      when(~covid_exclusion_primary).then(
      minimum_of((first_infection_event(codelists
      .covid_primary_codelist).date),
      (covid_codes_date),(medications.where(medications.dmd_code
      .is_in(codelists.covid_prescriptions_codelist))
      .date.minimum_for_patient()))))
    )
  
  #occurance of event in exclusion list within one month of secondary care diagnosis
  covid_secondary_sens_date = (
      apcs.sort_by(apcs.admission_date).where(
      (hospitalisation_diagnosis_matches(codelists
      .covid_secondary_codelist).exists_for_patient())
      |(hospitalisation_diagnosis_matches(codelists
      .coronavirus_unspecified).exists_for_patient()))
      .first_for_patient().admission_date
  )
  covid_exclusion_secondary = (case(
    when((hospitalisation_diagnosis_matches(codelists
    .covid_secondary_exclusion_codelist))
    .admission_date.minimum_for_patient()
    .is_on_or_between(covid_secondary_sens_date - days(30),
    covid_secondary_sens_date + days(30)))
    .then((hospitalisation_diagnosis_matches(codelists
    .covid_secondary_exclusion_codelist))
    .exists_for_patient())
  ))
  
  # #covid secondary care
  # if codelist_type == "specific" :
  #   dataset.covid_secondary = (
  #     apcs.where(apcs.primary_diagnosis
  #     .is_in(codelists.covid_secondary_codelist))
  #     .exists_for_patient()
  #     |apcs.where(apcs.secondary_diagnosis
  #     .is_in(codelists.covid_secondary_codelist))
  #     .exists_for_patient()
  #   )
  # if codelist_type == "sensitive" :
  #   dataset.covid_secondary = (
  #     (hospitalisation_diagnosis_matches(codelists
  #     .covid_secondary_codelist)
  #     .exists_for_patient())
  #     |(hospitalisation_diagnosis_matches(codelists
  #     .coronavirus_unspecified).exists_for_patient())
  #     &(~covid_exclusion_secondary)
  #   )
    
  #covid secondary care date
  if codelist_type == "specific" :
    dataset.covid_secondary_date = (
      apcs.sort_by(apcs.admission_date)
      .where(apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist) 
      |apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      .first_for_patient().admission_date
    )
    dataset.covid_los = (
      (apcs.sort_by(apcs.admission_date)
      .where(apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist) 
      |apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      .first_for_patient().discharge_date -
      dataset.covid_secondary_date).days*24
    )
  if codelist_type == "sensitive" :
    dataset.covid_secondary_date = (case(
      when(~covid_exclusion_secondary)
      .then(covid_secondary_sens_date))
    )
    dataset.covid_los = (
      (apcs.sort_by(apcs.admission_date).where(
      (hospitalisation_diagnosis_matches(codelists
      .covid_secondary_codelist).exists_for_patient())
      |(hospitalisation_diagnosis_matches(codelists
      .coronavirus_unspecified).exists_for_patient()))
      .first_for_patient().discharge_date -
      dataset.covid_secondary_date).days*24
    )
  
##outcomes - unspecified respiratory infection

# #unspecified respiratory virus primary care
# if codelist_type == "sensitive" :
#   if cohort == "older_adults" :
#     if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
#       dataset.overall_resp_primary = (
#         (dataset.rsv_primary)|(dataset.flu_primary)|(dataset.covid_primary)
#         |(has_infection_event(codelists.
#         respiratory_virus_primary_codelist))
#         |(emergency_care_diagnosis_matches(codelists.
#         rtri_attendance).exists_for_patient())
#         |(emergency_care_diagnosis_matches(codelists.
#         copd_exacerbation_attendance).exists_for_patient())
#         |(has_infection_event(codelists
#         .copd_exacerbation_primary_codelist))
#         |(has_infection_event(codelists
#         .asthma_exacerbation_primary_codelist))
#       )
#     else:
#       dataset.overall_resp_primary = (
#         (dataset.rsv_primary)|(dataset.flu_primary)
#         |(has_infection_event(codelists.
#         respiratory_virus_primary_codelist))
#         |(emergency_care_diagnosis_matches(codelists.
#         rtri_attendance).exists_for_patient())
#         |(emergency_care_diagnosis_matches(codelists.
#         copd_exacerbation_attendance).exists_for_patient())
#         |(has_infection_event(codelists
#         .copd_exacerbation_primary_codelist))
#         |(has_infection_event(codelists
#         .asthma_exacerbation_primary_codelist))
#       )
#   else:
#     if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
#       dataset.overall_resp_primary = (
#         (dataset.rsv_primary)|(dataset.flu_primary)|(dataset.covid_primary)
#         |(has_infection_event(codelists.
#         respiratory_virus_primary_codelist))
#         |(emergency_care_diagnosis_matches(codelists.
#         rtri_attendance).exists_for_patient())
#       )
#     else:
#       dataset.overall_resp_primary = (
#         (dataset.rsv_primary)|(dataset.flu_primary)
#         |(has_infection_event(codelists.
#         respiratory_virus_primary_codelist))
#         |(emergency_care_diagnosis_matches(codelists.
#         rtri_attendance).exists_for_patient())
#       )

#unspecified respiratory virus primary care date
if codelist_type == "sensitive" :
  if cohort == "older_adults" :
    if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
      dataset.overall_resp_primary_date = (
        minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date,
        dataset.covid_primary_date, first_infection_event(codelists.
        respiratory_virus_primary_codelist).date,
        emergency_care_diagnosis_matches(codelists.
        rtri_attendance).arrival_date.minimum_for_patient(),
        emergency_care_diagnosis_matches(codelists.
        copd_exacerbation_attendance)
        .arrival_date.minimum_for_patient(),
        first_infection_event(codelists
        .copd_exacerbation_primary_codelist).date,
        first_infection_event(codelists
        .asthma_exacerbation_primary_codelist).date)
      ) 
    else:
      dataset.overall_resp_primary_date = (
        minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date,
        first_infection_event(codelists.
        respiratory_virus_primary_codelist).date,
        emergency_care_diagnosis_matches(codelists.
        rtri_attendance).arrival_date.minimum_for_patient(),
        emergency_care_diagnosis_matches(codelists.
        copd_exacerbation_attendance)
        .arrival_date.minimum_for_patient(),
        first_infection_event(codelists
        .copd_exacerbation_primary_codelist).date,
        first_infection_event(codelists
        .asthma_exacerbation_primary_codelist).date)
      )
  else:
    if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
      dataset.overall_resp_primary_date = (
        minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date,
        dataset.covid_primary_date, first_infection_event(codelists.
        respiratory_virus_primary_codelist).date,
        emergency_care_diagnosis_matches(codelists.
        rtri_attendance).arrival_date.minimum_for_patient())
      )
    else:
      dataset.overall_resp_primary_date = (
        minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date,
        first_infection_event(codelists.
        respiratory_virus_primary_codelist).date,
        emergency_care_diagnosis_matches(codelists.
        rtri_attendance).arrival_date.minimum_for_patient())
      )

# #unspecified respiratory virus secondary care
# if codelist_type == "sensitive" :
#   if cohort == "older_adults" :
#     if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
#       dataset.overall_resp_secondary = (
#         (dataset.rsv_secondary)|(dataset.flu_secondary)
#         |(dataset.covid_secondary)
#         |(hospitalisation_diagnosis_matches(codelists.
#         respiratory_virus_secondary_codelist)
#         .exists_for_patient())
#         |(hospitalisation_diagnosis_matches(codelists
#         .copd_exacerbation_secondary_codelist)
#         .exists_for_patient())
#         |(hospitalisation_diagnosis_matches(codelists
#         .asthma_exacerbation_secondary_codelist)
#         .exists_for_patient())
#       )
#     else:
#       dataset.overall_resp_secondary = (
#         (dataset.rsv_secondary)|(dataset.flu_secondary)
#         |(hospitalisation_diagnosis_matches(codelists.
#         respiratory_virus_secondary_codelist)
#         .exists_for_patient())
#         |(hospitalisation_diagnosis_matches(codelists
#         .copd_exacerbation_secondary_codelist)
#         .exists_for_patient())
#         |(hospitalisation_diagnosis_matches(codelists
#         .asthma_exacerbation_secondary_codelist)
#         .exists_for_patient())
#       )
#   else:
#     if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
#       dataset.overall_resp_secondary = (
#         (dataset.rsv_secondary)|(dataset.flu_secondary)
#         |(dataset.covid_secondary)
#         |(hospitalisation_diagnosis_matches(codelists.
#         respiratory_virus_secondary_codelist)
#         .exists_for_patient())
#       )
#     else:
#       dataset.overall_resp_secondary = (
#         (dataset.rsv_secondary)|(dataset.flu_secondary)
#         |(hospitalisation_diagnosis_matches(codelists.
#         respiratory_virus_secondary_codelist)
#         .exists_for_patient())
#       )
  
#unspecified respiratory virus secondary care date
if codelist_type == "sensitive" :
  if cohort == "older_adults" :
    if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
      dataset.overall_resp_secondary_date = (
        minimum_of(dataset.rsv_secondary_date,
        dataset.flu_secondary_date, dataset.covid_secondary_date,
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).admission_date
        .minimum_for_patient(), hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist)
        .admission_date.minimum_for_patient(),
        hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist)
        .admission_date.minimum_for_patient())
      ) 
      dataset.overall_resp_los = (
        (minimum_of(dataset.rsv_secondary_date,
        dataset.flu_secondary_date, dataset.covid_secondary_date,
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).admission_date
        .minimum_for_patient(), hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist)
        .admission_date.minimum_for_patient(),
        hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist)
        .discharge_date.minimum_for_patient()) -
        dataset.overall_resp_secondary_date).days*24
      ) 
    else:
      dataset.overall_resp_secondary_date = (
        minimum_of(dataset.rsv_secondary_date,
        dataset.flu_secondary_date, hospitalisation_diagnosis_matches(codelists
        .respiratory_virus_secondary_codelist).admission_date
        .minimum_for_patient(), hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist)
        .admission_date.minimum_for_patient(),
        hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist)
        .admission_date.minimum_for_patient())
      )
      dataset.overall_resp_los = (
        (minimum_of(dataset.rsv_secondary_date,
        dataset.flu_secondary_date, hospitalisation_diagnosis_matches(codelists
        .respiratory_virus_secondary_codelist).admission_date
        .minimum_for_patient(), hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist)
        .admission_date.minimum_for_patient(),
        hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist)
        .discharge_date.minimum_for_patient()) -
        dataset.overall_resp_secondary_date).days*24
      )
  else:
    if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
      dataset.overall_resp_secondary_date = (
        minimum_of(dataset.rsv_secondary_date,
        dataset.flu_secondary_date, dataset.covid_secondary_date,
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist)
        .admission_date.minimum_for_patient())
      )
      dataset.overall_resp_los = (
        (minimum_of(dataset.rsv_secondary_date,
        dataset.flu_secondary_date, dataset.covid_secondary_date,
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist)
        .discharge_date.minimum_for_patient()) -
        dataset.overall_resp_secondary_date).days*24
      )
    else:
      dataset.overall_resp_secondary_date = (
        minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date, 
        hospitalisation_diagnosis_matches(codelists
        .respiratory_virus_secondary_codelist).admission_date
        .minimum_for_patient())
      )
      dataset.overall_resp_los = (
        (minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date, 
        hospitalisation_diagnosis_matches(codelists
        .respiratory_virus_secondary_codelist).discharge_date
        .minimum_for_patient()) - dataset
        .overall_resp_secondary_date).days*24
      )

## outcomes - mortality 

#rsv mortality 
rsv_mortality = (
  cause_of_death_matches(codelists
  .rsv_secondary_codelist)
)

#rsv mortality date
dataset.rsv_mortality_date = (case(
  when(rsv_mortality).then(ons_deaths.date))
)

#flu mortality 
flu_mortality = (
  cause_of_death_matches(codelists
  .flu_secondary_codelist)
)

#rsv mortality date
dataset.flu_mortality_date = (case(
  when(flu_mortality).then(ons_deaths.date))
)

if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :

  #covid mortality 
  covid_mortality = (
    cause_of_death_matches(codelists
    .covid_secondary_codelist)
  )
  
  #covid mortality date
  dataset.covid_mortality_date = (case(
    when(covid_mortality).then(ons_deaths.date))
  )

#overall mortality
if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
  
  overall_resp_mortality = (
    (rsv_mortality)|(flu_mortality)
    |(covid_mortality)|(cause_of_death_matches(codelists
    .respiratory_virus_secondary_codelist))
  )

else:

  overall_resp_mortality = (
    (rsv_mortality)|(flu_mortality)
    |(cause_of_death_matches(codelists
    .respiratory_virus_secondary_codelist))
  )

#overall mortality date
dataset.overall_resp_mortality_date = (case(
  when(overall_resp_mortality).then(ons_deaths.date))
)

#all cause mortality
dataset.all_cause_mortality = ons_deaths.exists_for_patient()

#all cause mortality date
dataset.all_cause_mortality_date = ons_deaths.date

## comorbidities for secondary investigation

if investigation_type == "secondary" :

  from additional_comorbidities import (
    filter_codes_by_category, smoking_status,
    hazardous_drinking, drug_usage, has_asthma,
    has_reactive_airway, has_copd,
    has_cystic_fibrosis, has_other_resp,
    has_diabetes, has_addisons, severe_obesity,
    has_chd, has_ckd, has_cld, has_cnd, has_crd,
    has_cancer, immunosuppressed, has_sickle_cell,
  )
  
  if cohort == "adults" or cohort == "older_adults" :
    
    dataset.smoking_status = smoking_status
    dataset.hazardous_drinking = hazardous_drinking
    dataset.drug_usage = drug_usage
    dataset.has_asthma = has_asthma
    dataset.has_copd = has_copd
    dataset.has_cystic_fibrosis = has_cystic_fibrosis
    dataset.has_other_resp = has_other_resp
    dataset.has_diabetes = has_diabetes
    dataset.has_addisons = has_addisons
    dataset.severe_obesity = severe_obesity
    dataset.has_chd = has_chd
    dataset.has_ckd = has_ckd
    dataset.has_cld = has_cld
    dataset.has_cnd = has_cnd
    dataset.has_cancer = has_cancer
    dataset.immunosuppressed = immunosuppressed
    dataset.has_sickle_cell = has_sickle_cell

  if cohort == "children_and_adolescents" :

    dataset.has_asthma_reactive_airway = case(
      when(dataset.age <= 5).then(has_reactive_airway),
      when(dataset.age > 5).then(has_asthma),
      otherwise = False
    )
