import json, sys
from pathlib import Path 

from datetime import date, datetime
from ehrql import create_dataset, case, when, maximum_of, minimum_of, years, days
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

dataset = create_dataset()
dataset.configure_dummy_data(population_size = 100000)

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
  dataset.severe_immunodeficiency = (
    has_prior_event(codelists
    .severe_immunodeficiency_code)
  )

#infant risk group (cardiac disease, pulmonary hypertension)
if cohort == "infants" or cohort == "infants_subgroup" :
  dataset.risk_group_infants = (case(
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
if cohort == "older_adults" :
  care_home_tpp = (
    addresses.for_patient_on(index_date)
    .care_home_is_potential_match.when_null_then(False)
  )
  care_home_code = (has_prior_event(codelists.carehome_codelist))
  dataset.care_home = care_home_tpp | care_home_code

#define population
dataset.define_population(practice_registrations.exists_for_patient())

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

dataset.is_female_or_male = is_female_or_male
dataset.is_appropriate_age = is_appropriate_age
dataset.has_imd = has_imd
