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

#find difference between two dates in hours
def diff_dates_hours(date1, date2):
    return (date2-date1).days*24

#find absolute difference between two dates in days
def diff_dates_days(date1, date2):
    return (date2-date1).days

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

#care home resident - currently excluses anyone with a care home code or a care home flag
#if cohort == "older_adults" :
care_home_tpp = (
  addresses.for_patient_on(index_date)
  .care_home_is_potential_match.when_null_then(False)
)
care_home_code = (has_prior_event(codelists.carehome_codelist))
care_home = care_home_tpp | care_home_code

##define populations
#infants
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
#everyone else
else :
  dataset.define_population(
    registered_patients
    & is_female_or_male
    & is_appropriate_age
    & has_imd
    & (~care_home)
  )
# #adults or children
# if cohort == "adults" or cohort == "children_and_adolescents" :
#   dataset.define_population(
#     registered_patients
#     & is_female_or_male
#     & is_appropriate_age
#     & has_imd
#   )

#extract registration and sex  
dataset.registered = registered_patients
dataset.sex = patients.sex

#extract age (for infants in months)
if cohort == "infants" or cohort == "infants_subgroup" :
  dataset.age = age_at_start_months
else:
  dataset.age = age_at_start

#extract date of death
dataset.death_date = ons_deaths.date 

#extract latest ethnicity code for patient
dataset.latest_ethnicity_code = (
  clinical_events.where(clinical_events
  .snomedct_code.is_in(codelists.ethnicity_codes))
  .where(clinical_events.date.is_on_or_before(index_date))
  .sort_by(clinical_events.date)
  .last_for_patient()
  .snomedct_code
)

#extract patients IMD rank
dataset.imd_rounded = addresses.for_patient_on(index_date).imd_rounded

#extract rural/urban classification
dataset.rural_urban_classification = addresses.for_patient_on(index_date).rural_urban_classification

#extract patients household info
dataset.household_pseudo_id = household_memberships_2020.household_pseudo_id
dataset.household_size = household_memberships_2020.household_size

#extract patients practice's pseudonymised identifier
dataset.practice_pseudo_id = (
  (practice_registrations.for_patient_on(index_date))
  .practice_pseudo_id
)

#extract practice and patient information
dataset.region = (
  (practice_registrations.for_patient_on(index_date))
  .practice_nuts1_region_name
)
dataset.stp = (
  (practice_registrations.for_patient_on(index_date))
  .practice_stp
)

#extract date deregistered from practice
dataset.deregistration_date = (
  (practice_registrations
  .for_patient_on(index_date)).end_date
)

##define comorbidities

#define earliest prior vaccination date 
prior_vaccination_date = study_start_date - years(1)

#define seasons for covid
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d")
covid_current_vacc_min = datetime.strptime("2020-09-01", "%Y-%m-%d")
covid_prior_vacc_min = datetime.strptime("2021-09-01", "%Y-%m-%d") 

#extract vaccination information
if cohort == "adults" or cohort == "older_adults" or cohort == "children_and_adolescents" :
  #extract flu vaccination in previous season
  dataset.prior_flu_vaccination = (
    vaccinations.where(vaccinations.target_disease.is_in(["Influenza"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(prior_vaccination_date, index_date))
    .exists_for_patient()
  )
  #extract flu vaccination in current season
  dataset.flu_vaccination_date = (
    vaccinations.where(vaccinations.target_disease.is_in(["Influenza"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(index_date, study_end_date))
    .first_for_patient().date
  )
  #extract covid vaccination in previous season if applicable
  if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_prior_vacc_min :
    if cohort == "adults" or cohort == "older_adults" or cohort == "children_and_adolescents" :
      dataset.last_covid_vaccination_date = (
      vaccinations.where(vaccinations.target_disease.is_in(["SARS-COV-2"]))
      .sort_by(vaccinations.date)
      .where(vaccinations.date.is_on_or_between(prior_vaccination_date, index_date))
      .last_for_patient().date
    )
  #extract covid vaccination in current season if applicable
  if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_current_vacc_min :
    if cohort == "adults" or cohort == "older_adults" or cohort == "children_and_adolescents" :
      dataset.covid_vaccination_date = (
        vaccinations.where(vaccinations.target_disease.is_in(["SARS-COV-2"]))
        .sort_by(vaccinations.date)
        .where(vaccinations.date.is_on_or_between(index_date, study_end_date))
        .first_for_patient().date
      )

##define outcomes - rsv

#extract rsv primary care dates for primary analysis ('specific' phenotype)
if codelist_type == "specific" :
  if cohort == "infants" or cohort == "infants_subgroup" :
    #extract date of first episode - looking at the first date for which there is
    #a code in the RSV primary codelist or a code within diagnosis 1 or 2 
    #in the bronchiolitis attendance codelist
    dataset.rsv_primary_date = (
      minimum_of((first_infection_event(codelists.rsv_primary_codelist).date),
      ((emergency_care_attendances.where((emergency_care_attendances
      .diagnosis_01.is_in(codelists.bronchiolitis_attendance))
      |(emergency_care_attendances.diagnosis_02
      .is_in(codelists.bronchiolitis_attendance)))
      .where(emergency_care_attendances.arrival_date
      .is_on_or_between(index_date, study_end_date)))
      .arrival_date.minimum_for_patient()))
    )
    #extract date of second episode - using the same criteria, looking for the 
    #first occurrence 14 days after the first episode
    dataset.rsv_primary_date_second = (
      minimum_of((is_infection_event(codelists.rsv_primary_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date), ((emergency_care_attendances
      .where((emergency_care_attendances.diagnosis_01
      .is_in(codelists.bronchiolitis_attendance))
      |(emergency_care_attendances.diagnosis_02
      .is_in(codelists.bronchiolitis_attendance)))
      .where(emergency_care_attendances.arrival_date
      .is_on_or_between(dataset.rsv_primary_date + days(14), study_end_date)))
      .arrival_date.minimum_for_patient()))
    )
  else :
    #extract date of first episode - using only the RSV primary codelist
    dataset.rsv_primary_date = (
      first_infection_event(codelists
      .rsv_primary_codelist).date
    )
    #extract date of second episode - using the same criteria, looking for the
    #first occurrence 14 days after the first episode
    dataset.rsv_primary_date_second = (
      is_infection_event(codelists
      .rsv_primary_codelist).where(clinical_events
      .date.is_on_or_after(dataset
      .rsv_primary_date + days(14)))
      .sort_by(clinical_events.date)
      .first_for_patient().date
    )
#extract rsv primary care dates for sensitivity analysis ('sensitive' phenotype)
else :
  #count number of distinct codes in RSV sensitive codelist which occur within 2 weeks
  #of each other - looking at the first episode
  rsv_code_number = (
    clinical_events.where(clinical_events.date
    .is_on_or_between(first_infection_event(codelists
    .rsv_sensitive_codelist).date, first_infection_event(codelists
    .rsv_sensitive_codelist).date + days(14))).snomedct_code
    .is_in(codelists.rsv_sensitive_codelist)
    .count_distinct_for_patient()
  )
  #get the date of first occurrence a code above, if at least 2 codes are present 
  # - looking at the first episode
  rsv_codes_date = (
    case(when(rsv_code_number > 1)
    .then(first_infection_event(codelists
    .rsv_sensitive_codelist).date))
  )
  #get occurrence of event in exclusion list within one month of rsv_codes_date 
  # - looking at the first date for which there is a code in the RSV exclusion
  #codelist within one month before or after the date of rsv_codes_date, or a 
  #code in the RSV prescriptions codelist within one month before or after the
  #date of rsv_codes_date
  rsv_exclusion_primary = (case(
    when(first_infection_event(codelists.rsv_primary_exclusion_codelist)
    .date.is_on_or_between(rsv_codes_date - days(30), rsv_codes_date + days(30)))
    .then(True), when(medications.where(medications.dmd_code
    .is_in(codelists.rsv_prescriptions_codelist)).date.minimum_for_patient()
    .is_on_or_between(rsv_codes_date - days(30), rsv_codes_date + days(30)))
    .then(True), otherwise = False)
  )
  if cohort == "infants" or cohort == "infants_subgroup" :
    #extract date of first episode - looking at when the exclusion criteria is
    #not met, and taking the minimum of the date of the first code in the RSV
    #primary codelist, the date of the first code in the RSV sensitive codelist,
    #the date of the first prescription in the RSV prescriptions codelist
    #if there is at least one code in the RSV sensitive codelist, the date of 
    #the first bronchiolitis attendance, or the date of the first wheeze attendance
    dataset.rsv_primary_date = (case(
      when(~rsv_exclusion_primary).then(
      minimum_of((first_infection_event(codelists
      .rsv_primary_codelist).date), (rsv_codes_date),
      case(when(rsv_code_number >= 1).then(medications
      .where(medications.dmd_code.is_in(codelists
      .rsv_prescriptions_codelist)).where(medications
      .date.is_on_or_between(index_date, study_end_date))
      .date.minimum_for_patient())),
      (emergency_care_diagnosis_matches(codelists
      .bronchiolitis_attendance).where(emergency_care_attendances
      .arrival_date.is_on_or_between(index_date, study_end_date))
      .arrival_date.minimum_for_patient()), 
      ((emergency_care_diagnosis_matches(codelists.wheeze_attendance)
      .where(emergency_care_attendances.arrival_date
      .is_on_or_between(index_date, study_end_date))
      .arrival_date.minimum_for_patient())))))
    )
    #count number of distinct codes in RSV sensitive codelist which occur 
    #within 2 weeks - looking at the second episode
    rsv_code_number_second = (
      (clinical_events.where(clinical_events
      .date.is_on_or_between(is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient()
      .date, is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient()
      .date + days(14)))).snomedct_code
      .is_in(codelists.rsv_sensitive_codelist)
      .count_distinct_for_patient()
    )
    #get the date of first occurrence a code above, if at least 2 codes are present
    # - looking at the second episode
    rsv_codes_date_second = (
      case(when(rsv_code_number_second > 1)
      .then(is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient().date))
    )
    #get occurrence of event in exclusion list within one month of 
    #rsv_codes_date_second - looking at the same criteria as the first episode
    rsv_exclusion_primary_second = (case(
      when((is_infection_event(codelists.rsv_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events
      .date).first_for_patient().date.
      is_on_or_between(rsv_codes_date_second - days(30),
      rsv_codes_date_second + days(30)))
      |(medications.where(medications.dmd_code
      .is_in(codelists.rsv_prescriptions_codelist))
      .where(medications.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).date.minimum_for_patient()
      .is_on_or_between(rsv_codes_date_second - days(30), 
      rsv_codes_date_second + days(30))))
      .then(has_infection_event(codelists
      .rsv_primary_exclusion_codelist)))
    )
    #extract date of second episode - using the same criteria as the first episode
    dataset.rsv_primary_date_second = (case(
      when(~rsv_exclusion_primary).then(
      minimum_of((is_infection_event(codelists.rsv_primary_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date), (rsv_codes_date_second),
      (case(when(rsv_code_number_second >= 1).then((medications
      .where(medications.dmd_code.is_in(codelists
      .rsv_prescriptions_codelist)).where(medications.date
      .is_on_or_between(dataset.rsv_primary_date + days(14), study_end_date))
      .date.minimum_for_patient())))), (emergency_care_diagnosis_matches(
      codelists.bronchiolitis_attendance).where(emergency_care_attendances
      .arrival_date.is_on_or_between(dataset.rsv_primary_date + days(14), 
      study_end_date)).arrival_date.minimum_for_patient()),
      (emergency_care_diagnosis_matches(codelists.wheeze_attendance)
      .where(emergency_care_attendances.arrival_date
      .is_on_or_between(dataset.rsv_primary_date + days(14), study_end_date))
      .arrival_date.minimum_for_patient()))))
    )
  else :
    #extract date of first episode - looking at when the exclusion criteria is
    #not met, and taking the minimum of the date of the first code in the RSV
    #primary codelist, the date of the first code in the RSV sensitive codelist,
    #or the date of the first prescription in the RSV prescriptions codelist
    #when there is at least one code in the RSV sensitive codelist
    dataset.rsv_primary_date = (case(
      when(~rsv_exclusion_primary).then(
      minimum_of((first_infection_event(codelists
      .rsv_primary_codelist).date), (rsv_codes_date),
      case(when(rsv_code_number >= 1).then(medications
      .where(medications.dmd_code.is_in(codelists
      .rsv_prescriptions_codelist))
      .date.minimum_for_patient())))))
    )
    #count number of distinct codes in RSV sensitive codelist which occur
    #within 2 weeks - looking at the second episode
    rsv_code_number_second = (
      (clinical_events.where(clinical_events
      .date.is_on_or_between(is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient()
      .date, is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient()
      .date + days(14)))).snomedct_code
      .is_in(codelists.rsv_sensitive_codelist)
      .count_distinct_for_patient()
    )
    #get the date of first occurrence a code above, if at least 2 codes are present
    # - looking at the second episode
    rsv_codes_date_second = (
      case(when(rsv_code_number_second > 1)
      .then(is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient().date))
    )
    #get occurrence of event in exclusion list within one month of 
    #rsv_codes_date_second - looking at the same criteria as the first episode
    rsv_exclusion_primary_second = (case(
      when((is_infection_event(codelists.rsv_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events
      .date).first_for_patient().date
      .is_on_or_between(rsv_codes_date_second - days(30),
      rsv_codes_date_second + days(30)))
      |(medications.where(medications.dmd_code
      .is_in(codelists.rsv_prescriptions_codelist))
      .where(medications.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).date.minimum_for_patient()
      .is_on_or_between(rsv_codes_date_second - days(30), 
      rsv_codes_date_second + days(30))))
      .then(has_infection_event(codelists
      .rsv_primary_exclusion_codelist)))
    )
    #extract date of second episode - using the same criteria as the first episode
    dataset.rsv_primary_date_second = (case(
      when(~rsv_exclusion_primary).then(
      minimum_of((is_infection_event(codelists.rsv_primary_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date), (case(when(rsv_code_number > 1)
      .then(is_infection_event(codelists.rsv_sensitive_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date))), (medications.where(medications
      .dmd_code.is_in(codelists.rsv_prescriptions_codelist))
      .where(medications.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).date.minimum_for_patient()))))
    )

#extract rsv secondary care dates for primary analysis ('specific' phenotype)
if codelist_type == "specific" :
  #extract date of first episode - looking at the first date for which there is
  #a code in the RSV secondary codelist as the primary or secondary diagnosis
  dataset.rsv_secondary_date = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    index_date, study_end_date))
    .first_for_patient().admission_date
  )
  #get discharge date for first episode
  rsv_secondary_discharge = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)))
    .where(apcs.discharge_date
    .is_on_or_between(dataset.rsv_secondary_date,
    study_end_date)).first_for_patient()
    .discharge_date
  )
  #extract length of stay for first episode, in hours
  dataset.rsv_los = (
    diff_dates_hours(dataset.rsv_secondary_date,
    rsv_secondary_discharge)
  )
  #extract date of second episode - using the same criteria as the first episode
  dataset.rsv_secondary_date_second = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_after(dataset
    .rsv_secondary_date + days(14)))
    .first_for_patient().admission_date
  )
  #get discharge date for second episode
  rsv_secondary_discharge_second = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)))
    .where(apcs.discharge_date.is_on_or_between(
    dataset.rsv_secondary_date_second,
    study_end_date)).first_for_patient()
    .discharge_date
  )
  #extract length of stay for second episode, in hours
  dataset.rsv_los_second = (
    diff_dates_hours(dataset.rsv_secondary_date_second,
    rsv_secondary_discharge_second)
  )
#extract rsv secondary care dates for sensitivity analysis ('sensitive' phenotype)
else :
  #get date of first diagnosis code (in any position) from the RSV sensitive 
  #secondary care codelist - looking at the first episode
  rsv_secondary_sens_date = (
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .unspecified_lrti).exists_for_patient()))
    .where(apcs.admission_date.is_on_or_between(
    index_date, study_end_date))
    .first_for_patient().admission_date
  )
  #get occurrence of event in exclusion list within one month of an occurrence 
  #of rsv_secondary_sens_date 
  rsv_exclusion_secondary = (case(
    when((hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_exclusion_codelist))
    .admission_date.minimum_for_patient()
    .is_on_or_between(rsv_secondary_sens_date - days(30),
    rsv_secondary_sens_date + days(30)))
    .then(True), otherwise = False)
  )
  #extract date of first episode - looking at when the exclusion criteria is
  #not met and taking the value of rsv_secondary_sens_date
  dataset.rsv_secondary_date = (case(
    when(~rsv_exclusion_secondary)
    .then(rsv_secondary_sens_date))
  )
  #get discharge date for first episode
  rsv_secondary_discharge = (case(
    when(~rsv_exclusion_secondary).then(
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .unspecified_lrti).exists_for_patient()))
    .where(apcs.discharge_date.is_on_or_between(
    dataset.rsv_secondary_date, study_end_date))
    .first_for_patient().discharge_date))
  )
  #extract length of stay for first episode, in hours
  dataset.rsv_los = (
    diff_dates_hours(dataset.rsv_secondary_date,
    rsv_secondary_discharge)
  )  
  #extract date of second episode - using the same criteria as the first episode
  dataset.rsv_secondary_date_second = (case(
    when(~rsv_exclusion_secondary)
    .then(apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .unspecified_lrti).exists_for_patient()))
    .where(apcs.admission_date.is_on_or_after(
    dataset.rsv_secondary_date + days(14)))
    .first_for_patient().admission_date))
  )
  #get discharge date for second episode
  rsv_secondary_discharge_second = (case(
    when(~rsv_exclusion_secondary).then(
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .unspecified_lrti).exists_for_patient()))
    .where(apcs.discharge_date.is_on_or_between(dataset
    .rsv_secondary_date_second, study_end_date))
    .first_for_patient().discharge_date))  
  )
  #extract length of stay for second episode, in hours
  dataset.rsv_los_second = (
     diff_dates_hours(dataset.rsv_secondary_date_second,
     rsv_secondary_discharge_second)
  )
  
##extract outcomes - flu FINISH COMMENTING ON THE CODE

#extract flu primary care dates for primary analysis ('specific' phenotype)
if codelist_type == "specific" :
  #extract date of first episode - looking at the first date for which there is
  #a code in the flu primary codelist
  dataset.flu_primary_date = (
    first_infection_event(codelists
    .flu_primary_codelist).date
  )
  #extract date of second episode - using the same criteria as the first episode
  dataset.flu_primary_date_second = (
    is_infection_event(codelists
    .flu_primary_codelist).where(clinical_events
    .date.is_on_or_after(dataset.flu_primary_date + days(14)))
    .sort_by(clinical_events.date)
    .first_for_patient().date
  )
#extract flu primary care dates for sensitivity analysis ('sensitive' phenotype)
else :
  #get date of first case of either ARI or fever for first episode
  ari_date = first_infection_event(codelists.ari_primary_codelist).date
  fever_date = first_infection_event(codelists.fever_codelist).date
  #define an occurence of ILI (adapted from WHO definition) - look at the 
  #presence of a code in the ARI primary codelist and the fever codelist, 
  #at least one code from each must be present within 14 days of each other
  #to be considered an ILI case 
  ILI_case = (case(
    when((diff_dates_days(ari_date, fever_date) <= 14)
    & (diff_dates_days(ari_date, fever_date) >= -14))
    .then(True), otherwise = False)
  )
  #get date of first occurence of ILI
  ILI_date = (case(
    when(ILI_case).then(minimum_of(ari_date, fever_date)))
  )
  #occurrence of event in exclusion list within one month of ILI
  flu_exclusion_primary = (case(
    when(first_infection_event(codelists.flu_primary_exclusion_codelist)
    .date.is_on_or_between(first_infection_event(codelists
    .flu_sensitive_codelist).date - days(30), first_infection_event(
    codelists.flu_sensitive_codelist).date + days(30))).then(True), 
    when(first_infection_event(codelists.flu_primary_exclusion_codelist)
    .date.is_on_or_between(ILI_date - days(30), ILI_date + days(30)))
    .then(True), when(medications.where(medications.dmd_code
    .is_in(codelists.flu_prescriptions_codelist)).where(medications
    .date.is_on_or_between(index_date, study_end_date)).date
    .minimum_for_patient().is_on_or_between(ILI_date - days(30),
    ILI_date + days(30))).then(True), 
    otherwise = False)
  )
  #get date of first flu episode
  dataset.flu_primary_date = (case(
    when(~flu_exclusion_primary).then(
    minimum_of((first_infection_event(codelists
    .flu_sensitive_codelist).date), (ILI_date),
    (medications.where(medications.dmd_code
    .is_in(codelists.flu_prescriptions_codelist))
    .where(medications.date.is_on_or_between(
    index_date, study_end_date)).date
    .minimum_for_patient()))))
  )
  #get date of first case of either ARI or fever for second episode
  ari_date_second = (
    is_infection_event(codelists.ari_primary_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .flu_primary_date + days(14))).sort_by(clinical_events.date)
    .first_for_patient().date
  )
  fever_date_second = (
    is_infection_event(codelists.fever_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .flu_primary_date + days(14))).sort_by(clinical_events.date)
    .first_for_patient().date
  )
  #define second occurrence of ILI using same criteria as first episode
  ILI_case_second = (case(
    when((diff_dates_days(ari_date_second, fever_date_second) <= 14)
    & (diff_dates_days(ari_date_second, fever_date_second) >= -14))
    .then(True), otherwise = False)
  )
  #get date of second occurence of ILI
  ILI_date_second = (case(
    when(ILI_case_second).then(minimum_of(ari_date_second,
    fever_date_second)))
  )
  #occurrence of event in exclusion list within one month of second ILI 
  flu_exclusion_primary_second = (case(
    when(is_infection_event(codelists.flu_primary_exclusion_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .flu_primary_date + days(14))).sort_by(clinical_events
    .date).first_for_patient().date.is_on_or_between(
    is_infection_event(codelists.flu_sensitive_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .flu_primary_date + days(14))).sort_by(clinical_events
    .date).first_for_patient().date - days(30), 
    is_infection_event(codelists.flu_sensitive_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .flu_primary_date + days(14))).sort_by(clinical_events
    .date).first_for_patient().date + days(30))).then(True), 
    when(is_infection_event(codelists.flu_primary_exclusion_codelist)
    .where(clinical_events.date.is_on_or_between(
    dataset.flu_primary_date + days(14), study_end_date))
    .sort_by(clinical_events.date).first_for_patient().date
    .is_on_or_between(ILI_date_second - days(30), 
    ILI_date_second + days(30))).then(True), when(medications
    .where(medications.dmd_code.is_in(codelists
    .flu_prescriptions_codelist)).where(medications
    .date.is_on_or_between(ILI_date_second - days(30), 
    ILI_date_second + days(30))).sort_by(medications.date)
    .first_for_patient().date.is_on_or_between(dataset
    .flu_primary_date + days(14), study_end_date))
    .then(True), 
    otherwise = False)
  )
  #get date of second flu episode
  dataset.flu_primary_date_second = (case(
    when(~flu_exclusion_primary_second).then(
    minimum_of((is_infection_event(codelists
   .flu_sensitive_codelist).where(clinical_events
    .date.is_on_or_between(dataset
    .flu_primary_date + days(14), study_end_date))
    .sort_by(clinical_events.date).first_for_patient()
    .date), (ILI_date_second), (medications.where(
    medications.dmd_code.is_in(codelists.
    flu_prescriptions_codelist)).where(medications.date
    .is_on_or_between(dataset.flu_primary_date + days(14),
    study_end_date)).date.minimum_for_patient()))))
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
  
  #date of first occurrence of two of the above codes within 2 weeks
  covid_codes_date = (
    case(when(covid_code_number > 1)
    .then(first_infection_event(codelists
    .covid_sensitive_codelist).date))
  )
  
  #occurrence of event in exclusion list within one month of covid_codes_date
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
  #     |#(covid_code_number >1)|(medications.where(medications.dmd_code
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
  
  #occurrence of event in exclusion list within one month of secondary care diagnosis
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
  #     |#apcs.where(apcs.secondary_diagnosis
  #     .is_in(codelists.covid_secondary_codelist))
  #     .exists_for_patient()
  #   )
  # if codelist_type == "sensitive" :
  #   dataset.covid_secondary = (
  #     (hospitalisation_diagnosis_matches(codelists
  #     .covid_secondary_codelist)
  #     .exists_for_patient())
  #     |#(hospitalisation_diagnosis_matches(codelists
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
#         |#(has_infection_event(codelists.
#         respiratory_virus_primary_codelist))
#         |#(emergency_care_diagnosis_matches(codelists.
#         rtri_attendance).exists_for_patient())
#         |#(emergency_care_diagnosis_matches(codelists.
#         copd_exacerbation_attendance).exists_for_patient())
#         |#(has_infection_event(codelists
#         .copd_exacerbation_primary_codelist))
#         |#(has_infection_event(codelists
#         .asthma_exacerbation_primary_codelist))
#       )
#     else:
#       dataset.overall_resp_primary = (
#         (dataset.rsv_primary)|(dataset.flu_primary)
#         |#(has_infection_event(codelists.
#         respiratory_virus_primary_codelist))
#         |#(emergency_care_diagnosis_matches(codelists.
#         rtri_attendance).exists_for_patient())
#         |#(emergency_care_diagnosis_matches(codelists.
#         copd_exacerbation_attendance).exists_for_patient())
#         |#(has_infection_event(codelists
#         .copd_exacerbation_primary_codelist))
#         |#(has_infection_event(codelists
#         .asthma_exacerbation_primary_codelist))
#       )
#   else:
#     if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
#       dataset.overall_resp_primary = (
#         (dataset.rsv_primary)|(dataset.flu_primary)|(dataset.covid_primary)
#         |#(has_infection_event(codelists.
#         respiratory_virus_primary_codelist))
#         |#(emergency_care_diagnosis_matches(codelists.
#         rtri_attendance).exists_for_patient())
#       )
#     else:
#       dataset.overall_resp_primary = (
#         (dataset.rsv_primary)|(dataset.flu_primary)
#         |#(has_infection_event(codelists.
#         respiratory_virus_primary_codelist))
#         |#(emergency_care_diagnosis_matches(codelists.
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
#         |#(dataset.covid_secondary)
#         |#(hospitalisation_diagnosis_matches(codelists.
#         respiratory_virus_secondary_codelist)
#         .exists_for_patient())
#         |#(hospitalisation_diagnosis_matches(codelists
#         .copd_exacerbation_secondary_codelist)
#         .exists_for_patient())
#         |#(hospitalisation_diagnosis_matches(codelists
#         .asthma_exacerbation_secondary_codelist)
#         .exists_for_patient())
#       )
#     else:
#       dataset.overall_resp_secondary = (
#         (dataset.rsv_secondary)|(dataset.flu_secondary)
#         |#(hospitalisation_diagnosis_matches(codelists.
#         respiratory_virus_secondary_codelist)
#         .exists_for_patient())
#         |#(hospitalisation_diagnosis_matches(codelists
#         .copd_exacerbation_secondary_codelist)
#         .exists_for_patient())
#         |#(hospitalisation_diagnosis_matches(codelists
#         .asthma_exacerbation_secondary_codelist)
#         .exists_for_patient())
#       )
#   else:
#     if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
#       dataset.overall_resp_secondary = (
#         (dataset.rsv_secondary)|(dataset.flu_secondary)
#         |#(dataset.covid_secondary)
#         |#(hospitalisation_diagnosis_matches(codelists.
#         respiratory_virus_secondary_codelist)
#         .exists_for_patient())
#       )
#     else:
#       dataset.overall_resp_secondary = (
#         (dataset.rsv_secondary)|(dataset.flu_secondary)
#         |#(hospitalisation_diagnosis_matches(codelists.
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
