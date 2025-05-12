import json, sys
from pathlib import Path

from datetime import date, datetime
from ehrql import Dataset, create_dataset, case, when, maximum_of, minimum_of, years, days
from ehrql.tables.tpp import (
  patients,
  medications,
  #ons_deaths,
  addresses,
  clinical_events,
  practice_registrations,
  household_memberships_2020,
  vaccinations,
  apcs,
  emergency_care_attendances,
  parents,
  ethnicity_from_sus
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
dataset.configure_dummy_data(population_size = 10000)

# dataset = Dataset()

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

#get date patient ages into cohort 
if cohort == "infants" or cohort == "infants_subgroup" :
  age_date = patients.date_of_birth
  age_out_date = patients.date_of_birth + years(2)
elif cohort == "children_and_adolescents" :
  age_date = patients.date_of_birth + years(2) 
  age_out_date = patients.date_of_birth + years(18)
elif cohort == "adults" :
  age_date = patients.date_of_birth + years(18)
  age_out_date = patients.date_of_birth + years(65)
else :
  age_date = patients.date_of_birth + years(65)
  age_out_date = patients.date_of_birth + years(110)

#set index date (and registration date) as last date of either start date or age date
#so that patients are the correct age for the cohort when looking at records
if cohort == "infants" or cohort == "infants_subgroup" :
  index_date = maximum_of(study_start_date, study_start_date)
else : 
  index_date = maximum_of(study_start_date, age_date)

registration_date = index_date - years(1)

#set end date as first date of either end date or age out date 
#so that patients are the correct age for the cohort when looking at records
followup_end_date = minimum_of(study_end_date, age_out_date)

#extract patient specific follow up dates
dataset.patient_index_date = index_date
dataset.patient_end_date = followup_end_date

#define patients status: alive/dead
was_alive = (
  (patients.date_of_death.is_after(index_date))
  |(patients.date_of_death.is_null())
)

#define patients age
age_at_start = patients.age_on(study_start_date)
age_at_end = patients.age_on(study_end_date)
age_months = (index_date - patients.date_of_birth).months
age_at_start_months = (study_start_date - patients.date_of_birth).months
age_at_end_months = (study_end_date - patients.date_of_birth).months

#get patients who meet registration criteria
#(1 year continuous registration, for non-infants)
if cohort == "infants" or cohort == "infants_subgroup" :
  registered_patients = practice_registrations.for_patient_on(index_date).exists_for_patient()
else :
  registered_patients = practice_registrations.for_patient_on(registration_date).exists_for_patient()

#have sex
is_female_or_male = patients.sex.is_in(["female", "male"])

#have age
if cohort == "infants" or cohort == "infants_subgroup" :
  is_appropriate_age = (age_at_start_months < 24) & (age_at_end_months >= 0)
elif cohort == "children_and_adolescents" :
  is_appropriate_age = (age_at_start < 18) & (age_at_end >= 2)
elif cohort == "adults" :
  is_appropriate_age = (age_at_start < 65) & (age_at_end >= 18)
else :
  is_appropriate_age = (age_at_start < 110) & (age_at_end >= 65)

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
infection_events = (
  clinical_events.where(clinical_events.date
  .is_on_or_between(index_date, followup_end_date))
)

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

if cohort == "infants_subgroup" :
  mother_id_present = parents.mother_id.is_not_null()

##define populations

#infants
if cohort == "infants" :
  dataset.define_population(
    was_alive
    & registered_patients
    & is_female_or_male
    & is_appropriate_age
    & has_imd
    & (~severe_immunodeficiency)
    & (~risk_group_infants)
    & (~care_home)
  )
#infants linked to mothers
elif cohort == "infants_subgroup" :
  dataset.define_population(
    was_alive
    & registered_patients
    & is_female_or_male
    & is_appropriate_age
    & has_imd
    & mother_id_present
    & (~severe_immunodeficiency)
    & (~risk_group_infants)
    & (~care_home)
  )
#everyone else
else :
  dataset.define_population(
    was_alive
    & registered_patients
    & is_female_or_male
    & is_appropriate_age
    & has_imd
    & (~care_home)
  )

#extract registration and sex  
dataset.registered = registered_patients
dataset.sex = patients.sex

#extract age (for infants in months)
if cohort == "infants" or cohort == "infants_subgroup" :
  dataset.age = age_at_start_months
else:
  dataset.age = patients.age_on(index_date) #gets the patients age on their specific index date

#extract date of death
dataset.death_date = patients.date_of_death

#extract latest ethnicity code for patient
if cohort == "infants" or cohort == "infants_subgroup" :
  dataset.latest_ethnicity_group = (
  clinical_events.where(clinical_events
  .snomedct_code.is_in(codelists.ethnicity_codes))
  .sort_by(clinical_events.date)
  .last_for_patient().snomedct_code
  .to_category(codelists.ethnicity_codes)
  )
  #extract HES ethnicity
  dataset.latest_ethnicity_group_hes = ethnicity_from_sus.code
else:
  dataset.latest_ethnicity_group = (
  clinical_events.where(clinical_events
  .snomedct_code.is_in(codelists.ethnicity_codes))
  .where(clinical_events.date.is_on_or_before(index_date))
  .sort_by(clinical_events.date)
  .last_for_patient().snomedct_code
  .to_category(codelists.ethnicity_codes)
  )

#extract patients IMD rank
dataset.imd_rounded = addresses.for_patient_on(index_date).imd_rounded

#extract rural/urban classification
dataset.rural_urban_classification = (
  addresses.for_patient_on(index_date).rural_urban_classification
)

#extract patients household info
if study_start_date == datetime.strptime("2020-09-01", "%Y-%m-%d").date() :
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

#extract mothers ID
if cohort == "infants_subgroup" :
  dataset.mother_id = parents.mother_id
  dataset.birth_date = patients.date_of_birth

##define comorbidities

#define earliest date to look for prior vaccination
prior_vaccination_date = study_start_date - years(1)

#define seasons for covid
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d").date()
covid_current_vacc_min = datetime.strptime("2020-09-01", "%Y-%m-%d").date()
covid_prior_vacc_min = datetime.strptime("2021-09-01", "%Y-%m-%d").date()

#extract vaccination information
if cohort == "adults" or cohort == "older_adults" or cohort == "children_and_adolescents" :
  
  #extract flu vaccination in previous season
  dataset.prior_flu_vaccination = (
    vaccinations.where(vaccinations.target_disease.is_in(["INFLUENZA"]))
    .sort_by(vaccinations.date).where(vaccinations
    .date.is_on_or_between(prior_vaccination_date, index_date))
    .exists_for_patient()
  )
  
  #extract flu vaccination in current season
  dataset.flu_vaccination_date = (
    vaccinations.where(vaccinations.target_disease.is_in(["INFLUENZA"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(index_date, followup_end_date))
    .first_for_patient().date
  )
  
  #extract covid vaccination in previous season if applicable
  if study_start_date >= covid_prior_vacc_min :
    
    dataset.last_covid_vaccination_date = (
      vaccinations.where(vaccinations.target_disease
      .is_in(["SARS-2 Coronavirus"])).sort_by(vaccinations.date)
      .where(vaccinations.date.is_on_or_before(index_date))
      .last_for_patient().date
    )
  
  #extract covid vaccination in current season if applicable
  if study_start_date >= covid_current_vacc_min :
    
    dataset.covid_vaccination_date = (
      vaccinations.where(vaccinations.target_disease
      .is_in(["SARS-2 Coronavirus"])).sort_by(vaccinations.date)
      .where(vaccinations.date.is_on_or_between(index_date,
      followup_end_date)).first_for_patient().date
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
      .is_on_or_between(index_date, followup_end_date)))
      .arrival_date.minimum_for_patient()))
    )
    
    #extract date of second episode - using the same criteria, looking for the 
    #first occurrence 14 days after the first episode
    dataset.rsv_primary_second_date = (
      minimum_of((is_infection_event(codelists.rsv_primary_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date), ((emergency_care_attendances
      .where((emergency_care_attendances.diagnosis_01
      .is_in(codelists.bronchiolitis_attendance))
      |(emergency_care_attendances.diagnosis_02
      .is_in(codelists.bronchiolitis_attendance)))
      .where(emergency_care_attendances.arrival_date
      .is_on_or_between(dataset.rsv_primary_date + days(14), followup_end_date)))
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
    dataset.rsv_primary_second_date = (
      is_infection_event(codelists
      .rsv_primary_codelist).where(clinical_events
      .date.is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date)
      .first_for_patient().date
    )
    
#extract rsv primary care dates for sensitivity analysis ('sensitive' phenotype)
else :
  
  #get dates of events and corresponding codes 
  dataset.rsv_code1 = (
    first_infection_event(codelists.rsv_sensitive_codelist)
    .snomedct_code
  )
  dataset.rsv_code1_date = (
    first_infection_event(codelists.rsv_sensitive_codelist)
    .date
  )
  rsv_code_number_date1 = (
    (clinical_events.where(clinical_events
    .date.is_on_or_between(dataset.rsv_code1_date, dataset.rsv_code1_date))
    .where(clinical_events.snomedct_code
    .is_in(codelists.rsv_sensitive_codelist)))
    .snomedct_code.count_distinct_for_patient()
  )

  dataset.rsv_code2 = (case(
    when(rsv_code_number_date1 > 1).then(None),
    otherwise = is_infection_event(codelists.rsv_sensitive_codelist)
    .where(clinical_events.date.is_after(dataset.rsv_code1_date))
    .sort_by(clinical_events.date).first_for_patient()
    .snomedct_code)
  )
  dataset.rsv_code2_date = (case(
    when(rsv_code_number_date1 > 1).then(None),
    otherwise = is_infection_event(codelists.rsv_sensitive_codelist)
    .where(clinical_events.date.is_after(dataset.rsv_code1_date))
    .sort_by(clinical_events.date).first_for_patient().date)
  )
  rsv_code_number_date2 = (
    (clinical_events.where(clinical_events
    .date.is_on_or_between(dataset.rsv_code2_date, dataset.rsv_code2_date))
    .where(clinical_events.snomedct_code
    .is_in(codelists.rsv_sensitive_codelist)))
    .snomedct_code.count_distinct_for_patient()
  )

  dataset.rsv_code3 = (case(
    when(rsv_code_number_date2 > 1).then(None),
    otherwise = is_infection_event(codelists.rsv_sensitive_codelist)
    .where(clinical_events.date.is_after(dataset.rsv_code2_date))
    .sort_by(clinical_events.date).first_for_patient()
    .snomedct_code)
  )
  dataset.rsv_code3_date = (case(
    when(rsv_code_number_date2 > 1).then(None),
    otherwise = is_infection_event(codelists.rsv_sensitive_codelist)
    .where(clinical_events.date.is_after(dataset.rsv_code2_date))
    .sort_by(clinical_events.date).first_for_patient().date)
  )
  rsv_code_number_date3 = (
    (clinical_events.where(clinical_events
    .date.is_on_or_between(dataset.rsv_code3_date, dataset.rsv_code3_date))
    .where(clinical_events.snomedct_code
    .is_in(codelists.rsv_sensitive_codelist)))
    .snomedct_code.count_distinct_for_patient()
  )

  dataset.rsv_code4 = (case(
    when(rsv_code_number_date3 > 1).then(None),
    otherwise = is_infection_event(codelists.rsv_sensitive_codelist)
    .where(clinical_events.date.is_after(dataset.rsv_code3_date))
    .sort_by(clinical_events.date).first_for_patient()
    .snomedct_code)
  )
  dataset.rsv_code4_date = (case(
    when(rsv_code_number_date3 > 1).then(None),
    otherwise = is_infection_event(codelists.rsv_sensitive_codelist)
    .where(clinical_events.date.is_after(dataset.rsv_code3_date))
    .sort_by(clinical_events.date).first_for_patient().date)
  )
  rsv_code_number_date4 = (
    (clinical_events.where(clinical_events
    .date.is_on_or_between(dataset.rsv_code3_date, dataset.rsv_code4_date))
    .where(clinical_events.snomedct_code
    .is_in(codelists.rsv_sensitive_codelist)))
    .snomedct_code.count_distinct_for_patient()
  )

  rsv_code_number = (case(
    when(rsv_code_number_date1 > 1)
    .then(rsv_code_number_date1),
    when(rsv_code_number_date2 > 1)
    .then(rsv_code_number_date2),
    when(rsv_code_number_date3 > 1)
    .then(rsv_code_number_date3),
    when(rsv_code_number_date4 > 1)
    .then(rsv_code_number_date4),
    when(diff_dates_days(dataset.rsv_code1_date,
    dataset.rsv_code2_date) <= 14).then(2),
    when(diff_dates_days(dataset.rsv_code2_date,
    dataset.rsv_code3_date) <= 14).then(2),
    when(diff_dates_days(dataset.rsv_code3_date,
    dataset.rsv_code4_date) <= 14).then(2),
    otherwise = rsv_code_number_date1)
  )

  dataset.rsv_code_number = rsv_code_number

  rsv_codes_date = (case(
    when(rsv_code_number_date1 > 1)
    .then(dataset.rsv_code1_date),
    when(rsv_code_number_date2 > 1)
    .then(dataset.rsv_code2_date),
    when(rsv_code_number_date3 > 1)
    .then(dataset.rsv_code3_date),
    when(rsv_code_number_date4 > 1)
    .then(dataset.rsv_code4_date),
    when((rsv_code_number > 1) &
    (diff_dates_days(dataset.rsv_code1_date,
    dataset.rsv_code2_date) <= 14))
    .then(dataset.rsv_code1_date),
    when((rsv_code_number > 1) &
    (diff_dates_days(dataset.rsv_code2_date,
    dataset.rsv_code3_date) <= 14))
    .then(dataset.rsv_code2_date),
    when((rsv_code_number > 1) &
    (diff_dates_days(dataset.rsv_code3_date,
    dataset.rsv_code4_date) <= 14))
    .then(dataset.rsv_code3_date),
    otherwise = None)
  )

  dataset.rsv_codes_date = rsv_codes_date

  #count number of distinct codes in RSV sensitive codelist which occur within 2 weeks
  #of each other - looking at the first episode
  # rsv_code_number = (
  #   (clinical_events.where(clinical_events
  #   .date.is_on_or_between(first_infection_event(codelists
  #   .rsv_sensitive_codelist).date, first_infection_event(codelists
  #   .rsv_sensitive_codelist).date + days(14)))
  #   .where(clinical_events.snomedct_code
  #   .is_in(codelists.rsv_sensitive_codelist)))
  #   .snomedct_code.count_distinct_for_patient()
  # )
 
  #get the date of first occurrence a code above, if at least 2 codes are present 
  # - looking at the first episode
  # rsv_codes_date = (
  #   case(when(rsv_code_number > 1)
  #   .then(first_infection_event(codelists
  #   .rsv_sensitive_codelist).date))
  # )
  
  #get the date of the occurrence of first relevant prescription
  rsv_med_date = (
    medications.where(medications.dmd_code.is_in(codelists
    .rsv_prescriptions_codelist)).where(medications.date
    .is_on_or_between(index_date, followup_end_date))
    .date.minimum_for_patient()
  )
  
  #get occurrence of event in exclusion list within one month of rsv_codes_date 
  # - looking at the first date for which there is a code in the RSV exclusion
  #codelist within one month before or after the date of rsv_codes_date, or a 
  #code in the RSV prescriptions codelist within one month before or after the
  #date of rsv_codes_date
  rsv_exclusion_primary = (case(
    when(first_infection_event(codelists.rsv_primary_exclusion_codelist)
    .date.is_on_or_between(rsv_codes_date - days(30),
    rsv_codes_date + days(30))).then(True),
    when(first_infection_event(codelists.rsv_primary_exclusion_codelist)
    .date.is_on_or_between(rsv_med_date - days(30),
    rsv_med_date + days(30))).then(True), otherwise = False)
  )
  
  #define prescription inclusion - i.e. presence of a code from codelist with
  #relevant prescription
  rsv_med_inclusion_date = (case(
    when(first_infection_event(codelists.rsv_sensitive_codelist)
    .date.is_on_or_between(rsv_med_date - days(14), rsv_med_date + days(14)))
    .then(rsv_med_date))
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
      case(when(rsv_code_number >= 1).then(rsv_med_inclusion_date)),
      (emergency_care_diagnosis_matches(codelists
      .bronchiolitis_attendance).where(emergency_care_attendances
      .arrival_date.is_on_or_between(index_date, followup_end_date))
      .arrival_date.minimum_for_patient()), 
      ((emergency_care_diagnosis_matches(codelists.wheeze_attendance)
      .where(emergency_care_attendances.arrival_date
      .is_on_or_between(index_date, followup_end_date))
      .arrival_date.minimum_for_patient())))))
    )
    
    #count number of distinct codes in RSV sensitive codelist which occur 
    #within 2 weeks - looking at the second episode
    rsv_code_number_second = (
      (clinical_events.where(clinical_events
      .date.is_on_or_between(is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient().date, 
      is_infection_event(codelists.rsv_sensitive_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date + days(14)))).snomedct_code
      .is_in(codelists.rsv_sensitive_codelist)
      .count_distinct_for_patient()
    )
    
    #get the date of first occurrence a code above, if at least 2 codes are present
    # - looking at the second episode
    rsv_codes_second_date = (
      case(when(rsv_code_number_second > 1)
      .then(is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient().date))
    )
    
    #get the date of the occurrence of first relevant prescription
    # - looking at the second episode
    rsv_med_second_date = (
      medications.where(medications.dmd_code.is_in(codelists
      .rsv_prescriptions_codelist)).where(medications.date
      .is_on_or_between(dataset.rsv_primary_date + days(14),
      followup_end_date)).date.minimum_for_patient()
    )
    
    #get occurrence of event in exclusion list within one month of 
    #rsv_codes_second_date - looking at the same criteria as the first episode
    rsv_exclusion_primary_second = (case(
      when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_between(dataset
      .rsv_primary_date + days(14), followup_end_date))
      .sort_by(clinical_events.date).first_for_patient().date
      .is_on_or_between(rsv_codes_second_date - days(30),
      rsv_codes_second_date + days(30))).then(True),
      when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_between(dataset
      .rsv_primary_date + days(14), followup_end_date))
      .sort_by(clinical_events.date).first_for_patient().date
      .is_on_or_between(rsv_med_second_date - days(30),
      rsv_med_second_date + days(30))).then(True), otherwise = False)
    )
    
    #define prescription inclusion - i.e. presence of a code from codelist with
    #relevant prescription
    rsv_med_inclusion_second_date = (case(
      when(is_infection_event(codelists.rsv_sensitive_codelist)
      .where(clinical_events.date.is_on_or_between(dataset
      .rsv_primary_date + days(14), followup_end_date))
      .sort_by(clinical_events.date).first_for_patient()
      .date.is_on_or_between(rsv_med_second_date - days(14),
      rsv_med_second_date + days(14))).then(rsv_med_second_date))
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.rsv_primary_second_date = (case(
      when(~rsv_exclusion_primary).then(
      minimum_of((is_infection_event(codelists.rsv_primary_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date), (rsv_codes_second_date),
      (case(when(rsv_code_number_second >= 1)
      .then(rsv_med_inclusion_second_date))),
      (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance)
      .where(emergency_care_attendances.arrival_date
      .is_on_or_between(dataset.rsv_primary_date + days(14), 
      followup_end_date)).arrival_date.minimum_for_patient()),
      (emergency_care_diagnosis_matches(codelists.wheeze_attendance)
      .where(emergency_care_attendances.arrival_date
      .is_on_or_between(dataset.rsv_primary_date + days(14),
      followup_end_date)).arrival_date.minimum_for_patient()))))
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
      case(when(rsv_code_number >= 1)
      .then(rsv_med_inclusion_date)))))
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
    rsv_codes_second_date = (
      case(when(rsv_code_number_second > 1)
      .then(is_infection_event(codelists
      .rsv_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.rsv_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient()
      .date))
    )
    
    #get the date of the occurrence of first relevant prescription
    # - looking at the second episode
    rsv_med_second_date = (
      medications.where(medications.dmd_code.is_in(codelists
      .rsv_prescriptions_codelist)).where(medications.date
      .is_on_or_between(dataset.rsv_primary_date + days(14),
      followup_end_date)).date.minimum_for_patient()
    )
    
    #get occurrence of event in exclusion list within one month of 
    #rsv_codes_second_date - looking at the same criteria as the first episode
    rsv_exclusion_primary_second = (case(
      when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_between(dataset
      .rsv_primary_date + days(14), followup_end_date))
      .sort_by(clinical_events.date).first_for_patient().date
      .is_on_or_between(rsv_codes_second_date - days(30),
      rsv_codes_second_date + days(30))).then(True),
      when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_between(dataset
      .rsv_primary_date + days(14), followup_end_date))
      .sort_by(clinical_events.date).first_for_patient().date
      .is_on_or_between(rsv_med_second_date - days(30),
      rsv_med_second_date + days(30))).then(True), otherwise = False)
    )
    
    #define prescription inclusion - i.e. presence of a code from codelist with
    #relevant prescription
    rsv_med_inclusion_second_date = (case(
      when(is_infection_event(codelists.rsv_sensitive_codelist)
      .where(clinical_events.date.is_on_or_between(dataset
      .rsv_primary_date + days(14), followup_end_date))
      .sort_by(clinical_events.date).first_for_patient()
      .date.is_on_or_between(rsv_med_second_date - days(14),
      rsv_med_second_date + days(14))).then(rsv_med_second_date))
    )
   
    #extract date of second episode - using the same criteria as the first episode
    dataset.rsv_primary_second_date = (case(
      when(~rsv_exclusion_primary).then(
      minimum_of((is_infection_event(codelists.rsv_primary_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .rsv_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date), (case(when(rsv_code_number_second > 1)
      .then(rsv_med_inclusion_second_date))))))
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
    index_date, followup_end_date))
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
    followup_end_date)).first_for_patient()
    .discharge_date
  )
  
  #extract length of stay for first episode, in hours
  dataset.rsv_los = (
    diff_dates_hours(dataset.rsv_secondary_date,
    rsv_secondary_discharge)
  )
  
  #extract date of second episode - using the same criteria as the first episode
  dataset.rsv_secondary_second_date = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.rsv_secondary_date + days(14),
    followup_end_date)).first_for_patient()
    .admission_date
  )
  
  #get discharge date for second episode
  rsv_secondary_discharge_second = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)))
    .where(apcs.discharge_date.is_on_or_between(
    dataset.rsv_secondary_second_date,
    followup_end_date)).first_for_patient()
    .discharge_date
  )
  
  #extract length of stay for second episode, in hours
  dataset.rsv_los_second = (
    diff_dates_hours(dataset.rsv_secondary_second_date,
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
    index_date, followup_end_date))
    .admission_date.minimum_for_patient()
  )
  
  # #get info for separate inclusion criteria
  # dataset.rsv_secondary_spec_criteria = (
  #   apcs.sort_by(apcs.admission_date)
  #   .where((apcs.primary_diagnosis
  #   .is_in(codelists.rsv_secondary_codelist)) 
  #   |(apcs.secondary_diagnosis
  #   .is_in(codelists.rsv_secondary_codelist)))
  #   .where(apcs.admission_date.is_on_or_between(
  #   index_date, followup_end_date))
  #   .first_for_patient().admission_date
  # )
  # dataset.rsv_secondary_criteria_one = (
  #   apcs.sort_by(apcs.admission_date).where(
  #   (hospitalisation_diagnosis_matches(codelists
  #   .rsv_secondary_codelist).exists_for_patient()))
  #   .where(apcs.admission_date.is_on_or_between(
  #   index_date, followup_end_date))
  #   .admission_date.minimum_for_patient()
  # )
  # dataset.rsv_secondary_criteria_two = (
  #   apcs.sort_by(apcs.admission_date).where(
  #   (hospitalisation_diagnosis_matches(codelists
  #   .unspecified_lrti).exists_for_patient()))
  #   .where(apcs.admission_date.is_on_or_between(
  #   index_date, followup_end_date))
  #   .admission_date.minimum_for_patient()
  # )
  
  #get occurrence of event in exclusion list within one month of an occurrence 
  #of rsv_secondary_sens_date 
  rsv_exclusion_secondary = (case(
    when((hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_exclusion_codelist))
    .admission_date.minimum_for_patient()
    .is_on_or_between(maximum_of(index_date,
    rsv_secondary_sens_date - days(30)),
    minimum_of(rsv_secondary_sens_date + days(30),
    followup_end_date))).then(True), otherwise = False)
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
    dataset.rsv_secondary_date, followup_end_date))
    .discharge_date.minimum_for_patient()))
  )
  
  #extract length of stay for first episode, in hours
  dataset.rsv_los = (
    diff_dates_hours(dataset.rsv_secondary_date,
    rsv_secondary_discharge)
  )  
  
  #get date of first diagnosis code (in any position) from the RSV sensitive 
  #secondary care codelist - looking at the second episode
  rsv_secondary_sens_second_date = (
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .unspecified_lrti).exists_for_patient()))
    .where(apcs.admission_date.is_on_or_between(
    dataset.rsv_secondary_date + days(14), followup_end_date))
    .admission_date.minimum_for_patient()
  )
  
  #get occurrence of event in exclusion list within one month of an occurrence 
  #of rsv_secondary_sens_second_date 
  rsv_exclusion_secondary_second = (case(
    when((hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_exclusion_codelist))
    .admission_date.minimum_for_patient()
    .is_on_or_between(maximum_of(index_date,
    rsv_secondary_sens_second_date - days(30)),
    minimum_of(rsv_secondary_sens_second_date + days(30),
    followup_end_date))).then(True), otherwise = False)
  )
  
  #extract date of second episode - using the same criteria as the first episode
  dataset.rsv_secondary_second_date = (case(
    when(~rsv_exclusion_secondary_second)
    .then(rsv_secondary_sens_second_date))
  )
  
  #get discharge date for second episode
  rsv_secondary_discharge_second = (case(
    when(~rsv_exclusion_secondary_second).then(
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .rsv_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .unspecified_lrti).exists_for_patient()))
    .where(apcs.discharge_date.is_on_or_between(dataset
    .rsv_secondary_second_date, followup_end_date))
    .discharge_date.minimum_for_patient()))  
  )
  
  #extract length of stay for second episode, in hours
  dataset.rsv_los_second = (
    diff_dates_hours(dataset.rsv_secondary_second_date,
    rsv_secondary_discharge_second)
  )
  
##extract outcomes - flu 

#extract flu primary care dates for primary analysis ('specific' phenotype)
if codelist_type == "specific" :
  
  #extract date of first episode - looking at the first date for which there is
  #a code in the flu primary codelist
  dataset.flu_primary_date = (
    first_infection_event(codelists
    .flu_primary_codelist).date
  )
  
  #extract date of second episode - using the same criteria as the first episode
  dataset.flu_primary_second_date = (
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
  
  #get date of occurrence of first relevant prescription
  flu_med_date = (
    medications.where(medications.dmd_code.is_in(codelists
    .flu_prescriptions_codelist)).where(medications.date
    .is_on_or_between(index_date, followup_end_date))
    .date.minimum_for_patient()
  )
  
  #occurrence of event in exclusion list within one month of ILI
  flu_exclusion_primary = (case(
    when(first_infection_event(codelists.flu_primary_exclusion_codelist)
    .date.is_on_or_between(first_infection_event(codelists
    .flu_sensitive_codelist).date - days(30), first_infection_event(
    codelists.flu_sensitive_codelist).date + days(30))).then(True), 
    when(first_infection_event(codelists.flu_primary_exclusion_codelist)
    .date.is_on_or_between(ILI_date - days(30), ILI_date + days(30)))
    .then(True), when(first_infection_event(codelists
    .flu_primary_exclusion_codelist).date.is_on_or_between(
    flu_med_date - days(30), flu_med_date + days(30)))
    .then(True), otherwise = False)
  )
  
  #get date of first flu episode
  dataset.flu_primary_date = (case(
    when(~flu_exclusion_primary).then(
    minimum_of((first_infection_event(codelists
    .flu_sensitive_codelist).date), (ILI_date),
    (flu_med_date))))
  )
  
  #get date of first case of either ARI or fever for second episode
  ari_second_date = (
    is_infection_event(codelists.ari_primary_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .flu_primary_date + days(14))).sort_by(clinical_events.date)
    .first_for_patient().date
  )
  fever_second_date = (
    is_infection_event(codelists.fever_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .flu_primary_date + days(14))).sort_by(clinical_events.date)
    .first_for_patient().date
  )
  
  #define second occurrence of ILI using same criteria as first episode
  ILI_case_second = (case(
    when((diff_dates_days(ari_second_date, fever_second_date) <= 14)
    & (diff_dates_days(ari_second_date, fever_second_date) >= -14))
    .then(True), otherwise = False)
  )
  
  #get date of second occurence of ILI
  ILI_second_date = (case(
    when(ILI_case_second).then(minimum_of(ari_second_date,
    fever_second_date)))
  )
  
  #get date of occurrence of first relevant prescription
  flu_med_second_date = (
    medications.where(medications.dmd_code.is_in(codelists
    .flu_prescriptions_codelist)).where(medications.date
    .is_on_or_between(dataset.flu_primary_date + days(14),
    followup_end_date)).date.minimum_for_patient()
  )
  
  #occurrence of event in exclusion list within one month of second ILI 
  flu_exclusion_primary_second = (case(
    when(is_infection_event(codelists.flu_primary_exclusion_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .flu_primary_date + days(14))).sort_by(clinical_events
    .date).first_for_patient().date.is_on_or_between(
    is_infection_event(codelists.flu_sensitive_codelist)
    .where(clinical_events.date.is_on_or_between(dataset
    .flu_primary_date + days(14), followup_end_date))
    .sort_by(clinical_events.date).first_for_patient().date - days(30), 
    is_infection_event(codelists.flu_sensitive_codelist)
    .where(clinical_events.date.is_on_or_between(dataset
    .flu_primary_date + days(14), followup_end_date))
    .sort_by(clinical_events.date).first_for_patient()
    .date + days(30))).then(True), when(is_infection_event(
    codelists.flu_primary_exclusion_codelist).where(
    clinical_events.date.is_on_or_between(dataset
    .flu_primary_date + days(14), followup_end_date))
    .sort_by(clinical_events.date).first_for_patient().date
    .is_on_or_between(ILI_second_date - days(30), 
    ILI_second_date + days(30))).then(True),
    when(is_infection_event(codelists.flu_primary_exclusion_codelist)
    .where(clinical_events.date.is_on_or_between(dataset
    .flu_primary_date + days(14), followup_end_date))
    .sort_by(clinical_events.date).first_for_patient().date
    .is_on_or_between(flu_med_second_date - days(30), 
    flu_med_second_date + days(30))).then(True), otherwise = False)
  )
  
  #get date of second flu episode
  dataset.flu_primary_second_date = (case(
    when(~flu_exclusion_primary_second).then(
    minimum_of((is_infection_event(codelists
   .flu_sensitive_codelist).where(clinical_events
    .date.is_on_or_after(dataset.flu_primary_date + days(14)))
    .sort_by(clinical_events.date).first_for_patient()
    .date), (ILI_second_date), (flu_med_second_date))))
  )
  
#extract flu secondary care dates for primary analysis ('specific' phenotype)
if codelist_type == "specific" :
  
  #extract date of first episode - looking at the first date for which there is
  #a code in the flu secondary codelist as the primary or secondary diagnosis
  dataset.flu_secondary_date = (
    apcs.sort_by(apcs.admission_date)
    .where(apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_codelist) 
    |apcs.secondary_diagnosis
    .is_in(codelists.flu_secondary_codelist))
    .where(apcs.admission_date.is_on_or_between(
    index_date, followup_end_date)).admission_date
    .minimum_for_patient()
  )
  
  #get discharge date for first episode
  flu_secondary_discharge = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.flu_secondary_codelist)))
    .where(apcs.discharge_date
    .is_on_or_between(dataset.flu_secondary_date,
    followup_end_date)).discharge_date
    .minimum_for_patient()
  )
  
  #extract length of stay for first episode, in hours
  dataset.flu_los = (
    diff_dates_hours(dataset.flu_secondary_date,
    flu_secondary_discharge)
  )
  
  #extract date of second episode - using the same criteria as the first episode
  dataset.flu_secondary_second_date = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_codelist)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.flu_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.flu_secondary_date + days(14),
    followup_end_date)).admission_date
    .minimum_for_patient()
  )
  
  #get discharge date for second episode
  flu_secondary_discharge_second = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.flu_secondary_codelist)))
    .where(apcs.discharge_date.is_on_or_between(
    dataset.flu_secondary_second_date,
    followup_end_date)).discharge_date
    .minimum_for_patient()
  )
  
  #extract length of stay for second episode, in hours
  dataset.flu_los_second = (
    diff_dates_hours(dataset.flu_secondary_second_date,
    flu_secondary_discharge_second)
  )
  
#extract flu secondary care dates for sensitivity analysis ('sensitive' phenotype)
else : 
  
  #get date of first diagnosis code (in any position) from the flu sensitive
  #secondary care codelist or ARI secondary care codelist - looking at the first episode
  flu_secondary_sens_date = (
    apcs.sort_by(apcs.admission_date)
    .where((hospitalisation_diagnosis_matches(codelists
    .flu_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .ari_secondary_codelist).exists_for_patient()))
    .where(apcs.admission_date.is_on_or_between(
    index_date, followup_end_date)).admission_date
    .minimum_for_patient()
  )
  
  # #get info for separate inclusion criteria
  # dataset.flu_secondary_spec_criteria = (
  #   apcs.sort_by(apcs.admission_date)
  #   .where(apcs.primary_diagnosis
  #   .is_in(codelists.flu_secondary_codelist) 
  #   |apcs.secondary_diagnosis
  #   .is_in(codelists.flu_secondary_codelist))
  #   .where(apcs.admission_date.is_on_or_between(
  #   index_date, followup_end_date)).admission_date
  #   .minimum_for_patient()
  # )
  # dataset.flu_secondary_criteria_one = (
  #   apcs.sort_by(apcs.admission_date)
  #   .where((hospitalisation_diagnosis_matches(codelists
  #   .flu_secondary_codelist).exists_for_patient()))
  #   .where(apcs.admission_date.is_on_or_between(
  #   index_date, followup_end_date)).admission_date
  #   .minimum_for_patient()
  # )
  # dataset.flu_secondary_criteria_two = (
  #   apcs.sort_by(apcs.admission_date)
  #   .where((hospitalisation_diagnosis_matches(codelists
  #   .ari_secondary_codelist).exists_for_patient()))
  #   .where(apcs.admission_date.is_on_or_between(
  #   index_date, followup_end_date)).admission_date
  #   .minimum_for_patient()
  # )
  
  #occurance of event in exclusion list within one month of flu_secondary_sens_date
  flu_exclusion_secondary = (case(
    when((hospitalisation_diagnosis_matches(codelists
    .flu_secondary_exclusion_codelist))
    .where(apcs.admission_date.is_on_or_before(followup_end_date))
    .admission_date.minimum_for_patient()
    .is_on_or_between(maximum_of(index_date,
    flu_secondary_sens_date - days(30)), minimum_of(
    flu_secondary_sens_date + days(30), followup_end_date)))
    .then(True), otherwise = False)
  )
  
  #extract date of first episode - looking at when the exclusion criteria is
  #not met
  dataset.flu_secondary_date = (case(
    when(~flu_exclusion_secondary)
    .then(flu_secondary_sens_date))
  )
  
  #get discharge date for first episode
  flu_secondary_discharge = (case(
    when(~flu_exclusion_secondary).then(
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .flu_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .ari_secondary_codelist).exists_for_patient()))
    .where(apcs.discharge_date.is_on_or_between(
    dataset.flu_secondary_date, followup_end_date))
    .discharge_date.minimum_for_patient()))
  )
  
  #extract length of stay for first episode, in hours
  dataset.flu_los = (
    diff_dates_hours(dataset.flu_secondary_date,
    flu_secondary_discharge)
  )
  
  #get date of first diagnosis code (in any position) from the flu sensitive 
  #secondary care codelist - looking at the second episode
  flu_secondary_sens_second_date = (
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .flu_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .ari_secondary_codelist).exists_for_patient()))
    .where(apcs.admission_date.is_on_or_between(
    dataset.flu_secondary_date + days(14), followup_end_date))
    .admission_date.minimum_for_patient()
  )
  
  #get occurrence of event in exclusion list within one month of an occurrence 
  #of flu_secondary_sens_second_date
  flu_exclusion_secondary_second = (case(
    when((hospitalisation_diagnosis_matches(codelists
    .flu_secondary_exclusion_codelist))
    .admission_date.minimum_for_patient()
    .is_on_or_between(maximum_of(index_date,
    flu_secondary_sens_second_date - days(30)),
    minimum_of(flu_secondary_sens_second_date + days(30),
    followup_end_date))).then(True), otherwise = False)
  )
  
  #extract date of second episode - using the same criteria as the first episode
  dataset.flu_secondary_second_date = (case(
    when(~flu_exclusion_secondary_second)
    .then(flu_secondary_sens_second_date))
  )
  
  #get discharge date for second episode
  flu_secondary_discharge_second = (case(
    when(~flu_exclusion_secondary_second).then(
    apcs.sort_by(apcs.admission_date).where(
    (hospitalisation_diagnosis_matches(codelists
    .flu_secondary_codelist).exists_for_patient())
    |(hospitalisation_diagnosis_matches(codelists
    .ari_secondary_codelist).exists_for_patient()))
    .where(apcs.discharge_date.is_on_or_between(dataset
    .flu_secondary_second_date, followup_end_date))
    .first_for_patient().discharge_date))  
  )
  
  #extract length of stay for second episode, in hours
  dataset.flu_los_second = (
    diff_dates_hours(dataset.flu_secondary_second_date,
    flu_secondary_discharge_second)
  )

##extract outcomes - covid

if study_start_date >= covid_season_min :
  
  #extract covid primary care dates for primary analysis ('specific' phenotype)
  if codelist_type == "specific" :
    
    #extract date of first episode 
    dataset.covid_primary_date = (
      first_infection_event(codelists
      .covid_primary_codelist).date
    )
    
    #extract date of second episode
    dataset.covid_primary_second_date = (
      is_infection_event(codelists.covid_primary_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .covid_primary_date + days(14))).sort_by(clinical_events
      .date).first_for_patient().date
    )
    
  else :
    
    #count number of clinical codes in covid symptom list which occur within 14 days
    #looking at the first episode
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
    
    #get date of first occurrence of relevant prescription
    covid_med_date = (
      medications.where(medications.dmd_code.is_in(codelists
      .covid_prescriptions_codelist)).where(medications.date
      .is_on_or_between(index_date, followup_end_date))
      .date.minimum_for_patient()
    )
    
    #occurrence of event in exclusion list within one month of covid_codes_date
    #- looking at the first date for which there is a code in the covid exclusion
    #codelist within one month before or after the date of covid_codes_date, or a 
    #code in the covid prescriptions codelist within one month before or after the
    #date of covid_codes_date
    covid_exclusion_primary = (case(
      when(first_infection_event(codelists.covid_primary_exclusion_codelist)
      .date.is_on_or_between(covid_codes_date - days(30),
      covid_codes_date + days(30))).then(True),
      when(first_infection_event(codelists
      .covid_primary_exclusion_codelist).date.is_on_or_between(
      covid_med_date - days(30), covid_med_date + days(30)))
      .then(True), otherwise = False)
    )
    
    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a code in the covid primary codelist, 
    #a code in the covid sensitive codelist, or a prescription in the covid
    #prescriptions codelist
    dataset.covid_primary_date = (case(
      when(~covid_exclusion_primary).then(
      minimum_of((first_infection_event(codelists
      .covid_primary_codelist).date), (covid_codes_date),
      (medications.where(medications.dmd_code
      .is_in(codelists.covid_prescriptions_codelist))
      .where(medications.date.is_on_or_between(index_date,
      followup_end_date)).date.minimum_for_patient()))))
    )
    
    #count number of clinical codes in covid symptom list for second episode
    covid_code_number_second = (
      (clinical_events.where(clinical_events
      .date.is_on_or_between(is_infection_event(codelists
      .covid_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.covid_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient()
      .date, is_infection_event(codelists
      .covid_sensitive_codelist).where(clinical_events.date
      .is_on_or_after(dataset.covid_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient()
      .date + days(14)))).snomedct_code
      .is_in(codelists.covid_sensitive_codelist)
      .count_distinct_for_patient()
    )
    
    #date of first occurrence of two of the above codes within 2 weeks - for second episode
    covid_codes_second_date = (
      case(when(covid_code_number_second > 1)
      .then(is_infection_event(codelists
      .covid_sensitive_codelist).where(clinical_events
      .date.is_on_or_after(dataset.covid_primary_date + days(14)))
      .sort_by(clinical_events.date).first_for_patient()
      .date))
    )
    
    #get date of first occurrence of relevant prescription - for second episode
    covid_med_second_date = (
      medications.where(medications.dmd_code.is_in(codelists
      .covid_prescriptions_codelist)).where(medications.date
      .is_on_or_between(dataset.covid_primary_date + days(14),
      followup_end_date)).date.minimum_for_patient()
    )
    
    #occurrence of event in exclusion list within one month of covid_codes_second_date
    # - using the same criteria as the first episode
    covid_exclusion_primary_second = (case(
      when(is_infection_event(codelists.covid_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_between(dataset
      .covid_primary_date + days(14), followup_end_date))
      .sort_by(clinical_events.date).first_for_patient().date
      .is_on_or_between(covid_codes_second_date - days(30),
      covid_codes_second_date + days(30))).then(True),
      when(is_infection_event(codelists.covid_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_between(dataset
      .covid_primary_date + days(14), followup_end_date))
      .sort_by(clinical_events.date).first_for_patient().date
      .is_on_or_between(covid_med_second_date - days(30),
      covid_med_second_date + days(30))).then(True), otherwise = False )                                 
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.covid_primary_second_date = (case(
      when(~covid_exclusion_primary).then(
      minimum_of((is_infection_event(codelists.covid_primary_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .covid_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date), (case(when(covid_code_number_second > 1)
      .then(is_infection_event(codelists.covid_sensitive_codelist)
      .where(clinical_events.date.is_on_or_after(dataset
      .covid_primary_date + days(14))).sort_by(clinical_events.date)
      .first_for_patient().date))), (medications.where(medications
      .dmd_code.is_in(codelists.covid_prescriptions_codelist))
      .where(medications.date.is_on_or_between(dataset
      .covid_primary_date + days(14), followup_end_date))
      .date.minimum_for_patient()))))
    )
    
  #extract covid secondary care dates for primary analysis ('specific' phenotype)
  if codelist_type == "specific" :
    #extract date of first episode - looking at the first date for which there is
    #a code in the covid secondary codelist as the primary or secondary diagnosis
    dataset.covid_secondary_date = (
      apcs.sort_by(apcs.admission_date)
      .where(apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist) 
      |apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      .where(apcs.admission_date.is_on_or_between(
      index_date, followup_end_date)).first_for_patient()
      .admission_date
    )
    
    #get discharge date for first episode
    covid_secondary_discharge = (
      apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      |(apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist)))
      .where(apcs.discharge_date
      .is_on_or_between(dataset.covid_secondary_date,
      followup_end_date)).first_for_patient()
      .discharge_date
    )
    
    #extract length of stay for first episode, in hours
    dataset.covid_los = (
      diff_dates_hours(dataset.covid_secondary_date,
      covid_secondary_discharge)
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.covid_secondary_second_date = (
      apcs.sort_by(apcs.admission_date)
      .where(apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist) 
      |apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      .where(apcs.admission_date.is_on_or_between(
      dataset.covid_secondary_date + days(14), followup_end_date))
      .first_for_patient().admission_date
    )
    
    #get discharge date for second episode
    covid_secondary_discharge_second = (
      apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      |(apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist)))
      .where(apcs.discharge_date
      .is_on_or_between(dataset.covid_secondary_second_date,
      followup_end_date)).first_for_patient()
      .discharge_date
    )
    
    #extract length of stay for second episode, in hours
    dataset.covid_los_second = (
      diff_dates_hours(dataset.covid_secondary_second_date,
      covid_secondary_discharge_second)
    )
    
  else :
    
    #get date of first diagnosis code (in any position) from the covid sensitive
    #secondary care codelist - looking at the first episode
    covid_secondary_sens_date = (
      apcs.sort_by(apcs.admission_date).where(
      (hospitalisation_diagnosis_matches(codelists
      .covid_secondary_codelist).exists_for_patient())
      |(hospitalisation_diagnosis_matches(codelists
      .coronavirus_unspecified).exists_for_patient()))
      .where(apcs.admission_date.is_on_or_between(index_date,
      followup_end_date)).admission_date.minimum_for_patient()
    )
    
    #get occurrence of event in exclusion list within one month of covid_secondary_sens_date
    #- looking at the first date for which there is a code in the covid secondary
    #exclusion codelist within one month before or after the date of covid_secondary_sens_date
    covid_exclusion_secondary = (case(
      when((hospitalisation_diagnosis_matches(codelists
      .covid_secondary_exclusion_codelist))
      .admission_date.minimum_for_patient()
      .is_on_or_between(maximum_of(index_date,
      covid_secondary_sens_date - days(30)),
      minimum_of(covid_secondary_sens_date + days(30),
      followup_end_date))).then(True), otherwise = False)
    )
    
    #extract date of first episode - looking at when the exclusion criteria is
    #not met 
    dataset.covid_secondary_date = (case(
      when(~covid_exclusion_secondary)
      .then(covid_secondary_sens_date))
    )
    
    #get discharge date for first episode
    covid_secondary_discharge = (case(
      when(~covid_exclusion_secondary).then(
      apcs.sort_by(apcs.admission_date).where(
      (hospitalisation_diagnosis_matches(codelists
      .covid_secondary_codelist).exists_for_patient())
      |(hospitalisation_diagnosis_matches(codelists
      .coronavirus_unspecified).exists_for_patient()))
      .where(apcs.discharge_date.is_on_or_between(dataset
      .covid_secondary_date, followup_end_date))
      .discharge_date.minimum_for_patient()))   
    )
    
    #extract length of stay for first episode, in hours
    dataset.covid_los = (
      diff_dates_hours(dataset.covid_secondary_date,
      covid_secondary_discharge)
    )
    
    #get date of first diagnosis code (in any position) from the covid sensitive
    #secondary care codelist - looking at the second episode
    covid_secondary_sens_second_date = (
      apcs.sort_by(apcs.admission_date).where(
      (hospitalisation_diagnosis_matches(codelists
      .covid_secondary_codelist).exists_for_patient())
      |(hospitalisation_diagnosis_matches(codelists
      .coronavirus_unspecified).exists_for_patient()))
      .where(apcs.admission_date.is_on_or_between(
      dataset.covid_secondary_date + days(14), followup_end_date))
      .admission_date.minimum_for_patient()
    )
    
    #get occurrence of event in exclusion list within one month of an occurrence
    #of covid_secondary_sens_second_date
    covid_exclusion_secondary_second = (case(
      when((hospitalisation_diagnosis_matches(codelists
      .covid_secondary_exclusion_codelist))
      .admission_date.minimum_for_patient()
      .is_on_or_between(maximum_of(index_date,
      covid_secondary_sens_second_date - days(30)),
      minimum_of(covid_secondary_sens_second_date + days(30),
      followup_end_date))).then(True), otherwise = False)
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.covid_secondary_second_date = (case(
      when(~covid_exclusion_secondary_second)
      .then(covid_secondary_sens_second_date))
    )
    
    #get discharge date for second episode
    covid_secondary_discharge_second = (case(
      when(~covid_exclusion_secondary_second).then(
      apcs.sort_by(apcs.admission_date).where(
      (hospitalisation_diagnosis_matches(codelists
      .covid_secondary_codelist).exists_for_patient())
      |(hospitalisation_diagnosis_matches(codelists
      .coronavirus_unspecified).exists_for_patient()))
      .where(apcs.discharge_date.is_on_or_between(dataset
      .covid_secondary_second_date, followup_end_date))
      .first_for_patient().discharge_date))  
    )
    
    #extract length of stay for second episode, in hours
    dataset.covid_los_second = (
      diff_dates_hours(dataset.covid_secondary_second_date,
      covid_secondary_discharge_second)
    )

##extract outcomes - unspecified respiratory infection

#extract unspecified respiratory infection primary care dates for sensitivity analysis ('sensitive' phenotype)
if codelist_type == "sensitive" :
  
  #count number of clinical codes in overall respiratory symptom list which occur within 14 days
  overall_resp_code_number = (
    (clinical_events.where(clinical_events
    .date.is_on_or_between(first_infection_event(codelists
    .respiratory_virus_primary_codelist).date,
    first_infection_event(codelists.respiratory_virus_primary_codelist)
    .date + days(14))).where(clinical_events.snomedct_code
    .is_in(codelists.respiratory_virus_primary_codelist)))
    .snomedct_code.count_distinct_for_patient()
  )
  
  #date of first occurrence of two of the above codes within 2 weeks
  overall_resp_codes_date = (
    case(when(overall_resp_code_number > 1)
    .then(first_infection_event(codelists
    .respiratory_virus_primary_codelist).date))
  )
  
  #occurrence of event in exclusion list within one month of overall_resp_codes_date
  # - looking at the first date for which there is a code in the respiratory virus
  #primary exclusion codelist within one month before or after the date of
  #overall_resp_codes_date
  overall_resp_exclusion_primary = (case(
    when(first_infection_event(codelists
    .respiratory_virus_primary_exclusion_codelist)
    .date.is_on_or_between(overall_resp_codes_date - days(30), 
    overall_resp_codes_date + days(30))).then(True), otherwise = False)
  )
  
  if cohort == "older_adults" :
    
    if study_start_date >= covid_season_min :
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV primary episode, flu primary episode,
      #covid primary episode, a code in the respiratory virus primary codelist,
      #a code in emergency care for a respiratory tract infection, a code in 
      #emergency care for a COPD exacerbation, a code in emergency care for an 
      #asthma exacerbation, a code in primary care for a COPD exacerbation,
      #or a code in primary care for an asthma exacerbation
      dataset.overall_resp_primary_date = (case(
        when(~overall_resp_exclusion_primary).then(
        minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date,
        dataset.covid_primary_date, first_infection_event(codelists.
        respiratory_virus_primary_codelist).date,
        emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient(),
        emergency_care_diagnosis_matches(codelists.copd_exacerbation_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient(), first_infection_event(codelists
        .copd_exacerbation_primary_codelist).date, first_infection_event(
        codelists.asthma_exacerbation_primary_codelist).date)))
      ) 
      
      #count number of clinical codes in overall respiratory symptom list
      # - for second episode
      overall_resp_code_number_second = (
        (clinical_events.where(clinical_events
        .date.is_on_or_between(is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events.date
        .is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient()
        .date, is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events.date
        .is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient()
        .date + days(14)))).snomedct_code
        .is_in(codelists.respiratory_virus_primary_codelist)
        .count_distinct_for_patient()
      )
      
      #date of first occurrence of two of the above codes within 2 weeks - for second episode
      overall_resp_codes_second_date = (
        case(when(overall_resp_code_number_second > 1)
        .then(is_infection_event(codelists.respiratory_virus_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date))
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
      # - using the same criteria as the first episode
      overall_resp_exclusion_primary_second = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(dataset
        .overall_resp_primary_date + days(14), followup_end_date))
        .sort_by(clinical_events.date).first_for_patient().date
        .is_on_or_between(overall_resp_codes_second_date - days(30),
        overall_resp_codes_second_date + days(30))).then(True),
        otherwise = False )                                 
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_primary_second_date = (case(
        when(~overall_resp_exclusion_primary_second).then(
        minimum_of(dataset.rsv_primary_second_date, dataset
        .flu_primary_second_date, dataset.covid_primary_second_date,
        is_infection_event(codelists.respiratory_virus_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date, emergency_care_diagnosis_matches(
        codelists.rtri_attendance).where(emergency_care_attendances.arrival_date
        .is_on_or_between(dataset.overall_resp_primary_date + days(14),
        followup_end_date)).arrival_date.minimum_for_patient(),
        emergency_care_diagnosis_matches(codelists.copd_exacerbation_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(dataset.rsv_primary_date + days(14), 
        followup_end_date)).arrival_date.minimum_for_patient(),
        is_infection_event(codelists.copd_exacerbation_primary_codelist)
        .sort_by(clinical_events.date).first_for_patient().date,
        is_infection_event(codelists.asthma_exacerbation_primary_codelist)
        .sort_by(clinical_events.date).first_for_patient().date)))
      )
      
    else:
      
      #extract date of first episode - looking at the first date for which there is
      #a RSV primary episode, a flu primary episode, a code in the respiratory 
      #virus primary codelist, a code in emergency care for a respiratory tract
      #infection, a code in emergency care for a COPD exacerbation, a code in
      #emergency care for an asthma exacerbation, a code in primary care for a
      #COPD exacerbation, or a code in primary care for an asthma exacerbation
      dataset.overall_resp_primary_date = (case(
        when(~overall_resp_exclusion_primary).then(
        minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date,
        first_infection_event(codelists.respiratory_virus_primary_codelist)
        .date, emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient(),
        emergency_care_diagnosis_matches(codelists
        .copd_exacerbation_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient(),
        first_infection_event(codelists.copd_exacerbation_primary_codelist)
        .date, first_infection_event(codelists
        .asthma_exacerbation_primary_codelist).date)))
      )
      
      #count number of clinical codes in overall respiratory symptom list 
      # - for second episode
      overall_resp_code_number_second = (
        (clinical_events.where(clinical_events
        .date.is_on_or_between(is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events.date
        .is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient()
        .date, is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events.date
        .is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient()
        .date + days(14)))).snomedct_code
        .is_in(codelists.respiratory_virus_primary_codelist)
        .count_distinct_for_patient()
      )
      
      #date of first occurrence of two of the above codes within 2 weeks
      #- for second episode
      overall_resp_codes_second_date = (
        case(when(overall_resp_code_number_second > 1)
        .then(is_infection_event(codelists.respiratory_virus_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date))
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
      # - using the same criteria as the first episode
      overall_resp_exclusion_primary_second = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(dataset
        .overall_resp_primary_date + days(14), followup_end_date))
        .sort_by(clinical_events.date).first_for_patient().date
        .is_on_or_between(overall_resp_codes_second_date - days(30),
        overall_resp_codes_second_date + days(30))).then(True),
        otherwise = False )                                 
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_primary_second_date = (case(
        when(~overall_resp_exclusion_primary_second).then(
        minimum_of(dataset.rsv_primary_second_date,
        dataset.flu_primary_second_date, is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events
        .date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date,
        emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date.is_on_or_between(
        dataset.overall_resp_primary_date + days(14), followup_end_date))
        .arrival_date.minimum_for_patient(), emergency_care_diagnosis_matches(
        codelists.copd_exacerbation_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(dataset.rsv_primary_date + days(14), 
        followup_end_date)).arrival_date.minimum_for_patient(),
        is_infection_event(codelists.copd_exacerbation_primary_codelist)
        .sort_by(clinical_events.date).first_for_patient().date,
        is_infection_event(codelists.asthma_exacerbation_primary_codelist)
        .sort_by(clinical_events.date).first_for_patient().date)))
      )
      
  else:
    
    if study_start_date >= covid_season_min :
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV primary episode, flu primary episode,
      #covid primary episode, a code in the respiratory virus primary codelist,
      #or a code in emergency care for a respiratory tract infection
      dataset.overall_resp_primary_date = (case(
        when(~overall_resp_exclusion_primary).then(
        minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date,
        dataset.covid_primary_date, first_infection_event(codelists
        .respiratory_virus_primary_codelist).date, 
        emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient())))
      )
      
      #count number of clinical codes in overall respiratory symptom list 
      # - for second episode
      overall_resp_code_number_second = (
        (clinical_events.where(clinical_events
        .date.is_on_or_between(is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events.date
        .is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient()
        .date, is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events.date
        .is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient()
        .date + days(14)))).snomedct_code
        .is_in(codelists.respiratory_virus_primary_codelist)
        .count_distinct_for_patient()
      )
      
      #date of first occurrence of two of the above codes within 2 weeks
      #- for second episode
      overall_resp_codes_second_date = (
        case(when(overall_resp_code_number_second > 1)
        .then(is_infection_event(codelists.respiratory_virus_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date))
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
      # - using the same criteria as the first episode
      overall_resp_exclusion_primary_second = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(dataset
        .overall_resp_primary_date + days(14), followup_end_date))
        .sort_by(clinical_events.date).first_for_patient().date
        .is_on_or_between(overall_resp_codes_second_date - days(30),
        overall_resp_codes_second_date + days(30))).then(True),
        otherwise = False )                                 
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_primary_second_date = (
        minimum_of(dataset.rsv_primary_second_date,
        dataset.flu_primary_second_date, dataset.covid_primary_second_date,
        is_infection_event(codelists.respiratory_virus_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date, emergency_care_diagnosis_matches(
        codelists.rtri_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(dataset
        .overall_resp_primary_date + days(14), followup_end_date))
        .arrival_date.minimum_for_patient())
      )
      
    else:
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV primary episode, flu primary episode,
      #a code in the respiratory virus primary codelist, or a code in emergency care
      #for a respiratory tract infection
      dataset.overall_resp_primary_date = (case(
        when(~overall_resp_exclusion_primary).then(
        minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date,
        first_infection_event(codelists.respiratory_virus_primary_codelist)
        .date, emergency_care_diagnosis_matches(codelists.
        rtri_attendance).where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient())))
      )
      
      #count number of clinical codes in overall respiratory symptom list 
      # - for second episode
      overall_resp_code_number_second = (
        (clinical_events.where(clinical_events
        .date.is_on_or_between(is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events.date
        .is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient()
        .date, is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events.date
        .is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient()
        .date + days(14)))).snomedct_code
        .is_in(codelists.respiratory_virus_primary_codelist)
        .count_distinct_for_patient()
      )
      
      #date of first occurrence of two of the above codes within 2 weeks 
      #- for second episode
      overall_resp_codes_second_date = (
        case(when(overall_resp_code_number_second > 1)
        .then(is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events
        .date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date))
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
      overall_resp_exclusion_primary_second = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(dataset
        .overall_resp_primary_date + days(14), followup_end_date))
        .sort_by(clinical_events.date).first_for_patient().date
        .is_on_or_between(overall_resp_codes_second_date - days(30),
        overall_resp_codes_second_date + days(30))).then(True),
        otherwise = False )                                 
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_primary_second_date = (case(
        when(~overall_resp_exclusion_primary_second).then(
        minimum_of(dataset.rsv_primary_second_date,
        dataset.flu_primary_second_date, is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events
        .date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date,
        emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date.is_on_or_between(
        dataset.overall_resp_primary_date + days(14), followup_end_date))
        .arrival_date.minimum_for_patient())))
      )
  
#extract unspecified respiratory infection secondary care dates for sensitivity analysis ('sensitive' phenotype)
if codelist_type == "sensitive" :
  
  if cohort == "older_adults" :
    
    #extract date of first episode - looking at the first date for which there is
    #a code in the respiratory virus secondary codelist (in any position), a 
    #code in the COPD exacerbation secondary codelist (in any position), or a 
    #code in the asthma exacerbation secondary codelist (in any position)
    overall_resp_secondary_sens_date = (
      minimum_of(hospitalisation_diagnosis_matches(codelists.
      respiratory_virus_secondary_codelist).where(apcs.admission_date
      .is_on_or_between(index_date, followup_end_date))
      .admission_date.minimum_for_patient(),
      hospitalisation_diagnosis_matches(codelists
      .copd_exacerbation_secondary_codelist).where(apcs.admission_date
      .is_on_or_between(index_date, followup_end_date))
      .admission_date.minimum_for_patient(),
      hospitalisation_diagnosis_matches(codelists
      .asthma_exacerbation_secondary_codelist).where(apcs.admission_date
      .is_on_or_between(index_date, followup_end_date))
      .admission_date.minimum_for_patient())
    )
    
    #occurrence of event in exclusion list within one month of overall_resp_secondary_sens_date
    # - looking at the first date for which there is a code in the respiratory virus
    #secondary exclusion codelist within one month before or after the date of
    #overall_resp_secondary_sens_date
    overall_resp_exclusion_secondary = (case(
      when(hospitalisation_diagnosis_matches(codelists
      .respiratory_virus_secondary_exclusion_codelist)
      .where(apcs.admission_date.is_on_or_between(
      maximum_of(index_date, overall_resp_secondary_sens_date - days(30)),
      minimum_of(overall_resp_secondary_sens_date + days(30),
      followup_end_date))).exists_for_patient())
      .then(True), otherwise = False)  
    )
    
    if study_start_date >= covid_season_min :
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV secondary episode, flu secondary episode,
      #covid secondary episode or overall_resp_secondary_sens_date
      dataset.overall_resp_secondary_date = (case(
        when(~overall_resp_exclusion_secondary).then(
        minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date, 
        dataset.covid_secondary_date, overall_resp_secondary_sens_date)))
      ) 
      
      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(~overall_resp_exclusion_secondary).then(case(
        when(dataset.overall_resp_secondary_date == dataset
        .rsv_secondary_date).then(rsv_secondary_discharge), 
        when(dataset.overall_resp_secondary_date == dataset
        .flu_secondary_date).then(flu_secondary_discharge),
        when(dataset.overall_resp_secondary_date == dataset
        .covid_secondary_date).then(covid_secondary_discharge),
        when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
        .then(apcs.sort_by(apcs.admission_date).where(
        (hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).exists_for_patient())
        |(hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist).exists_for_patient())
        |(hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist).exists_for_patient()))
        .where(apcs.discharge_date.is_on_or_between(dataset.
        overall_resp_secondary_date, followup_end_date))
        .first_for_patient().discharge_date))))
      )
      
      #extract length of stay for first episode, in hours
      dataset.overall_resp_los = (
        diff_dates_hours(dataset.overall_resp_secondary_date,
        overall_resp_secondary_discharge)
      ) 
      
      #extract date of second episode - using the same criteria as the first episode
      overall_resp_secondary_sens_second_date = (
        minimum_of(hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).where(apcs.admission_date
        .is_on_or_between(dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date.minimum_for_patient(), 
        hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist)
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date.minimum_for_patient(), 
        hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist)
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date + days(14), 
        followup_end_date)).admission_date.minimum_for_patient())
      )
      
      #occurrence of event in exclusion list within one month of an occurrence
      #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
      overall_resp_exclusion_secondary_second = (case(
        when(hospitalisation_diagnosis_matches(codelists
        .respiratory_virus_secondary_exclusion_codelist)
        .where(apcs.admission_date.is_on_or_between(maximum_of(index_date,
        overall_resp_secondary_sens_second_date - days(30)),
        minimum_of(overall_resp_secondary_sens_second_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)  
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_secondary_second_date = (case(
        when(~overall_resp_exclusion_secondary_second).then(
        minimum_of(dataset.rsv_secondary_second_date,
        dataset.flu_secondary_second_date, 
        dataset.covid_secondary_second_date,
        overall_resp_secondary_sens_second_date)))
      ) 
      
      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(~overall_resp_exclusion_secondary_second).then(case(
        when(dataset.overall_resp_secondary_second_date == dataset
        .rsv_secondary_second_date).then(rsv_secondary_discharge_second), 
        when(dataset.overall_resp_secondary_second_date == dataset
        .flu_secondary_second_date).then(flu_secondary_discharge_second),
        when(dataset.overall_resp_secondary_second_date == dataset
        .covid_secondary_second_date).then(covid_secondary_discharge_second),
        when(dataset.overall_resp_secondary_second_date == 
        overall_resp_secondary_sens_second_date)
        .then(apcs.sort_by(apcs.admission_date).where(
        (hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).exists_for_patient())
        |(hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist).exists_for_patient())
        |(hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist).exists_for_patient()))
        .where(apcs.discharge_date.is_on_or_between(dataset.
        overall_resp_secondary_second_date, followup_end_date))
        .first_for_patient().discharge_date))))
      )
      
      #extract length of stay for second episode, in hours
      dataset.overall_resp_los_second = (
        diff_dates_hours(dataset.overall_resp_secondary_second_date,
        overall_resp_secondary_discharge_second)
      ) 
      
    else:
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV secondary episode, flu secondary episode,
      #or overall_resp_secondary_sens_date
      dataset.overall_resp_secondary_date = (case(
        when(~overall_resp_exclusion_secondary).then(
        minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date, 
        overall_resp_secondary_sens_date)))
      ) 
      
      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(~overall_resp_exclusion_secondary).then(case(
        when(dataset.overall_resp_secondary_date == dataset
        .rsv_secondary_date).then(rsv_secondary_discharge), 
        when(dataset.overall_resp_secondary_date == dataset
        .flu_secondary_date).then(flu_secondary_discharge),
        when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
        .then(apcs.sort_by(apcs.admission_date).where(
        (hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).exists_for_patient())
        |(hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist).exists_for_patient())
        |(hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist).exists_for_patient()))
        .where(apcs.discharge_date.is_on_or_between(dataset.
        overall_resp_secondary_date, followup_end_date))
        .first_for_patient().discharge_date))))
      )
      
      #extract length of stay for first episode, in hours
      dataset.overall_resp_los = (
        diff_dates_hours(dataset.overall_resp_secondary_date,
        overall_resp_secondary_discharge)
      ) 
      
      #extract date of second episode - using the same criteria as the first episode
      overall_resp_secondary_sens_second_date = (
        minimum_of(hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).where(apcs.admission_date
        .is_on_or_between(dataset.overall_resp_secondary_date + days(14), 
        followup_end_date)).admission_date.minimum_for_patient(),
        hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist)
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date + days(14), followup_end_date))
        .admission_date.minimum_for_patient(), 
        hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist)
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date + days(14), 
        followup_end_date)).admission_date.minimum_for_patient())
      )
      
      #occurrence of event in exclusion list within one month of an occurrence
      #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
      overall_resp_exclusion_secondary_second = (case(
        when(hospitalisation_diagnosis_matches(codelists
        .respiratory_virus_secondary_exclusion_codelist)
        .where(apcs.admission_date.is_on_or_between(maximum_of(index_date,
        overall_resp_secondary_sens_second_date - days(30)),
        minimum_of(overall_resp_secondary_sens_second_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)  
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_secondary_second_date = (case(
        when(~overall_resp_exclusion_secondary_second).then(
        minimum_of(dataset.rsv_secondary_second_date,
        dataset.flu_secondary_second_date, 
        overall_resp_secondary_sens_second_date)))
      ) 
      
      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(~overall_resp_exclusion_secondary_second).then(case(
        when(dataset.overall_resp_secondary_second_date == dataset
        .rsv_secondary_second_date).then(rsv_secondary_discharge_second), 
        when(dataset.overall_resp_secondary_second_date == dataset
        .flu_secondary_second_date).then(flu_secondary_discharge_second),
        when(dataset.overall_resp_secondary_second_date == 
        overall_resp_secondary_sens_second_date)
        .then(apcs.sort_by(apcs.admission_date).where(
        (hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).exists_for_patient())
        |(hospitalisation_diagnosis_matches(codelists
        .copd_exacerbation_secondary_codelist).exists_for_patient())
        |(hospitalisation_diagnosis_matches(codelists
        .asthma_exacerbation_secondary_codelist).exists_for_patient()))
        .where(apcs.discharge_date.is_on_or_between(dataset.
        overall_resp_secondary_second_date, followup_end_date))
        .first_for_patient().discharge_date))))
      )
      
      #extract length of stay for second episode, in hours
      dataset.overall_resp_los_second = (
        diff_dates_hours(dataset.overall_resp_secondary_second_date,
        overall_resp_secondary_discharge_second)
      ) 
      
  else:
    
    #extract date of first episode - looking at the first date for which there is
    #a code in the respiratory virus secondary codelist (in any position)
    overall_resp_secondary_sens_date = (
      hospitalisation_diagnosis_matches(codelists.
      respiratory_virus_secondary_codelist).where(apcs.admission_date
      .is_on_or_between(index_date, followup_end_date))
      .admission_date.minimum_for_patient()
    )
    
    #occurrence of event in exclusion list within one month of overall_resp_secondary_sens_date
    # - looking at the first date for which there is a code in the respiratory virus
    #secondary exclusion codelist within one month before or after the date of
    #overall_resp_secondary_sens_date
    overall_resp_exclusion_secondary = (case(
      when(hospitalisation_diagnosis_matches(codelists
      .respiratory_virus_secondary_exclusion_codelist)
      .where(apcs.admission_date.is_on_or_between(maximum_of(index_date,
      overall_resp_secondary_sens_date - days(30)),
      minimum_of(overall_resp_secondary_sens_date + days(30),
      followup_end_date))).exists_for_patient())
      .then(True), otherwise = False)  
    )
    
    if study_start_date >= covid_season_min :
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV secondary episode, flu secondary episode,
      #covid secondary episode or overall_resp_secondary_sens_date
      dataset.overall_resp_secondary_date = (case(
        when(~overall_resp_exclusion_secondary).then(
        minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date, 
        dataset.covid_secondary_date, overall_resp_secondary_sens_date)))
      ) 
      
      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(~overall_resp_exclusion_secondary).then(case(
        when(dataset.overall_resp_secondary_date == dataset
        .rsv_secondary_date).then(rsv_secondary_discharge), 
        when(dataset.overall_resp_secondary_date == dataset
        .flu_secondary_date).then(flu_secondary_discharge),
        when(dataset.overall_resp_secondary_date == dataset
        .covid_secondary_date).then(covid_secondary_discharge),
        when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
        .then(apcs.sort_by(apcs.admission_date).where(   
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).exists_for_patient())
        .where(apcs.discharge_date.is_on_or_between(dataset
        .overall_resp_secondary_date, followup_end_date))
        .first_for_patient().discharge_date))))
      )
      
      #extract length of stay for first episode, in hours
      dataset.overall_resp_los = (
        diff_dates_hours(dataset.overall_resp_secondary_date,
        overall_resp_secondary_discharge)
      ) 
      
      #extract date of second episode - using the same criteria as the first episode
      overall_resp_secondary_sens_second_date = (
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).where(apcs.admission_date
        .is_on_or_between(dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date.minimum_for_patient()
      )
      
      #occurrence of event in exclusion list within one month of an occurrence
      #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
      overall_resp_exclusion_secondary_second = (case(
        when(hospitalisation_diagnosis_matches(codelists
        .respiratory_virus_secondary_exclusion_codelist)
        .where(apcs.admission_date.is_on_or_between(maximum_of(index_date,
        overall_resp_secondary_sens_second_date - days(30)),
        minimum_of(overall_resp_secondary_sens_second_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)  
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_secondary_second_date = (case(
        when(~overall_resp_exclusion_secondary_second).then(
        minimum_of(dataset.rsv_secondary_second_date,
        dataset.flu_secondary_second_date, 
        dataset.covid_secondary_second_date,
        overall_resp_secondary_sens_second_date)))
      ) 
      
      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(~overall_resp_exclusion_secondary_second).then(case(
        when(dataset.overall_resp_secondary_second_date == dataset
        .rsv_secondary_second_date).then(rsv_secondary_discharge_second), 
        when(dataset.overall_resp_secondary_second_date == dataset
        .flu_secondary_second_date).then(flu_secondary_discharge_second),
        when(dataset.overall_resp_secondary_second_date == dataset
        .covid_secondary_second_date).then(covid_secondary_discharge_second),
        when(dataset.overall_resp_secondary_second_date == 
        overall_resp_secondary_sens_second_date).then(
        apcs.sort_by(apcs.admission_date).where(   
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).exists_for_patient())
        .where(apcs.discharge_date.is_on_or_between(dataset
        .overall_resp_secondary_second_date, followup_end_date))
        .first_for_patient().discharge_date))))
      )
      
      #extract length of stay for second episode, in hours
      dataset.overall_resp_los_second = (
        diff_dates_hours(dataset.overall_resp_secondary_second_date,
        overall_resp_secondary_discharge_second)
      ) 
      
    else:
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV secondary episode, flu secondary episode,
      #or overall_resp_secondary_sens_date
      dataset.overall_resp_secondary_date = (case(
        when(~overall_resp_exclusion_secondary).then(
        minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date, 
        overall_resp_secondary_sens_date)))
      ) 
      
      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(~overall_resp_exclusion_secondary).then(case(
        when(dataset.overall_resp_secondary_date == dataset
        .rsv_secondary_date).then(rsv_secondary_discharge), 
        when(dataset.overall_resp_secondary_date == dataset
        .flu_secondary_date).then(flu_secondary_discharge),
        when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
        .then(apcs.sort_by(apcs.admission_date).where(   
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).exists_for_patient())
        .where(apcs.discharge_date.is_on_or_between(dataset
        .overall_resp_secondary_date, followup_end_date))
        .first_for_patient().discharge_date))))
      )
      
      #extract length of stay for first episode, in hours
      dataset.overall_resp_los = (
        diff_dates_hours(dataset.overall_resp_secondary_date,
        overall_resp_secondary_discharge)
      ) 
      
      #extract date of second episode - using the same criteria as the first episode
      overall_resp_secondary_sens_second_date = (
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).where(apcs.admission_date
        .is_on_or_between(dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date.minimum_for_patient()
      )
      
      #occurrence of event in exclusion list within one month of an occurrence
      #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
      overall_resp_exclusion_secondary_second = (case(
        when(hospitalisation_diagnosis_matches(codelists
        .respiratory_virus_secondary_exclusion_codelist)
        .where(apcs.admission_date.is_on_or_between(maximum_of(index_date,
        overall_resp_secondary_sens_second_date - days(30)),
        minimum_of(overall_resp_secondary_sens_second_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)  
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_secondary_second_date = (case(
        when(~overall_resp_exclusion_secondary_second).then(
        minimum_of(dataset.rsv_secondary_second_date,
        dataset.flu_secondary_second_date, 
        overall_resp_secondary_sens_second_date)))
      ) 
      
      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(~overall_resp_exclusion_secondary_second).then(case(
        when(dataset.overall_resp_secondary_second_date == dataset
        .rsv_secondary_second_date).then(rsv_secondary_discharge_second), 
        when(dataset.overall_resp_secondary_second_date == dataset
        .flu_secondary_second_date).then(flu_secondary_discharge_second),
        when(dataset.overall_resp_secondary_second_date == 
        overall_resp_secondary_sens_second_date).then(
        apcs.sort_by(apcs.admission_date).where(   
        hospitalisation_diagnosis_matches(codelists.
        respiratory_virus_secondary_codelist).exists_for_patient())
        .where(apcs.discharge_date.is_on_or_between(dataset
        .overall_resp_secondary_second_date, followup_end_date))
        .first_for_patient().discharge_date))))
      )
      
      #extract length of stay for second episode, in hours
      dataset.overall_resp_los_second = (
        diff_dates_hours(dataset.overall_resp_secondary_second_date,
        overall_resp_secondary_discharge_second)
      ) 

# ## extract mortality outcomes
# 
# #rsv mortality date
# dataset.rsv_mortality_date = (case(
#   when((cause_of_death_matches(codelists
#   .rsv_secondary_codelist)) & (dataset.death_date
#   .is_on_or_between(index_date, followup_end_date)))
#   .then(dataset.death_date))
# )
# 
# #rsv mortality date
# dataset.flu_mortality_date = (case(
#   when((cause_of_death_matches(codelists
#   .flu_secondary_codelist)) & (dataset.death_date
#   .is_on_or_between(index_date, followup_end_date)))
#   .then(dataset.death_date))
# )
# 
# if study_start_date >= covid_season_min :
#   
#   #covid mortality date
#   dataset.covid_mortality_date = (case(
#     when((cause_of_death_matches(codelists
#     .covid_secondary_codelist)) & (dataset.death_date
#     .is_on_or_between(index_date, followup_end_date)))
#     .then(dataset.death_date))
#   )
# 
# #overall mortality
# if codelist_type == "sensitive" :
#   if study_start_date >= covid_season_min :
#     
#     #overall mortality date
#     dataset.overall_resp_mortality_date = (case(
#       when((cause_of_death_matches(codelists
#       .rsv_secondary_codelist)) & (dataset.death_date
#       .is_on_or_between(index_date, followup_end_date)))
#       .then(dataset.death_date), when((cause_of_death_matches(
#       codelists.flu_secondary_codelist)) & (dataset.death_date
#       .is_on_or_between(index_date, followup_end_date)))
#       .then(dataset.death_date), when((cause_of_death_matches(
#       codelists.covid_secondary_codelist)) & (dataset.death_date
#       .is_on_or_between(index_date, followup_end_date)))
#       .then(dataset.death_date), when((cause_of_death_matches(
#       codelists.respiratory_virus_secondary_codelist)) & (dataset
#       .death_date.is_on_or_between(index_date, followup_end_date)))
#       .then(dataset.death_date), otherwise = None)
#     )
#   
#   else:
#   
#     #overall mortality date
#     dataset.overall_resp_mortality_date = (case(
#       when((cause_of_death_matches(codelists
#       .rsv_secondary_codelist)) & (dataset.death_date
#       .is_on_or_between(index_date, followup_end_date)))
#       .then(dataset.death_date), when((cause_of_death_matches(
#       codelists.flu_secondary_codelist)) & (dataset.death_date
#       .is_on_or_between(index_date, followup_end_date)))
#       .then(dataset.death_date), when((cause_of_death_matches(
#       codelists.respiratory_virus_secondary_codelist)) & (dataset
#       .death_date.is_on_or_between(index_date, followup_end_date)))
#       .then(dataset.death_date), otherwise = None)
#     )

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
  
  if cohort == "older_adults" :
    
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
