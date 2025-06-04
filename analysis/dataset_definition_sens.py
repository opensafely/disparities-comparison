import json, sys
from pathlib import Path

from datetime import date, datetime
from ehrql import Dataset, create_dataset, case, when, maximum_of, minimum_of, years, days, months
from ehrql.tables.tpp import (
  patients,
  medications,
  addresses,
  clinical_events,
  practice_registrations,
  apcs,
  emergency_care_attendances,
  parents
)

from variable_lib import (
  emergency_care_diagnosis_matches,
  hospitalisation_diagnosis_matches
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
codelist_type = args[4] #specific or sensitive
investigation_type = args[5] #primary/secondary/sensitivity

# Change these in ./analysis/design/study-dates.R if necessary
study_start_date = datetime.strptime(study_dates[args[2]], "%Y-%m-%d").date()
study_end_date = datetime.strptime(study_dates[args[3]], "%Y-%m-%d").date()

#get date patient ages into and out of cohort 
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

#set index date (and registration date) as latest date of either start date or age date
#so that patients are the correct age for the cohort when looking at records
if cohort == "infants" or cohort == "infants_subgroup" :
  index_date = maximum_of(study_start_date, study_start_date)
else : 
  index_date = maximum_of(study_start_date, age_date)

#define date for registration period
registration_date = index_date - months(3)

#set end date as earliest date of either end date or age out date 
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
  registered_patients = (
    practice_registrations.for_patient_on(index_date).exists_for_patient()
  )
else :
  registered_patients = (
    practice_registrations.for_patient_on(registration_date)
    .exists_for_patient()
  )

#get patients that are either male or female
is_female_or_male = patients.sex.is_in(["female", "male"])

#get patients of appropriate age for the cohort
if cohort == "infants" or cohort == "infants_subgroup" :
  is_appropriate_age = (age_at_start_months < 24) & (age_at_end_months >= 0)
elif cohort == "children_and_adolescents" :
  is_appropriate_age = (age_at_start < 18) & (age_at_end >= 2)
elif cohort == "adults" :
  is_appropriate_age = (age_at_start < 65) & (age_at_end >= 18)
else :
  is_appropriate_age = (age_at_start < 110) & (age_at_end >= 65)

#get patients who have information on IMD
has_imd = (addresses.for_patient_on(index_date).imd_rounded.is_not_null())

##define functions for queries

#events occurring before index date
prior_events = (
  clinical_events.where(clinical_events.date.is_on_or_before(index_date))
)

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
    .exists_for_patient()),
    otherwise = False)
  )

#care home resident - currently excluses anyone with a care home code
#or a care home flag
care_home_tpp = (
  addresses.for_patient_on(index_date)
  .care_home_is_potential_match.when_null_then(False)
)
care_home_code = (has_prior_event(codelists.carehome_codelist))
care_home = care_home_tpp | care_home_code

#presence of maternal linkage for infants subgroup 
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

#define first season for covid outcomes
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d").date()

##define function for outcome identification
def get_codes_dates(codelist_name, num_events, start_date, num_codes):

    # Dynamically get the codelist object
    pathogen_codelist = getattr(codelists, codelist_name)

    # Get all relevant RSV events sorted by date
    all_events = (
        clinical_events.where(
            clinical_events.date.is_on_or_between(start_date, followup_end_date)
        )
        .where(clinical_events.snomedct_code.is_in(pathogen_codelist))
        .sort_by(clinical_events.date)
    )

    # Get the first event
    event = all_events.first_for_patient()

    # # Use this as the default if we don't match any others
    # default_event = event

    # Start with an empty list of possible cases for the date and code
    date_cases = []
    code_cases = []

    # For the next three events ...
    for n in range(num_events):
        # Check if there are multiple distinct codes within 14 days
        events_in_date_window = all_events.where(
            all_events.date.is_on_or_between(event.date, event.date + days(14))
        )
        has_multiple_codes = (
            events_in_date_window.snomedct_code.count_distinct_for_patient() >= num_codes
        )
        # Append this event to the lists of cases
        if num_codes == 1:
          date_cases.append(event.date)
        else:
          date_cases.append(
            when(has_multiple_codes).then(event.date)
          )
        code_cases.append(
            when(has_multiple_codes).then(event.snomedct_code)
        )
        # Get the next event after this one and repeat
        event = all_events.where(
            all_events.date.is_after(event.date)
        ).first_for_patient()

    if num_codes != 1:
      codes_date = case(*date_cases, otherwise = None)
    code = case(*code_cases, otherwise = None)

    if num_codes == 1: 
      return(date_cases) 
    else:
      return(codes_date, code)

##define outcomes - rsv

#extract rsv primary care dates for 'sensitive' phenotype

#infant and infant subroup cohorts
if cohort == "infants" or cohort == "infants_subgroup" :
  
  #extract date of first episode - looking at when the exclusion criteria is
  #not met, and taking the minimum of the date of the first code in the RSV
  #primary codelist, the date of the first code in the RSV sensitive codelist,
  #the date of the first prescription in the RSV prescriptions codelist
  #if there is at least one code in the RSV sensitive codelist, the date of 
  #the first bronchiolitis attendance, or the date of the first wheeze attendance

  #first define inclusion from specific phenotype
  rsv_primary_spec = (
    minimum_of((first_infection_event(codelists.rsv_primary_codelist).date),
    ((emergency_care_attendances.where((emergency_care_attendances
    .diagnosis_01.is_in(codelists.bronchiolitis_attendance))
    |(emergency_care_attendances.diagnosis_02
    .is_in(codelists.bronchiolitis_attendance)))
    .where(emergency_care_attendances.arrival_date
    .is_on_or_between(index_date, followup_end_date)))
    .arrival_date.minimum_for_patient()))
  )

  if rsv_primary_spec is None :

    #get dates of events and corresponding codes 
    rsv_codes_date, rsv_code = (
        get_codes_dates("rsv_sensitive_codelist", 4, index_date, 2)
    )

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
        when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(rsv_codes_date - days(30),
        rsv_codes_date + days(30))).exists_for_patient()).then(True),
        when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(rsv_med_date - days(30),
        rsv_med_date + days(30))).exists_for_patient()).then(True),
        otherwise = False)
    )

    #define prescription inclusion - i.e. presence of a code from codelist with
    #relevant prescription - the presence of a code from the sensitive codelist
    #is required for inclusion 
    rsv_med_inclusion_date = (case(
        when(is_infection_event(codelists.rsv_sensitive_codelist)
        .where(clinical_events.date.is_on_or_between(rsv_med_date - days(14),
        rsv_med_date + days(14))).exists_for_patient())
        .then(minimum_of((is_infection_event(codelists
        .rsv_sensitive_codelist).where(clinical_events
        .date.is_on_or_between(rsv_med_date - days(14),
        rsv_med_date + days(14))).sort_by(clinical_events.date)
        .first_for_patient().date), (rsv_med_date))), otherwise = None)
    )
    
    #extract outcome date - prioritising inclusion from specific phenotype
    dataset.rsv_primary_date = (case(
        when(~rsv_exclusion_primary).then(
        minimum_of((rsv_codes_date), (rsv_med_inclusion_date),
        (emergency_care_diagnosis_matches(codelists
        .bronchiolitis_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient()), 
        ((emergency_care_diagnosis_matches(codelists.wheeze_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient())))),
        otherwise = None)
    )

  else : 
     
    dataset.rsv_primary_date = rsv_primary_spec

  #first get inclusion from specific phenotype
  rsv_primary_spec_second = (
    minimum_of((is_infection_event(codelists.rsv_primary_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .rsv_primary_date + days(14))).sort_by(clinical_events.date)
    .first_for_patient().date), ((emergency_care_attendances
    .where((emergency_care_attendances.diagnosis_01
    .is_in(codelists.bronchiolitis_attendance))
    |(emergency_care_attendances.diagnosis_02
    .is_in(codelists.bronchiolitis_attendance)))
    .where(emergency_care_attendances.arrival_date
    .is_on_or_between(dataset.rsv_primary_date + days(14),
    followup_end_date))).arrival_date.minimum_for_patient()))
  )

  if rsv_primary_spec_second is None :
  
    #get dates of events and corresponding codes 
    rsv_codes_second_date, rsv_code_second = (
        get_codes_dates("rsv_sensitive_codelist", 4,
                        dataset.rsv_primary_date + days(14), 2)
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
        .where(clinical_events.date.is_on_or_between(maximum_of(
        dataset.rsv_primary_date + days(14), rsv_codes_second_date - days(30)),
        rsv_codes_second_date + days(30))).exists_for_patient()).then(True),
        when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(maximum_of(
        dataset.rsv_primary_date + days(14), rsv_med_second_date - days(30)),
        rsv_med_second_date + days(30))).exists_for_patient())
        .then(True), otherwise = False)
    )
    
    #define prescription inclusion - i.e. presence of a code from codelist with
    #relevant prescription - the presence of a code from the sensitive codelist
    #is required for inclusion 
    rsv_med_inclusion_second_date = (case(
        when(is_infection_event(codelists.rsv_sensitive_codelist)
        .where(clinical_events.date.is_on_or_between(
        rsv_med_second_date - days(14), rsv_med_second_date + days(14)))
        .exists_for_patient()).then(minimum_of((is_infection_event(
        codelists.rsv_sensitive_codelist).where(clinical_events
        .date.is_on_or_between(maximum_of(rsv_med_second_date - days(14),
        dataset.rsv_primary_date + days(14)), rsv_med_second_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date),
        (rsv_med_second_date))), otherwise = None)
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.rsv_primary_second_date = (case(
        when(~rsv_exclusion_primary_second).then(
        minimum_of((rsv_codes_second_date),
        (rsv_med_inclusion_second_date),
        (emergency_care_diagnosis_matches(codelists
        .bronchiolitis_attendance).where(
        emergency_care_attendances.arrival_date
        .is_on_or_between(dataset.rsv_primary_date + days(14), 
        followup_end_date)).arrival_date.minimum_for_patient()),
        (emergency_care_diagnosis_matches(codelists.wheeze_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(dataset.rsv_primary_date + days(14),
        followup_end_date)).arrival_date.minimum_for_patient()))),
        otherwise = None)
    )

  else :
    
    dataset.rsv_primary_second_date = rsv_primary_spec_second
  
#all cohorts above the age of 2 
else :
  
  #extract date of first episode - looking at when the exclusion criteria is
  #not met, and taking the minimum of the date of the first code in the RSV
  #primary codelist, the date of the first code in the RSV sensitive codelist,
  #or the date of the first prescription in the RSV prescriptions codelist
  #when there is at least one code in the RSV sensitive codelist

  #first define inclusion from specific phenotype
  rsv_primary_spec = (
    first_infection_event(codelists
    .rsv_primary_codelist).date
  )

  if rsv_primary_spec is None :

    #get dates of events and corresponding codes 
    rsv_codes_date, rsv_code = (
        get_codes_dates("rsv_sensitive_codelist", 4, index_date, 2)
    )

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
        when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(rsv_codes_date - days(30),
        rsv_codes_date + days(30))).exists_for_patient()).then(True),
        when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(rsv_med_date - days(30),
        rsv_med_date + days(30))).exists_for_patient()).then(True),
        otherwise = False)
    )

    #define prescription inclusion - i.e. presence of a code from codelist with
    #relevant prescription - the presence of a code from the sensitive codelist
    #is required for inclusion 
    rsv_med_inclusion_date = (case(
        when(is_infection_event(codelists.rsv_sensitive_codelist)
        .where(clinical_events.date.is_on_or_between(rsv_med_date - days(14),
        rsv_med_date + days(14))).exists_for_patient())
        .then(minimum_of((is_infection_event(codelists
        .rsv_sensitive_codelist).where(clinical_events
        .date.is_on_or_between(rsv_med_date - days(14),
        rsv_med_date + days(14))).sort_by(clinical_events.date)
        .first_for_patient().date), (rsv_med_date))), otherwise = None)
    )

    #extract date - prioritising inclusion from specific phenotype
    dataset.rsv_primary_date = (case(
        when(~rsv_exclusion_primary).then(
        minimum_of((rsv_codes_date),
        (rsv_med_inclusion_date))),
        otherwise = None)
    )

  else : 
     
    dataset.rsv_primary_date = rsv_primary_spec
  
  #first get inclusion from specific phenotype
  rsv_primary_spec_second = (
    is_infection_event(codelists
    .rsv_primary_codelist).where(clinical_events
    .date.is_on_or_after(dataset.rsv_primary_date + days(14)))
    .sort_by(clinical_events.date)
    .first_for_patient().date
  )

  if rsv_primary_spec_second is None :

    #get dates of events and corresponding codes 
    rsv_codes_second_date, rsv_code_second = (
        get_codes_dates("rsv_sensitive_codelist", 4,
                        dataset.rsv_primary_date + days(14), 2)
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
        .where(clinical_events.date.is_on_or_between(maximum_of(
        dataset.rsv_primary_date + days(14), rsv_codes_second_date - days(30)),
        rsv_codes_second_date + days(30))).exists_for_patient()).then(True),
        when(is_infection_event(codelists.rsv_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(maximum_of(
        dataset.rsv_primary_date + days(14), rsv_med_second_date - days(30)),
        rsv_med_second_date + days(30))).exists_for_patient())
        .then(True), otherwise = False)
    )
    
    #define prescription inclusion - i.e. presence of a code from codelist with
    #relevant prescription - the presence of a code from the sensitive codelist
    #is required for inclusion 
    rsv_med_inclusion_second_date = (case(
        when(is_infection_event(codelists.rsv_sensitive_codelist)
        .where(clinical_events.date.is_on_or_between(
        rsv_med_second_date - days(14), rsv_med_second_date + days(14)))
        .exists_for_patient()).then(minimum_of((is_infection_event(
        codelists.rsv_sensitive_codelist).where(clinical_events
        .date.is_on_or_between(maximum_of(rsv_med_second_date - days(14),
        dataset.rsv_primary_date + days(14)), rsv_med_second_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date),
        (rsv_med_second_date))), otherwise = None)
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.rsv_primary_second_date = (case(
        when(~rsv_exclusion_primary_second).then(
        minimum_of((rsv_codes_second_date),
        (rsv_med_inclusion_second_date))),
        otherwise = None)
    )

  else :
     
    dataset.rsv_primary_second_date = rsv_primary_spec_second

#extract rsv secondary care dates for 'sensitive' phenotype

#first define inclusion from specific phenotype
rsv_secondary_spec = (
  apcs.sort_by(apcs.admission_date)
  .where((apcs.primary_diagnosis
  .is_in(codelists.rsv_secondary_codelist)) 
  |(apcs.secondary_diagnosis
  .is_in(codelists.rsv_secondary_codelist)))
  .where(apcs.admission_date.is_on_or_between(
  index_date, followup_end_date))
  .first_for_patient().admission_date
)

if rsv_secondary_spec is None :

  #get date of first diagnosis code (in any position) from the RSV sensitive 
  #secondary care codelist - looking at the first episode
  rsv_secondary_sens_date = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.unspecified_lrti)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.unspecified_lrti)))
    .where(apcs.admission_date.is_on_or_between(
    index_date, followup_end_date))
    .first_for_patient().admission_date
  )

  #get occurrence of event in exclusion list within one month of an occurrence 
  #of rsv_secondary_sens_date 
  rsv_exclusion_secondary = (case(
    when(apcs.sort_by(apcs.admission_date).where(
    (apcs.primary_diagnosis.is_in(codelists
    .rsv_secondary_exclusion_codelist)) 
    |(apcs.secondary_diagnosis.is_in(codelists
    .rsv_secondary_exclusion_codelist)))
    .where(apcs.admission_date.minimum_for_patient()
    .is_on_or_between(maximum_of(index_date,
    rsv_secondary_sens_date - days(30)),
    minimum_of(rsv_secondary_sens_date + days(30),
    followup_end_date))).exists_for_patient())
    .then(True), otherwise = False)
  )

  #extract date of first episode - looking at when the exclusion criteria is
  #not met and taking the value of rsv_secondary_sens_date

  #extract date - prioritising inclusion from specific phenotype
  dataset.rsv_secondary_date = (case(
    when(~rsv_exclusion_secondary)
    .then(rsv_secondary_sens_date),
    otherwise = None)
  )

  #get discharge date for first episode
  rsv_secondary_discharge = (
    apcs.sort_by(apcs.admission_date).where(
    (apcs.primary_diagnosis
    .is_in(codelists.unspecified_lrti)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.unspecified_lrti)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.rsv_secondary_date, dataset.rsv_secondary_date))
    .discharge_date.minimum_for_patient()
  )

else :
  
  dataset.rsv_secondary_date = rsv_secondary_spec

  #get discharge date for first episode
  rsv_secondary_discharge = (
    apcs.sort_by(apcs.admission_date).where(
    (apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.rsv_secondary_date, dataset.rsv_secondary_date))
    .discharge_date.minimum_for_patient()
  )

#extract length of stay for first episode, in hours
dataset.rsv_los = (
  diff_dates_hours(dataset.rsv_secondary_date,
  rsv_secondary_discharge)
)  

#first define inclusion from specific phenotype
rsv_secondary_spec_second = (
  apcs.sort_by(apcs.admission_date)
  .where((apcs.primary_diagnosis
  .is_in(codelists.rsv_secondary_codelist)) 
  |(apcs.secondary_diagnosis
  .is_in(codelists.rsv_secondary_codelist)))
  .where(apcs.admission_date.is_on_or_between(
  dataset.rsv_secondary_date + days(14), followup_end_date))
  .first_for_patient().admission_date
)

if rsv_secondary_spec_second is None :

  #get date of first diagnosis code (in any position) from the RSV sensitive 
  #secondary care codelist - looking at the second episode
  rsv_secondary_sens_second_date = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.unspecified_lrti)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.unspecified_lrti)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.rsv_secondary_date + days(14), followup_end_date))
    .admission_date.minimum_for_patient()
  )

  #get occurrence of event in exclusion list within one month of an occurrence 
  #of rsv_secondary_sens_second_date 
  rsv_exclusion_secondary_second = (case(
    when(apcs.sort_by(apcs.admission_date).where(
    (apcs.primary_diagnosis.is_in(codelists
    .rsv_secondary_exclusion_codelist)) 
    |(apcs.secondary_diagnosis.is_in(codelists
    .rsv_secondary_exclusion_codelist)))
    .where(apcs.admission_date.minimum_for_patient()
    .is_on_or_between(maximum_of(
    dataset.rsv_secondary_date + days(14),
    rsv_secondary_sens_second_date - days(30)),
    minimum_of(rsv_secondary_sens_second_date + days(30),
    followup_end_date))).exists_for_patient())
    .then(True), otherwise = False)
  )

  #extract date of second episode - using the same criteria as the first episode
  dataset.rsv_secondary_second_date = (case(
    when(~rsv_exclusion_secondary_second)
    .then(rsv_secondary_sens_second_date),
    otherwise = None)
  )

  #get discharge date for second episode
  rsv_secondary_discharge_second = (
    apcs.sort_by(apcs.admission_date).where(
    (apcs.primary_diagnosis
    .is_in(codelists.unspecified_lrti)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.unspecified_lrti)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.rsv_secondary_second_date,
    dataset.rsv_secondary_second_date))
    .discharge_date.minimum_for_patient()
  )

else :
   
  dataset.rsv_secondary_second_date = rsv_secondary_spec_second

  #get discharge date for second episode
  rsv_secondary_discharge_second = (
    apcs.sort_by(apcs.admission_date).where(
    (apcs.primary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)) 
    |(apcs.secondary_diagnosis
    .is_in(codelists.rsv_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.rsv_secondary_second_date,
    dataset.rsv_secondary_second_date))
    .discharge_date.minimum_for_patient()
  )

#extract length of stay for second episode, in hours
dataset.rsv_los_second = (
  diff_dates_hours(dataset.rsv_secondary_second_date,
  rsv_secondary_discharge_second)
)
  
##extract outcomes - flu

#extract flu primary care dates for 'sensitive' phenotype

#first define inclusion from specific phenotype
flu_primary_spec = (
  first_infection_event(codelists
  .flu_primary_codelist).date
)

if flu_primary_spec is None :

  #get date of first case of either ARI or fever for first episode
  ari_dates = (
    get_codes_dates("ari_primary_codelist", 4, index_date, 1)
  )
  fever_dates = (
    get_codes_dates("fever_codelist", 4, index_date, 1)
  )

  ILI_pairs = []
  ILI_date_cases = []

  for ari_date in ari_dates:
      for fever_date in fever_dates:
          close_in_time = diff_dates_days(ari_date, fever_date) <= abs(14)
          ILI_pairs.append(when(close_in_time).then(True))
          ILI_date_cases.append(when(close_in_time)
          .then(minimum_of(ari_date, fever_date)))

  ILI_case = case(*ILI_pairs, otherwise = False)
  ILI_date = case(*ILI_date_cases, otherwise = None)

  #get date of occurrence of first relevant prescription
  flu_med_date = (
    medications.where(medications.dmd_code.is_in(codelists
    .flu_prescriptions_codelist)).where(medications.date
    .is_on_or_between(index_date, followup_end_date))
    .date.minimum_for_patient()
  )

  #occurrence of event in exclusion list within one month of ILI
  flu_exclusion_primary = (case(
    when(is_infection_event(codelists.flu_primary_exclusion_codelist)
    .where(clinical_events.date.is_on_or_between(ILI_date - days(30),
    ILI_date + days(30))).exists_for_patient()).then(True),
    when(is_infection_event(codelists.flu_primary_exclusion_codelist)
    .where(clinical_events.date.is_on_or_between(flu_med_date - days(30),
    flu_med_date + days(30))).exists_for_patient()).then(True),
    otherwise = False)
  )

  #get date of first flu episode

  #then extract date - prioritising inclusion from specific phenotype
  dataset.flu_primary_date = (case(
    when(~flu_exclusion_primary).then(
    minimum_of((ILI_date), (flu_med_date))))
  )

else :
   
  dataset.flu_primary_date = flu_primary_spec

#first define inclusion from specific phenotype
flu_primary_spec_second = (
  is_infection_event(codelists
  .flu_primary_codelist).where(clinical_events
  .date.is_on_or_after(dataset.flu_primary_date + days(14)))
  .sort_by(clinical_events.date)
  .first_for_patient().date
)

if flu_primary_spec_second is None :

  #get date of first case of either ARI or fever for second episode
  ari_second_dates = (
    get_codes_dates("ari_primary_codelist", 4,
                    dataset.flu_primary_date + days(14), 1)
  )
  fever_second_dates = (
    get_codes_dates("fever_codelist", 4,
                    dataset.flu_primary_date + days(14), 1)
  )

  ILI_pairs_second = []
  ILI_second_date_cases = []

  for ari_date in ari_second_dates:
      for fever_date in fever_second_dates:
          close_in_time = diff_dates_days(ari_date, fever_date) <= abs(14)
          ILI_pairs_second.append(when(close_in_time).then(True))
          ILI_second_date_cases.append(when(close_in_time)
          .then(minimum_of(ari_date, fever_date)))

  ILI_case_second = case(*ILI_pairs_second, otherwise = False)
  ILI_second_date = case(*ILI_second_date_cases, otherwise = None)

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
    .where(clinical_events.date.is_on_or_between(maximum_of(
    dataset.flu_primary_date + days(14), ILI_second_date - days(30)),
    ILI_second_date + days(30))).exists_for_patient()).then(True),
    when(is_infection_event(codelists.flu_primary_exclusion_codelist)
    .where(clinical_events.date.is_on_or_between(maximum_of(
    dataset.flu_primary_date + days(14), flu_med_second_date - days(30)),
    flu_med_second_date + days(30))).exists_for_patient())
    .then(True), otherwise = False)
  )

  #get date of second flu episode

  #then extract date - prioritising inclusion from specific phenotype
  dataset.flu_primary_second_date = (case(
    when(~flu_exclusion_primary_second).then(
    minimum_of((ILI_second_date), (flu_med_second_date))),
    otherwise = None)
  )

else :
   
   dataset.flu_primary_second_date = flu_primary_spec_second
  
#extract flu secondary care dates for 'sensitive' phenotype

#first define inclusion from specific phenotype
flu_secondary_spec = (
  apcs.sort_by(apcs.admission_date)
  .where((apcs.primary_diagnosis
  .is_in(codelists.flu_secondary_codelist) )
  |(apcs.secondary_diagnosis
  .is_in(codelists.flu_secondary_codelist)))
  .where(apcs.admission_date.is_on_or_between(
  index_date, followup_end_date)).admission_date
  .minimum_for_patient()
)

if flu_secondary_spec is None :
   
  #get date of first diagnosis code (in any position) from the flu sensitive
  #secondary care codelist or ARI secondary care codelist - looking at the first episode
  flu_secondary_sens_date = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.ari_secondary_codelist) )
    |(apcs.secondary_diagnosis
    .is_in(codelists.ari_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    index_date, followup_end_date)).admission_date
    .minimum_for_patient()
  )

  #occurance of event in exclusion list within one month of flu_secondary_sens_date
  flu_exclusion_secondary = (case(
    when(apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_exclusion_codelist) )
    |(apcs.secondary_diagnosis.is_in(codelists
    .flu_secondary_exclusion_codelist)))
    .where(apcs.admission_date.minimum_for_patient()
    .is_on_or_between(maximum_of(index_date,
    flu_secondary_sens_date - days(30)), minimum_of(
    flu_secondary_sens_date + days(30), followup_end_date)))
    .exists_for_patient()).then(True),
    otherwise = False)
  )

  #extract date of first episode - looking at when the exclusion criteria is
  #not met

  #then extract date - prioritising inclusion from specific phenotype
  dataset.flu_secondary_date = (case(
    when(~flu_exclusion_secondary)
    .then(flu_secondary_sens_date),
    otherwise = None)
  )

  #get discharge date for first episode
  flu_secondary_discharge = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.ari_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.ari_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.flu_secondary_date, dataset
    .flu_secondary_date)).discharge_date
    .minimum_for_patient()
  )

else :
   
  dataset.flu_secondary_date = flu_secondary_spec

  #get discharge date for first episode
  flu_secondary_discharge = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.flu_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.flu_secondary_date, dataset
    .flu_secondary_date)).discharge_date
    .minimum_for_patient()
  )

#extract length of stay for first episode, in hours
dataset.flu_los = (
  diff_dates_hours(dataset.flu_secondary_date,
  flu_secondary_discharge)
)

#get date of first diagnosis code (in any position) from the flu sensitive 
#secondary care codelist - looking at the second episode

#first define inclusion from specific phenotype
flu_secondary_spec_second = (
  apcs.sort_by(apcs.admission_date)
  .where((apcs.primary_diagnosis
  .is_in(codelists.flu_secondary_codelist) )
  |(apcs.secondary_diagnosis
  .is_in(codelists.flu_secondary_codelist)))
  .where(apcs.admission_date.is_on_or_between(
  dataset.flu_secondary_date + days(14),
  followup_end_date)).admission_date
  .minimum_for_patient()
)

if flu_secondary_spec_second is None :

  #extract date - prioritising inclusion from specific phenotype
  flu_secondary_sens_second_date = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.ari_secondary_codelist) )
    |(apcs.secondary_diagnosis
    .is_in(codelists.ari_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.flu_secondary_date + days(14), followup_end_date))
    .admission_date.minimum_for_patient()
  )

  #get occurrence of event in exclusion list within one month of an occurrence 
  #of flu_secondary_sens_second_date
  flu_exclusion_secondary_second = (case(
    when(apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_exclusion_codelist))
    |(apcs.secondary_diagnosis.is_in(codelists
    .flu_secondary_exclusion_codelist))).where(
    apcs.admission_date.minimum_for_patient()
    .is_on_or_between(maximum_of(
    dataset.flu_secondary_date + days(14),
    flu_secondary_sens_second_date - days(30)),
    minimum_of(flu_secondary_sens_second_date + days(30),
    followup_end_date))).exists_for_patient())
    .then(True), otherwise = False)
  )

  #extract date of second episode - using the same criteria as the first episode
  dataset.flu_secondary_second_date = (case(
    when(~flu_exclusion_secondary_second)
    .then(flu_secondary_sens_second_date),
    otherwise = None)
  )

  #get discharge date for second episode
  flu_secondary_discharge_second = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.ari_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.ari_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.flu_secondary_second_date,
    dataset.flu_secondary_second_date))
    .discharge_date.minimum_for_patient()
  )

else :

  dataset.flu_secondary_second_date = flu_secondary_spec_second

  #get discharge date for second episode
  flu_secondary_discharge_second = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.flu_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.flu_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.flu_secondary_second_date,
    dataset.flu_secondary_second_date))
    .discharge_date.minimum_for_patient()
  )

#extract length of stay for second episode, in hours
dataset.flu_los_second = (
  diff_dates_hours(dataset.flu_secondary_second_date,
  flu_secondary_discharge_second)
)

##extract outcomes - covid

if study_start_date >= covid_season_min :
  
  #extract covid primary care dates for 'sensitive' phenotype 
  
  
  #first define inclusion from specific phenotype
  covid_primary_spec = (
    first_infection_event(codelists
    .covid_primary_codelist).date
  )
  
  if covid_primary_spec is None :

    #count number of clinical codes in covid symptom list which occur within 14 days
    #looking at the first episode
    
    #get dates of events and corresponding codes 
    covid_codes_date, covid_code = (
      get_codes_dates("covid_sensitive_codelist", 4, index_date, 2)
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
      when(is_infection_event(codelists.covid_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_between(covid_codes_date - days(30),
      covid_codes_date + days(30))).exists_for_patient()).then(True),
      when(is_infection_event(codelists.covid_primary_exclusion_codelist)
      .where(clinical_events.date.is_on_or_between(covid_med_date - days(30),
      covid_med_date + days(30))).exists_for_patient()).then(True),
      otherwise = False)
    )
    
    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a code in the covid primary codelist, 
    #a code in the covid sensitive codelist, or a prescription in the covid
    #prescriptions codelist

    #extract date - prioritising inclusion from specific phenotype
    dataset.covid_primary_date = (case(
      when(~covid_exclusion_primary).then(
      minimum_of((covid_codes_date), (covid_med_date))),
      otherwise = None)
    )

  else :

    dataset.covid_primary_date = covid_primary_spec
  
  #extract date of second episode - using the same criteria as the first episode
  
  #first define inclusion from specific phenotype
  covid_primary_spec_second = (
    is_infection_event(codelists.covid_primary_codelist)
    .where(clinical_events.date.is_on_or_after(dataset
    .covid_primary_date + days(14))).sort_by(clinical_events
    .date).first_for_patient().date
  )

  if covid_primary_spec_second is None :

    #count number of clinical codes in covid symptom list for second episode
    
    #get dates of events and corresponding codes 
    covid_codes_second_date, covid_code_second = (
      get_codes_dates("covid_sensitive_codelist", 4,
                      dataset.covid_primary_date + days(14), 2)
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
      .where(clinical_events.date.is_on_or_between(
      maximum_of(dataset.covid_primary_date + days(14),
      covid_codes_second_date - days(30)),
      covid_codes_second_date + days(30))).exists_for_patient())
      .then(True), when(is_infection_event(codelists
      .covid_primary_exclusion_codelist).where(clinical_events
      .date.is_on_or_between(maximum_of(dataset
      .covid_primary_date + days(14), covid_med_second_date - days(30)),
      covid_med_second_date + days(30))).exists_for_patient())
      .then(True), otherwise = False)
    )
    
    #then extract date - prioritising inclusion from specific phenotype
    dataset.covid_primary_second_date = (case(
      when(~covid_exclusion_primary_second).then(
      minimum_of((covid_codes_second_date),
      (covid_med_second_date))),
      otherwise = None)
    )

  else :

    dataset.covid_primary_second_date = covid_primary_spec_second

    
  #extract covid secondary care dates for 'sensitive' phenotype 
    
  #first define inclusion from specific phenotype
  covid_secondary_spec = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.covid_secondary_codelist))
    |(apcs.secondary_diagnosis
    .is_in(codelists.covid_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    index_date, followup_end_date)).admission_date
    .minimum_for_patient()
  )

  if covid_secondary_spec is None :
  
    #get date of first diagnosis code (in any position) from the covid sensitive
    #secondary care codelist - looking at the first episode
    covid_secondary_sens_date = (
      apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis
      .is_in(codelists.coronavirus_unspecified))
      |(apcs.secondary_diagnosis
      .is_in(codelists.coronavirus_unspecified)))
      .where(apcs.admission_date.is_on_or_between(index_date,
      followup_end_date)).admission_date.minimum_for_patient()
    )
    
    #get occurrence of event in exclusion list within one month of covid_secondary_sens_date
    #- looking at the first date for which there is a code in the covid secondary
    #exclusion codelist within one month before or after the date of covid_secondary_sens_date
    covid_exclusion_secondary = (case(
      when((apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis.is_in(codelists
      .covid_secondary_exclusion_codelist))
      |(apcs.secondary_diagnosis.is_in(codelists
      .covid_secondary_exclusion_codelist))).where(apcs
      .admission_date.minimum_for_patient()
      .is_on_or_between(maximum_of(index_date,
      covid_secondary_sens_date - days(30)),
      minimum_of(covid_secondary_sens_date + days(30),
      followup_end_date)))).exists_for_patient())
      .then(True), otherwise = False)
    )
    
    #extract date of first episode - looking at when the exclusion criteria is
    #not met 

    #extract date - prioritising inclusion from specific phenotype
    dataset.covid_secondary_date = (case(
      when(covid_secondary_spec.is_not_null())
      .then(covid_secondary_spec),
      when((covid_secondary_spec is None) &
      (~covid_exclusion_secondary))
      .then(covid_secondary_sens_date),
      otherwise = None)
    )
    
    #get discharge date for first episode
    covid_secondary_discharge = (
      apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis
      .is_in(codelists.coronavirus_unspecified))
      |(apcs.secondary_diagnosis
      .is_in(codelists.coronavirus_unspecified)))
      .where(apcs.admission_date.is_on_or_between(
      dataset.covid_secondary_date, dataset.covid_secondary_date))
      .discharge_date.minimum_for_patient()
    )

  else :
  
    dataset.covid_secondary_date = covid_secondary_spec

    #get discharge date for first episode
    covid_secondary_discharge = (
      apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      |(apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist)))
      .where(apcs.admission_date.is_on_or_between(
      dataset.covid_secondary_date, dataset.covid_secondary_date))
      .discharge_date.minimum_for_patient()
    )
  
  #extract length of stay for first episode, in hours
  dataset.covid_los = (
    diff_dates_hours(dataset.covid_secondary_date,
    covid_secondary_discharge)
  )

  #first define inclusion from specific phenotype
  covid_secondary_spec_second = (
    apcs.sort_by(apcs.admission_date)
    .where((apcs.primary_diagnosis
    .is_in(codelists.covid_secondary_codelist) )
    |(apcs.secondary_diagnosis
    .is_in(codelists.covid_secondary_codelist)))
    .where(apcs.admission_date.is_on_or_between(
    dataset.covid_secondary_date + days(14), followup_end_date))
    .admission_date.minimum_for_patient()
  )

  if covid_secondary_spec_second is None :
  
    #get date of first diagnosis code (in any position) from the covid sensitive
    #secondary care codelist - looking at the second episode
    covid_secondary_sens_second_date = (
      apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis
      .is_in(codelists.coronavirus_unspecified))
      |(apcs.secondary_diagnosis
      .is_in(codelists.coronavirus_unspecified)))
      .where(apcs.admission_date.is_on_or_between(
      dataset.covid_secondary_date + days(14), followup_end_date))
      .admission_date.minimum_for_patient()
    )
    
    #get occurrence of event in exclusion list within one month of an occurrence
    #of covid_secondary_sens_second_date
    covid_exclusion_secondary_second = (case(
      when((apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis.is_in(codelists
      .covid_secondary_exclusion_codelist))
      |(apcs.secondary_diagnosis.is_in(codelists
      .covid_secondary_exclusion_codelist))).where(apcs
      .admission_date.minimum_for_patient()
      .is_on_or_between(maximum_of(
      dataset.covid_secondary_date + days(14),
      covid_secondary_sens_second_date - days(30)),
      minimum_of(covid_secondary_sens_second_date + days(30),
      followup_end_date)))).exists_for_patient())
      .then(True), otherwise = False)
    )

    #extract date - prioritising inclusion from specific phenotype
    dataset.covid_secondary_second_date = (case(
      when(covid_secondary_spec_second.is_not_null())
      .then(covid_secondary_spec_second),
      when((covid_secondary_spec_second is None) &
      (~covid_exclusion_secondary_second))
      .then(covid_secondary_sens_second_date),
      otherwise = None)
    )
    
    #get discharge date for second episode
    covid_secondary_discharge_second = (
      apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis
      .is_in(codelists.coronavirus_unspecified))
      |(apcs.secondary_diagnosis
      .is_in(codelists.coronavirus_unspecified)))
      .where(apcs.admission_date.is_on_or_between(
      dataset.covid_secondary_second_date,
      dataset.covid_secondary_second_date))
      .discharge_date.minimum_for_patient()
    )

  else :
    
    dataset.covid_secondary_second_date = covid_secondary_spec_second

    #get discharge date for second episode
    covid_secondary_discharge_second = (
      apcs.sort_by(apcs.admission_date)
      .where((apcs.primary_diagnosis
      .is_in(codelists.covid_secondary_codelist))
      |(apcs.secondary_diagnosis
      .is_in(codelists.covid_secondary_codelist)))
      .where(apcs.admission_date.is_on_or_between(
      dataset.covid_secondary_second_date,
      dataset.covid_secondary_second_date))
      .discharge_date.minimum_for_patient()
    )
  
  #extract length of stay for second episode, in hours
  dataset.covid_los_second = (
    diff_dates_hours(dataset.covid_secondary_second_date,
    covid_secondary_discharge_second)
  )

##extract outcomes - unspecified respiratory infection

#older adults
if cohort == "older_adults" :
  
  #covid seasons
  if study_start_date >= covid_season_min :

    #prioritise pathogen specific outcomes first
    overall_resp_spec = (
      minimum_of((dataset.rsv_primary_date),
      (dataset.flu_primary_date), (dataset.covid_primary_date))
    )

    #extract unspecified respiratory infection primary care dates for 'sensitive' phenotype
    if overall_resp_spec is None :
    
      #count number of clinical codes in overall respiratory symptom list which occur within 14 days
      #get dates of events and corresponding codes 
      overall_resp_codes_date, overall_resp_code = (
        get_codes_dates("respiratory_virus_primary_codelist", 4, index_date, 2)
      )

      #occurrence of event in exclusion list within one month of overall_resp_codes_date
      # - looking at the first date for which there is a code in the respiratory virus
      #primary exclusion codelist within one month before or after the date of
      #overall_resp_codes_date
      overall_resp_exclusion_primary = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(
        overall_resp_codes_date - days(30),
        overall_resp_codes_date + days(30))).exists_for_patient())
        .then(True), otherwise = False)
      )
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV primary episode, flu primary episode,
      #covid primary episode, a code in the respiratory virus primary codelist,
      #a code in emergency care for a respiratory tract infection, a code in 
      #emergency care for a COPD exacerbation, a code in emergency care for an 
      #asthma exacerbation, a code in primary care for a COPD exacerbation,
      #or a code in primary care for an asthma exacerbation
      dataset.overall_resp_primary_date = (case(
        when(~overall_resp_exclusion_primary).then(
        minimum_of((emergency_care_diagnosis_matches(codelists
        .rtri_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(index_date,
        followup_end_date)).arrival_date.minimum_for_patient()),
        (emergency_care_diagnosis_matches(codelists
        .copd_exacerbation_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient()), (first_infection_event(
        codelists.copd_exacerbation_primary_codelist).date),
        (first_infection_event(codelists
        .asthma_exacerbation_primary_codelist).date))),
        otherwise = None)
      )

    else :

      dataset.overall_resp_primary_date = overall_resp_spec

    #prioritise pathogen specific outcomes first
    overall_resp_spec_second = (
      minimum_of((dataset.rsv_primary_second_date),
      (dataset.flu_primary_second_date), (dataset.covid_primary_second_date))
    )

    if overall_resp_spec_second is None :
    
      #count number of clinical codes in overall respiratory symptom list 
      # - for second episode and date of first occurrence of two of the
      # codes within 2 weeks - for second episode
      overall_resp_codes_second_date, overall_resp_code_second = (
        get_codes_dates("respiratory_virus_primary_codelist", 4,
                        dataset.overall_resp_primary_date + days(14), 2)
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
      # - using the same criteria as the first episode
      overall_resp_exclusion_primary_second = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(
        maximum_of(dataset.overall_resp_primary_date + days(14),
        overall_resp_codes_second_date - days(30)),
        overall_resp_codes_second_date + days(30)))
        .exists_for_patient()).then(True),
        otherwise = False)
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_primary_second_date = (case(
        when(~overall_resp_exclusion_primary_second).then(
        minimum_of((is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events
        .date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date),
        (overall_resp_codes_second_date), (emergency_care_diagnosis_matches(
        codelists.rtri_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(dataset
        .overall_resp_primary_date + days(14), followup_end_date))
        .arrival_date.minimum_for_patient()),
        (emergency_care_diagnosis_matches(codelists
        .copd_exacerbation_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(dataset.rsv_primary_date + days(14), 
        followup_end_date)).arrival_date.minimum_for_patient()),
        (is_infection_event(codelists.copd_exacerbation_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date),
        (is_infection_event(codelists.asthma_exacerbation_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date))),
        otherwise = None)
      )

    else :

      dataset.overall_resp_primary_second_date = overall_resp_spec_second
    
  #pre covid seasons  
  else:
    
    #prioritise pathogen specific outcomes first  
    overall_resp_spec = (
      minimum_of((dataset.rsv_primary_date),
      (dataset.flu_primary_date))
    )

    if overall_resp_spec is None :

      #count number of clinical codes in overall respiratory symptom list which occur within 14 days
      #get dates of events and corresponding codes 
      overall_resp_codes_date, overall_resp_code = (
        get_codes_dates("respiratory_virus_primary_codelist", 4, index_date, 2)
      )

      #occurrence of event in exclusion list within one month of overall_resp_codes_date
      # - looking at the first date for which there is a code in the respiratory virus
      #primary exclusion codelist within one month before or after the date of
      #overall_resp_codes_date
      overall_resp_exclusion_primary = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(
        overall_resp_codes_date - days(30),
        overall_resp_codes_date + days(30))).exists_for_patient())
        .then(True), otherwise = False)
      )
      
      #extract date of first episode - looking at the first date for which there is
      #a RSV primary episode, a flu primary episode, a code in the respiratory 
      #virus primary codelist, a code in emergency care for a respiratory tract
      #infection, a code in emergency care for a COPD exacerbation, a code in
      #emergency care for an asthma exacerbation, a code in primary care for a
      #COPD exacerbation, or a code in primary care for an asthma exacerbation
      dataset.overall_resp_primary_date = (case(
        when(~overall_resp_exclusion_primary).then(
        minimum_of((first_infection_event(codelists
        .respiratory_virus_primary_codelist).date),
        (emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient()),
        (emergency_care_diagnosis_matches(codelists
        .copd_exacerbation_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient()),
        (overall_resp_codes_date), (first_infection_event(
        codelists.copd_exacerbation_primary_codelist)
        .date), (first_infection_event(codelists
        .asthma_exacerbation_primary_codelist).date))),
        otherwise = None)
      )

    else :
      
      dataset.overall_resp_primary_date = overall_resp_spec

    #prioritise pathogen specific outcomes first   
    overall_resp_spec_second = (
      minimum_of((dataset.rsv_primary_second_date),
      (dataset.flu_primary_second_date))
    )

    if overall_resp_spec_second is None :

      #count number of clinical codes in overall respiratory symptom list 
      # - for second episode and date of first occurrence of two of the
      # codes within 2 weeks - for second episode
      overall_resp_codes_second_date, overall_resp_code_second = (
        get_codes_dates("respiratory_virus_primary_codelist", 4,
                        dataset.overall_resp_primary_date + days(14), 2)
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
      # - using the same criteria as the first episode
      overall_resp_exclusion_primary_second = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(
        maximum_of(dataset.overall_resp_primary_date + days(14),
        overall_resp_codes_second_date - days(30)),
        overall_resp_codes_second_date + days(30)))
        .exists_for_patient()).then(True),
        otherwise = False)
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_primary_second_date = (case(
        when(~overall_resp_exclusion_primary_second).then(
        minimum_of((is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events
        .date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date),
        (emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date.is_on_or_between(
        dataset.overall_resp_primary_date + days(14), followup_end_date))
        .arrival_date.minimum_for_patient()),
        (emergency_care_diagnosis_matches(codelists
        .copd_exacerbation_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(dataset.rsv_primary_date + days(14), 
        followup_end_date)).arrival_date.minimum_for_patient()),
        (overall_resp_codes_second_date), (is_infection_event(
        codelists.copd_exacerbation_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date),
        (is_infection_event(codelists.asthma_exacerbation_primary_codelist)
        .where(clinical_events.date.is_on_or_after(dataset
        .overall_resp_primary_date + days(14))).sort_by(clinical_events
        .date).first_for_patient().date))),
        otherwise = None)
      )

    else :

      dataset.overall_resp_primary_second_date = overall_resp_spec_second
    
#cohorts that are not older adults    
else:
  
  #covid seasons
  if study_start_date >= covid_season_min :

    #prioritise pathogen specific outcomes first
    overall_resp_spec = (
      minimum_of((dataset.rsv_primary_date),
      (dataset.flu_primary_date), (dataset.covid_primary_date))
    )

    if overall_resp_spec is None :

      #count number of clinical codes in overall respiratory symptom list which occur within 14 days
      #get dates of events and corresponding codes 
      overall_resp_codes_date, overall_resp_code = (
        get_codes_dates("respiratory_virus_primary_codelist", 4, index_date, 2)
      )

      #occurrence of event in exclusion list within one month of overall_resp_codes_date
      # - looking at the first date for which there is a code in the respiratory virus
      #primary exclusion codelist within one month before or after the date of
      #overall_resp_codes_date
      overall_resp_exclusion_primary = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(
        overall_resp_codes_date - days(30),
        overall_resp_codes_date + days(30))).exists_for_patient())
        .then(True), otherwise = False)
      )
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV primary episode, flu primary episode,
      #covid primary episode, a code in the respiratory virus primary codelist,
      #or a code in emergency care for a respiratory tract infection
      dataset.overall_resp_primary_date = (case(
        when(~overall_resp_exclusion_primary).then(
        minimum_of(first_infection_event(codelists
        .respiratory_virus_primary_codelist).date, 
        emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient())),
        otherwise = None)
      )

    else :

      dataset.overall_resp_primary_date = overall_resp_spec

    #prioritise pathogen specific outcomes first
    overall_resp_spec_second = (
      minimum_of((dataset.rsv_primary_second_date),
      (dataset.flu_primary_second_date), (dataset.covid_primary_second_date))
    )

    if overall_resp_spec_second is None :
    
      #count number of clinical codes in overall respiratory symptom list 
      # - for second episode and date of first occurrence of two of the
      # codes within 2 weeks - for second episode
      overall_resp_codes_second_date, overall_resp_code_second = (
        get_codes_dates("respiratory_virus_primary_codelist", 4,
                        dataset.overall_resp_primary_date + days(14), 2)
      )
      #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
      # - using the same criteria as the first episode
      overall_resp_exclusion_primary_second = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(
        maximum_of(dataset.overall_resp_primary_date + days(14),
        overall_resp_codes_second_date - days(30)),
        overall_resp_codes_second_date + days(30)))
        .exists_for_patient()).then(True),
        otherwise = False)
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_primary_second_date = (case(
        when(~overall_resp_exclusion_primary_second).then(
        minimum_of((is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events
        .date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date),
        (overall_resp_codes_second_date), (emergency_care_diagnosis_matches(
        codelists.rtri_attendance).where(emergency_care_attendances
        .arrival_date.is_on_or_between(dataset
        .overall_resp_primary_date + days(14), followup_end_date))
        .arrival_date.minimum_for_patient()))),
        otherwise = None)
      )

    else :

      dataset.overall_resp_primary_second_date = overall_resp_spec_second
    
  #pre-covid seasons  
  else:

    #prioritise pathogen specific outcomes first
    overall_resp_spec = (
      minimum_of((dataset.rsv_primary_date),
      (dataset.flu_primary_date))
    )

    if overall_resp_spec is None :

      #count number of clinical codes in overall respiratory symptom list which occur within 14 days
      #get dates of events and corresponding codes 
      overall_resp_codes_date, overall_resp_code = (
        get_codes_dates("respiratory_virus_primary_codelist", 4, index_date, 2)
      )

      #occurrence of event in exclusion list within one month of overall_resp_codes_date
      # - looking at the first date for which there is a code in the respiratory virus
      #primary exclusion codelist within one month before or after the date of
      #overall_resp_codes_date
      overall_resp_exclusion_primary = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(
        overall_resp_codes_date - days(30),
        overall_resp_codes_date + days(30))).exists_for_patient())
        .then(True), otherwise = False)
      )
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV primary episode, flu primary episode,
      #a code in the respiratory virus primary codelist, or a code in emergency care
      #for a respiratory tract infection
      dataset.overall_resp_primary_date = (case(
        when(~overall_resp_exclusion_primary).then(
        minimum_of(first_infection_event(codelists
        .respiratory_virus_primary_codelist).date,
        emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date
        .is_on_or_between(index_date, followup_end_date))
        .arrival_date.minimum_for_patient())),
        otherwise = None)
      )

    else :
    
      dataset.overall_resp_primary_date = overall_resp_spec

    #prioritise pathogen specific outcomes first   
    overall_resp_spec_second = (
      minimum_of((dataset.rsv_primary_second_date),
      (dataset.flu_primary_second_date))
    )

    if overall_resp_spec_second is None :
    
      #count number of clinical codes in overall respiratory symptom list 
      # - for second episode and date of first occurrence of two of the
      # codes within 2 weeks - for second episode
      overall_resp_codes_second_date, overall_resp_code_second = (
        get_codes_dates("respiratory_virus_primary_codelist", 4,
                        dataset.overall_resp_primary_date + days(14), 2)
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
      overall_resp_exclusion_primary_second = (case(
        when(is_infection_event(codelists
        .respiratory_virus_primary_exclusion_codelist)
        .where(clinical_events.date.is_on_or_between(
        maximum_of(dataset.overall_resp_primary_date + days(14),
        overall_resp_codes_second_date - days(30)),
        overall_resp_codes_second_date + days(30)))
        .exists_for_patient()).then(True),
        otherwise = False)
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_primary_second_date = (case(
        when(~overall_resp_exclusion_primary_second).then(
        minimum_of((is_infection_event(codelists
        .respiratory_virus_primary_codelist).where(clinical_events
        .date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
        .sort_by(clinical_events.date).first_for_patient().date),
        (overall_resp_codes_second_date),
        (emergency_care_diagnosis_matches(codelists.rtri_attendance)
        .where(emergency_care_attendances.arrival_date.is_on_or_between(
        dataset.overall_resp_primary_date + days(14), followup_end_date))
        .arrival_date.minimum_for_patient()))),
        otherwise = None)
      )

    else :

      dataset.overall_resp_primary_second_date = overall_resp_spec_second

#extract unspecified respiratory infection secondary care dates for 'sensitive' phenotype 

#older adults
if cohort == "older_adults" :
  
  #covid seasons
  if study_start_date >= covid_season_min :

    #prioritise pathogen specific outcomes first
    overall_resp_secondary_spec = (
      minimum_of(dataset.rsv_secondary_date,
      dataset.flu_secondary_date, dataset.covid_secondary_date)
    )

    if overall_resp_secondary_spec is None :

      #extract date of first episode - looking at the first date for which there is
      #a code in the respiratory virus secondary codelist (in any position), a 
      #code in the COPD exacerbation secondary codelist (in any position), or a 
      #code in the asthma exacerbation secondary codelist (in any position)
      overall_resp_secondary_sens_date = (
        minimum_of((apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        index_date, followup_end_date)).admission_date
        .minimum_for_patient()), (apcs.sort_by(apcs
        .admission_date).where((apcs.primary_diagnosis
        .is_in(codelists.copd_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .copd_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        index_date, followup_end_date)).admission_date
        .minimum_for_patient()), (apcs.sort_by(apcs
        .admission_date).where((apcs.primary_diagnosis
        .is_in(codelists.asthma_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .asthma_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        index_date, followup_end_date)).admission_date
        .minimum_for_patient()))
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_secondary_sens_date
      # - looking at the first date for which there is a code in the respiratory virus
      #secondary exclusion codelist within one month before or after the date of
      #overall_resp_secondary_sens_date
      overall_resp_exclusion_secondary = (case(
        when(apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        maximum_of(index_date, overall_resp_secondary_sens_date - days(30)),
        minimum_of(overall_resp_secondary_sens_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)
      )
    
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV secondary episode, flu secondary episode,
      #covid secondary episode or overall_resp_secondary_sens_date
      dataset.overall_resp_secondary_date = (case(
        when(~overall_resp_exclusion_secondary).then(
        overall_resp_secondary_sens_date),
        otherwise = None)
      ) 
      
      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
        .then(apcs.sort_by(apcs.admission_date).where(
        (apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.primary_diagnosis
        .is_in(codelists.copd_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .copd_exacerbation_secondary_codelist))
        |(apcs.primary_diagnosis
        .is_in(codelists.asthma_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .asthma_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date, dataset
        .overall_resp_secondary_date)).first_for_patient()
        .discharge_date), otherwise = None)
      )

    else :

      dataset.overall_resp_secondary_date = overall_resp_secondary_spec

      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(dataset.overall_resp_secondary_date == dataset
        .rsv_secondary_date).then(rsv_secondary_discharge), 
        when(dataset.overall_resp_secondary_date == dataset
        .flu_secondary_date).then(flu_secondary_discharge),
        when(dataset.overall_resp_secondary_date == dataset
        .covid_secondary_date).then(covid_secondary_discharge),
        otherwise = None)
      )

    #prioritise pathogen specific outcomes first
    overall_resp_secondary_spec_second = (
      minimum_of(dataset.rsv_secondary_second_date,
      dataset.flu_secondary_second_date,
      dataset.covid_secondary_second_date)
    )

    if overall_resp_secondary_spec_second is None :
    
      #extract length of stay for first episode, in hours
      dataset.overall_resp_los = (
        diff_dates_hours(dataset.overall_resp_secondary_date,
        overall_resp_secondary_discharge)
      ) 
      
      #extract date of second episode - using the same criteria as the first episode
      overall_resp_secondary_sens_second_date = (
        minimum_of((apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date
        .minimum_for_patient()), (apcs.sort_by(apcs
        .admission_date).where((apcs.primary_diagnosis
        .is_in(codelists.copd_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .copd_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date
        .minimum_for_patient()), (apcs.sort_by(apcs
        .admission_date).where((apcs.primary_diagnosis
        .is_in(codelists.asthma_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .asthma_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date
        .minimum_for_patient()))
      )
      
      #occurrence of event in exclusion list within one month of an occurrence
      #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
      overall_resp_exclusion_secondary_second = (case(
        when(apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist)))
        .where(apcs.admission_date.is_on_or_between(maximum_of(
        dataset.overall_resp_secondary_date + days(14),
        overall_resp_secondary_sens_second_date - days(30)),
        minimum_of(overall_resp_secondary_sens_second_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)  
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_secondary_second_date = (case(
        when(~overall_resp_exclusion_secondary_second).then(
        overall_resp_secondary_sens_second_date),
        otherwise = None)
      ) 
      
      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(dataset.overall_resp_secondary_second_date == 
        overall_resp_secondary_sens_second_date)
        .then(apcs.sort_by(apcs.admission_date).where(
        (apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.primary_diagnosis
        .is_in(codelists.copd_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .copd_exacerbation_secondary_codelist))
        |(apcs.primary_diagnosis
        .is_in(codelists.asthma_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .asthma_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_second_date, dataset
        .overall_resp_secondary_second_date)).first_for_patient()
        .discharge_date), otherwise = None)
      )

    else :
    
      dataset.overall_resp_secondary_second_date = overall_resp_secondary_spec_second

      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(dataset.overall_resp_secondary_second_date == dataset
        .rsv_secondary_second_date).then(rsv_secondary_discharge_second), 
        when(dataset.overall_resp_secondary_second_date == dataset
        .flu_secondary_second_date).then(flu_secondary_discharge_second),
        when(dataset.overall_resp_secondary_second_date == dataset
        .covid_secondary_second_date).then(covid_secondary_discharge_second),
        otherwise = None)
      )

    #extract length of stay for second episode, in hours
    dataset.overall_resp_los_second = (
      diff_dates_hours(dataset.overall_resp_secondary_second_date,
      overall_resp_secondary_discharge_second)
    ) 
    
  #pre-covid seasons  
  else:

    #prioritise pathogen specific outcomes first
    overall_resp_secondary_spec = (
      minimum_of(dataset.rsv_secondary_date,
      dataset.flu_secondary_date)
    )

    if overall_resp_secondary_spec is None :

      #extract date of first episode - looking at the first date for which there is
      #a code in the respiratory virus secondary codelist (in any position), a 
      #code in the COPD exacerbation secondary codelist (in any position), or a 
      #code in the asthma exacerbation secondary codelist (in any position)
      overall_resp_secondary_sens_date = (
        minimum_of((apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        index_date, followup_end_date)).admission_date
        .minimum_for_patient()), (apcs.sort_by(apcs
        .admission_date).where((apcs.primary_diagnosis
        .is_in(codelists.copd_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .copd_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        index_date, followup_end_date)).admission_date
        .minimum_for_patient()), (apcs.sort_by(apcs
        .admission_date).where((apcs.primary_diagnosis
        .is_in(codelists.asthma_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .asthma_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        index_date, followup_end_date)).admission_date
        .minimum_for_patient()))
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_secondary_sens_date
      # - looking at the first date for which there is a code in the respiratory virus
      #secondary exclusion codelist within one month before or after the date of
      #overall_resp_secondary_sens_date
      overall_resp_exclusion_secondary = (case(
        when(apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        maximum_of(index_date, overall_resp_secondary_sens_date - days(30)),
        minimum_of(overall_resp_secondary_sens_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)
      )
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV secondary episode, flu secondary episode,
      #or overall_resp_secondary_sens_date
      dataset.overall_resp_secondary_date = (case(
        when(~overall_resp_exclusion_secondary).then(
        overall_resp_secondary_sens_date),
        otherwise = None)
      ) 
      
      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
        .then(apcs.sort_by(apcs.admission_date).where(
        (apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.primary_diagnosis
        .is_in(codelists.copd_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .copd_exacerbation_secondary_codelist))
        |(apcs.primary_diagnosis
        .is_in(codelists.asthma_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .asthma_exacerbation_secondary_codelist)))
        .where(apcs.discharge_date.is_on_or_between(dataset
        .overall_resp_secondary_date, dataset
        .overall_resp_secondary_date)).first_for_patient()
        .discharge_date), otherwise = None)
      )

    else :

      dataset.overall_resp_secondary_date = overall_resp_secondary_spec

      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(dataset.overall_resp_secondary_date == dataset
        .rsv_secondary_date).then(rsv_secondary_discharge), 
        when(dataset.overall_resp_secondary_date == dataset
        .flu_secondary_date).then(flu_secondary_discharge),
        otherwise = None)
      )

    #prioritise pathogen specific outcomes first
    overall_resp_secondary_spec_second = (
      minimum_of(dataset.rsv_secondary_second_date,
      dataset.flu_secondary_second_date)
    )

    if overall_resp_secondary_spec_second is None :
    
      #extract length of stay for first episode, in hours
      dataset.overall_resp_los = (
        diff_dates_hours(dataset.overall_resp_secondary_date,
        overall_resp_secondary_discharge)
      ) 
      
      #extract date of second episode - using the same criteria as the first episode
      overall_resp_secondary_sens_second_date = (
        minimum_of((apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date
        .minimum_for_patient()), (apcs.sort_by(apcs
        .admission_date).where((apcs.primary_diagnosis
        .is_in(codelists.copd_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .copd_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date
        .minimum_for_patient()), (apcs.sort_by(apcs
        .admission_date).where((apcs.primary_diagnosis
        .is_in(codelists.asthma_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .asthma_exacerbation_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        dataset.overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date
        .minimum_for_patient()))
      )
      
      #occurrence of event in exclusion list within one month of an occurrence
      #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
      overall_resp_exclusion_secondary_second = (case(
        when(apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist)))
        .where(apcs.admission_date.is_on_or_between(maximum_of(
        dataset.overall_resp_secondary_date + days(14),
        overall_resp_secondary_sens_second_date - days(30)),
        minimum_of(overall_resp_secondary_sens_second_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)  
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_secondary_second_date = (case(
        when(~overall_resp_exclusion_secondary).then(
        overall_resp_secondary_sens_date),
        otherwise = None)
      ) 
      
      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(dataset.overall_resp_secondary_second_date == 
        overall_resp_secondary_sens_second_date)
        .then(apcs.sort_by(apcs.admission_date).where(
        (apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.primary_diagnosis
        .is_in(codelists.copd_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .copd_exacerbation_secondary_codelist))
        |(apcs.primary_diagnosis
        .is_in(codelists.asthma_exacerbation_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .asthma_exacerbation_secondary_codelist)))
        .where(apcs.discharge_date.is_on_or_between(dataset
        .overall_resp_secondary_second_date, dataset
        .overall_resp_secondary_second_date)).first_for_patient()
        .discharge_date), otherwise = None)
      )

    else :

      dataset.overall_resp_secondary_second_date = overall_resp_secondary_spec_second

      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(dataset.overall_resp_secondary_second_date == dataset
        .rsv_secondary_second_date).then(rsv_secondary_discharge_second), 
        when(dataset.overall_resp_secondary_second_date == dataset
        .flu_secondary_second_date).then(flu_secondary_discharge_second),
        otherwise = None)
      )
    
    #extract length of stay for second episode, in hours
    dataset.overall_resp_los_second = (
      diff_dates_hours(dataset.overall_resp_secondary_second_date,
      overall_resp_secondary_discharge_second)
    ) 
    
#cohorts that are not older adults   
else:
  
  #covid seasons
  if study_start_date >= covid_season_min :

    #prioritise pathogen specific outcomes first
    overall_resp_secondary_spec = (
      minimum_of(dataset.rsv_secondary_date,
      dataset.flu_secondary_date, dataset.covid_secondary_date)
    )

    if overall_resp_secondary_spec is None :

      #extract date of first episode - looking at the first date for which there is
      #a code in the respiratory virus secondary codelist (in any position)
      overall_resp_secondary_sens_date = (
        apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        index_date, followup_end_date)).admission_date
        .minimum_for_patient()
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_secondary_sens_date
      # - looking at the first date for which there is a code in the respiratory virus
      #secondary exclusion codelist within one month before or after the date of
      #overall_resp_secondary_sens_date
      overall_resp_exclusion_secondary = (case(
        when(apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        maximum_of(index_date, overall_resp_secondary_sens_date - days(30)),
        minimum_of(overall_resp_secondary_sens_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)
      )

      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV secondary episode, flu secondary episode,
      #covid secondary episode or overall_resp_secondary_sens_date
      dataset.overall_resp_secondary_date = (case(
        when(~overall_resp_exclusion_secondary).then(
        overall_resp_secondary_sens_date),
        otherwise = None)
      )
      
      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
        .then(apcs.sort_by(apcs.admission_date).where(   
        (apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date, dataset
        .overall_resp_secondary_date)).first_for_patient()
        .discharge_date), otherwise = None)
      )

    else :

      dataset.overall_resp_secondary_date = overall_resp_secondary_spec

      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(dataset.overall_resp_secondary_date == dataset
        .rsv_secondary_date).then(rsv_secondary_discharge), 
        when(dataset.overall_resp_secondary_date == dataset
        .flu_secondary_date).then(flu_secondary_discharge),
        when(dataset.overall_resp_secondary_date == dataset
        .covid_secondary_date).then(covid_secondary_discharge),
        otherwise = None)
      )

    #extract length of stay for first episode, in hours
    dataset.overall_resp_los = (
      diff_dates_hours(dataset.overall_resp_secondary_date,
      overall_resp_secondary_discharge)
    ) 

    #prioritise pathogen specific outcomes first
    overall_resp_secondary_spec_second = (
      minimum_of(dataset.rsv_secondary_second_date,
      dataset.flu_secondary_second_date,
      dataset.covid_secondary_second_date)
    )

    if overall_resp_secondary_spec_second is None :
    
      #extract date of second episode - using the same criteria as the first episode
      overall_resp_secondary_sens_second_date = (
        apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date.minimum_for_patient()
      )
      
      #occurrence of event in exclusion list within one month of an occurrence
      #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
      overall_resp_exclusion_secondary_second = (case(
        when(apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist)))
        .where(apcs.admission_date.is_on_or_between(maximum_of(
        dataset.overall_resp_secondary_date + days(14),
        overall_resp_secondary_sens_second_date - days(30)),
        minimum_of(overall_resp_secondary_sens_second_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)  
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_secondary_second_date = (case(
        when(~overall_resp_exclusion_secondary_second).then(
        overall_resp_secondary_sens_second_date),
        otherwise = None)
      )
      
      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(dataset.overall_resp_secondary_second_date == overall_resp_secondary_sens_second_date)
        .then(apcs.sort_by(apcs.admission_date).where(   
        (apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_second_date, dataset
        .overall_resp_secondary_second_date)).first_for_patient()
        .discharge_date), otherwise = None)
      )

    else :

      dataset.overall_resp_secondary_second_date = overall_resp_secondary_spec_second

      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(dataset.overall_resp_secondary_second_date == dataset
        .rsv_secondary_second_date).then(rsv_secondary_discharge_second), 
        when(dataset.overall_resp_secondary_second_date == dataset
        .flu_secondary_second_date).then(flu_secondary_discharge_second),
        when(dataset.overall_resp_secondary_second_date == dataset
        .covid_secondary_second_date).then(covid_secondary_discharge_second),
        otherwise = None)
      )
    
    #extract length of stay for second episode, in hours
    dataset.overall_resp_los_second = (
      diff_dates_hours(dataset.overall_resp_secondary_second_date,
      overall_resp_secondary_discharge_second)
    ) 
    
  #pre-covid seasons  
  else:

    #prioritise pathogen specific outcomes first
    overall_resp_secondary_spec = (
      minimum_of(dataset.rsv_secondary_date,
      dataset.flu_secondary_date)
    )
  
    if overall_resp_secondary_spec is None :

      #extract date of first episode - looking at the first date for which there is
      #a code in the respiratory virus secondary codelist (in any position)
      overall_resp_secondary_sens_date = (
        apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        index_date, followup_end_date)).admission_date
        .minimum_for_patient()
      )
      
      #occurrence of event in exclusion list within one month of overall_resp_secondary_sens_date
      # - looking at the first date for which there is a code in the respiratory virus
      #secondary exclusion codelist within one month before or after the date of
      #overall_resp_secondary_sens_date
      overall_resp_exclusion_secondary = (case(
        when(apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist)))
        .where(apcs.admission_date.is_on_or_between(
        maximum_of(index_date, overall_resp_secondary_sens_date - days(30)),
        minimum_of(overall_resp_secondary_sens_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)
      )
      
      #extract date of first episode where the exclusion criteria is not met
      # - get the first date of either a RSV secondary episode, flu secondary episode,
      #or overall_resp_secondary_sens_date
      dataset.overall_resp_secondary_date = (case(
        when((dataset.rsv_secondary_date.is_not_null())
        |(dataset.flu_secondary_date.is_not_null()))
        .then(minimum_of(dataset.rsv_secondary_date,
        dataset.flu_secondary_date)), when((dataset
        .rsv_secondary_date.is_null()) &
        (dataset.flu_secondary_date.is_null()) &
        (~overall_resp_exclusion_secondary)).then(
        overall_resp_secondary_sens_date),
        otherwise = None)
      )
      
      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
        .then(apcs.sort_by(apcs.admission_date).where(   
        (apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date, dataset
        .overall_resp_secondary_date)).first_for_patient()
        .discharge_date), otherwise = None)
      )

    else :

      dataset.overall_resp_secondary_date = overall_resp_secondary_spec

      #get discharge date for first episode
      overall_resp_secondary_discharge = (case(
        when(dataset.overall_resp_secondary_date == dataset
        .rsv_secondary_date).then(rsv_secondary_discharge), 
        when(dataset.overall_resp_secondary_date == dataset
        .flu_secondary_date).then(flu_secondary_discharge),
        otherwise = None)
      )
    
    #extract length of stay for first episode, in hours
    dataset.overall_resp_los = (
      diff_dates_hours(dataset.overall_resp_secondary_date,
      overall_resp_secondary_discharge)
    )

    #prioritise pathogen specific outcomes first
    overall_resp_secondary_spec_second = (
      minimum_of(dataset.rsv_secondary_second_date,
      dataset.flu_secondary_second_date)
    )
    
    if overall_resp_secondary_spec_second is None :

      #extract date of second episode - using the same criteria as the first episode
      overall_resp_secondary_sens_second_date = (
        apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_date + days(14),
        followup_end_date)).admission_date.minimum_for_patient()
      )
      
      #occurrence of event in exclusion list within one month of an occurrence
      #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
      overall_resp_exclusion_secondary_second = (case(
        when(apcs.sort_by(apcs.admission_date)
        .where((apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_exclusion_codelist)))
        .where(apcs.admission_date.is_on_or_between(maximum_of(
        dataset.overall_resp_secondary_date + days(14),
        overall_resp_secondary_sens_second_date - days(30)),
        minimum_of(overall_resp_secondary_sens_second_date + days(30),
        followup_end_date))).exists_for_patient())
        .then(True), otherwise = False)  
      )
      
      #extract date of second episode - using the same criteria as the first episode
      dataset.overall_resp_secondary_second_date = (case(
        when((dataset.rsv_secondary_second_date.is_not_null())
        |(dataset.flu_secondary_second_date.is_not_null()))
        .then(minimum_of(dataset.rsv_secondary_second_date,
        dataset.flu_secondary_second_date)),
        when((dataset.rsv_secondary_second_date.is_null()) &
        (dataset.flu_secondary_second_date.is_null()) &
        (~overall_resp_exclusion_secondary_second))
        .then(overall_resp_secondary_sens_second_date),
        otherwise = None)
      ) 
      
      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(dataset.overall_resp_secondary_second_date == overall_resp_secondary_sens_second_date)
        .then(apcs.sort_by(apcs.admission_date).where(   
        (apcs.primary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist))
        |(apcs.secondary_diagnosis.is_in(codelists
        .respiratory_virus_secondary_codelist)))
        .where(apcs.admission_date.is_on_or_between(dataset
        .overall_resp_secondary_second_date, dataset
        .overall_resp_secondary_second_date)).first_for_patient()
        .discharge_date), otherwise = None)
      )

    else :

      dataset.overall_resp_secondary_second_date = overall_resp_secondary_spec_second

      #get discharge date for second episode
      overall_resp_secondary_discharge_second = (case(
        when(dataset.overall_resp_secondary_second_date == dataset
        .rsv_secondary_second_date).then(rsv_secondary_discharge_second), 
        when(dataset.overall_resp_secondary_second_date == dataset
        .flu_secondary_second_date).then(flu_secondary_discharge_second),
        otherwise = None)
      )
    
    #extract length of stay for second episode, in hours
    dataset.overall_resp_los_second = (
      diff_dates_hours(dataset.overall_resp_secondary_second_date,
      overall_resp_secondary_discharge_second)
    ) 
    