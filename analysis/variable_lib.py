import operator
from functools import reduce
import json, sys
from pathlib import Path
from datetime import datetime

from ehrql.codes import ICD10Code
from ehrql import case, days, when, years, months, maximum_of, minimum_of
from ehrql.tables.tpp import (
    emergency_care_attendances, 
    apcs,
    clinical_events,
    patients,
    ons_deaths,
    practice_registrations,
    medications
)

import codelists

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

#gp events occuring after index date but before end of follow up
gp_events = (
  clinical_events.where(clinical_events.date
  .is_on_or_between(index_date, followup_end_date))
)

#query gp_events for existence of event-in-codelist 
def is_gp_event(codelist, where = True):
    return (
        gp_events.where(where)
        .where(gp_events.snomedct_code.is_in(codelist))
    )

#query gp_events for existence of event-in-codelist (get first of these)
def has_gp_event(codelist, where = True):
    return (
        gp_events.where(where)
        .where(gp_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .exists_for_patient()
    )

#query gp_events for date of most recent event-in-codelist
def last_gp_event(codelist, where = True):
    return (
        gp_events.where(where)
        .where(gp_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )
  
#query gp_events for date of earliest event-in-codelist
def first_gp_event(codelist, where = True):
    return (
        gp_events.where(where)
        .where(gp_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

#hospital events occuring after index date but before end of follow up
hospital_events = (
  apcs.where(apcs.admission_date
  .is_on_or_between(index_date, followup_end_date))
)

#find difference between two dates in hours
def diff_dates_hours(date1, date2):
    return (date2-date1).days*24

#find absolute difference between two dates in days
def diff_dates_days(date1, date2):
    return (date2-date1).days

##define function for outcome identification
def get_codes_dates(codelist_name, num_events, start_date, num_codes):

    # Dynamically get the codelist object
    pathogen_codelist = getattr(codelists, codelist_name)

    # Get all relevant events sorted by date
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

###############################################################################
# from https://github.com/opensafely/comparative-booster-spring2023/blob/main/analysis/variables_lib.py
###############################################################################

def _registrations_overlapping_period(start_date, end_date):
    regs = practice_registrations
    return regs.where(
        regs.start_date.is_on_or_before(start_date)
        & (regs.end_date.is_after(end_date) | regs.end_date.is_null())
    )

def practice_registration_as_of(date):
    regs = _registrations_overlapping_period(date, date)
    return regs.sort_by(regs.start_date, regs.end_date).first_for_patient()

def has_a_continuous_practice_registration_spanning(start_date, end_date):
    return _registrations_overlapping_period(start_date, end_date).exists_for_patient()

###############################################################################
# from https://github.com/opensafely/cis-pop-validation-ehrql/blob/cce4f0bfaffa5370b00f847fbcfe742c7a13aeeb/analysis/variable_lib.py#L100
###############################################################################

def any_of(conditions):
    return reduce(operator.or_, conditions)

def emergency_care_diagnosis_matches(codelist):
    conditions = [
        getattr(emergency_care_attendances, column_name).is_in(codelist)
        for column_name in [f"diagnosis_{i:02d}" for i in range(1, 3)]
    ]
    return emergency_care_attendances.where(any_of(conditions))

def hospitalisation_diagnosis_matches(codelist):
    code_strings = set()
    for code in codelist:
        # Pass the string through the ICD10Code to constructor to validate that it has
        # the expected format
        code_string = ICD10Code(code)._to_primitive_type()
        code_strings.add(code_string)
    conditions = [
        # The reason a plain substring search like this works is twofold:
        #
        # * ICD-10 codes all start with the sequence [A-Z][0-9] and do not contain
        #   such a sequence in any other part of the code. In this sense they are
        #   suffix-free and two codes will only match at they start if they match at
        #   all.
        #
        # * Although the codes are not prefix-free they are organised hierarchically
        #   such that code A0123 represents a child concept of code A01. So although
        #   the naive substring matching below will find code A01 if code A0123 is
        #   present, this happens to be the behaviour we actually want.
        #
        # Obviously this is all far from ideal though, and later we hope to be able
        # to pull these codes out in a separate table and handle the matching
        # properly.
        hospital_events.all_diagnoses.contains(code_string)
        for code_string in code_strings
    ]
    return hospital_events.where(any_of(conditions))
  
###############################################################################
# from https://github.com/opensafely/comparative-booster-spring2023/blob/main/analysis/dataset_definition.py
###############################################################################

# query if causes of death match a given codelist
def cause_of_death_matches(codelist):
    conditions = [
        getattr(ons_deaths, column_name).is_in(codelist)
        for column_name in (["underlying_cause_of_death"]+[f"cause_of_death_{i:02d}" for i in range(1, 16)])
    ]
    return any_of(conditions)

def hospitalisation_primary_secondary_diagnosis_matches(codelist):

    conditions = [
        (hospital_events.primary_diagnosis.is_in(codelist))|
        (hospital_events.secondary_diagnosis.is_in(codelist))
    ]
    return hospital_events.where(any_of(conditions))
