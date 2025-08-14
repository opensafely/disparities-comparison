import json, sys
from pathlib import Path 

from datetime import date, datetime
from ehrql import (
  Dataset, create_dataset, case, when, maximum_of, minimum_of, years, months
)
from ehrql.tables.tpp import ( 
  patients, 
  addresses, 
  clinical_events,
  practice_registrations,
  parents
)

from variable_lib import (
  get_eligible_registrations,
  has_a_continuous_practice_registration_spanning,
  has_prior_event,
  hospitalisation_diagnosis_matches,
)

import codelists

# dataset = Dataset()
dataset = create_dataset()
dataset.configure_dummy_data(population_size = 10000)

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
study_start_date = datetime.strptime(study_dates[args[2]], "%Y-%m-%d").date()
study_end_date = datetime.strptime(study_dates[args[3]], "%Y-%m-%d").date()

#get date patient ages into cohort 
if cohort == "infants" or cohort == "infants_subgroup" :
  age_date = patients.date_of_birth
  age_out_date = patients.date_of_birth + years(2)
  registration_date = (
    practice_registrations.sort_by(practice_registrations.start_date)
    .first_for_patient().start_date
  )
elif cohort == "children_and_adolescents" :
  age_date = patients.date_of_birth + years(2) 
  age_out_date = patients.date_of_birth + years(18)
elif cohort == "adults" :
  age_date = patients.date_of_birth + years(18)
  age_out_date = patients.date_of_birth + years(65)
else :
  age_date = patients.date_of_birth + years(65)
  age_out_date = patients.date_of_birth + years(110)
  
# Define the first period of active registration within the interval of interest.
# Define entry_date and exit_date for each patient during each interval. 
# Only events happening between these dates are elegible to be queried.
first_registration_date = (
  get_eligible_registrations(study_start_date, minimum_of(study_end_date, age_out_date))
  .sort_by(practice_registrations.start_date)
  .first_for_patient().start_date
)

#set index date as latest date of either start date, age date or registration date
#so that patients are the correct age for the cohort when looking at records
index_date = maximum_of(study_start_date, age_date, first_registration_date)

#define date for registration period
registration_date = index_date - months(3)

#set end date as first date of either end date or age out date 
#so that patients are the correct age for the cohort when looking at records
followup_end_date = minimum_of(study_end_date, age_out_date)

#extract patient specific follow up dates
dataset.patient_index_date = index_date
dataset.patient_end_date = followup_end_date

#define patients status: alive/dead
was_alive = (
  (patients.date_of_death.is_after(index_date))|
  (patients.date_of_death.is_null())
)

#define patients age
age_at_start = patients.age_on(study_start_date)
age_at_end = patients.age_on(study_end_date)
age_months = (index_date - patients.date_of_birth).months
age_at_start_months = (study_start_date - patients.date_of_birth).months
age_at_end_months = (study_end_date - patients.date_of_birth).months

#get patients who meet registration criteria
#(3 months continuous registration, for non-infants)
if cohort == "infants" or cohort == "infants_subgroup" :
  registered_patients = (
    (registration_date < (age_date + months(3))) | (
      practice_registrations.for_patient_on(index_date).exists_for_patient()
    )
  )
else :
  registered_patients = (
    has_a_continuous_practice_registration_spanning(registration_date, index_date)
  )

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
has_imd = addresses.for_patient_on(index_date).imd_rounded.is_not_null()

#define dataset definition settings from command line arguments
start_year = study_start_date.year
end_year = study_end_date.year

#define population
if cohort == "infants_subgroup" :
  mother_id_present = parents.mother_id.is_not_null()
  dataset.define_population(
    was_alive
    & is_appropriate_age
    & mother_id_present
  )
else :
  dataset.define_population(
    was_alive
    & is_appropriate_age
  )

#registration and sex
dataset.registered = registered_patients
dataset.is_female_or_male = is_female_or_male

#get patients IMD
dataset.has_imd = has_imd

##exclusion criteria

#combined severe immunodeficiency syndrome
if cohort == "infants" or cohort == "infants_subgroup" :
  dataset.severe_immunodeficiency = (
    has_prior_event(codelists.severe_immunodeficiency_code)
  )

#infant risk group (cardiac disease, pulmonary hypertension)
if cohort == "infants" or cohort == "infants_subgroup" :
  dataset.risk_group_infants = (case(
    when(age_months < 12)
    .then(hospitalisation_diagnosis_matches(codelists.ventilation_codes).exists_for_patient()),
    when(
      (age_months >=12) &
      (age_months <= 23) &
      ((clinical_events.where(clinical_events.ctv3_code.is_in(codelists.cardiac_disease_codelist)).exists_for_patient())|
       (clinical_events.where(clinical_events.snomedct_code.is_in(codelists.pulmonary_hypertension_codelist)).exists_for_patient()))
    )
    .then(hospitalisation_diagnosis_matches(codelists.ventilation_codes).exists_for_patient()),
    otherwise = False)
  )

#care home resident
care_home_tpp = (
  addresses.for_patient_on(index_date).care_home_is_potential_match.when_null_then(False)
)
care_home_code = has_prior_event(codelists.carehome_codelist)
dataset.care_home = (care_home_tpp) | (care_home_code)

if cohort == "infants_subgroup" :
  mother_id_present = parents.mother_id.is_not_null()
  #maternal linkage available
  dataset.mother_id = parents.mother_id
  dataset.birth_date = patients.date_of_birth
