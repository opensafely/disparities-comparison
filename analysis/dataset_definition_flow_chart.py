import json, sys
from pathlib import Path 

from datetime import date, datetime
from ehrql.tables import table_from_file, PatientFrame, Series
from ehrql import (
  Dataset, create_dataset, case, when, maximum_of, minimum_of, years, days
)
from ehrql.tables.tpp import ( 
  patients, 
  addresses, 
  clinical_events,
  practice_registrations,
  apcs,
  parents
)

from variable_lib import (
  has_a_continuous_practice_registration_spanning,
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

#define dataset definition settings from command line arguments
start_year = study_start_date.year
end_year = study_end_date.year

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

#define population
dataset.define_population(
  was_alive
  & is_appropriate_age
  & practice_registrations.for_patient_on(index_date).exists_for_patient()
)

#registration and sex
dataset.registered = registered_patients
dataset.is_female_or_male = is_female_or_male

# #age
# dataset.is_appropriate_age = is_appropriate_age

#get patients IMD
dataset.has_imd = has_imd

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
care_home_tpp = (
  addresses.for_patient_on(index_date)
  .care_home_is_potential_match.when_null_then(False)
)
care_home_code = (has_prior_event(codelists.carehome_codelist))
dataset.care_home = care_home_tpp | care_home_code

if cohort == "infants_subgroup" :
  
  mother_id_present = parents.mother_id.is_not_null()
  
  #maternal linkage available
  dataset.mother_id = parents.mother_id
  dataset.birth_date = patients.date_of_birth
  
#   #maternal linkage available
#   dataset.mother_id_present = parents.mother_id.is_not_null()
#   
#   #tell ehrql to use patients from process file
#   @table_from_file(f"output/flow_chart/cohort_mothers_processed_{start_year}_{end_year}.arrow")
#   
#   #extract these patients where index date is the date of birth of the linked infant
#   class matched_patients(PatientFrame) :
#     index_date = Series(date)
#   
#   #mothers registration for 1 year prior to index date
#   dataset.mother_registered = (
#     has_a_continuous_practice_registration_spanning(matched_patients
#     .index_date - years(1), matched_patients.index_date)
#   )
