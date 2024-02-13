import json, sys
from pathlib import Path 

from datetime import date, datetime
from ehrql import Dataset, case, when, maximum_of, minimum_of, years
from ehrql.tables.tpp import ( 
  patients, 
  medications,
  ons_deaths,
  addresses, 
  clinical_events,
  practice_registrations,
  household_memberships_2020,
  vaccinations,
  apcs
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

# Change these in ./analysis/design/study-dates.R if necessary
study_start_date = study_dates[args[2]]
study_end_date = study_dates[args[3]]
index_date = study_start_date
registration_date = index_date - years(1)

age_at_start = patients.age_on(study_start_date)
age_at_end = patients.age_on(study_end_date)
age_months = (index_date - patients.date_of_birth).months
age_at_start_months = (study_start_date - patients.date_of_birth).months

#get patients who are registered, have sex, age, and imd info
registered_patients = case(
  when(args[1] == "adults").then(practice_registrations
  .for_patient_on(registration_date).exists_for_patient()),
  when(args[1] == "children_adolescents").then(practice_registrations
  .for_patient_on(registration_date).exists_for_patient()),
  otherwise = practice_registrations.for_patient_on(index_date).exists_for_patient()
)
is_female_or_male = patients.sex.is_in(["female", "male"])
is_appropriate_age = case(
  when(args[1] == "older_adults").then((age_at_start <= 110) & (age_at_end >= 65)),
  when(args[1] == "adults").then((age_at_start <= 64) & (age_at_end >= 18)),
  when(args[1] == "children_adolescents").then((age_at_start <= 17) & (age_at_end >= 2)),
  when(args[1] == "infants").then(age_at_start_months <= 23),
  when(args[1] == "infants_subgroup").then(age_at_start_months <= 23)
)
has_imd = (addresses.for_patient_on(index_date).imd_rounded.is_not_null())

#define population
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
  when(args[1] == "older_adults").then(age_at_start),
  when(args[1] == "adults").then(age_at_start),
  when(args[1] == "children_adolescents").then(age_at_start),
  when(args[1] == "infants").then(age_at_start_months),
  when(args[1] == "infants_subgroup").then(age_at_start_months)
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

##comorbidities

#medication date
medication_date = index_date - years(1)

#has asthma if there is an asthma diagnosis and a recent medication prescribed 
if args[1] != "infants" and args[1] != "infants_subgroup" :
  dataset.has_asthma = (
    clinical_events.where(clinical_events.snomedct_code.is_in(codelists.asthma_codelist))
    .exists_for_patient() & medications.where(medications.dmd_code
    .is_in(codelists.asthma_medications))
    .where(medications.date.is_on_or_between(medication_date, index_date))
    .exists_for_patient()
  )

#reactive airway disease diagnosis 
if args[1] == "children_adolescents" : 
  dataset.has_reactive_airway = (
  clinical_events.where(clinical_events.snomedct_code
  .is_in(reactive_airway_disease_code))
  .exists_for_patient()
  )

#copd diagnosis
if args[1] == "adults" or args[1] == "older_adults" :
  dataset.has_copd = (
  clinical_events.where(clinical_events.snomedct_code.is_in(codelists.copd_codelist))
  #& medications.where(medications.dmd_code.is_in(codelists.copd_medications))
  .exists_for_patient()
  )

#pulmonary fibrosis diagnosis
if args[1] == "adults" or args[1] == "older_adults" :
  dataset.has_pulmonary_fibrosis = (
  clinical_events.where(clinical_events.snomedct_code
  .is_in(codelists.pulmonary_fibrosis_codelist))
  .where(clinical_events.date.is_on_or_before(index_date))
  .exists_for_patient()
  )

#define earliest vaccination date 
vaccination_date = study_start_date - years(1)

#define seasons for covid
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d")

#vaccinations
if args[1] == "adults" or args[1] == "older_adults" or args[1] == "children_adolescents" :
  flu_vaccination = (
  vaccinations.where(vaccinations.target_disease.is_in(["Influenza"]))
  .sort_by(vaccinations.date)
  .where(vaccinations.date.is_on_or_between(vaccination_date, index_date))
  .exists_for_patient()
  )

if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
  if args[1] == "adults" or args[1] == "older_adults" or args[1] == "children_adolescents" :
    covid_vaccination_count = (
    vaccinations.where(vaccinations.target_disease.is_in(["SARS-COV-2"]))
    .sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_before(index_date))
    .count_for_patient()
    )


##outcomes

#rsv primary
dataset.rsv_primary = (
  clinical_events.where(clinical_events.ctv3_code
  .is_in(codelists.covid_primary_codelist)) #change codelist when available
  .exists_for_patient()
)

#date
dataset.rsv_primary_date = (
  clinical_events.where(clinical_events.ctv3_code
  .is_in(codelists.covid_primary_codelist)) #change codelist when available
  .sort_by(clinical_events.date)
  .first_for_patient().date
)

#rsv secondary
dataset.rsv_secondary = (
  apcs.where(apcs.primary_diagnosis
  .is_in(codelists.covid_secondary_codelist)) #change codelist when available
  .exists_for_patient()
  |apcs.where(apcs.secondary_diagnosis
  .is_in(codelists.covid_secondary_codelist)) #change codelist when available
  .exists_for_patient()
)

#date
dataset.rsv_secondary_date = (
  apcs.sort_by(apcs.admission_date)
  .where(apcs.primary_diagnosis.is_in(codelists.covid_secondary_codelist) #change codelist when available
  |apcs.secondary_diagnosis.is_in(codelists.covid_secondary_codelist)) #change codelist when available
  .first_for_patient().admission_date
)

#covid primary 
if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
  dataset.covid_primary = (
    clinical_events.where(clinical_events.ctv3_code
    .is_in(codelists.covid_primary_codelist))
    .exists_for_patient()
  )

#date
if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
  dataset.covid_primary_date = (
    clinical_events.where(clinical_events.ctv3_code
    .is_in(codelists.covid_primary_codelist))
    .sort_by(clinical_events.date)
    .first_for_patient().date
  )

#covid secondary
if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
  dataset.covid_secondary = (
    apcs.where(apcs.primary_diagnosis
    .is_in(codelists.covid_secondary_codelist))
    .exists_for_patient()
    |apcs.where(apcs.secondary_diagnosis
    .is_in(codelists.covid_secondary_codelist))
    .exists_for_patient()
  )

#date
if datetime.strptime(study_start_date, "%Y-%m-%d") >= covid_season_min :
  dataset.covid_secondary_date = (
    apcs.sort_by(apcs.admission_date)
    .where(apcs.primary_diagnosis.is_in(codelists.covid_secondary_codelist) 
    |apcs.secondary_diagnosis.is_in(codelists.covid_secondary_codelist)) 
    .first_for_patient().admission_date
  )

#flu primary 
dataset.flu_primary = (
  clinical_events.where(clinical_events.ctv3_code
  .is_in(codelists.covid_primary_codelist)) #change codelist when available
  .exists_for_patient()
)

#date
dataset.flu_primary_date = (
  clinical_events.where(clinical_events.ctv3_code
  .is_in(codelists.covid_primary_codelist)) #change codelist when available
  .sort_by(clinical_events.date)
  .first_for_patient().date
)

#flu secondary
dataset.flu_secondary = (
  apcs.where(apcs.primary_diagnosis
  .is_in(codelists.covid_secondary_codelist)) #change codelist when available
  .exists_for_patient()
  |apcs.where(apcs.secondary_diagnosis
  .is_in(codelists.covid_secondary_codelist)) #change codelist when available
  .exists_for_patient()
)

#date
dataset.flu_secondary_date = (
  apcs.sort_by(apcs.admission_date)
  .where(apcs.primary_diagnosis.is_in(codelists.covid_secondary_codelist) #change codelist when available
  |apcs.secondary_diagnosis.is_in(codelists.covid_secondary_codelist)) #change codelist when available
  .first_for_patient().admission_date
)

##exclusion criteria

#combined severe immunodeficiency syndrome
if args[1] == "infants" or args[1] == "infants_subgroup" :
  dataset.severe_immunodeficiency = (
  clinical_events.where(clinical_events.snomedct_code
  .is_in(codelists.severe_immunodeficiency_code))
  .exists_for_patient()
  )

#infant risk group (cardiac disease, pulmonary hypertension)
if args[1] == "infants" or args[1] == "infants_subgroup" :
  dataset.risk_group_infants = (
  clinical_events.where(clinical_events.ctv3_code
  .is_in(codelists.cardiac_disease_codelist))
  .exists_for_patient()
  |clinical_events.where(clinical_events.snomedct_code
  .is_in(codelists.pulmonary_hypertension_codelist))
  .exists_for_patient()
  )

#events occurring before index date
prior_events = clinical_events.where(clinical_events.date.is_on_or_before(index_date))

#query prior_events for existence of event-in-codelist
def has_prior_event(codelist, where=True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .exists_for_patient()
    )

#care home resident
if args[1] == "older_adults" :
  dataset.care_home_tpp = (addresses.for_patient_on(index_date)
  .care_home_is_potential_match.when_null_then(False))
  dataset.care_home_code = (has_prior_event(codelists.carehome_codelist))
