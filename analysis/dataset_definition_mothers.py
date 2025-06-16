import json, sys
from pathlib import Path

from datetime import date, datetime
from ehrql import Dataset, case, when, years
from ehrql.tables import table_from_file, PatientFrame, Series

from ehrql.tables.tpp import (
  patients,
  clinical_events,
  vaccinations,
)

from variable_lib import (
  has_a_continuous_practice_registration_spanning,
  has_prior_event,
  filter_codes_by_category
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
study_start_date = datetime.strptime(study_dates[args[2]], "%Y-%m-%d").date()
study_end_date = datetime.strptime(study_dates[args[3]], "%Y-%m-%d").date()

#define dataset definition settings from command line arguments
cohort = args[1]
codelist_type = args[4]
investigation_type = args[5]
start_year = study_start_date.year
end_year = study_end_date.year

#tell ehrql to use patients from process file
@table_from_file(f"output/data/input_mothers_processed_{start_year}_{end_year}_{codelist_type}_{investigation_type}.arrow")

#extract these patients where index date is the date of birth of the linked infant
class matched_patients(PatientFrame) :
  index_date = Series(date)

##define populations

#mothers registration for 1 year prior to index date
registered_mothers = (
  has_a_continuous_practice_registration_spanning(
    matched_patients.index_date - years(1), matched_patients.index_date
  )
)

#extract mothers whose patient id matches those who were extracted with infants
dataset.define_population(
  registered_mothers
)

##extract maternal info

#gestational age - not sure this will be available
#gestational_age = 

#mothers age at birth of infant
dataset.maternal_age = patients.age_on(matched_patients.index_date)

#mothers smoking status
most_recent_smoking_code = (
  clinical_events.where(clinical_events.ctv3_code.is_in(codelists.clear_smoking_codes))
  .sort_by(clinical_events.date).where(clinical_events.date
  .is_on_or_between(matched_patients.index_date - years(1), matched_patients.index_date))
  .last_for_patient().ctv3_code.to_category(codelists.clear_smoking_codes)
)
ever_smoked = (
  clinical_events.where(clinical_events.ctv3_code.is_in(filter_codes_by_category(codelists.clear_smoking_codes, include = ["S", "E"])))
  .exists_for_patient()
)
dataset.maternal_smoking_status = (case(
  when(most_recent_smoking_code == "S")
  .then("Current"),
  when((most_recent_smoking_code == "E")|((most_recent_smoking_code == "N") & (ever_smoked == True)))
  .then("Former"),
  when((most_recent_smoking_code == "N") & (ever_smoked == False))
  .then("Never"),
  otherwise = None)
)

#mothers drinking 
dataset.maternal_drinking = has_prior_event(codelists.drinking_codelist)

#mothers drug use
dataset.maternal_drug_usage = (
  has_prior_event(codelists.drug_usage_codelist + codelists.drug_intervention_codelist + codelists.drug_assessment_declination_codelist)
)

#pertussis vaccination
dataset.maternal_pertussis_vaccination = (
    vaccinations.where(vaccinations.target_disease.is_in(["Pertussis"])).sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(matched_patients.index_date - years(1), matched_patients.index_date))
    .exists_for_patient()
  )

#flu vaccination
dataset.maternal_flu_vaccination = (
    vaccinations.where(vaccinations.target_disease.is_in(["Influenza"])).sort_by(vaccinations.date)
    .where(vaccinations.date.is_on_or_between(matched_patients.index_date - years(1), matched_patients.index_date))
    .exists_for_patient()
  )
