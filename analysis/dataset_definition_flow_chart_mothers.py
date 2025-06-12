import json, sys
from pathlib import Path 

from datetime import date, datetime
from ehrql import Dataset, years
from ehrql.tables import table_from_file, PatientFrame, Series
from ehrql.tables.tpp import practice_registrations

from variable_lib import has_a_continuous_practice_registration_spanning

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
start_year = study_start_date.year
end_year = study_end_date.year

#tell ehrql to use patients from process file
@table_from_file(f"output/flow_chart/cohort_mothers_processed_{start_year}_{end_year}.arrow")

#extract these patients where index date is the date of birth of the linked infant
class matched_patients(PatientFrame) :
  index_date = Series(date)

##define populations
registered_mothers = (
  practice_registrations.for_patient_on(matched_patients.index_date).exists_for_patient()
)

#extract mothers whose patient id matches those who were extracted with infants
dataset.define_population(
  registered_mothers
)

#mothers registration for 1 year prior to index date
dataset.mother_registered = (
  has_a_continuous_practice_registration_spanning(
    matched_patients.index_date - years(1), matched_patients.index_date
  )
)
