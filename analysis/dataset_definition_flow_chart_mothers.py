import json, sys
from pathlib import Path 

from datetime import date, datetime
from ehrql import Dataset
from ehrql.tables.tpp import ( 
  patients, 
  parents,
  practice_registrations
)

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
study_start_date = datetime.strptime(study_dates[args[1]], "%Y-%m-%d").date()
study_end_date = datetime.strptime(study_dates[args[2]], "%Y-%m-%d").date()

#define patients age
age_at_start_months = (study_start_date - patients.date_of_birth).months
is_appropriate_age = (age_at_start_months <= 23) & (age_at_start_months >= 0)

#define population
dataset.define_population(is_appropriate_age)

#maternal linkage available
dataset.mother_id = parents.mother_id
dataset.birth_date = patients.date_of_birth
