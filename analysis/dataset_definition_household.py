##household information extraction
import json, sys
from pathlib import Path 

from datetime import datetime
from ehrql import Dataset
from ehrql.tables.tpp import patients, household_memberships_2020, addresses

#######################################################################################
# Import study dates defined in "./analysis/design/study-dates.R" script and then exported
# to JSON
#######################################################################################
study_dates = json.loads(
  Path("analysis/design/study-dates.json").read_text(),
)

args = sys.argv

#define dataset definition settings from command line arguments
study_start_date = datetime.strptime(study_dates[args[1]], "%Y-%m-%d").date()
study_end_date = datetime.strptime(study_dates[args[2]], "%Y-%m-%d").date()

# Define the dataset
dataset = Dataset()

#define the population
dataset.define_population(household_memberships_2020.household_pseudo_id.is_not_null())

#get household ID
dataset.household_pseudo_id = household_memberships_2020.household_pseudo_id

#get household size
dataset.household_size = household_memberships_2020.household_size

#get patient age at study start date
dataset.age = patients.age_on(study_start_date)

#get rural/urban classification
dataset.rural_urban_classification = addresses.for_patient_on(study_start_date).rural_urban_classification
