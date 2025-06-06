##import dependencies
import json, sys
from pathlib import Path
from datetime import datetime
from ehrql import minimum_of
import codelists

from variable_lib import (first_gp_event,
  emergency_care_diagnosis_matches,
  hospitalisation_primary_secondary_diagnosis_matches
)

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

#define seasons for covid
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d").date()

##define outcomes - rsv
  
#extract date of first episode - looking at the first date for which there is
#a code in the RSV primary codelist or a code within diagnosis 1 or 2 
#in the bronchiolitis attendance codelist - FOR INFANTS ONLY
rsv_primary_date_infants = (
    minimum_of(
      (first_gp_event(codelists.rsv_primary_codelist).date),
      (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance)
        .arrival_date.minimum_for_patient())
    )
  )
  
#extract date of first episode - using only the RSV primary codelist
rsv_primary_date = (
    first_gp_event(codelists.rsv_primary_codelist).date
  )

#extract rsv secondary care dates for primary analysis ('specific' phenotype)
#extract date of first episode - looking at the first date for which there is
#a code in the RSV secondary codelist as the primary or secondary diagnosis
rsv_secondary_date = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_codelist)
  .admission_date.minimum_for_patient()
)

##extract outcomes - flu 

#extract date of first episode - looking at the first date for which there is
#a code in the flu primary codelist
flu_primary_date = (
  first_gp_event(codelists.flu_primary_codelist).date
)
  
#extract date of first episode - looking at the first date for which there is
#a code in the flu secondary codelist as the primary or secondary diagnosis
flu_secondary_date = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_codelist)
  .admission_date.minimum_for_patient()
)

##extract outcomes - covid

if study_start_date >= covid_season_min :
  
  #extract date of first episode 
  covid_primary_date = (
    first_gp_event(codelists.covid_primary_codelist).date
  )

  #extract date of first episode - looking at the first date for which there is
  #a code in the covid secondary codelist as the primary or secondary diagnosis
  covid_secondary_date = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.covid_secondary_codelist)
    .admission_date.minimum_for_patient()
  )
