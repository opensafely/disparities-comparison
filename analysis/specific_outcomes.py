##import dependencies

from datetime import date, datetime
from ehrql import case, when, minimum_of
from ehrql.tables.tpp import clinical_events, apcs, emergency_care_attendances
import codelists

##define functions for queries

#infections occuring after index date but before study end date
infection_events = (
  clinical_events.where(clinical_events.date
  .is_on_or_between(index_date, followup_end_date))
)
  
#query infection_events for date of earliest event-in-codelist
def first_infection_event(codelist, where = True):
    return (
        infection_events.where(where)
        .where(infection_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

#define seasons for covid
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d").date()

##define outcomes - rsv
  
#extract date of first episode - looking at the first date for which there is
#a code in the RSV primary codelist or a code within diagnosis 1 or 2 
#in the bronchiolitis attendance codelist - FOR INFANTS ONLY
rsv_primary_date_infants = (
  minimum_of((first_infection_event(codelists.rsv_primary_codelist).date),
  ((emergency_care_attendances.where((emergency_care_attendances
  .diagnosis_01.is_in(codelists.bronchiolitis_attendance))
  |(emergency_care_attendances.diagnosis_02
  .is_in(codelists.bronchiolitis_attendance)))
  .where(emergency_care_attendances.arrival_date
  .is_on_or_between(index_date, followup_end_date)))
  .arrival_date.minimum_for_patient()))
)
  
#extract date of first episode - using only the RSV primary codelist
rsv_primary_date = (
  first_infection_event(codelists
  .rsv_primary_codelist).date
)

#extract rsv secondary care dates for primary analysis ('specific' phenotype)
#extract date of first episode - looking at the first date for which there is
#a code in the RSV secondary codelist as the primary or secondary diagnosis
rsv_secondary_date = (
  apcs.sort_by(apcs.admission_date)
  .where((apcs.primary_diagnosis
  .is_in(codelists.rsv_secondary_codelist)) 
  |(apcs.secondary_diagnosis
  .is_in(codelists.rsv_secondary_codelist)))
  .where(apcs.admission_date.is_on_or_between(
  index_date, followup_end_date))
  .first_for_patient().admission_date
)

##extract outcomes - flu 

#extract date of first episode - looking at the first date for which there is
#a code in the flu primary codelist
flu_primary_date = (
  first_infection_event(codelists
  .flu_primary_codelist).date
)
  
#extract date of first episode - looking at the first date for which there is
#a code in the flu secondary codelist as the primary or secondary diagnosis
flu_secondary_date = (
  apcs.sort_by(apcs.admission_date)
  .where(apcs.primary_diagnosis
  .is_in(codelists.flu_secondary_codelist) 
  |apcs.secondary_diagnosis
  .is_in(codelists.flu_secondary_codelist))
  .where(apcs.admission_date.is_on_or_between(
  index_date, followup_end_date)).admission_date
  .minimum_for_patient()
)

##extract outcomes - covid

if study_start_date >= covid_season_min :
  
  #extract date of first episode 
  covid_primary_date = (
    first_infection_event(codelists
    .covid_primary_codelist).date
  )

  #extract date of first episode - looking at the first date for which there is
  #a code in the covid secondary codelist as the primary or secondary diagnosis
  covid_secondary_date = (
    apcs.sort_by(apcs.admission_date)
    .where(apcs.primary_diagnosis
    .is_in(codelists.covid_secondary_codelist) 
    |apcs.secondary_diagnosis
    .is_in(codelists.covid_secondary_codelist))
    .where(apcs.admission_date.is_on_or_between(
    index_date, followup_end_date)).first_for_patient()
    .admission_date
  )
