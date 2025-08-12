import json, sys
from pathlib import Path

from datetime import datetime
from ehrql import Dataset, case, when, maximum_of, minimum_of, years, days
from ehrql.tables.tpp import patients

from variable_lib import (
  get_eligible_registrations,
  gp_events,
  first_gp_event,
  is_gp_event,
  emergency_events,
  hospital_events,
  prescribing_events,
  diff_dates_days,
  diff_dates_hours,
  get_codes_dates,
  emergency_care_diagnosis_matches,
  hospitalisation_primary_secondary_diagnosis_matches
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
  get_eligible_registrations(study_start_date, study_end_date)
  .sort_by(practice_registrations.start_date)
  .first_for_patient().start_date
)

#set index date as latest date of either start date, age date or registration date
#so that patients are the correct age for the cohort when looking at records
index_date = maximum_of(study_start_date, age_date, first_registration_date)

#define date for registration period
registration_date = index_date - months(3)

#set end date as earliest date of either end date or age out date 
#so that patients are the correct age for the cohort when looking at records
followup_end_date = minimum_of(study_end_date, age_out_date)

#define first season for covid outcomes
covid_season_min = datetime.strptime("2019-09-01", "%Y-%m-%d").date()

##RSV outcomes

#extract rsv primary care dates for 'sensitive' phenotype

#get dates of events and corresponding codes 
rsv_codes_date, rsv_code = (
  get_codes_dates("rsv_sensitive_codelist", 4, index_date, 2)
)

#get the date of the occurrence of first relevant prescription
rsv_med_date = (
  prescribing_events.where(prescribing_events.dmd_code.is_in(codelists.rsv_prescriptions_codelist))
  .date.minimum_for_patient()
)

#get occurrence of event in exclusion list within one month of rsv_codes_date 
# - looking at the first date for which there is a code in the RSV exclusion
#codelist within one month before or after the date of rsv_codes_date, or a 
#code in the RSV prescriptions codelist within one month before or after the
#date of rsv_codes_date
rsv_exclusion_primary = (case(
  when(
    is_gp_event(codelists.rsv_primary_exclusion_codelist)
    .where(gp_events.date.is_on_or_between(rsv_codes_date - days(30), rsv_codes_date + days(30)))
    .exists_for_patient()
  )
  .then(True),
  when(
    is_gp_event(codelists.rsv_primary_exclusion_codelist)
    .where(gp_events.date.is_on_or_between(rsv_med_date - days(30), rsv_med_date + days(30)))
    .exists_for_patient()
  )
  .then(True),
  otherwise = False)
)

#define prescription inclusion - i.e. presence of a code from codelist with
#relevant prescription - the presence of a code from the sensitive codelist
#is required for inclusion 
rsv_med_inclusion_date = (case(
  when(
    is_gp_event(codelists.rsv_sensitive_codelist)
    .where(gp_events.date.is_on_or_between(rsv_med_date - days(14), rsv_med_date + days(14)))
    .exists_for_patient()
  )
  .then(
    minimum_of(
      (is_gp_event(codelists.rsv_sensitive_codelist)
        .where(gp_events.date.is_on_or_between(rsv_med_date - days(14), rsv_med_date + days(14)))
        .sort_by(gp_events.date).first_for_patient().date),
      (rsv_med_date)
    )
  ),
  otherwise = None)
)

#infant and infant subroup cohorts
if cohort == "infants" or cohort == "infants_subgroup" :
  
  #extract date of first episode - looking at when the exclusion criteria is
  #not met, and taking the minimum of the date of the first code in the RSV
  #primary codelist, the date of the first code in the RSV sensitive codelist,
  #the date of the first prescription in the RSV prescriptions codelist
  #if there is at least one code in the RSV sensitive codelist, the date of 
  #the first bronchiolitis attendance, or the date of the first wheeze attendance
  
  #first define inclusion from specific phenotype
  rsv_primary_spec = (
    minimum_of(
      (first_gp_event(codelists.rsv_primary_codelist).date),
      (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance)
        .arrival_date.minimum_for_patient())
    )
  )
  #then extract date - prioritising inclusion from specific phenotype
  dataset.rsv_primary_date = (case(
    when(rsv_primary_spec.is_not_null())
    .then(rsv_primary_spec),
    when((rsv_primary_spec.is_null()) & (~rsv_exclusion_primary))
    .then(
      minimum_of(
        (rsv_codes_date),
        (rsv_med_inclusion_date),
        (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance)
          .arrival_date.minimum_for_patient()),
        (emergency_care_diagnosis_matches(codelists.wheeze_attendance)
          .arrival_date.minimum_for_patient())
      )
    ),
    otherwise = None)
  )
  
  #get dates of events and corresponding codes 
  rsv_codes_second_date, rsv_code_second = (
    get_codes_dates("rsv_sensitive_codelist", 4,
                    dataset.rsv_primary_date + days(14), 2)
  )
  
  #get the date of the occurrence of first relevant prescription
  # - looking at the second episode
  rsv_med_second_date = (
    prescribing_events.where(prescribing_events.dmd_code.is_in(codelists.rsv_prescriptions_codelist))
    .where(prescribing_events.date.is_on_or_after(dataset.rsv_primary_date + days(14)))
    .date.minimum_for_patient()
  )
  
  #get occurrence of event in exclusion list within one month of 
  #rsv_codes_second_date - looking at the same criteria as the first episode
  rsv_exclusion_primary_second = (case(
    when(
      is_gp_event(codelists.rsv_primary_exclusion_codelist)
      .where(
        gp_events.date.is_on_or_between(
          maximum_of(dataset.rsv_primary_date + days(14), rsv_codes_second_date - days(30)),
          rsv_codes_second_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    when(
      is_gp_event(codelists.rsv_primary_exclusion_codelist)
      .where(
        gp_events.date.is_on_or_between(
          maximum_of(dataset.rsv_primary_date + days(14), rsv_med_second_date - days(30)),
          rsv_med_second_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    otherwise = False)
  )
  
  #define prescription inclusion - i.e. presence of a code from codelist with
  #relevant prescription - the presence of a code from the sensitive codelist
  #is required for inclusion 
  rsv_med_inclusion_second_date = (case(
    when(
      is_gp_event(codelists.rsv_sensitive_codelist)
      .where(
        gp_events.date.is_on_or_between(
          rsv_med_second_date - days(14), rsv_med_second_date + days(14)
        )
      ).exists_for_patient()
    )
    .then(
      minimum_of(
        (is_gp_event(codelists.rsv_sensitive_codelist)
          .where(
            gp_events.date.is_on_or_between(
              maximum_of(rsv_med_second_date - days(14), dataset.rsv_primary_date + days(14)),
              rsv_med_second_date + days(14)
            )
          ).sort_by(gp_events.date).first_for_patient().date),
        (rsv_med_second_date)
      )
    ),
    otherwise = None)
  )
  
  #first get inclusion from specific phenotype
  rsv_primary_spec_second = (
    minimum_of(
      (is_gp_event(codelists.rsv_primary_codelist)
        .where(gp_events.date.is_on_or_after(dataset.rsv_primary_date + days(14)))
        .sort_by(gp_events.date).first_for_patient().date),
      (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance)
        .where(emergency_events.arrival_date.is_on_or_after(dataset.rsv_primary_date + days(14)))
        .arrival_date.minimum_for_patient())
    )
  )
  
  #extract date of second episode - using the same criteria as the first episode
  dataset.rsv_primary_second_date = (case(
    when(rsv_primary_spec_second.is_not_null())
    .then(rsv_primary_spec_second),
    when((rsv_primary_spec_second.is_null()) & (~rsv_exclusion_primary_second))
    .then(
      minimum_of(
        (rsv_codes_second_date),
        (rsv_med_inclusion_second_date),
        (emergency_care_diagnosis_matches(codelists.bronchiolitis_attendance)
          .where(emergency_events.arrival_date.is_on_or_after(dataset.rsv_primary_date + days(14)))
          .arrival_date.minimum_for_patient()),
        (emergency_care_diagnosis_matches(codelists.wheeze_attendance)
          .where(emergency_events.arrival_date.is_on_or_after(dataset.rsv_primary_date + days(14)))
          .arrival_date.minimum_for_patient())
      )
    ),
    otherwise = None)
  )
  
#all cohorts above the age of 2 
else :
  
  #extract date of first episode - looking at when the exclusion criteria is
  #not met, and taking the minimum of the date of the first code in the RSV
  #primary codelist, the date of the first code in the RSV sensitive codelist,
  #or the date of the first prescription in the RSV prescriptions codelist
  #when there is at least one code in the RSV sensitive codelist
  
  #first define inclusion from specific phenotype
  rsv_primary_spec = (
    first_gp_event(codelists.rsv_primary_codelist).date
  )
  #then extract date - prioritising inclusion from specific phenotype
  dataset.rsv_primary_date = (case(
    when(rsv_primary_spec.is_not_null())
    .then(rsv_primary_spec),
    when((rsv_primary_spec.is_null()) & (~rsv_exclusion_primary))
    .then(minimum_of(rsv_codes_date, rsv_med_inclusion_date)),
    otherwise = None)
  )
  
  #get dates of events and corresponding codes 
  rsv_codes_second_date, rsv_code_second = (
    get_codes_dates("rsv_sensitive_codelist", 4,
                    dataset.rsv_primary_date + days(14), 2)
  )
  
  #get the date of the occurrence of first relevant prescription
  # - looking at the second episode
  rsv_med_second_date = (
    prescribing_events.where(prescribing_events.dmd_code.is_in(codelists.rsv_prescriptions_codelist))
    .where(prescribing_events.date.is_on_or_after(dataset.rsv_primary_date + days(14)))
    .date.minimum_for_patient()
  )
  
  #get occurrence of event in exclusion list within one month of 
  #rsv_codes_second_date - looking at the same criteria as the first episode
  rsv_exclusion_primary_second = (case(
    when(
      is_gp_event(codelists.rsv_primary_exclusion_codelist)
      .where(
        gp_events.date.is_on_or_between(
          maximum_of(dataset.rsv_primary_date + days(14), rsv_codes_second_date - days(30)),
          rsv_codes_second_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    when(
      is_gp_event(codelists.rsv_primary_exclusion_codelist)
      .where(
        gp_events.date.is_on_or_between(
          maximum_of(dataset.rsv_primary_date + days(14), rsv_med_second_date - days(30)),
          rsv_med_second_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    otherwise = False)
  )
  
  #define prescription inclusion - i.e. presence of a code from codelist with
  #relevant prescription - the presence of a code from the sensitive codelist
  #is required for inclusion 
  rsv_med_inclusion_second_date = (case(
    when(
      is_gp_event(codelists.rsv_sensitive_codelist)
      .where(gp_events.date.is_on_or_between(rsv_med_second_date - days(14), rsv_med_second_date + days(14)))
      .exists_for_patient()
    )
    .then(
      minimum_of(
        (is_gp_event(codelists.rsv_sensitive_codelist)
          .where(
            gp_events.date.is_on_or_between(
              maximum_of(rsv_med_second_date - days(14), dataset.rsv_primary_date + days(14)),
              rsv_med_second_date + days(14)
            )
          ).sort_by(gp_events.date).first_for_patient().date),
        (rsv_med_second_date)
      )
    ),
    otherwise = None)
  )
  
  #first get inclusion from specific phenotype
  rsv_primary_spec_second = (
    is_gp_event(codelists.rsv_primary_codelist)
    .where(gp_events.date.is_on_or_after(dataset.rsv_primary_date + days(14)))
    .sort_by(gp_events.date).first_for_patient().date
  )
  
  #extract date of second episode - using the same criteria as the first episode
  dataset.rsv_primary_second_date = (case(
    when(rsv_primary_spec_second.is_not_null())
    .then(rsv_primary_spec_second),
    when((rsv_primary_spec_second.is_null()) & (~rsv_exclusion_primary_second))
    .then(minimum_of(rsv_codes_second_date, rsv_med_inclusion_second_date)),
    otherwise = None)
  )

#extract rsv secondary care dates for 'sensitive' phenotype

#get date of first diagnosis code (in any position) from the RSV sensitive 
#secondary care codelist - looking at the first episode
rsv_secondary_sens_date = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.unspecified_lrti)
  .admission_date.minimum_for_patient()
)

#get occurrence of event in exclusion list within one month of an occurrence 
#of rsv_secondary_sens_date 
rsv_exclusion_secondary = (case(
  when(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_exclusion_codelist)
    .where(
      hospital_events.admission_date.minimum_for_patient()
      .is_on_or_between(rsv_secondary_sens_date - days(30), rsv_secondary_sens_date + days(30))
    ).exists_for_patient()
  )
  .then(True),
  otherwise = False)
)

#extract date of first episode - looking at when the exclusion criteria is
#not met and taking the value of rsv_secondary_sens_date

#first define inclusion from specific phenotype
rsv_secondary_spec = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_codelist)
  .admission_date.minimum_for_patient()
)
#then extract date - prioritising inclusion from specific phenotype
dataset.rsv_secondary_date = (case(
  when(rsv_secondary_spec.is_not_null())
  .then(rsv_secondary_spec),
  when((rsv_secondary_spec.is_null()) & (~rsv_exclusion_secondary))
  .then(rsv_secondary_sens_date),
  otherwise = None)
)

#get discharge date for first episode
rsv_secondary_discharge = (case(
  when(rsv_secondary_spec.is_not_null())
  .then(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_codelist)
    .where(hospital_events.admission_date.is_on_or_between(dataset.rsv_secondary_date, dataset.rsv_secondary_date))
    .discharge_date.minimum_for_patient()
  ),
  when(rsv_secondary_spec.is_null())
  .then(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.unspecified_lrti)
    .where(hospital_events.admission_date.is_on_or_between(dataset.rsv_secondary_date, dataset.rsv_secondary_date))
    .discharge_date.minimum_for_patient()
  ),
  otherwise = None)
)

#extract length of stay for first episode, in hours
dataset.rsv_los = (
  diff_dates_hours(dataset.rsv_secondary_date, rsv_secondary_discharge)
)  

#get date of first diagnosis code (in any position) from the RSV sensitive 
#secondary care codelist - looking at the second episode
rsv_secondary_sens_second_date = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.unspecified_lrti)
  .where(hospital_events.admission_date.is_on_or_after(dataset.rsv_secondary_date + days(14)))
  .admission_date.minimum_for_patient()
)

#get occurrence of event in exclusion list within one month of an occurrence 
#of rsv_secondary_sens_second_date 
rsv_exclusion_secondary_second = (case(
  when(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_exclusion_codelist)
    .where(
      hospital_events.admission_date.minimum_for_patient().is_on_or_between(
        maximum_of(dataset.rsv_secondary_date + days(14), rsv_secondary_sens_second_date - days(30)),
        rsv_secondary_sens_second_date + days(30)
      )
    ).exists_for_patient()
  )
  .then(True),
  otherwise = False)
)

#first define inclusion from specific phenotype
rsv_secondary_spec_second = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_codelist)
  .where(hospital_events.admission_date.is_on_or_after(dataset.rsv_secondary_date + days(14)))
  .admission_date.minimum_for_patient()
)

#extract date of second episode - using the same criteria as the first episode
dataset.rsv_secondary_second_date = (case(
  when(rsv_secondary_spec_second.is_not_null())
  .then(rsv_secondary_spec_second),
  when((rsv_secondary_spec_second.is_null()) & (~rsv_exclusion_secondary_second))
  .then(rsv_secondary_sens_second_date),
  otherwise = None)
)

#get discharge date for second episode
rsv_secondary_discharge_second = (case(
  when(rsv_secondary_spec_second.is_not_null())
  .then(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.rsv_secondary_codelist)
    .where(hospital_events.admission_date.is_on_or_between(dataset.rsv_secondary_second_date, dataset.rsv_secondary_second_date))
    .discharge_date.minimum_for_patient()
  ),
  when(rsv_secondary_spec_second.is_null())
  .then(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.unspecified_lrti)
    .where(hospital_events.admission_date.is_on_or_between(dataset.rsv_secondary_second_date, dataset.rsv_secondary_second_date))
    .discharge_date.minimum_for_patient()
  ),
  otherwise = None)
)

#extract length of stay for second episode, in hours
dataset.rsv_los_second = (
  diff_dates_hours(dataset.rsv_secondary_second_date, rsv_secondary_discharge_second)
)

##flu outcomes

#extract flu primary care dates for 'sensitive' phenotype

#get date of first case of either ARI or fever for first episode
ari_dates = (
  get_codes_dates("ari_primary_codelist", 4, index_date, 1)
)
fever_dates = (
  get_codes_dates("fever_codelist", 4, index_date, 1)
)

ILI_pairs = []
ILI_date_cases = []

for ari_date in ari_dates:
    for fever_date in fever_dates:
        close_in_time = diff_dates_days(ari_date, fever_date) <= abs(14)
        ILI_pairs.append(when(close_in_time).then(True))
        ILI_date_cases.append(when(close_in_time)
        .then(minimum_of(ari_date, fever_date)))

ILI_case = case(*ILI_pairs, otherwise = False)
ILI_date = case(*ILI_date_cases, otherwise = None)

#get date of occurrence of first relevant prescription
flu_med_date = (
  prescribing_events.where(prescribing_events.dmd_code.is_in(codelists.flu_prescriptions_codelist))
  .date.minimum_for_patient()
)

#occurrence of event in exclusion list within one month of ILI
flu_exclusion_primary = (case(
  when(
    is_gp_event(codelists.flu_primary_exclusion_codelist)
    .where(gp_events.date.is_on_or_between(ILI_date - days(30), ILI_date + days(30)))
    .exists_for_patient()
  )
  .then(True),
  when(
    is_gp_event(codelists.flu_primary_exclusion_codelist)
    .where(gp_events.date.is_on_or_between(flu_med_date - days(30), flu_med_date + days(30)))
    .exists_for_patient()
  )
  .then(True),
  otherwise = False)
)

#get date of first flu episode

#first define inclusion from specific phenotype
flu_primary_spec = (
  first_gp_event(codelists.flu_primary_codelist).date
)
#then extract date - prioritising inclusion from specific phenotype
dataset.flu_primary_date = (case(
  when(flu_primary_spec.is_not_null())
  .then(flu_primary_spec),
  when((flu_primary_spec.is_null()) & (~flu_exclusion_primary))
  .then(minimum_of(ILI_date, flu_med_date)))
)

#get date of first case of either ARI or fever for second episode
ari_second_dates = (
  get_codes_dates("ari_primary_codelist", 4,
                  dataset.flu_primary_date + days(14), 1)
)
fever_second_dates = (
  get_codes_dates("fever_codelist", 4,
                  dataset.flu_primary_date + days(14), 1)
)

ILI_pairs_second = []
ILI_second_date_cases = []

for ari_date in ari_second_dates:
    for fever_date in fever_second_dates:
        close_in_time = diff_dates_days(ari_date, fever_date) <= abs(14)
        ILI_pairs_second.append(when(close_in_time).then(True))
        ILI_second_date_cases.append(when(close_in_time)
        .then(minimum_of(ari_date, fever_date)))

ILI_case_second = case(*ILI_pairs_second, otherwise = False)
ILI_second_date = case(*ILI_second_date_cases, otherwise = None)

#get date of occurrence of first relevant prescription
flu_med_second_date = (
  prescribing_events.where(prescribing_events.dmd_code.is_in(codelists.flu_prescriptions_codelist))
  .where(prescribing_events.date.is_on_or_after(dataset.flu_primary_date + days(14)))
  .date.minimum_for_patient()
)

#occurrence of event in exclusion list within one month of second ILI 
flu_exclusion_primary_second = (case(
  when(
    is_gp_event(codelists.flu_primary_exclusion_codelist)
    .where(
      gp_events.date.is_on_or_between(
        maximum_of(dataset.flu_primary_date + days(14), ILI_second_date - days(30)),
        ILI_second_date + days(30)
      )
    ).exists_for_patient()
  )
  .then(True),
  when(
    is_gp_event(codelists.flu_primary_exclusion_codelist)
    .where(
      gp_events.date.is_on_or_between(
        maximum_of(dataset.flu_primary_date + days(14), flu_med_second_date - days(30)),
        flu_med_second_date + days(30)
      )
    ).exists_for_patient()
  )
  .then(True),
  otherwise = False)
)

#get date of second flu episode

#first define inclusion from specific phenotype
flu_primary_spec_second = (
  is_gp_event(codelists.flu_primary_codelist)
  .where(gp_events.date.is_on_or_after(dataset.flu_primary_date + days(14)))
  .sort_by(gp_events.date).first_for_patient().date
)
#then extract date - prioritising inclusion from specific phenotype
dataset.flu_primary_second_date = (case(
  when(flu_primary_spec_second.is_not_null())
  .then(flu_primary_spec_second),
  when((flu_primary_spec_second.is_null()) & (~flu_exclusion_primary_second))
  .then(minimum_of(ILI_second_date, flu_med_second_date)),
  otherwise = None)
)

#extract flu secondary care dates for 'sensitive' phenotype

#get date of first diagnosis code (in any position) from the flu sensitive
#secondary care codelist or ARI secondary care codelist - looking at the first episode
flu_secondary_sens_date = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.ari_secondary_codelist)
  .admission_date.minimum_for_patient()
)

#occurance of event in exclusion list within one month of flu_secondary_sens_date
flu_exclusion_secondary = (case(
  when(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_exclusion_codelist)
    .where(
      hospital_events.admission_date.minimum_for_patient().is_on_or_between(
        flu_secondary_sens_date - days(30), flu_secondary_sens_date + days(30)
      )
    ).exists_for_patient()
  )
  .then(True),
  otherwise = False)
)

#extract date of first episode - looking at when the exclusion criteria is
#not met

#first define inclusion from specific phenotype
flu_secondary_spec = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_codelist)
  .admission_date.minimum_for_patient()
)
#then extract date - prioritising inclusion from specific phenotype
dataset.flu_secondary_date = (case(
  when(flu_secondary_spec.is_not_null())
  .then(flu_secondary_spec),
  when((flu_secondary_spec.is_null()) & (~flu_exclusion_secondary))
  .then(flu_secondary_sens_date),
  otherwise = None)
)

#get discharge date for first episode
flu_secondary_discharge = (case(
  when(flu_secondary_spec.is_not_null())
  .then(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_codelist)
    .where(hospital_events.admission_date.is_on_or_between(dataset.flu_secondary_date, dataset.flu_secondary_date))
    .discharge_date.minimum_for_patient()
  ),
  when(flu_secondary_spec.is_null())
  .then(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.ari_secondary_codelist)
    .where(hospital_events.admission_date.is_on_or_between(dataset.flu_secondary_date, dataset.flu_secondary_date))
    .discharge_date.minimum_for_patient()
  ),
  otherwise = None)
)

#extract length of stay for first episode, in hours
dataset.flu_los = (
  diff_dates_hours(dataset.flu_secondary_date, flu_secondary_discharge)
)

#get date of first diagnosis code (in any position) from the flu sensitive 
#secondary care codelist - looking at the second episode

#first define inclusion from specific phenotype
flu_secondary_spec_second = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_codelist)
  .where(hospital_events.admission_date.is_on_or_after(dataset.flu_secondary_date + days(14)))
  .admission_date.minimum_for_patient()
)
#then extract date - prioritising inclusion from specific phenotype
flu_secondary_sens_second_date = (
  hospitalisation_primary_secondary_diagnosis_matches(codelists.ari_secondary_codelist)
  .where(hospital_events.admission_date.is_on_or_after(dataset.flu_secondary_date + days(14)))
  .admission_date.minimum_for_patient()
)

#get occurrence of event in exclusion list within one month of an occurrence 
#of flu_secondary_sens_second_date
flu_exclusion_secondary_second = (case(
  when(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_exclusion_codelist)
    .where(
      hospital_events.admission_date.minimum_for_patient().is_on_or_between(
        maximum_of(dataset.flu_secondary_date + days(14), flu_secondary_sens_second_date - days(30)),
        flu_secondary_sens_second_date + days(30)
      )
    ).exists_for_patient()
  )
  .then(True),
  otherwise = False)
)

#extract date of second episode - using the same criteria as the first episode
dataset.flu_secondary_second_date = (case(
  when(flu_secondary_spec_second.is_not_null())
  .then(flu_secondary_spec_second),
  when((flu_secondary_spec_second.is_null()) & (~flu_exclusion_secondary_second))
  .then(flu_secondary_sens_second_date),
  otherwise = None)
)

#get discharge date for second episode
flu_secondary_discharge_second = (case(
  when(flu_secondary_spec_second.is_not_null())
  .then(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.flu_secondary_codelist)
    .where(
      hospital_events.admission_date.is_on_or_between(
        dataset.flu_secondary_second_date, dataset.flu_secondary_second_date
      )
    ).discharge_date.minimum_for_patient()
  ),
  when(flu_secondary_spec_second.is_null())
  .then(
    hospitalisation_primary_secondary_diagnosis_matches(codelists.ari_secondary_codelist)
    .where(
      hospital_events.admission_date.is_on_or_between(
        dataset.flu_secondary_second_date, dataset.flu_secondary_second_date
      )
    ).discharge_date.minimum_for_patient()
  ),
  otherwise = None)
)

#extract length of stay for second episode, in hours
dataset.flu_los_second = (
  diff_dates_hours(dataset.flu_secondary_second_date, flu_secondary_discharge_second)
)

##COVID-19 outcomes
  
#extract covid primary care dates for 'sensitive' phenotype
if study_start_date >= covid_season_min :
  
  #count number of clinical codes in covid symptom list which occur within 14 days
  #looking at the first episode
  
  #get dates of events and corresponding codes 
  covid_codes_date, covid_code = (
    get_codes_dates("covid_sensitive_codelist", 4, index_date, 2)
  )
  
  #get date of first occurrence of relevant prescription
  covid_med_date = (
    prescribing_events.where(prescribing_events.dmd_code.is_in(codelists.covid_prescriptions_codelist))
    .date.minimum_for_patient()
  )
  
  #occurrence of event in exclusion list within one month of covid_codes_date
  #- looking at the first date for which there is a code in the covid exclusion
  #codelist within one month before or after the date of covid_codes_date, or a 
  #code in the covid prescriptions codelist within one month before or after the
  #date of covid_codes_date
  covid_exclusion_primary = (case(
    when(
      is_gp_event(codelists.covid_primary_exclusion_codelist)
      .where(gp_events.date.is_on_or_between(covid_codes_date - days(30), covid_codes_date + days(30)))
      .exists_for_patient()
    )
    .then(True),
    when(
      is_gp_event(codelists.covid_primary_exclusion_codelist)
      .where(gp_events.date.is_on_or_between(covid_med_date - days(30), covid_med_date + days(30)))
      .exists_for_patient()
    )
    .then(True),
    otherwise = False)
  )
  
  #extract date of first episode where the exclusion criteria is not met
  # - get the first date of either a code in the covid primary codelist, 
  #a code in the covid sensitive codelist, or a prescription in the covid
  #prescriptions codelist
  
  #first define inclusion from specific phenotype
  covid_primary_spec = (
    first_gp_event(codelists.covid_primary_codelist).date
  )
  #then extract date - prioritising inclusion from specific phenotype
  dataset.covid_primary_date = (case(
    when(covid_primary_spec.is_not_null())
    .then(covid_primary_spec),
    when((covid_primary_spec.is_null()) & (~covid_exclusion_primary))
    .then(minimum_of(covid_codes_date, covid_med_date)),
    otherwise = None)
  )
  
  #count number of clinical codes in covid symptom list for second episode
  
  #get dates of events and corresponding codes 
  covid_codes_second_date, covid_code_second = (
    get_codes_dates("covid_sensitive_codelist", 4,
                    dataset.covid_primary_date + days(14), 2)
  )
  
  #get date of first occurrence of relevant prescription - for second episode
  covid_med_second_date = (
    prescribing_events.where(prescribing_events.dmd_code.is_in(codelists.covid_prescriptions_codelist))
    .where(prescribing_events.date.is_on_or_after(dataset.covid_primary_date + days(14)))
    .date.minimum_for_patient()
  )
  
  #occurrence of event in exclusion list within one month of covid_codes_second_date
  # - using the same criteria as the first episode
  covid_exclusion_primary_second = (case(
    when(
      is_gp_event(codelists.covid_primary_exclusion_codelist)
      .where(
        gp_events.date.is_on_or_between(
          maximum_of(dataset.covid_primary_date + days(14), covid_codes_second_date - days(30)),
          covid_codes_second_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    when(
      is_gp_event(codelists.covid_primary_exclusion_codelist)
      .where(
        gp_events.date.is_on_or_between(
          maximum_of(dataset.covid_primary_date + days(14), covid_med_second_date - days(30)),
          covid_med_second_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    otherwise = False)
  )
  
  #extract date of second episode - using the same criteria as the first episode
  
  #first define inclusion from specific phenotype
  covid_primary_spec_second = (
    is_gp_event(codelists.covid_primary_codelist)
    .where(gp_events.date.is_on_or_after(dataset.covid_primary_date + days(14)))
    .sort_by(gp_events.date).first_for_patient().date
  )
  #then extract date - prioritising inclusion from specific phenotype
  dataset.covid_primary_second_date = (case(
    when(covid_primary_spec_second.is_not_null())
    .then(covid_primary_spec_second),
    when((covid_primary_spec_second.is_null()) & (~covid_exclusion_primary_second))
    .then(minimum_of(covid_codes_second_date, covid_med_second_date)),
    otherwise = None)
  )
  
  #extract covid secondary care dates for 'sensitive' phenotype
  
  #get date of first diagnosis code (in any position) from the covid sensitive
  #secondary care codelist - looking at the first episode
  covid_secondary_sens_date = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.coronavirus_unspecified)
    .admission_date.minimum_for_patient()
  )
  
  #get occurrence of event in exclusion list within one month of covid_secondary_sens_date
  #- looking at the first date for which there is a code in the covid secondary
  #exclusion codelist within one month before or after the date of covid_secondary_sens_date
  covid_exclusion_secondary = (case(
    when(
      hospitalisation_primary_secondary_diagnosis_matches(codelists.covid_secondary_exclusion_codelist)
      .where(
        hospital_events.admission_date.minimum_for_patient().is_on_or_between(
          covid_secondary_sens_date - days(30), covid_secondary_sens_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    otherwise = False)
  )
  
  #extract date of first episode - looking at when the exclusion criteria is
  #not met 
  
  #first define inclusion from specific phenotype
  covid_secondary_spec = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.covid_secondary_codelist)
    .admission_date.minimum_for_patient()
  )
  #then extract date - prioritising inclusion from specific phenotype
  dataset.covid_secondary_date = (case(
    when(covid_secondary_spec.is_not_null())
    .then(covid_secondary_spec),
    when((covid_secondary_spec.is_null()) & (~covid_exclusion_secondary))
    .then(covid_secondary_sens_date),
    otherwise = None)
  )
  
  #get discharge date for first episode
  covid_secondary_discharge = (case(
    when(covid_secondary_spec.is_not_null())
    .then(
      hospitalisation_primary_secondary_diagnosis_matches(codelists.covid_secondary_codelist)
      .where(hospital_events.admission_date.is_on_or_between(dataset.covid_secondary_date, dataset.covid_secondary_date))
      .discharge_date.minimum_for_patient()
    ),
    when(covid_secondary_spec.is_null())
    .then(
      hospitalisation_primary_secondary_diagnosis_matches(codelists.coronavirus_unspecified)
      .where(hospital_events.admission_date.is_on_or_between(dataset.covid_secondary_date, dataset.covid_secondary_date))
      .discharge_date.minimum_for_patient()
    ),
    otherwise = None)
  )
  
  #extract length of stay for first episode, in hours
  dataset.covid_los = (
    diff_dates_hours(dataset.covid_secondary_date, covid_secondary_discharge)
  )
  
  #get date of first diagnosis code (in any position) from the covid sensitive
  #secondary care codelist - looking at the second episode
  covid_secondary_sens_second_date = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.coronavirus_unspecified)
    .where(hospital_events.admission_date.is_on_or_after(dataset.covid_secondary_date + days(14)))
    .admission_date.minimum_for_patient()
  )
  
  #get occurrence of event in exclusion list within one month of an occurrence
  #of covid_secondary_sens_second_date
  covid_exclusion_secondary_second = (case(
    when(
      hospitalisation_primary_secondary_diagnosis_matches(codelists.covid_secondary_exclusion_codelist)
      .where(
        hospital_events.admission_date.minimum_for_patient().is_on_or_between(
          maximum_of(dataset.covid_secondary_date + days(14), covid_secondary_sens_second_date - days(30)),
          covid_secondary_sens_second_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    otherwise = False)
  )
  
  #first define inclusion from specific phenotype
  covid_secondary_spec_second = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.covid_secondary_codelist)
    .where(hospital_events.admission_date.is_on_or_after(dataset.covid_secondary_date + days(14)))
    .admission_date.minimum_for_patient()
  )
  #then extract date - prioritising inclusion from specific phenotype
  dataset.covid_secondary_second_date = (case(
    when(covid_secondary_spec_second.is_not_null())
    .then(covid_secondary_spec_second),
    when((covid_secondary_spec_second.is_null()) & (~covid_exclusion_secondary_second))
    .then(covid_secondary_sens_second_date),
    otherwise = None)
  )
  
  #get discharge date for second episode
  covid_secondary_discharge_second = (case(
    when(covid_secondary_spec_second.is_not_null())
    .then(
      hospitalisation_primary_secondary_diagnosis_matches(codelists.covid_secondary_codelist)
      .where(
        hospital_events.admission_date.is_on_or_between(
          dataset.covid_secondary_second_date, dataset.covid_secondary_second_date
        )
      ).discharge_date.minimum_for_patient()
    ),
    when( covid_secondary_spec_second.is_null())
    .then(
      hospitalisation_primary_secondary_diagnosis_matches(codelists.coronavirus_unspecified)
      .where(
        hospital_events.admission_date.is_on_or_between(
          dataset.covid_secondary_second_date, dataset.covid_secondary_second_date
        )
      ).discharge_date.minimum_for_patient()
    ),
    otherwise = None)
  )
  
  #extract length of stay for second episode, in hours
  dataset.covid_los_second = (
    diff_dates_hours(dataset.covid_secondary_second_date, covid_secondary_discharge_second)
  )

##unspecified respiratory virus outcomes

#count number of clinical codes in overall respiratory symptom list which occur within 14 days
#get dates of events and corresponding codes 
overall_resp_codes_date, overall_resp_code = (
  get_codes_dates("respiratory_virus_primary_codelist", 4, index_date, 2)
)

#occurrence of event in exclusion list within one month of overall_resp_codes_date
# - looking at the first date for which there is a code in the respiratory virus
#primary exclusion codelist within one month before or after the date of
#overall_resp_codes_date
overall_resp_exclusion_primary = (case(
  when(
    is_gp_event(codelists.respiratory_virus_primary_exclusion_codelist)
    .where(gp_events.date.is_on_or_between(overall_resp_codes_date - days(30), overall_resp_codes_date + days(30)))
    .exists_for_patient()
  )
  .then(True),
  otherwise = False)
)

#older adults
if cohort == "older_adults" :

  #extract date of first episode - looking at the first date for which there is
  #a code in the respiratory virus secondary codelist (in any position), a 
  #code in the COPD exacerbation secondary codelist (in any position), or a 
  #code in the asthma exacerbation secondary codelist (in any position)
  overall_resp_secondary_sens_date = (
    minimum_of(
      (hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
        .admission_date.minimum_for_patient()),
      (hospitalisation_primary_secondary_diagnosis_matches(codelists.copd_exacerbation_secondary_codelist)
        .admission_date.minimum_for_patient()),
      (hospitalisation_primary_secondary_diagnosis_matches(codelists.asthma_exacerbation_secondary_codelist)
        .admission_date.minimum_for_patient())
    )
  )
  
  #occurrence of event in exclusion list within one month of overall_resp_secondary_sens_date
  # - looking at the first date for which there is a code in the respiratory virus
  #secondary exclusion codelist within one month before or after the date of
  #overall_resp_secondary_sens_date
  overall_resp_exclusion_secondary = (case(
    when(
      hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_exclusion_codelist)
      .where(
        hospital_events.admission_date.is_on_or_between(
          overall_resp_secondary_sens_date - days(30), overall_resp_secondary_sens_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    otherwise = False)
  )

  #covid seasons
  if study_start_date >= covid_season_min :

    #extract unspecified respiratory infection primary care dates for 'sensitive' phenotype
    
    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a RSV primary episode, flu primary episode,
    #covid primary episode, a code in the respiratory virus primary codelist,
    #a code in emergency care for a respiratory tract infection, a code in 
    #emergency care for a COPD exacerbation, a code in emergency care for an 
    #asthma exacerbation, a code in primary care for a COPD exacerbation,
    #or a code in primary care for an asthma exacerbation
    dataset.overall_resp_primary_date = (case(
      when(
        (dataset.rsv_primary_date.is_not_null())|
        (dataset.flu_primary_date.is_not_null())|
        (dataset.covid_primary_date.is_not_null())
      )
      .then(minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date, dataset.covid_primary_date)),
      when(
        (dataset.rsv_primary_date.is_null()) &
        (dataset.flu_primary_date.is_null()) &
        (dataset.covid_primary_date.is_null()) &
        (~overall_resp_exclusion_primary)
      )
      .then(
        minimum_of(
          (emergency_care_diagnosis_matches(codelists.rtri_attendance)
            .arrival_date.minimum_for_patient()),
          (emergency_care_diagnosis_matches(codelists.copd_exacerbation_attendance)
            .arrival_date.minimum_for_patient()),
          (first_gp_event(codelists.copd_exacerbation_primary_codelist).date),
          (first_gp_event(codelists.asthma_exacerbation_primary_codelist).date)
        )
      ),
      otherwise = None)
    ) 
    
    #count number of clinical codes in overall respiratory symptom list 
    # - for second episode and date of first occurrence of two of the
    # codes within 2 weeks - for second episode
    overall_resp_codes_second_date, overall_resp_code_second = (
      get_codes_dates("respiratory_virus_primary_codelist", 4,
                      dataset.overall_resp_primary_date + days(14), 2)
    )
    
    #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
    # - using the same criteria as the first episode
    overall_resp_exclusion_primary_second = (case(
      when(
        is_gp_event(codelists.respiratory_virus_primary_exclusion_codelist)
        .where(
          gp_events.date.is_on_or_between(
            maximum_of(dataset.overall_resp_primary_date + days(14), overall_resp_codes_second_date - days(30)),
            overall_resp_codes_second_date + days(30)
          )
        ).exists_for_patient()
      )
      .then(True),
      otherwise = False)
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.overall_resp_primary_second_date = (case(
      when(
        (dataset.rsv_primary_second_date.is_not_null())|
        (dataset.flu_primary_second_date.is_not_null())|
        (dataset.covid_primary_second_date.is_not_null())
      )
      .then(minimum_of(dataset.rsv_primary_second_date, dataset.flu_primary_second_date, dataset.covid_primary_second_date)),
      when(
        (dataset.rsv_primary_second_date.is_null()) &
        (dataset.flu_primary_second_date.is_null()) &
        (dataset.covid_primary_second_date.is_null()) &
        (~overall_resp_exclusion_primary_second)
      )
      .then(
        minimum_of(
          (is_gp_event(codelists.respiratory_virus_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .sort_by(gp_events.date).first_for_patient().date),
          (overall_resp_codes_second_date),
          (emergency_care_diagnosis_matches(codelists.rtri_attendance)
            .where(emergency_events.arrival_date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .arrival_date.minimum_for_patient()),
          (emergency_care_diagnosis_matches(codelists.copd_exacerbation_attendance)
            .where(emergency_events.arrival_date.is_on_or_after(dataset.rsv_primary_date + days(14)))
            .arrival_date.minimum_for_patient()),
          (is_gp_event(codelists.copd_exacerbation_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .sort_by(gp_events.date).first_for_patient().date),
          (is_gp_event(codelists.asthma_exacerbation_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .sort_by(gp_events.date).first_for_patient().date)
        )
      ),
      otherwise = None)
    )

    #extract unspecified respiratory infection secondary care dates for 'sensitive' phenotype
    
    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a RSV secondary episode, flu secondary episode,
    #covid secondary episode or overall_resp_secondary_sens_date
    dataset.overall_resp_secondary_date = (case(
      when(
        (dataset.rsv_secondary_date.is_not_null())|
        (dataset.flu_secondary_date.is_not_null())|
        (dataset.covid_secondary_date.is_not_null())
      )
      .then(minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date, dataset.covid_secondary_date)),
      when(
        (dataset.rsv_secondary_date.is_null()) &
        (dataset.flu_secondary_date.is_null()) &
        (dataset.covid_secondary_date.is_null()) &
        (~overall_resp_exclusion_secondary)
      )
      .then(overall_resp_secondary_sens_date),
      otherwise = None)
    ) 
    
    #get discharge date for first episode
    overall_resp_secondary_discharge = (case(
      when(dataset.overall_resp_secondary_date == dataset.rsv_secondary_date)
      .then(rsv_secondary_discharge), 
      when(dataset.overall_resp_secondary_date == dataset.flu_secondary_date)
      .then(flu_secondary_discharge),
      when(dataset.overall_resp_secondary_date == dataset.covid_secondary_date)
      .then(covid_secondary_discharge),
      when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
      .then(
        hospital_events.sort_by(hospital_events.admission_date)
        .where(
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist).exists_for_patient())|
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.copd_exacerbation_secondary_codelist).exists_for_patient())|
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.asthma_exacerbation_secondary_codelist).exists_for_patient())
        )
        .where(
          hospital_events.admission_date.is_on_or_between(
            dataset.overall_resp_secondary_date, dataset.overall_resp_secondary_date
          )
        ).first_for_patient().discharge_date
      ),
      otherwise = None)
    )
    
    #extract length of stay for first episode, in hours
    dataset.overall_resp_los = (
      diff_dates_hours(dataset.overall_resp_secondary_date, overall_resp_secondary_discharge)
    ) 
    
    #extract date of second episode - using the same criteria as the first episode
    overall_resp_secondary_sens_second_date = (
      minimum_of(
        (hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
          .where(hospital_events.admission_date.is_on_or_after(dataset.overall_resp_secondary_date + days(14)))
          .admission_date.minimum_for_patient()),
        (hospitalisation_primary_secondary_diagnosis_matches(codelists.copd_exacerbation_secondary_codelist)
          .where(hospital_events.admission_date.is_on_or_after(dataset.overall_resp_secondary_date + days(14)))
          .admission_date.minimum_for_patient()),
        (hospitalisation_primary_secondary_diagnosis_matches(codelists.asthma_exacerbation_secondary_codelist)
          .where(hospital_events.admission_date.is_on_or_after(dataset.overall_resp_secondary_date + days(14)))
          .admission_date.minimum_for_patient())
      )
    )
    
    #occurrence of event in exclusion list within one month of an occurrence
    #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
    overall_resp_exclusion_secondary_second = (case(
      when(
        hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_exclusion_codelist)
        .where(
          hospital_events.admission_date.is_on_or_between(
            maximum_of(dataset.overall_resp_secondary_date + days(14), overall_resp_secondary_sens_second_date - days(30)),
            overall_resp_secondary_sens_second_date + days(30)
          )
        ).exists_for_patient()
      )
      .then(True),
      otherwise = False)  
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.overall_resp_secondary_second_date = (case(
      when(
        (dataset.rsv_secondary_second_date.is_not_null())|
        (dataset.flu_secondary_second_date.is_not_null())|
        (dataset.covid_secondary_second_date.is_not_null())
      )
      .then(minimum_of(dataset.rsv_secondary_second_date, dataset.flu_secondary_second_date, dataset.covid_secondary_second_date)),
      when(
        (dataset.rsv_secondary_second_date.is_null()) &
        (dataset.flu_secondary_second_date.is_null()) &
        (dataset.covid_secondary_second_date.is_null()) &
        (~overall_resp_exclusion_secondary_second)
      )
      .then(overall_resp_secondary_sens_second_date),
      otherwise = None)
    ) 
    
    #get discharge date for second episode
    overall_resp_secondary_discharge_second = (case(
      when(dataset.overall_resp_secondary_second_date == dataset.rsv_secondary_second_date)
      .then(rsv_secondary_discharge_second), 
      when(dataset.overall_resp_secondary_second_date == dataset.flu_secondary_second_date)
      .then(flu_secondary_discharge_second),
      when(dataset.overall_resp_secondary_second_date == dataset.covid_secondary_second_date)
      .then(covid_secondary_discharge_second),
      when(dataset.overall_resp_secondary_second_date == overall_resp_secondary_sens_second_date)
      .then(
        hospital_events.sort_by(hospital_events.admission_date)
        .where(
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist).exists_for_patient())|
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.copd_exacerbation_secondary_codelist).exists_for_patient())|
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.asthma_exacerbation_secondary_codelist).exists_for_patient())
        )
        .where(
          hospital_events.admission_date.is_on_or_between(
            dataset.overall_resp_secondary_second_date, dataset.overall_resp_secondary_second_date
          )
        ).first_for_patient().discharge_date
      ),
      otherwise = None)
    )
    
    #extract length of stay for second episode, in hours
    dataset.overall_resp_los_second = (
      diff_dates_hours(dataset.overall_resp_secondary_second_date, overall_resp_secondary_discharge_second)
    )
      
  #pre covid seasons  
  else:

    #extract unspecified respiratory infection primary care dates for 'sensitive' phenotype
    
    #extract date of first episode - looking at the first date for which there is
    #a RSV primary episode, a flu primary episode, a code in the respiratory 
    #virus primary codelist, a code in emergency care for a respiratory tract
    #infection, a code in emergency care for a COPD exacerbation, a code in
    #emergency care for an asthma exacerbation, a code in primary care for a
    #COPD exacerbation, or a code in primary care for an asthma exacerbation
    dataset.overall_resp_primary_date = (case(
      when((dataset.rsv_primary_date.is_not_null())|(dataset.flu_primary_date.is_not_null()))
      .then(minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date)),
      when((dataset.rsv_primary_date.is_null()) & (dataset.flu_primary_date.is_null()) & (~overall_resp_exclusion_primary))
      .then(
        minimum_of(
          (first_gp_event(codelists.respiratory_virus_primary_codelist).date),
          (emergency_care_diagnosis_matches(codelists.rtri_attendance)
            .where(emergency_events.arrival_date.is_on_or_between(index_date, followup_end_date))
            .arrival_date.minimum_for_patient()),
          (emergency_care_diagnosis_matches(codelists.copd_exacerbation_attendance)
            .where(emergency_events.arrival_date.is_on_or_between(index_date, followup_end_date))
            .arrival_date.minimum_for_patient()),
          (overall_resp_codes_date),
          (first_gp_event(codelists.copd_exacerbation_primary_codelist).date),
          (first_gp_event(codelists.asthma_exacerbation_primary_codelist).date)
        )
      ),
      otherwise = None)
    )
    
    #count number of clinical codes in overall respiratory symptom list 
    # - for second episode and date of first occurrence of two of the
    # codes within 2 weeks - for second episode
    overall_resp_codes_second_date, overall_resp_code_second = (
      get_codes_dates("respiratory_virus_primary_codelist", 4,
                      dataset.overall_resp_primary_date + days(14), 2)
    )
    
    #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
    # - using the same criteria as the first episode
    overall_resp_exclusion_primary_second = (case(
      when(
        is_gp_event(codelists.respiratory_virus_primary_exclusion_codelist)
        .where(
          gp_events.date.is_on_or_between(
            maximum_of(dataset.overall_resp_primary_date + days(14), overall_resp_codes_second_date - days(30)),
            overall_resp_codes_second_date + days(30)
          )
        ).exists_for_patient()
      )
      .then(True),
      otherwise = False)
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.overall_resp_primary_second_date = (case(
      when((dataset.rsv_primary_second_date.is_not_null())|(dataset.flu_primary_second_date.is_not_null()))
      .then(minimum_of(dataset.rsv_primary_second_date, dataset.flu_primary_second_date)),
      when((dataset.rsv_primary_second_date.is_null()) & (dataset.flu_primary_second_date.is_null()) & (~overall_resp_exclusion_primary_second))
      .then(
        minimum_of(
          (is_gp_event(codelists.respiratory_virus_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .sort_by(gp_events.date).first_for_patient().date),
          (emergency_care_diagnosis_matches(codelists.rtri_attendance)
            .where(emergency_events.arrival_date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .arrival_date.minimum_for_patient()),
          (emergency_care_diagnosis_matches(codelists.copd_exacerbation_attendance)
            .where(emergency_events.arrival_date.is_on_or_after(dataset.rsv_primary_date + days(14)))
            .arrival_date.minimum_for_patient()),
          (overall_resp_codes_second_date),
          (is_gp_event(codelists.copd_exacerbation_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .sort_by(gp_events.date).first_for_patient().date),
          (is_gp_event(codelists.asthma_exacerbation_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .sort_by(gp_events.date).first_for_patient().date)
        )
      ),
      otherwise = None)
    )

    #extract unspecified respiratory infection secondary care dates for 'sensitive' phenotype

    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a RSV secondary episode, flu secondary episode,
    #or overall_resp_secondary_sens_date
    dataset.overall_resp_secondary_date = (case(
      when((dataset.rsv_secondary_date.is_not_null())|(dataset.flu_secondary_date.is_not_null()))
      .then(minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date)),
      when((dataset.rsv_secondary_date.is_null()) & (dataset.flu_secondary_date.is_null()) & (~overall_resp_exclusion_secondary))
      .then(overall_resp_secondary_sens_date),
      otherwise = None)
    ) 
    
    #get discharge date for first episode
    overall_resp_secondary_discharge = (case(
      when(dataset.overall_resp_secondary_date == dataset.rsv_secondary_date)
      .then(rsv_secondary_discharge), 
      when(dataset.overall_resp_secondary_date == dataset.flu_secondary_date)
      .then(flu_secondary_discharge),
      when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
      .then(
        hospital_events.sort_by(hospital_events.admission_date)
        .where(
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist).exists_for_patient())|
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.copd_exacerbation_secondary_codelist).exists_for_patient())|
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.asthma_exacerbation_secondary_codelist).exists_for_patient())
        ).where(
          hospital_events.discharge_date.is_on_or_between(
            dataset.overall_resp_secondary_date, dataset.overall_resp_secondary_date
          )
        ).first_for_patient().discharge_date
      ),
      otherwise = None)
    )
    
    #extract length of stay for first episode, in hours
    dataset.overall_resp_los = (
      diff_dates_hours(dataset.overall_resp_secondary_date, overall_resp_secondary_discharge)
    ) 
    
    #extract date of second episode - using the same criteria as the first episode
    overall_resp_secondary_sens_second_date = (
      minimum_of(
        (hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
          .where(hospital_events.admission_date.is_on_or_after(dataset.overall_resp_secondary_date + days(14)))
          .admission_date.minimum_for_patient()),
        (hospitalisation_primary_secondary_diagnosis_matches(codelists.copd_exacerbation_secondary_codelist)
          .where(hospital_events.admission_date.is_on_or_after(dataset.overall_resp_secondary_date + days(14)))
          .admission_date.minimum_for_patient()),
        (hospitalisation_primary_secondary_diagnosis_matches(codelists.asthma_exacerbation_secondary_codelist)
          .where(hospital_events.admission_date.is_on_or_after(dataset.overall_resp_secondary_date + days(14)))
          .admission_date.minimum_for_patient())
      )
    )
    
    #occurrence of event in exclusion list within one month of an occurrence
    #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
    overall_resp_exclusion_secondary_second = (case(
      when(
        hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_exclusion_codelist)
        .where(
          hospital_events.admission_date.is_on_or_between(
            maximum_of(dataset.overall_resp_secondary_date + days(14), overall_resp_secondary_sens_second_date - days(30)),
            overall_resp_secondary_sens_second_date + days(30)
          )
        ).exists_for_patient()
      )
      .then(True),
      otherwise = False)  
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.overall_resp_secondary_second_date = (case(
      when((dataset.rsv_secondary_second_date.is_not_null())|(dataset.flu_secondary_second_date.is_not_null()))
      .then(minimum_of(dataset.rsv_secondary_second_date, dataset.flu_secondary_second_date)),
      when((dataset.rsv_secondary_second_date.is_null()) & (dataset.flu_secondary_second_date.is_null()) & (~overall_resp_exclusion_secondary))
      .then(overall_resp_secondary_sens_date),
      otherwise = None)
    ) 
    
    #get discharge date for second episode
    overall_resp_secondary_discharge_second = (case(
      when(dataset.overall_resp_secondary_second_date == dataset.rsv_secondary_second_date)
      .then(rsv_secondary_discharge_second), 
      when(dataset.overall_resp_secondary_second_date == dataset.flu_secondary_second_date)
      .then(flu_secondary_discharge_second),
      when(dataset.overall_resp_secondary_second_date == overall_resp_secondary_sens_second_date)
      .then(
        hospital_events.sort_by(hospital_events.admission_date)
        .where(
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist).exists_for_patient())|
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.copd_exacerbation_secondary_codelist).exists_for_patient())|
          (hospitalisation_primary_secondary_diagnosis_matches(codelists.asthma_exacerbation_secondary_codelist).exists_for_patient())
        ).where(
          hospital_events.discharge_date.is_on_or_between(
            dataset.overall_resp_secondary_second_date, dataset.overall_resp_secondary_second_date
          )
        ).first_for_patient().discharge_date
      ),
      otherwise = None)
    )
    
    #extract length of stay for second episode, in hours
    dataset.overall_resp_los_second = (
      diff_dates_hours(dataset.overall_resp_secondary_second_date, overall_resp_secondary_discharge_second)
    ) 

    
#cohorts that are not older adults    
else:

  #extract date of first episode - looking at the first date for which there is
  #a code in the respiratory virus secondary codelist (in any position)
  overall_resp_secondary_sens_date = (
    hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
    .admission_date.minimum_for_patient()
  )
  
  #occurrence of event in exclusion list within one month of overall_resp_secondary_sens_date
  # - looking at the first date for which there is a code in the respiratory virus
  #secondary exclusion codelist within one month before or after the date of
  #overall_resp_secondary_sens_date
  overall_resp_exclusion_secondary = (case(
    when(
      hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_exclusion_codelist)
      .where(
        hospital_events.admission_date.is_on_or_between(
          overall_resp_secondary_sens_date - days(30), overall_resp_secondary_sens_date + days(30)
        )
      ).exists_for_patient()
    )
    .then(True),
    otherwise = False)
  )
  
  #covid seasons
  if study_start_date >= covid_season_min :

    #extract unspecified respiratory infection primary care dates for 'sensitive' phenotype
    
    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a RSV primary episode, flu primary episode,
    #covid primary episode, a code in the respiratory virus primary codelist,
    #or a code in emergency care for a respiratory tract infection
    dataset.overall_resp_primary_date = (case(
      when(
        (dataset.rsv_primary_date.is_not_null())|
        (dataset.flu_primary_date.is_not_null())|
        (dataset.covid_primary_date.is_not_null())
      )
      .then(minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date, dataset.covid_primary_date)),
      when(
        (dataset.rsv_primary_date.is_null()) &
        (dataset.flu_primary_date.is_null()) &
        (dataset.covid_primary_date.is_null()) &
        (~overall_resp_exclusion_primary)
      )
      .then(
        minimum_of(
          (first_gp_event(codelists.respiratory_virus_primary_codelist).date), 
          (emergency_care_diagnosis_matches(codelists.rtri_attendance)
            .arrival_date.minimum_for_patient())
        )
      ),
      otherwise = None)
    )
    
    #count number of clinical codes in overall respiratory symptom list 
    # - for second episode and date of first occurrence of two of the
    # codes within 2 weeks - for second episode
    overall_resp_codes_second_date, overall_resp_code_second = (
      get_codes_dates("respiratory_virus_primary_codelist", 4,
                      dataset.overall_resp_primary_date + days(14), 2)
    )
    #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
    # - using the same criteria as the first episode
    overall_resp_exclusion_primary_second = (case(
      when(
        is_gp_event(codelists.respiratory_virus_primary_exclusion_codelist)
        .where(
          gp_events.date.is_on_or_between(
            maximum_of(dataset.overall_resp_primary_date + days(14), overall_resp_codes_second_date - days(30)),
            overall_resp_codes_second_date + days(30)
          )
        ).exists_for_patient()
      )
      .then(True),
      otherwise = False)
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.overall_resp_primary_second_date = (case(
      when(
        (dataset.rsv_primary_second_date.is_not_null())|
        (dataset.flu_primary_second_date.is_not_null())|
        (dataset.covid_primary_second_date.is_not_null())
      )
      .then(minimum_of(dataset.rsv_primary_second_date, dataset.flu_primary_second_date, dataset.covid_primary_second_date)),
      when(
        (dataset.rsv_primary_second_date.is_null()) &
        (dataset.flu_primary_second_date.is_null()) &
        (dataset.covid_primary_second_date.is_null()) &
        (~overall_resp_exclusion_primary_second)
      )
      .then(
        minimum_of(
          (is_gp_event(codelists.respiratory_virus_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .sort_by(gp_events.date).first_for_patient().date),
          (overall_resp_codes_second_date),
          (emergency_care_diagnosis_matches(codelists.rtri_attendance)
            .where(emergency_events.arrival_date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .arrival_date.minimum_for_patient())
        )
      ),
      otherwise = None)
    )

    #extract unspecified respiratory infection secondary care dates for 'sensitive' phenotype
    
    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a RSV secondary episode, flu secondary episode,
    #covid secondary episode or overall_resp_secondary_sens_date
    dataset.overall_resp_secondary_date = (case(
      when(
        (dataset.rsv_secondary_date.is_not_null())|
        (dataset.flu_secondary_date.is_not_null())|
        (dataset.covid_secondary_date.is_not_null())
      )
      .then(minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date, dataset.covid_secondary_date)),
      when(
        (dataset.rsv_secondary_date.is_null()) &
        (dataset.flu_secondary_date.is_null()) &
        (dataset.covid_secondary_date.is_null()) &
        (~overall_resp_exclusion_secondary)
      )
      .then(overall_resp_secondary_sens_date),
      otherwise = None)
    )
    
    #get discharge date for first episode
    overall_resp_secondary_discharge = (case(
      when(dataset.overall_resp_secondary_date == dataset.rsv_secondary_date)
      .then(rsv_secondary_discharge), 
      when(dataset.overall_resp_secondary_date == dataset.flu_secondary_date)
      .then(flu_secondary_discharge),
      when(dataset.overall_resp_secondary_date == dataset.covid_secondary_date)
      .then(covid_secondary_discharge),
      when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
      .then(
        hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
        .where(
          hospital_events.admission_date.is_on_or_between(
            dataset.overall_resp_secondary_date, dataset.overall_resp_secondary_date
          )
        ).discharge_date.minimum_for_patient()
      ),
      otherwise = None)
    )
    
    #extract length of stay for first episode, in hours
    dataset.overall_resp_los = (
      diff_dates_hours(dataset.overall_resp_secondary_date, overall_resp_secondary_discharge)
    ) 
    
    #extract date of second episode - using the same criteria as the first episode
    overall_resp_secondary_sens_second_date = (
      hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
      .where(hospital_events.admission_date.is_on_or_after(dataset.overall_resp_secondary_date + days(14)))
      .admission_date.minimum_for_patient()
    )
    
    #occurrence of event in exclusion list within one month of an occurrence
    #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
    overall_resp_exclusion_secondary_second = (case(
      when(
        hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_exclusion_codelist)
        .where(
          hospital_events.admission_date.is_on_or_between(
            maximum_of(dataset.overall_resp_secondary_date + days(14), overall_resp_secondary_sens_second_date - days(30)),
            overall_resp_secondary_sens_second_date + days(30)
          )
        ).exists_for_patient()
      )
      .then(True),
      otherwise = False)  
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.overall_resp_secondary_second_date = (case(
      when(
        (dataset.rsv_secondary_second_date.is_not_null())|
        (dataset.flu_secondary_second_date.is_not_null())|
        (dataset.covid_secondary_second_date.is_not_null())
      )
      .then(minimum_of(dataset.rsv_secondary_second_date, dataset.flu_secondary_second_date, dataset.covid_secondary_second_date)),
      when(
        (dataset.rsv_secondary_second_date.is_null()) &
        (dataset.flu_secondary_second_date.is_null()) &
        (dataset.covid_secondary_second_date.is_null()) &
        (~overall_resp_exclusion_secondary_second)
      )
      .then(overall_resp_secondary_sens_second_date),
      otherwise = None)
    )
    
    #get discharge date for second episode
    overall_resp_secondary_discharge_second = (case(
      when(dataset.overall_resp_secondary_second_date == dataset.rsv_secondary_second_date)
      .then(rsv_secondary_discharge_second), 
      when(dataset.overall_resp_secondary_second_date == dataset.flu_secondary_second_date)
      .then(flu_secondary_discharge_second),
      when(dataset.overall_resp_secondary_second_date == dataset.covid_secondary_second_date)
      .then(covid_secondary_discharge_second),
      when(dataset.overall_resp_secondary_second_date == overall_resp_secondary_sens_second_date)
      .then(
        hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
        .where(
          hospital_events.admission_date.is_on_or_between(
            dataset.overall_resp_secondary_second_date, dataset.overall_resp_secondary_second_date
          )
        ).discharge_date.minimum_for_patient()
      ),
      otherwise = None)
    )
    
    #extract length of stay for second episode, in hours
    dataset.overall_resp_los_second = (
      diff_dates_hours(dataset.overall_resp_secondary_second_date, overall_resp_secondary_discharge_second)
    )
    
  #pre-covid seasons  
  else:

    #extract unspecified respiratory infection primary care dates for 'sensitive' phenotype
    
    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a RSV primary episode, flu primary episode,
    #a code in the respiratory virus primary codelist, or a code in emergency care
    #for a respiratory tract infection
    dataset.overall_resp_primary_date = (case(
      when((dataset.rsv_primary_date.is_not_null())|(dataset.flu_primary_date.is_not_null()))
      .then(minimum_of(dataset.rsv_primary_date, dataset.flu_primary_date)),
      when((dataset.rsv_primary_date.is_null()) & (dataset.flu_primary_date.is_null()) & (~overall_resp_exclusion_primary))
      .then(
        minimum_of(
          (first_gp_event(codelists.respiratory_virus_primary_codelist).date),
          (emergency_care_diagnosis_matches(codelists.rtri_attendance)
            .arrival_date.minimum_for_patient())
        )
      ),
      otherwise = None)
    )
    
    #count number of clinical codes in overall respiratory symptom list 
    # - for second episode and date of first occurrence of two of the
    # codes within 2 weeks - for second episode
    overall_resp_codes_second_date, overall_resp_code_second = (
      get_codes_dates("respiratory_virus_primary_codelist", 4,
                      dataset.overall_resp_primary_date + days(14), 2)
    )
    
    #occurrence of event in exclusion list within one month of overall_resp_codes_second_date
    overall_resp_exclusion_primary_second = (case(
      when(
        is_gp_event(codelists.respiratory_virus_primary_exclusion_codelist)
        .where(
          gp_events.date.is_on_or_between(
            maximum_of(dataset.overall_resp_primary_date + days(14), overall_resp_codes_second_date - days(30)),
            overall_resp_codes_second_date + days(30)
          )
        ).exists_for_patient()
      )
      .then(True),
      otherwise = False)
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.overall_resp_primary_second_date = (case(
      when((dataset.rsv_primary_second_date.is_not_null())|(dataset.flu_primary_second_date.is_not_null()))
      .then(minimum_of(dataset.rsv_primary_second_date, dataset.flu_primary_second_date)),
      when((dataset.rsv_primary_second_date.is_null()) & (dataset.flu_primary_second_date.is_null()) & (~overall_resp_exclusion_primary_second))
      .then(
        minimum_of(
          (is_gp_event(codelists.respiratory_virus_primary_codelist)
            .where(gp_events.date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .sort_by(gp_events.date).first_for_patient().date),
          (overall_resp_codes_second_date),
          (emergency_care_diagnosis_matches(codelists.rtri_attendance)
            .where(emergency_events.arrival_date.is_on_or_after(dataset.overall_resp_primary_date + days(14)))
            .arrival_date.minimum_for_patient())
        )
      ),
      otherwise = None)
    )

    #extract unspecified respiratory infection secondary care dates for 'sensitive' phenotype

    #extract date of first episode where the exclusion criteria is not met
    # - get the first date of either a RSV secondary episode, flu secondary episode,
    #or overall_resp_secondary_sens_date
    dataset.overall_resp_secondary_date = (case(
      when((dataset.rsv_secondary_date.is_not_null())|(dataset.flu_secondary_date.is_not_null()))
      .then(minimum_of(dataset.rsv_secondary_date, dataset.flu_secondary_date)),
      when((dataset.rsv_secondary_date.is_null()) & (dataset.flu_secondary_date.is_null()) & (~overall_resp_exclusion_secondary))
      .then(overall_resp_secondary_sens_date),
      otherwise = None)
    )
    
    #get discharge date for first episode
    overall_resp_secondary_discharge = (case(
      when(dataset.overall_resp_secondary_date == dataset.rsv_secondary_date)
      .then(rsv_secondary_discharge), 
      when(dataset.overall_resp_secondary_date == dataset.flu_secondary_date)
      .then(flu_secondary_discharge),
      when(dataset.overall_resp_secondary_date == overall_resp_secondary_sens_date)
      .then(
        hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
        .where(
          hospital_events.admission_date.is_on_or_between(
            dataset.overall_resp_secondary_date, dataset.overall_resp_secondary_date
          )
        ).discharge_date.minimum_for_patient()
      ),
      otherwise = None)
    )
    
    #extract length of stay for first episode, in hours
    dataset.overall_resp_los = (
      diff_dates_hours(dataset.overall_resp_secondary_date, overall_resp_secondary_discharge)
    ) 
    
    #extract date of second episode - using the same criteria as the first episode
    overall_resp_secondary_sens_second_date = (
      hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
      .where(hospital_events.admission_date.is_on_or_after(dataset.overall_resp_secondary_date + days(14)))
      .admission_date.minimum_for_patient()
    )
    
    #occurrence of event in exclusion list within one month of an occurrence
    #of overall_resp_secondary_sens_second_date - using the same criteria as the first episode
    overall_resp_exclusion_secondary_second = (case(
      when(
        hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_exclusion_codelist)
        .where(
          hospital_events.admission_date.is_on_or_between(
            maximum_of(dataset.overall_resp_secondary_date + days(14), overall_resp_secondary_sens_second_date - days(30)),
            overall_resp_secondary_sens_second_date + days(30)
          )
        ).exists_for_patient()
      )
      .then(True),
      otherwise = False)  
    )
    
    #extract date of second episode - using the same criteria as the first episode
    dataset.overall_resp_secondary_second_date = (case(
      when((dataset.rsv_secondary_second_date.is_not_null())|(dataset.flu_secondary_second_date.is_not_null()))
      .then(minimum_of(dataset.rsv_secondary_second_date, dataset.flu_secondary_second_date)),
      when((dataset.rsv_secondary_second_date.is_null()) & (dataset.flu_secondary_second_date.is_null()) & (~overall_resp_exclusion_secondary_second))
      .then(overall_resp_secondary_sens_second_date),
      otherwise = None)
    ) 
    
    #get discharge date for second episode
    overall_resp_secondary_discharge_second = (case(
      when(dataset.overall_resp_secondary_second_date == dataset.rsv_secondary_second_date)
      .then(rsv_secondary_discharge_second), 
      when(dataset.overall_resp_secondary_second_date == dataset.flu_secondary_second_date)
      .then(flu_secondary_discharge_second),
      when(dataset.overall_resp_secondary_second_date == overall_resp_secondary_sens_second_date)
      .then(
        hospitalisation_primary_secondary_diagnosis_matches(codelists.respiratory_virus_secondary_codelist)
        .where(
          hospital_events.admission_date.is_on_or_between(
            dataset.overall_resp_secondary_second_date, dataset.overall_resp_secondary_second_date
          )
        ).discharge_date.minimum_for_patient()
      ),
      otherwise = None)
    )
    
    #extract length of stay for second episode, in hours
    dataset.overall_resp_los_second = (
      diff_dates_hours(dataset.overall_resp_secondary_second_date, overall_resp_secondary_discharge_second)
    )
