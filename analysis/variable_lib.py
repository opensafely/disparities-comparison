import operator
from functools import reduce

from ehrql.codes import SNOMEDCTCode, CTV3Code, ICD10Code
from ehrql import case, days, when
from ehrql.tables.tpp import (emergency_care_attendances, 
apcs, clinical_events, patients)

###############################################################################
# from https://github.com/opensafely/comparative-booster-spring2023/blob/main/analysis/variables_lib.py
###############################################################################

def _registrations_overlapping_period(start_date, end_date):
    regs = practice_registrations
    return regs.where(
        regs.start_date.is_on_or_before(start_date)
        & (regs.end_date.is_after(end_date) | regs.end_date.is_null())
    )

def practice_registration_as_of(date):
    regs = _registrations_overlapping_period(date, date)
    return regs.sort_by(regs.start_date, regs.end_date).first_for_patient()

def has_a_continuous_practice_registration_spanning(start_date, end_date):
    return _registrations_overlapping_period(start_date, end_date).exists_for_patient()
  
def most_recent_bmi(*, minimum_age_at_measurement, where=True):
    age_threshold = patients.date_of_birth + days(
        # This is obviously inexact but, given that the dates of birth are rounded to
        # the first of the month anyway, there's no point trying to be more accurate
        int(365.25 * minimum_age_at_measurement)
    )
    return (
        # This captures just explicitly recorded BMI observations rather than attempting
        # to calculate it from height and weight measurements. Investigation has shown
        # this to have no real benefit it terms of coverage or accuracy.
        clinical_events.where(clinical_events.ctv3_code == CTV3Code("22K.."))
        .where(clinical_events.date >= age_threshold)
        .where(where)
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

###############################################################################
# from https://github.com/opensafely/cis-pop-validation-ehrql/blob/cce4f0bfaffa5370b00f847fbcfe742c7a13aeeb/analysis/variable_lib.py#L100
###############################################################################

def any_of(conditions):
    return reduce(operator.or_, conditions)

def emergency_care_diagnosis_matches(codelist):
    conditions = [
        getattr(emergency_care_attendances, column_name).is_in(codelist)
        for column_name in [f"diagnosis_{i:02d}" for i in range(1, 25)]
    ]
    return emergency_care_attendances.where(any_of(conditions))

def hospitalisation_diagnosis_matches(codelist):
    code_strings = set()
    for code in codelist:
        # Pass the string through the ICD10Code to constructor to validate that it has
        # the expected format
        code_string = ICD10Code(code)._to_primitive_type()
        code_strings.add(code_string)
    conditions = [
        # The reason a plain substring search like this works is twofold:
        #
        # * ICD-10 codes all start with the sequence [A-Z][0-9] and do not contain
        #   such a sequence in any other part of the code. In this sense they are
        #   suffix-free and two codes will only match at they start if they match at
        #   all.
        #
        # * Although the codes are not prefix-free they are organised hierarchically
        #   such that code A0123 represents a child concept of code A01. So although
        #   the naive substring matching below will find code A01 if code A0123 is
        #   present, this happens to be the behaviour we actually want.
        #
        # Obviously this is all far from ideal though, and later we hope to be able
        # to pull these codes out in a separate table and handle the matching
        # properly.
        apcs.all_diagnoses.contains(code_string)
        for code_string in code_strings
    ]
    return apcs.where(any_of(conditions))
