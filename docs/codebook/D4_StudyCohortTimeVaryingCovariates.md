## Metadata


| Table name                | D4_StudyCohortTimeVaryingCovariates`<Main/Supplementary>`                                                                                                                                      |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Time-varying covariates measured from T0 until end of follow-up for the study cohort. Includes project-specific intercurrent event and censoring indicators for pooled logistic/IPCW analyses. |
| Source                    | D4_StudyCohort`<Main/Supplementary>`; source clinical/administrative event data                                                                                                                |
| Step                      | create_tv_covariates.R                                                                                                                                                                         |
| Content                   | Longitudinal covariate states over follow-up                                                                                                                                                   |
| Population                | Selected study cohort                                                                                                                                                                          |
| Unit of Observation (UoO) | person-time (interval-based representation)                                                                                                                                                    |

## Codebook


| variable_in_output      | variable(s) needed | retrieved | computed | format                 | rule_of_calculation                                                                                                                                                                                    | description                                                                              | vocabulary |
| ----------------------- | ------------------ | --------- | -------- | ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------- | ---------- |
| person_id               |                    | x         |          | character              |                                                                                                                                                                                                        | person_id                                                                                |            |
| T0                      |                    | x         |          | date                   |                                                                                                                                                                                                        | time-zero (moment at which eligibility is assessed, group assigned and follow up starts) |            |
| group                   |                    | x         |          | character              |                                                                                                                                                                                                        | EXPOSED or CONTROL                                                                       |            |
| match_id                |                    | x         |          | character              |                                                                                                                                                                                                        | Present only when matching is used                                                       |            |
| date                    |                    |           | x        | date                   | Calendar date of the row, daily from T0 to end of follow-up                                                                                                                                            | Calendar date                                                                            |            |
| time_days               |                    |           | x        | integer (time-varying) | Time in days since T0 up to general_end_fup_est1/est2                                                                                                                                                  | Time in days from T0 at which the time-varying event takes place                         |            |
| censor_nonadmin         |                    |           | x        | binary (time-varying)  | long fomat (time_varying) of binary indicator D4_StudyCohortFup$censor_nonadmin_ipcw; Indicator equals 1 at the end of follow up if the censoring reason is one of IE3, IE5, loss to follow-up, else 0 | Combined non-administrative censoring indicator used for IPCW                            |            |
| SV_HOSP_LENGTH_TV       | D3_CONCEPTS$PP_HOSP_LENGTH |           | x        | binary (time-varying)  | Indicator equals 1 during hospitalization episodes occurring between T0 and end of follow-up; 0 otherwise                                                                                              | Time-varying hospitalization status                                                      |            |
| SV_IMMUNOCOMPROMISED_TV | D3_CONCEPTS$PP_IMMUNOCOMPROMISED |           | x        | binary (time-varying)  | Indicator switches from 0 to 1 at first diagnosis of acute/incident immunosuppressing condition after T0 and remains 1 thereafter                                                                      | Time-varying immunosuppressing condition status (absorbing)                              |            |

Notes:

`censor_nonadmin` is the dependent variable to be fit using a formula of the form:

* Numerator: `censor_nonadmin ~ as.factor(group) + time_days + time_days_sqr`
* Denominator: `censor_nonadmin ~ as.factor(group) + time_days + time_days_sqr + baseline matching variables + sv_hospitalization_tv + sv_immunosup_diagnosis1_tv + ... + sv_immunosup_diagnosisN_tv `

Where "group" is the treatment assigned and an offset needs to be considered.

General notes about implementation:

1. IPCW in this project is applied only to non-administrative censoring:
   * Intercurrent event 3 (non-target COVID-19 vaccination per arm definition)
   * Intercurrent event 5 (other preventive COVID-19 treatment)
   * Loss to follow-up due to leaving data source
2. Intercurrent events 1 (estimand 1), 2 and 4 (both estimands) are treatment policy strategy components and do not trigger censoring in the primary analysis.
3. Death is either part of the composite outcome (estimand 1) or the while alive policy is applied (estimand 2). It is not treated as a censoring event for IPCW.
4. Row generation rule: one row per person-time unit (day) from T0 until the earliest of general_end_fup:
   * composite outcome event (Estimand 1) or while alive (Estimand 2)
   * non-administrative censoring (censor_nonadmin_ipcw = 1)
   * administrative end of follow-up
5. Row counts are therefore not expected to be equal across individuals.
6. For IPCW model fitting, censoring rows can be retained according to implementation convention, but rows after first censoring/event are not included in risk sets.
7. Covariates are aligned with outcome and censoring risk sets at each time interval.

## Example


| person_id   | T0         | group   | match_id | date       | time_days | SV_IMMUNOCOMPROMISED_TV | SV_HOSP_LENGTH_TV | censor_nonadmin |
| ----------- | ---------- | ------- | -------- | ---------- | --------- | ----------------------- | ----------------- | --------------- |
| 10000002510 | 2021-01-01 | EXPOSED | 1        | 2021-01-01 | 0         | FALSE                   | FALSE             | 0               |
| 10000002510 | 2021-01-01 | EXPOSED | 1        | 2021-01-02 | 1         | FALSE                   | FALSE             | 0               |
| 10000002510 | 2021-01-01 | EXPOSED | 1        | 2021-01-03 | 2         | FALSE                   | FALSE             | 0               |
| ...         | ...        | ...     | ...      | ...        | ...       | ...                     | ...               | ...             |
