## Metadata

| Table name                | D4_StudyCohort_IPCW_<Main/Supplementary>_Estimand<1/2>                                                                                                 |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Description               | Person-time table with pooled logistic censoring probabilities and cumulative IPCW for weighted analyses.                                              |
| Source                    | D4_StudyCohort<Main/Supplementary>; D4_StudyCohortFup<Main/Supplementary>; D4_StudyCohortTimeVaryingCovariates<Main/Supplementary>; ipcw_variables.csv |
| Step                      | transformations/T4_statistical_analysis/source_code/compute_ipcw.R                                                                                     |
| Content                   | Per-interval censoring indicators, numerator/denominator probabilities, and cumulative (trimmed) IPCW.                                                 |
| Population                | Matched exposed and matched controls with person-time expansion.                                                                                       |
| Unit of Observation (UoO) | One row per person_id, T0, and time_days interval.                                                                                                     |

## Codebook

| variable name   | Description                                                    | Format    | Vocabulary          | Variables from source used for the variable |
| --------------- | -------------------------------------------------------------- | --------- | ------------------- | ------------------------------------------- |
| person_id       | Person identifier                                              | Character |                     |                                             |
| T0              | Start of follow-up                                             | Date      |                     |                                             |
| group           | Group indicator                                                | Character | EXPOSED, CONTROL    |                                             |
| match_id        | Matching identifier when available                             | Character |                     |                                             |
| time_days       | Follow-up day index from T0                                    | Integer   | >= 1                | time_days (selected by script)              |
| censor_nonadmin | Non-administrative censoring indicator for the interval        | Integer   | 0, 1                | censor_nonadmin_*                           |
| outcome_event   | Outcome indicator at interval end                              | Integer   | 0, 1                | event_count_*, fup_length_days_*            |
| p_num           | Predicted censoring probability from numerator model           | Numeric   | (0, 1)              | numerator pooled logistic model             |
| p_den           | Predicted censoring probability from denominator model         | Numeric   | (0, 1)              | denominator pooled logistic model           |
| p_obs_num       | Probability of observed censoring process in numerator model   | Numeric   | (0, 1)              | p_num, censor_nonadmin                      |
| p_obs_den       | Probability of observed censoring process in denominator model | Numeric   | (0, 1)              | p_den, censor_nonadmin                      |
| ipcw_step       | Interval-specific stabilized IPCW factor                       | Numeric   | > 0                 | p_obs_num / p_obs_den                       |
| ipcw_cum        | Cumulative IPCW through the interval                           | Numeric   | > 0                 | cumprod(ipcw_step) by person                |
| ipcw_cum_trim   | Truncated cumulative IPCW (1st to 99th percentile)             | Numeric   | > 0                 | ipcw_cum with quantile truncation           |
| analysis        | Analysis cohort label                                          | Character | Main, Supplementary |                                             |
