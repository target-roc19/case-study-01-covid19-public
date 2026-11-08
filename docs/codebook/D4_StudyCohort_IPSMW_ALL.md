## Metadata

| Table name                | D4_StudyCohort_IPSMW_ALL_<Main/Supplementary>                                                                                           |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Person-level IPSMW dataset for the full population: all exposed and all control persons, with propensity scores and stabilised weights. |
| Source                    | D4_StudyCohort`<Main/Supplementary>`; D4_StudyCohortFup`<Main/Supplementary>`; ipsmw_variables.csv                                      |
| Step                      | transformations/T4_statistical_analysis/source_code/compute_ipsmw.R (via functions/fn_compute_ipsmw.R)                                  |
| Content                   | One row per person and T0 (both exposed and control), with PS score and IPSMW weights.                                                  |
| Population                | All exposed and all control persons in the matched cohort (not restricted to compliant exposed)                                         |
| Unit of Observation (UoO) | Person and T0                                                                                                                           |

## Codebook

| variable_in_output | variable(s) needed                   | retrieved | computed | format  | rule_of_calculation                                                        | description                                   | vocabulary          |
| ------------------ | ------------------------------------ | --------- | -------- | ------- | -------------------------------------------------------------------------- | --------------------------------------------- | ------------------- |
| person_id          | D4_StudyCohort`<Main/Supplementary>` | x         |          | string  | Carried from the study cohort                                              | Unique person identifier                      |                     |
| match_id           | D4_StudyCohort`<Main/Supplementary>` | x         |          | string  | Carried from the study cohort                                              | Matched-set identifier                        |                     |
| T0                 | D4_StudyCohort`<Main/Supplementary>` | x         |          | date    | Carried from the study cohort                                              | Time zero                                     | YYYY-MM-DD          |
| group              | D4_StudyCohort`<Main/Supplementary>` | x         |          | string  | Carried from the study cohort                                              | Treatment group                               | EXPOSED, CONTROL    |
| compliant_exposed  | compliance rule                      |           | x        | logical | Compliance flag for exposed persons                                        | Whether the exposed person is compliant       | TRUE / FALSE        |
| analysis           |                                      |           | x        | string  | Analysis label (`Main` or `Supplementary`)                                 | Analysis the row belongs to                   | Main, Supplementary |
| psm_score          | PS model (`ipsmw_variables.csv`)     |           | x        | numeric | Predicted probability from the final PS model, bounded to [1e-6, 1 - 1e-6] | Propensity score                              | 0 to 1              |
| ipsmw_raw          |                                      |           | x        | numeric | psm_score / (1 - psm_score)                                                | Unstabilised IPSMW weight                     | > 0                 |
| ipsmw_norm         |                                      |           | x        | numeric | ipsmw_raw / mean(ipsmw_raw) within group                                   | Normalised IPSMW weight (mean 1 within group) | > 0                 |
| ipsmw_raw_trunc    | ipsmw_raw                            |           | x        | numeric | ipsmw_raw truncated at the 1st and 99th percentiles of the group           | Truncated unstabilised IPSMW weight           | > 0                 |
| ipsmw_norm_trunc   | ipsmw_norm                           |           | x        | numeric | ipsmw_norm truncated at the 1st and 99th percentiles of the group          | Truncated normalised IPSMW weight             | > 0                 |
| include_estimand2  |                                      |           | x        | logical | Always TRUE for this dataset (no additional eligibility restriction)       | Inclusion flag for estimand 2                 | TRUE                |

## Processing Steps

| Step number | Description                                                                                                                             |
| ----------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| Step 1      | Score all exposed persons with the final PS model (`psm_score_all`), not only compliant exposed.                                        |
| Step 2      | Reuse the control-group scores from the compliant-exposed model fit, and compute `ipsmw_raw` and `ipsmw_norm` for exposed and controls. |
| Step 3      | Combine controls and exposed persons (`rbindlist`) and set `include_estimand2` = TRUE.                                                  |
| Step 4      | Truncate weights at the configured 1st and 99th percentiles.                                                                            |
| Step 5      | Save as `D4_StudyCohort_IPSMW_ALL_<Main/Supplementary>.parquet` in the D4 analytic datasets folder.                                     |

## Example

| person_id | match_id | T0         | group   | compliant_exposed | psm_score | ipsmw_raw | ipsmw_norm | include_estimand2 |
| --------- | -------- | ---------- | ------- | ----------------- | --------- | --------- | ---------- | ----------------- |
| 1         | 10       | 2024-11-15 | EXPOSED | TRUE              | 0.40      | 0.67      | 0.95       | TRUE              |
