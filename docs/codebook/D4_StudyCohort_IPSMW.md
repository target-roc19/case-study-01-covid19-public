## Metadata

| Table name                | D4_StudyCohort_IPSMW_`<Main/Supplementary>`                                                          |
| ------------------------- | ---------------------------------------------------------------------------------------------------- |
| Description               | Person-level principal stratum scores and inverse probability of stratum membership weights (IPSMW). |
| Source                    | D4_StudyCohort`<Main/Supplementary>`; D4_StudyCohortFup`<Main/Supplementary>`; ipsmw_variables.csv   |
| Step                      | transformations/T4_statistical_analysis/source_code/compute_ipsmw.R                                  |
| Content                   | Principal stratum score for controls and IPSMW for estimand 2 weighting                              |
| Population                | Matched exposed and matched controls                                                                 |
| Unit of Observation (UoO) | One row per person_id, match_id, and T0                                                              |

## Codebook

| variable name     | Description                                                                                       | variable(s) needed                    | retrieved | computed | format    | rule_of_calculation                                                     | vocabulary       |
| ----------------- | ------------------------------------------------------------------------------------------------- | ------------------------------------- | --------- | -------- | --------- | ----------------------------------------------------------------------- | ---------------- |
| person_id         | Person identifier                                                                                 | D4_StudyCohort::person_id             | x         |          | Character |                                                                         |                  |
| match_id          | Matching identifier                                                                               | D4_StudyCohort::match_id              | x         |          | Character |                                                                         |                  |
| T0                | Start of follow-up                                                                                | D4_StudyCohort::T0                    | x         |          | Date      |                                                                         |                  |
| group             | Group indicator                                                                                   | D4_StudyCohort::group                 | x         |          | Character |                                                                         | EXPOSED, CONTROL |
| analysis          | Analysis cohort label                                                                             |                                       |           | x        | Character | Main or Supplementary                                                   |                  |
| compliant_exposed | Compliance indicator for exposed individuals                                                      | ie1_second_dose_target_exposed, group |           | x        | Logical   | IF group == EXPOSED THEN !is.na(ie1_second_dose_target_exposed) ELSE NA | TRUE, FALSE      |
| psm_score         | Predicted probability of belonging to the compliant exposed stratum (predicted for controls only) | IPSMW model                           |           | x        | Numeric   | predict(model, type = "response") for controls; set to NA for exposed   | (0, 1)           |
| ipsmw_raw         | Inverse probability of stratum membership weight                                                  | psm_score, group                      |           | x        | Numeric   | IF group == CONTROL THEN psm_score/(1-psm_score) ELSE 1                 | > 0              |
| ipsmw_raw         | Stabilized IPSMW                                                                                  | ipsmw_raw                             |           | x        | Numeric   | ipsmw_raw := ipsmw_raw                                                  | > 0              |
| include_estimand2 | Indicator for estimand 2 weighted analysis population                                             | group, compliant_exposed              |           | x        | Logical   | TRUE if group == CONTROL OR compliant_exposed == TRUE                   | TRUE, FALSE      |

## Processing Steps

1. Merge D4 study cohort and follow-up datasets by person_id, T0, and group.
2. Derive compliant_exposed from ie1_second_dose_target_exposed for exposed only.
3. Fit logistic model on exposed only with compliant_exposed as outcome and baseline covariates from ipsmw_variables metadata.
4. Predict principal stratum scores for controls.
5. Compute IPSMW: controls psm_score/(1-psm_score), exposed fixed to 1.
6. Mark estimand-2 analysis set as compliant exposed plus all controls.
