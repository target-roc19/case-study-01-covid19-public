## Metadata

| Table name                | D5_Censor_ModelBuilding_IPCW_<Main/Supplementary>_Estimand<1/2>                                                    |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| Description               | Inclusion and exclusion diagnostics for IPCW denominator model covariates from metadata.                           |
| Source                    | D4_StudyCohortFup<Main/Supplementary>; D4_StudyCohortTimeVaryingCovariates<Main/Supplementary>; ipcw_variables.csv |
| Step                      | transformations/T4_statistical_analysis/source_code/compute_ipcw.R                                                 |
| Content                   | Data availability and variance checks with exclusion reasons per candidate variable.                               |
| Population                | Candidate baseline and time-varying covariates in IPCW metadata.                                                   |
| Unit of Observation (UoO) | One row per candidate variable.                                                                                    |

## Codebook

| Variable name    | Description                                         | Format    | Vocabulary                                |
| ---------------- | --------------------------------------------------- | --------- | ----------------------------------------- |
| variable_id      | Candidate variable name from metadata               | Character |                                           |
| var_type         | Variable type from metadata                         | Character | NUM, CAT, TF                              |
| domain           | Covariate domain                                    | Character | BASELINE, TIMEVARYING, BOTH               |
| force_in         | Force variable in regardless of data-driven checks  | Logical   | TRUE, FALSE                               |
| force_out        | Force variable out regardless of data-driven checks | Logical   | TRUE, FALSE                               |
| exists_in_data   | Variable present in assembled IPCW analysis dataset | Logical   | TRUE, FALSE                               |
| missing_pct      | Percent missing in assembled IPCW analysis dataset  | Numeric   | [0, 100]                                  |
| nonzero_variance | Variable has at least two unique non-missing values | Logical   | TRUE, FALSE                               |
| include_r1       | Included in denominator candidate set               | Logical   | TRUE, FALSE                               |
| included_r2      | Included in final denominator model                 | Logical   | TRUE, FALSE                               |
| reason_excluded  | Reason variable is not included in final model      | Character | missing_in_data, zero_variance, force_out |
| analysis         | Analysis cohort label                               | Character | Main, Supplementary                       |
