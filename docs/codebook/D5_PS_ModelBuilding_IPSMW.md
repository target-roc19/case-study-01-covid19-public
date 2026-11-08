## Metadata

| Table name                | D5_PS_ModelBuilding_IPSMW_`<Main/Supplementary>`                                                   |
| ------------------------- | -------------------------------------------------------------------------------------------------- |
| Description               | Inclusion and exclusion diagnostics for IPSMW principal stratum model covariates.                  |
| Source                    | D4_StudyCohort`<Main/Supplementary>`; D4_StudyCohortFup`<Main/Supplementary>`; ipsmw_variables.csv |
| Step                      | transformations/T4_statistical_analysis/source_code/compute_ipsmw.R                                |
| Content                   | Model-building checks and reasons for variable exclusion                                           |
| Population                | Candidate baseline covariates for IPSMW model                                                      |
| Unit of Observation (UoO) | One row per candidate variable                                                                     |

## Codebook

| Variable name    | Description                                                           | format    | vocabulary                                                           |
| ---------------- | --------------------------------------------------------------------- | --------- | -------------------------------------------------------------------- |
| variable_id      | Candidate variable name from metadata                                 | Character |                                                                      |
| var_type         | Variable type from metadata                                           | Character | NUM, CAT, TF                                                         |
| force_in         | Force variable in regardless of data-driven checks                    | Logical   | TRUE, FALSE                                                          |
| force_out        | Force variable out regardless of data-driven checks                   | Logical   | TRUE, FALSE                                                          |
| exists_in_data   | Variable present in analysis data                                     | Logical   | TRUE, FALSE                                                          |
| missing_pct      | Percent missing in exposed analysis subset                            | Numeric   | [0, 100]                                                             |
| nonzero_variance | Variable has at least two unique non-missing values in exposed subset | Logical   | TRUE, FALSE                                                          |
| include_r1       | Included in initial model fit                                         | Logical   | TRUE, FALSE                                                          |
| included_r2      | Included in final model                                               | Logical   | TRUE, FALSE                                                          |
| reason_excluded  | Reason variable is not included in final model                        | Character | missing_in_data, zero_variance, force_out, non_convergence, large_se |
| analysis         | Analysis cohort label (Main or Supplementary)                         | Character | Main, Supplementary                                                  |
