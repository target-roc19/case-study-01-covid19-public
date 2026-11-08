## Metadata

| Table name                | D5_ASD_POSTWEIGHT_IPCW_<Main/Supplementary>_Estimand<1/2>                                              |
| ------------------------- | ------------------------------------------------------------------------------------------------------ |
| Description               | Weighted absolute standardized differences (ASD) for each covariate, exposed vs control, using IPCW.   |
| Source                    | D4_StudyCohortCovariates<Main/Supplementary>; D4_StudyCohort_IPCW<Main/Supplementary>_Estimand<1/2>; covariate metadata CSV files |
| Step                      | transformations/T4_statistical_analysis/source_code/describe_ipcw.R                                    |
| Content                   | Covariate-level balance after IPCW weighting.                                                          |
| Population                | EXPOSED and CONTROL rows of the D4 covariate table, weighted by the final truncated IPCW.               |
| Unit of Observation (UoO) | One row per covariate.                                                                                 |

## Codebook

| Variable name | Description                                                                          | Format    | Vocabulary | Variables from source used for the variable |
| ------------- | ------------------------------------------------------------------------------------ | --------- | ---------- | ------------------------------------------- |
| var           | Covariate column name in the D4 covariate table                                      | Character |            | Covariate metadata: var                     |
| label         | Covariate display label                                                              | Character |            | Covariate metadata: label                   |
| type          | Covariate type (numeric or categorical)                                              | Character |            | Covariate metadata: type                    |
| asd_1         | Weighted standardized difference using `ipcw_cum_trim` (missing weights set to 1; NA if a group is empty) | Numeric |  | D4_StudyCohortCovariates: var, group; D4_StudyCohort_IPCW: ipcw_cum_trim |
