## Metadata

| Table name                | D5_ASD_PREWEIGHT_IPSMW_<Main/Supplementary>                                                                   |
| ------------------------- | ------------------------------------------------------------------------------------------------------------- |
| Description               | Unweighted absolute standardized differences (ASD) for each covariate, exposed vs control, before IPSMW.      |
| Source                    | D4_StudyCohortCovariates<Main/Supplementary>; D4_StudyCohortFup<Main/Supplementary>; covariate metadata CSV files |
| Step                      | transformations/T4_statistical_analysis/source_code/describe_ipsmw.R                                          |
| Content                   | Covariate-level balance before IPSMW weighting.                                                               |
| Population                | Compliant exposed (compliant_exposed == TRUE) and CONTROL rows of the D4 covariate table.                      |
| Unit of Observation (UoO) | One row per covariate.                                                                                        |

## Codebook

| Variable name | Description                                                                      | Format    | Vocabulary | Variables from source used for the variable |
| ------------- | -------------------------------------------------------------------------------- | --------- | ---------- | ------------------------------------------- |
| var           | Covariate column name in the D4 covariate table                                  | Character |            | Covariate metadata: var                     |
| label         | Covariate display label                                                          | Character |            | Covariate metadata: label                   |
| type          | Covariate type (numeric or categorical)                                          | Character |            | Covariate metadata: type                    |
| asd_1         | Unweighted standardized difference, exposed vs control (NA if a group is empty)  | Numeric   |            | D4_StudyCohortCovariates: var, group; D4_StudyCohortFup: compliant_exposed |
