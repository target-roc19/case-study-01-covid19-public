## Metadata

| Table name                | D5_PS_Coef_IPSMW_`<Main/Supplementary>`                                                               |
| ------------------------- | ----------------------------------------------------------------------------------------------------- |
| Description               | Coefficients and standard errors from the exposed-only principal stratum model used to compute IPSMW. |
| Source                    | D4_StudyCohort`<Main/Supplementary>`; D4_StudyCohortFup`<Main/Supplementary>`; ipsmw_variables.csv    |
| Step                      | transformations/T4_statistical_analysis/source_code/compute_ipsmw.R                                   |
| Content                   | Model coefficients, uncertainty, and odds-ratio scale effects                                         |
| Population                | Exposed individuals in the matched cohort                                                             |
| Unit of Observation (UoO) | One row per model coefficient                                                                         |

## Codebook

| Variable name | Description                                   | Format    | Vocabulary | Variables from source used for the variable |
| ------------- | --------------------------------------------- | --------- | ---------- | ------------------------------------------- |
| coefficient   | Model term name                               | Character |            |                                             |
| estimate      | Point estimate of coefficient                 | Numeric   |            |                                             |
| std_error     | Standard error                                | Numeric   |            |                                             |
| z_score       | z statistic                                   | Numeric   |            |                                             |
| p_value       | Two-sided p-value                             | Numeric   | [0, 1]     |                                             |
| exp_estimate  | Exponentiated estimate (odds ratio scale)     | Numeric   | > 0        |                                             |
| analysis      | Analysis cohort label (Main or Supplementary) | Character |            |                                             |
