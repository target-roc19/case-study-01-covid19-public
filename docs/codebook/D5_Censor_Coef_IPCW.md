## Metadata

| Table name                | D5_Censor_Coef_IPCW_<Main/Supplementary>_Estimand<1/2>                                                             |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| Description               | Coefficients and standard errors from pooled logistic censoring models used to compute IPCW.                       |
| Source                    | D4_StudyCohortFup<Main/Supplementary>; D4_StudyCohortTimeVaryingCovariates<Main/Supplementary>; ipcw_variables.csv |
| Step                      | transformations/T4_statistical_analysis/source_code/compute_ipcw.R                                                 |
| Content                   | Numerator and denominator model coefficients, uncertainty, and odds-ratio scale effects.                           |
| Population                | Person-time rows in matched exposed and controls.                                                                  |
| Unit of Observation (UoO) | One row per model coefficient and model type.                                                                      |

## Codebook

| Variable name | Description                               | Format    | Vocabulary             | Variables from source used for the variable |
| ------------- | ----------------------------------------- | --------- | ---------------------- | ------------------------------------------- |
| model_type    | Censoring model component                 | Character | numerator, denominator |                                             |
| coefficient   | Model term name                           | Character |                        |                                             |
| estimate      | Point estimate of coefficient             | Numeric   |                        |                                             |
| std_error     | Standard error                            | Numeric   | > 0                    |                                             |
| z_score       | z statistic                               | Numeric   |                        |                                             |
| p_value       | Two-sided p-value                         | Numeric   | [0, 1]                 |                                             |
| exp_estimate  | Exponentiated estimate (odds ratio scale) | Numeric   | > 0                    |                                             |
| analysis      | Analysis cohort label                     | Character | Main, Supplementary    |                                             |
