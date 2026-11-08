## Metadata

| Table name                | D5_exp_estimate_table_IPCW_<Main/Supplementary>_Estimand<1/2>                                                     |
| ------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| Description               | Exponentiated denominator-model coefficients (odds ratios) with 95% confidence intervals for IPCW.                |
| Source                    | D5_Censor_Coef_IPCW<Main/Supplementary>_Estimand<1/2>                                                             |
| Step                      | transformations/T4_statistical_analysis/source_code/describe_ipcw.R                                                |
| Content                   | Denominator-model odds ratios, 95% CI, and a formatted OR (CI) string, excluding the intercept.                   |
| Population                | Not applicable (model-level table).                                                                               |
| Unit of Observation (UoO) | One row per denominator-model coefficient, excluding the intercept.                                               |

## Codebook

| Variable name        | Description                                                | Format    | Vocabulary | Variables from source used for the variable |
| -------------------- | ---------------------------------------------------------- | --------- | ---------- | ------------------------------------------- |
| model_type           | Censoring model component (always "denominator")           | Character | denominator | D5_Censor_Coef_IPCW: model_type            |
| coefficient          | Model term name                                            | Character |            | D5_Censor_Coef_IPCW: coefficient            |
| estimate             | Point estimate of coefficient (log-odds scale)             | Numeric   |            | D5_Censor_Coef_IPCW: estimate               |
| std_error            | Standard error                                             | Numeric   | > 0        | D5_Censor_Coef_IPCW: std_error              |
| z_score              | z statistic                                                | Numeric   |            | D5_Censor_Coef_IPCW: z_score                |
| p_value              | Two-sided p-value                                          | Numeric   | [0, 1]     | D5_Censor_Coef_IPCW: p_value                |
| exp_estimate         | Exponentiated estimate (odds ratio)                        | Numeric   | > 0        | D5_Censor_Coef_IPCW: exp_estimate           |
| analysis             | Analysis cohort label                                      | Character | Main, Supplementary | D5_Censor_Coef_IPCW: analysis      |
| exp_estimate_lower   | Lower 95% CI bound of the odds ratio                       | Numeric   | > 0        | D5_Censor_Coef_IPCW: estimate, std_error    |
| exp_estimate_upper   | Upper 95% CI bound of the odds ratio                       | Numeric   | > 0        | D5_Censor_Coef_IPCW: estimate, std_error    |
| OR_ci                | Formatted odds ratio with 95% CI, "OR (lower-upper)" (2 decimals) | Character |     | D5_Censor_Coef_IPCW: exp_estimate, estimate, std_error |
| estimand             | Estimand number                                            | Integer   | 1, 2       | Pipeline loop variable (estimand)           |
