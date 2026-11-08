## Metadata

| Table name                | D5_exp_estimate_table_IPSMW_<Main/Supplementary>                                                          |
| ------------------------- | --------------------------------------------------------------------------------------------------------- |
| Description               | Exponentiated principal-stratum model coefficients (odds ratios) with 95% confidence intervals for IPSMW.  |
| Source                    | D5_PS_Coef_IPSMW<Main/Supplementary>                                                                      |
| Step                      | transformations/T4_statistical_analysis/source_code/describe_ipsmw.R                                       |
| Content                   | Principal-stratum model odds ratios, 95% CI, and a formatted OR (CI) string, excluding the intercept.     |
| Population                | Not applicable (model-level table).                                                                       |
| Unit of Observation (UoO) | One row per model coefficient, excluding the intercept.                                                   |

## Codebook

| Variable name      | Description                                                       | Format    | Vocabulary          | Variables from source used for the variable |
| ------------------ | ----------------------------------------------------------------- | --------- | ------------------- | ------------------------------------------- |
| coefficient        | Model term name                                                   | Character |                     | D5_PS_Coef_IPSMW: coefficient               |
| estimate           | Point estimate of coefficient (log-odds scale)                    | Numeric   |                     | D5_PS_Coef_IPSMW: estimate                  |
| std_error          | Standard error                                                    | Numeric   | > 0                 | D5_PS_Coef_IPSMW: std_error                 |
| z_score            | z statistic                                                       | Numeric   |                     | D5_PS_Coef_IPSMW: z_score                   |
| p_value            | Two-sided p-value                                                 | Numeric   | [0, 1]              | D5_PS_Coef_IPSMW: p_value                   |
| exp_estimate       | Exponentiated estimate (odds ratio)                               | Numeric   | > 0                 | D5_PS_Coef_IPSMW: exp_estimate              |
| analysis           | Analysis cohort label                                             | Character | Main, Supplementary | D5_PS_Coef_IPSMW: analysis                  |
| exp_estimate_lower | Lower 95% CI bound of the odds ratio                              | Numeric   | > 0                 | D5_PS_Coef_IPSMW: estimate, std_error       |
| exp_estimate_upper | Upper 95% CI bound of the odds ratio                              | Numeric   | > 0                 | D5_PS_Coef_IPSMW: estimate, std_error       |
| OR_ci              | Formatted odds ratio with 95% CI, "OR (lower-upper)" (2 decimals) | Character |                     | D5_PS_Coef_IPSMW: exp_estimate, estimate, std_error |
