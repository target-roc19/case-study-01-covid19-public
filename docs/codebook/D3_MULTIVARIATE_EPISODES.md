## Metadata


| Table name                | D3_MULTIVARIATE_EPISODES                                                                                           |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| Description               | Multivariate episode table combining the univariate study-variable intervals into one set of intervals per person. |
| Source                    | `D3_UNIVARIATE_EPISODES_parquet/`, `D3_SPELLS.parquet`, `study_variables.csv` (BRIDGE)                             |
| Step                      | transformations/T3_study_design/source_code/create_multivariate_episodes.R                                         |
| Content                   | One row per person and combined interval, with the status of each eligible study variable.                         |
| Population                | Persons represented in `D3_SPELLS`                                                                                 |
| Unit of Observation (UoO) | Person and multivariate episode interval                                                                           |

## Codebook


| variable_in_output         | variable(s) needed       | retrieved | computed | format          | rule_of_calculation                                    | description                          | vocabulary       |
| -------------------------- | ------------------------ | --------- | -------- | --------------- | ------------------------------------------------------ | ------------------------------------ | ---------------- |
| person_id                  | `D3_SPELLS.person_id`    | x         |          | string          | Carried from univariate episodes                       | Unique person identifier             |                  |
| start_episode              | `D3_UNIVARIATE_EPISODES` |           | x        | date            | Start of the combined interval                         | Start of the combined episode        | YYYY-MM-DD       |
| end_episode                | `D3_UNIVARIATE_EPISODES` |           | x        | date            | End of the combined interval                           | End of the combined episode          | YYYY-MM-DD       |
| `INCL_*`, `EXCL_*`, `SV_*` | `D3_UNIVARIATE_EPISODES` |           | x        | logical/integer | Combined status of each study variable in the interval | Study-variable status in the episode | Metadata-defined |

## Processing Steps


| Step number | Description                                                                                                                                         |
| ----------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| Step 1      | Load study variables with eligibility = TRUE (excluding `raw_concept` = D3 and `STABLE_PRECONDITIONS`) and the list of person IDs from `D3_SPELLS`. |
| Step 2      | Run `episodeR::create_multivariate_episodes` on `D3_UNIVARIATE_EPISODES_parquet/`, in batches.                                                      |
| Step 3      | Write the output to `D3_MULTIVARIATE_EPISODES.parquet`, used by the eligibility step.                                                               |

## Example


| person_id | start_episode | end_episode | SV_EXAMPLE_1 | ... | SV_EXAMPLE_N |
| --------- | ------------- | ----------- | ------------ | --- | ------------ |
| 1         | 2024-10-01    | 2025-01-01  | TRUE         | ... | TRUE         |
