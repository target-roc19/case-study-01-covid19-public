## Metadata


| Table name                | D3_UNIVARIATE_EPISODES                                                                                                         |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| Description               | Univariate episode table: one set of intervals per study variable flagged with eligibility = TRUE, built from concept records. |
| Source                    | `D3_CONCEPTS` (hive-partitioned parquet), `D3_SPELLS.parquet`, `study_variables.csv` (BRIDGE)                                  |
| Step                      | transformations/T3_study_design/source_code/create_univariate_episodes.R                                                       |
| Content                   | One row per person, study variable and episode interval.                                                                       |
| Population                | Persons represented in `D3_SPELLS`                                                                                             |
| Unit of Observation (UoO) | Person, study variable and episode interval                                                                                    |

## Codebook


| variable_in_output | variable(s) needed            | retrieved | computed | format                 | rule_of_calculation                                                                | description               | vocabulary                 |
| ------------------ | ----------------------------- | --------- | -------- | ---------------------- | ---------------------------------------------------------------------------------- | ------------------------- | -------------------------- |
| person_id          | `D3_SPELLS.person_id`         | x         |          | string                 | Carried from `D3_SPELLS`                                                           | Unique person identifier  |                            |
| variable_id        | `study_variables.csv`         |           | x        | string                 | Hive partition key, one partition per study variable                               | Study variable identifier | `INCL_*`, `EXCL_*`, `SV_*` |
| start_episode      | `D3_CONCEPTS.date`, look-back |           | x        | date                   | Concept date minus configured start look-back, or configured eligibility start     | Start of the episode      | YYYY-MM-DD                 |
| end_episode        | `D3_CONCEPTS.date`, look-back |           | x        | date                   | Concept date plus configured end look-back, or end of study when missing inclusion | End of the episode        | YYYY-MM-DD                 |
| value              | `D3_CONCEPTS.value`           |           | x        | string/logical/numeric | Carried from the selected concept; gaps filled per `missing_set_to`                | Value during the episode  | Concept-specific           |

## Processing Steps


| Step number | Description                                                                                                                                                |
| ----------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Step 1      | Load study variables with eligibility = TRUE, excluding `raw_concept` = D3 and `STABLE_PRECONDITIONS`. Convert look-back columns to non-negative integers. |
| Step 2      | Load `D3_SPELLS` and extract unique `person_id`s.                                                                                                          |
| Step 3      | Create a DuckDB view on `D3_CONCEPTS`, restricted to the `concept_id`s required by the study variables.                                                    |
| Step 4      | Delete the previous output hive folder and run `episodeR::create_univariate_episodes` with batching, writing to `D3_UNIVARIATE_EPISODES_parquet/`.         |

## Example


| person_id | variable_id  | start_episode | end_episode | value |
| --------- | ------------ | ------------- | ----------- | ----- |
| 1         | SV_EXAMPLE_1 | 2024-10-01    | 2025-01-01  | TRUE  |
