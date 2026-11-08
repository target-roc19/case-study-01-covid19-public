## Metadata

| Table name                | D3_ANCHORED_VARIABLES                                                                                                                                                            |
| ------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Study variables anchored to each person's time zero (T0), computed over the configured anchoring windows. Hive-partitioned by variable.                                          |
| Source                    | `D3_CONCEPTS` (concepts hive), `D4_StudyCohort<Main/Supplementary>.parquet` (T0 per person), anchoring metadata (config_project: `create_anchored_datasets$input$metadata_file`) |
| Step                      | transformations/T3_study_design/source_code/create_anchored_hive.R                                                                                                               |
| Content                   | One row per person, anchored variable and anchoring window.                                                                                                                      |
| Population                | Union of unique `person_id` and `T0` across the Main and Supplementary cohorts (anchoring run once on the combined population)                                                   |
| Unit of Observation (UoO) | Person, T0 and anchored variable                                                                                                                                                 |

## Codebook

| variable_in_output | variable(s) needed      | retrieved | computed | format       | rule_of_calculation                                             | description                        | vocabulary       |
| ------------------ | ----------------------- | --------- | -------- | ------------ | --------------------------------------------------------------- | ---------------------------------- | ---------------- |
| person_id          | `D4_StudyCohort` cohort | x         |          | string       | Carried from the study cohort                                   | Unique person identifier           |                  |
| T0                 | `D4_StudyCohort` cohort | x         |          | date         | Carried from the study cohort                                   | Anchor date (time zero)            | YYYY-MM-DD       |
| variable_id        | anchoring metadata      |           | x        | string       | Hive partition key, one partition per anchored variable         | Anchored study-variable identifier | Metadata-defined |
| value              | `D3_CONCEPTS`           |           | x        | per metadata | Value of the concept within the anchoring window relative to T0 | Anchored value                     | Metadata-defined |

## Processing Steps

| Step number | Description                                                                                                   |
| ----------- | ------------------------------------------------------------------------------------------------------------- |
| Step 1      | Load the anchoring window metadata, which defines the variables, data types and windows to anchor.            |
| Step 2      | Load the Main and Supplementary study cohorts, and build the combined unique `person_id` and `T0` population. |
| Step 3      | Run `anchoR::anchor_by_variable` on the combined population, writing to `D3_ANCHORED_VARIABLES_parquet/`.     |

## Example

| person_id | T0         | variable_id | value |
| --------- | ---------- | ----------- | ----- |
| 1         | 2024-11-15 | SV_EXAMPLE  | 1     |
