## Metadata

| Table name                |                                                                                                                                                                                                                                                                                           |
| ------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Database with all CDM tables available in the Data instance. The tables are edited so they include Unique Identifier for each records, table source and row ID. Moreover, records with missing `person_id` are deleted and repeated records are deleted for all table except MEDICINES. |
| Source                    | CDM data instance, PERSONS, VACCINES, MEDICINES, OBSERVATION_PERIODS, MEDICAL_OBSERVATIONS, EVENTS, SURVEY_OBSERVATIONS, SURVEY_ID, VISIT_OCCURRENCE, PROCEDURES                                                                                                                          |
| Step                      | transformations/T2_semantic_harmonization/`set_db.R`                                                                                                                                                                                                                                    |
| Content                   | CDM records from Data instance. One table within the Database per CDM table                                                                                                                                                                                                               |
| Population                | All population (all the records form the data instance, unless they have been excluded if repeated)                                                                                                                                                                                       |
| Unit of Observation (UoO) | One row per record if `person_id` it is not missing, otherwise delete                                                                                                                                                                                                                   |
| File format               | DuckDb database (.db)                                                                                                                                                                                                                                                                     |

## Codebook

| variable_in_output | variable(s) needed | retrieved | computed | format          | rule_of_calculation                 | description                                                                                                | vocabulary |
| ------------------ | ------------------ | --------- | -------- | --------------- | ----------------------------------- | ---------------------------------------------------------------------------------------------------------- | ---------- |
| `ori_table`      |                    |           | x        | String          | Add the name of the table of origin | Name of the CDM table source                                                                               |            |
| `unique_id`      |                    |           | x        | string          | Add an id number as an UUID         | A Universally Unique Identifier (UUID) is a 128-bit number, displayed as a 36-character hexadecimal string |            |
| \<CDM column 1\>   | cdm:CDM column 1   | x         |          | string/int/date |                                     | Other CDM columns from the specific CDM table                                                              |            |
| ...                | ...                | ...       | ...      | ...             | ...                                 | ...                                                                                                        |            |
| \<CDM column N\>   | cdm:CDM column N   | x         |          | string/int/date |                                     | Other CDM columns from the specific CDM table                                                              |            |

## Processing Steps

| Step number | Action                                                                                      |
| ----------- | :------------------------------------------------------------------------------------------ |
| Step 1      | Import all csv files from the CDM to the database                                           |
| Step 2      | For each database, delete duplicates using all columns of the table, except MEDICINES table |
| Step 3      | Delete rows that miss `person_id`                                                         |
| Step 4      | Add the `ori_table` and `unique_id` columns to each table                               |
| Step 5      | Save the number of rows per table                                                           |

## Example

| `person_id` |  |  |  |
| ------------- | - | - | - |
| 1             |  |  |  |
