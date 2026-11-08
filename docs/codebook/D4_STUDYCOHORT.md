## Metadata


| Table name                | D4_StudyCohort`<Main/Supplementary>`                                                                                                                                                  |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Study cohort of exposed and matched control person-spells, produced by CohortBuilder::build_study_cohort (matching without replacement for Main; with replacement for Supplementary). |
| Source                    | D3_Eligibility (eligible_exposed, eligible_control, start, end, covariates)                                                                                                           |
| Step                      | transformations/T3_study_design/source_code/create_cohort.R                                                                                                                           |
| Content                   | Matched sets of EXPOSED and CONTROL rows with T0, match_id and baseline covariates; UNMATCHED exposed rows kept in Main                                                               |
| Population                | Eligible person(-spells) from D3_Eligibility                                                                                                                                          |
| Unit of Observation (UoO) | person(-spell) selected as exposed or control in the study cohort                                                                                                                     |
| Output                    | data/D4_analytic_datasets/D4_StudyCohortMain.parquet and D4_StudyCohortSupplementary.parquet                                                                                          |

## Codebook


| variable_in_output | variable(s) needed                  | retrieved | computed | format    | rule_of_calculation                                             | description                                                                                | vocabulary                          |
| ------------------ | ----------------------------------- | --------- | -------- | --------- | --------------------------------------------------------------- | ------------------------------------------------------------------------------------------ | ----------------------------------- |
| person_id          | person_id (D3_Eligibility)          | x         |          | character | copied from D3_Eligibility                                      | Person identifier                                                                          |                                     |
| match_id           | assigned by CohortBuilder           |           | x        | integer   | one id per matched set; NA for UNMATCHED                        | Matched set identifier                                                                     |                                     |
| boot_id            |                                     |           | x        | integer   | fixed to 0 (no bootstrap)                                       | Bootstrap identifier                                                                       | 0                                   |
| T0                 |                                     |           | x        | date      | exposed start date, assigned to both members of the matched set | Index date (time zero)                                                                     |                                     |
| group              |                                     |           | x        | character | EXPOSED, CONTROL, or UNMATCHED (exposed without a match)        | Exposure group                                                                             | EXPOSED, CONTROL, UNMATCHED         |
| start_episode      | start (D3_Eligibility)              | x         |          | date      | copied from D3_Eligibility                                      | Start of the eligible episode                                                              |                                     |
| end_episode        | end (D3_Eligibility)                | x         |          | date      | copied from D3_Eligibility                                      | End of the eligible episode                                                                |                                     |
| year_of_birth      | year_of_birth (D3_Eligibility)      | x         |          | integer   | copied from D3_Eligibility                                      | Year of birth (age proxy)                                                                  |                                     |
| SV_SEX             | SV_SEX (D3_Eligibility)             | x         |          | character | copied from D3_Eligibility                                      | Sex                                                                                        | per D3 dictionary (observed: F)     |
| SV_REGION          | SV_REGION (D3_Eligibility)          | x         |          | character | copied from D3_Eligibility                                      | Place of residence (CPRD practice or VID neighbourhood); missing stored as the text "NULL" | defined by dictionary file          |
| SV_SES             | SV_SES (D3_Eligibility)             | x         |          | character | copied from D3_Eligibility; stored as character                 | Socio-economic status (see open questions)                                                 | deap specific (e.g., 1, 2, 3, 4, 5) |
| SV_PREG_STATUS     | SV_PREG_STATUS (D3_Eligibility)     | x         |          | logical   | copied from D3_Eligibility                                      | Pregnancy status                                                                           | TRUE, FALSE                         |
| SV_CDC_RISK        | SV_CDC_RISK (D3_Eligibility)        | x         |          | character | copied from D3_Eligibility; stored as VARCHAR                   | Number of CDC high-risk conditions (see open questions)                                    | 0, 1, 2                             |
| SV_HU_PC_CONTACT   | SV_HU_PC_CONTACT (D3_Eligibility)   | x         |          | numeric   | copied from D3_Eligibility; stored as DOUBLE                    | Healthcare utilisation, primary care contact (proxy; definition to confirm)                |                                     |
| SV_HU_HOSP_CONTACT | SV_HU_HOSP_CONTACT (D3_Eligibility) | x         |          | numeric   | copied from D3_Eligibility; stored as DOUBLE                    | Healthcare utilisation, hospital contact (proxy; definition to confirm)                    |                                     |

Notes:

1. Main (D4_StudyCohortMain.parquet): 14,670 rows: 6,796 EXPOSED, 6,796 CONTROL, 1,078 UNMATCHED (match_id NA). Supplementary: 15,517 rows with 11,915 distinct person_id.
2. Column types in the parquet differ from the earlier codebook: SV_SES is BOOLEAN (not string), SV_CDC_RISK is VARCHAR (not integer). Confirm which is intended.
3. SV_HU_CONTACT and SV_SMOKING are not in the parquet. The parquet has SV_HU_PC_CONTACT and SV_HU_HOSP_CONTACT instead.
4. Matching variables from config_project.yaml (create_cohort matching vars) are kept in this output as listed above.
5. Main contains persons appearing in more than one row (14,670 rows, 14,583 distinct person_id). Some appear as both EXPOSED and CONTROL (for example person 10000019346). Needs review against the without-replacement design.

## Processing steps

### Main analysis: `D4_StudyCohort_Main` matching without replacement

Matching without replacement will be implemented using a greedy matching approach (i.e., individuals should only be enrolled once). We will use essentially a matching without replacement algorithm sequentially, whereby first matched pairs are found, then duplicates are removed (keeping units enrolled at earlier T0's) from both the matched pairs. The available pool is then updated, removing the spells from a person that has already been matched. The process of updating the available pools of spells, matching, and pair checking is repeated until no eligible exposed or control remain unmatched in the pool, or not further matches can be found (whatever occurs first).

First, initialize pool of available spells (all eligible exposed and control spells). While exposed spells remain on the available pool, repeat the following steps:

1. Loop over exposed spells ordered by index date T0.
   1.1. From D3_Eligibility, keep all rows eligible_exposed = TRUE or eligible_control = TRUE.
   1.2. Build exact matching profiles from the matching variables, encoded as groupkey.
   1.3. Define T0 for exposed candidates as their exposed start date.
   1.4. Order exposed candidates by ascending T0, then apply deterministic seeded tie-breaking for reproducibility.
   1.5. For each exposed candidate, find currently available controls with same groupkey, compatible year_of_birth rule (as configured), exposed T0 within control eligibility interval (`start` and `end` defined in D3_Eligibility) different person_id (no self-match).
   1.6 Select one control per exposed candidate using deterministic seeded ranking (reproducible random choice).
   1.7 If a control is found, create one matched set (EXPOSED + CONTROL), assign a unique match_id, and assign exposed T0 to both rows.
   1.8 If no control is found, continue; this exposed spell may become UNMATCHED in final output unless matched in a later round.
2. Resolve within-round conflicts.
   2.1. If multiple exposed candidates select the same control, keep the highest-priority proposal (earliest exposed order). If the exposed order is equal, pick one at random.
   2.2. If a person appears in accepted proposals in both roles (as EXPOSED and CONTROL), keep the earliest-priority proposal and dissolve the others.
   2.3. This guarantees person-level no replacement within the round.
3. Update pools and iterate.
   3.1. Remove all persons accepted in this round from both exposed and control pools.

Build final output labels.

- EXPOSED: exposed spells accepted in matched sets.
- CONTROL: selected controls from matched sets.
- UNMATCHED: exposed spells never accepted as EXPOSED by the end of all rounds.

Tie-breaking rules for repeated selection of the same control unit:

1. Primary priority: earliest exposed T0.
2. Secondary priority: deterministic seeded ordering for ties.

### Sensitivity analysis: `D4_StudyCohort_Supplementary` matching with replacement

As a sensitivity analysis, controls (unexposed) will be allowed to be matched multiple times (i.e., matching with replacement). While exposed spells can only be matched once, a person may serve as control before being exposed.

Computationally, the following steps are performed looping over year of birth in the population (i.e., stratiying by year of birth):

1. From D3_Eligibility, select all rows with eligible_exposed == TRUE and all rows with eligible_control == TRUE. Define the exact matching profile as the combination of the matching variables (use a group-key profile factor encoding the specific combination of matching variables). Define group EXPOSED and CONTROL.
2. Among rows with eligible_exposed == TRUE, define T0 as this start date.
3. Order the exposed candidates by ascending T0. When multiple exposed candidates share the same T0 and matching profile, break ties using a fixed random seed so that the matching procedure is reproducible.
4. For each exposed candidate in this order, identify the set of available control rows with eligible_control == TRUE and the same matching profile. From this set, sample one control (with replacement).
5. If a control is found, create one matched set consisting of the exposed person and the sampled control, assign a unique match_id, and assign the exposed person's T0 to both members of the matched set.
6. If no eligible control is available for an exposed candidate, label that exposed candidate as UNMATCHED.
7. Continue to the next exposed candidate.

## Example

Two matched sets from D4_StudyCohortMain.parquet (Main analysis). Columns shortened to those listed in the codebook.


| person_id   | match_id | boot_id | group   | T0         | start_episode | end_episode | year_of_birth | SV_SEX | SV_REGION | SV_SES | SV_PREG_STATUS | SV_CDC_RISK | SV_HU_PC_CONTACT | SV_HU_HOSP_CONTACT |
| ----------- | -------- | ------- | ------- | ---------- | ------------- | ----------- | ------------- | ------ | --------- | ------ | -------------- | ----------- | ---------------- | ------------------ |
| 10000000717 | 1        | 0       | CONTROL | 2021-01-01 | 2021-01-01    | 2022-12-31  | 1984          | F      | NULL      | 3      | FALSE          | 0           | 1                | 0                  |
| 10000002510 | 1        | 0       | EXPOSED | 2021-01-01 | 2021-01-01    | 2022-12-31  | 1984          | F      | NULL      | 3      | FALSE          | 0           | 1                | 0                  |
| 10000002005 | 2        | 0       | CONTROL | 2021-01-01 | 2021-01-01    | 2022-12-31  | 1997          | F      | NULL      | 1      | FALSE          | 0           | 1                | 0                  |
| 10000002662 | 2        | 0       | EXPOSED | 2021-01-01 | 2021-01-01    | 2022-12-31  | 1997          | F      | NULL      | 1      | FALSE          | 0           | 1                | 0                  |
