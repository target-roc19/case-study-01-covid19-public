## Metadata

| Table name                | D3_CONCEPTS_parquet/ and D3_CONCEPTS.duckdb                                                                                                                                                                                                                                              |
| ------------------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Description               | Concept table specific for the study. This database has the records related to each concept used in the study.                                                                                                                                                                           |
| Source                    | D2_CDM,D3_SPELLS, dap_specific_codelist_<cdm_tab>.parquet, RWEBRIDGE:`preprocessing_concept.csv ` , RWEBRIDGE:`dictionary.csv `, RWEBRIDGE:`concept_meaning.csv `, RWEBRIDGE:`concept_range.csv                                                                                          |
| Step                      | transformations/T2_semantic_harmonization/create_simple_concepts.R, transformations/T2_semantic_harmonization/create_complex_concepts.R                                                                                                                                                  |
| Content                   | Records from D2_CDM categorized in `concept_id`.                                                                                                                                                                                                                                         |
| Population                | All population selected within the `D3_SPELLS` table, for this study is the older adults                                                                                                                                                                                                 |
| Unit of Observation (UoO) | All records that can be categorized on `concept_id`. Thus, we have one row per record categorized by one or more concept ids (it is possible that the same record is categorized in different concepts). Only exclude records before the `start_look_back` date defined in config_values |
| File format               | Parquet file organised in a Parquet Hive with '`concept_id`' as the partition variable.                                                                                                                                                                                                  |

## Codebook

| variable_in_output | variable(s) needed                                                                                                               | retrieved | computed | format | rule_of_calculation                          | description                                                                                                                                                                                                                                                                                                                                        | vocabulary                                 |
| ------------------ | -------------------------------------------------------------------------------------------------------------------------------- | --------- | -------- | ------ | -------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------ |
| `person_id`        | D2_CDM::<cdm_table>:`person_id` `D3_SPELLS::person_id`                                                                           | x         |          | string | Keep `person_id` also present in `D3_SPELLS` | unique person identifier                                                                                                                                                                                                                                                                                                                           |                                            |
| `ori_table`        | D2_CDM::<cdm_table>:`table`                                                                                                      | x         |          | string |                                              | Origin table name (CDM)                                                                                                                                                                                                                                                                                                                            | EVENTS, VACCINES, MEDICAL_OBSERVATIONS,... |
| `unique_id`        | D2_CDM::<cdm_table>:`unique_id`                                                                                                  | x         |          | string |                                              | A Universally Unique Identifier (UUID) is a 128-bit number, displayed as a 36-character hexadecimal string                                                                                                                                                                                                                                         |                                            |
| `concept_id`       | dap_specific_codelist_<cdm_table>:`concept_id`, `dap_specific_concept_map`:`concept_id`, preprocessing_concepts.csv:`concept_id` | x         |          | string | see processing steps                         | Concept Name/Description (defined in events_codelist, medicines_codelist, vaccines_codelist,`dap_specific_concept_map` and preprocessing), Study Concepts as defined in the RWE-BRIDGE metadata (codelists, `dap_specific_concept_map`). Value assigned from the codelists/`dap_specific_concept_map` to the identnfied code in the data instance. |                                            |
| `coding_system`    | codelist_<cdm_table>:`coding_system`                                                                                             | x         |          | string | see processing steps                         | Coding system used for identifying the code. For concepts defined in the DAP specific concept map from the RWE-BRIDGE we assign the unique ID defined in the metadata.                                                                                                                                                                             |                                            |
| `code`             | codelist_<cdm_table>:`code`                                                                                                      | x         |          | string | see processing steps                         | For concepts defined in the DAP specific concept map from the RWE-BRIDGE we assign the unique ID defined in the metadata.                                                                                                                                                                                                                          |                                            |
| `date`             | D2_CDM::<cdm_table>:`<see processing steps>` OR `dap_specific_concept_map`:`keep_date_column`                                    | x         |          | date   | see processing steps                         | The date in which the code and coding-system were recorded.                                                                                                                                                                                                                                                                                        |                                            |
| `value`            | D2_CDM::<cdm_table>:`<see processing steps>` OR `dap_specific_concept_map`:`keep_value_column`                                   |           | x        | string | see processing steps                         | Value takes the numeric value for numeric concepts (weight, laboratory values, …), TRUE for concepts defined from the presence of the record and the CAT for categorical data                                                                                                                                                                      | INT,TRUE/FALSE,CAT                         |

## Creation of simple concepts

| Step number                       | Actions                                                                                                                                                                                                                                                                                                                                                                                                                              |
| --------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Step 1                            | Calculate `since_when_data_complete` as config_values /`start_look_back`                                                                                                                                                                                                                                                                                                                                                             |
| Use of dap_specific_codelists     |                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Step 2                            | Import DAP_SPECIFIC codelists to the database for each CDM table that has a dap_specific_codelist (from now in this explanation <add_cdm_table_name_here>_codelist). Select rows where match_status = 'MATCHED', apply unique by `code`, `coding_system` and `concept_id`                                                                                                                                                            |
| Step 3                            | Create temporal table for each CDM table with concept information select records that fall after the 'date column' > since_when_data_complete. the value column is defined as TRUE when using the codelist.                                                                                                                                                                                                                          |
|                                   | I**f events_codelist available** For EVENTS: For `events_code` apply upper. Then, events.event_code = event_free_text IF event_record_vocabulary = 'FREE_TEXT ELSE events.event_code = events.code event_code                                                                                                                                                                                                                        |
|                                   | **If medicines_codelist available** For MEDICINES: apply upper to medicines.code = upper("medicinal_product_atc_code") medicines.coding_system = "PRODCODEID" WHEN DAP == CPRD else "ATC", medicines.date =`medicines.date_prescription` WHEN `medicines.date_dispensing` IS NULL ELSE `medicines.date_dispensing                                                                                                                    |
|                                   | **If vaccines_codeslist available** For VACCINES:vaccines.code ` = upper(`vaccines.vx_type `) WHEN `vaccines.vx_atc ` IS NULL OR vx_atc = ' ' ELSE upper(vaccines.x_atc) , vaccines.date` = `vaccines.vx_record_date` WHEN `vaccines.vx_admin_date` IS NULL ELSE vaccines.vx_admin_date vaccines.coding_system `= 'VX_TYPE' WHEN`vaccines.vx_atc `IS NULL OR`vaccinesvx_atc `= ' ' ELSE 'VX_ATC'                                     |
|                                   | **If procedures_codeslist available** For PROCEDURES: For `procedure_code` apply upper                                                                                                                                                                                                                                                                                                                                               |
| Step 4                            | Delete records from the temporal table that date is empty                                                                                                                                                                                                                                                                                                                                                                            |
| Step 5                            | Apply join between DAP specific codelist and temporal CDM table                                                                                                                                                                                                                                                                                                                                                                      |
|                                   | - For EVENTS: events.event_code = events_codelist.code AND events.event_record_vocabulary = events_codelist.`coding_system`                                                                                                                                                                                                                                                                                                          |
|                                   | - For MEDICINES: medicines.code = medicines_codelist.code AND medicines.`coding_system` = t2.`coding_system`                                                                                                                                                                                                                                                                                                                         |
| Step 6                            | Add results in concept_table with the column names as described above                                                                                                                                                                                                                                                                                                                                                                |
| Use of `dap_specific_concept_map` |                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Step 7                            | Using the RWEBRIDGE::`dap_specific_concept_map` table, records that follow the combination of values in the `expected_value_X` column within the cdm column names defined in the `column_name_X`. <br /><br />Select the **date** of the concept as: CDM column defined in the `keep_date_column_name`, date >= `start_look_back`. <br />Select the **value** of the concept as: CDM column defined in the `keep_value_column_name`. |
| Step 8                            | Add results in concept_table with the column names as described above                                                                                                                                                                                                                                                                                                                                                                |

## Creation of complex concepts

In this process, we generate ALL possible records related to each of the new concepts (aka complex concepts) during the person's medical history. So, when applicable, we keep all combinations of records (e.g., BMI calculated from height and weight).

We will select the final record used for the analysis during the anchoring process. This process is applied on the D4_StudyVariables_Anchored

### create_or.R

Select all records related to a sub concept of a preprocessing concept and apply unique on it.

Create variables using an OR algorithm, while removing double entries of components recorded on the same date, e.g., in the concept table you have diabetes type 1, type 2 etc records from different source (medicines, events, procedures). We want to create overall "diabetes" using an OR rule (if you have type 1 or type 2 or both, you have diabetes). To do this, rbind them together to create the overall concept "diabetes". Then check for duplicates in the sense of multiple entries in the same combined concept on the same date.

### create_or_true.R

Same logic as `create_or.R`, but in addition the resulting records receive `value = TRUE`.

### create_bmi_int.R

This function presents a standard approach to processing and creating a person’s BMI value at any time. The possible useful input concepts that we can use are HEIGHT, WEIGHT, and BMI, all of them separately. With HEIGHT and WEIGHT, we can calculate BMI; however, we must first apply several processes. These are Standardisation, filtering values by age, BMI calculation, and filtering out impossible values.

| Step   | Action                                                                                                                                       |
| ------ | :------------------------------------------------------------------------------------------------------------------------------------------- |
|        | If height and weight are available, create new concept BMI_estimation:                                                                       |
| Step 1 | Standardise height component to metres. Rounding on decimal 4 digits.                                                                        |
|        | - keep value =< 2400, remove values > 2400                                                                                                   |
|        | - when value between 24 and 240 THEN value/100 (cm to m)                                                                                     |
|        | - when value between 241 and 2400 then value/1000 (mm to m)                                                                                  |
| Step 2 | Standardise weight component to kg.Rounding on decimal 4 digits.                                                                             |
|        | - keep value =< 21000, remove values >210000                                                                                                 |
|        | - when value between 2000 and 21000 THEN value/1000 (grams to kg)                                                                            |
| Step 3 | Filter out the standardised height and weight values using the BMI_age_filter. Delete cases outside the range defined by the BMI_age_filter. |
| Step 4 | Calculate the BMI estimation                                                                                                                 |
| Step 5 | Perform a Cartesian product between the WEIGHT and HEIGHT                                                                                    |
|        | with the clause that the date of the record of weight must be after the height record:                                                       |
|        | **NOT APPLICABLE FOR 1026 STUDY** - for age <= 18 years: weight records needs to be within 2 years after height measurement                  |
|        | - for older than 18 years: weight records needs to be within 3 years after height measurement                                                |
| Step 6 | After the Cartesian product. Rounding to 4 decimal digits, clean the records that fall outside a BMI range of 10 to 80.                      |
| Step 7 | Order by person ID, weight date, height date, and value.                                                                                     |
| Step 8 | Select the most recent record by person ID and weight date.                                                                                  |
| Step 9 | Add column importance and assign it with 2 for all BMI_estimation records.                                                                   |
|        | **If BMI is available as a concept:**                                                                                                        |
| Step 1 | Load BMI records. Rounding on decimal 4 digits.                                                                                              |
| Step 2 | Remove records that fall outside a BMI range of 10 to 80.                                                                                    |
| Step 3 | Add column importance and assign it with 1 for all BMI records.                                                                              |
|        | **Finally create new concept PP_BMI_INT:**                                                                                                   |
| Step 1 | Combine the BMI_estimation and the BMI records.                                                                                              |
| Step 2 | If you have multiple records on the same day. Keep record with the lowest importance and highest BMI value                                   |

Version 2 of description

Implements a standardized algorithm to compute BMI values using HEIGHT and WEIGHT records (and optionally BMI records if available). This includes:

* Standardizing height records ≤ 2400 into meters using predefined ranges and rounding the result
  * Value is in:
    * cm between 24 and 240 → divide by 100
    * mm between 241 and 2400 → divide by 1000
* Standardizing weight to kilograms and rounding
  * Value is in grams between 2000 and 210000 → divide by 1000
* Select weight records under 210.
* Filtering values based on age-specific valid ranges (`BMI_age_filter`)
* Calculate age at the record date and apply filtering to both height and weight records
* `BMI_age_filter` defines valid ranges by age, where min/max represent acceptable bounds for height, weight, and age at the record date
* Performing a constrained Cartesian join of height × weight based on age-dependent time windows (above and below 18 years)
  * Constraints:
    * For each weight measurement, select the most relevant height by considering prior height records for the same individual. The closest height recorded before the weight date is retained to ensure temporal consistency. Records with missing height values are excluded, resulting in a single valid height per weight observation.
    * BMI is then derived from the paired height and weight values. Height measurements are only considered if they fall within a defined time window relative to the weight, with stricter limits applied to individuals ≤ 18 years due to growth. The BMI is calculated using the standard formula and rounded. Implausible values are excluded to reduce the impact of data errors, and results are labeled for downstream prioritization.
* Calculating BMI and restricting results to values between 10 and 80
* Selecting the best BMI estimate per person per day and assigning priority scores (2 for estimated BMI, 1 for directly measured BMI)

After combining estimated and measured BMI values, the algorithm retains, per person per day, the record with the lowest priority score and, in case of ties, the highest BMI value.

---

### create_bmi_cat.R

For adults (≥18), loads `PP_BMI_INT` and classifies records using BMI thresholds defined in `concept_ranges.csv`, then maps these to concept meanings under the `BMI_CAT` super‑concept.

For children (2–18 years), CDC LMS reference data is used to interpolate age‑ and sex‑specific percentiles. BMI categories are then assigned based on CDC percentile cut‑offs.

---

### create_smokingstatus.R

Mirrors the logic of BMI categorization:
Maps smoking category records using concept meanings under `PP_SMOKING_CAT`, prioritizes records with a coding system containing `dap_spec_id`, sorts by person, date, and priority, and selects the maximum‑priority record for each person‑day.
The processed records are saved.

---

### create_death_aesi

Using the D3_SPELLS:death_date create a concept record for person with a death date with D3_CONCEPT:date = D3_SPELLS:death_date and value = TRUE

---

### create_hemtransplant

A record for `PP_HEMTRANSPLANT_POP` is generated when there is a transplantation/CAR-T event before or the same date as the immunosuppressant record. The logic is defined as:

**Condition A (Transplant):**

* (`IM_SOLIDHEMATRANSPLANT_AESI` **OR** `IM_HEMTRANSP_COV` **OR** `TP_HEMTRANSP_COV`)
* **AND** `DP_IMMUNOSUPPR`

**OR**

**Condition B (CAR-T Cell):**

* (`TP_CARTCELLPROC_COV` **OR** `TP_CARTCELL_COV`)
* **AND** `DP_IMMUNOSUPPR`

**Date Logic:**
The date of the new record should be the **date of the immunosuppressant record** (the later of the two dates) in the combination.

**SQL proposal**

**SQL Logic Example**

```sql
WITH event_family AS (
    SELECT * FROM D3_CONCEPTS 
    WHERE concept_id IN (
        'IM_SOLIDHEMATRANSPLANT_AESI', 'IM_HEMTRANSP_COV', 'TP_HEMTRANSP_COV',
        'TP_CARTCELLPROC_COV', 'TP_CARTCELL_COV'
    )
),
cases AS (
    SELECT DISTINCT
        dp.*
    FROM (SELECT * FROM concept_table WHERE concept_id = 'DP_IMMUNOSUPPR') AS dp
    INNER JOIN event_family AS fam
    ON dp.person_id = fam.person_id
    	AND dp.date >= fam.date
)
SELECT *
FROM cases;
```

---

### create_transplant

Algorithm as hemtransplant but with different component. In this case we will identify all individuals who have evidence of a transplant (either via diagnosis code IM_TRANSPLANTRECIPIENT_COV or procedure code TP_TRANSPLANTRECIPIENT) and who are treated with immunosuppressants.
The event date is defined as the date of the second-occurring record between the transplant-related code and the immunosuppressant treatment.

---

### create_contuse_91_days

WAITING FOR FURTHER DETAILS

---

### create_dose20mg_14days

WAITING FOR FURTHER DETAILS

---

### create_hepimp_asc_pop

Algorithm as hemtransplant but with different component. In this case we will identify all individuals who are hepatic impared when someone has `D_HEPIMPAIRMENT_COV`  and `D_ASCITES_COV` on his/her history of records.

---

### calculate_MELD

1. **Select records**: Selects records creatinine, bilirubin, and INR measurements (check preprocessing concepts metadat for list of concepts id)
2. **Match by date**: For each creatinine reading, find corresponding bilirubin and INR values from the same date.
3. **Validate completeness**: Only process records where all three lab values are available; discard incomplete records.
4. **Adjust creatinine**: Cap creatinine at 4.0 mg/dL if measured value ≥ 4.0 or if patient had ≥2 dialysis treatments within a week prior.
5. **Adjust INR**: Set any INR value below 1.0 to 1.0 (prevents invalid logarithms).
6. **Apply formula**: Calculate MELD = 9.57×log(creatinine) + 3.78×log(bilirubin) + 11.2×log(INR) + 6.43.
7. **Round result**: Round the calculated MELD score to the nearest whole number.
8. **Return output**: Return person ID, date, concept_id = "PP_CAL_**MELD**" and final MELD score as value for each valid record.

---

### identify_MELD_10_high

1. **Select records: Read records with concept_id = "PP_CAL_**MELD**"**
2. Subset records: Select MELD records equal or above 10
3. Rename concept_id: concept_id = "PP_**MELD**_10"
4. Return output

---

### calculate_PUGH

Step 0: clean lab values

1) impute units of lab records according to BRIDGE/LAB_units_if_not_stored
2) convert original mo_unit of lab records to target units based on the BRIDGE/LAB_unit_conversion.
3) if the converted value is within threshold (BRIDGE/LAB_threshold_with_unit), accept as cleaned value; otherwise, either discard (code 0), or attempt multiplication factor 1 (code 99), or other specified multiplication factors (code 1, 2, ...) according to BRIDGE/LAB_unit_conversion/next_attempt

---

Step 1: Calculate SOC

**SOC** (Standardized Prothrombin Ratio) is calculated from two alternative laboratory measurements:

**From LAB_PTI (Prothrombin Index - percentage):**

```
SOC = (LAB_PTI × 0.12) - 12
```

**From LAB_PTT (Prothrombin Time - seconds):**

```
SOC = LAB_PTT - 12
```

---

**Step 2: Categorize Lab Values into Grades**

Each input parameter is converted into a grade (1, 2, or 3) based on defined thresholds:

**Bilirubin (LAB_BILIRUBIN) - mg/dL**

| Points | Criteria    |
| ------ | ----------- |
| 1      | ≤ 1.9999999 |
| 2      | 2.0 - 3.0   |
| 3      | > 3.0       |

**Albumin (LAB_ALBUMIN) - g/dL**

| Points | Criteria  |
| ------ | --------- |
| 1      | > 3.5     |
| 2      | 2.8 - 3.5 |
| 3      | < 2.8     |

**Prothrombin Time - Using LAB_INR (preferred)**

| Points | Criteria  |
| ------ | --------- |
| 1      | < 1.7     |
| 2      | 1.7 - 2.3 |
| 3      | > 2.3     |

**Prothrombin Time - Using SOC (if LAB_INR missing)**

| Points | Criteria |
| ------ | -------- |
| 1      | < 4      |
| 2      | 4 - 6    |
| 3      | > 6      |

**Encephalopathy (N_ENCEPHALOPATHY_COV) - Categorical**

| Points | Criteria                         |
| ------ | -------------------------------- |
| 1      | Not present (default)            |
| 2      | Present (minimal encephalopathy) |

**Ascites (D_ASCITES_COV) - Categorical**

| Points | Criteria                 |
| ------ | ------------------------ |
| 1      | Not present (default)    |
| 2      | Value = 1 (mild ascites) |

---

**Step 3: Priority Rule for Prothrombin Component**

When both LAB_INR and SOC are available, the priority is:

```
Prothrombin Grade = COALESCE(grade_inr, grade_soc)
```

This means:

1. **Use LAB_INR grade if available** (preferred measurement)
2. **Fall back to SOC grade if LAB_INR is missing**
3. **Default to grade 1** if both are missing

---

##### Step 4: Calculate Final Child Pugh Score

The **final Child Pugh score** is the sum of all five component grades:

```
Child Pugh Score = Grade(Bilirubin) 
                 + Grade(Albumin) 
                 + Grade(Prothrombin - INR preferred, SOC if needed) 
                 + Grade(Ascites) 
                 + Grade(Encephalopathy)
```

**Default handling for missing values:**

- Each component defaults to **Grade 1** if no measurement is available
- Minimum possible score: **5** (all components at Grade 1)
- Maximum possible score: **15** (all components at Grade 3)

**Clinical interpretation:**

- Child Pugh score **≥ 7** indicates significant hepatic impairment

---

### identify_CHILDPUGH_7_high

1. Load PP_CAL_CHILD_PUGH generated a priory
2. Select those records that have value >= 7
3. Rename concept_id to PP_CHILD_PUGH_7

---

### create_egfr_uacr_renal

Step 0: clean lab values

1) impute units of lab records according to BRIDGE/LAB_units_if_not_stored
2) convert original mo_unit of lab records to target units based on the BRIDGE/LAB_unit_conversion.
3) if the converted value is within threshold (BRIDGE/LAB_threshold_with_unit), accept as cleaned value; otherwise, either discard (code 0), or attempt multiplication factor 1 (code 99), or other specified multiplication factors (code 1, 2, ...) according to BRIDGE/LAB_unit_conversion/next_attempt

---

#### Calculation of EGFR:

The estimated glomerular filtration rate (eGFR) will be defined using values recorded in the data source, either via diagnosis codes or laboratory results. In addition, whenever there is a serum creatinine measurement within the 18 months before and including time zero, the eGFR will be calculated using the 2021 Chronic Kidney Disease Epidemiology Collaboration (CKD-EPI) equation:

eGFR = 142 * min(Scr/k, 1)^a1 * max(Scr/k, 1)^a2 * c^Age * d [if female]

Where: Scr is serum creatinine concentration (mg/dL); k = 0.7 for females and 0.9 for males; a1 = -0.241 for females and -0.302 for males; a2 = -1.200; c = 0.9938; and d = 1.012 (applied if female). The term min(Scr/k, 1) represents the minimum of (Scr/k) and 1, and max(Scr/k, 1) represents the maximum of (Scr/k) and 1. The exponent a1 is used when serum creatinine is <= 0.9 mg/dL for males and <= 0.7 mg/dL for females, while the exponent a2 is used when serum creatinine is > 0.9 mg/dL for males and > 0.7 mg/dL for females.

Process:

1. Load LAB_EGFR, LAB_CREATININE, PP_AGE, PP_SEX
2. For the LAB_CREATININE records calculate the eGFR value by following the formula defined above
3. Select those abnormal eGFR records (value < 60) that have another abnormal eGFR event between 90 and 540 days before the event date and do not have any normal eGFR event (value >= 60) between 89 and -1 days before the event record.
4. Use the complete information from the selected records and asign concept_id as PP_EGFR_UACR_RENAL,

#### Calculation of UACR:

To Calculate UACR = urine albumin (mg/dL) / urine creatinine (g/dL).

Process:

1. Load LAB_URINE_ALB and LAB_URINE_CREATININE
2. On the days that there is a record for both LAB_URINE_ALB and LAB_URINE_CREATININE calculate the UACR following the formula above. If LAB_URINE_CREATININE is 0 asign NA to the result.
3. Join the UACR calculated concept records and LAB_URINE_ALBCREATININE
4. Identify those abnormal UACR records (value >= 30) that have another abnormal UACR event between 90 and 540 days before the event date and do not have any normal UACR event (value < 30) between 89 and -1 days before the event record.
5. Use the complete information from the selected records and asign concept_id as PP_EGFR_UACR_RENAL,

---

### create_renalimpairment_pop

When a second record of G_RENALIMPAIRMENTUNS_COV happens, the person is considered renaly impared.

---

### calculate_num_medications

Load all record from the medicines table, apply distinct by person_id, date and code and defined concept_id as PP_NUMMEDICATIONS . Disclaimer, we will count the number of records during anchoring.

---

### categorize_2

NOT NEEDED FOR THE INTERIM REPORT
