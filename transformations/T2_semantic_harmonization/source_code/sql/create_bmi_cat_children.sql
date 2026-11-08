cdc_ref_bmichildren AS (
  SELECT
    CAST(sex AS VARCHAR) AS sex,
    TRY_CAST(age AS DOUBLE) AS age_months,
    TRY_CAST(bmi_l AS DOUBLE) AS bmi_l,
    TRY_CAST(bmi_m AS DOUBLE) AS bmi_m,
    TRY_CAST(bmi_s AS DOUBLE) AS bmi_s
  FROM read_csv_auto(
    '{cdc_ref_path}',
    delim = ',',
    header = TRUE
  )
),

child_candidates AS (
  SELECT
    bmi.person_id,
    bmi.date,
    bmi.sex,
    bmi.bmi_value,
    bmi.age,
    ROUND(bmi.age * 24.0) / 2.0 AS age_months
  FROM bmi_measurements bmi
  WHERE bmi.age * 12.0 BETWEEN 24.0 AND 216.0
    AND bmi.sex IS NOT NULL
    AND bmi.bmi_value IS NOT NULL
),

child_with_reference AS (
  SELECT
    child.person_id,
    child.date,
    child.bmi_value,
    ref.bmi_l,
    ref.bmi_m,
    ref.bmi_s
  FROM child_candidates child
  LEFT JOIN cdc_ref_bmichildren ref
    ON child.sex = ref.sex
    AND child.age_months = ref.age_months
),

child_scored AS (
  SELECT
    person_id,
    date,
    CASE
      WHEN bmi_l IS NULL OR bmi_m IS NULL OR bmi_s IS NULL
      THEN NULL
      WHEN bmi_l = 0
      THEN LN(bmi_value / bmi_m) / bmi_s
      ELSE ((POWER(bmi_value / bmi_m, bmi_l) - 1.0) / (bmi_l * bmi_s))
    END AS bmi_z
  FROM child_with_reference
),

child_results AS (
  SELECT
    CAST(NULL AS VARCHAR) AS ori_table,
    CAST(NULL AS UUID) AS unique_id,
    person_id,
    CAST(NULL AS VARCHAR) AS code,
    CAST(NULL AS VARCHAR) AS coding_system,
    CAST(
      -- 5th, 85th, and 95th CDC BMI percentiles expressed as z-score cutoffs.
      CASE
        WHEN bmi_z IS NULL
        THEN 0
        WHEN bmi_z < -1.6448536269514729
        THEN 1
        WHEN bmi_z < 1.0364333894937898
        THEN 2
        WHEN bmi_z < 1.6448536269514722
        THEN 3
        ELSE 4
      END AS VARCHAR
    ) AS value,
    date,
    CAST(NULL AS INTEGER) AS tag,
    '{complex_concept_name}' AS concept_id
  FROM child_scored
)
