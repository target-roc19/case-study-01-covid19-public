-- # This SQL script presents a standard approach on how to process the create the bmi value of a person at any time point
-- # The possible useful input concepts that we can use are height, weight and BMI, all of them separately.
-- # With height and weight we can calculate BMI, however we need first to apply several processes.
-- # These are: Standarization, filtering value per age, bmi calculation and filtering not possible values.

/*------------------------------------------------------------------
 Step 1 — Standardize height
   • "cm" → m
   • "mm" → m
   • "inch" → m
-------------------------------------------------------------------*/
COPY (
WITH standardized_height AS (
  SELECT
    person_id,
    ori_table,
    unique_id,
    coding_system,
    code,
    date AS height_date,
    CASE
      WHEN value_int BETWEEN 24 AND 240
        THEN ROUND(value_int / 100.0, 4)
      WHEN value_int BETWEEN 241 AND 2400
        THEN ROUND(value_int / 1000.0, 4)
      ELSE ROUND(value_int, 4)
    END AS height
  FROM (
    SELECT
      person_id,
      ori_table,
      unique_id,
      coding_system,
      code,
      date,
      TRY_CAST(value AS DECIMAL(10,4)) AS value_int
    FROM {complex_concept_name}
    WHERE concept_id = 'HEIGHT'
      AND TRY_CAST(value AS DECIMAL(10,4)) <= 2400
  ) t
)
,

/*------------------------------------------------------------------
 Step 2 — Standardize weight
   • "grams" -> kilograms
-------------------------------------------------------------------*/
standardized_weight AS (
    SELECT
        person_id,
        ori_table,
        unique_id,
        coding_system,
        code,
        weight_date,
        weight
    FROM (
        SELECT
            person_id,
            ori_table,
            unique_id,
            coding_system,
            code,
            date AS weight_date,
            CASE
                WHEN value_int BETWEEN 2000 AND 210000
                    THEN ROUND(value_int / 1000.0, 4)
                ELSE ROUND(value_int, 4)
            END AS weight
        FROM (
            SELECT
                person_id,
                ori_table,
                unique_id,
                coding_system,
                code,
                date,
                TRY_CAST(value AS DECIMAL(10,4)) AS value_int
            FROM {complex_concept_name}
            WHERE concept_id = 'WEIGHT'
        ) t
        WHERE value_int IS NOT NULL
    ) w
    WHERE weight <= 210
)
,

population_dedup AS (
    SELECT
        person_id,
        MIN(birth_date) AS birth_date
    FROM population
    WHERE birth_date IS NOT NULL
    GROUP BY person_id
)
,

/*------------------------------------------------------------------
 Step 3 — Compute age
-------------------------------------------------------------------*/
weight_with_age AS (
    SELECT
        w.person_id,
        w.ori_table,
        w.unique_id,
        w.coding_system,
        w.code,
        w.weight_date,
        w.weight,
        (w.weight_date - p.birth_date) / 365.25 AS age_at_date
    FROM standardized_weight w
    INNER JOIN population_dedup p ON w.person_id = p.person_id
)
,

height_with_age AS (
    SELECT
        h.person_id,
        h.height_date,
        h.height,
        (h.height_date - p.birth_date) / 365.25 AS age_at_date
    FROM standardized_height h
    INNER JOIN population_dedup p ON h.person_id = p.person_id
)
,

/*------------------------------------------------------------------
 Step 4 — Apply BMI age filter
-------------------------------------------------------------------*/
filtered_weight AS (
    SELECT
        wa.*
    FROM weight_with_age wa
    INNER JOIN bmi_age_filter baf
        ON wa.age_at_date >= baf.MIN_AGE AND wa.age_at_date < baf.MAX_AGE
       AND wa.weight >= baf.MIN_WEIGHT AND wa.weight < baf.MAX_WEIGHT
)
,

filtered_height AS (
    SELECT
        ha.*
    FROM height_with_age ha
    INNER JOIN bmi_age_filter baf
        ON ha.age_at_date >= baf.MIN_AGE AND ha.age_at_date < baf.MAX_AGE
       AND ha.height >= baf.MIN_HEIGHT AND ha.height < baf.MAX_HEIGHT
)
,

height_final AS (
  SELECT
  h.person_id,
  w.ori_table,
  w.unique_id,
  w.coding_system,
  w.code,
  w.weight_date,
  h.height_date,
  h.height
  FROM filtered_weight w
  INNER JOIN filtered_height h
  ON w.person_id = h.person_id
  AND h.height_date <= w.weight_date
  QUALIFY
  ROW_NUMBER() OVER (
    PARTITION BY h.person_id, w.weight_date
    ORDER BY h.height_date DESC
  ) = 1
  AND h.height IS NOT NULL
)
,

/*------------------------------------------------------------------
 Step 5 — Join height + weight → estimated BMI
-------------------------------------------------------------------*/
bmi_estimated AS (
    SELECT
        fw.person_id,
        fw.ori_table,
        fw.unique_id,
        fw.coding_system,
        fw.code,
        fw.weight_date AS bmi_date,
        hf.height_date,
        ROUND(fw.weight / (hf.height * hf.height), 4) AS bmi,
        2 AS importance
    FROM filtered_weight fw
    INNER JOIN height_final hf
        ON  fw.person_id  = hf.person_id
        AND fw.weight_date = hf.weight_date
       AND (
            (fw.age_at_date <= 18
             AND fw.weight_date <= hf.height_date + INTERVAL 2 YEAR)
         OR (fw.age_at_date > 18
             AND fw.weight_date <= hf.height_date + INTERVAL 3 YEAR)
       )
    WHERE fw.weight / (hf.height * hf.height) BETWEEN 10 AND 80
)
,
/*------------------------------------------------------------------
  Step 6 — Normalise existing BMI values
-------------------------------------------------------------------*/
  bmi_clean AS (
    SELECT
    person_id,
    ori_table,
    unique_id,
    coding_system,
    code,
    date,
    ROUND(bmi_value, 4) AS bmi,
    1 AS importance
    FROM (
    SELECT
        cr.person_id,
        cr.ori_table,
        cr.unique_id,
        cr.coding_system,
        cr.code,
        cr.date,
        TRY_CAST(cr.value AS DECIMAL(10,4)) AS bmi_value
    FROM {complex_concept_name} cr
    WHERE cr.concept_id = 'BMI'
  ) t
    WHERE bmi_value BETWEEN 10 AND 80
)
,

/*------------------------------------------------------------------
  Step 7 — Combine calculated and existing BMI
-------------------------------------------------------------------*/
all_bmi_dedup AS (
  SELECT
    person_id,
    ori_table,
    unique_id,
    coding_system,
    code,
    bmi_datetime AS date,
    value,
    importance
  FROM (
    SELECT *,
      ROW_NUMBER() OVER (
        PARTITION BY person_id, bmi_datetime
        ORDER BY importance ASC, value DESC
      ) AS rn
    FROM (
      SELECT
        person_id,
        ori_table,
        unique_id,
        coding_system,
        code,
        date AS bmi_datetime,
        ROUND(bmi, 4) AS value,
        1 AS importance
    FROM bmi_clean
      UNION ALL
      SELECT
        person_id,
        ori_table,
        unique_id,
        coding_system,
        code,
        bmi_date AS bmi_datetime,
        ROUND(bmi, 4) AS value,
        2 AS importance
    FROM bmi_estimated
    ) t
  ) t2
  WHERE rn = 1
)

SELECT
  person_id,
  ori_table,
  unique_id,
  '{complex_concept_name}' AS concept_id,
  coding_system,
  code,
  date,
  value
FROM all_bmi_dedup
ORDER BY
    person_id,
    date,
    value
) TO '{output_dir}'
(
    FORMAT PARQUET,
    PARTITION_BY (concept_id), APPEND
);
