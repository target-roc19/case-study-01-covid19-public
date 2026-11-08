COPY (

  WITH source_concepts AS (
    SELECT
      ori_table,
      unique_id,
      person_id,
      code,
      coding_system,
      CAST(value AS VARCHAR) AS value,
      TRY_CAST(date AS DATE) AS date,
      concept_id
    FROM '{complex_concept_name}'
  ),

  bmi_measurements AS (
    SELECT
      src.ori_table,
      src.unique_id,
      src.code,
      src.coding_system,
      src.person_id,
      src.date,
      TRY_CAST(src.value AS DOUBLE) AS bmi_value,
      p.sex_at_instance_creation AS sex,
      CAST(
        date_diff(
          'day',
          TRY_CAST(p.birth_date AS DATE),
          src.date
        ) + 1 AS DOUBLE
      ) / 365.25 AS age,
      src.concept_id
    FROM source_concepts src
    INNER JOIN persons p
      ON src.person_id = p.person_id
    WHERE src.concept_id = 'PP_BMI_INT'
    AND src.date IS NOT NULL
  ),

  {child_ctes}

  adult_results AS (
    SELECT
      bmi.ori_table,
      bmi.unique_id,
      bmi.person_id,
      bmi.code,
      bmi.coding_system,
      CAST(ranges.new_value AS VARCHAR) AS value,
      bmi.date,
      bmi.concept_id
    FROM bmi_measurements bmi
    INNER JOIN concept_ranges ranges
      ON ranges.concept_id = bmi.concept_id
      AND bmi.bmi_value >= TRY_CAST(ranges.lower_range AS DOUBLE)
      AND bmi.bmi_value < TRY_CAST(ranges.upper_range AS DOUBLE)
    WHERE bmi.age > 18.0
  ),

  concept_meaning_results AS (
    SELECT
      src.ori_table,
      src.unique_id,
      src.person_id,
      src.code,
      src.coding_system,
      CAST(mappings.value AS VARCHAR) AS value,
      src.date,
      src.concept_id
    FROM source_concepts src
    INNER JOIN concept_meaning AS mappings
      ON src.concept_id = mappings.concept_id
  ),

  combined_results AS (
    {child_results_union}
    SELECT * FROM adult_results
    UNION ALL
    SELECT * FROM concept_meaning_results
  ),

  deduplicated_results AS (
    SELECT
      ori_table,
      unique_id,
      person_id,
      code,
      coding_system,
      value,
      date
    FROM (
      SELECT
        res.*,
        ROW_NUMBER() OVER (
          PARTITION BY res.person_id, res.date
          ORDER BY COALESCE(
            TRY_CAST(res.value AS DOUBLE),
            -1.0
          ) DESC
        ) AS row_num
      FROM combined_results res
    )
    WHERE row_num = 1
  )

  SELECT
    ori_table,
    unique_id,
    person_id,
    code,
    coding_system,
    value,
    date,
    '{complex_concept_name}' AS concept_id
  FROM deduplicated_results
) TO '{output_dir}' (
  FORMAT PARQUET,
  PARTITION_BY (concept_id),
  APPEND TRUE
);
