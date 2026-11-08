COPY (
WITH joined_data AS (
    SELECT
        ct.person_id,
        ct.ori_table,
        ct.unique_id,
        cm.super_concept_id AS concept_id,
        ct.coding_system,
        ct.code,
        ct.date,
        TRY_CAST(cm.value AS DOUBLE) AS value_num,
        CASE
            WHEN lower(ct.coding_system) LIKE '%dap%' THEN 1
            ELSE 2
        END AS metadata_priority
    FROM "{complex_concept_name}" AS ct
    JOIN "{concept_meaning}" AS cm
      ON ct.concept_id = cm.concept_id
),
ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY
                person_id,
                date,
                metadata_priority
            ORDER BY
                value_num DESC,
                unique_id
        ) AS rn
    FROM joined_data
)
SELECT
    person_id,
    ori_table,
    unique_id,
    concept_id,
    coding_system,
    code,
    date,
    CAST(value_num AS VARCHAR) AS value
FROM ranked
WHERE rn = 1
) TO '{output_dir}'
(
    FORMAT PARQUET,
    PARTITION_BY (concept_id),
    APPEND
);
