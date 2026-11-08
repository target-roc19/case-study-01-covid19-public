CREATE VIEW concept_table AS
  SELECT
    ori_table,
    unique_id,
    person_id,
    code,
    coding_system,
    UPPER(CAST(value AS VARCHAR)) AS value,
    concept_id,
    date
  FROM read_parquet(
    '{dir_parquet_concepts}/**/*.parquet',
    hive_partitioning = TRUE
  )
