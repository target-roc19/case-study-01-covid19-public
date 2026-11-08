COPY (
    WITH 
        initial AS (
            SELECT * 
            FROM {complex_concept_name}
            WHERE concept_id IN ({obesity_concepts})
                OR (concept_id = {bmi_cat_concept} AND TRY_CAST(value AS INTEGER) = 4) 
        ) 

    SELECT 
        ori_table,
        unique_id,
        person_id,
        code,
        coding_system,
        TRY_CAST('TRUE' AS VARCHAR) AS value,
        date,
        '{complex_concept_name}' AS concept_id
    FROM initial
    ) 
    TO '{output_dir}' (
        FORMAT PARQUET,
        PARTITION_BY (concept_id),
        APPEND TRUE
    );
