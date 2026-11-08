COPY (
    SELECT DISTINCT
    med_clean.ori_table,
    med_clean.unique_id,
    med_clean.person_id,
    med_clean.code,
    med_clean.coding_system,
    'TRUE' AS value,
    codelist.concept_id,
    med_clean.date

    FROM MEDICINES_EDITED med_clean
        INNER JOIN DAP_SPECIFIC_CODELIST_MEDICINES codelist
        ON med_clean.code = codelist.code AND
        med_clean.coding_system = codelist.coding_system
    )
    TO '{dir_parquet_concepts}'(
        FORMAT PARQUET,
        PARTITION_BY (concept_id),
        APPEND TRUE
    );
