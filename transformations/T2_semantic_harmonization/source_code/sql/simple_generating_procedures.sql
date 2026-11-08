COPY (
   SELECT
        DISTINCT prod_clean.ori_table,
        prod_clean.unique_id,
        prod_clean.person_id,
        codelist.code,
        codelist.coding_system,
        'TRUE' AS value,
        codelist.concept_id,
        prod_clean.procedure_date AS date
   FROM PROCEDURES_EDITED prod_clean
   INNER JOIN DAP_SPECIFIC_CODELIST_PROCEDURES codelist
        ON prod_clean.procedure_code = codelist.code AND
         prod_clean.procedure_code_vocabulary = UPPER(codelist.coding_system)
    )
   TO '{dir_parquet_concepts}' (
      FORMAT PARQUET,
      PARTITION_BY (concept_id),
      APPEND TRUE
    );
