COPY (
     SELECT DISTINCT
     vac_clean.ori_table,
     vac_clean.unique_id,
     vac_clean.person_id,
     vac_clean.code,
     vac_clean.coding_system,
     'TRUE' AS value,
     codelist.concept_id,
     vac_clean.date

     FROM (SELECT *
          FROM VACCINES_EDITED
          ) vac_clean

     INNER JOIN DAP_SPECIFIC_CODELIST_VACCINES codelist
          ON vac_clean.code = codelist.code
          AND vac_clean.coding_system = codelist.coding_system
     )
     TO '{dir_parquet_concepts}' (
          FORMAT PARQUET,
     PARTITION_BY (concept_id), APPEND TRUE);
