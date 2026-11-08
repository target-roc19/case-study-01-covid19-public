COPY (
	SELECT DISTINCT
    eve_clean.ori_table,
    eve_clean.unique_id,
    eve_clean.person_id,
    codelist.code,
    codelist.coding_system,
    'TRUE' AS value,
    codelist.concept_id,
    start_date_record AS date

    FROM EVENTS_EDITED eve_clean
    INNER JOIN DAP_SPECIFIC_CODELIST_EVENTS codelist
		ON eve_clean.event_code = codelist.code
    	AND eve_clean.event_record_vocabulary = UPPER(codelist.coding_system)
    )
    TO '{dir_parquet_concepts}' (
      	FORMAT PARQUET,
     	PARTITION_BY (concept_id),
      	APPEND TRUE
    );
