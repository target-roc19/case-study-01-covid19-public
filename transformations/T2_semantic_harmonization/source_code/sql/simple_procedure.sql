CREATE VIEW PROCEDURES_EDITED AS
SELECT
    ori_table,
    unique_id,
    person_id,
    procedure_code,
    procedure_date,
    UPPER(procedure_code_vocabulary) AS procedure_code_vocabulary
FROM
    (
        SELECT
            *
        FROM
            {tables_prefix} PROCEDURES e
            INNER JOIN D3_SPELLS d ON e.person_id = d.person_id
    )
WHERE
    procedure_date IS NOT NULL
    AND procedure_date >= TRY_CAST ('{start_lookback}' AS DATE)
    AND procedure_date <= TRY_CAST ('{end_study_date}' AS DATE)
