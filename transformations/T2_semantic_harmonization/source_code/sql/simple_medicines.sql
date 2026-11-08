CREATE VIEW MEDICINES_EDITED AS
WITH
    medicines AS (
        SELECT
            t1.ori_table,
            t1.unique_id,
            t1.person_id,
            upper({code_col}) AS code,
            '{coding_system}' AS coding_system,
            'TRUE' AS value,
            CASE
                WHEN date_dispensing IS NULL THEN date_prescription
                ELSE date_dispensing
            END AS date
        FROM
            {tables_prefix} MEDICINES t1
            INNER JOIN D3_SPELLS d ON t1.person_id = d.person_id
    ),
    medicines_filter AS (
        SELECT
            *
        FROM
            medicines
        WHERE
            date IS NOT NULL
            AND date >= TRY_CAST ('{start_lookback}' AS DATE)
            AND date <= TRY_CAST ('{end_study_date}' AS DATE)
    )
SELECT
    *
FROM
    medicines_filter;
