CREATE VIEW VACCINES_EDITED AS
WITH
    vaccines_edited AS (
        SELECT
            ori_table,
            unique_id,
            person_id,
            CASE
                WHEN vx_atc IS NULL
                OR vx_atc = ' '
                OR vx_atc = 'NA' THEN upper(vx_type)
                ELSE upper(vx_atc)
            END AS code,
            CASE
                WHEN vx_admin_date IS NULL THEN TRY_CAST (vx_record_date AS DATE)
                ELSE TRY_CAST (vx_admin_date AS DATE)
            END AS date,
            CASE
                WHEN vx_atc IS NULL
                OR vx_atc = ' '
                OR vx_atc = 'NA' THEN 'VX_TYPE'
                ELSE 'VX_ATC'
            END AS coding_system
        FROM
            (
                SELECT
                    *
                FROM
                    {tables_prefix} VACCINES e
                    INNER JOIN D3_SPELLS d ON e.person_id = d.person_id
            )
    ),
    vaccines_filtered AS (
        SELECT
            *
        FROM
            vaccines_edited
        WHERE
            date IS NOT NULL
            AND date >= TRY_CAST ('{start_lookback}' AS DATE)
            AND date <= TRY_CAST ('{end_study_date}' AS DATE)
    )
SELECT
    *
FROM
    vaccines_filtered;
