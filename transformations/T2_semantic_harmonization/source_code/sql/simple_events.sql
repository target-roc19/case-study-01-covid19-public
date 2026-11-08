CREATE VIEW EVENTS_EDITED AS
SELECT
    ori_table,
    unique_id,
    person_id,
    CASE
        WHEN UPPER(event_record_vocabulary) = 'FREE_TEXT' THEN UPPER(event_free_text)
        ELSE UPPER(event_code)
    END AS event_code,
    start_date_record,
    event_record_vocabulary
FROM
    (
        SELECT
            *
        FROM
            {tables_prefix} EVENTS e
            INNER JOIN D3_SPELLS d ON e.person_id = d.person_id
    )
WHERE
    start_date_record IS NOT NULL
    AND start_date_record >= TRY_CAST ('{start_lookback}' AS DATE)
    AND start_date_record <= TRY_CAST ('{end_study_date}' AS DATE)
