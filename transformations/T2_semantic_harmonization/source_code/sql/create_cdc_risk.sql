WITH
    -- Assign each concept a unique bit position for bitmask encoding
    concept_map AS (
        SELECT
            concept_id,
            POW(
                2,
                ROW_NUMBER() OVER (
                    ORDER BY
                        concept_id
                )
            )::BIGINT AS bit
        FROM
            (
                SELECT DISTINCT
                    concept_id
                FROM
                    {source_table_name}
            )
    ),
    -- Collapse each (person, date) to a single bitmask of concepts seen that day
    daily_bits AS (
        SELECT
            TRY_CAST (t.date AS DATE) AS date,
            t.person_id,
            BIT_OR(m.bit) AS day_bit
        FROM
            {source_table_name} t
            JOIN concept_map m USING (concept_id)
        WHERE
            t.date IS NOT NULL
        GROUP BY ALL
    ),
    -- Roll up a 10-year trailing window of concept bits per (person, date)
    -- 3652 days ≈ 10 years (accounting for leap years via 365.2 * 10)
    cdc_values AS (
        SELECT
            person_id,
            date,
            BIT_COUNT(
                BIT_OR(day_bit) OVER (
                    PARTITION BY
                        person_id
                    ORDER BY
                        date RANGE BETWEEN INTERVAL 3652 DAYS PRECEDING
                        AND CURRENT ROW
                )
            ) AS new_value
        FROM
            daily_bits
    ),
    cdc_categories AS (
        SELECT DISTINCT
            t1.person_id,
            t1.date,
            t2.new_value AS value
        FROM
            cdc_values t1
            JOIN concept_ranges t2 ON t1.new_value BETWEEN t2.lower_range AND t2.upper_range
    )
SELECT
    CAST(NULL AS VARCHAR) AS ori_table,
    CAST(NULL AS UUID) AS unique_id,
    person_id,
    CAST(NULL AS VARCHAR) AS code,
    CAST(NULL AS VARCHAR) AS coding_system,
    CAST(value AS VARCHAR) AS value,
    '{new_concept_id}' AS concept_id,
    date
FROM
    cdc_categories
