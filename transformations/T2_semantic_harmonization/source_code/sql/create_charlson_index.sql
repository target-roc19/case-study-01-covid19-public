WITH
    -- Pull the canonical weight for each concept once.
    -- Assumes one distinct weight per concept_id in the table.
    concept_weights AS (
        SELECT DISTINCT
            concept_id,
            TRY_CAST (value AS INTEGER) AS weight
        FROM
            concept_meaning
    ),
    -- Map each concept to its bit position (used for bitmask encoding/decoding)
    -- Join weight in here so the final decode can use it.
    concept_map AS (
        SELECT
            concept_id,
            weight,
            POW(
                2,
                ROW_NUMBER() OVER (
                    ORDER BY
                        concept_id
                ) - 1
            )::BIGINT AS bit
        FROM
            concept_weights
    ),
    daily_bits AS (
        SELECT
            t.person_id,
            TRY_CAST (t.date AS DATE) AS date,
            BIT_OR(m.bit) AS day_bit
        FROM
            {source_table_name} t
            JOIN concept_map m USING (concept_id)
        GROUP BY ALL
    ),
    windowed AS (
        SELECT
            person_id,
            date,
            BIT_OR(day_bit) OVER (
                PARTITION BY
                    person_id
                ORDER BY
                    date RANGE BETWEEN INTERVAL 3652 DAYS PRECEDING
                    AND CURRENT ROW
            ) AS concept_bitmask
        FROM
            daily_bits
    ),
    charlson AS (
        SELECT
            w.person_id,
            w.date,
            SUM(
                CASE
                    WHEN (w.concept_bitmask & m.bit) = m.bit THEN m.weight
                    ELSE 0
                END
            ) AS value
        FROM
            windowed w
            CROSS JOIN concept_map m
        GROUP BY ALL
    )
SELECT
    TRY_CAST (NULL AS VARCHAR) AS ori_table,
    TRY_CAST (NULL AS UUID) AS unique_id,
    person_id,
    TRY_CAST (NULL AS VARCHAR) AS code,
    TRY_CAST (NULL AS VARCHAR) AS coding_system,
    TRY_CAST (value AS VARCHAR) AS value,
    '{new_concept_id}' AS concept_id,
    date
FROM
    charlson
