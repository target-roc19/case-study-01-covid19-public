CREATE TABLE concept_table (
    person_id TEXT,
    ori_table TEXT,   -- Original CDM table
    unique_id UUID,  -- Row ID in CDM_database
    concept_id TEXT,
    coding_system TEXT,
    code TEXT,
    date DATE,
    value TEXT,
    CONSTRAINT unique_ori_concept UNIQUE (unique_id, concept_id)
);
