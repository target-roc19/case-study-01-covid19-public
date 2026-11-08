################################################################
# create_univariate_episodes ---
# Aim:
# Build D3_UNIVARIATE_EPISODES by running the episodeR univariate
# episodes pipeline on study variables flagged with eligibility = TRUE.
#
# Input files: ---
# - configuration/config_project.yaml
# - configuration/config_values.yaml
# - transformations/common_configuration/RWE-BRIDGE/study_variables.csv
# - data/D3_study_variables/D3_CONCEPTS_parquet (hive-partitioned parquet folder)
# - data/D3_study_variables/D3_SPELLS.parquet
#
# Output: ---
# - data/D3_study_variables/D3_UNIVARIATE_EPISODES_parquet/
################################################################

# Initial setup ---
## Script logger ---
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("Creating D3 Univariate Episodes")
  lm$start_capturing_prints()
}

logger::log_info("[Univariate episodes] - START")
logger::log_info("Start of the create_univariate_episodes script.")

## Config loading ---
picard::load_config()
uv_config <- config_project$create_univariate_episodes
dir_d3 <- config_project$outputs$dir_d3

## Audit setup ---
picard::audit_start(
  file_name = "create_univariate_episodes",
  deap_name = config_project$DEAP_configuration$name
)

# Load study variables metadata ---
logger::log_info("Loading study variables metadata from BRIDGE.")
sv_meta <- picard::load(file.path(
  config_pipeline$common$dir_bridge,
  "study_variables.csv"
)) |>
  data.table::as.data.table()

# Filter to eligibility == TRUE ---
logger::log_info("Filtering study variables to eligibility == TRUE.")
sv_meta <- sv_meta[eligibility == TRUE]
logger::log_info(paste0(
  "Number of study variables with eligibility = TRUE: ",
  nrow(sv_meta)
))

# Preprocess look-back columns (convert to non-negative integer) ---
sv_meta[, start_look_back := abs(as.integer(start_look_back))]
sv_meta[, end_look_back := abs(as.integer(end_look_back))]

sv_meta <- sv_meta[raw_concept %notin% "D3"]
sv_meta <- sv_meta[variable_id %notin% "STABLE_PRECONDITIONS"]
# Add batch column (no batching by default) ---
sv_meta[, batch := TRUE]

# Load D3_SPELLS to extract person_ids ---
logger::log_info("Loading D3_SPELLS to extract person_ids.")
d3_spells_dir <- config_project$create_univariate_episodes$d3_spells
D3_SPELLS <- picard::load(
  d3_spells_dir
) |>
  data.table::as.data.table()

person_ids <- unique(D3_SPELLS$person_id)
logger::log_info(paste0("Number of unique person_ids: ", length(person_ids)))

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# D3_CONCEPTS: read only required concept_id partitions from hive-partitioned parquet folder
concept_ids <- unique(sv_meta$concept_id)
concept_ids <- concept_ids[!is.na(concept_ids) & nzchar(concept_ids)]
concept_id_filter <- paste(sprintf("'%s'", concept_ids), collapse = ", ")
DBI::dbExecute(
  con,
  sprintf(
    "CREATE OR REPLACE VIEW D3_CONCEPTS AS
   SELECT * FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE, union_by_name = TRUE)
   WHERE concept_id IN (%s)",
    file.path(config_project$create_univariate_episodes$d3_concepts),
    concept_id_filter
  )
)

unlink(file.path(uv_config$output_hive_path), recursive = TRUE) # clean output directory before writing new results

# Resolve SQL directory from episodeR package ---
sql_dir <- system.file("sql", package = "episodeR")

# Run the univariate episodes pipeline ---
logger::log_info("Running episodeR::univariate_episodes_pipeline.")
episodeR::univariate_episodes_pipeline(
  study_variables = sv_meta,
  con = con,
  person_ids = person_ids,
  sql_dir = sql_dir,
  start_study_date = config_values$start_study_date,
  end_date_missing_inclusion = config_values$end_study_date,
  output_hive_path = file.path(uv_config$output_hive_path),
  batch_size = uv_config$batch_size,
  batch_column = "batch",
  missing_col = "missing_set_to"
)

logger::log_info("[Univariate episodes] - END")

## Audit end ---
picard::audit_end()

## Script end ---
if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
