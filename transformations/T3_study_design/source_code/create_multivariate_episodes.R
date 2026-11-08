################################################################
# create_multivariate_episodes ---
# Aim:
# Build D3_MULTIVARIATE_EPISODES by running the episodeR multivariate
# episodes pipeline on study variables flagged with eligibility = TRUE.
#
# Input files: ---
# - configuration/config_project.yaml
# - configuration/config_values.yaml
# - transformations/common_configuration/RWE-BRIDGE/study_variables.csv
# - data/D3_study_variables/D3_UNIVARIATE_EPISODES_parquet (hive-partitioned parquet folder)
# - data/D3_study_variables/D3_SPELLS.parquet
#
# Output: ---
# - data/D3_study_variables/D3_MULTIVARIATE_EPISODES_parquet/
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
  lm$start_script("Creating D3 Multivariate Episodes")
  lm$start_capturing_prints()
}

logger::log_info("[Multivariate episodes] - START")
logger::log_info("Start of the create_multivariate_episodes script.")

## Config loading ---
picard::load_config()
mv_config <- config_project$create_multivariate_episodes
dir_d3 <- config_project$outputs$dir_d3

## Audit setup ---
picard::audit_start(
  file_name = "create_multivariate_episodes",
  deap_name = config_project$DEAP_configuration$name
)

# Load study variables metadata ---
logger::log_info("Loading study variables metadata from BRIDGE.")
sv_meta <- data.table::fread(file.path(
  config_pipeline$common$dir_bridge,
  "study_variables.csv"
))

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

# Add batch column (no batching by default) ---
sv_meta[, batch := FALSE]
sv_meta <- sv_meta[raw_concept %notin% "D3"]
sv_meta <- sv_meta[variable_id %notin% "STABLE_PRECONDITIONS"]
# Load D3_SPELLS to extract person_ids ---
logger::log_info("Loading D3_SPELLS to extract person_ids.")
D3_SPELLS <- picard::load(
  mv_config$d3_spells_file_name
)
person_ids <- unique(D3_SPELLS$person_id)
logger::log_info(paste0("Number of unique person_ids: ", length(person_ids)))

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Resolve SQL directory from episodeR package ---
sql_dir <- system.file("sql", package = "episodeR")

# Run the multivariate episodes pipeline ---
logger::log_info("Running episodeR::multivariate_episodes_pipeline.")
episodeR::multivariate_episodes_pipeline(
  study_variables = sv_meta,
  con = con,
  d3_univariate_episodes_path = file.path(
    mv_config$d3_univariate_episodes
  ),
  sql_dir = sql_dir,
  output_path = "data/D3_study_variables/D3_MULTIVARIATE_EPISODES",
  person_ids = person_ids,
  batch_size = mv_config$batch_size,
  batch_column = "batch",
  data_type_col = "data_type"
)

DBI::dbDisconnect(con, shutdown = TRUE)

logger::log_info("[Multivariate episodes] - END")

## Audit end ---
picard::audit_end()

## Script end ---
if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
