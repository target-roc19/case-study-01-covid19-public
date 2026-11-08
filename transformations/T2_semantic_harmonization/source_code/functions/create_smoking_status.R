# create_smoking_status

# Loads all `PP_CHARLSON_INDEX` records and assigns weights based on `concept_meaning.csv`.
# Daily values are calculated by summing all weights per person per day.
# A rolling 10‑year cumulative value is then computed for each date.
# Results are saved.
create_smoking_status <- function(
  input_dir,
  output_dir,
  complex_concept_name = NULL,
  sub_concept_ids = NULL
) {
  # Normalise inputs
  params <- normalise_inputs(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids
  )
  # Validate inputs
  validate_inputs(
    input_dir = params$input_dir,
    complex_concept_name = params$complex_concept_name,
    sub_concept_ids = params$sub_concept_ids
  )

  # Clean output directory
  ensure_clean_output_directory(params$output_dir, params$complex_concept_name)

  # Getting existing sub_concept_ids
  existing_sub_concepts <- prepare_sub_concept_ids(
    sub_concept_ids = params$sub_concept_ids,
    input_directory = params$input_dir
  )

  # Open DuckDB connection
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(
    con,
    glue::glue(
      "CREATE OR REPLACE VIEW {params$complex_concept_name} AS
        SELECT *
        FROM read_parquet(
          ['{input_dir}/**/*.parquet'],
          hive_partitioning = true,
          union_by_name     = true
          )
        WHERE concept_id IN ({partitions})",
      partitions = paste0("'", existing_sub_concepts, "'", collapse = ", ")
    )
  )

  ##############################################################################
  # Actual work
  ##############################################################################
  ## Loading

  # Loading the meaning for each concept related to the CHARLSON index.
  concept_meaning_path <- config_project$create_complex_concepts$concept_meaning # nolint
  concept_meaning <- picard::load(concept_meaning_path)[
    super_concept_id %in% "PP_SMOKING_CAT"
  ]

  # mapping remove L_SMOKESTATUS_COV only for CPRD
  if (config_project$DEAP_configuration$name == "CPRD") {
    concept_meaning <- concept_meaning[!concept_id %in% "L_SMOKESTATUS_COV"]
  }

  duckdb::duckdb_register(con, "concept_meaning", concept_meaning)


  ##############################################################################
  # Running new concept-specific logic
  ##############################################################################
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      paste0(config_t2$sql_dir, "create_smoking_status.sql"), # nolint
      params = list(
        complex_concept_name = params$complex_concept_name,
        concept_meaning = "concept_meaning",
        output_dir = output_dir
      )
    ),
    conn = con
  )

  ##############################################################################
  # Output results
  ##############################################################################
  # Checking rows affected
  counts <- get_counts(
    connection = con,
    complex_concept_name = params$complex_concept_name,
    output_directory = params$output_dir
  )
  logger::log_info(crayon::cyan(
    'Counts:\n',
    paste(capture.output(print(t(counts), sep = ":"))[-1], collapse = '\n')
  ))
}
