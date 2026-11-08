#' Create CDC Risk
#' @description This function presents a standard approach to processing and
#' creating a person’s CDC risk score.
#' @param input_dir path
#' @param output_dir path
#' @param complex_concept_name string
#' @param sub_concept_ids character vector of concept_ids to include in the BMI.
#'
#' @return
create_cdc_risk <- function(
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
      "CREATE OR REPLACE VIEW {complex_concept_name} AS
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

  # Actual work ----
  ## Loading concept ranges
  concept_ranges <- picard::load(
    file_path = config_pipeline$common$dir_bridge,
    file_name = "concept_ranges.csv"
  )[concept_id == params$complex_concept_name]
  duckdb::duckdb_register(con, "concept_ranges", concept_ranges)

  main_query <- picard::load_sql_query(
    paste0(config_t2$sql_dir, "create_cdc_risk.sql"),
    params = list(
      source_table_name = params$complex_concept_name,
      new_concept_id = params$complex_concept_name
    )
  )

  DBI::dbExecute(
    con,
    glue::glue(
      "COPY (
        {main_query}
      ) TO '{params$output_dir}'
      (
        FORMAT PARQUET,
        PARTITION_BY (concept_id),
        APPEND
      )"
    )
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
    "Counts:\n",
    paste(capture.output(print(t(counts), sep = ":"))[-1], collapse = "\n")
  ))
}
