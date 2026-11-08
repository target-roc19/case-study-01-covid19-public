#' Create BMI
#' @description This function presents a standard approach to processing and
#' creating a person’s BMI value at any time.
#' @param input_dir path
#' @param output_dir path
#' @param complex_concept_name string
#' @param sub_concept_ids character vector of concept_ids to include in the BMI.
#'
#' @return
create_bmi_int <- function(
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
  # Add population
  population_path <- config_project$create_simple_concepts$d3_spells # nolint
  population <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT person_id, birth_date
      FROM read_parquet('{population_path}')"
    )
  )
  duckdb::duckdb_register(con, "population", population)

  # Add the BMI filters
  bmi_filter_path <- config_pipeline$common$bmi_age_filter # nolint
  bmi_age_filter <- picard::load(bmi_filter_path)
  duckdb::duckdb_register(con, "bmi_age_filter", bmi_age_filter)

  # Execute main query and persist the result as a partitioned parquet dataset
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      paste0(config_t2$sql_dir, "create_bmi_int.sql"), # nolint
      params = list(
        output_dir = params$output_dir,
        complex_concept_name = params$complex_concept_name
      )
    ),
    conn = con
  )

  # Checking rows affected
  validation <- validate_output(
    connection = con,
    complex_concept_name = params$complex_concept_name,
    output_directory = params$output_dir
  )

  logger::log_info(crayon::cyan(
    '\nCounts:\n',
    paste(
      capture.output(print(t(validation$counts), sep = ":"))[-1],
      collapse = '\n'
    )
  ))
}
