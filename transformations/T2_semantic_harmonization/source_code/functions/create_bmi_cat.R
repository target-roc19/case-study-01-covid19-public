#' Create BMI Category
#' @param input_dir path
#' @param output_dir path
#' @param complex_concept_name string
#' @param sub_concept_ids character vector of concept_ids to include in the BMI.
#' @description For adults (≥18), loads PP_BMI_INT and classifies records using
#' BMI thresholds defined in concept_ranges.csv, then maps these to concept
#' meanings under the BMI_CAT super‑concept.
#' For children (2–18 years), CDC LMS reference data is used to interpolate
#' age‑ and sex‑specific percentiles.
#' BMI categories are then assigned based on CDC percentile cut‑offs.
categorize_bmi <- function(
  input_dir,
  output_dir,
  complex_concept_name = NULL,
  sub_concept_ids = NULL
) {
  picard::load_config()
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

  if (is.null(existing_sub_concepts) || length(existing_sub_concepts) == 0) {
    logger::log_warn(
      paste0(
        "No existing sub-concept IDs found for concept: ",
        params$complex_concept_name,
        ". Skipping BMI categorization."
      )
    )
    return(invisible(NULL))
  }

  # Open DuckDB connection
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(
    con,
    glue::glue(
      "CREATE OR REPLACE VIEW {params$complex_concept_name} AS
        SELECT *
        FROM read_parquet(
          ['{params$input_dir}/**/*.parquet'],
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
  # Add persons
  persons_path <- config_project$process_observation_periods$D3_person_file # nolint
  persons <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT person_id, birth_date, sex_at_instance_creation
      FROM read_parquet('{persons_path}')"
    )
  )
  duckdb::duckdb_register(con, "persons", persons)

  # Add the BMI filters
  concept_ranges_path <- config_pipeline$common$concept_ranges # nolint
  concept_ranges <- picard::load(
    concept_ranges_path
  )
  duckdb::duckdb_register(con, "concept_ranges", concept_ranges)

  # Loading the meaning for each concept related to the BMI categories.
  concept_meaning_path <- config_project$create_complex_concepts$concept_meaning # nolint
  concept_meaning <- picard::load(concept_meaning_path)[
    super_concept_id %in% "BMI_CAT"
  ]
  duckdb::duckdb_register(con, "concept_meaning", concept_meaning)

  if (config_values$analysis_for_children) {
    # nolint
    logger::log_info(
      "Analysis is for children. BMI categorization will use CDC percentiles."
    )

    child_ctes <- picard::load_sql_query(
      paste0(config_t2$sql_dir, "create_bmi_cat_children.sql"), # nolint
      params = list(
        complex_concept_name = params$complex_concept_name,
        cdc_ref_path = file.path(
          config_pipeline$common$dir_bridge, # nolint
          "cdc_ref_bmichildren.csv"
        )
      )
    )
  } else {
    logger::log_info(
      "Analysis is not for children. Pediatric BMI categorization is skipped."
    )
    child_ctes <- ""
  }

  child_results_union <- if (config_values$analysis_for_children) {
    # nolint
    paste0(
      "    SELECT * FROM child_results\n",
      "    UNION ALL\n"
    )
  } else {
    ""
  }

  picard::execute_sql_file(
    sql = picard::load_sql_query(
      paste0(config_t2$sql_dir, "create_bmi_cat.sql"), # nolint
      params = list(
        child_ctes = child_ctes,
        child_results_union = child_results_union,
        output_dir = params$output_dir,
        complex_concept_name = params$complex_concept_name
      )
    ),
    conn = con
  )

  ##############################################################################
  # Output results
  ##############################################################################
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
