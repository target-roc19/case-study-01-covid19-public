# Pass death_registry concept and print log

create_death_reg <- function(input_dir,
                         output_dir,
                         complex_concept_name = NULL,
                         sub_concept_ids = NULL) {
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
  dir_intermediate_t2 <- file.path(
    config_t2$T2$root,
    config_t2$T2$intermediate
  )

  # Initialize cleanup on exit
  on.exit(
    {
      if (exists("concepts_db_conn", inherits = FALSE)) {
        duckdb::dbDisconnect(concepts_db_conn)
      }
    },
    add = TRUE
  )

 # required columns
  required_cols_d3_concept <- config_project$create_complex_concepts$required_cols_d3_concept

  if (length(existing_sub_concepts) > 0) {
    concepts_db_conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
    query <- glue::glue(
      "SELECT *
          FROM read_parquet(
          '{input_dir}/**/*.parquet',
          hive_partitioning = true,
          union_by_name     = true
          )
        WHERE concept_id IN ({partitions})
        AND date IS NOT NULL",
      partitions = paste0("'", existing_sub_concepts, "'", collapse = ", ")
    )
    registry <- data.table::as.data.table(DBI::dbGetQuery(concepts_db_conn, query))
    registry[, value := TRUE]
    registry[, concept_id := complex_concept_name]

    missing_cols <- required_cols_d3_concept[!required_cols_d3_concept %in% names(registry)]
    lapply(missing_cols, function(x) registry[, eval(x) := NA])

    # Return summary invisibly
    arrow::write_dataset(
      registry[, ..required_cols_d3_concept],
      path = output_dir,
      format = "parquet",
      partitioning = "concept_id"
    )

    counts <- get_counts(
      connection = concepts_db_conn,
      complex_concept_name = complex_concept_name,
      output_directory = output_dir,
      data_object = registry
    )

    logger::log_info(crayon::cyan(
      'Counts:\n',
      paste(capture.output(print(t(counts), sep = ":"))[-1], collapse = '\n')
    ))

    return(invisible(counts))
  } else {
    logger::log_info(crayon::cyan(
      "No DEATH_REGISTRY data found for concept_id: ",
      complex_concept_name,
      ". No output generated."
    ))
  }
}
