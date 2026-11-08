# Creating indicator if someone is dead at the day of death

create_death <- function(input_dir,
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

  d3_spells_file <- config_project$create_complex_concepts$d3_spells
  D3_SPELLS <- picard::load(d3_spells_file)
  results <- unique(D3_SPELLS[, .(person_id, death_date)])

  # Defining value = true if date (death_date) is not missing
  results[, date := death_date]
  results <- results[!is.na(date)] # selecting the cases that are dead
  results[, value := TRUE]
  results[, concept_id := complex_concept_name]

  # Add missing columns
  required_cols_d3_concept <- config_project$create_complex_concepts$required_cols_d3_concept
  missing_cols <- required_cols_d3_concept[!required_cols_d3_concept %in% names(results)]
  lapply(missing_cols, function(x) results[, eval(x) := NA])

  concepts_db_conn <- duckdb::dbConnect(
    duckdb::duckdb(),
    ":memory:"
  )

  # combine D3 death with concept_id = I_COVID19DX_COV if it exists
  if (
    length(existing_sub_concepts) > 0 &&
    any(existing_sub_concepts %in% "I_COVID19DX_COV")
  ) {

    # Write death results to DuckDB
    DBI::dbWriteTable(
      concepts_db_conn,
      "results",
      results,
      overwrite = TRUE
    )

    # first row of concept_id = I_COVID19DX_COV per person
    # AND having a death date in D3_SPELLS
    query <- glue::glue(
      "
      WITH first_per_person AS (
        SELECT *
        FROM read_parquet(
          '{input_dir}/**/*.parquet',
          hive_partitioning = true,
          union_by_name     = true
        )
        WHERE concept_id IN ({partitions})
        QUALIFY ROW_NUMBER() OVER (
          PARTITION BY person_id
          ORDER BY date ASC NULLS LAST
        ) = 1
      )

      SELECT
        c.* EXCLUDE (date),
        r.date AS date
      FROM first_per_person AS c
      INNER JOIN results AS r
        ON c.person_id = r.person_id
      ",
      partitions = paste0(
        "'", existing_sub_concepts, "'",
        collapse = ", "
      )
    )
    combined <- data.table::as.data.table(
      DBI::dbGetQuery(concepts_db_conn, query)
    )

    combined[, value := TRUE]
    combined[, concept_id := complex_concept_name]

    missing_cols <- required_cols_d3_concept[
      !required_cols_d3_concept %in% names(combined)
    ]

    lapply(
      missing_cols,
      function(x) combined[, eval(x) := NA]
    )
    results <- combined
  }

  # Return summary invisibly
  arrow::write_dataset(
    results[, ..required_cols_d3_concept],
    path = output_dir,
    format = "parquet",
    partitioning = "concept_id"
  )

  counts <- get_counts(
    connection = concepts_db_conn,
    complex_concept_name = complex_concept_name,
    output_directory = output_dir,
    data_object = results
  )

  logger::log_info(crayon::cyan(
    'Counts:\n',
    paste(capture.output(print(t(counts), sep = ":"))[-1], collapse = '\n')
  ))

  return(invisible(counts))
}
