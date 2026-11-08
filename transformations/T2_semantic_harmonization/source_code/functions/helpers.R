#' Helpers for creating complex concepts functions

# Normalisers ----

#' Normalise sub_concept_ids
#'
#' Normalises sub_concept_ids by trimming whitespace and converting to
#'  uppercase. If any changes are made to the input sub_concept_ids, a warning
#'  is issued listing the original values that were modified.
#'
#' @param sub_concept_ids Character vector of sub_concept_ids to normalise.
#'
#' @returns A character vector of sub_concept_ids.
normalise_sub_concept_ids <- function(sub_concept_ids) {
  normalised <- gsub("\\s+", "", toupper(sub_concept_ids))

  dirty <- sub_concept_ids[normalised != sub_concept_ids]
  if (length(dirty) > 0L) {
    logger::log_warn(
      glue::glue(
        "Whitespace removed from sub_concept_ids: ",
        "{paste(dirty, collapse = ', ')}."
      ),
      class = "sub_concept_ids_whitespace_warn"
    )
  }

  normalised
}

#' Normalise inputs
#'
#' @param sub_concept_ids A character vector of sub_concept_ids.
#' @param complex_concept_name A character string of the complex concept name.
#' @param input_dir Path to the input directory.
#' @param output_dir Path to the output directory.
#'
#' @returns A list of normalised inputs.
normalise_inputs <- function(
  sub_concept_ids,
  complex_concept_name,
  input_dir,
  output_dir
) {
  list(
    sub_concept_ids = normalise_sub_concept_ids(sub_concept_ids),
    complex_concept_name = trimws(toupper(complex_concept_name)),
    input_dir = normalizePath(input_dir, mustWork = TRUE),
    output_dir = normalizePath(output_dir, mustWork = FALSE)
  )
}

# Validators ----

#' Validate inputs
#'
#' @param sub_concept_ids A character vector of sub_concept_ids.
#' @param complex_concept_name A character string of the complex concept name.
#' @param input_directory Path to the input directory.
#'
#' @returns Invisibly returns TRUE if the validated input parameters of all
#'  checks are passed.
validate_inputs <- function(
  sub_concept_ids,
  complex_concept_name,
  input_directory
) {
  errors <- checkmate::makeAssertCollection()

  checkmate::assert_character(
    sub_concept_ids,
    min.len = 1L,
    any.missing = FALSE,
    .var.name = "sub_concept_ids",
    add = errors
  )
  checkmate::assert_string(
    complex_concept_name,
    min.chars = 1L,
    .var.name = "complex_concept_name",
    add = errors
  )
  checkmate::assert_string(
    input_directory,
    .var.name = "input_directory",
    add = errors
  )

  checkmate::reportAssertions(errors)
  invisible(TRUE)
}

# Checkers for which partitions exist ----

#' Get existing partitions
#'
#' Checks which of the specified sub_concept_ids have corresponding partitions
#'  in the input directory. If no partitions are found, a warning is issued and
#'  NULL is returned. Otherwise, a character vector of existing sub_concept_ids
#'  is returned.
#'
#' @param sub_concept_ids Character vector of sub_concept_ids.
#' @param input_directory Path to the input directory.
#'
#' @returns A character vector of existing sub_concept_ids if any are found,
#'   otherwise NULL with a warning.
get_existing_partitions <- function(sub_concept_ids, input_directory) {
  partition_paths <- fs::path(
    input_directory,
    paste0("concept_id=", sub_concept_ids)
  )
  existing_partitions <- sub_concept_ids[fs::dir_exists(partition_paths)]

  if (length(existing_partitions) == 0L) {
    logger::log_warn(
      glue::glue(
        "None of the requested concept_id partitions exist under ",
        "'{input_directory}'. Returning NULL."
      ),
      class = "non_existing_partition_warn"
    )
    return(NULL)
  }

  existing_partitions
}

#' Checks and reports missing partitions
#'
#' Checks which of the specified sub_concept_ids do not have corresponding
#'   partitions in the input directory and issues a warning listing the missing
#'   partitions. If all partitions are present, it does nothing.
#'
#' @param sub_concept_ids Character vector of sub_concept_ids.
#' @param existing_partitions Character vector of existing sub_concept_ids with
#'  partitions in the input directory, as returned by `get_existing_partitions`.
#'
#' @returns Returns which sub_concept_ids have no parquet files in the input
#'   directory.
report_missing_partitions <- function(sub_concept_ids, existing_partitions) {
  missing_partitions <- setdiff(sub_concept_ids, existing_partitions)
  if (length(missing_partitions) > 0L) {
    logger::log_warn(
      glue::glue(
        "Partitions not found on disk, skipping: ",
        "{paste(missing_partitions, collapse = ', ')}."
      ),
      class = "missing_partition_warn"
    )
  }
  invisible(NULL)
}

#' Get non-empty partitions
#'
#' Checks which of the existing partitions contain non-empty parquet files. If
#'  all matched partitions are empty, a warning is issued and NULL is returned.
#'  Otherwise, a character vector of non-empty sub_concept_ids is returned.
#'
#' @param existing_partitions Character vector of existing sub_concept_ids with
#'   partitions in the input directory, as returned by
#' `get_existing_partitions`.
#' @param input_directory Path to the input directory.
#'
#' @returns Returns which of the existing partitions contain non-empty parquet
#'   files, or NULL if all are empty.
get_non_empty_partitions <- function(existing_partitions, input_directory) {
  non_empty_partitions <- purrr::keep(existing_partitions, function(cid) {
    info <- fs::dir_info(
      fs::path(input_directory, paste0("concept_id=", cid)),
      glob = "*.parquet",
      recurse = TRUE
    )
    nrow(info) > 0L && any(info$size > 0L)
  })

  if (length(non_empty_partitions) == 0L) {
    logger::log_warn(
      "All matched partitions contain empty parquet files. Returning NULL.",
      class = "empty_partition_warn"
    )
    return(NULL)
  }

  non_empty_partitions
}

#' Checks and returns non-empty, non-missing sub_concept_ids
#'
#' The function wraps the process of validating sub_concept_ids, checking for
#'  existing partitions, reporting missing partitions, and identifying non-empty
#'  partitions. It returns a character vector of sub_concept_ids that have
#'  non-empty parquet files in the input directory, or NULL if none are valid.
#'
#' @param sub_concept_ids Character vector of sub_concept_ids to check.
#' @param input_directory Path to the input directory.
#'
#' @returns A character vector of valid sub_concept_ids with non-empty parquet
#'  files, or NULL if none are valid.
prepare_sub_concept_ids <- function(sub_concept_ids, input_directory) {
  existing_partitions <- get_existing_partitions(
    sub_concept_ids = sub_concept_ids,
    input_directory = input_directory
  )

  if (is.null(existing_partitions)) {
    return(NULL)
  }

  report_missing_partitions(
    sub_concept_ids = sub_concept_ids,
    existing_partitions = existing_partitions
  )

  get_non_empty_partitions(
    existing_partitions = existing_partitions,
    input_directory = input_directory
  )
}

# Clean output directory ----

#' Clean output directory
#'
#' Ensures that the output directory for the complex concept is clean by
#'  deleting any existing partition directory for the complex concept name. If
#'  the partition directory does not exist, it does nothing.
#'
#' @param output_directory Path to the output directory.
#' @param complex_concept_name Character string of the complex concept name.
#'
#' @returns NULL invisibly.
ensure_clean_output_directory <- function(
  output_directory,
  complex_concept_name
) {
  out_partition_dir <- fs::path(
    output_directory,
    paste0("concept_id=", complex_concept_name)
  )
  unlink(out_partition_dir, recursive = TRUE)
  invisible(NULL)
}

# Checkers to validate function output ----

#' Get counts
#'
#' Retrieves counts of total person_ids and unique person_ids from the database
#'  that have been assigned to the new complex concept name. It also returns
#'  the count of rows in the output partition for the complex concept.
#'
#' @param connection A connection object.
#' @param complex_concept_name Character string of the complex concept name.
#' @param output_directory Path to the output directory where the new partition was
#' written.
#' @param simple_concept_table Optional. A character string of the simple concept
#' @param data_object Optional. A data frame or data table containing the data to count.
#'
#' @returns A dataframe containing total_person_ids, unique_person_ids, and
#'   partition_count.
get_counts <- function(
  connection,
  complex_concept_name,
  output_directory,
  simple_concept_table = NULL,
  data_object = NULL
) {

  # Only one input source should be supplied
  if (
    !is.null(simple_concept_table) &&
    !is.null(data_object)
  ) {
    stop(
      "Provide either 'simple_concept_table' or 'data_object', not both."
    )
  }

  # Count directly from an R object
  if (!is.null(data_object)) {

    if (!"person_id" %in% names(data_object)) {
      stop("'data_object' must contain a 'person_id' column.")
    }

    counts <- data.frame(
      total_person_ids = nrow(data_object),
      unique_person_ids = length(unique(data_object$person_id))
    )

  # Count from a DuckDB table
  } else if (!is.null(simple_concept_table)) {

    count_query <- glue::glue(
      "
      SELECT
        COUNT(*) AS total_person_ids,
        COUNT(DISTINCT person_id) AS unique_person_ids
      FROM {simple_concept_table};
      "
    )

    counts <- DBI::dbGetQuery(
      conn = connection,
      count_query
    )

  # Existing behaviour
  } else {

    count_query <- glue::glue(
      "
      SELECT
        COUNT(*) AS total_person_ids,
        COUNT(DISTINCT person_id) AS unique_person_ids
      FROM {complex_concept_name};
      "
    )

    counts <- DBI::dbGetQuery(
      conn = connection,
      count_query
    )
  }

  # Count rows actually written to output
  row_count <- DBI::dbGetQuery(
    conn = connection,
    glue::glue(
      "
      SELECT COUNT(*) AS n
      FROM read_parquet(
        ['{output_directory}/**/*.parquet'],
        hive_partitioning = true,
        union_by_name = true,
        hive_types = {{'concept_id' : VARCHAR}}
      )
      WHERE concept_id = '{complex_concept_name}';
      "
    )
  )$n

  if (row_count == 0L) {

    logger::log_warn(
      glue::glue(
        "Output partition for '{complex_concept_name}' was written but ",
        "contains 0 rows. Check source data."
      ),
      class = "empty_output_warn"
    )

  } else {

    counts$partition_count <- row_count

    if (row_count != counts$unique_person_ids) {
      logger::log_warn(
        glue::glue(
          "Row count in output partition ({row_count}) does not match ",
          "unique person_id count ({counts$unique_person_ids})."
        ),
        class = "output_row_count_warn"
      )
    }
  }

  counts
}

#' Check output column names
#'
#' @param connection A connection object.
#' @param complex_concept_name Character string of the complex concept name.
#' @param output_directory Path to the output directory where the new partition
#'  was written.
#'
#' @returns NULL
verify_column_names <- function(
  connection,
  complex_concept_name,
  output_directory
) {
  # col_names <- DBI::dbGetQuery(
  #   conn = connection,
  #   glue::glue("DESCRIBE FROM read_parquet('{output_directory}')")
  # )$column_name
  col_names <- names(DBI::dbGetQuery(
    conn = connection,
    glue::glue(
      "SELECT *
        FROM read_parquet(
          ['{output_directory}/**/*.parquet'],
          hive_partitioning = true,
          union_by_name     = true,
          hive_types = {{'concept_id' : VARCHAR}})
          LIMIT 0"
    )
  ))

  expected_cols <- c(
    "ori_table",
    "unique_id",
    "person_id",
    "code",
    "coding_system",
    "value",
    "concept_id",
    "date"
  )

  missing_cols <- setdiff(expected_cols, col_names)
  if (length(missing_cols) > 0L) {
    logger::log_warn(
      glue::glue(
        "The following expected columns are missing from the output view ",
        "'{complex_concept_name}': {paste(missing_cols, collapse = ', ')}."
      ),
      class = "missing_columns_warn"
    )
  }

  extra_cols <- setdiff(col_names, expected_cols)
  if (length(extra_cols) > 0L) {
    logger::log_warn(
      glue::glue(
        "The following unexpected columns are present in the output ",
        "'{complex_concept_name}': {paste(extra_cols, collapse = ', ')}."
      ),
      class = "extra_columns_warn"
    )
  }

  actual_cols <- col_names[col_names %in% expected_cols]
  mismatches <- actual_cols[actual_cols != expected_cols]
  if (length(mismatches) > 0L) {
    logger::log_warn(
      glue::glue(
        "Columns out of order: {paste(mismatches, collapse = ', ')}"
      ),
      class = "column_order_warn"
    )
  }
}

#' Validate output
#'
#' @param connection A connection object.
#' @param complex_concept_name Name of the complex concept created.
#' @param output_directory Path to the output directory where the new partition
#'   was written.
#'
#' @returns List containing counts of total_person_ids, unique_person_ids, and
#'   partition_count, as well as any warnings captured during validation.
validate_output <- function(
  connection,
  complex_concept_name,
  output_directory
) {
  captured_warnings <- character(0)
  withCallingHandlers(
    {
      verify_column_names(
        connection = connection,
        complex_concept_name = complex_concept_name,
        output_directory = output_directory
      )
    },
    warning = function(w) {
      captured_warnings <- c(captured_warnings, conditionMessage(w))
    }
  )

  counts <- get_counts(
    connection = connection,
    complex_concept_name = complex_concept_name,
    output_directory = output_directory
  )

  return(list(counts = counts, warnings = captured_warnings))
}
