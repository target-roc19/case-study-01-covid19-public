prior_vaccine <- function(
  data,
  person_col = "person_id",
  date_col = "date",
  value_col = "value",
  filter_value = "VP_TARGET_COVID"
) {
  dt <- data.table::as.data.table(data)
  # Compare each record against the immediately previous vaccine record per person.
  dt <- dt[get(value_col) == filter_value]
  data.table::setorderv(dt, cols = c(person_col, date_col), na.last = TRUE)

  dt[, index_record := seq_len(.N), by = person_col]
  dt <- dt[index_record == 1]
  dt[, (date_col) := get(date_col) + 1][, index_record := NULL]
  dt[, value := TRUE]
  dt
}


# ============================================================================
# Factory function: create_complex_concept
# ============================================================================
# Centralizes boilerplate logic for all complex concept creation functions.
# Parameterizes data transformation via transform_fn callback.
# ============================================================================

create_complex_concept <- function(
  input_dir,
  output_dir,
  complex_concept_name = NULL,
  sub_concept_ids = NULL,
  transform_fn
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

  # Initialize cleanup on exit
  on.exit(
    {
      if (exists("concepts_db_conn", inherits = FALSE)) {
        duckdb::dbDisconnect(concepts_db_conn)
      }
      if (exists("d2_db_conn", inherits = FALSE)) {
        duckdb::dbDisconnect(d2_db_conn)
      }
    },
    add = TRUE
  )

  # Open DuckDB connection
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Read parquet records from hive-style partitions
  data <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT *
        FROM read_parquet(
          '{params$input_dir}/**/*.parquet',
          hive_partitioning = true,
          union_by_name     = true
        )
        WHERE concept_id IN ({partitions})",
      partitions = paste0("'", existing_sub_concepts, "'", collapse = ", ")
    )
  ))

  # Apply the transformation function (the varying part)
  data <- transform_fn(data, params$complex_concept_name)

  # Add missing columns
  picard::load_config()
  required_cols_d3_concept <- config_project$create_complex_concepts$required_cols_d3_concept
  missing_cols <- required_cols_d3_concept[!required_cols_d3_concept %in% names(data)]
  lapply(missing_cols, function(x) data[, eval(x) := NA])

  # Write dataset
  arrow::write_dataset(
    data[, ..required_cols_d3_concept],
    path = params$output_dir,
    format = "parquet",
    partitioning = "concept_id"
  )
}

# ============================================================================
# Transformation functions: define varying logic for each concept
# ============================================================================

transform_prev_other_covid <- function(data, complex_concept_name) {
  data <- prior_vaccine(data, filter_value = "VP_NONTARGET_COVID")
  data[, concept_id := complex_concept_name]
  data
}

transform_prior_pfizer_vac <- function(data, complex_concept_name) {
  data <- prior_vaccine(data, filter_value = "VP_TARGET_COVID")
  data[, concept_id := complex_concept_name]
  data
}

transform_exposure <- function(data, complex_concept_name) {
  data <- data[value %in% "VP_TARGET_COVID"]
  data[, value := TRUE]
  data[, concept_id := complex_concept_name]
  data
}

# Selecting only second exposed dose
transform_ie1 <- function(data, complex_concept_name) {
  # Calculate the second dose of VP_TARGET_COVID
  data <- data[value %in% "VP_TARGET_COVID"]
  data.table::setorderv(data, cols = c("person_id", "date"))
  data[, index := seq_len(.N), by = person_id]
  data <- data[index == 2][, index := NULL]
  data[, value := TRUE]
  data[, date := as.Date(date)]
  data[, concept_id := complex_concept_name]
  data
}

# Selecting 3 or higher doses
transform_ie2 <- function(data, complex_concept_name) {
  # Select all doses from the third dose onwards for VP_TARGET_COVID
  data <- data[value %in% "VP_TARGET_COVID"]
  data.table::setorderv(data, cols = c("person_id", "date"))
  data[, index := seq_len(.N), by = person_id]
  data <- data[index > 2][, index := NULL]
  data[, value := TRUE]
  data[, date := as.Date(date)]
  data[, concept_id := complex_concept_name]
  data
}

# Select nontarget first dose of exposed
transform_ie3_exp <- function(data, complex_concept_name) {
  exposed_id <- data[value %in% "VP_TARGET_COVID", person_id]
  data <- data[person_id %in% exposed_id & value %in% "VP_NONTARGET_COVID"]

  data.table::setorderv(data, cols = c("person_id", "date"))
  data[, index := seq_len(.N), by = person_id]
  data <- data[index == 1][, index := NULL]

  data[, value := TRUE]
  data[, date := as.Date(date)]
  data[, concept_id := complex_concept_name]
  data
}

# Select the first dose of any vaccine for each person
transform_ie3_cont <- function(data, complex_concept_name) {
  data.table::setorderv(data, cols = c("person_id", "date"))
  data[, index := seq_len(.N), by = person_id]
  data <- data[index == 1][, index := NULL]
  data[, value := TRUE]
  data[, date := as.Date(date)]
  data[, concept_id := complex_concept_name]
  data
}

# ============================================================================
# Public functions: thin wrappers around the factory
# ============================================================================

create_prev_other_covid <- function(input_dir,
                                    output_dir,
                                    complex_concept_name = NULL,
                                    sub_concept_ids = NULL) {
  create_complex_concept(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids,
    transform_fn = transform_prev_other_covid
  )
}

create_prior_pfizer_vac <- function(input_dir,
                                    output_dir,
                                    complex_concept_name = NULL,
                                    sub_concept_ids = NULL) {
  create_complex_concept(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids,
    transform_fn = transform_prior_pfizer_vac
  )
}

create_exposure <- function(input_dir,
                            output_dir,
                            complex_concept_name = NULL,
                            sub_concept_ids = NULL) {
  create_complex_concept(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids,
    transform_fn = transform_exposure
  )
}

create_ie1 <- function(input_dir,
                       output_dir,
                       complex_concept_name = NULL,
                       sub_concept_ids = NULL) {
  create_complex_concept(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids,
    transform_fn = transform_ie1
  )
}

create_ie2 <- function(input_dir,
                       output_dir,
                       complex_concept_name = NULL,
                       sub_concept_ids = NULL) {
  create_complex_concept(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids,
    transform_fn = transform_ie2
  )
}

create_ie3_exp <- function(input_dir,
                           output_dir,
                           complex_concept_name = NULL,
                           sub_concept_ids = NULL) {
  create_complex_concept(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids,
    transform_fn = transform_ie3_exp
  )
}

create_ie3_cont <- function(input_dir,
                            output_dir,
                            complex_concept_name = NULL,
                            sub_concept_ids = NULL) {
  create_complex_concept(
    input_dir = input_dir,
    output_dir = output_dir,
    complex_concept_name = complex_concept_name,
    sub_concept_ids = sub_concept_ids,
    transform_fn = transform_ie3_cont
  )
}
