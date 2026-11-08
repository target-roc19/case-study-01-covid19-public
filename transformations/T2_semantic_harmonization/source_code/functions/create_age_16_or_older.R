# Flagging the day that a person is over 19

create_age_16_or_older <- function(
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
  
  picard::load_config()
  person_information <- data.table::as.data.table(arrow::read_parquet(config_project$create_complex_concepts$d3_spells))
  results <- create_older_than(person_information, 16)
  results <- data.table::as.data.table(results)
  results[, concept_id := complex_concept_name]

  picard::load_config()
  # Add missing columns
  required_cols_d3_concept <- config_project$create_complex_concepts$required_cols_d3_concept
  missing_cols <- required_cols_d3_concept[!required_cols_d3_concept %in% names(results)]
  lapply(missing_cols , function(x) results[, eval(x) := NA])
  
  arrow::write_dataset(
    results[, ..required_cols_d3_concept],
    path = output_dir,
    format = "parquet",
    partitioning = "concept_id"
  )
}

create_older_than <- function(person_information, age) {
  # Input, table with person_id and birth_date
  # Defining value = true from the age date + 1
  person_information[, date := clock::add_months(birth_date, 12 * age, invalid = "previous")]
  person_information[, value := "TRUE"]

  return(person_information)
}

get_birth_date <- function(person_information) {
  person_information <- unique(person_information[, .(person_id, birth_date)])
  return(person_information)
}
