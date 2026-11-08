# This function defines record of start and end of enrollement within a database.
# When value = 1 we have a start of enrollmenet and when value 0 we have end.
# The enrollement needs to be at least of 12 months for the matching design, except for children that have the start of enrollment the same day as the birth_date
# Input: D3_SPELLS.fst

create_enrolled_6_months <- function(
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
  results <- calculate_enrollement(concepts_db_conn, 183)
  results <- data.table::as.data.table(results)
  results[, concept_id := complex_concept_name]

  # Add missing columns
  required_cols_d3_concept <- config_project$create_complex_concepts$required_cols_d3_concept
  missing_cols <- required_cols_d3_concept[!required_cols_d3_concept %in% names(results)]
  lapply(missing_cols, function(x) results[, eval(x) := NA])

  arrow::write_dataset(
    results[, ..required_cols_d3_concept],
    path = output_dir,
    format = "parquet",
    partitioning = "concept_id"
  )
}


calculate_enrollement <- function(concepts_db_conn, length_enroll) {
  library(data.table)
  picard::load_config()
  results <- data.table::as.data.table(arrow::read_parquet(config_project$create_complex_concepts$d3_spells))
  results[, start_at_birth := fifelse(birth_date == start_spell, 1, 0)]
  results[, length_spell := difftime(end_spell, start_spell)]
  results <- results[start_at_birth == 1 | length_spell > length_enroll]
  results[start_at_birth == 0, start_spell :=
    clock::add_days(start_spell, length_enroll)]
  results[, end_spell := end_spell + 1]
  results[, start_value := "TRUE"][, end_value := "FALSE"]

  results <- melt(
    results[, c("person_id", "start_spell", "end_spell", "start_value", "end_value")],
    id.vars = "person_id",
    measure.vars = list(c("start_spell", "end_spell"), c("start_value", "end_value")),
    variable.name = "concept_id",
    value.name = c("date", "value"),
    na.rm = TRUE
  )
  return(results)
}
