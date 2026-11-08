# Flagging the day that a person is over 19

create_length_base <- function(
    input_dir,
    output_dir,
    complex_concept_name = NULL,
    sub_concept_ids = NULL,
    start_name = NA,
    end_name = NA
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
  concepts_db_conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  # Read parquet records from hive-style partitions and keep only requested concepts.
  results <- data.table::as.data.table(DBI::dbGetQuery(
    concepts_db_conn,
    glue::glue(
      "SELECT *
        FROM read_parquet(
          '{input_dir}/**/*.parquet',
          hive_partitioning = true,
          union_by_name     = true
          )
        WHERE concept_id IN ({partitions})",
      partitions = paste0("'", existing_sub_concepts, "'", collapse = ", ")
    )
  ))
  
  results[concept_id %in% end_name, value := "FALSE"]
  results[concept_id %in% end_name, date := date + 1]
  results[,concept_id := complex_concept_name]
  
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


create_preg_status <- function( input_dir,
                                output_dir,
                                complex_concept_name = NULL,
                                sub_concept_ids = NULL){
  create_length_base( input_dir,
                      output_dir,
                      complex_concept_name,
                      sub_concept_ids,
                      start_name = "PREGNANCY_START",
                      end_name = "PREGNANCY_END")
  
  
}

create_hosp_length <- function( input_dir,
                                output_dir,
                                complex_concept_name = NULL,
                                sub_concept_ids = NULL){
  create_length_base( input_dir,
                     output_dir,
                     complex_concept_name,
                     sub_concept_ids,
                     start_name = "HOSP_LENGTH_START",
                     end_name = "HOSP_LENGTH_END")
  
  
}