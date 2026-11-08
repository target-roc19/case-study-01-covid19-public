#' Create prior obese complex concept
#'
#' @param input_dir Input directory containing partitioned parquet files.
#' @param output_dir Output directory where the new partition will be written.
#' @param complex_concept_name Name of the new complex concept
#'  to create (used as partition value).
#' @param sub_concept_ids Character vector of sub_concept_ids to include
#'  in the construction of the complex concept (matched against existing
#'  partitions).
#'
#' @returns Invisibly returns a list summarizing the operation,
#'  including matched partitions and output path.
create_prior_obese <- function(
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
          ['{params$input_dir}/**/*.parquet'],
          hive_partitioning = true,
          union_by_name     = true
          )
        WHERE concept_id IN ({partitions})",
      partitions = paste0("'", existing_sub_concepts, "'", collapse = ", ")
    )
  )
  
  BMI_available <- any(existing_sub_concepts %in% "OBESE")
  obes_available <- any(grepl("OBES",existing_sub_concepts))
  if(BMI_available && obes_available){
    tryCatch(
      {
        picard::execute_sql_file(
          sql = picard::load_sql_query(
            paste0(config_t2$sql_dir, "create_prior_obese.sql"), # nolint
            params = list(
              output_dir = params$output_dir,
              complex_concept_name = params$complex_concept_name,
              obesity_concepts = paste(
                glue::glue(
                  "'",
                  {
                    existing_sub_concepts[grep(
                      "OBES",
                      existing_sub_concepts
                    )]
                  },
                  "'"
                ),
                collapse = ", "
              ),
              bmi_cat_concept = paste(
                glue::glue(
                  "'",
                  {
                    existing_sub_concepts[grep(
                      "BMI",
                      existing_sub_concepts
                    )]
                  },
                  "'"
                ),
                collapse = ", "
              )
            )
          ),
          conn = con
        )
      },
      error = function(e) {
        rlang::abort(
          glue::glue(
            "SQL script failed for '{params$complex_concept_name}':\n 
            {conditionMessage(e)}"
          ),
          class = "duckdb_copy_error"
        )
      }
    )
    # Checking rows affected
    validation <- validate_output(
      connection = con,
      complex_concept_name = params$complex_concept_name,
      output_directory = params$output_dir
    )
    
    logger::log_info(crayon::cyan(
      'Counts:\n',
      paste(
        capture.output(print(t(validation$counts), sep = ":"))[-1],
        collapse = '\n'
      )
    ))
  }else{
    logger::log_info("CREATE OBESE, NOT ALL CONCEPTS NEEDED AVAILABLE")
  }
  
}
