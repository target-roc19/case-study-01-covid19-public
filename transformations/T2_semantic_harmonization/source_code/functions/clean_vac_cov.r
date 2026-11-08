#' Create COVID-19 vaccine records by consolidating and processing vaccine concepts.
#'
#' This function processes COVID-19 vaccine data from both categorized and ATC
#' concept sources, enriches them with manufacturer information, applies
#' standardization, and generates the final D3_COVIDVACCINES output.
#'
#' Mirrors the structure of `create_or()` by accepting input/output directories
#' and retrieving all configuration from picard's loaded config objects.
#'
#' @param input_dir Path to the concept database directory (input_dir).
#'   Mapped from `config_pipeline$databases$dir_concept`.
#' @param output_dir Path to the D3 output directory (output_dir).
#'   Mapped from `config_project$output$dir_d3`.
#' @param complex_concept_name Name of the complex concept to create (default: "VP_COVID").
#'   Not used in current implementation but kept for structural consistency.
#' @param sub_concept_ids Concept IDs to include (default: NULL).
#'   Not used in current implementation but kept for structural consistency.
#'
#' @details
#' Configuration directories are loaded via `picard::load_config()`:
#' - `dir_concept` from `config_pipeline$databases$dir_concept`
#' - `config_pipeline$databases$dir_d2_db` from `config_pipeline$databases$config_pipeline$databases$dir_d2_db`
#' - `dir_intermediate_t2` from `config_t2$T2` root + intermediate
#' - `dir_bridge` from `config_pipeline$common$dir_bridge`
#' - `this_dap` from `config_project$DEAP_configuration$name`
#'
#' @returns Invisibly returns a list containing:
#'   \item{records_created}{Number of records created}
#'   \item{excluded_records}{Summary of records excluded by birth_date}
#'   \item{output_file}{Path to D3_COVIDVACCINES.fst file}
#'
#' @keywords internal
clean_vac_cov <- function(
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
      if (exists("d2_db_conn", inherits = FALSE)) {
        duckdb::dbDisconnect(d2_db_conn)
      }
    },
    add = TRUE
  )

  logger::log_info("Creating COVID-19 vaccine records - START")
  picard::load_config()
  # Establishing database connections --------------------------------------------------
  concepts_db_conn <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  d2_db_conn <- duckdb::dbConnect(duckdb::duckdb(), config_pipeline$databases$dir_d2_db)

  # Loading D3_SPELLS for excluding records prior birth_date
  d3_spells_file <- config_project$create_complex_concepts$d3_spells
  D3_SPELLS <- picard::load(d3_spells_file)
  D3_SPELLS <- unique(D3_SPELLS[, .(person_id, birth_date)])

  if (length(existing_sub_concepts) > 0) {
    # Collecting records from the categorized covid vaccines --------------------------------------------------
    vaccine_records <- fetch_vaccine_concepts(
      connection = concepts_db_conn,
      input_dir = input_dir,
      existing_sub_concepts = existing_sub_concepts,
      spells_data = D3_SPELLS,
      end_exposure_assessment_window = config_values$end_exposure_assessment_window
    )
    logger::log_info(
      "COVID-19 vaccine records fetched: {nrow(vaccine_records$data)}"
    )

    if (nrow(vaccine_records$excluded) > 0) {
      saveRDS(
        vaccine_records$excluded,
        file.path(params$dir_intermediate_t2, "excluded_records_vaccine_1.rds")
      )
    }

    vaccine_records <- vaccine_records$data
    no_records <- nrow(vaccine_records) == 0
    if (no_records) {
      logger::log_warn(
        "No records found for categorized vaccine concepts: {paste0(cat_concept_names, collapse = ', ')}"
      )
    }

    vaccine_records[, vx_manufacturer := concept_id]

    # logging dose 1, dose 2, dose 3 counts
    for (i_dose in sort(unique(vaccine_records$value))){
      logger::log_info(
        "COVID-19 vaccine records fetched from vx_dose for dose {i_dose}: {nrow(vaccine_records[value == i_dose])}"
      )
    }

    # Clean vaccine records using hierarchy
    vaccine_record_period <- c(14, 40)
    PP_VP_COVID <- clean_vaccine_records(
      dataset = vaccine_records,
      person_id = "person_id",
      rec_period = vaccine_record_period,
      c_date = "date",
      c_hierarchy = NULL,
      keep.searching = TRUE
    )

    # logging dose 1, dose 2, dose 3 counts after cleaning
    PP_VP_COVID[, dose := 1:.N, by = person_id]
    n_dose_log <- min(5, max(PP_VP_COVID$dose))
    for (i_dose in seq(n_dose_log)) {
      logger::log_info(
        "COVID-19 vaccine records after cleaning for dose {i_dose}: {nrow(PP_VP_COVID[dose == i_dose])}"
      )
    }
    if (max(PP_VP_COVID$dose) > 5) {
      logger::log_warn(
        "Some individuals have more than 5 doses recorded. Only the first 5 doses are logged."
      )
    }

    # Standardizing vaccine types --------------------------------------------------
    PP_VP_COVID[, value := concept_id]
    PP_VP_COVID[, concept_id := complex_concept_name]

    records_created <- nrow(PP_VP_COVID)
    logger::log_info("COVID-19 vaccine records created: {records_created}")

    # Add missing columns
    required_cols_d3_concept <- config_project$create_complex_concepts$required_cols_d3_concept
    missing_cols <- required_cols_d3_concept[!required_cols_d3_concept %in% names(PP_VP_COVID)]
    lapply(missing_cols, function(x) PP_VP_COVID[, eval(x) := NA])

    # Return summary invisibly
    arrow::write_dataset(
      PP_VP_COVID[, ..required_cols_d3_concept],
      path = output_dir,
      format = "parquet",
      partitioning = "concept_id"
    )
    logger::log_info("Creating COVID-19 vaccine records - SUCCEED")
  } else {
    logger::log_warn("COVID VACCINES NOT AVAILABLE - CHECK DATA")
  }
}


#' Fetch vaccine concepts from the concept database
#'
#' @keywords internal
fetch_vaccine_concepts <- function(connection, input_dir, existing_sub_concepts, spells_data, end_exposure_assessment_window) {
  query <- glue::glue(
    "SELECT *
          FROM read_parquet(
          '{input_dir}/**/*.parquet',
          hive_partitioning = true,
          union_by_name     = true
          )
        WHERE concept_id IN ({partitions})
        AND date IS NOT NULL
        AND date <= '{end_exposure_assessment_window}'",
    partitions = paste0("'", existing_sub_concepts, "'", collapse = ", ")
  )


  data <- data.table::as.data.table(DBI::dbGetQuery(connection, query))
  # Merge with spells data for birth_date filtering
  data <- data.table::merge.data.table(data, spells_data, by = "person_id", all.x = TRUE)
  excluded <- data[!is.na(birth_date) & date < birth_date]
  data <- data[!is.na(birth_date) & date >= birth_date | is.na(birth_date)][, birth_date := NULL]

  list(data = data, excluded = excluded)
}
