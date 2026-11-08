#######################################################
# Setting up the environment
#######################################################
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_simple_concepts.R")
  # Start capturing print statements
  lm$start_capturing_prints()
}

picard::load_config()

logger::log_info("[Create simple concepts] - START")

dir_concept <- config_pipeline$databases$dir_concept
dir_d2_db <- config_pipeline$databases$dir_d2_db
root <- config_t2$T2$root
intermediate <- config_t2$T2$intermediate
dir_intermediate_t2 <- file.path(root, intermediate)
deap_name <- config_project$DEAP_configuration$name
dir_d3 <- config_project$output$dir_d3

start_lookback <- config_values$start_lookback
end_study_date <- config_values$end_study_date

#######################################################
# Connect to CDM origin database and concepts database
#######################################################
logger::log_info("To create a new database, we delete the old one if present.")
if (file.exists(dir_concept)) {
  if (exists("concepts_db_conn")) {
    duckdb::dbDisconnect(concepts_db_conn)
    rm(concepts_db_conn)
  }
  if (base::file.remove(dir_concept)) {
    logger::log_info("Old concepts database removed")
  }
}

# New database is created.
concepts_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_concept)

# Files are seved in parquet we prepare an empthy folder where to save them.
dir_parquet_concepts <- file.path(dir_d3, "D3_CONCEPTS_parquet")
# As before we delete and create.
if (file.exists(dir_parquet_concepts)) {
  unlink(dir_parquet_concepts, recursive = TRUE)
}
dir.create(dir_parquet_concepts)


# Creation of concepts_table within the database, this is the output of this
# step, we now create an empthy table within the db we just created.
picard::execute_sql_file(
  sql = picard::load_sql_query(
    file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_concept_table.sql")
  ),
  concepts_db_conn
)

#######################################################
# PREPARING DATASETS
#######################################################

# Attach CDM origin database to pre-matching database
attach_name <- "d2_db_conn"
picard::execute_sql_file(
  sql = picard::load_sql_query(
    file.path(config_t2$T2$root, config_t2$T2$sql_queries, "attach_database.sql"),
    params = list(
      database_path = dir_d2_db,
      attach_name = attach_name
    )
  ),
  conn = concepts_db_conn
)

#
schema_name <- "main"
if (!is.null(schema_name)) {
  tables_prefix <- paste0(attach_name, ".", schema_name, ".")
} else {
  tables_prefix <- paste0(attach_name, ".")
}


# d3 in the db its a column for the distinct on person id
# load and add for each of the sdm maps
d3_spells_available <- TRUE
if (d3_spells_available) {
  logger::log_info("Loading D3_SPELLS dataset")
  d3_spells <- picard::load(
    config_project$create_simple_concepts$d3_spells
  ) |>
    _[, .(person_id)] |>
    unique()
} else {
  logger::log_info(
    "D3_SPELLS dataset not found, using person_ids from PERSONS"
  )
  d3_spells <- picard::load(
    config_project$create_spells$d3_person_file
  ) |>
    _[, .(person_id)] |>
    unique()
}

if ("D3_SPELLS" %in% DBI::dbListTables(concepts_db_conn)) {
  DBI::dbExecute(concepts_db_conn, "DROP TABLE IF EXISTS D3_SPELLS")
}

# save d3_spells into concepts_table
DBI::dbWriteTable(
  conn = concepts_db_conn,
  value = d3_spells,
  name = "D3_SPELLS",
  overwrite = TRUE
)

#######################################################
# Create tables in concepts_table
#######################################################

# Identification of D2-TABLES
tables_query_d2 <- paste0(
  "SELECT table_name FROM information_schema.tables",
  " WHERE table_catalog = '", attach_name, "'"
)
tables_available_d2 <-
  DBI::dbGetQuery(concepts_db_conn, tables_query_d2)$table_name


# If since_when_data_complete is required in future steps,
# save it to config at this point

if ("EVENTS" %in% tables_available_d2) {
  logger::log_info("Preparing EVENTS dataset")
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_events.sql"),
      params = list(
        tables_prefix = tables_prefix,
        end_study_date = end_study_date,
        start_lookback = start_lookback
      )
    ),
    conn = concepts_db_conn
  )
} else {
  logger::log_info("EVENTS table not found in D2 database")
}

if ("VACCINES" %in% tables_available_d2) {
  logger::log_info("Preparing VACCINES dataset")
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_vaccines.sql"),
      params = list(
        tables_prefix = tables_prefix,
        end_study_date = end_study_date,
        start_lookback = start_lookback
      )
    ),
    conn = concepts_db_conn
  )
} else {
  logger::log_info("VACCINES table not found in D2 database")
}

if ("MEDICINES" %in% tables_available_d2) {
  logger::log_info("Preparing MEDICINES dataset")
  # Determine the name of the column that contains the code values and the
  # coding system value based on the DAP type
  # TODO is this something that we can move to a config file?
  if (deap_name == "CPRD") {
    code_col <- "medicinal_product_id"
    coding_system <- "PRODCODEID"
  } else {
    code_col <- "medicinal_product_atc_code"
    coding_system <- "ATC"
  }

  # Create a temporary table with edited medicine data, including a code
  # column and a coding_system column
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_medicines.sql"),
      params = list(
        code_col = code_col,
        coding_system = coding_system,
        tables_prefix = tables_prefix,
        end_study_date = end_study_date,
        start_lookback = start_lookback
        )
      ),
    conn = concepts_db_conn
  )
} else {
  logger::log_info("MEDICINES table not found in D2 database")
}

if ("PROCEDURES" %in% tables_available_d2) {
  logger::log_info("Preparing PROCEDURES dataset")
  # Create a database view named PROCEDURES_EDITED.
  # remove all records with null procedure_date,
  # as they cannot be used for concept identification
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_procedure.sql"),
      params = list(
        tables_prefix = tables_prefix,
        end_study_date = end_study_date,
        start_lookback = start_lookback
      )
    ),
    conn = concepts_db_conn
  )
} else {
  logger::log_info("PROCEDURES table not found in D2 database")
}

logger::log_info("Succeeded")


#######################################################
# IDENTIFICATION OF CONCEPTS
#######################################################
# Import to concepts database
logger::log_info("IMPORTING DAP SPECIFIC CODELISTS")
cols_codelist <- c("code", "concept_id", "coding_system")

if ("EVENTS" %in% tables_available_d2) {
  logger::log_info("Preparing DAP_SPECIFIC_CODELIST_EVENTS")
  codelist_events <- picard::load(config_t2$codelist_events) |>
    _[match_status == "MATCHED"]
  lapply(cols_codelist, function(x) codelist_events[, eval(x) := toupper(get(x))])
  DBI::dbWriteTable(
    concepts_db_conn, codelist_events,
    name = "DAP_SPECIFIC_CODELIST_EVENTS"
  )
} else {
  logger::log_info(
    "Skipping DAP_SPECIFIC_CODELIST_EVENTS
     (EVENTS table not found in D2 database)"
  )
}

if ("MEDICINES" %in% tables_available_d2) {
  logger::log_info("Preparing DAP_SPECIFIC_CODELIST_MEDICINES")
  codelist_medicines <- picard::load(config_t2$codelist_medicines) |>
    _[match_status == "MATCHED"]
  lapply(
    cols_codelist, function(x) codelist_medicines[, eval(x) := toupper(get(x))]
  )
  DBI::dbWriteTable(
    concepts_db_conn, codelist_medicines,
    name = "DAP_SPECIFIC_CODELIST_MEDICINES"
  )
} else {
  logger::log_info(
    "Skipping DAP_SPECIFIC_CODELIST_MEDICINES
     (MEDICINES table not found in D2 database)"
  )
}

if ("PROCEDURES" %in% tables_available_d2) {
  logger::log_info("Preparing DAP_SPECIFIC_CODELIST_PROCEDURES")
  codelist_procedures <- picard::load(
    config_t2$codelist_procedures
  ) |>
    _[match_status == "MATCHED"]
  lapply(
    cols_codelist, function(x) codelist_procedures[, eval(x) := toupper(get(x))]
  )
  DBI::dbWriteTable(
    concepts_db_conn, codelist_procedures,
    name = "DAP_SPECIFIC_CODELIST_PROCEDURES"
  )
} else {
  logger::log_info(
    "Skipping DAP_SPECIFIC_CODELIST_PROCEDURES
     (PROCEDURES table not found in D2 database)"
  )
}

if ("VACCINES" %in% tables_available_d2) {
  logger::log_info("Preparing DAP_SPECIFIC_CODELIST_VACCINES")
  codelist_vaccines <- picard::load(
    config_t2$codelist_vaccines
  ) |>
    _[match_status == "MATCHED"]
  lapply(
    cols_codelist, function(x) codelist_vaccines[, eval(x) := toupper(get(x))]
  )
  DBI::dbWriteTable(
    concepts_db_conn, codelist_vaccines,
    name = "DAP_SPECIFIC_CODELIST_VACCINES"
  )
} else {
  logger::log_info(
    "Skipping DAP_SPECIFIC_CODELIST_VACCINES
     (VACCINES table not found in D2 database)"
  )
}


#######################################################
# Generating concepts
#######################################################

# Identification of DAP-SPECIFIC-CONCEPTS
tables_query <- "
  SELECT table_name
  FROM information_schema.tables"
tables_available <- DBI::dbGetQuery(concepts_db_conn, tables_query)$table_name

dap_specific_concept_map <- data.table::as.data.table(
  picard::load(
    file_path = dir_intermediate_t2,
    file_name = "dap_specific_concept_map.rds"
  )
)
dap_specific_concept_map <- dap_specific_concept_map[
  cdm_table_name %in% tables_available
]

logger::log_info("Generating Additional concepts")
if (nrow(dap_specific_concept_map) > 0) {
  # Attach CDM origin database to pre-matching database
  T2.DMM::create_dap_specific_concept(
    codelist = dap_specific_concept_map,
    name_attachment = tables_prefix,
    save_db = concepts_db_conn,
    dir_save = dir_parquet_concepts,
    add_meaning = FALSE,
    save_in_parquet = TRUE,
    intermediate_type = "VIEW",
  )
} else {
  logger::log_info(
    paste0(
      "0 Additional concepts generated,",
      " dap_specific_concept_map is empty"
    )
  )
}
# update values
DBI::dbExecute(
  concepts_db_conn,
  "
  UPDATE concept_table
  SET value = CASE
    WHEN UPPER(value) = 'POSITIVE' THEN 'TRUE'
    WHEN UPPER(value) = 'NEGATIVE' THEN 'FALSE'
    ELSE UPPER(value)
  END
  "
)

#######################################################
# Creation of parquet files with the identified concepts
#######################################################

if ("DAP_SPECIFIC_CODELIST_PROCEDURES" %in% tables_available) {
  logger::log_info("Generating PROCEDURES concepts")
  # Join the edited procedure data with the DAP-specific codelist to
  # create a table of concepts
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_generating_procedures.sql"),
      params = list(
        dir_parquet_concepts = dir_parquet_concepts
      )
    ),
    conn = concepts_db_conn
  )
}

if ("DAP_SPECIFIC_CODELIST_VACCINES" %in% tables_available) {
  logger::log_info("Generating VACCINES concepts")
  # Join the edited vaccine data with the DAP-specific codelist to
  # create a table of concepts
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_generating_vaccines.sql"),
      params = list(
        dir_parquet_concepts = dir_parquet_concepts
      )
    ),
    conn = concepts_db_conn
  )
}

if ("DAP_SPECIFIC_CODELIST_EVENTS" %in% tables_available) {
  # Get total row
  logger::log_info("Generating EVENTS concepts")
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_generating_events.sql"),
      params = list(
        dir_parquet_concepts = dir_parquet_concepts
      )
    ),
    conn = concepts_db_conn
  )
}

if ("DAP_SPECIFIC_CODELIST_MEDICINES" %in% tables_available) {
  logger::log_info("Generating MEDICINES concepts")
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_generating_medicines.sql"),
      params = list(
        dir_parquet_concepts = dir_parquet_concepts
      )
    ),
    conn = concepts_db_conn
  )
}

DBI::dbExecute(concepts_db_conn, "DROP TABLE IF EXISTS concept_table")

picard::execute_sql_file(
  sql = picard::load_sql_query(
    file.path(config_t2$T2$root, config_t2$T2$sql_queries, "simple_view_concept_table.sql"),
    params = list(
      dir_parquet_concepts = dir_parquet_concepts
    )
  ),
  conn = concepts_db_conn
)

duckdb::dbDisconnect(concepts_db_conn)

rm(
  dap_specific_concept_map, concepts_db_conn, cols_codelist,
  attach_name, tables_prefix, code_col,
  coding_system
)

logger::log_info(c("[Create Concepts Database] - SUCCEED"))

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
