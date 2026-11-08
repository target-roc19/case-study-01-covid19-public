# Step specific for CPRD
# This step replaces set_db.R

logger::log_info("Loading the database and performing operations")
## Config loading ----
picard::load_config()
# inputs
filepath_cdm_metadata <- config_project$set_db$cdm_metadata
cdm_metadata <- picard::load(filepath_cdm_metadata)

# deap configs
dir_d2 <- config_project$DEAP_configuration$data_instance
database_path <- config_project$DEAP_configuration$database_path
cdm_tables_names <- config_t2$cdm_table_names

# output database
dir_d2_db <- config_project$set_db$output
d2_parquet_folder <- file.path(
  "transformations", "T2_semantic_harmonization",
  "intermediate_data_file", "d2_parquets"
)
file_format <- "csv"


by_file <- FALSE
if (file.exists(dir_d2_db)) {
  file.remove(dir_d2_db)
}
if (file.exists(paste0(dir_d2_db, ".wal"))) {
  file.remove(paste0(dir_d2_db, ".wal"))
}

if (file.exists(d2_parquet_folder)) {
  unlink(d2_parquet_folder, recursive = TRUE)
}
dir.create(d2_parquet_folder)

db_connection <- duckdb::dbConnect(duckdb::duckdb(), dir_d2_db)

# Validate file_format parameter
if (!file_format %in% c("csv", "parquet", "auto")) {
  stop("file_format must be one of: 'csv', 'parquet', or 'auto'")
}

# Loop through each table in cdm_tables_names
for (table in cdm_tables_names) {
  # What table are we going to read
  message(paste0("[load_db]: Reading for table: ", table))
  
  # Function to find matching files based on format
  find_matching_files <- function(format) {
    if (format == "csv") {
      pattern <- paste0(table, ".*\\.csv$")
    } else if (format == "parquet") {
      pattern <- paste0(table, ".*\\.parquet$")
    }
    
    list.files(
      path = dir_d2,
      pattern = pattern,
      full.names = TRUE
    )
  }
  
  # Determine which files to use based on file_format setting
  matching_files <- c()
  selected_format <- NULL
  
  if (file_format == "auto") {
    # Try parquet first, then csv
    parquet_files <- find_matching_files("parquet")
    csv_files <- find_matching_files("csv")
    
    if (length(parquet_files) > 0) {
      matching_files <- parquet_files
      selected_format <- "parquet"
    } else if (length(csv_files) > 0) {
      matching_files <- csv_files
      selected_format <- "csv"
    }
  } else {
    # Use specified format
    matching_files <- find_matching_files(file_format)
    selected_format <- file_format
  }
  
  # Skip this loop if there are no files to read
  if (length(matching_files) == 0) {
    message(paste0(
      "[load_db]: No files found for ", table, ", in: ", dir_d2
    ))
    next
  }
  
  message(paste0(
    "[load_db]: Using ", selected_format, " format for table: ", table
  ))
  
  table_index <- 0
  
  if (table %in% "PERSONS") {
    matching_files <- "IGNORING_LOOP"
  }
  
  for (file_to_load in matching_files) {
    table_index <- table_index + 1
    
    temp_name <- paste0("temp_", table)
    # For CSV files (original logic)
    
    if (table %in% "PERSONS") {
      # For CSV files (original logic)
      query <- paste0(
        "CREATE OR REPLACE TEMP TABLE ", temp_name, ' AS
                  SELECT * FROM read_csv_auto("',
        file.path(dir_d2, table), '*.csv",
                  union_by_name = true,
                  ALL_VARCHAR = true,
                  nullstr = "NA" );'
      )
    } else {
      query <- paste0(
        "CREATE OR REPLACE TEMP TABLE ", temp_name, ' AS
                    SELECT * FROM read_csv("',
        file_to_load, '",
                    ALL_VARCHAR = true,
                    nullstr = "NA" );'
      )
    }
    
    # Execute the query
    DBI::dbExecute(db_connection, query)
    
    # Yeah, done
    message(paste0(
      "[load_db]: Finished reading ", table, " (", selected_format, ")"
    ))
    
    # Checking if any mandatory columns are missing within the database.
    cols_in_table <- DBI::dbListFields(db_connection, temp_name)
    standard_cdm_table_columns <- unique(
      cdm_metadata[TABLE %in% table, Variable]
    )
    # Identify mandatory columns according to CDM specification
    mandatory_colums <- unique(
      cdm_metadata[
        TABLE %in% table & stringr::str_detect(Mandatory, "Yes") == TRUE,
        Variable
      ]
    )
    # Find mandatory columns that are missing from the loaded data
    mandatory_missing_in_db <- unique(
      mandatory_colums[!mandatory_colums %in% cols_in_table]
    )
    # Extract date and character column specifications from metadata
    date_cols <- cdm_metadata[
      TABLE %in% table & stringr::str_detect(Format, "yyyymmdd") == TRUE,
      Variable
    ]
    character_cols <- cdm_metadata[
      TABLE %in% table & stringr::str_detect(Format, "Character") == TRUE,
      Variable
    ]
    
    # ===== ADD MISSING MANDATORY COLUMNS =====
    # Create any mandatory columns that are missing from the data files
    if (length(mandatory_missing_in_db) > 0) {
      message(paste0(
        "[load_db]: The following mandatory columns are missing in table: ",
        table
      ))
      message(paste(mandatory_missing_in_db, collapse = ", "))
      
      invisible(lapply(mandatory_missing_in_db, function(new_column) {
        DBI::dbExecute(db_connection, paste0(
          "ALTER TABLE ", temp_name, " ADD COLUMN ", new_column, " VARCHAR;"
        ))
      }))
    }
    # ===== REMOVE NON-STANDARD COLUMNS =====
    # Identify columns in the data that are not part of the CDM specification
    additional_columns <- cols_in_table[
      !cols_in_table %in% standard_cdm_table_columns
    ]
    if (length(additional_columns) > 0) {
      message(paste0(
        paste0(
          "[load_db]: The following columns are not part",
          " of the CDM table but are in the files : "
        ),
        paste(additional_columns, collapse = ",")
      ))
      
      invisible(lapply(additional_columns, function(new_column) {
        message(paste0(
          "    Dropping column : ", new_column
        ))
        tryCatch(
          {
            DBI::dbExecute(db_connection, paste0(
              "ALTER TABLE ", temp_name, " DROP COLUMN ", new_column, " ;"
            ))
          },
          error = function(e) {
            message(paste0(
              "    ACTION FAILED "
            ))
          }
        )
      }))
    }
    
    # ===== DATE COLUMN TYPE CONVERSION =====
    # Find date columns that actually exist in the loaded table
    available_date_cols <- cols_in_table[cols_in_table %in% date_cols]
    
    if (length(available_date_cols) > 0) {
      message(paste0(
        "[load_db]: The following columns are identified with date format: ",
        paste(available_date_cols, collapse = ",")
      ))
      invisible(lapply(available_date_cols, function(new_column) {
        message(paste0("Converting: ", new_column))
        tryCatch(
          {
            # For parquet files, dates might already be in proper format
            # Try direct conversion first
            DBI::dbExecute(db_connection, paste0(
              "ALTER TABLE ", temp_name, " ALTER ", new_column, " TYPE DATE;"
            ))
          },
          error = function(e) {
            # If direct conversion fails
            message(
              "      Direct conversion failed. Attempting to fix with STRPTIME."
            )
            
            # Handle different date formats that might come from parquet vs csv
            if (selected_format == "parquet") {
              # Parquet might have dates in different formats
              # Try common parquet date formats first
              tryCatch(
                {
                  DBI::dbExecute(db_connection, paste0(
                    "UPDATE ", temp_name,
                    " SET ", new_column,
                    " = CAST(TRY_STRPTIME(CAST(", new_column,
                    " AS VARCHAR), '%Y%m%d') AS DATE) WHERE ",
                    new_column, " IS NOT NULL;"
                  ))
                },
                error = function(e2) {
                  # Fall back to string-based conversion
                  DBI::dbExecute(db_connection, paste0(
                    "UPDATE ", temp_name,
                    " SET ", new_column, " = NULL WHERE ",
                    new_column,
                    " NOT SIMILAR TO '^[0-9]{8}$|^[0-9]{4}-[0-9]{2}-[0-9]{2}$';"
                  ))
                  
                  # Handle both YYYYMMDD and YYYY-MM-DD formats
                  DBI::dbExecute(db_connection, paste0(
                    "UPDATE ", temp_name,
                    " SET ", new_column, " = CASE
                  WHEN ", new_column,
                    " SIMILAR TO '^[0-9]{8}$' THEN STRPTIME(",
                    new_column, ", '%Y%m%d')
                  WHEN ", new_column,
                    " SIMILAR TO '^[0-9]{4}-[0-9]{2}-[0-9]{2}$' THEN STRPTIME(",
                    new_column, ", '%Y-%m-%d')
                  ELSE NULL
                END WHERE ", new_column, " IS NOT NULL;"
                  ))
                }
              )
            } else {
              # CSV format handling (original logic)
              # Nullify invalid values first
              DBI::dbExecute(db_connection, paste0(
                "UPDATE ", temp_name,
                " SET ", new_column, " = NULL WHERE ",
                new_column, " NOT SIMILAR TO '^[0-9]{8}$';"
              ))
              
              # Apply STRPTIME to reformat valid date strings
              DBI::dbExecute(db_connection, paste0(
                "UPDATE ", temp_name,
                " SET ", new_column, " = STRPTIME(", new_column,
                ", '%Y%m%d') WHERE ", new_column, " IS NOT NULL AND ",
                new_column, " <> '';"
              ))
            }
            
            # Retry altering the column type to DATE
            DBI::dbExecute(db_connection, paste0(
              "ALTER TABLE ", temp_name, " ALTER ", new_column, " TYPE DATE;"
            ))
            
            message(
              "      Column successfully converted to DATE after fixing format."
            )
          }
        )
      }))
    }
    
    if (table %in% "PERSONS") {
      # This only applied for PERSONS table
      query <- sprintf(
        "SELECT '%s' AS ori_table,
        rn AS unique_id,
        * EXCLUDE(rn)
        FROM (SELECT *, uuid() AS rn
              FROM (SELECT DISTINCT *
              FROM %s
              WHERE person_id IS NOT NULL
              AND person_id <> '' AND person_id <> 'NA'))",
        table, temp_name
      )
    } else if (table %in% "PRODUCTS") {
      query <- sprintf(
        "SELECT
        FROM %s",
        temp_name
      )
    } else {
      query <- sprintf(
        "SELECT '%s' AS ori_table,
        rn AS unique_id,
        * EXCLUDE(rn)
        FROM (SELECT *, uuid() AS rn
        FROM %s) WHERE person_id IS NOT NULL AND
        person_id <> '' AND person_id <> 'NA'",
        table, temp_name
      )
    }
    
    DBI::dbExecute(
      db_connection,
      glue::glue(
        "COPY ({query})
                    TO '{d2_parquet_folder}/{table}_{table_index}.parquet'"
      )
    )
    
    DBI::dbExecute(
      db_connection,
      glue::glue("DROP TABLE {temp_name}")
    )
  }
}

DBI::dbDisconnect(db_connection)
rm(db_connection)
gc()

db_connection <- duckdb::dbConnect(duckdb::duckdb(), dir_d2_db)
for (table in cdm_tables_names) {
  DBI::dbExecute(
    db_connection,
    glue::glue(
      "CREATE VIEW main.{table} AS FROM
      read_parquet('{d2_parquet_folder}/{table}_*.parquet')"
    )
  )
}

DBI::dbDisconnect(db_connection)

rm(
  db_connection, query, temp_name,
  available_date_cols, additional_columns,
  file_format, dir_d2
)
gc()
logger::log_success("Finished set_db step")
