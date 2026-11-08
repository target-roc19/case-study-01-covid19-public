#######################################################
# Setting up the environment
#######################################################
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("process_codelist.R")
  lm$start_capturing_prints()
}

picard::audit_start()
picard::load_config()
logger::log_info("Doing the dap specific code list.")

# saving directory
root <- config_t2$T2$root
intermediate <- config_t2$T2$intermediate
dir_intermediate_t2 <- file.path(root, intermediate)
dir_d2_db <- config_pipeline$databases$dir_d2_db
cdm_tables_names <- config_t2$cdm_table_names

###################################################
# Set code list
###################################################
deap_name <- config_project$DEAP_configuration$name

# Load the csv files

dap_coding_systems <- picard::load(
  file_path = config_pipeline$common$dir_bridge,
  file_name = "coding_systems.csv"
)
dap_specific_concept_map <- picard::load(
  file_path = config_pipeline$common$dir_bridge,
  file_name = "dap_specific_concept_map.csv"
)

# import events code list from meta data file
event_codelist_file <- config_project$process_codelist$events_codelist_latest

d2_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_d2_db)
schema_name <- "main."
list_of_tables <- unique(DBI::dbListTables(d2_db_conn))

######################################
# CREATE DAP SPECIFIC EVENTS CODELIST
######################################
if ("EVENTS" %in% cdm_tables_names && "EVENTS" %in% list_of_tables) {
  logger::log_info("Events Codelist Name")

  study_codelist <- picard::load(event_codelist_file)[
    ,
    .(coding_system, code, event_abbreviation, type, tags, system)
  ][
    ,
    concept_id := paste0(system, "_", event_abbreviation, "_", type)
  ][
    ,
    `:=`(
      concept_id = toupper(concept_id),
      coding_system = toupper(coding_system),
      tags = tolower(tags)
    )
  ][
    coding_system %in% unique(dap_coding_systems$coding_system)
  ]

  study_codelist <- unique(study_codelist)[
    ,
    priority := ifelse(tags == "narrow", 1, 2)
  ][
    ,
    c("concept_id", "code", "coding_system", "tags", "priority") # exclude event_abbreviation, type, system
  ]

  column_info_list <- list(
    list(
      source_column = c("event_code", "event_record_vocabulary"),
      alias_name = c("code", "coding_system")
    ),
    list(
      source_column = c("event_free_text"),
      alias_name = c("code")
    )
  )

  result_list <- T2.DMM::get_unique_codelist(
    d2_db_conn,
    column_info_list,
    tb_name = paste0(schema_name, "EVENTS")
  )

  result_list[[1]] <- result_list[[1]][
    ,
    variable := ifelse(.N > 1, "event_code", NA)
  ]
  result_list[[2]] <- result_list[[2]][
    ,
    `:=`(
      coding_system = ifelse(.N > 1, "FREE_TEXT", NA),
      variable = ifelse(.N > 1, "FREE_TEXT", NA)
    )
  ]
  unique_codelist <- data.table::rbindlist(result_list, use.names = TRUE)
  rm(result_list)

  unique_codelist <- unique_codelist[
    !is.na(code) & !code %in% "" & !is.na(coding_system)
  ]

  codesheet_new <- unique(
    T2.DMM::create_dap_specific_codelist(
      dap_codes = unique_codelist,
      codelist = data.table::copy(study_codelist),
    )
  )

  codesheet_new <- codesheet_new[tags == "narrow"]
  picard::save(
    data = codesheet_new,
    file_path =
      config_project$process_codelist$output_codelist_events
  )
  print("Events")
  print(table(codesheet_new$match_status, codesheet_new$coding_system))
  print(table(codesheet_new$match_status, codesheet_new$concept_id))

  rm(codesheet_new, study_codelist, column_info_list, unique_codelist)
}

######################################
# CREATE DAP SPECIFIC MEDICINES CODELIST
######################################
if ("MEDICINES" %in% cdm_tables_names && "MEDICINES" %in% list_of_tables) {
  logger::log_info(paste0("Medicines Codelist"))

  study_codelist <- picard::load(
    config_project$process_codelist$medicines_codelist_latest
  )[
    ,
    .(drug_abbreviation, code, tags, coding_system)
  ][, `:=`(
    concept_id = toupper(drug_abbreviation)
  )][
    , tags := tolower(tags)
  ][
    , priority := ifelse(tags == "narrow", 1, 2)
  ][
    , drug_abbreviation := NULL
  ]

  if (deap_name == "CPRD") {
    study_codelist <- study_codelist[coding_system == "PRODCODEID"]
  } else {
    study_codelist <- study_codelist[coding_system == "ATC"]
  }

  if (
    "medicinal_product_atc_code" %in% DBI::dbGetQuery(
      d2_db_conn,
      paste0("PRAGMA table_info('", schema_name, "MEDICINES", "')")
    )$name
  ) {
    column_info_list <- list(
      list(
        source_column = c("medicinal_product_atc_code"),
        alias_name = c("code")
      )
    )

    result_list_atc <- T2.DMM::get_unique_codelist(
      d2_db_conn,
      column_info_list,
      tb_name = paste0(schema_name, "MEDICINES")
    )[[1]]
  } else {
    result_list_atc <- data.table::data.table(code = NA, COUNT = NA)
  }

  if (
    "medicinal_product_id" %in% DBI::dbGetQuery(
      d2_db_conn,
      paste0("PRAGMA table_info('", schema_name, "MEDICINES", "')")
    )$name
  ) {
    column_info_list <- list(
      list(
        source_column = c("medicinal_product_id"),
        alias_name = c("code")
      )
    )
    result_list_product <-
      T2.DMM::get_unique_codelist(
        d2_db_conn,
        column_info_list,
        tb_name = paste0(schema_name, "MEDICINES")
      )[[1]]
  } else {
    result_list_product <- data.table(code = NA, COUNT = NA)
  }

  if (nrow(result_list_atc) > 1) {
    result_list_atc <- result_list_atc[, `:=`(
      variable = "medicinal_product_atc_code",
      coding_system = "ATC"
    )]

    if (deap_name == "CPRD") {
      result_list_atc <- result_list_atc[, coding_system := "PRODCODEID"]
    }
  } else {
    result_list_atc <- result_list_atc[, `:=`(
      variable = NA,
      coding_system = NA
    )]
  }

  if (nrow(result_list_product) > 1) {
    result_list_product <- result_list_product[, `:=`(
      variable = "medicinal_product_id",
      coding_system = "PRODCODEID"
    )]
  } else {
    result_list_product <- result_list_product[, `:=`(
      variable = NA,
      coding_system = NA
    )]
  }

  unique_codelist <- unique(
    data.table::rbindlist(
      list(result_list_atc, result_list_product),
      use.names = TRUE,
      fill = TRUE
    )
  )
  unique_codelist <- unique_codelist[
    !is.na(code) & !code %in% "" & !is.na(coding_system)
  ]

  codesheet_new <- unique(
    T2.DMM::create_dap_specific_codelist(
      unique_codelist,
      data.table::copy(study_codelist)
    )
  )

  codesheet_new <- codesheet_new[tags == "narrow"]
  picard::save(
    data = codesheet_new,
    file_path = config_project$process_codelist$output_codelist_medicines,
    create_plot = TRUE,
    exclude_columns_from_plots = NULL,
  )
  print("Medicines")
  print(table(codesheet_new$match_status, codesheet_new$coding_system))
  print(table(codesheet_new$match_status, codesheet_new$concept_id))
  rm(
    result_list_atc, codesheet_new,
    study_codelist, column_info_list, unique_codelist
  )
  gc()
}

######################################
# CREATE DAP SPECIFIC VACCINES CODELIST
######################################
######################################
if ("VACCINES" %in% cdm_tables_names) {
  logger::log_info("Vaccines Codelist ")

  study_codelist <- picard::load(
    config_project$process_codelist$vaccines_codelist_latest
  )[
    , .(drug_abbreviation, code, tags, coding_system)
  ][
    , `:=`(
      concept_id = toupper(drug_abbreviation)
    )
  ][
    , tags := tolower(tags)
  ][
    , priority := ifelse(tags == "narrow", 1, 2)
  ]
  # ][
  #   , unique(.)
  # ]
  # the select and group by clause
  column_info_list <- list(
    list(
      source_column = c("vx_atc"),
      alias_name = c("code")
    ),
    list(
      source_column = c("vx_type"),
      alias_name = "code"
    )
  )

  # Call the function with the database connection and column information list
  result_list <- T2.DMM::get_unique_codelist(
    d2_db_conn, column_info_list,
    tb_name = paste0(schema_name, "VACCINES")
  )

  # combine event_code and free_text into one table
  result_list[[1]] <- result_list[[1]][, `:=`(
    coding_system = ifelse(.N > 0, "vx_atc", NA),
    variable = ifelse(.N > 0, "vx_atc", NA)
  )]
  result_list[[2]] <- result_list[[2]][, `:=`(
    coding_system = ifelse(.N > 0, "vx_type", NA),
    variable = ifelse(.N > 0, "vx_type", NA)
  )]

  unique_codelist <- data.table::rbindlist(result_list, use.names = TRUE)
  rm(result_list)

  unique_codelist <- unique_codelist[
    !is.na(code) & !code %in% "" & !is.na(coding_system)
  ][, coding_system := toupper(coding_system)]

  codesheet_new <- unique(
    T2.DMM::create_dap_specific_codelist(
      unique_codelist,
      data.table::copy(study_codelist),
      start_with_codingsystems = c("ATC", "VX_ATC", "VX_TYPE")
    )
  )

  codesheet_new <- codesheet_new[tags == "narrow"]
  picard::save(
    data = codesheet_new,
    file_path = config_project$process_codelist$output_codelist_vaccines,
    create_plot = TRUE,
    exclude_columns_from_plots = NULL,
  )
  print("Vaccines")
  print(table(codesheet_new$match_status, codesheet_new$coding_system))
  print(table(codesheet_new$match_status, codesheet_new$concept_id))

  rm(codesheet_new, study_codelist, column_info_list, unique_codelist)
  gc()
}


######################################
# CREATE DAP SPECIFIC PROCEDURES CODELIST
######################################
if ("PROCEDURES" %in% cdm_tables_names && "PROCEDURES" %in% list_of_tables) {
  logger::log_info("Procedures Codelist ")

  study_codelist <- picard::load(event_codelist_file)[
    ,
    .(coding_system, code, event_abbreviation, type, tags, system)
  ][
    ,
    concept_id := paste0(system, "_", event_abbreviation, "_", type)
  ][
    ,
    `:=`(
      concept_id = toupper(concept_id),
      coding_system = toupper(coding_system),
      tags = tolower(tags)
    )
  ][
    coding_system %in% unique(dap_coding_systems$coding_system)
  ]

  study_codelist <- unique(study_codelist)[
    ,
    priority := ifelse(tags == "narrow", 1, 2)
  ][
    ,
    c("concept_id", "code", "coding_system", "tags", "priority")
  ]

  # This is the first difference between procedure and events.
  column_info_list <- list(
    list(
      source_column = c("procedure_code", "procedure_code_vocabulary"),
      alias_name = c("code", "coding_system")
    )
  )

  # Call the function with the database connection and column information list
  result_list <- T2.DMM::get_unique_codelist(
    d2_db_conn,
    column_info_list,
    tb_name = paste0(schema_name, "PROCEDURES")
  )

  result_list[[1]] <- result_list[[1]][
    , variable := ifelse(.N > 1, "procedure_code", NA)
  ]

  unique_codelist <- data.table::rbindlist(result_list, use.names = TRUE)
  rm(result_list)

  unique_codelist <- unique_codelist[
    !is.na(code) & !code %in% "" & !is.na(coding_system)
  ][
    ,
    coding_system := toupper(coding_system)
  ]

  # finally, create dap-specific codelist from intersection of both
  codesheet_new <- unique(
    T2.DMM::create_dap_specific_codelist(
      unique_codelist,
      data.table::copy(study_codelist)
    )
  )

  # save codesheet_new as output specified in program list
  picard::save(
    data = codesheet_new,
    file_path =
      config_project$process_codelist$output_codelist_procedures,
    create_plot = FALSE
  )
  print("Procedures")
  print(table(codesheet_new$match_status, codesheet_new$coding_system))
  print(table(codesheet_new$match_status, codesheet_new$concept_id))
  rm(codesheet_new, study_codelist, column_info_list, unique_codelist)
  gc()
}

######################################
# PRE-PROCESS DAP SPECIFIC CONCEPT MAP
######################################
dap_specific_concept_map <- picard::load(
  config_project$process_codelist$dap_specific_concept_map
)
dap_specific_concept_map <- dap_specific_concept_map[dap_name %in% deap_name]
dap_specific_concept_map[dap_specific_concept_map == ""] <- NA
dap_specific_concept_map[dap_specific_concept_map == "NA"] <- NA

map_missing_info <- dap_specific_concept_map[
  is.na(column_name_1) | is.na(expected_value_1) | is.na(cdm_table_name)
]
if (nrow(map_missing_info) > 0) {
  logger::log_info(
    paste0(
      "Some concepts miss either cdm_table, column_name1 or",
      " expected_value 1 in the dap_specific_concept_map"
    )
  )
  # logger::log_info(unique(map_missing_info$concept_id))
  # logger::log_info(map_missing_info)
  # picard::save(
  #   data = map_missing_info,
  #   file_path = config_project$process_codelist$output_map_missing_infomation
  # )
}
dap_specific_concept_map <- dap_specific_concept_map[
  !is.na(column_name_1) & !is.na(expected_value_1) & !is.na(cdm_table_name)
]
dap_specific_concept_map[, dap_spec_id := paste0("dap_spec_id-", .I)]
picard::save(
  data = dap_specific_concept_map,
  file_path = config_project$process_codelist$output_concept_map
)

duckdb::dbDisconnect(d2_db_conn)

rm(
  d2_db_conn,
  dap_specific_concept_map,
  map_missing_info,
  dap_coding_systems
)
picard::audit_end()
gc()
if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
