#' Read Validation Config
#'
#' @param config_path Path to YAML config file.
#' @return Parsed config list.
#' @export
read_validation_config <- function(config_path) {
  yaml::read_yaml(config_path)
}

#' Create Config Template
#'
#' @param output_path Optional path where config template YAML should be written.
#'   Defaults to configuration/validation-config.yaml.
#' @param workflow_path Optional path where workflow YAML should be written.
#'   Defaults to .github/workflows/rwe-bridge-validator.yaml.
#' @param run_script_path Optional path where the local/CI run script should be written.
#'   Defaults to run/run_rwe_bridge_validator.R.
#' @return Invisible list of output paths.
#' @export
create_config_template <- function(
  output_path = "configuration/validation-config.yaml",
  workflow_path = ".github/workflows/rwe-bridge-validator.yaml",
  run_script_path = "run/run_rwe_bridge_validator.R"
) {
  template_roots <- c(
    file.path("rwebridge.validator", "templates"),
    "templates"
  )

  find_template <- function(relative_path) {
    for (root in template_roots) {
      candidate <- file.path(root, relative_path)
      if (file.exists(candidate)) {
        return(candidate)
      }
    }
    NULL
  }

  template_map <- list(
    config = find_template("validation-config.yaml"),
    workflow = find_template(file.path(".github", "workflows", "rwe-bridge-validator.yaml")),
    run_script = find_template(file.path("run", "run_rwe_bridge_validator.R"))
  )

  missing_templates <- names(template_map)[vapply(template_map, is.null, logical(1))]
  if (length(missing_templates) > 0) {
    stop(
      "Template file(s) not found: ",
      paste(missing_templates, collapse = ", "),
      ". Expected under rwebridge.validator/templates or templates."
    )
  }

  output_map <- list(
    config = output_path,
    workflow = workflow_path,
    run_script = run_script_path
  )

  for (nm in names(output_map)) {
    dir.create(dirname(output_map[[nm]]), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(template_map[[nm]], output_map[[nm]], overwrite = TRUE)
    if (!ok) {
      stop("Could not write ", nm, " template to: ", output_map[[nm]])
    }
  }

  invisible(output_map)
}

check_circular_dependencies <- function(composite_vars) {
  errors <- list()
  if (is.null(composite_vars) || nrow(composite_vars) == 0) {
    return(errors)
  }

  graph <- list()
  for (i in seq_len(nrow(composite_vars))) {
    v <- composite_vars[["variable_id"]][i]
    s <- composite_vars[["sub_variable_id"]][i]
    if (!is.na(v) && !is.na(s)) {
      if (is.null(graph[[v]])) {
        graph[[v]] <- character(0)
      }
      graph[[v]] <- unique(c(graph[[v]], s))
    }
  }

  visited <- list()
  in_stack <- list()

  dfs <- function(node, path = character(0)) {
    visited[[node]] <<- TRUE
    in_stack[[node]] <<- TRUE
    path <- c(path, node)

    neighbors <- graph[[node]]
    if (!is.null(neighbors)) {
      for (next_node in neighbors) {
        if (is.null(visited[[next_node]])) {
          cyc <- dfs(next_node, path)
          if (!is.null(cyc)) {
            return(cyc)
          }
        } else if (!is.null(in_stack[[next_node]]) && in_stack[[next_node]]) {
          idx <- which(path == next_node)[1]
          return(path[idx:length(path)])
        }
      }
    }

    in_stack[[node]] <<- FALSE
    NULL
  }

  for (node in names(graph)) {
    if (is.null(visited[[node]])) {
      cyc <- dfs(node)
      if (!is.null(cyc)) {
        errors[[length(errors) + 1]] <- paste0(
          "Circular dependency in composite_study_variables: ",
          paste(cyc, collapse = " -> "),
          " -> ", cyc[1]
        )
      }
    }
  }

  errors
}

validate_composite_hierarchy <- function(composite_vars, study_var_ids) {
  errors <- list()
  if (is.null(composite_vars) || nrow(composite_vars) == 0) {
    return(errors)
  }

  composite_ids <- unique(composite_vars[["variable_id"]][!is.na(composite_vars[["variable_id"]])])
  valid_sources <- unique(c(study_var_ids, composite_ids))

  for (cid in composite_ids) {
    idx <- composite_vars[["variable_id"]] == cid & !is.na(composite_vars[["sub_variable_id"]])
    sub_ids <- unique(composite_vars[["sub_variable_id"]][idx])
    invalid <- setdiff(sub_ids, valid_sources)
    if (length(invalid) > 0) {
      errors[[length(errors) + 1]] <- paste0(
        "Invalid sub_variable_id for ", cid, ": ", paste(invalid, collapse = ", ")
      )
    }
  }

  c(errors, check_circular_dependencies(composite_vars))
}

new_results <- function() {
  list(
    summary = list(
      files_found = list(),
      files_missing = list(),
      validation_status = "NOT_RUN"
    ),
    errors = list(),
    warnings = list(),
    passed_checks = list()
  )
}

add_error <- function(results, msg) {
  results$errors[[length(results$errors) + 1]] <- msg
  results
}

add_pass <- function(results, msg) {
  results$passed_checks[[length(results$passed_checks) + 1]] <- msg
  results
}

# Concept ID comparisons are case-insensitive via toupper().
normalize_concept_ids <- function(x) {
  values <- as.character(x)
  values <- values[!is.na(values)]
  toupper(values)
}


resolve_cdm_metadata_path <- function(config, data_dir = ".") {
  configured_path <- if (!is.null(config$cdm_metadata_file)) {
    config$cdm_metadata_file
  } else {
    NULL
  }
  common_configuration_dir <- if (!is.null(config$common_configuration_dir)) {
    config$common_configuration_dir
  } else {
    "transformations/common_configuration"
  }

  candidates <- c(
    configured_path,
    file.path(common_configuration_dir, "CDM_metadata.rds"),
    file.path(data_dir, common_configuration_dir, "CDM_metadata.rds"),
    file.path(data_dir, "CDM_metadata.rds")
  )
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])

  for (candidate in candidates) {
    if (check_file_exists(candidate)) {
      return(candidate)
    }
  }

  NULL
}

normalize_metadata_character <- function(x) {
  out <- trimws(as.character(x))
  out[!is.na(out) & !nzchar(out)] <- NA_character_
  out
}

parse_cdm_cols_list <- function(x) {
  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (length(values) == 0) {
    return(character(0))
  }

  values <- normalize_metadata_character(values)
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(character(0))
  }

  values <- gsub("^c\\s*\\((.*)\\)$", "\\1", values, perl = TRUE)
  pieces <- unlist(strsplit(values, "\\s*[,;|]\\s*", perl = TRUE), use.names = FALSE)
  pieces <- gsub("^[\\\"']|[\\\"']$", "", trimws(pieces))
  unique(pieces[nzchar(pieces)])
}

validate_dap_specific_concept_map_cdm <- function(dap_specific_concept_map, cdm_metadata) {
  validation <- list(errors = list(), passed_checks = list())

  if (is.null(dap_specific_concept_map) || nrow(dap_specific_concept_map) == 0) {
    return(validation)
  }

  if (!("cdm_table_name" %in% names(dap_specific_concept_map))) {
    validation$errors[[length(validation$errors) + 1]] <-
      "dap_specific_concept_map is missing required column: cdm_table_name"
    return(validation)
  }

  missing_metadata_columns <- setdiff(c("TABLE", "cols_list"), names(cdm_metadata))
  if (length(missing_metadata_columns) > 0) {
    validation$errors[[length(validation$errors) + 1]] <- paste0(
      "CDM_metadata is missing required column(s): ",
      paste(missing_metadata_columns, collapse = ", ")
    )
    return(validation)
  }

  map_tables <- normalize_metadata_character(dap_specific_concept_map[["cdm_table_name"]])
  metadata_tables <- normalize_metadata_character(cdm_metadata[["TABLE"]])
  valid_tables <- unique(metadata_tables[!is.na(metadata_tables)])

  missing_table_rows <- which(is.na(map_tables))
  if (length(missing_table_rows) > 0) {
    validation$errors[[length(validation$errors) + 1]] <- paste0(
      "dap_specific_concept_map.cdm_table_name is missing in row(s): ",
      paste(missing_table_rows, collapse = ", ")
    )
  }

  invalid_tables <- unique(map_tables[!is.na(map_tables) & !(map_tables %in% valid_tables)])
  if (length(invalid_tables) > 0) {
    validation$errors[[length(validation$errors) + 1]] <- paste0(
      "dap_specific_concept_map.cdm_table_name not found in CDM_metadata.TABLE: ",
      paste(invalid_tables, collapse = ", ")
    )
  }

  table_check_passed <- length(missing_table_rows) == 0 && length(invalid_tables) == 0
  if (table_check_passed) {
    validation$passed_checks[[length(validation$passed_checks) + 1]] <-
      "All dap_specific_concept_map.cdm_table_name values match CDM_metadata.TABLE"
  }

  mapped_column_fields <- c(
    grep("^column_name_[0-9]+$", names(dap_specific_concept_map), value = TRUE),
    intersect(c("keep_value_column_name", "keep_date_column_name"), names(dap_specific_concept_map))
  )

  invalid_column_references <- character(0)
  tables_without_columns <- character(0)
  column_cache <- list()

  for (row_index in seq_len(nrow(dap_specific_concept_map))) {
    table_name <- map_tables[row_index]
    if (is.na(table_name) || !(table_name %in% valid_tables)) {
      next
    }

    if (is.null(column_cache[[table_name]])) {
      table_rows <- !is.na(metadata_tables) & metadata_tables == table_name
      column_cache[[table_name]] <- parse_cdm_cols_list(cdm_metadata[["cols_list"]][table_rows])
    }
    valid_columns <- column_cache[[table_name]]

    if (length(valid_columns) == 0) {
      tables_without_columns <- unique(c(tables_without_columns, table_name))
      next
    }

    for (field in mapped_column_fields) {
      value <- normalize_metadata_character(dap_specific_concept_map[[field]][row_index])
      if (is.na(value)) {
        next
      }
      if (!(value %in% valid_columns)) {
        invalid_column_references <- c(
          invalid_column_references,
          sprintf("row %d %s='%s' (TABLE='%s')", row_index, field, value, table_name)
        )
      }
    }
  }

  if (length(tables_without_columns) > 0) {
    validation$errors[[length(validation$errors) + 1]] <- paste0(
      "CDM_metadata.cols_list has no values for TABLE(s): ",
      paste(tables_without_columns, collapse = ", ")
    )
  }

  if (length(invalid_column_references) > 0) {
    validation$errors[[length(validation$errors) + 1]] <- paste0(
      "dap_specific_concept_map column reference(s) not found in ",
      "CDM_metadata.cols_list for the specified cdm_table_name: ",
      paste(invalid_column_references, collapse = "; ")
    )
  }

  if (
    table_check_passed &&
      length(tables_without_columns) == 0 &&
      length(invalid_column_references) == 0
  ) {
    validation$passed_checks[[length(validation$passed_checks) + 1]] <- paste0(
      "All dap_specific_concept_map column_name_*, keep_value_column_name, and ",
      "keep_date_column_name values match CDM_metadata.cols_list"
    )
  }

  validation
}

#' Validate Metadata from YAML
#'
#' @param config_path Path to YAML config.
#' @param verbose Print progress.
#' @return Validation results list.
#' @export
validate_metadata_from_yaml <- function(config_path, verbose = TRUE) {
  cfg <- read_validation_config(config_path)
  validate_metadata(cfg, verbose = verbose)
}

#' Validate Metadata and Codelists
#'
#' @param config Config list.
#' @param verbose Print progress.
#' @return Validation results list.
#' @export
validate_metadata <- function(config, verbose = TRUE) {
  results <- new_results()

  data_dir <- if (!is.null(config$data_dir)) config$data_dir else "."
  files <- config$files
  reference_files <- config$reference_files
  transforms <- if (!is.null(config$reference_transforms)) config$reference_transforms else list()

  required <- c("study_variables")
  optional <- c("concept_preprocessing", "composite_study_variables", "study_cohort")

  required_paths <- list()
  for (nm in required) {
    p <- find_file_with_extension(data_dir, files[[nm]])
    required_paths[[nm]] <- p
    if (is.null(p)) {
      results$summary$files_missing[[nm]] <- files[[nm]]
      results <- add_error(results, paste("Required file missing:", nm))
    } else {
      results$summary$files_found[[nm]] <- p
    }
  }

  if (length(results$errors) > 0) {
    results$summary$validation_status <- "FAILED"
    return(invisible(results))
  }

  optional_paths <- list()
  for (nm in optional) {
    p <- find_file_with_extension(data_dir, files[[nm]])
    optional_paths[[nm]] <- p
    if (is.null(p)) {
      results$summary$files_missing[[nm]] <- files[[nm]]
    } else {
      results$summary$files_found[[nm]] <- p
    }
  }

  refs <- list()
  if (!is.null(reference_files)) {
    for (nm in names(reference_files)) {
      p <- find_file_with_extension(data_dir, reference_files[[nm]])
      if (is.null(p)) {
        results$summary$files_missing[[nm]] <- reference_files[[nm]]
        next
      }
      dt <- safe_read_file(p)
      refs[[nm]] <- apply_concept_transform(dt, transforms[[nm]])
      results$summary$files_found[[nm]] <- p
    }
  }

  dap_specific_concept_map <- refs[["dap_specific_concept_map"]]
  if (!is.null(dap_specific_concept_map)) {
    cdm_metadata_path <- resolve_cdm_metadata_path(config, data_dir = data_dir)
    if (is.null(cdm_metadata_path)) {
      expected_path <- if (!is.null(config$cdm_metadata_file)) {
        config$cdm_metadata_file
      } else {
        file.path("transformations", "common_configuration", "CDM_metadata.rds")
      }
      results$summary$files_missing[["CDM_metadata"]] <- expected_path
      results <- add_error(
        results,
        paste0(
          "CDM_metadata.rds is required to validate dap_specific_concept_map; expected at ",
          expected_path
        )
      )
    } else {
      cdm_metadata <- safe_read_file(cdm_metadata_path)
      results$summary$files_found[["CDM_metadata"]] <- cdm_metadata_path
      if (is.null(cdm_metadata)) {
        results <- add_error(results, paste0("CDM_metadata could not be loaded: ", cdm_metadata_path))
      } else {
        dap_validation <- validate_dap_specific_concept_map_cdm(
          dap_specific_concept_map,
          cdm_metadata
        )
        for (error in dap_validation$errors) {
          results <- add_error(results, error)
        }
        for (check in dap_validation$passed_checks) {
          results <- add_pass(results, check)
        }
      }
    }
  }

  study_variables <- safe_read_file(required_paths$study_variables)
  if (is.null(study_variables)) {
    results <- add_error(results, "study_variables could not be loaded")
    results$summary$validation_status <- "FAILED"
    return(invisible(results))
  }

  concept_preprocessing <- safe_read_file(optional_paths$concept_preprocessing)
  composite_study_variables <- safe_read_file(optional_paths$composite_study_variables)
  study_cohort <- safe_read_file(optional_paths$study_cohort)

  reference_concepts <- character(0)
  for (nm in names(refs)) {
    dt <- refs[[nm]]
    if (!is.null(dt) && "concept_id" %in% names(dt)) {
      reference_concepts <- c(reference_concepts, unique(normalize_concept_ids(dt[["concept_id"]])))
    }
  }
  reference_concepts <- unique(reference_concepts)

  prep_concepts <- character(0)
  prep_sub_concepts <- character(0)
  if (!is.null(concept_preprocessing)) {
    if ("concept_id" %in% names(concept_preprocessing)) {
      prep_concepts <- unique(normalize_concept_ids(concept_preprocessing[["concept_id"]]))
    }
    if ("sub_concept_id" %in% names(concept_preprocessing)) {
      prep_sub_concepts <- unique(normalize_concept_ids(concept_preprocessing[["sub_concept_id"]]))
    }
  }

  study_var_ids <- if ("variable_id" %in% names(study_variables)) {
    unique(study_variables[["variable_id"]][!is.na(study_variables[["variable_id"]])])
  } else {
    character(0)
  }
  study_var_concepts <- if ("concept_id" %in% names(study_variables)) {
    unique(normalize_concept_ids(study_variables[["concept_id"]]))
  } else {
    character(0)
  }

  valid_study_concepts <- unique(c(reference_concepts, prep_concepts))
  bad_study_concepts <- setdiff(study_var_concepts, valid_study_concepts)
  if (length(bad_study_concepts) > 0) {
    results <- add_error(results, paste0(
      "study_variables.concept_id not found in references/preprocessing: ",
      paste(bad_study_concepts, collapse = ", ")
    ))
  } else {
    results <- add_pass(results, "study_variables.concept_id validation passed")
  }

  if (!is.null(concept_preprocessing)) {
    valid_prep_concepts <- unique(c(prep_sub_concepts, study_var_concepts))
    bad_prep_concepts <- setdiff(prep_concepts, valid_prep_concepts)
    if (length(bad_prep_concepts) > 0) {
      results <- add_error(results, paste0(
        "concept_preprocessing.concept_id invalid: ",
        paste(bad_prep_concepts, collapse = ", ")
      ))
    }

    valid_sub <- unique(c(prep_concepts, reference_concepts))
    bad_sub <- setdiff(prep_sub_concepts, valid_sub)
    if (length(bad_sub) > 0) {
      results <- add_error(results, paste0(
        "concept_preprocessing.sub_concept_id invalid: ",
        paste(bad_sub, collapse = ", ")
      ))
    }
  }

  if (!is.null(composite_study_variables)) {
    if ("variable_id" %in% names(composite_study_variables)) {
      composite_ids <- unique(composite_study_variables[["variable_id"]][!is.na(composite_study_variables[["variable_id"]])])
      bad_composite_ids <- setdiff(composite_ids, study_var_ids)
      if (length(bad_composite_ids) > 0) {
        results <- add_error(results, paste0(
          "composite_study_variables.variable_id not in study_variables: ",
          paste(bad_composite_ids, collapse = ", ")
        ))
      }
    }

    if ("sub_variable_id" %in% names(composite_study_variables) && "variable_id" %in% names(composite_study_variables)) {
      composite_ids <- unique(composite_study_variables[["variable_id"]][!is.na(composite_study_variables[["variable_id"]])])
      valid_sub_ids <- unique(c(study_var_ids, composite_ids))
      sub_ids <- unique(composite_study_variables[["sub_variable_id"]][!is.na(composite_study_variables[["sub_variable_id"]])])
      bad_sub_ids <- setdiff(sub_ids, valid_sub_ids)
      if (length(bad_sub_ids) > 0) {
        results <- add_error(results, paste0(
          "composite_study_variables.sub_variable_id invalid: ",
          paste(bad_sub_ids, collapse = ", ")
        ))
      }

      hierarchy_errors <- validate_composite_hierarchy(composite_study_variables, study_var_ids)
      if (length(hierarchy_errors) > 0) {
        for (e in hierarchy_errors) {
          results <- add_error(results, e)
        }
      } else {
        results <- add_pass(results, "composite_study_variables hierarchy validation passed")
      }
    }
  }

  if (!is.null(study_cohort) && "variable_id" %in% names(study_cohort)) {
    cohort_ids <- unique(study_cohort[["variable_id"]][!is.na(study_cohort[["variable_id"]])])
    bad_cohort_ids <- setdiff(cohort_ids, study_var_ids)
    if (length(bad_cohort_ids) > 0) {
      results <- add_error(results, paste0(
        "study_cohort.variable_id not in study_variables: ",
        paste(bad_cohort_ids, collapse = ", ")
      ))
    } else {
      results <- add_pass(results, "study_cohort.variable_id validation passed")
    }
  }

  results$summary$validation_checks <- list(
    total_errors = length(results$errors),
    total_warnings = length(results$warnings),
    total_passed = length(results$passed_checks)
  )
  results$summary$validation_status <- if (length(results$errors) == 0) "PASSED" else "FAILED"

  if (verbose) {
    cat("Validation status:", results$summary$validation_status, "\n")
    cat(
      "Passed:", results$summary$validation_checks$total_passed,
      "Errors:", results$summary$validation_checks$total_errors, "\n"
    )
  }

  invisible(results)
}


# ============================================================================
# MAIN VALIDATION FUNCTION (GITHUB ACTIONS VERSION)
# ============================================================================

validate_rwe_bridge_gh <- function(config, verbose = TRUE) {
  # Initialize results
  results <- list(
    summary = list(
      files_found = list(),
      files_missing = list()
    ),
    errors = list(),
    warnings = list(),
    passed_checks = list()
  )

  if (verbose) {
    cat("=", rep.int("=", 70), "\n", sep = "")
    cat("RWE-BRIDGE METADATA VALIDATOR (GitHub Actions)\n")
    cat("=", rep.int("=", 70), "\n\n", sep = "")
  }

  # Extract configuration
  data_dir <- config$data_dir
  reference_file_paths <- config$reference_files

  # ========================================================================
  # STEP 1: FILE EXISTENCE CHECK
  # ========================================================================

  if (verbose) {
    cat("STEP 1: Checking file existence\n")
    cat("-", rep.int("-", 68), "\n\n", sep = "")
  }

  # Check required core metadata files
  required_files <- c("study_variables")
  optional_files <- c("concept_preprocessing", "composite_study_variables", "study_cohort")

  required_status <- list()
  required_paths <- list()

  if (verbose) cat("Required core metadata files:\n")
  for (file in required_files) {
    filepath <- find_file_with_extension(data_dir, file)
    exists <- !is.null(filepath)
    required_status[[file]] <- exists
    required_paths[[file]] <- filepath

    if (exists) {
      if (verbose) cat("  ✓", file, "\n")
      results$summary$files_found[[file]] <- filepath
    } else {
      if (verbose) cat("  ✗", file, "NOT FOUND\n")
      results$summary$files_missing[[file]] <- file.path(data_dir, file)
      results$errors[[length(results$errors) + 1]] <-
        paste("Required file missing:", file)
    }
  }

  if (!all(unlist(required_status))) {
    results$summary$validation_status <- "FAILED - Missing required files"
    return(invisible(results))
  }

  if (verbose) cat("\nOptional core metadata files:\n")
  optional_status <- list()
  optional_paths <- list()

  for (file in optional_files) {
    filepath <- find_file_with_extension(data_dir, file)
    exists <- !is.null(filepath)
    optional_status[[file]] <- exists
    optional_paths[[file]] <- filepath

    if (exists) {
      if (verbose) cat("  ✓", file, "\n")
      results$summary$files_found[[file]] <- filepath
    } else {
      if (verbose) cat("  ○", file, "(not found)\n")
      results$summary$files_missing[[file]] <- file.path(data_dir, file)
    }
  }

  if (verbose) cat("\nReference codelists:\n")
  ref_status <- list()

  for (ref_name in names(reference_file_paths)) {
    ref_path <- find_file_with_extension(data_dir, reference_file_paths[[ref_name]])
    if (is.null(ref_path)) {
      if (verbose) cat("  ○", ref_name, "(path not provided)\n")
      ref_status[[ref_name]] <- FALSE
    } else {
      exists <- check_file_exists(ref_path)
      ref_status[[ref_name]] <- exists

      if (exists) {
        if (verbose) cat("  ✓", ref_name, "\n")
        results$summary$files_found[[ref_name]] <- ref_path
      } else {
        if (verbose) cat("  ✗", ref_name, "(not found)\n")
        results$summary$files_missing[[ref_name]] <- ref_path
      }
    }
  }

  cdm_metadata_path <- NULL
  if (isTRUE(ref_status$dap_specific_concept_map)) {
    cdm_metadata_path <- resolve_cdm_metadata_path(config, data_dir = data_dir)
    if (is.null(cdm_metadata_path)) {
      expected_path <- if (!is.null(config$cdm_metadata_file)) {
        config$cdm_metadata_file
      } else {
        file.path("transformations", "common_configuration", "CDM_metadata.rds")
      }
      results$summary$files_missing[["CDM_metadata"]] <- expected_path
      if (verbose) cat("  ✗ CDM_metadata (not found at ", expected_path, ")\n", sep = "")
    } else {
      results$summary$files_found[["CDM_metadata"]] <- cdm_metadata_path
      if (verbose) cat("  ✓ CDM_metadata\n")
    }
  }

  if (verbose) cat("\n")

  # ========================================================================
  # STEP 2: LOAD FILES
  # ========================================================================

  if (verbose) {
    cat("=", rep.int("=", 70), "\n", sep = "")
    cat("STEP 2: Loading data files\n")
    cat("=", rep.int("=", 70), "\n\n", sep = "")
  }

  study_variables <- safe_read_file(required_paths$study_variables)

  if (is.null(study_variables)) {
    results$errors[[length(results$errors) + 1]] <- "Failed to load study_variables"
    if (verbose) cat("✗ Failed to load study_variables\n")
    results$summary$validation_status <- "FAILED - Cannot load required file"
    return(invisible(results))
  }

  if (verbose) cat("✓ Loaded study_variables (", nrow(study_variables), " rows)\n", sep = "")

  # Load optional files
  concept_preprocessing <- NULL
  if (optional_status$concept_preprocessing) {
    concept_preprocessing <- safe_read_file(optional_paths$concept_preprocessing)
    if (!is.null(concept_preprocessing)) {
      if (verbose) cat("✓ Loaded concept_preprocessing (", nrow(concept_preprocessing), " rows)\n", sep = "")
    }
  }

  composite_study_variables <- NULL
  if (optional_status$composite_study_variables) {
    composite_study_variables <- safe_read_file(optional_paths$composite_study_variables)
    if (!is.null(composite_study_variables)) {
      if (verbose) cat("✓ Loaded composite_study_variables (", nrow(composite_study_variables), " rows)\n", sep = "")
    }
  }

  study_cohort <- NULL
  if (optional_status$study_cohort) {
    study_cohort <- safe_read_file(optional_paths$study_cohort)
    if (!is.null(study_cohort)) {
      if (verbose) cat("✓ Loaded study_cohort (", nrow(study_cohort), " rows)\n", sep = "")
    }
  }

  # ========================================================================
  # STEP 3: LOAD REFERENCE FILES AND APPLY TRANSFORMATIONS
  # ========================================================================

  if (verbose) {
    cat("\n", "=", rep.int("=", 70), "\n", sep = "")
    cat("STEP 3: Loading and transforming reference codelists\n")
    cat("=", rep.int("=", 70), "\n\n", sep = "")
  }

  dap_specific_concept_map <- NULL
  if (ref_status$dap_specific_concept_map) {
    file_path <- find_file_with_extension(data_dir, reference_file_paths$dap_specific_concept_map)
    dap_specific_concept_map <- safe_read_file(file_path)
    if (!is.null(dap_specific_concept_map)) {
      if (verbose) cat("✓ Loaded dap_specific_concept_map (", nrow(dap_specific_concept_map), " rows)\n", sep = "")
    }
  }

  cdm_metadata <- NULL
  if (!is.null(dap_specific_concept_map) && !is.null(cdm_metadata_path)) {
    cdm_metadata <- safe_read_file(cdm_metadata_path)
    if (!is.null(cdm_metadata)) {
      if (verbose) cat("✓ Loaded CDM_metadata (", nrow(cdm_metadata), " rows)\n", sep = "")
    }
  }

  # Load and transform events codelist
  codelist_events <- NULL
  if (ref_status$codelist_events) {
    file_path <- find_file_with_extension(data_dir, reference_file_paths$codelist_events)
    codelist_events <- safe_read_file(file_path)
    if (!is.null(codelist_events)) {
      if (verbose) cat("✓ Loaded codelist_events (", nrow(codelist_events), " rows, before transform)\n", sep = "")
      codelist_events <- transform_events_concept_id(codelist_events)
      if (verbose) {
        cat("  → Transformed concept_id with: paste0(system, '_', event_abbreviation, '_', type)\n")
        cat("  → Result: ", nrow(unique(codelist_events[, .(concept_id)])), " unique concept_ids\n", sep = "")
      }
    }
  }

  # Load and transform procedures codelist
  codelist_procedures <- NULL
  if (ref_status$codelist_procedures) {
    file_path <- find_file_with_extension(data_dir, reference_file_paths$codelist_procedures)
    codelist_procedures <- safe_read_file(file_path)
    if (!is.null(codelist_procedures)) {
      if (verbose) cat("✓ Loaded codelist_procedures (", nrow(codelist_procedures), " rows, before transform)\n", sep = "")
      codelist_procedures <- transform_procedures_concept_id(codelist_procedures)
      if (verbose) {
        cat("  → Transformed concept_id with: paste0(system, '_', event_abbreviation, '_', type)\n")
        cat("  → Result: ", nrow(unique(codelist_procedures[, .(concept_id)])), " unique concept_ids\n", sep = "")
      }
    }
  }

  # Load and transform medicines codelist
  codelist_medicines <- NULL
  if (ref_status$codelist_medicines) {
    file_path <- find_file_with_extension(data_dir, reference_file_paths$codelist_medicines)
    codelist_medicines <- safe_read_file(file_path)
    if (!is.null(codelist_medicines)) {
      if (verbose) cat("✓ Loaded codelist_medicines (", nrow(codelist_medicines), " rows, before transform)\n", sep = "")
      codelist_medicines <- transform_medicines_concept_id(codelist_medicines)
      if (verbose) {
        cat("  → Transformed concept_id with: toupper(drug_abbreviation)\n")
        cat("  → Result: ", nrow(unique(codelist_medicines[, .(concept_id)])), " unique concept_ids\n", sep = "")
      }
    }
  }

  # Load vaccines (no transformation)
  codelist_vaccines <- NULL
  if (ref_status$codelist_vaccines) {
    file_path <- find_file_with_extension(data_dir, reference_file_paths$codelist_vaccines)
    codelist_vaccines <- safe_read_file(file_path)
    if (!is.null(codelist_vaccines)) {
      if (verbose) cat("✓ Loaded codelist_vaccines (", nrow(codelist_vaccines), " rows)\n", sep = "")
    }
  }

  # ========================================================================
  # STEP 4: BUILD REFERENCE CONCEPT ID LIST
  # ========================================================================

  if (verbose) {
    cat("\n", "=", rep.int("=", 70), "\n", sep = "")
    cat("STEP 4: Building reference concept ID catalog\n")
    cat("=", rep.int("=", 70), "\n\n", sep = "")
  }

  reference_concept_ids <- c()
  reference_sources <- list()

  if (!is.null(dap_specific_concept_map) && "concept_id" %in% names(dap_specific_concept_map)) {
    concepts <- unique(normalize_concept_ids(dap_specific_concept_map$concept_id))
    reference_concept_ids <- c(reference_concept_ids, concepts)
    reference_sources$dap_specific_concept_map <- concepts
    if (verbose) cat("  From dap_specific_concept_map:", length(concepts), "unique concept_ids\n")
  }

  if (!is.null(codelist_events) && "concept_id" %in% names(codelist_events)) {
    concepts <- unique(normalize_concept_ids(codelist_events$concept_id))
    reference_concept_ids <- c(reference_concept_ids, concepts)
    reference_sources$codelist_events <- concepts
    if (verbose) cat("  From codelist_events:", length(concepts), "unique concept_ids\n")
  }

  if (!is.null(codelist_medicines) && "concept_id" %in% names(codelist_medicines)) {
    concepts <- unique(normalize_concept_ids(codelist_medicines$concept_id))
    reference_concept_ids <- c(reference_concept_ids, concepts)
    reference_sources$codelist_medicines <- concepts
    if (verbose) cat("  From codelist_medicines:", length(concepts), "unique concept_ids\n")
  }

  if (!is.null(codelist_procedures) && "concept_id" %in% names(codelist_procedures)) {
    concepts <- unique(normalize_concept_ids(codelist_procedures$concept_id))
    reference_concept_ids <- c(reference_concept_ids, concepts)
    reference_sources$codelist_procedures <- concepts
    if (verbose) cat("  From codelist_procedures:", length(concepts), "unique concept_ids\n")
  }

  if (!is.null(codelist_vaccines) && "concept_id" %in% names(codelist_vaccines)) {
    concepts <- unique(normalize_concept_ids(codelist_vaccines$concept_id))
    reference_concept_ids <- c(reference_concept_ids, concepts)
    reference_sources$codelist_vaccines <- concepts
    if (verbose) cat("  From codelist_vaccines:", length(concepts), "unique concept_ids\n")
  }

  reference_concept_ids <- unique(reference_concept_ids)
  if (verbose) cat("\n  Total unique concept_ids in all reference files:", length(reference_concept_ids), "\n\n")

  # ========================================================================
  # BUILD STUDY DATA LISTS
  # ========================================================================

  preprocessing_concept_ids <- c()
  preprocessing_sub_concept_ids <- c()
  if (!is.null(concept_preprocessing) && "concept_id" %in% names(concept_preprocessing)) {
    preprocessing_concept_ids <- unique(normalize_concept_ids(concept_preprocessing[["concept_id"]]))
    if ("sub_concept_id" %in% names(concept_preprocessing)) {
      preprocessing_sub_concept_ids <- unique(normalize_concept_ids(concept_preprocessing[["sub_concept_id"]]))
    }
  }

  study_vars_ids <- c()
  study_vars_concept_ids <- c()
  if (!is.null(study_variables)) {
    if ("variable_id" %in% names(study_variables)) {
      study_vars_ids <- unique(study_variables[!is.na(variable_id), variable_id])
    }
    if ("concept_id" %in% names(study_variables)) {
      study_vars_concept_ids <- unique(normalize_concept_ids(study_variables[["concept_id"]]))
    }
  }

  # ========================================================================
  # PERFORM VALIDATION CHECKS
  # ========================================================================

  if (verbose) {
    cat("=", rep.int("=", 70), "\n", sep = "")
    cat("STEP 5: Performing validation checks\n")
    cat("=", rep.int("=", 70), "\n\n", sep = "")
  }

  # CHECK 0: dap_specific_concept_map against CDM metadata
  if (verbose) cat("CHECK 0: dap_specific_concept_map CDM metadata validation\n")
  if (!is.null(dap_specific_concept_map)) {
    if (is.null(cdm_metadata)) {
      error_msg <- if (is.null(cdm_metadata_path)) {
        "CDM_metadata.rds is required to validate dap_specific_concept_map; expected at transformations/common_configuration/CDM_metadata.rds"
      } else {
        paste0("CDM_metadata could not be loaded: ", cdm_metadata_path)
      }
      results$errors[[length(results$errors) + 1]] <- error_msg
      if (verbose) cat("✗ ERROR: ", error_msg, "\n\n", sep = "")
    } else {
      dap_validation <- validate_dap_specific_concept_map_cdm(
        dap_specific_concept_map,
        cdm_metadata
      )
      for (error in dap_validation$errors) {
        results$errors[[length(results$errors) + 1]] <- error
        if (verbose) cat("✗ ERROR: ", error, "\n", sep = "")
      }
      for (check in dap_validation$passed_checks) {
        results$passed_checks[[length(results$passed_checks) + 1]] <- check
        if (verbose) cat("✓ ", check, "\n", sep = "")
      }
      if (verbose) cat("\n")
    }
  } else if (verbose) {
    cat("⚠ SKIPPED: dap_specific_concept_map not found or empty\n\n")
  }

  # CHECK 1: study_variables.concept_id
  if (verbose) cat("CHECK 1: study_variables.concept_id validation\n")
  if (!is.null(study_variables) && "concept_id" %in% names(study_variables)) {
    valid_concept_ids <- unique(c(reference_concept_ids, preprocessing_concept_ids))
    invalid_concepts <- setdiff(study_vars_concept_ids, valid_concept_ids)

    if (length(invalid_concepts) > 0) {
      error_msg <- paste0(
        "study_variables.concept_id NOT found in reference or preprocessing: ",
        paste(invalid_concepts, collapse = ", ")
      )
      results$errors[[length(results$errors) + 1]] <- error_msg
      if (verbose) cat("✗ ERROR: ", error_msg, "\n\n", sep = "")
    } else {
      results$passed_checks[[length(results$passed_checks) + 1]] <-
        "All study_variables.concept_id values valid"
      if (verbose) {
        in_ref <- length(intersect(study_vars_concept_ids, reference_concept_ids))
        in_prep <- length(setdiff(study_vars_concept_ids, reference_concept_ids))
        cat("✓ PASSED (", in_ref, " in reference, ", in_prep, " in preprocessing)\n\n", sep = "")
      }
    }
  } else if (verbose) {
    cat("⚠ SKIPPED: concept_id column not found\n\n")
  }

  # CHECK 2: concept_preprocessing validation
  if (verbose) cat("CHECK 2: concept_preprocessing validation\n")
  if (!is.null(concept_preprocessing) && nrow(concept_preprocessing) > 0 &&
    "concept_id" %in% names(concept_preprocessing)) {
    # Check 2A: concept_id validation
    valid_sources <- unique(c(preprocessing_sub_concept_ids, study_vars_concept_ids))
    invalid_prep_concepts <- setdiff(preprocessing_concept_ids, valid_sources)

    if (length(invalid_prep_concepts) > 0) {
      error_msg <- paste0(
        "concept_preprocessing.concept_id NOT valid: ",
        paste(invalid_prep_concepts, collapse = ", ")
      )
      results$errors[[length(results$errors) + 1]] <- error_msg
      if (verbose) cat("✗ ERROR (concept_id): ", error_msg, "\n", sep = "")
    } else {
      results$passed_checks[[length(results$passed_checks) + 1]] <-
        "All concept_preprocessing.concept_id values valid"
      if (verbose) cat("✓ concept_id: PASSED\n")
    }

    # Check 2B: sub_concept_id validation
    valid_sub_concepts <- unique(c(preprocessing_concept_ids, reference_concept_ids))
    invalid_sub_concepts <- setdiff(preprocessing_sub_concept_ids, valid_sub_concepts)

    if (length(invalid_sub_concepts) > 0) {
      error_msg <- paste0(
        "concept_preprocessing.sub_concept_id NOT valid: ",
        paste(invalid_sub_concepts, collapse = ", ")
      )
      results$errors[[length(results$errors) + 1]] <- error_msg
      if (verbose) cat("✗ ERROR (sub_concept_id): ", error_msg, "\n\n", sep = "")
    } else {
      results$passed_checks[[length(results$passed_checks) + 1]] <-
        "All concept_preprocessing.sub_concept_id values valid"
      if (verbose) cat("✓ sub_concept_id: PASSED\n\n")
    }
  } else if (verbose) {
    cat("⚠ SKIPPED: file not found or empty\n\n")
  }

  # CHECK 3: composite_study_variables validation
  if (verbose) cat("CHECK 3: composite_study_variables validation\n")
  if (!is.null(composite_study_variables) && nrow(composite_study_variables) > 0) {
    # Validate basic foreign keys
    check_3_passed <- TRUE

    if ("variable_id" %in% names(composite_study_variables)) {
      composite_var_ids <- unique(composite_study_variables[!is.na(variable_id), variable_id])

      # All composite variable_ids must be in study_variables
      missing_var_ids <- setdiff(composite_var_ids, study_vars_ids)

      if (length(missing_var_ids) > 0) {
        error_msg <- paste0(
          "composite_study_variables.variable_id NOT in study_variables: ",
          paste(unique(missing_var_ids), collapse = ", ")
        )
        results$errors[[length(results$errors) + 1]] <- error_msg
        if (verbose) cat("✗ ERROR (variable_id): ", error_msg, "\n", sep = "")
        check_3_passed <- FALSE
      } else {
        if (verbose) cat("✓ variable_id: PASSED\n")
      }
    }

    if ("sub_variable_id" %in% names(composite_study_variables)) {
      composite_sub_var_ids <- unique(composite_study_variables[!is.na(sub_variable_id), sub_variable_id])

      # All sub_variable_ids must be either in study_variables OR be another composite variable
      # This allows composites to be composed of other composites
      valid_sub_targets <- unique(c(study_vars_ids, composite_var_ids))
      missing_sub_var_ids <- setdiff(composite_sub_var_ids, valid_sub_targets)

      if (length(missing_sub_var_ids) > 0) {
        error_msg <- paste0(
          "composite_study_variables.sub_variable_id NOT in study_variables or composite variables: ",
          paste(unique(missing_sub_var_ids), collapse = ", ")
        )
        results$errors[[length(results$errors) + 1]] <- error_msg
        if (verbose) cat("✗ ERROR (sub_variable_id): ", error_msg, "\n", sep = "")
        check_3_passed <- FALSE
      } else {
        results$passed_checks[[length(results$passed_checks) + 1]] <-
          "All composite_study_variables.sub_variable_id valid"
        if (verbose) cat("✓ sub_variable_id: PASSED\n\n")
      }
    }

    # Validate composite hierarchy (including circular dependency check)
    if (verbose) cat("  Checking composite hierarchy...\n")
    hierarchy_errors <- validate_composite_hierarchy(composite_study_variables, study_vars_ids)

    if (length(hierarchy_errors) > 0) {
      for (error in hierarchy_errors) {
        results$errors[[length(results$errors) + 1]] <- error
        if (verbose) cat("  ✗ ERROR: ", error, "\n", sep = "")
      }
      check_3_passed <- FALSE
    } else if (verbose) {
      cat("  ✓ Composite hierarchy: VALID (no circular dependencies)\n")
    }

    if (check_3_passed) {
      results$passed_checks[[length(results$passed_checks) + 1]] <-
        "All composite_study_variables validations passed (including hierarchy)"
      if (verbose) cat("\n✓ CHECK 3: PASSED\n\n")
    } else if (verbose) {
      cat("\n")
    }
  } else if (verbose) {
    cat("⚠ SKIPPED: file not found or empty\n\n")
  }

  # CHECK 4: study_cohort validation
  if (verbose) cat("CHECK 4: study_cohort validation\n")
  if (!is.null(study_cohort) && nrow(study_cohort) > 0 &&
    "variable_id" %in% names(study_cohort)) {
    cohort_var_ids <- study_cohort[!is.na(variable_id), variable_id]
    missing_cohort_var_ids <- setdiff(cohort_var_ids, study_vars_ids)

    if (length(missing_cohort_var_ids) > 0) {
      error_msg <- paste0(
        "study_cohort.variable_id NOT in study_variables: ",
        paste(unique(missing_cohort_var_ids), collapse = ", ")
      )
      results$errors[[length(results$errors) + 1]] <- error_msg
      if (verbose) cat("✗ ERROR: ", error_msg, "\n\n", sep = "")
    } else {
      results$passed_checks[[length(results$passed_checks) + 1]] <-
        "All study_cohort.variable_id valid"
      if (verbose) cat("✓ PASSED\n\n")
    }
  } else if (verbose) {
    cat("⚠ SKIPPED: file not found or empty\n\n")
  }

  # ========================================================================
  # FINAL SUMMARY
  # ========================================================================

  results$summary$validation_checks <- list(
    total_errors = length(results$errors),
    total_warnings = length(results$warnings),
    total_passed = length(results$passed_checks)
  )

  if (verbose) {
    cat("=", rep.int("=", 70), "\n", sep = "")
    cat("VALIDATION SUMMARY\n")
    cat("=", rep.int("=", 70), "\n\n", sep = "")

    cat("Validation Results:\n")
    cat("  ✓ Passed:", results$summary$validation_checks$total_passed, "\n")
    cat("  ✗ Errors:", results$summary$validation_checks$total_errors, "\n")
    cat("  ⚠ Warnings:", results$summary$validation_checks$total_warnings, "\n\n")
  }

  if (results$summary$validation_checks$total_errors == 0) {
    results$summary$validation_status <- "PASSED"
  } else {
    results$summary$validation_status <- "FAILED"
  }

  if (verbose) {
    cat("Status: ", results$summary$validation_status, "\n", sep = "")
    cat("=", rep.int("=", 70), "\n\n", sep = "")
  }

  invisible(results)
}
