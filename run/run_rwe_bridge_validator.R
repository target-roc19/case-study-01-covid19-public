`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

get_validation_status <- function(results) {
  status <- results$summary$validation_status
  if (is.null(status) || !nzchar(status)) {
    return("UNKNOWN")
  }
  status
}

get_validation_checks <- function(results) {
  checks <- results$summary$validation_checks
  if (!is.null(checks)) {
    return(checks)
  }

  list(
    total_errors = length(results$errors %||% list()),
    total_warnings = length(results$warnings %||% list()),
    total_passed = length(results$passed_checks %||% list())
  )
}

build_validation_summary <- function(results) {
  checks <- get_validation_checks(results)
  status <- get_validation_status(results)
  passed_checks <- results$passed_checks %||% list()
  warnings <- results$warnings %||% list()
  errors <- results$errors %||% list()

  summary <- sprintf("## RWE-BRIDGE Metadata Validation Results\n\n")
  summary <- paste0(summary, sprintf("**Status:** %s\n\n", status))
  summary <- paste0(
    summary,
    sprintf(
      "### Summary\n- Passed: %d\n- Errors: %d\n- Warnings: %d\n\n",
      checks$total_passed,
      checks$total_errors,
      checks$total_warnings
    )
  )

  if (length(passed_checks) > 0) {
    summary <- paste0(summary, "### Passed Checks\n")
    for (check in passed_checks) {
      summary <- paste0(summary, sprintf("- %s\n", check))
    }
    summary <- paste0(summary, "\n")
  }

  if (length(warnings) > 0) {
    summary <- paste0(summary, "### Warnings\n")
    for (warning in warnings) {
      summary <- paste0(summary, sprintf("- %s\n", warning))
    }
    summary <- paste0(summary, "\n")
  }

  if (length(errors) > 0) {
    summary <- paste0(summary, "### Errors\n")
    for (error in errors) {
      summary <- paste0(summary, sprintf("- %s\n", error))
    }
    summary <- paste0(summary, "\n")
  }

  summary
}

write_github_outputs <- function(
  results,
  summary_text = build_validation_summary(results),
  output_path = Sys.getenv("GITHUB_OUTPUT", "")
) {
  if (!nzchar(output_path)) {
    return(invisible(FALSE))
  }

  checks <- get_validation_checks(results)
  status <- get_validation_status(results)
  delimiter <- "__RWE_BRIDGE_SUMMARY__"

  cat(
    "validation_status=", status, "\n",
    sep = "",
    file = output_path,
    append = TRUE
  )
  cat(
    "total_errors=", checks$total_errors, "\n",
    sep = "",
    file = output_path,
    append = TRUE
  )
  cat(
    "total_passed=", checks$total_passed, "\n",
    sep = "",
    file = output_path,
    append = TRUE
  )
  cat(
    "total_warnings=", checks$total_warnings, "\n",
    sep = "",
    file = output_path,
    append = TRUE
  )
  cat(
    "validation_summary<<", delimiter, "\n",
    summary_text,
    delimiter, "\n",
    sep = "",
    file = output_path,
    append = TRUE
  )

  invisible(TRUE)
}

write_validation_summary <- function(
  results,
  summary_text = build_validation_summary(results),
  summary_path = Sys.getenv("GITHUB_STEP_SUMMARY", "")
) {
  if (nzchar(summary_path)) {
    cat(summary_text, file = summary_path, append = TRUE)
  }

  invisible(summary_text)
}

run_rwe_bridge_validator <- function(
  config_path = Sys.getenv("CONFIG_PATH", "configuration/validation-config.yaml"),
  validator_script = Sys.getenv("VALIDATOR_SCRIPT", "rwebridge.validator/R/validate.R"),
  results_file = Sys.getenv("RESULTS_FILE", "validation_results.rds"),
  verbose = !tolower(Sys.getenv("VALIDATOR_VERBOSE", "true")) %in% c("false", "0", "no")
) {
  validator_dir <- dirname(validator_script)
  helper_scripts <- file.path(validator_dir, c("io.R", "transforms.R"))
  for (script in helper_scripts) {
    if (!file.exists(script)) {
      stop("Required validator helper script not found: ", script)
    }
    source(script)
  }
  source(validator_script)
  validate_rwe_bridge <- get("validate_rwe_bridge_gh", mode = "function")
  results <- validate_rwe_bridge(yaml::read_yaml(config_path), verbose = verbose)
  summary_text <- build_validation_summary(results)
  saveRDS(results, results_file)
  write_github_outputs(results, summary_text = summary_text)
  write_validation_summary(results, summary_text = summary_text)
  invisible(results)
}

if (sys.nframe() == 0) {
  results <- run_rwe_bridge_validator()
  status <- get_validation_status(results)
  if (startsWith(status, "FAILED")) {
    quit(save = "no", status = 1)
  }
} else if (interactive()) {
  run_rwe_bridge_validator()
}
