# ---------------------------------------------------------------
# Step: compute_ipsmw
#
# Input:
#   D4_StudyCohort_Main(.parquet)
#   D4_StudyCohort_Supplementary(.parquet)
#   D4_StudyCohortFupMain/Supplementary(.parquet)
#   metadata/ipsmw_variables.csv
#
# Output:
#   D4_StudyCohort_IPSMW_Main/Supplementary.parquet
#   D5_PS_Coef_IPSMW_Main/Supplementary.csv
#   D5_PS_ModelBuilding_IPSMW_Main/Supplementary.csv
#   D4_StudyCohortFupMain/Supplementary.parquet (updated with compliant_exposed)
# ---------------------------------------------------------------

if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("compute_ipsmw.R")
  lm$start_capturing_prints()
}

# Load helper utilities used by this orchestration script.
source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "CohortPropensity",
  "fit_ps_model.R"
))

source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "fn_compute_ipsmw.R"
))

# Load configuration utilities (I/O and YAML normalization).
source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "fn_config_utilities.R"
))

# Load project configuration specifying analyses and estimands.
config_project <- picard::load_config("configuration/config_project.yaml")
deap_name <- config_project$DEAP_configuration$name
compute_ipsmw_config <- config_project$compute_ipsmw

## Audit setup ---
picard::audit_start(
  file_name = "compute_ipsmw",
  deap_name = deap_name
)
logger::log_info("Start of the compute_ipsmw script.")

# Validate selected analyses against allowed values (Main or Supplementary).
selected_analyses <- normalize_vec_config(compute_ipsmw_config$analyses)
allowed_analyses <- c("Main", "Supplementary")
invalid_analyses <- setdiff(selected_analyses, allowed_analyses)
if (length(invalid_analyses) > 0) {
  stop(paste0("Invalid analyses value(s): ", paste(invalid_analyses, collapse = ", "), "."))
}

# SAP requirement: IPSMW must be estimated only for estimand 2.
selected_estimands <- as.integer(normalize_vec_config(compute_ipsmw_config$estimands))
if (length(selected_estimands) != 1L || selected_estimands[1] != 2L) {
  stop("compute_ipsmw must be configured with estimands: [2] (SAP requirement).")
}

# Extract input/output configuration paths from the config section.
input_dir_d4 <- compute_ipsmw_config$input$dir_d4
cohort_file_template <- compute_ipsmw_config$input$cohort_file
fup_file_template <- compute_ipsmw_config$input$fup_file
cov_file_template <- compute_ipsmw_config$input$cov_file

metadata_file <- compute_ipsmw_config$metadata$ipsmw_variables_file

output_dir_d4 <- compute_ipsmw_config$output$dir_d4
output_dir_d5 <- compute_ipsmw_config$output$dir_d5

# Metadata controls model covariate selection and factor handling.
ipsmw_variables <- data.table::fread(metadata_file, sep = ";")

# Process both analyses independently to create parallel Main/Supplementary
# IPSMW outputs.
for (analysis_label in selected_analyses) {
  logger::log_info(paste("Computing IPSMW for", analysis_label))

  # Construct input file paths by substituting {analysis} placeholder.
  cohort_file <- gsub("\\{analysis\\}", analysis_label, cohort_file_template)
  fup_file <- gsub("\\{analysis\\}", analysis_label, fup_file_template)
  cov_file <- gsub("\\{analysis\\}", analysis_label, cov_file_template)
  # Load input data for this analysis.
  cohort_data <- picard::load(input_dir_d4, cohort_file)[group %in% c("EXPOSED", "CONTROL")]
  fup_data <- picard::load(input_dir_d4, fup_file)[group %in% c("EXPOSED", "CONTROL")]
  cov_data <- picard::load(input_dir_d4, cov_file)[group %in% c("EXPOSED", "CONTROL")]

  # Add cov_data$SV_CCI to cohort_data
  cols_to_keep <- intersect(
    names(cov_data),
    ipsmw_variables$var
  )
  cols_to_keep <- c("person_id", "T0", "match_id", setdiff(cols_to_keep, names(cohort_data)))
  cohort_data <- data.table::merge.data.table(
    cohort_data,
    cov_data[, ..cols_to_keep, with = FALSE],
    by = c("person_id", "T0", "match_id"),
    all.x = TRUE,
    all.y = FALSE
  )

  # Keep only variable in ipsmw_variables for model fitting and prediction
  cohort_data <- cohort_data[, c("person_id", "T0", "match_id", "group", ipsmw_variables$var), with = FALSE]

  # Core computation: derive compliance, fit model on exposed, predict control
  # principal stratum scores, and compute IPSMW with error handling.
  tryCatch(
    {
      res <- compute_ipsmw(
        cohort_data = cohort_data,
        fup_data = fup_data,
        ps_variables = ipsmw_variables,
        analysis_label = analysis_label,
        compliance_col = "compliant_exposed",
        primary_method = "glm",
        ps_trunc_bounds = c(
          compute_ipsmw_config$truncation$q_lower,
          compute_ipsmw_config$truncation$q_upper
        ),
        save_diagnostics = compute_ipsmw_config$save_diagnostics
      )

      # Save person-level IPSMW dataset.
      picard::save(
        data = res$D4_StudyCohort_IPSMW,
        file_path = output_dir_d4,
        file_name = paste0("D4_StudyCohort_IPSMW_", analysis_label, ".parquet")
      )

      # Save person-level IPSMW dataset.
      picard::save(
        data = res$D4_StudyCohort_IPSMW_ALL,
        file_path = output_dir_d4,
        file_name = paste0("D4_StudyCohort_IPSMW_ALL_", analysis_label, ".parquet")
      )

      # Save model coefficient summaries.
      picard::save(
        data = res$D5_PS_Coef_IPSMW,
        file_path = output_dir_d5,
        file_name = paste0("D5_PS_Coef_IPSMW_", analysis_label, ".csv")
      )

      # Save variable screening/model-building diagnostics.
      picard::save(
        data = res$D5_PS_ModelBuilding_IPSMW,
        file_path = output_dir_d5,
        file_name = paste0("D5_PS_ModelBuilding_IPSMW_", analysis_label, ".csv")
      )

      logger::log_info(paste(
        "Finished",
        analysis_label,
        "| IPSMW rows:",
        nrow(res$D4_StudyCohort_IPSMW)
      ))
    },
    error = function(e) {
      logger::log_warn(paste(
        "IPSMW computation failed for",
        analysis_label,
        "| Error:",
        conditionMessage(e)
      ))
    }
  )
}

# Clean the environment
objects_to_remove <- c(
  "cohort_data", "fup_data", "res",
  "ipsmw_variables", "selected_analyses",
  "analysis_label"
)

rm(list = intersect(objects_to_remove, ls()))
gc()

# Close logs
picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of the compute_ipsmw script.")
