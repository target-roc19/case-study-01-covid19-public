# ---------------------------------------------------------------
# Step: compute_ipcw
# Description:
#   Compute stabilised inverse probability of censoring weights (IPCW) for
#   each analysis and estimand based on the D4 cohort, follow-up, and
#   time-varying covariate data.
#
# Input:
#   D4_StudyCohort_Main/Supplementary(.parquet)
#   D4_StudyCohortFupMain/Supplementary(.parquet)
#   D4_StudyCohortTimeVaryingCovariatesMain/Supplementary(.parquet)
#   metadata/ipcw_variables.csv
#
# Output:
#   D4_StudyCohort_IPCW_Main/Supplementary.parquet
#   D5_Censor_Coef_IPCW_Main/Supplementary.csv
#   D5_Censor_ModelBuilding_IPCW_Main/Supplementary.csv
#   D5_Censor_by_Time_Main/Supplementary_Estimand1/2.png
# ---------------------------------------------------------------

if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("compute_ipcw.R")
  lm$start_capturing_prints()
}

# Load required libraries
library(ggplot2)

## Source functions ---
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
  "fn_compute_ipcw.R"
))

source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "fn_plot_censor_by_time.R"
))

# Load configuration utilities (I/O and YAML normalization).
source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "fn_config_utilities.R"
))

## Load project config ---
# Load project configuration which contains file paths, parameters, and selections.
config_project <- picard::load_config("configuration/config_project.yaml")
deap_name <- config_project$DEAP_configuration$name
compute_ipcw_config <- config_project$compute_ipcw

## Audit setup ---
picard::audit_start(
  file_name = "compute_ipcw",
  deap_name = config_project$DEAP_configuration$name
)
logger::log_info("Start of the compute_ipcw script.")

# Validate selected analyses against allowed values (Main or Supplementary).
selected_analyses <- normalize_vec_config(compute_ipcw_config$analyses)
allowed_analyses <- c("Main", "Supplementary")
invalid_analyses <- setdiff(selected_analyses, allowed_analyses)
if (length(invalid_analyses) > 0) {
  stop(paste0("Invalid analyses value(s): ", paste(invalid_analyses, collapse = ", "), "."))
}

# Validate selected estimands against allowed values (1 or 2).
selected_estimands <- as.integer(normalize_vec_config(compute_ipcw_config$estimands))
invalid_estimands <- setdiff(selected_estimands, c(1L, 2L))
if (length(invalid_estimands) > 0) {
  stop(paste0("Invalid estimands value(s): ", paste(invalid_estimands, collapse = ", "), "."))
}

## Set input/output paths and parameters from config ---
# Extract input/output configuration paths and parameters from config section.
input_dir_d4 <- compute_ipcw_config$input$dir_d4
cohort_file_template <- compute_ipcw_config$input$cohort_file
fup_file_template <- compute_ipcw_config$input$fup_file
tv_file_template <- compute_ipcw_config$input$tv_file
cov_file_template <- compute_ipcw_config$input$cov_file

metadata_file <- compute_ipcw_config$metadata$ipcw_variables_file
q_lower <- as.numeric(compute_ipcw_config$truncation$q_lower)
q_upper <- as.numeric(compute_ipcw_config$truncation$q_upper)
prediction_eps <- as.numeric(compute_ipcw_config$prediction_eps)
fit_groups <- compute_ipcw_config$fit_groups # "exposed", "control", or "both"

output_dir_d4 <- compute_ipcw_config$output$dir_d4
output_dir_d5 <- compute_ipcw_config$output$dir_d5

# Load IPCW variable metadata that controls model covariate selection and factor handling.
ipcw_variables <- data.table::fread(metadata_file, sep = ";")

# Normalize metadata fields expected by this script.
ipcw_variables[, domain := toupper(domain)]

#-----------------------------------------------------#
# Compute IPCW for each selected analysis and estimand.
#-----------------------------------------------------#
for (analysis_label in selected_analyses) {
  logger::log_info(paste("Computing IPCW for", analysis_label))

  # Construct input file paths by substituting {analysis} placeholder.
  cohort_file <- gsub("\\{analysis\\}", analysis_label, cohort_file_template)
  fup_file <- gsub("\\{analysis\\}", analysis_label, fup_file_template)
  tv_file <- gsub("\\{analysis\\}", analysis_label, tv_file_template)
  cov_file <- gsub("\\{analysis\\}", analysis_label, cov_file_template)

  # Load input data for this analysis.
  cohort_data <- picard::load(input_dir_d4, cohort_file)[group %in% c("EXPOSED", "CONTROL")]
  fup_data <- picard::load(input_dir_d4, fup_file)[group %in% c("EXPOSED", "CONTROL")]
  tv_data <- picard::load(input_dir_d4, tv_file)[group %in% c("EXPOSED", "CONTROL")]
  cov_data <- picard::load(input_dir_d4, cov_file)[group %in% c("EXPOSED", "CONTROL")]

  # Compute IPCW weights for each configured estimand.
  for (estimand in selected_estimands) {
    logger::log_info(paste("Computing IPCW for", analysis_label, "estimand", estimand))

    cohort_data_est <- cohort_data
    fup_data_est <- fup_data
    tv_data_est <- tv_data
    cov_data_est <- cov_data

    # Add cov_data_est$SV_CCI to cohort_data_est
    cols_to_keep <- intersect(
      names(cov_data_est),
      ipcw_variables$var
    )
    cols_to_keep <- c("person_id", "T0", "match_id", setdiff(cols_to_keep, names(cohort_data_est)))
    cohort_data_est <- data.table::merge.data.table(
      cohort_data_est,
      cov_data_est[, ..cols_to_keep, with = FALSE],
      by = c("person_id", "T0", "match_id"),
      all.x = TRUE,
      all.y = FALSE
    )

    # SAP estimand 2 population: keep compliant exposed and all controls.
    if (estimand == 2L) {
      key_cols <- c("person_id", "T0", "group", "match_id")
      eligible_ids <- unique(fup_data[group == "CONTROL" | (group == "EXPOSED" & compliant_exposed), ..key_cols])

      cohort_data_est <- cohort_data[eligible_ids, on = key_cols, nomatch = 0L]
      fup_data_est <- fup_data[eligible_ids, on = key_cols, nomatch = 0L]
      tv_data_est <- tv_data[eligible_ids, on = key_cols, nomatch = 0L]

      logger::log_info(paste(
        "Applied estimand 2 restriction for",
        analysis_label,
        "| cohort rows:", nrow(cohort_data_est),
        "| fup rows:", nrow(fup_data_est),
        "| tv rows:", nrow(tv_data_est),
        "| cov rows:", nrow(cov_data_est)
      ))
    }

    # Create estimand-specific suffix for output files.
    estimand_suffix <- paste0("_Estimand", estimand)

    # Plot proportion of non-administratively censored by time and group.
    # This plot helps determine appropriate polynomial order for time.
    if (compute_ipcw_config$plot_censor_by_time) {
      plot_result <- plot_censor_by_time(
        tv_data = tv_data_est,
        time_col = "time_days",
        censor_col = "censor_nonadmin",
        group_levels = c("EXPOSED", "CONTROL"),
        analysis_label = analysis_label,
        estimand = estimand,
        deap_name = deap_name
      )

      # Save censoring by time plot.
      ggplot2::ggsave(
        filename = paste0("D5_Censor_by_Time_", analysis_label, estimand_suffix, ".png"),
        plot = plot_result$plot,
        path = output_dir_d5,
        width = 10,
        height = 6,
        dpi = 300
      )

      logger::log_info(paste(
        "Saved censoring by time plot for",
        analysis_label,
        "estimand",
        estimand
      ))
    }

    # Core IPCW computation with error handling.
    tryCatch(
      {
        res <- compute_ipcw(
          cohort_data = cohort_data_est,
          fup_data = fup_data_est,
          tv_data = tv_data_est,
          ps_variables = ipcw_variables,
          analysis_label = analysis_label,
          estimand = estimand,
          q_lower = q_lower,
          q_upper = q_upper,
          prediction_eps = prediction_eps,
          primary_method = "biglm",
          fit_groups = fit_groups # or "exposed" or "both"
        )

        # Persist person-level IPCW dataset with weights and diagnostics.
        picard::save(
          data = res$D4_StudyCohort_IPCW,
          file_path = output_dir_d4,
          file_name = paste0("D4_StudyCohort_IPCW_", analysis_label, estimand_suffix, ".parquet")
        )

        # Persist model coefficients from numerator and denominator models.
        picard::save(
          data = res$D5_Censor_Coef_IPCW,
          file_path = output_dir_d5,
          file_name = paste0("D5_Censor_Coef_IPCW_", analysis_label, estimand_suffix, ".csv")
        )

        # Persist variable screening and model-building diagnostics.
        picard::save(
          data = res$D5_Censor_ModelBuilding_IPCW,
          file_path = output_dir_d5,
          file_name = paste0("D5_Censor_ModelBuilding_IPCW_", analysis_label, estimand_suffix, ".csv")
        )

        logger::log_info(paste(
          "Finished",
          analysis_label,
          "estimand",
          estimand,
          "| IPCW rows:",
          nrow(res$D4_StudyCohort_IPCW)
        ))
      },
      error = function(e) {
        logger::log_warn(paste(
          "IPCW computation failed for",
          analysis_label,
          "estimand",
          estimand,
          "| Error:",
          conditionMessage(e)
        ))
      }
    )
  }
}

# Clean the environment
objects_to_remove <- c(
  "cohort_data", "fup_data", "tv_data", "cohort_data_est", "fup_data_est", "tv_data_est", "res",
  "ipcw_variables", "selected_analyses", "selected_estimands",
  "analysis_label", "estimand", "estimand_suffix", "eligible_ids", "key_cols", "plot_result"
)


rm(list = intersect(objects_to_remove, ls()))
gc()

# Close logs
picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of the compute_ipcw script.")
