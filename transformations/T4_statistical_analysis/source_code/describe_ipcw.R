# ---------------------------------------------------------------
# Step: describe_ipcw
#
# Input:
#   D4_StudyCohortCovariatesMain/Supplementary.parquet
#   D4_StudyCohort_IPCW_Main/Supplementary.parquet
#   D5_Censor_Coef_IPCW_Main/Supplementary.csv
#
# Output:
#   D5_ASD_PREWEIGHT_IPCW_Main/Supplementary_Estimand1/2.csv
#   D5_ASD_POSTWEIGHT_IPCW_Main/Supplementary_Estimand1/2.csv
#   Figure3_IPCW_Main/Supplementary_Estimand1/2.png
#   D5_weight_description_IPCW_Main/Supplementary.csv
#   D5_exp_estimate_table_IPCW_Main/Supplementary.csv
#   D5_Table4_IPCW_Main/Supplementary.csv
#   D6_report/IPCW_density_plot_Main/Supplementary.png
#   D6_report/IPCW_histogram_plot_Main/Supplementary.png
# ---------------------------------------------------------------

if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("describe_ipcw.R")
  lm$start_capturing_prints()
}

# Load configuration utilities (I/O and YAML normalization).
source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "fn_config_utilities.R"
))

# Load weights and diagnostic functions (ASD, Table 4, visualizations).
source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "fn_weights_diagnostics.R"
))

# Load project configuration which specifies which analyses/estimands to process.
config_project <- picard::load_config("configuration/config_project.yaml")
deap_name <- config_project$DEAP_configuration$name
compute_ipcw_config <- config_project$compute_ipcw

## Audit setup ---
picard::audit_start(
  file_name = "describe_ipcw",
  deap_name = deap_name
)
logger::log_info("Start of the describe_ipcw script.")
simulate_d4_config <- config_project$simulate_d4_data
metadata_file <- compute_ipcw_config$metadata$ipcw_variables_file
ipcw_metadata <- data.table::fread(metadata_file)

# Validate selected analyses against allowed values (Main or Supplementary).
selected_analyses <- normalize_vec_config(compute_ipcw_config$analyses)
allowed_analyses <- c("Main", "Supplementary")
invalid_analyses <- setdiff(selected_analyses, allowed_analyses)
if (length(invalid_analyses) > 0) {
  stop(paste0("Invalid analyses value(s): ", paste(invalid_analyses, collapse = ", "), "."))
}

# Validate selected estimands (1 or 2).
selected_estimands <- as.integer(normalize_vec_config(compute_ipcw_config$estimands))
invalid_estimands <- setdiff(selected_estimands, c(1L, 2L))
if (length(invalid_estimands) > 0) {
  stop(paste0("Invalid estimands value(s): ", paste(invalid_estimands, collapse = ", "), "."))
}

# Set input/output directories pointing to compute_ipcw outputs.
input_dir_d4 <- compute_ipcw_config$output$dir_d4
input_dir_d5 <- compute_ipcw_config$output$dir_d5
output_dir_d5 <- compute_ipcw_config$output$dir_d5
cov_file_template <- simulate_d4_config$output$cov_file
metadata_files <- unlist(simulate_d4_config$metadata, use.names = FALSE)

# Iterate over all analysis-estimand combinations to create diagnostic summaries.
for (analysis_label in selected_analyses) {
  # Construct file paths for cohort, follow-up, and covariate data
  cohort_file <- gsub(
    "{analysis}",
    analysis_label,
    compute_ipcw_config$input$cohort_file,
    fixed = TRUE
  )
  tv_file <- gsub(
    "{analysis}",
    analysis_label,
    compute_ipcw_config$input$tv_file,
    fixed = TRUE
  )
  cov_file <- gsub(
    "{analysis}",
    analysis_label,
    compute_ipcw_config$input$cov_file,
    fixed = TRUE
  )

  # Load cohort, follow-up, and covariate data
  d4_cohort <- data.table::as.data.table(picard::load(
    compute_ipcw_config$input$dir_d4,
    cohort_file
  ))[group %in% c("EXPOSED", "CONTROL")]
  d4_tv_cov <- data.table::as.data.table(picard::load(
    compute_ipcw_config$input$dir_d4,
    tv_file
  ))[group %in% c("EXPOSED", "CONTROL")]
  d4_cov <- data.table::as.data.table(picard::load(
    compute_ipcw_config$input$dir_d4,
    cov_file
  ))[group %in% c("EXPOSED", "CONTROL")]

  # Add cov_data$SV_CCI to cohort_data
  cols_to_keep <- intersect(
    names(d4_cov),
    ipcw_metadata$var
  )
  cols_to_keep <- c("person_id", "T0", "match_id", setdiff(cols_to_keep, names(d4_cohort)))
  d4_cohort <- data.table::merge.data.table(
    d4_cohort,
    d4_cov[, ..cols_to_keep, with = FALSE],
    by = c("person_id", "T0", "match_id"),
    all.x = TRUE,
    all.y = FALSE
  )

  # Compute ASD tables before and after weighting for top covariates in model
  pre_asd <- compute_covariate_asd_table(
    covariate_data = d4_cov,
    metadata_files = metadata_files
  )

  # Compute for each analysis-estimand pair
  for (estimand in selected_estimands) {
    logger::log_info(paste("Describing IPCW for", analysis_label, "estimand", estimand))

    estimand_suffix <- paste0("_Estimand", estimand)

    # Check if required input files exist before attempting to load.
    d4_ipcw_file <- paste0("D4_StudyCohort_IPCW_", analysis_label, estimand_suffix, ".parquet")
    d5_censor_coef_file <- paste0("D5_Censor_Coef_IPCW_", analysis_label, estimand_suffix, ".csv")
    d5_model_building_file <- paste0("D5_Censor_ModelBuilding_IPCW_", analysis_label, estimand_suffix, ".csv")

    # Check if main IPCW dataset exists; if not, skip this analysis-estimand pair.
    ipcw_file_path <- file.path(input_dir_d4, d4_ipcw_file)
    if (!file.exists(ipcw_file_path)) {
      logger::log_warn(paste(
        "Skipping IPCW diagnostics for",
        analysis_label,
        "estimand",
        estimand,
        "| Missing input file:",
        d4_ipcw_file,
        "(compute_ipcw may have failed for this analysis)"
      ))
      next
    }

    # Load IPCW-weighted dataset and model coefficients for this analysis-estimand pair.
    d4_ipcw <- data.table::as.data.table(picard::load(
      input_dir_d4,
      d4_ipcw_file
    ))

    d5_censor_coef <- data.table::as.data.table(picard::load(
      input_dir_d5,
      d5_censor_coef_file
    ))

    d5_model_building <- data.table::as.data.table(picard::load(
      input_dir_d5,
      d5_model_building_file
    ))

    # Validate presence of required truncated weight column.
    if (!"ipcw_cum_trim" %in% names(d4_ipcw)) {
      stop(paste0("Missing 'ipcw_cum_trim' in D4 IPCW table for ", analysis_label, " estimand ", estimand, "."))
    }

    # Build post-weight ASD diagnostics from final person-level IPCW weights.
    person_weights <- extract_person_level_ipcw_weights(d4_ipcw)
    post_asd <- compute_covariate_asd_table(
      covariate_data = d4_cov,
      metadata_files = metadata_files,
      weight_data = person_weights,
      weight_col = "ipcw_cum_trim"
    )

    picard::save(
      data = pre_asd,
      file_path = output_dir_d5,
      file_name = paste0("D5_ASD_PREWEIGHT_IPCW_", analysis_label, estimand_suffix, ".csv")
    )

    picard::save(
      data = post_asd,
      file_path = output_dir_d5,
      file_name = paste0("D5_ASD_POSTWEIGHT_IPCW_", analysis_label, estimand_suffix, ".csv")
    )

    # Build Figure 3 comparing top ASD covariates before and after weighting
    fig3 <- build_asd_comparison_plot(pre_asd = pre_asd, post_asd = post_asd, top_n = 15L) +
      ggplot2::ggtitle(paste(deap_name, "Figure 3 IPCW", analysis_label, "Estimand", estimand))

    ggplot2::ggsave(
      filename = paste0(deap_name, "_Figure3_IPCW_", analysis_label, estimand_suffix, ".png"),
      path = output_dir_d5,
      plot = fig3,
      width = 16,
      height = 16,
      units = "cm",
      dpi = 300
    )

    # Calculate 1st and 99th percentiles for weight truncation visualization.
    trunc_lo <- stats::quantile(d4_ipcw$ipcw_cum, probs = 0.01, na.rm = TRUE)
    trunc_hi <- stats::quantile(d4_ipcw$ipcw_cum, probs = 0.99, na.rm = TRUE)

    # Build long-format plotting data for propensity scores.
    # Propensity scores (numerator and denominator models).
    ps_plot_data <- data.table::rbindlist(list(
      d4_ipcw[, .(group, value = p_num, type = "Numerator Model")],
      d4_ipcw[, .(group, value = p_den, type = "Denominator Model")]
    ))
    plot_data_density <- ps_plot_data
    plot_data_density[, type := factor(type, levels = c("Numerator Model", "Denominator Model"))]

    # Create facetted density plot showing propensity scores by treatment group.
    fig_ipcw_density <- ggplot2::ggplot(
      plot_data_density,
      ggplot2::aes(x = value, color = group, fill = group)
    ) +
      ggplot2::scale_fill_manual(values = c(
        "CONTROL" = "dodgerblue3",
        "EXPOSED" = "firebrick3"
      )) +
      ggplot2::scale_color_manual(values = c(
        "CONTROL" = "dodgerblue3",
        "EXPOSED" = "firebrick3"
      )) +
      ggplot2::geom_density(alpha = 0.25, linewidth = 0.8) +
      ggplot2::facet_wrap(~type, scales = "free") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = "Propensity Score (Censoring Probability)",
        y = "Density",
        title = paste("Propensity Score Distribution -", analysis_label, "Estimand", estimand)
      )

    # Save weight distribution visualization.
    ggplot2::ggsave(
      filename = paste0(deap_name, "_Figure4_PSC_density_plot_", analysis_label, estimand_suffix, ".png"),
      path = output_dir_d5,
      plot = fig_ipcw_density,
      width = 18,
      height = 12,
      dpi = 300,
      units = "cm"
    )

    # Create facetted histogram of IPCW weights only.
    plot_data_hist <- data.table::rbindlist(list(
      d4_ipcw[, .(group, weight = ipcw_cum, truncation = "Before truncation")],
      d4_ipcw[, .(group, weight = ipcw_cum_trim, truncation = "After truncation")]
    ))
    plot_data_hist[, truncation := factor(truncation, levels = c("Before truncation", "After truncation"))]

    fig_ipcw_hist <- ggplot2::ggplot(
      plot_data_hist,
      ggplot2::aes(x = weight, fill = group)
    ) +
      ggplot2::geom_histogram(alpha = 0.90, bins = 50) +
      ggplot2::scale_fill_manual(values = c(
        "CONTROL" = "dodgerblue3",
        "EXPOSED" = "firebrick3"
      )) +
      ggplot2::facet_wrap(~truncation, scales = "free") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = "IPCW Weight",
        y = "Count",
        title = paste("IPCW Histogram -", analysis_label, "Estimand", estimand)
      )

    # Save histogram as Figure 5.
    ggplot2::ggsave(
      filename = paste0(deap_name, "_Figure5_IPCW_histogram_plot_", analysis_label, estimand_suffix, ".png"),
      path = output_dir_d5,
      plot = fig_ipcw_hist,
      width = 18,
      height = 12,
      dpi = 300,
      units = "cm"
    )

    # ===== Alternative Figures: Split by Non-Administrative Censoring Status =====
    # Extract censoring status from d4_ipcw and merge with propensity score data.
    if (compute_ipcw_config$plots_by_censoring) {
      logger::log_info(paste(
        "Generating additional plots stratified by non-administrative censoring status for",
        analysis_label,
        "estimand",
        estimand
      ))

      censoring_info <- d4_ipcw[, .(
        person_id,
        T0,
        group,
        censor_nonadmin,
        censoring_status = data.table::fifelse(
          censor_nonadmin == 1L,
          "With Non-Admin Censoring",
          "No Non-Admin Censoring"
        )
      )]

      # Merge propensity score data with censoring status (by row join).
      plot_data_density_by_censor <- ps_plot_data[, censoring_status := rep(
        censoring_info$censoring_status,
        times = length(unique(ps_plot_data$type))
      )]

      # Create facetted density plot of propensity scores by censoring status.
      fig_ipcw_density_by_censor <- ggplot2::ggplot(
        plot_data_density_by_censor,
        ggplot2::aes(x = value, color = censoring_status, fill = censoring_status)
      ) +
        ggplot2::scale_fill_manual(values = c(
          "With Non-Admin Censoring" = "firebrick3",
          "No Non-Admin Censoring" = "dodgerblue3"
        )) +
        ggplot2::scale_color_manual(values = c(
          "With Non-Admin Censoring" = "firebrick3",
          "No Non-Admin Censoring" = "dodgerblue3"
        )) +
        ggplot2::geom_density(alpha = 0.25, linewidth = 0.8) +
        ggplot2::facet_wrap(group ~ type, scales = "free") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          x = "Propensity Score (Censoring Probability)",
          y = "Density",
          title = paste("Propensity Score Distribution by Censoring Status -", analysis_label, "Estimand", estimand)
        )

      # Save density plot with censoring stratification.
      ggplot2::ggsave(
        filename = paste0(deap_name, "_Figure4_PSC_density_by_censoring_", analysis_label, estimand_suffix, ".png"),
        path = output_dir_d5,
        plot = fig_ipcw_density_by_censor,
        width = 18,
        height = 12,
        dpi = 300,
        units = "cm"
      )

      # Create histogram data split by non-administrative censoring.
      plot_data_hist_by_censor_long <- data.table::rbindlist(list(
        d4_ipcw[, .(
          weight = ipcw_cum,
          truncation = "Before truncation",
          group,
          censoring_status = data.table::fifelse(
            censor_nonadmin == 1L,
            "With Non-Admin Censoring",
            "No Non-Admin Censoring"
          )
        )],
        d4_ipcw[, .(
          weight = ipcw_cum_trim,
          truncation = "After truncation",
          group,
          censoring_status = data.table::fifelse(
            censor_nonadmin == 1L,
            "With Non-Admin Censoring",
            "No Non-Admin Censoring"
          )
        )]
      ))
      plot_data_hist_by_censor_long[, truncation := factor(truncation, levels = c("Before truncation", "After truncation"))]

      fig_ipcw_hist_by_censor <- ggplot2::ggplot(
        plot_data_hist_by_censor_long,
        ggplot2::aes(x = weight, fill = censoring_status)
      ) +
        ggplot2::geom_histogram(alpha = 0.75) +
        ggplot2::scale_fill_manual(values = c(
          "With Non-Admin Censoring" = "firebrick3",
          "No Non-Admin Censoring" = "dodgerblue3"
        )) +
        ggplot2::facet_wrap(group ~ truncation, scales = "free") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          x = "IPCW",
          y = "Count",
          title = paste("IPCW Histogram by Censoring Status -", analysis_label, "Estimand", estimand)
        )

      # Save histogram with censoring stratification.
      ggplot2::ggsave(
        filename = paste0(deap_name, "_Figure5_IPCW_histogram_by_censoring_", analysis_label, estimand_suffix, ".png"),
        path = output_dir_d5,
        plot = fig_ipcw_hist_by_censor,
        width = 18,
        height = 12,
        dpi = 300,
        units = "cm"
      )
    }

    # Compute weight descriptive statistics by group.
    weight_table <- describe_iptw(
      p_matrix = d4_ipcw,
      groups = c("CONTROL", "EXPOSED"),
      weight_col = "ipcw_cum_trim",
      custom_truncation = c(
        compute_ipcw_config$truncation$q_lower,
        compute_ipcw_config$truncation$q_upper
      ),
      digits = 4
    )
    weight_table$analysis <- analysis_label
    weight_table$estimand <- estimand

    # Save weight summary table.
    picard::save(
      data = weight_table,
      file_path = output_dir_d5,
      file_name = paste0(deap_name, "_Table5_D5_weight_description_IPCW_", analysis_label, estimand_suffix, ".csv")
    )

    # Extract and format model coefficients as exponentiated estimates (ORs) with confidence intervals.
    exp_estimate_table <- data.table::as.data.table(
      d5_censor_coef[model_type == "denominator" & coefficient != "(Intercept)"]
    )

    # Compute 95% confidence interval bounds for exponentiated coefficients.
    exp_estimate_table[, exp_estimate_lower := exp(estimate + qnorm(0.025) * std_error)]
    exp_estimate_table[, exp_estimate_upper := exp(estimate + qnorm(0.975) * std_error)]
    exp_estimate_table[, OR_ci := paste0(
      round(exp_estimate, 2),
      " (",
      round(exp_estimate_lower, 2),
      "-",
      round(exp_estimate_upper, 2),
      ")"
    )]
    exp_estimate_table[, estimand := estimand]

    # Save coefficients table for reporting.
    picard::save(
      data = exp_estimate_table,
      file_path = output_dir_d5,
      file_name = paste0("D5_exp_estimate_table_IPCW_", analysis_label, estimand_suffix, ".csv")
    )

    # Build Table 4 content by combining prevalence with denominator-model ORs.
    table4_vars <- base::merge(
      x = d5_model_building[included_r2 == TRUE, .(variable_id, variable_in_data, domain)],
      y = ipcw_metadata[, .(variable_id = var, label)],
      by = "variable_id",
      all.x = TRUE
    )
    table4_vars <- unique(table4_vars)
    table4_vars[, source_name := data.table::fifelse(
      domain %in% c("TIMEVARYING", "BOTH"),
      "TIMEVARYING",
      "BASELINE"
    )]

    table4_ipcw <- build_table4_prevalence_or(
      variable_map = table4_vars,
      coef_table = exp_estimate_table,
      source_lookup = list(
        BASELINE = d4_cohort,
        TIMEVARYING = d4_tv_cov
      ),
      prevalence_digits = 1,
      or_digits = 2
    )

    # Convert to data.table and add analysis/estimand metadata only if results exist.
    table4_ipcw <- data.table::as.data.table(table4_ipcw)
    if (nrow(table4_ipcw) > 0) {
      table4_ipcw[, analysis := analysis_label]
      table4_ipcw[, estimand := estimand]

      picard::save(
        data = table4_ipcw,
        file_path = output_dir_d5,
        file_name = paste0(deap_name, "_Table4_D5_IPCW_", analysis_label, estimand_suffix, ".csv")
      )
    } else {
      logger::log_warn(paste(
        "Table 4 for",
        analysis_label,
        "estimand",
        estimand,
        "is empty (no included covariates with prevalence/OR estimates)"
      ))
    }
  }
}

# Clean the environment
objects_to_remove <- c(
  "d4_cohort", "d4_tv_cov", "d4_cov",
  "pre_asd", "post_asd", "fig3", "plot_data_density", "plot_data_hist",
  "plot_data_density_by_censor", "plot_data_hist_by_censor_long", "censoring_info",
  "d4_ipcw", "d5_censor_coef", "d5_model_building",
  "person_weights", "exp_estimate_table",
  "fig_ipcw_density", "fig_ipcw_hist", "fig_ipcw_density_by_censor", "fig_ipcw_hist_by_censor",
  "weight_table", "trunc_lo", "trunc_hi",
  "table4_vars", "table4_ipcw", "ps_plot_data",
  "analysis_label", "estimand", "estimand_suffix",
  "ipcw_metadata", "selected_analyses", "selected_estimands"
)

rm(list = intersect(objects_to_remove, ls()))
gc()

# Close logs
picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of the describe_ipcw script.")
