#' Compute IPSMW objects for one analysis cohort
#'
#' Implements the principal-stratum weighting workflow for one analysis arm:
#' 1) read the precomputed compliance indicator from follow-up,
#' 2) fit exposed-only logistic model,
#' 3) predict principal stratum scores,
#' 4) compute control IPSMW and estimand-2 inclusion indicator,
#' 5) return person-level outputs, model coefficients, and model-building logs.
#'
#' Data flow and expected dimensions:
#' - Starts from `cohort_data` (person-level) and keeps selected groups only.
#' - Reads `compliant_exposed` from `fup_data` and merges it into cohort rows
#'   using `person_id`, `T0`, `group`.
#' - Fits the compliance model on target-group rows only (default: exposed),
#'   then predicts scores for the full merged cohort table.
#' - The main IPSMW output is person-level (not person-time): typically one row
#'   per retained cohort row after group filtering.
#'
#' Output dimensions:
#' - `D4_StudyCohort_IPSMW`: person-level table, usually close to filtered
#'   `cohort_data` in row count.
#' - `D5_PS_Coef_IPSMW`: one row per coefficient in the final logistic model.
#' - `D5_PS_ModelBuilding_IPSMW`: one row per variable in `ps_variables` with
#'   inclusion/exclusion diagnostics.
#'
#' @param cohort_data Data frame/data.table with study cohort rows.
#' @param fup_data Data frame/data.table with follow-up variables.
#' @param ps_variables Data table from metadata defining model covariates.
#' @param analysis_label Character scalar identifying analysis arm.
#' @param se_threshold Numeric threshold for large standard errors in Round 1.
#' @param compliance_col Character name of the precomputed compliance indicator
#'   in fup_data. Default is "compliant_exposed".
#' @param group_levels Character vector of group levels to include (default: c("EXPOSED", "CONTROL")).
#' @param target_group Character scalar specifying which group to model for compliance (default: "EXPOSED").
#' @param use_speedglm Logical scalar. If TRUE, use speedglm for faster model fitting; if FALSE, use standard glm (default: TRUE).
#' @param save_diagnostics Logical scalar. If TRUE, save residual and quasi-separation diagnostics to the diagnostics path (default: FALSE).
#' @param diagnostics_path Character scalar specifying the directory path to save PSM diagnostics. If NULL and save_diagnostics=TRUE, defaults to "data/D5_results/PSM_diagnostics".
#'
#' @return Named list with D4 IPSMW table, D5 coefficient table, D5 model
#'   building table, and updated FUP table containing compliant_exposed.
#'
#' @noRd
compute_ipsmw <- function(
  cohort_data,
  fup_data,
  ps_variables,
  analysis_label,
  se_threshold = 1e+06,
  compliance_col = "compliant_exposed",
  group_levels = c("EXPOSED", "CONTROL"),
  target_group = "EXPOSED",
  primary_method = "speedglm",
  ps_trunc_bounds = c(0.01, 0.99),
  save_diagnostics = FALSE,
  diagnostics_path = NULL
) {
  # Normalize to data.table for consistent by-reference updates.
  cohort_dt <- data.table::as.data.table(cohort_data)
  fup_dt <- data.table::as.data.table(fup_data)

  # Keep only rows matching specified group levels.
  cohort_dt <- cohort_dt[group %in% group_levels]

  # Validate required join keys in FUP data.
  merge_keys <- c("person_id", "T0", "group")
  missing_keys <- setdiff(merge_keys, names(fup_dt))
  if (length(missing_keys) > 0) {
    stop(
      paste0(
        "FUP data for ",
        analysis_label,
        " is missing key columns: ",
        paste(missing_keys, collapse = ", ")
      )
    )
  }

  # Validate required compliance variable used as the exposed principal-stratum label.
  if (!(compliance_col %in% names(fup_dt))) {
    stop(
      paste0(
        "FUP data for ",
        analysis_label,
        " must contain column '",
        compliance_col,
        "'."
      )
    )
  }

  # Keep only columns required for compliance merge and rename to the expected label.
  fup_subset <- fup_dt[, c(merge_keys, compliance_col), with = FALSE]
  data.table::setnames(fup_subset, old = compliance_col, new = "compliant_exposed")

  # # Merge compliance indicator into cohort-level modeling data.
  # cohort_model <- base::merge(
  #   cohort_dt,
  #   fup_subset[, .(person_id, T0, group, compliant_exposed)],
  #   by = merge_keys,
  #   all.x = TRUE
  # )
  # Left join: keep all cohort rows, add compliant_exposed from fup_subset.
  cohort_model <- fup_subset[cohort_dt, on = merge_keys]

  # -----  check for empty cells in compliant exposed -----
  # drop those individuals if empty cells
  # grab categorical variables
  cat_vars <- ps_variables$var[ps_variables$vartype == "CAT"]

  exposed <- cohort_model[group == "EXPOSED"]

  # for testing
  # exposed <- exposed[
  #   !(group == "EXPOSED" &
  #       compliant_exposed == TRUE &
  #       CALENDAR_QUARTER == "2021_Q1")
  # ]

  empty_cells <- lapply(cat_vars, function(v) {
    # Count compliant people among EXPOSED
    tab <- exposed[
      group == "EXPOSED",
      .(
        n_true = sum(compliant_exposed == TRUE, na.rm = TRUE)
      ),
      by = v
    ]

    # Levels where there are no compliant people
    empty <- tab[
      n_true == 0
    ]

    if (nrow(empty) == 0) {
      return(NULL)
    }

    list(
      variable = v,
      table = empty
    )
  })

  # Remove variables without empty cells
  empty_cells <- Filter(Negate(is.null), empty_cells)


  # Log and drop
  if (length(empty_cells) > 0) {
    for (x in empty_cells) {
      v <- x$variable
      tab <- x$table

      for (i in seq_len(nrow(tab))) {
        logger::log_info(
          "No compliant exposed cases: variable={v}, level={tab[[v]][i]}, ",
          "n_true={tab$n_true[i]}"
        )
      }

      # Drop everyone with these problematic levels
      cohort_model <- cohort_model[
        !get(v) %in% tab[[v]]
      ]
    }
  }


  # ---- data preperation ------
  # Normalize shared categorical covariates once on the full merged cohort.
  present_ps <- ps_variables[var %in% names(cohort_model)]
  for (i in seq_len(nrow(present_ps))) {
    v <- present_ps$var[i]
    vtype <- present_ps$vartype[i]
    refcat <- as.character(present_ps$reference_category[i])

    if (vtype %in% c("CAT", "TF")) {
      data.table::set(cohort_model, j = v, value = as.factor(cohort_model[[v]]))
      if (!is.na(refcat) && refcat %in% levels(cohort_model[[v]])) {
        data.table::set(
          cohort_model,
          j = v,
          value = stats::relevel(cohort_model[[v]], ref = refcat)
        )
      }
    }
  }

  # Target-group-only modeling cohort (typically exposed for compliance modeling).
  model_data <- data.table::copy(cohort_model[group == target_group])
  # Drop rows where compliance cannot be evaluated.
  model_data <- model_data[!is.na(compliant_exposed)]
  # Logistic regression expects a numeric binary response.
  model_data[, compliant_exposed_binary := as.integer(compliant_exposed)]

  if (nrow(model_data) == 0) {
    stop(paste0("No exposed rows available for IPSMW model in ", analysis_label, "."))
  }

  # Initialize model-building tracking table from metadata.
  model_building <- data.table::copy(ps_variables[, .(
    variable_id = var,
    var_type = vartype,
    force_in = if ("force_in" %in% names(ps_variables)) force_in else TRUE,
    force_out = if ("force_out" %in% names(ps_variables)) force_out else FALSE
  )])

  # Flag covariates missing entirely in the analysis data.
  model_building[, exists_in_data := variable_id %in% names(model_data)]

  # Compute missingness percentages for candidate variables.
  model_building[, missing_pct := vapply(
    variable_id,
    function(v) {
      if (!(v %in% names(model_data))) {
        return(NA_real_)
      }
      100 * sum(is.na(model_data[[v]])) / nrow(model_data)
    },
    numeric(1)
  )]

  # Compute non-zero variance flags to avoid singular predictors.
  model_building[, nonzero_variance := vapply(
    variable_id,
    function(v) {
      if (!(v %in% names(model_data))) {
        return(FALSE)
      }
      length(unique(model_data[[v]][!is.na(model_data[[v]])])) >= 2
    },
    logical(1)
  )]

  # Round 1 inclusion bookkeeping.
  model_building[, include_r1 := FALSE]
  model_building[, reason_excluded := NA_character_]

  model_building[exists_in_data == FALSE, reason_excluded := "missing_in_data"]
  model_building[nonzero_variance == FALSE & exists_in_data == TRUE, reason_excluded := "zero_variance"]
  model_building[force_out == TRUE, reason_excluded := "force_out"]

  # Initial predictor set must pass data presence/variance checks and not be
  # force-excluded by metadata.
  eligible_vars <- model_building[
    exists_in_data == TRUE & nonzero_variance == TRUE & force_out == FALSE,
    variable_id
  ]

  if (length(eligible_vars) == 0) {
    stop(paste0("No eligible predictors available for IPSMW model in ", analysis_label, "."))
  }

  model_building[variable_id %in% eligible_vars, include_r1 := TRUE]

  # Round 1 model fit.
  logger::log_info(paste(
    "IPSMW model",
    analysis_label,
    "Round 1 with",
    length(eligible_vars),
    "predictors"
  ))

  model_r1 <- fit_ps_model(
    predictors = eligible_vars,
    cohort_covariates = model_data,
    outcome_col = "compliant_exposed_binary",
    primary_method = primary_method
  )

  if (is.null(model_r1)) {
    stop(paste0("Round 1 IPSMW model failed for ", analysis_label, "."))
  }

  # Non-converged models mark all Round-1 variables as problematic.
  if (!isTRUE(model_r1$converged)) {
    model_building[include_r1 == TRUE, reason_excluded := "non_convergence"]
  }

  # Identify unstable variables by large standard errors in coefficient blocks.
  coef_summary_r1 <- as.data.frame(summary(model_r1)$coefficients)
  for (v in eligible_vars) {
    coef_rows <- grep(paste0("^", v), rownames(coef_summary_r1))
    if (length(coef_rows) > 0) {
      max_se <- max(coef_summary_r1[coef_rows, "Std. Error"])
      if (is.finite(max_se) && max_se > se_threshold) {
        model_building[variable_id == v, reason_excluded := "large_se"]
      }
    }
  }

  # Variables flagged in Round 1 are removed and model is re-fit in Round 2.
  problematic_vars <- model_building[
    include_r1 == TRUE & !is.na(reason_excluded),
    variable_id
  ]

  if (length(problematic_vars) > 0) {
    r2_predictors <- setdiff(eligible_vars, problematic_vars)
    if (length(r2_predictors) == 0) {
      stop(paste0("All predictors excluded after Round 1 in ", analysis_label, "."))
    }

    logger::log_info(paste(
      "IPSMW model",
      analysis_label,
      "Round 2 removing:",
      paste(problematic_vars, collapse = ", ")
    ))

    model_final <- fit_ps_model(
      predictors = r2_predictors,
      cohort_covariates = model_data,
      outcome_col = "compliant_exposed_binary",
      primary_method = primary_method
    )

    if (is.null(model_final)) {
      stop(paste0("Round 2 IPSMW model failed for ", analysis_label, "."))
    }

    model_building[, included_r2 := variable_id %in% r2_predictors]
  } else {
    # Use Round 1 fit directly when no issues are identified.
    model_final <- model_r1
    model_building[, included_r2 := include_r1]
  }

  # Extract coefficient diagnostics from final model.
  coef_summary <- as.data.frame(summary(model_final)$coefficients)
  d5_ps_coef <- data.table::data.table(
    coefficient = rownames(coef_summary),
    estimate = coef_summary[, 1],
    std_error = coef_summary[, 2],
    z_score = coef_summary[, 3],
    p_value = coef_summary[, 4],
    exp_estimate = exp(coef_summary[, 1]),
    analysis = analysis_label
  )

  # Score controls only to reduce memory usage for large datasets.
  score_data <- data.table::copy(cohort_model[group == "CONTROL"])

  # Predict principal stratum score for controls and store directly in table.
  score_data[, psm_score_all := stats::predict(model_final, newdata = score_data, type = "response")]

  # # Compute principal-stratum score from linear predictor + inverse-logit.
  # # This avoids predict-method differences across glm/speedglm objects.
  # score_terms <- stats::delete.response(stats::terms(model_final))
  # if (!is.null(model_final$xlevels)) {
  #   score_matrix <- stats::model.matrix(score_terms, data = score_data, xlev = model_final$xlevels)
  # } else {
  #   score_matrix <- stats::model.matrix(score_terms, data = score_data)
  # }

  # model_coef <- stats::coef(model_final)
  # missing_coef <- setdiff(colnames(score_matrix), names(model_coef))
  # if (length(missing_coef) > 0) {
  #   model_coef <- c(model_coef, stats::setNames(rep(0, length(missing_coef)), missing_coef))
  # }

  # model_coef <- model_coef[colnames(score_matrix)]
  # model_coef[is.na(model_coef)] <- 0

  # linear_predictor <- as.numeric(score_matrix %*% model_coef)
  # psm_score <- stats::plogis(linear_predictor)

  eps <- 1e-6
  score_data[, psm_score_all := pmin(pmax(psm_score_all, eps), 1 - eps)]

  # Build person-level IPSMW output for controls.
  d4_controls <- score_data[, .(
    person_id,
    match_id,
    T0,
    group,
    compliant_exposed,
    psm_score_all
  )]

  d4_controls[, analysis := analysis_label]
  d4_controls[, psm_score := psm_score_all]
  d4_controls[, ipsmw_raw := psm_score / (1 - psm_score)]

  # normalize weights to mean 1 within the control group to avoid inflating sample size.
  d4_controls[, ipsmw_norm := ipsmw_raw / mean(ipsmw_raw, na.rm = TRUE)]

  # Add compliant exposed rows with design-based unit weight.
  d4_exposed <- cohort_model[group == "EXPOSED" & compliant_exposed == TRUE, .(
    person_id,
    match_id,
    T0,
    group,
    compliant_exposed,
    psm_score_all = NA_real_,
    analysis = analysis_label,
    psm_score = NA_real_,
    ipsmw_raw = 1,
    ipsmw_norm = 1
  )]

  d4_ipsmw <- data.table::rbindlist(list(d4_controls, d4_exposed), use.names = TRUE)


  # No additional stabilization term requested for IPSMW at this stage.
  d4_ipsmw[, include_estimand2 := TRUE]

  # ---------------------------------------------------------------
  # Apply truncation of the stabilized weights at 1st and 99th percentile
  # ---------------------------------------------------------------

  d4_ipsmw[, ipsmw_raw_trunc := NA_real_]
  d4_ipsmw[, ipsmw_norm_trunc := NA_real_]

  for (grp in c("EXPOSED", "CONTROL")) {
    idx <- d4_ipsmw$group == grp & !is.na(d4_ipsmw$ipsmw_raw) # & d4_ipsmw$include_after_trimming == TRUE
    if (sum(idx) == 0) {
      next
    }

    q <- quantile(d4_ipsmw[idx, ipsmw_raw], probs = ps_trunc_bounds)
    d4_ipsmw[
      idx,
      # If stabilized IPSMW less than 1st percentile set to 1st percentile
      ipsmw_raw_trunc := data.table::fifelse(
        ipsmw_raw < q[1],
        q[1],
        # on the other end set to 99th percentile if larger than 99th
        data.table::fifelse(ipsmw_raw > q[2], q[2], ipsmw_raw)
      )
    ]
    d4_ipsmw[
      idx,
      # If normalized IPSMW less than 1st percentile set to 1st percentile
      ipsmw_norm_trunc := data.table::fifelse(
        ipsmw_norm < q[1],
        q[1],
        # on the other end set to 99th percentile if larger than 99th
        data.table::fifelse(ipsmw_norm > q[2], q[2], ipsmw_norm)
      )
    ]

    logger::log_info(paste0(
      "Truncation ",
      grp,
      " -- weights truncated to [",
      round(q[1], 4),
      ", ",
      round(q[2], 4),
      "] ",
      "(", paste0("P", ps_trunc_bounds[1] * 100, "-P", ps_trunc_bounds[2] * 100), ")"
    ))
  }

  # Assemble model-building output table.
  d5_model_building <- model_building[, .(
    variable_id,
    var_type,
    force_in,
    force_out,
    exists_in_data,
    missing_pct,
    nonzero_variance,
    include_r1,
    included_r2,
    reason_excluded
  )]
  d5_model_building[, analysis := analysis_label]

  # ---------------------------------------------------------------
  # Build D4_StudyCohort_IPSMW_ALL: includes all exposed and all controls
  # with PSM and IPSMW scores computed for the full population
  # ---------------------------------------------------------------

  # Score ALL exposed participants (not just compliant)
  score_exposed_all <- data.table::copy(cohort_model[group == "EXPOSED"])
  score_exposed_all[, psm_score_all := stats::predict(model_final, newdata = score_exposed_all, type = "response")]

  eps <- 1e-6
  score_exposed_all[, psm_score_all := pmin(pmax(psm_score_all, eps), 1 - eps)]

  # Build person-level IPSMW output for ALL exposed.
  d4_exposed_all <- score_exposed_all[, .(
    person_id,
    match_id,
    T0,
    group,
    compliant_exposed,
    psm_score_all
  )]

  d4_exposed_all[, analysis := analysis_label]
  d4_exposed_all[, psm_score := psm_score_all]
  d4_exposed_all[, ipsmw_raw := psm_score / (1 - psm_score)]

  # normalize weights to mean 1 within the exposed group to avoid inflating sample size.
  d4_exposed_all[, ipsmw_norm := ipsmw_raw / mean(ipsmw_raw, na.rm = TRUE)]

  # For controls, reuse score_data that was already computed
  d4_controls_all <- data.table::copy(score_data)
  d4_controls_all <- d4_controls_all[, .(
    person_id,
    match_id,
    T0,
    group,
    compliant_exposed,
    psm_score_all
  )]

  d4_controls_all[, analysis := analysis_label]
  d4_controls_all[, psm_score := psm_score_all]
  d4_controls_all[, ipsmw_raw := psm_score / (1 - psm_score)]

  # normalize weights to mean 1 within the control group to avoid inflating sample size.
  d4_controls_all[, ipsmw_norm := ipsmw_raw / mean(ipsmw_raw, na.rm = TRUE)]

  # Combine all exposed and all controls
  d4_ipsmw_all <- data.table::rbindlist(list(d4_controls_all, d4_exposed_all), use.names = TRUE)

  # No additional stabilization term requested for IPSMW at this stage.
  d4_ipsmw_all[, include_estimand2 := TRUE]

  # ---------------------------------------------------------------
  # Apply truncation of the stabilized weights at 1st and 99th percentile
  # ---------------------------------------------------------------

  d4_ipsmw_all[, ipsmw_raw_trunc := NA_real_]
  d4_ipsmw_all[, ipsmw_norm_trunc := NA_real_]

  for (grp in c("EXPOSED", "CONTROL")) {
    idx <- d4_ipsmw_all$group == grp & !is.na(d4_ipsmw_all$ipsmw_raw)
    if (sum(idx) == 0) {
      next
    }

    q <- quantile(d4_ipsmw_all[idx, ipsmw_raw], probs = ps_trunc_bounds)
    d4_ipsmw_all[
      idx,
      ipsmw_raw_trunc := data.table::fifelse(
        ipsmw_raw < q[1],
        q[1],
        data.table::fifelse(ipsmw_raw > q[2], q[2], ipsmw_raw)
      )
    ]
    d4_ipsmw_all[
      idx,
      ipsmw_norm_trunc := data.table::fifelse(
        ipsmw_norm < q[1],
        q[1],
        data.table::fifelse(ipsmw_norm > q[2], q[2], ipsmw_norm)
      )
    ]
  }

  # ---------------------------------------------------------------
  # Save residual diagnostics and quasi-separation detection if requested
  # ---------------------------------------------------------------
  if (save_diagnostics) {
    if (is.null(diagnostics_path)) {
      diagnostics_path <- "data/D5_results/PSM_diagnostics"
    }

    # Create diagnostics directory if it doesn't exist
    if (!dir.exists(diagnostics_path)) {
      dir.create(diagnostics_path, recursive = TRUE)
    }

    # Extract residual diagnostics from the final model fitted on exposed data
    residual_diag <- data.table::data.table(
      obs_index = seq_len(nrow(model_data)),
      person_id = model_data$person_id,
      compliant_exposed = model_data$compliant_exposed_binary,
      fitted_prob = stats::fitted(model_final),
      deviance_residual = stats::residuals(model_final, type = "deviance"),
      pearson_residual = stats::residuals(model_final, type = "pearson"),
      std_residual = stats::residuals(model_final, type = "response")
    )

    # Add leverage (hat values) and Cook's distance
    # Note: hatvalues() and cooks.distance() only work with glm, not speedglm
    if (inherits(model_final, "glm") && !inherits(model_final, "speedglm")) {
      hat_vals <- stats::hatvalues(model_final)
      residual_diag[, leverage := hat_vals]

      # Calculate Cook's distance
      cooks_d <- stats::cooks.distance(model_final)
      residual_diag[, cooks_distance := cooks_d]
    } else {
      # For speedglm or other model types, set to NA
      residual_diag[, leverage := NA_real_]
      residual_diag[, cooks_distance := NA_real_]
    }

    # Save residual diagnostics
    res_diag_file <- file.path(diagnostics_path, paste0(analysis_label, "_residual_diagnostics.csv"))
    data.table::fwrite(residual_diag, file = res_diag_file)
    logger::log_info(paste("Saved residual diagnostics to", res_diag_file))

    # ---------------------------------------------------------------
    # Quasi-separation detection
    # ---------------------------------------------------------------
    coef_table <- as.data.frame(summary(model_final)$coefficients)
    separation_diag <- data.table::data.table(
      coefficient = rownames(coef_table),
      estimate = coef_table[, 1],
      std_error = coef_table[, 2],
      z_score = coef_table[, 3],
      p_value = coef_table[, 4]
    )

    # Flag coefficients with very large standard errors (sign of separation)
    separation_diag[, large_se_flag := is.infinite(std_error) | std_error > se_threshold]
    separation_diag[, infinite_estimate := is.infinite(estimate)]

    # Check for extreme predicted probabilities (near 0 or 1)
    extreme_pred <- data.table::data.table(
      metric = c(
        "min_fitted_prob", "max_fitted_prob", "n_fitted_prob_below_0.01",
        "n_fitted_prob_above_0.99", "n_perfect_separation"
      ),
      value = c(
        min(residual_diag$fitted_prob, na.rm = TRUE),
        max(residual_diag$fitted_prob, na.rm = TRUE),
        sum(residual_diag$fitted_prob < 0.01, na.rm = TRUE),
        sum(residual_diag$fitted_prob > 0.99, na.rm = TRUE),
        sum(residual_diag$fitted_prob %in% c(0, 1), na.rm = TRUE)
      )
    )

    # Save separation diagnostics
    sep_diag_file <- file.path(diagnostics_path, paste0(analysis_label, "_separation_diagnostics.csv"))
    data.table::fwrite(separation_diag, file = sep_diag_file)
    logger::log_info(paste("Saved quasi-separation diagnostics to", sep_diag_file))

    # Save extreme prediction summary
    extreme_file <- file.path(diagnostics_path, paste0(analysis_label, "_extreme_predictions.csv"))
    data.table::fwrite(extreme_pred, file = extreme_file)
    logger::log_info(paste("Saved extreme prediction summary to", extreme_file))

    # ---------------------------------------------------------------
    # Generate and save residual plots
    # ---------------------------------------------------------------
    tryCatch(
      {
        # Create a composite plot with ggplot2 if available
        if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("gridExtra", quietly = TRUE)) {
          # Convert residual diagnostics to data frame for plotting
          plot_data <- as.data.frame(residual_diag)

          # 1. Residuals vs Fitted values
          p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted_prob, y = deviance_residual)) +
            ggplot2::geom_point(alpha = 0.5, size = 2) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = paste("Residuals vs Fitted -", analysis_label),
              x = "Fitted Probabilities",
              y = "Deviance Residuals"
            )

          # 2. Q-Q plot for normality assessment
          p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = std_residual)) +
            ggplot2::stat_qq(alpha = 0.5, size = 2) +
            ggplot2::stat_qq_line(color = "red", linetype = "dashed") +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = paste("Q-Q Plot -", analysis_label),
              x = "Theoretical Quantiles",
              y = "Sample Quantiles"
            )

          # 3. Histogram of residuals
          p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = deviance_residual)) +
            ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
            ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = paste("Distribution of Residuals -", analysis_label),
              x = "Deviance Residuals",
              y = "Frequency"
            )

          # 4. Residuals by outcome (compliance status)
          plot_data$outcome_label <- ifelse(plot_data$compliant_exposed == 1, "Compliant", "Non-Compliant")
          p4 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = outcome_label, y = deviance_residual)) +
            ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7) +
            ggplot2::geom_jitter(alpha = 0.3, width = 0.2) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = paste("Residuals by Outcome -", analysis_label),
              x = "Compliance Status",
              y = "Deviance Residuals"
            )

          # Combine plots into a grid
          combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

          # Save combined plot
          plot_file <- file.path(diagnostics_path, paste0(analysis_label, "_residual_plots.pdf"))
          ggplot2::ggsave(
            filename = plot_file,
            plot = combined_plot,
            width = 12,
            height = 10,
            dpi = 300
          )
          logger::log_info(paste("Saved residual plots to", plot_file))
        } else {
          # Fallback to base R plotting if ggplot2 not available
          plot_file <- file.path(diagnostics_path, paste0(analysis_label, "_residual_plots.pdf"))
          grDevices::pdf(file = plot_file, width = 12, height = 10)

          par(mfrow = c(2, 2))

          # 1. Residuals vs Fitted
          plot(residual_diag$fitted_prob, residual_diag$deviance_residual,
            main = paste("Residuals vs Fitted -", analysis_label),
            xlab = "Fitted Probabilities",
            ylab = "Deviance Residuals",
            pch = 16, alpha = 0.5
          )
          abline(h = 0, col = "red", lty = 2)

          # 2. Q-Q plot
          stats::qqnorm(residual_diag$std_residual,
            main = paste("Q-Q Plot -", analysis_label),
            pch = 16
          )
          stats::qqline(residual_diag$std_residual, col = "red", lty = 2)

          # 3. Histogram of residuals
          hist(residual_diag$deviance_residual,
            main = paste("Distribution of Residuals -", analysis_label),
            xlab = "Deviance Residuals",
            ylab = "Frequency",
            breaks = 30,
            col = "steelblue"
          )
          abline(v = 0, col = "red", lty = 2)

          # 4. Scale-Location plot
          plot(residual_diag$fitted_prob, sqrt(abs(residual_diag$std_residual)),
            main = paste("Scale-Location -", analysis_label),
            xlab = "Fitted Probabilities",
            ylab = "sqrt(|Standardized Residuals|)",
            pch = 16
          )

          grDevices::dev.off()
          logger::log_info(paste("Saved residual plots (base R) to", plot_file))
        }
      },
      error = function(e) {
        logger::log_warn(paste("Failed to create residual plots:", conditionMessage(e)))
      }
    )
  }

  return(list(
    D4_StudyCohort_IPSMW = d4_ipsmw,
    D4_StudyCohort_IPSMW_ALL = d4_ipsmw_all,
    D5_PS_Coef_IPSMW = d5_ps_coef,
    D5_PS_ModelBuilding_IPSMW = d5_model_building
  ))
}
