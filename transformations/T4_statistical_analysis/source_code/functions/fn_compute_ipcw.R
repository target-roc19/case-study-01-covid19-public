#' Build IPCW outputs for one analysis arm
#'
#' This function constructs a person-time analysis table for IPCW model fitting,
#' then returns stabilisedweighted person-time output plus model diagnostics.
#'
#' Data flow and expected dimensions:
#' - Starts from `fup_data` and `tv_data` using an inner join on
#'   `person_id`, `T0`, `group` (and `match_id` if present).
#' - Adds baseline covariates from `cohort_data` via a left join.
#' - Restricts records to `time <= follow_up_days` for the selected estimand.
#' - Resulting modeling table is therefore long-format (person-time), typically
#'   close to `tv_data` in row count, but never larger than the joined
#'   `fup_data` x `tv_data` key intersection after filtering.
#'
#' Output dimensions:
#' - `D4_StudyCohort_IPCW`: one row per retained person-time record in the
#'   long analysis table.
#' - `D5_Censor_Coef_IPCW`: one row per fitted coefficient across numerator and
#'   denominator censoring models.
#' - `D5_Censor_ModelBuilding_IPCW`: one row per variable in `ps_variables`
#'   with inclusion/exclusion diagnostics.
#'
#' @param cohort_data Data frame/data.table with one row per person.
#' @param fup_data Data frame/data.table with follow-up summaries.
#' @param tv_data Data frame/data.table with time-varying covariates.
#' @param ps_variables Data table with metadata-driven variable control.
#' @param analysis_label Character scalar identifying analysis arm.
#' @param estimand Integer scalar estimand index (1 or 2).
#' @param q_lower Numeric lower quantile for truncation.
#' @param q_upper Numeric upper quantile for truncation.
#' @param prediction_eps Numeric clipping epsilon for predicted probabilities.
#' @param se_threshold Numeric threshold for large standard errors in
#' denominator model diagnostics.
#' @param group_levels Character vector of group levels to include
#' (default: c("EXPOSED", "CONTROL")).
#' @param reference_group Character scalar specifying reference level for
#' group_factor (default: "CONTROL").
#' @param time_col Character scalar or NULL. Column name for time in tv_data.
#' If NULL, defaults to "time_days".
#' @param censor_tv_col Character scalar or NULL. Column name for censoring
#' indicator in tv_data. If NULL, defaults to "censor_nonadmin".
#' @param event_col Character scalar or NULL. Column name for event count in
#' fup_data. If NULL, defaults to paste0("event_count_est", estimand).
#' @param fup_days_col Character scalar or NULL. Column name for follow-up
#' length in fup_data. If NULL, defaults to paste0("fup_length_days_est",
#' estimand).
#' @param time_terms Character vector specifying time polynomial terms
#' (default: c("linear", "quadratic")). Use c("linear") for linear-only,
#' c("linear", "quadratic") for polynomial.
#' @param primary_method Character scalar. Choice of primary fast method:
#'   "speedglm" (default) or "biglm". Ignored if use_speedglm = FALSE.
#' @param fit_groups Character scalar or vector specifying which group(s) to fit the
#' censoring model on. Options: "both" (default, fit on all data), "exposed",
#' "control", or c("exposed", "control"). Groups without a fitted model receive
#' weight = 1.0 (no adjustment).
#'
#' @return List with D4 IPCW table, D5 coefficients, and model-building table.
#'
#' @noRd
compute_ipcw <- function(
  cohort_data,
  fup_data,
  tv_data,
  ps_variables,
  analysis_label,
  estimand = 2L,
  q_lower = 0.01,
  q_upper = 0.99,
  prediction_eps = 1e-6,
  se_threshold = 1e+06,
  group_levels = c("EXPOSED", "CONTROL"),
  reference_group = "CONTROL",
  time_col = NULL,
  censor_tv_col = NULL,
  event_col = NULL,
  fup_days_col = NULL,
  time_terms = c("linear", "quadratic"),
  primary_method = "biglm",
  fit_groups = "both" # or "exposed" or "control"
) {
  # Normalize all input data to data.table for consistent by-reference updates.
  cohort_dt <- data.table::as.data.table(cohort_data)
  fup_dt <- data.table::as.data.table(fup_data)
  tv_dt <- data.table::as.data.table(tv_data)

  # Keep only rows matching specified group levels.
  cohort_dt <- cohort_dt[group %in% group_levels]

  # Define join keys for merging follow-up and time-varying data.
  key_cols <- c("person_id", "T0", "group")
  key_cols <- key_cols[key_cols %in% names(cohort_dt)]
  if (!"person_id" %in% key_cols || !"T0" %in% key_cols || !"group" %in% key_cols) {
    stop(paste0("Missing one or more key columns in cohort data for ", analysis_label, "."))
  }

  # Optional match_id for matched pair identification.
  if ("match_id" %in% names(cohort_dt)) {
    key_cols <- c(key_cols, "match_id")
  }

  # Validate estimand parameter.
  estimand <- as.integer(estimand)
  if (!(estimand %in% c(1L, 2L))) {
    stop("estimand must be 1 or 2.")
  }

  # Resolve column names: use provided values or fall back to estimand-specific defaults.
  if (is.null(time_col)) time_col <- "time_days"
  if (is.null(censor_tv_col)) censor_tv_col <- "censor_nonadmin"
  if (is.null(event_col)) event_col <- paste0("event_count_est", estimand)
  if (is.null(fup_days_col)) fup_days_col <- paste0("fup_length_days_est", estimand)

  # Validate that all required estimand-specific columns exist in input data.
  if (!(time_col %in% names(tv_dt))) {
    stop(paste0("Missing required column in tv_data: ", time_col, "."))
  }
  if (!(censor_tv_col %in% names(tv_dt))) {
    stop(paste0("Missing required column in tv_data: ", censor_tv_col, "."))
  }
  if (!(event_col %in% names(fup_dt))) {
    stop(paste0("Missing required column in fup_data: ", event_col, "."))
  }
  if (!(fup_days_col %in% names(fup_dt))) {
    stop(paste0("Missing required column in fup_data: ", fup_days_col, "."))
  }

  logger::log_info(paste("IPCW", analysis_label, "time column:", time_col))
  logger::log_info(paste("IPCW", analysis_label, "censoring column:", censor_tv_col))
  logger::log_info(paste("IPCW", analysis_label, "event/fup columns:", event_col, "/", fup_days_col))

  # Identify which columns to keep from each input dataset.
  fup_keep_cols <- unique(c(key_cols, event_col, fup_days_col))
  tv_keep_cols <- unique(c(key_cols, time_col, censor_tv_col))

  # Select baseline and time-varying covariates per metadata domain classification.
  baseline_vars <- ps_variables[domain %in% c("BASELINE", "BOTH"), var]
  tv_vars <- ps_variables[domain %in% c("TIMEVARYING", "BOTH"), var]
  baseline_vars <- baseline_vars[baseline_vars %in% names(cohort_dt)]
  tv_vars <- unique(c(tv_vars, paste0(tv_vars, "_est", estimand)))
  tv_vars <- tv_vars[tv_vars %in% names(tv_dt)]

  cohort_keep_cols <- unique(c(key_cols, baseline_vars))
  tv_keep_cols <- unique(c(tv_keep_cols, tv_vars))

  # Subset columns from cohort, follow-up, and time-varying data.
  cohort_sub <- cohort_dt[, cohort_keep_cols, with = FALSE]
  fup_sub <- fup_dt[, ..fup_keep_cols]
  tv_sub <- tv_dt[, ..tv_keep_cols]

  # Merge follow-up and time-varying data on key columns (inner join).
  # analysis <- base::merge(
  #   x = fup_sub,
  #   y = tv_sub,
  #   by = key_cols,
  #   all = FALSE
  # )
  analysis <- fup_sub[tv_sub, on = key_cols, nomatch = 0L]

  # Retrieve baseline covariates from D4_StudyCohort.
  # analysis <- base::merge(
  #   x = analysis,
  #   y = cohort_sub,
  #   by = key_cols,
  #   all.x = TRUE,
  #   suffixes = c("", "_cohort")
  # )
  analysis <- cohort_sub[analysis, on = key_cols]

  # Sort by person, baseline date, and time; filter to valid follow-up window.
  data.table::setorderv(analysis, c("person_id", "T0", time_col))
  analysis <- analysis[get(time_col) <= get(fup_days_col)]

  if (nrow(analysis) == 0) {
    stop(paste0("IPCW analysis dataset is empty for ", analysis_label, "."))
  }

  # Convert event and censoring indicators to binary integers.
  analysis[, event_count := as.integer(get(event_col) > 0)]
  analysis[, (censor_tv_col) := as.integer(get(censor_tv_col) > 0)]

  # Define outcome event: reached end of follow-up with event.
  analysis[, outcome_event := 0L]
  analysis[(get(time_col)+1) == get(fup_days_col) & event_count == 1L, outcome_event := 1L]

  # Defensive NA handling: coerce any remaining NAs to 0 in outcome and censoring columns.
  # This prevents model fitting failures when NA values appear in binary indicators.
  n_na_before <- sum(is.na(analysis[[censor_tv_col]]))
  analysis[is.na(get(censor_tv_col)), (censor_tv_col) := 0L]
  if (n_na_before > 0) {
    logger::log_warn(paste(
      "Replaced", n_na_before, "NA values in", censor_tv_col, "with 0L"
    ))
  }

  # Create treatment factor with configured reference level.
  analysis[, group_factor := as.factor(group)]
  if (reference_group %in% levels(analysis$group_factor)) {
    analysis[, group_factor := stats::relevel(group_factor, ref = reference_group)]
  }

  # Create time terms for numerator/denominator model formulas.
  analysis[, time_num := as.numeric(get(time_col))]
  if ("quadratic" %in% time_terms) {
    analysis[, time_num_sq := time_num^2]
  }

  # ===== Log censoring proportions by group (at person_id + T0 level) =====
  # Calculate proportion of unique (person_id, T0) with any non-admin censoring.
  person_level_censoring <- analysis[, .(
    has_censoring = as.integer(sum(get(censor_tv_col)) > 0)
  ), by = .(person_id, T0, group)]

  censoring_summary <- person_level_censoring[, .(
    n_persons_with_censoring = sum(has_censoring),
    n_persons_total = .N,
    prop_censored = sum(has_censoring) / .N
  ), by = group]

  for (r in seq_len(nrow(censoring_summary))) {
    grp <- censoring_summary$group[r]
    n_with <- censoring_summary$n_persons_with_censoring[r]
    n_tot <- censoring_summary$n_persons_total[r]
    prop <- censoring_summary$prop_censored[r]
    logger::log_info(paste(
      "IPCW", analysis_label,
      "censoring by group (person_id + T0 level):",
      grp, ":", n_with, "/", n_tot, "(",
      paste0(round(100 * prop, 2), "%)"),
      "with non-admin censoring"
    ))
  }

  # Normalize fit_groups parameter and determine which group(s) to fit.
  fit_groups <- tolower(fit_groups)
  if (fit_groups == "both") {
    groups_to_fit <- c("EXPOSED", "CONTROL")
  } else if (fit_groups == "exposed") {
    groups_to_fit <- "EXPOSED"
  } else if (fit_groups == "control") {
    groups_to_fit <- "CONTROL"
  } else {
    groups_to_fit <- toupper(fit_groups)
  }

  logger::log_info(paste("IPCW", analysis_label, "fitting on group(s):", paste(groups_to_fit, collapse = ", ")))

  # Initialize model building tracking table from metadata.
  model_building <- data.table::copy(ps_variables[, .(
    variable_id = var,
    var_type = vartype,
    force_in = if ("force_in" %in% names(ps_variables)) force_in else TRUE,
    force_out = if ("force_out" %in% names(ps_variables)) force_out else FALSE,
    domain
  )])

  # Map logical variable names to actual column names in analysis data.
  model_building[, variable_in_data := variable_id]
  model_building[domain %in% c("TIMEVARYING", "BOTH") & paste0(variable_id, "_est", estimand) %in% names(analysis), variable_in_data := paste0(variable_id, "_est", estimand)]

  # Check data presence and compute variable statistics.
  model_building[, exists_in_data := variable_in_data %in% names(analysis)]
  model_building[, missing_pct := vapply(
    variable_in_data,
    function(v) {
      if (!(v %in% names(analysis))) {
        return(NA_real_)
      }
      100 * sum(is.na(analysis[[v]])) / nrow(analysis)
    },
    numeric(1)
  )]

  model_building[, nonzero_variance := vapply(
    variable_in_data,
    function(v) {
      if (!(v %in% names(analysis))) {
        return(FALSE)
      }
      length(unique(analysis[[v]][!is.na(analysis[[v]])])) >= 2
    },
    logical(1)
  )]

  # Handle categorical and true/false variables: factor and set reference level.
  present_meta <- model_building[exists_in_data == TRUE]
  for (i in seq_len(nrow(present_meta))) {
    v <- present_meta$variable_in_data[i]
    meta_idx <- which(ps_variables$var == present_meta$variable_id[i])[1]
    vtype <- ps_variables$vartype[meta_idx]
    refcat <- as.character(ps_variables$reference_category[meta_idx])

    if (vtype %in% c("CAT", "TF")) {
      data.table::set(analysis, j = v, value = as.factor(analysis[[v]]))
      if (!is.na(refcat) && refcat %in% levels(analysis[[v]])) {
        data.table::set(
          analysis,
          j = v,
          value = stats::relevel(analysis[[v]], ref = refcat)
        )
      }
    }
  }

  # Mark covariates for inclusion in models: exclude missing/zero-variance/force_out.
  model_building[, include_r1 := FALSE]
  model_building[, included_r2 := FALSE]
  model_building[, reason_excluded := NA_character_]

  model_building[exists_in_data == FALSE, reason_excluded := "missing_in_data"]
  model_building[nonzero_variance == FALSE & exists_in_data == TRUE, reason_excluded := "zero_variance"]
  model_building[force_out == TRUE, reason_excluded := "force_out"]

  # Select denominator covariates: present, variable, not force-excluded.
  denominator_covariates <- model_building[
    exists_in_data == TRUE & nonzero_variance == TRUE & force_out == FALSE,
    variable_in_data
  ]

  model_building[variable_in_data %in% denominator_covariates, `:=`(include_r1 = TRUE, included_r2 = TRUE)]

  # Build numerator time terms based on time_terms parameter.
  time_basis <- "time_num"
  if ("quadratic" %in% time_terms) {
    time_basis <- c(time_basis, "time_num_sq")
  }

  # Memory diagnostics: log analysis table dimensions before expensive model fitting.
  analysis_rows <- nrow(analysis)
  analysis_cols <- ncol(analysis)
  analysis_mem_mb <- as.numeric(object.size(analysis)) / (1024^2)
  logger::log_info(paste(
    "IPCW", analysis_label, "analysis table dimensions:",
    analysis_rows, "rows x", analysis_cols, "columns,",
    round(analysis_mem_mb, 2), "MB"
  ))

  # Numerator: minimal model with treatment and configured time terms.
  numerator_terms <- c("group_factor", time_basis)
  num_formula <- as.formula(
    paste(censor_tv_col, "~", paste(numerator_terms, collapse = " + "))
  )

  # Denominator: add selected covariates to numerator terms.
  denominator_terms <- unique(c(numerator_terms, denominator_covariates))
  den_formula <- as.formula(
    paste(censor_tv_col, "~", paste(denominator_terms, collapse = " + "))
  )

  num_formula_str <- paste(deparse(num_formula), collapse = " ")
  den_formula_str <- paste(deparse(den_formula), collapse = " ")
  logger::log_info(paste("IPCW", analysis_label, "numerator formula:", num_formula_str))
  logger::log_info(paste("IPCW", analysis_label, "denominator formula:", den_formula_str))

  # Fit numerator and denominator logistic models for censoring probability.
  # If fit_groups is restricted, fit on subset; predict on all data.
  if (length(groups_to_fit) < length(group_levels) || !all(group_levels %in% groups_to_fit)) {
    # Fit on subset
    analysis_for_fit <- analysis[group %in% groups_to_fit]
    logger::log_info(paste(
      "IPCW", analysis_label,
      "fitting on subset: ", nrow(analysis_for_fit), "rows (out of", nrow(analysis), ")"
    ))
  } else {
    analysis_for_fit <- analysis
  }

  fit_num <- fit_ps_model(
    predictors = numerator_terms,
    cohort_covariates = analysis_for_fit,
    outcome_col = censor_tv_col,
    primary_method = primary_method
  )

  fit_den <- fit_ps_model(
    predictors = denominator_terms,
    cohort_covariates = analysis_for_fit,
    outcome_col = censor_tv_col,
    primary_method = primary_method
  )

  if (is.null(fit_num)) {
    stop(paste0("Numerator IPCW model failed for ", analysis_label, "."))
  }

  if (is.null(fit_den)) {
    stop(paste0("Denominator IPCW model failed for ", analysis_label, "."))
  }

  # Non-converged denominator model marks all included covariates as problematic.
  if (!isTRUE(fit_den$converged)) {
    model_building[include_r1 == TRUE, reason_excluded := "non_convergence"]
  }

  # Identify unstable variables by large standard errors in coefficient blocks.
  coef_summary_den <- as.data.frame(summary(fit_den)$coefficients)
  for (v in denominator_covariates) {
    coef_rows <- grep(paste0("^", v), rownames(coef_summary_den))
    if (length(coef_rows) > 0) {
      max_se <- max(coef_summary_den[coef_rows, "Std. Error"])
      if (is.finite(max_se) && max_se > se_threshold) {
        model_building[variable_in_data == v, reason_excluded := "large_se"]
      }
    }
  }

  # Predict censoring probability for each observation.
  # Explicitly pass newdata to support both glm/speedglm and bigglm model objects.
  p_num <- as.numeric(predict(fit_num, newdata = analysis, type = "response"))
  p_den <- as.numeric(predict(fit_den, newdata = analysis, type = "response"))

  # For groups not in fit_groups, set predictions to 1.0 (no censoring adjustment).
  # This results in ipcw_step = 1.0 (no weight modification) for those groups.
  if (length(groups_to_fit) < length(group_levels)) {
    groups_not_fit <- setdiff(group_levels, groups_to_fit)
    for (grp in groups_not_fit) {
      idx_grp <- analysis$group == grp
      p_num[idx_grp] <- 1.0
      p_den[idx_grp] <- 1.0
      logger::log_info(paste(
        "IPCW", analysis_label,
        "group", grp, "not fitted: setting p_num = p_den = 1.0 for",
        sum(idx_grp), "observations"
      ))
    }
  }

  # Clip predicted probabilities to avoid numerical extremes.
  p_num <- pmin(pmax(p_num, prediction_eps), 1 - prediction_eps)
  p_den <- pmin(pmax(p_den, prediction_eps), 1 - prediction_eps)

  analysis[, p_num := p_num]
  analysis[, p_den := p_den]

  # Probability of observed censoring outcome (observed/predicted).
  analysis[, p_obs_num := data.table::fifelse(get(censor_tv_col) == 1L, p_num, 1 - p_num)]
  analysis[, p_obs_den := data.table::fifelse(get(censor_tv_col) == 1L, p_den, 1 - p_den)]

  # Step weight = ratio of observed prob under numerator to denominator.
  analysis[, ipcw_step := p_obs_num / p_obs_den]
  # Cumulative weight within person-baseline stratum.
  analysis[, ipcw_cum := cumprod(ipcw_step), by = .(person_id, T0)]

  # Compute and apply quantile-based truncation to weight distribution.
  trunc_lo <- stats::quantile(analysis$ipcw_cum, probs = q_lower, na.rm = TRUE)
  trunc_hi <- stats::quantile(analysis$ipcw_cum, probs = q_upper, na.rm = TRUE)
  analysis[, ipcw_cum_trim := data.table::fifelse(
    ipcw_cum < trunc_lo,
    trunc_lo,
    data.table::fifelse(ipcw_cum > trunc_hi, trunc_hi, ipcw_cum)
  )]

  logger::log_info(paste(
    "IPCW", analysis_label,
    "truncation interval:",
    paste0("[", round(trunc_lo, 4), ", ", round(trunc_hi, 4), "]")
  ))

  # Helper: extract coefficient summaries from fitted model.
  # Handles both glm/speedglm and biglm output formats.
  extract_coef <- function(fit_obj, model_type, analysis_name) {
    s <- summary(fit_obj)

    # Check if this is a biglm/bigglm object (has $mat instead of $coefficients)
    if (inherits(fit_obj, "biglm") && !is.null(s$mat)) {
      # biglm format: matrix with columns Coef, (95%, CI), SE, p
      mat <- s$mat
      coef_estimates <- mat[, "Coef"]
      std_errors <- mat[, "SE"]
      p_values <- mat[, "p"]
      # Calculate z-score from coefficient and std error
      z_scores <- coef_estimates / std_errors
      coef_names <- rownames(mat)
    } else {
      # glm/speedglm format: summary$coefficients is a data.frame
      ctab <- as.data.frame(s$coefficients)
      coef_estimates <- ctab[, 1]
      std_errors <- ctab[, 2]
      z_scores <- ctab[, 3]
      p_values <- ctab[, 4]
      coef_names <- rownames(ctab)
    }

    data.table::data.table(
      model_type = model_type,
      coefficient = coef_names,
      estimate = coef_estimates,
      std_error = std_errors,
      z_score = z_scores,
      p_value = p_values,
      exp_estimate = exp(coef_estimates),
      analysis = analysis_name
    )
  }

  # Extract and combine coefficients from both models.
  d5_coef <- data.table::rbindlist(list(
    extract_coef(fit_num, "numerator", analysis_label),
    extract_coef(fit_den, "denominator", analysis_label)
  ))

  # Build model-building summary table for diagnostics and reporting.
  d5_model_building <- model_building[, .(
    variable_id,
    variable_in_data,
    var_type,
    domain,
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
  d5_model_building[, estimand := estimand]

  # Prepare final D4 IPCW output: person-level weights and diagnostics.
  # Conditionally include match_id if it exists in the analysis data.
  if ("match_id" %in% names(analysis)) {
    d4_ipcw <- analysis[, .(
      person_id,
      T0,
      group,
      match_id,
      time_days = get(time_col),
      censor_nonadmin = get(censor_tv_col),
      outcome_event,
      p_num,
      p_den,
      p_obs_num,
      p_obs_den,
      ipcw_step,
      ipcw_cum,
      ipcw_cum_trim
    )]
  } else {
    d4_ipcw <- analysis[, .(
      person_id,
      T0,
      group,
      time_days = get(time_col),
      censor_nonadmin = get(censor_tv_col),
      outcome_event,
      p_num,
      p_den,
      p_obs_num,
      p_obs_den,
      ipcw_step,
      ipcw_cum,
      ipcw_cum_trim
    )]
  }
  d4_ipcw[, analysis := analysis_label]
  d4_ipcw[, estimand := estimand]

  # Return list of three output tables.
  return(list(
    D4_StudyCohort_IPCW = d4_ipcw,
    D5_Censor_Coef_IPCW = d5_coef,
    D5_Censor_ModelBuilding_IPCW = d5_model_building
  ))
}
