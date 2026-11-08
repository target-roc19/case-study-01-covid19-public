#' Fit a binomial propensity-style model with fallback behavior
#'
#' Attempts to fit a logistic regression using a specified primary method
#' (speedglm or biglm) and falls back to base glm when the primary fails.
#' Predictors with zero variance are removed before model fitting.
#'
#' @param predictors Character vector. Candidate predictor variable names.
#' @param cohort_covariates Data frame/data.table containing predictors and the
#'   outcome column.
#' @param outcome_col Character scalar. Name of binary outcome column.
#' @param use_speedglm Logical scalar. If TRUE (default), use speedglm as
#'   primary method. If FALSE, use glm only (no fast methods).
#' @param primary_method Character scalar. Choice of primary method:
#'   "speedglm" (default), "biglm", or "glm". Ignored if use_speedglm = FALSE.
#'
#' @return A fitted model object or NULL when fitting fails.
#'
#' @noRd
fit_ps_model <- function(
  predictors,
  cohort_covariates,
  outcome_col,
  use_speedglm = TRUE,
  primary_method = "speedglm"
) {
  result <- NULL
  warning_msg <- NULL
  error_msg <- NULL
  model_method <- "speedglm"

  # Filter out predictors with no informational value for model fitting.
  valid_predictors <- c()
  dropped_predictors <- c()
  for (pred in predictors) {
    n_unique <- length(unique(cohort_covariates[[pred]][!is.na(cohort_covariates[[pred]])]))
    if (n_unique < 2) {
      dropped_predictors <- c(dropped_predictors, pred)
      logger::log_warn(paste0(
        "Dropping predictor '",
        pred,
        "' with only ",
        n_unique,
        " unique non-missing value(s)."
      ))
    } else {
      valid_predictors <- c(valid_predictors, pred)
    }
  }

  # Guard against impossible model specification.
  if (length(valid_predictors) == 0) {
    logger::log_error("All predictors have zero variance. Cannot fit model.")
    return(NULL)
  }

  if (length(dropped_predictors) > 0) {
    logger::log_info(paste0(
      "Removed ",
      length(dropped_predictors),
      " predictor(s) with zero variance: ",
      paste(dropped_predictors, collapse = ", ")
    ))
  }

  # Build model formula dynamically from the retained predictors.
  model_formula <- as.formula(paste0(
    outcome_col,
    " ~ ",
    paste(valid_predictors, collapse = " + ")
  ))

  # Local helper to run base glm and capture warnings consistently.
  run_glm_with_warnings <- function(formula_obj, model_data) {
    warning_msg <<- NULL
    model_method <<- "glm"
    withCallingHandlers(
      expr = glm(
        formula = formula_obj,
        data = model_data,
        family = "binomial"
      ),
      warning = function(w) {
        warning_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }

  # Local helper to run speedglm and capture warnings consistently.
  run_speedglm_with_warnings <- function(formula_obj, model_data) {
    warning_msg <<- NULL
    model_method <<- "speedglm"
    withCallingHandlers(
      expr = speedglm::speedglm(
        formula = formula_obj,
        data = model_data,
        family = binomial(),
        fitted = TRUE
      ),
      warning = function(w) {
        warning_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }

  # Local helper to run biglm for large datasets and capture warnings consistently.
  run_biglm_with_warnings <- function(formula_obj, model_data) {
    warning_msg <<- NULL
    model_method <<- "biglm"
    withCallingHandlers(
      expr = biglm::bigglm(
        formula = formula_obj,
        data = model_data,
        chunksize = 100000,
        maxit = 100,
        tolerance = 1e-6,
        family = binomial()
      ),
      warning = function(w) {
        warning_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }

  # Normalize primary_method parameter to lowercase for comparison.
  primary_method <- tolower(primary_method)
  if (!(primary_method %in% c("speedglm", "biglm", "glm"))) {
    logger::log_warn(paste0(
      "Invalid primary_method '", primary_method, "'. Using 'speedglm'."
    ))
    primary_method <- "speedglm"
  }

  # Attempt primary method first (speedglm, biglm, or glm), then fallback to glm if applicable.
  if (primary_method == "glm") {
    # Direct glm path when glm is primary method.
    result <- tryCatch(
      run_glm_with_warnings(model_formula, cohort_covariates),
      error = function(e) {
        error_msg <<- conditionMessage(e)
        logger::log_info(paste0("glm failed with error: ", error_msg))
        NULL
      }
    )
  } else if (use_speedglm) {
    # Try primary method first (speedglm or biglm), then fallback to glm.
    result <- tryCatch(
      if (primary_method == "speedglm") {
        run_speedglm_with_warnings(model_formula, cohort_covariates)
      } else {
        run_biglm_with_warnings(model_formula, cohort_covariates)
      },
      error = function(e) {
        error_msg <<- conditionMessage(e)
        logger::log_info(paste0(
          primary_method, " failed with error: ",
          error_msg,
          ". Falling back to glm()."
        ))
        run_glm_with_warnings(model_formula, cohort_covariates)
      }
    )
  } else {
    # Direct glm path when use_speedglm = FALSE (ignores primary_method).
    result <- tryCatch(
      run_glm_with_warnings(model_formula, cohort_covariates),
      error = function(e) {
        error_msg <<- conditionMessage(e)
        logger::log_info(paste0("glm failed with error: ", error_msg))
        NULL
      }
    )
  }

  # Emit concise execution diagnostics to logs.
  logger::log_info(paste0("Model fit using: ", model_method))

  if (!is.null(warning_msg)) {
    logger::log_info(paste0("Model warning: ", warning_msg))
  }

  # Normalize convergence status across glm and speedglm returns.
  if (is.null(result$converged) && !is.null(result$convergence)) {
    result$converged <- isTRUE(result$convergence)
  }

  return(result)
}
