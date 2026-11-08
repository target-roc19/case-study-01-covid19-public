# ---------------------------------------------------------------------------
# fn_best_worst_scenarios.R
# Helper for best/worst-case non-administrative censoring sensitivity analyses
# ---------------------------------------------------------------------------

library(data.table)

#' Apply Best/Worst-Case Scenario Imputation to Event Counts
#'
#' For rows with non-administrative censoring, this function imputes uncertain
#' outcomes according to one of four scenario definitions. Only rows with
#' `event_col == 0` are modified.
#'
#' Scenario mapping:
#' - 1: EXPOSED best (0), CONTROL worst (1)
#' - 2: EXPOSED worst (1), CONTROL best (0)
#' - 3: both best (0)
#' - 4: both worst (1)
#'
#' @param fup_dt A `data.table` follow-up dataset.
#' @param event_col Character name of the event-count column (for example,
#'   `"event_count_est1"` or `"event_count_est2"`).
#' @param censor_col Character name of the non-administrative censoring flag
#'   column (for example, `"censor_nonadmin_est1"` or
#'   `"censor_nonadmin_est2"`).
#' @param scenario_id Integer in 1:4 defining the best/worst pattern.
#' @param group_col Character name of the group column. Default is `"group"`.
#'
#' @return A modified `data.table`.
apply_best_worst_scenario <- function(fup_dt,
                                      event_col,
                                      censor_col,
                                      scenario_id,
                                      group_col = "group") {
  stopifnot(data.table::is.data.table(fup_dt))
  stopifnot(event_col %in% names(fup_dt))
  stopifnot(censor_col %in% names(fup_dt))
  stopifnot(group_col %in% names(fup_dt))
  stopifnot(!is.na(scenario_id), scenario_id %in% 1:4)

  # Convert censor flag to logical safely (TRUE/1 are considered censored).
  is_nonadmin <- !is.na(fup_dt[[censor_col]]) & as.logical(fup_dt[[censor_col]])

  # Only uncertain outcomes are imputed (existing events remain untouched).
  can_impute <- is_nonadmin & !is.na(fup_dt[[event_col]]) & (fup_dt[[event_col]] == 0)

  if (!any(can_impute)) {
    return(fup_dt)
  }

  # Determine group-specific targets for each scenario.

  # Differential pyr for exposed and control groups in best/worst-case scenarios
  # "best" is assumed to have maximum follow-up time (90 days) and "worst" is
  # assumed to have the event the day after the last observed follow-up time (1 day)
  if (scenario_id == 1L) {
    # Scenario BW
    exposed_target <- 0L
    control_target <- 1L
    # Impute pyr
    fup_dt[can_impute & fup_dt[[group_col]] == "EXPOSED", pyr := 90 / 365.25]
    fup_dt[can_impute & fup_dt[[group_col]] == "CONTROL", pyr := (pyr * 365.25 + 1) / 365.25]
  } else if (scenario_id == 2L) {
    # Scenario WB
    exposed_target <- 1L
    control_target <- 0L
    # Impute pyr
    fup_dt[can_impute & fup_dt[[group_col]] == "EXPOSED", pyr := (pyr * 365.25 + 1) / 365.25]
    fup_dt[can_impute & fup_dt[[group_col]] == "CONTROL", pyr := 90 / 365.25]
  } else if (scenario_id == 3L) {
    # Scenario BB
    exposed_target <- 0L
    control_target <- 0L
    # Impute pyr
    fup_dt[can_impute, pyr := 90 / 365.25]
  } else if (scenario_id == 4L) {
    # Scenario WW
    exposed_target <- 1L
    control_target <- 1L
    # Impute pyr
    fup_dt[can_impute, pyr := (pyr * 365.25 + 1) / 365.25]
  }

  idx_exp <- can_impute & (fup_dt[[group_col]] == "EXPOSED")
  idx_con <- can_impute & (fup_dt[[group_col]] == "CONTROL")

  if (any(idx_exp)) {
    fup_dt[idx_exp, (event_col) := exposed_target]
  }

  if (any(idx_con)) {
    fup_dt[idx_con, (event_col) := control_target]
  }

  # Impute pyr_offset for Poisson regression (log of pyr)
  fup_dt[can_impute, pyr_offset := log(pyr)]

  return(fup_dt)
}
