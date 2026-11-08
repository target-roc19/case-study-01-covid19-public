#' Vectorized Censoring of Follow-up Dates
#'
#' Computes censoring dates for individuals in a follow-up table, optionally returning
#' the reason for censoring. Supports individual-level and pair-level censoring,
#' selective post-pregnancy censoring, and is optimized for vectorized computation.
#'
#' @param aesi_fup_table A data.table or data.frame containing follow-up dates.
#' @param rw_id Optional identifier to append to output column names.
#' @param analysis_name Optional label for the analysis.
#' @param person_id_var Name of the person identifier column.
#' @param match_date_var Name of the matching date column.
#' @param match_id_var Name of the match/group identifier column.
#' @param group_var Name of the group variable column.
#' @param ind_censor_vars Character vector of individual-level censoring date columns.
#' @param pair_censoring Logical, whether to compute pair-level censoring.
#' @param pair_censor_vars Character vector of pair-level censoring date columns.
#' @param selective_postpreg_censor Logical, whether to apply selective post-pregnancy censoring.
#' @param preg_end_var Column name for pregnancy end date, required if selective_postpreg_censor = TRUE.
#' @param postpreg_censor_vars Character vector of allowed post-pregnancy censoring variables.
#' @param return_reason Logical, whether to compute and return censoring reason columns.
#'
#' @return A data.table with computed censoring dates (`end_fup*`) and optionally
#'   reason columns for each individual or pair.
#' @export
censorFupAesiVect <- function(
  aesi_fup_table,
  rw_id = NULL,
  analysis_name = "main",
  person_id_var = "person_id",
  match_date_var = "T0",
  match_id_var = "match_id",
  group_var = "group",
  ind_censor_vars = NULL,
  pair_censoring = TRUE,
  pair_censor_vars = NULL,
  # selective post-pregnancy censoring
  selective_postpreg_censor = FALSE,
  preg_end_var = NULL,
  postpreg_censor_vars = NULL,
  # optionally, return censoring reason
  return_reason = TRUE
) {
  # transform to data table object if not one already
  if (!data.table::is.data.table(aesi_fup_table)) {
    aesi_fup_table <- data.table::as.data.table(aesi_fup_table)
  }

  # consider selective post-preg censoring yes/no
  if (selective_postpreg_censor) {
    # check pregnancy column
    if (!is.null(preg_end_var) && !(preg_end_var %in% names(aesi_fup_table))) {
      logger::log_info(stop(paste0("Column '", preg_end_var, "' not found in aesi_fup_table.")))
    }
    # check allowed post-preg censor vars
    if (!is.null(postpreg_censor_vars)) {
      missing_vars <- setdiff(postpreg_censor_vars, names(aesi_fup_table))
      if (length(missing_vars) > 0) {
        logger::log_info(stop(paste0("The following postpreg_censor_vars are missing: ", paste(missing_vars, collapse = ", "))))
      }
    }

    # apply selective post-preg censoring
    if (!is.null(preg_end_var) && !is.null(postpreg_censor_vars)) {
      all_censor_vars_present <- intersect(c(ind_censor_vars, pair_censor_vars), names(aesi_fup_table))
      if (length(all_censor_vars_present) > 0) {
        for (var in all_censor_vars_present) {
          if (!is.null(var) && !(var %in% postpreg_censor_vars)) {
            # Precompute the logical mask
            mask <- !is.na(aesi_fup_table[[var]]) & aesi_fup_table[[var]] > aesi_fup_table[[preg_end_var]]

            # Assign NA only where mask is TRUE
            aesi_fup_table[mask, (var) := as.Date(NA)]
          }
        }
        logger::log_info(message(
          "Selective post-pregnancy censoring applied; allowed variables: ",
          paste(postpreg_censor_vars, collapse = ", ")
        ))
      }
    }
  }

  # Initialize variables
  CensorDate <- paste0("end_fup", rw_id)
  reason_endFUP_name <- paste0("reason_endfup")
  reason_endFUP_name_pair <- paste0("reason_endFUP_pair")
  reason_endFUP_name_pair_indi <- paste0("reason_endFUP_pair_indi")
  reason_endFUP_name_indi <- paste0("reason_endFUP_indi")
  reason_endFUP_name_indi_all <- paste0("reason_endFUP_indi_all")

  all_censor_vars <- c(ind_censor_vars, pair_censor_vars)

  if (pair_censoring) {
    # create an empty censoring date variable
    aesi_fup_table <- data.table::copy(aesi_fup_table)
    aesi_fup_table[, (CensorDate) := NA_Date_]

    # == Step 1

    # censoring date at the individual level, according to the pair-censoring variables
    aesi_fup_table[, pair_censor_indi := as.Date(do.call(pmin, c(.SD, na.rm = TRUE))), .SDcols = pair_censor_vars]
    # second, attribute the reason for end follow up based on previous minimum, with order of events as tie-breaker
    if (return_reason) {
      aesi_fup_table[, (reason_endFUP_name_pair_indi) := pair_censor_vars[whichMinDateVect(aesi_fup_table, pair_censor_vars)]]
    }

    # censoring date at the individual level, according to the individual censoring variables
    aesi_fup_table[, indi_censor := as.Date(do.call(pmin, c(.SD, na.rm = TRUE))), .SDcols = ind_censor_vars]
    # second, attribute the reason for end follow up based on previous minimum, with order of events as tie-breaker
    if (return_reason) {
      aesi_fup_table[, (reason_endFUP_name_indi) := ind_censor_vars[whichMinDateVect(aesi_fup_table, ind_censor_vars)]]
    }

    # censoring date at the individual level, accordfing to both individual and pair censoring variables
    aesi_fup_table[, indi_censor_all := as.Date(do.call(pmin, c(.SD, na.rm = TRUE))), .SDcols = all_censor_vars]
    # attribute the reason for end follow up based on previous minimum, with order of events as tie-breaker
    if (return_reason) {
      aesi_fup_table[, (reason_endFUP_name_indi_all) := all_censor_vars[whichMinDateVect(aesi_fup_table, all_censor_vars)]]
    }

    # days between the matched date and date until pair_censor_indi (note, it is still at the individual level)
    aesi_fup_table[, days_btw_pair_censor_indi := difftime(pair_censor_indi, get(match_date_var), units = "days")]
    # days between the matched date and date until indi_censor
    aesi_fup_table[, days_btw_indi_censor := difftime(indi_censor, get(match_date_var), units = "days")]
    # days between the matched date and date until indi_censor_all
    aesi_fup_table[, days_btw_indi_censor_all := difftime(indi_censor_all, get(match_date_var), units = "days")]

    # == Step 2
    # start a loop to identify people who experience individual censoring variables before the pair censoring date

    # if individual reason happens first, remove pair censoring reason at the individual level
    aesi_fup_table[days_btw_indi_censor <= days_btw_pair_censor_indi, days_btw_pair_censor_indi := NA_Date_]

    # consider censoring of a pair (minimum of pair_censor_indi, in days), for those who is missing 'censoring date'
    aesi_fup_table[is.na(get(CensorDate)), days_btw_pair_censor := min(days_btw_pair_censor_indi, na.rm = TRUE), by = match_id_var]

    # those whose individual censoring date is earlier than the pair censoring, or pair censoring does not happen ---> assign CensorDate as the individual censoring date.
    if (return_reason) {
      aesi_fup_table[is.na(get(CensorDate)) & (days_btw_indi_censor_all <= days_btw_pair_censor | is.na(days_btw_pair_censor)), (reason_endFUP_name) := get(reason_endFUP_name_indi_all)]
    }
    aesi_fup_table[is.na(get(CensorDate)) & (days_btw_indi_censor_all <= days_btw_pair_censor | is.na(days_btw_pair_censor)), (CensorDate) := get(match_date_var) + days_btw_indi_censor_all]

    # == Step 3
    # consider censoring of a pair among the remaining people

    if (return_reason) {
      # compute the reason for each match_id_var group
      aesi_fup_table[, (reason_endFUP_name_pair) := reason_endFUP_pair_indi[which.min(days_btw_pair_censor_indi)], by = match_id_var]

      # only update reason_endfup where CensorDate is NA
      aesi_fup_table[is.na(get(CensorDate)), (reason_endFUP_name) := get(reason_endFUP_name_pair)]
    }

    # compute days between pair censoring and match start
    aesi_fup_table[, days_btw_pair_censor := min(days_btw_pair_censor_indi, na.rm = TRUE), by = match_id_var]

    # assign pair censor reason based on the minimum days (ignoring NAs)
    aesi_fup_table[is.na(get(CensorDate)), (CensorDate) := get(match_date_var) + days_btw_pair_censor]

    # remove redundant
    aesi_fup_table[, c(
      "pair_censor_indi", "indi_censor", "indi_censor_all",
      "days_btw_pair_censor_indi", "days_btw_indi_censor", "days_btw_pair_censor", "days_btw_indi_censor_all"
    ) := NULL]

    if (return_reason) {
      aesi_fup_table[, c("reason_endFUP_pair_indi", "reason_endFUP_indi", "reason_endFUP_pair", "reason_endFUP_indi_all") := NULL]
    }
  } else { # if no pair-censoring variable is defined
    aesi_fup_table[, (CensorDate) := as.Date(do.call(pmin, c(.SD, na.rm = TRUE))), .SDcols = ind_censor_vars]
    if (return_reason) {
      aesi_fup_table[, (reason_endFUP_name) := ind_censor_vars[whichMinDateVect(aesi_fup_table, ind_censor_vars)]]
    }
  }

  # return censored data
  return(aesi_fup_table)
}

#' Identify Column of Minimum Date Per Row
#'
#' For each row in a data.table or data.frame, returns the column index
#' corresponding to the minimum value. Works for Date, POSIX, or numeric columns
#' and handles ties and all-NA rows.
#'
#' @param dt A data.table or data.frame containing the columns to compare.
#' @param vars Character vector of column names to evaluate.
#' @param ties.method How to break ties: `"first"` or `"last"`.
#'
#' @return An integer vector of column indices indicating the minimum per row,
#'   or NA if all values in a row are NA.
#' @export
whichMinDateVect <- function(dt, vars, ties.method = "first") {
  # dt : data.table or data.frame
  # vars: character vector of column names (Date/POSIX/numeric)
  # ties.method: "first" or "last" (match which.min => "first")

  # 1. numeric matrix of the date columns (days since epoch)
  mnum <- as.matrix(dt[, lapply(.SD, as.numeric), .SDcols = vars])

  # 2. mark rows where every value is NA/ non-finite
  all_na <- rowSums(is.finite(mnum)) == 0L

  # 3. replace NA with +Inf so they don't win the "min" contest
  mnum_clean <- mnum
  mnum_clean[is.na(mnum_clean)] <- Inf

  # 4. use max.col on -mnum_clean to get index of the smallest original value
  idx <- max.col(-mnum_clean, ties.method = ties.method)

  # 5. set rows that were all NA back to NA index
  idx[all_na] <- NA_integer_

  return(idx)
}

# Calculate follow-ups

# The function identifies whether the event occurred during the risk period(s), and calculated the follow-up length.
# The function requires start and end dates of the risk periods to be alread defined for the relevant individuals.

#' Calculate Follow-up Length
#'
#' This function calculates the follow-up length and event occurrence for individuals across one or more risk windows. The function computes follow-up time in days and person-years, as well as the occurrence of events during specified risk periods.
#'
#' @param aesi_fup_table A `data.table` object containing the follow-up data.
#' @param rw_id A string identifier for the suffix to be used for computed columns (default is `"main"`).
#' @param censor_date_var A string representing the variable name for the censoring date.
#' @param event_date_var A string representing the variable name for the event date (default is `"Date_COUNT"`).
#' @param start_risk_window_vars A character vector specifying the start dates of the risk windows.
#' @param end_risk_window_vars A character vector specifying the end dates of the risk windows.
#' @param scaling A numeric value used to scale the follow-up duration (default is `1`).
#'
#' @return A `data.table` with follow-up length, person-years, and event counts computed for the specified risk windows.
#'
#' @details This function computes follow-up time in days and person-years, based on the difference between the censoring date and the start of the risk window. It also calculates the occurrence of events during the risk periods. The function supports multiple risk windows and scales follow-up durations if necessary.
#'
#' @examples
#' # Example usage
#' result <- calculateFupLengthAesi(
#'   aesi_fup_table = df_matched_aesi, censor_date_var = "censoring_date",
#'   start_risk_window_vars = c("start_date_1", "start_date_2"),
#'   end_risk_window_vars = c("end_date_1", "end_date_2"), scaling = 1
#' )
calculateFupLengthAesi <- function(
  aesi_fup_table,
  rw_id = NULL, # indicator suffix to be given to computed columns (e.g., "_main")
  censor_date_var, # a single date, date of censoring
  event_date_var = "Date_COUNT",
  start_risk_window_vars, # a vector of dates denoting start dates of the risk window(s)
  end_risk_window_vars, # a vector of dates denoting the end of rw; start_ and end_rw_date form pairs
  scaling = 1
) {
  # aesi_fup_table = df_matched_aesi
  # rw_id = NULL
  # censor_date_var = "date_censoring"
  # event_date_var = "Date_COUNT"
  # start_risk_window_vars = startwindows
  # end_risk_window_vars = endwindows
  # scaling = 1

  fupvar <- paste0("fup", rw_id)
  pyrvar <- paste0("pyr", rw_id)
  eventvar <- paste0("eventCount", rw_id)

  aesi_fup_table[is.na(get(event_date_var)), (eventvar) := 0]
  if (length(start_risk_window_vars) == 1) {
    # follow-up in days
    aesi_fup_table[, (fupvar) := as.integer(get(censor_date_var) - get(start_risk_window_vars) + 1) / scaling]
    # person-years
    aesi_fup_table[, (pyrvar) := as.integer(get(censor_date_var) - get(start_risk_window_vars) + 1) / 365.25]
    # if event is during the follow up period, event = 1
    aesi_fup_table[get(event_date_var) != 0, (eventvar) := data.table::fifelse(dplyr::between(get(event_date_var), get(start_risk_window_vars), get(censor_date_var)), 1, 0)]
  } else {
    nwindow <- length(start_risk_window_vars)
    for (k in 1:nwindow) { #--- loop to go through each risk periods
      this_fup <- paste0(fupvar, k)
      this_event <- paste0(eventvar, k)
      this_start_risk <- start_risk_window_vars[k]
      this_end_risk <- end_risk_window_vars[k]

      # calculate fup days of the current risk period k
      aesi_fup_table[, (this_fup) := data.table::fifelse(get(censor_date_var) < get(this_start_risk), # if censoring happened before start_risk of period k
        0, # 0 days
        as.integer(pmin(get(censor_date_var), get(this_end_risk), na.rm = T) - get(this_start_risk) + 1) / scaling,
        na = 0
      )]

      # identify AESI occurrence during the fup of the current risk period k
      aesi_fup_table[!is.na(get(this_start_risk)), (this_event) := data.table::fifelse(dplyr::between(get(event_date_var), get(this_start_risk), get(this_end_risk)), 1, 0, na = 0)]
    }
    allfupvars <- unlist(str_split(paste0(fupvar, 1:nwindow), ", "))
    alleventvar <- unlist(str_split(paste0(eventvar, 1:nwindow), ", "))
    # sum up the event count variable over all windows
    aesi_fup_table[, (eventvar) := apply(.SD, 1, sum, na.rm = TRUE), .SDcols = alleventvar]
    # safety measure: if event date is after the censoring date, event should be 0
    aesi_fup_table[!is.na(get(event_date_var)) & get(event_date_var) > get(censor_date_var), (eventvar) := 0]
    # sum up the fup variables over all windows
    aesi_fup_table[, (fupvar) := apply(.SD, 1, sum, na.rm = TRUE), .SDcols = allfupvars]
  }
}
