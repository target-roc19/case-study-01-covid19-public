# ---------------------------------------------------------------------------
# fn_mask_counts.R
# Helpers for masking small counts in D5 analytic lookup outputs
# ---------------------------------------------------------------------------

library(data.table)

#' Mask Small Count Values
#'
#' Replaces count values strictly below `number` with the string `"<number"`.
#' Values that are already masked (contain `"<"`), negative, zero, `NA`,
#' `NaN`, or empty are returned unchanged.
#'
#' @param x Numeric or character vector of count values.
#' @param number Numeric threshold below which counts are masked. Default 5.
#'
#' @return A character vector with small counts replaced by `"<number"`.
mask_count <- function(x, number = 5) {
  unlist(sapply(x, function(s) {
    # Leave already-masked, missing, empty, negative, or zero values alone.
    s_num <- suppressWarnings(as.numeric(s))
    if (
      is.na(s_num) ||
        is.nan(s_num) ||
        s %in% c("NE", "NA", "") ||
        grepl("<", as.character(s)) ||
        isTRUE(s_num < 0) ||
        isTRUE(s_num == 0)
    ) {
      return(s)
    }
    # Replace counts strictly below the threshold.
    if (isTRUE(s_num < number)) {
      return(paste0("<", number))
    }
    # Round and format counts at or above the threshold.
    sprintf("%1.0f", s_num)
  }))
}

#' Apply Count Masking to D5 Analytic Lookup
#'
#' Applies `mask_count()` to standard patient/event count columns and then
#' suppresses person-time columns (`py_exp`, `py_con`) when the corresponding
#' event count was masked.
#'
#' Columns masked (when present):
#' - `n_pat_exp`, `n_out_exp`, `n_pat_con`, `n_out_con`
#'
#' Person-time columns set to `"NR"` when events are masked:
#' - `py_exp` (when `n_out_exp` is masked)
#' - `py_con` (when `n_out_con` is masked)
#'
#' @param dt A `data.table` D5 analytic lookup.
#' @param threshold Integer masking threshold. Default 5.
#'
#' @return Modified `data.table` with masked count and person-time columns.
apply_count_masking <- function(dt, threshold = 5L) {
  stopifnot(data.table::is.data.table(dt))
  threshold <- as.integer(threshold)

  # Mask patient and event count columns.
  count_cols <- c("n_pat_exp", "n_out_exp", "n_pat_con", "n_out_con")
  present_count_cols <- intersect(count_cols, names(dt))

  for (col in present_count_cols) {
    dt[, (col) := mask_count(get(col), number = threshold)]
  }

  # Suppress person-time when linked event count was masked.
  masked_token <- paste0("<", threshold)
  if (all(c("n_out_exp", "py_exp") %in% names(dt))) {
    dt[, py_exp := data.table::fifelse(
      n_out_exp == masked_token,
      "NR",
      format(round(suppressWarnings(as.numeric(py_exp)), 1L), nsmall = 1L)
    )]
  }
  if (all(c("n_out_con", "py_con") %in% names(dt))) {
    dt[, py_con := data.table::fifelse(
      n_out_con == masked_token,
      "NR",
      format(round(suppressWarnings(as.numeric(py_con)), 1L), nsmall = 1L)
    )]
  }

  return(dt)
}
