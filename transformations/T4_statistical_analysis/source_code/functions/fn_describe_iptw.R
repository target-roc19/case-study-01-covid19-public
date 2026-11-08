#' Summarize IPTW distribution by treatment group
#'
#' Creates a compact table with central tendency, spread, and percentile-based
#' diagnostics for a stabilized weight column.
#'
#' @param p_matrix Data table/data frame containing group and weight columns.
#' @param groups Character vector with group labels to summarize.
#' @param weight_col Character scalar with weight column name.
#' @param custom_truncation Numeric length-2 vector with optional custom
#'   truncation cutoffs to print in the final row.
#' @param digits Integer number of decimals used in output rounding.
#'
#' @return Data frame with two value columns per group (V1/V2 style).
#'
#' @noRd
describe_iptw <- function(
  p_matrix,
  groups = c("CONTROL", "EXPOSED"),
  weight_col = "ipsmw_raw",
  custom_truncation = c(NA, NA),
  digits = 2
) {
  # Build group-level summaries and bind them side-by-side.
  for (gname in groups) {
    # Core moments and quantiles for the selected weight column.
    mean_weight <- mean(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)
    median_weight <- median(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)
    sd_weight <- sd(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)
    quants <- quantile(
      p_matrix[group == gname, get(weight_col)],
      c(custom_truncation[1], 0.25, 0.75, custom_truncation[2]),
      na.rm = TRUE
    )
    min_weight <- min(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)
    max_weight <- max(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)

    # Assemble one temporary summary table per group.
    gtemp <- data.frame(
      labels = c(
        "Mean (SD)",
        "Q1-Q3",
        "Median",
        "Range, Min-Max",
        paste0("P",custom_truncation[1]*100,"-P",custom_truncation[2]*100),
        "If truncated, cut-offs"
      ),
      V1 = round(c(
        mean_weight,
        quants[2],
        median_weight,
        min_weight,
        quants[1],
        custom_truncation[1]
      ), digits),
      V2 = round(c(
        sd_weight,
        quants[3],
        NA,
        max_weight,
        quants[4],
        custom_truncation[2]
      ), digits)
    )

    # Rename value columns to keep group identity when cbind-ing.
    colnames(gtemp)[c(2, 3)] <- paste0(gname, c("_V1", "_V2"))

    if (gname == groups[1]) {
      gout <- gtemp
    } else {
      # Drop duplicate labels column for subsequent groups.
      gout <- cbind(gout, gtemp[, -1])
    }
  }

  return(gout)
}
