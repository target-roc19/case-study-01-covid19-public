#' This function generates a summary table for follow-up time, intercurrent
#' events, reasons for end of follow-up, and censoring for a single group
#' within a given dataset.
#'
#' It computes descriptive statistics for follow-up time, including median,
#' interquartile range (Q1, Q3), minimum, and maximum. It also summarizes
#' intercurrent events, end-of-follow-up outcomes, and censoring reasons as
#' counts and percentages relative to the total number of observations.
#'
#' Intercurrent events are identified based on non-missing values in the
#' specified columns.
#' End-of-follow-up and censoring categories are defined provided values,
#' allowing for flexible grouping of categories (e.g., combining multiple
#' reasons into a single row).
#'
#' Optionally, the function can also calculate follow-up summary statistics
#' separately for each censoring category. When `cens_fup = TRUE`,
#' median, Q1, Q3, minimum, and maximum follow-up times are displayed
#' below each censoring reason in the output table.
#'
#' Results are returned as a three-column table suitable for reporting (
#' according to the table shells).
#'
#' @param d A data frame or data.table containing patient-level data.
#' @param fup_time Character string specifying the column name for follow-up time.
#' @param reas_cens Character string specifying the column name indicating
#'   reason for end of follow-up or censoring.
#' @param header Character string used as the column header (e.g., group name).
#' @param ie_vars Character vector of column names indicating intercurrent
#'   events (non-missing values are counted as events).
#' @param ie_labels Character vector of labels corresponding to `ie_vars`.
#' @param eof_vars List of character vectors specifying values in
#'   `reas_cens` that define each end-of-follow-up category. Each element
#'   can contain one or more values.
#' @param eof_labels Character vector of labels corresponding to `eof_vars`.
#' @param cens_vars List of character vectors specifying values in
#'   `reas_cens` that define each censoring category. Each element can
#'   contain one or more values.
#' @param cens_labels Character vector of labels corresponding to
#'   `cens_vars`.
#' @param row_structure A vector with the order each section should be added
#' in the table. The default order is "header", "fup", "ie", "eof", "cens".
#' @param section_labels A list with the labels referring to each section
#' of the table that was added in row_structure. The default labels are:
#' fup  = "Person-months of follow-up",
#' ie   = "Occurrence of Intercurrent Events during follow-up, n (%)",
#' eof  = "Reason for end of follow-up, n (%)",
#' cens = "Reason for censoring, n (%)"
#' @param cens_fup If the summary statistics for each censoring reason
#' need to be calculated (default FALSE)
#' @param ie_fup If the summary statistics for each intercurrent event
#' need to be calculated (default FALSE)
#' @param n_dec Integer specifying the number of decimal places for rounding
#'   numeric outputs (default = 2).
#'
#' @examples
#' fup_ie_cens_tab(
#'   d,
#'   fup_time = "fup_length_months_1_3",
#'   reas_cens = "reason_censoring_1_3",
#'   header = "Overall",
#'   ie_vars = c("treat_disc_d", "partial_disc_d"),
#'   ie_labels = c("Treatment discontinuation", "Partial discontinuation"),
#'   eof_vars = list("disease_progression_d", "death_d"),
#'   eof_labels = c("Disease progression", "Death"),
#'   cens_vars = list(
#'     c("end_observation_data_d", "end_study_period"),
#'     "end_spell"
#'   ),
#'   cens_labels = c("Administrative censoring", "Disenrollment"),
#'   cens_fup = TRUE
#' )
#'
fup_ie_cens_tab <- function(d,
                            fup_time,
                            reas_cens,
                            header,
                            ie_vars,
                            ie_labels,
                            eof_vars,
                            eof_labels,
                            cens_vars,
                            cens_labels,
                            row_structure = c("header", "fup", "ie", "eof", "cens"),
                            section_labels = list(
                              fup  = "Person-months of follow-up",
                              ie   = "Occurrence of Intercurrent Events during follow-up, n (%)",
                              eof  = "Reason for end of follow-up, n (%)",
                              cens = "Reason for censoring, n (%)"
                            ),
                            cens_fup = FALSE,
                            ie_fup = FALSE,
                            n_dec = 2) {
  ## ---- Input validation ----
  # If the lenggth of the variables does not match the length of labels, stops
  if (length(ie_vars) != length(ie_labels)) {
    stop("ie_vars and ie_labels must have the same length")
  }

  if (length(eof_vars) != length(eof_labels)) {
    stop("eof_vars and eof_labels must have the same length")
  }

  if (length(cens_vars) != length(cens_labels)) {
    stop("cens_vars and cens_labels must have the same length")
  }

  # if thefup time vector is not part of the d
  if (!fup_time %in% names(d)) {
    stop("fup_time not found in d")
  }
  # same for reasons vector
  if (!reas_cens %in% names(d)) {
    stop("reas_cens not found in d")
  }
  # if the IE events columns are not part of d, stops
  if (!all(ie_vars %in% names(d))) {
    stop("Some ie_vars not found in d")
  }

  ## Extract the vectors
  fup_vec <- d[[fup_time]]
  cens_vec <- d[[reas_cens]]

  ## Check if the end of follow up/censoring reasons exist in the cens_vec
  if (!all(unlist(eof_vars) %in% unique(cens_vec))) {
    warning("Some eof_vars values not found in reas_cens")
  }

  if (!all(unlist(cens_vars) %in% unique(cens_vec))) {
    warning("Some cens_vars values not found in reas_cens")
  }

  ## Total rows, to be used as denominator
  n_total <- nrow(d)

  if (n_total == 0) warning("Dataset has zero rows")

  ## Person-months of follow up
  if (any(is.na(fup_vec))) {
    warning("Missing values in the follow-up vector")
  }

  # Calculate the summary statistics for persons-months of fup
  fup_stats <- summ_fun(fup_vec, n_dec)

  ## ---- Intercurrent events ----
  n_ie <- vapply(ie_vars, function(v) sum(!is.na(d[[v]])), numeric(1))
  p_ie <- round(100 * n_ie / n_total, n_dec)

  ## ---- Reasons for end of follow-up ----
  n_eof <- vapply(eof_vars, function(v) sum(cens_vec %in% v, na.rm = TRUE), numeric(1))
  p_eof <- round(100 * n_eof / n_total, n_dec)

  ## ---- Create output columns ----
  ie_col1 <- paste0("   ", ie_labels)
  ie_col2 <- as.character(n_ie)
  ie_col3 <- paste0(p_ie, "%")

  eof_col1 <- paste0("   ", eof_labels)
  eof_col2 <- as.character(n_eof)
  eof_col3 <- paste0(p_eof, "%")

  ## ---- Final table ----
  col1 <- c()
  col2 <- c()
  col3 <- c()

  for (section in row_structure) {
    if (section == "header") {
      col1 <- c(col1, "")
      col2 <- c(col2, paste0(header, " (N = ", n_total, ")"))
      col3 <- c(col3, "")
    }
    if (section == "fup") {
      col1 <- c(
        col1,
        section_labels$fup,
        "   Median (Q1, Q3)",
        "   Min, max"
      )

      col2 <- c(
        col2,
        "",
        as.character(fup_stats["med"]),
        as.character(fup_stats["min"])
      )

      col3 <- c(
        col3,
        "",
        paste0("(", fup_stats["q1"], ", ", fup_stats["q3"], ")"),
        as.character(fup_stats["max"])
      )
    }
    # if there is a section ie in the row structure
    if (section == "ie") {
      col1 <- c(col1, section_labels$ie)
      col2 <- c(col2, "")
      col3 <- c(col3, "")

      # iterate through each IE and add its row + optional fup stats
      for (i in seq_along(ie_vars)) {
        col1 <- c(col1, ie_col1[i])
        col2 <- c(col2, ie_col2[i])
        col3 <- c(col3, ie_col3[i])

        # if it was specified in the argument
        if (ie_fup) { # calculate fup for this intercurrent event
          idx <- !is.na(d[[ie_vars[i]]])
          n_i <- sum(idx, na.rm = TRUE)

          if (n_i > 0) {
            # apply the summary statistics function for IE subset
            fup_i <- summ_fun(fup_vec[idx], n_dec)

            col1 <- c(
              col1,
              "      Median (Q1, Q3)",
              "      Min, max"
            )

            col2 <- c(
              col2,
              as.character(fup_i["med"]),
              as.character(fup_i["min"])
            )

            col3 <- c(
              col3,
              paste0("(", fup_i["q1"], ", ", fup_i["q3"], ")"),
              as.character(fup_i["max"])
            )
          } else {
            col1 <- c(
              col1,
              "      Median (Q1, Q3)",
              "      Min, max"
            )
            col2 <- c(col2, "", "")
            col3 <- c(col3, "", "")
          }
        }
      }
    }
    # if there is a section eof in the row structure
    if (section == "eof") {
      col1 <- c(
        col1,
        section_labels$eof,
        eof_col1
      )

      col2 <- c(
        col2,
        "",
        eof_col2
      )

      col3 <- c(
        col3,
        "",
        eof_col3
      )
    }
    # if there is a section cens in the row structure
    if (section == "cens") {
      col1 <- c(col1, section_labels$cens)
      col2 <- c(col2, "")
      col3 <- c(col3, "")

      # for each censoring reason calculate n and p
      for (i in seq_along(cens_vars)) {
        idx <- cens_vec %in% cens_vars[[i]]

        n_i <- sum(idx, na.rm = TRUE)
        p_i <- round(100 * n_i / n_total, n_dec)

        col1 <- c(col1, paste0("   ", cens_labels[i]))
        col2 <- c(col2, as.character(n_i))
        col3 <- c(col3, paste0(p_i, "%"))

        # if it was specified in the argument
        if (cens_fup) { # calculate fup for each censoring reason

          # apply the summary statistics function for groups
          fup_i <- summ_fun_by_group(fup_vec, cens_vec, cens_vars[[i]], n_dec)

          col1 <- c(
            col1,
            "      Median (Q1, Q3)",
            "      Min, max"
          )

          if (!is.null(fup_i)) {
            col2 <- c(
              col2,
              as.character(fup_i["med"]),
              as.character(fup_i["min"])
            )

            col3 <- c(
              col3,
              paste0("(", fup_i["q1"], ", ", fup_i["q3"], ")"),
              as.character(fup_i["max"])
            )
          } else {
            col2 <- c(col2, "", "")
            col3 <- c(col3, "", "")
          }
        }
      }
    }
  }

  df <- data.frame(
    col1 = col1,
    col2 = col2,
    col3 = col3,
    stringsAsFactors = FALSE
  )

  return(df)
}

#' Create summary statistisc for a numeric vector
#'
#' This function computes descriptive summary statistics of a
#' numeric vector. It returns the median, IQR (q1, q3) min and
#' max.
#' The missing values are ignored in all calculations.
#' The numbers are rounded to 2 decimal points by default.
#' The formatting was avoided in purpose, and the user
#' can further wrangle the output in any desired way.
#' This function helps to avoid repeating these
#' calculations.
#' @param x A numeric vector for which summary statistics
#' will be calculated
#' @param n_dec Integer number of the decimals for rounding
summ_fun <- function(x, n_dec = 2) {
  x <- as.numeric(x)

  stats <- c(
    med = median(x, na.rm = TRUE),
    q1  = unname(quantile(x, 0.25, na.rm = TRUE)),
    q3  = unname(quantile(x, 0.75, na.rm = TRUE)),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )

  round(stats, n_dec)
}

#' Create summary statistics for a numeric vector within groups.
#'
#' This function computes descriptive summary statistics for a subset of a
#' numeric vector defined by one or more grouping values.
#'
#' The function uses `summ_fun()` internally to calculate the median,
#' quartiles (Q1, Q3), minimum, and maximum for the selected subset.
#'
#' If no observations match the specified grouping values, the function
#' returns NULL.
#'
#' @param num_vec A numeric vector for which summary statistics will be
#'   calculated.
#' @param group_vec A vector defining the grouping variable.
#' @param group_values A vector of values in `group_vec` used to define
#'   the subset of observations included in the summary calculations.
#' @param n_dec Integer specifying the number of decimal places used for
#'   rounding the output. Default is 2.
summ_fun_by_group <- function(num_vec,
                              group_vec,
                              group_values,
                              n_dec = 2) {
  idx <- group_vec %in% group_values
  if (!any(idx, na.rm = TRUE)) {
    return(NULL)
  }
  summ_fun(num_vec[idx], n_dec)
}
