################################################################
# fn_attrition_tables ---
# Aim:
# Build attrition tables for D3 eligibility and matched D4 cohorts.
#
# Tables:
# - Table 1(a): entry into eligible population
# - Table 1(b): matched cohort main analysis (without replacement)
# - Table 1(c): matched cohort supplementary analysis (with replacement)
################################################################

# Convert common encodings to logical without changing source data.
.as_logical <- function(x) {
  # Keep logical vectors unchanged.
  if (is.logical(x)) {
    return(x)
  }
  # Treat non-zero numeric/integer values as TRUE.
  if (is.numeric(x) || is.integer(x)) {
    return(x != 0)
  }
  # Normalize common string representations used in extracts.
  if (is.character(x)) {
    x_up <- toupper(trimws(x))
    return(x_up %in% c("TRUE", "T", "1", "YES", "Y"))
  }
  # Fallback for other classes.
  as.logical(x)
}

.assert_required_cols <- function(dt, required_cols, table_name) {
  # Fail early with a clear message when expected input fields are absent.
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "%s is missing required columns: %s",
        table_name,
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}

.pct <- function(numerator, denominator, digits = 1) {
  # Return blank-compatible NA when denominator is undefined or zero.
  if (is.na(denominator) || denominator == 0) {
    return(NA_character_)
  }
  format(round((numerator / denominator) * 100, digits), nsmall = digits)
}

.format_count <- function(x) {
  # Keep counts as character so output tables are report-ready on write.
  if (is.na(x)) {
    return(NA_character_)
  }
  format(x, scientific = FALSE, trim = TRUE)
}

.finalize_attrition_table <- function(dt, return_na = FALSE) {
  out <- data.table::copy(dt)
  if (!return_na) {
    # Replace missing display cells with empty strings for final CSV shells.
    out[is.na(N), N := ""]
    out[is.na(Percentage), Percentage := ""]
    out[is.na(Aux), Aux := ""]
  }
  out[]
}

#' Table 1(a): Attrition table for entry into the eligible population.
#'
#' @param d3_eligibility D3 eligibility dataset.
#' @param digits Number of decimals for percentages.
#' @param return_na Keep NA values if TRUE; otherwise replace with empty strings.
#'
#' @return data.table with columns Label, N, Percentage, Aux.
create_attrition_table_1a <- function(d3_eligibility, digits = 1, return_na = FALSE) {
  # Work on a copy to avoid modifying upstream objects by reference.
  dt <- data.table::as.data.table(data.table::copy(d3_eligibility))

  .assert_required_cols(
    dt,
    c(
      "person_id", "RECEIVES_EXPOSURE", "AGE_16_OR_OLDER", "ENROLLED_6_MONTHS",
      "STABLE_PRECONDITIONS", "SV_HOSP_6WEEKS", "PREV_OTHER_COVID_VAC",
      "SV_PRIOR_PREP_COVID", "SV_IMMUNOCOMPROMISED", "PRIOR_PFIZER_VAC",
      "in_eligible_pop", "eligible_exposed", "eligible_control"
    ),
    "d3_eligibility"
  )

  # Harmonize all logical flags used in shell criteria and totals.
  dt[, RECEIVES_EXPOSURE := .as_logical(RECEIVES_EXPOSURE)]
  dt[, AGE_16_OR_OLDER := .as_logical(AGE_16_OR_OLDER)]
  dt[, ENROLLED_6_MONTHS := .as_logical(ENROLLED_6_MONTHS)]
  dt[, STABLE_PRECONDITIONS := .as_logical(STABLE_PRECONDITIONS)]
  dt[, SV_HOSP_6WEEKS := .as_logical(SV_HOSP_6WEEKS)]
  dt[, PREV_OTHER_COVID_VAC := .as_logical(PREV_OTHER_COVID_VAC)]
  dt[, SV_PRIOR_PREP_COVID := .as_logical(SV_PRIOR_PREP_COVID)]
  dt[, SV_IMMUNOCOMPROMISED := .as_logical(SV_IMMUNOCOMPROMISED)]
  dt[, PRIOR_PFIZER_VAC := .as_logical(PRIOR_PFIZER_VAC)]
  dt[, in_eligible_pop := .as_logical(in_eligible_pop)]
  dt[, eligible_exposed := .as_logical(eligible_exposed)]
  dt[, eligible_control := .as_logical(eligible_control)]

  # Unique people in the base source population (n).
  n_total <- dt[, data.table::uniqueN(person_id)]

  # Exposed pathway in the shell.
  # n_e1: individuals receiving BNT162b2 in enrollment window.
  n_e1 <- dt[RECEIVES_EXPOSURE == TRUE, data.table::uniqueN(person_id)]
  # n_e2: first-ever COVID vaccination among exposed candidates.
  n_e2 <- dt[RECEIVES_EXPOSURE == TRUE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE, data.table::uniqueN(person_id)]
  # n_e3a to n_e3e: progressively apply exposed eligibility constraints.
  n_e3a <- dt[RECEIVES_EXPOSURE == TRUE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE, data.table::uniqueN(person_id)]
  n_e3b <- dt[
    RECEIVES_EXPOSURE == TRUE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE & ENROLLED_6_MONTHS == TRUE,
    data.table::uniqueN(person_id)
  ]
  n_e3c <- dt[
    RECEIVES_EXPOSURE == TRUE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE & ENROLLED_6_MONTHS == TRUE & SV_HOSP_6WEEKS == FALSE,
    data.table::uniqueN(person_id)
  ]
  n_e3d <- dt[
    RECEIVES_EXPOSURE == TRUE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE & ENROLLED_6_MONTHS == TRUE & SV_HOSP_6WEEKS == FALSE &
      SV_PRIOR_PREP_COVID == FALSE,
    data.table::uniqueN(person_id)
  ]
  n_e3e <- dt[
    RECEIVES_EXPOSURE == TRUE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE & ENROLLED_6_MONTHS == TRUE & SV_HOSP_6WEEKS == FALSE &
      SV_PRIOR_PREP_COVID == FALSE & SV_IMMUNOCOMPROMISED == FALSE,
    data.table::uniqueN(person_id)
  ]

  # Unexposed pathway in the shell.
  # n_c1: unvaccinated window anchor for control candidates.
  n_c1 <- dt[RECEIVES_EXPOSURE == FALSE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE, data.table::uniqueN(person_id)]
  # n_c2a to n_c2e: progressively apply control eligibility constraints.
  n_c2a <- dt[RECEIVES_EXPOSURE == FALSE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE, data.table::uniqueN(person_id)]
  n_c2b <- dt[
    RECEIVES_EXPOSURE == FALSE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE & ENROLLED_6_MONTHS == TRUE,
    data.table::uniqueN(person_id)
  ]
  n_c2c <- dt[
    RECEIVES_EXPOSURE == FALSE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE & ENROLLED_6_MONTHS == TRUE & SV_HOSP_6WEEKS == FALSE,
    data.table::uniqueN(person_id)
  ]
  n_c2d <- dt[
    RECEIVES_EXPOSURE == FALSE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE & ENROLLED_6_MONTHS == TRUE & SV_HOSP_6WEEKS == FALSE &
      SV_PRIOR_PREP_COVID == FALSE,
    data.table::uniqueN(person_id)
  ]
  n_c2e <- dt[
    RECEIVES_EXPOSURE == FALSE & PREV_OTHER_COVID_VAC == FALSE & PRIOR_PFIZER_VAC == FALSE & AGE_16_OR_OLDER == TRUE & ENROLLED_6_MONTHS == TRUE & SV_HOSP_6WEEKS == FALSE &
      SV_PRIOR_PREP_COVID == FALSE & SV_IMMUNOCOMPROMISED == FALSE,
    data.table::uniqueN(person_id)
  ]

  n_exposed <- dt[eligible_exposed == TRUE, data.table::uniqueN(person_id)] # Final exposed total after full eligibility logic.
  n_control <- dt[eligible_control == TRUE, data.table::uniqueN(person_id)] # Final unexposed total after full eligibility logic.

  # Build shell-aligned row labels and values in final reporting order.
  table_1a <- data.table::data.table(
    Label = c(
      "All individuals registered in the healthcare data source at any time between 01 June 2020 and 30 April 2022",
      "  AND received BNT162b2 mRNA COVID-19 vaccine during the enrolment period 01 December 2020 - 31 January 2022",
      "    AND received this as their first ever COVID vaccination",
      "      AND is aged 16 or older",
      "        AND is enrolled in the data source at least 6 months prior to receiving the vaccine",
      "          AND does not have stable pre-existing conditions, or if they do, has no record of hospitalization in the 6 weeks prior to vaccination",
      "            AND have not received any COVID-19 preventative medication or PrEP prior to index date",
      "              AND do not have an immunocompromised status",
      "Total Eligible Exposed",
      "  AND have at least one day during the enrolment period 01 December 2020 - 31 January 2022 in which they have not yet received any COVID-19 vaccination",
      "    AND is aged 16 or older on that day",
      "      AND is enrolled in the data source at least 6 months prior to that day",
      "        AND does not have stable pre-existing conditions, or if they do, has no record of hospitalization in the 6 weeks prior to that day",
      "          AND have not received any COVID-19 preventative medication or PrEP prior to that date",
      "            AND do not have an immunocompromised status",
      "Total Eligible Unexposed"
    ),
    N = c(
      .format_count(n_total), # Row 1 count.
      .format_count(n_e1), # Row 2 count.
      .format_count(n_e2), # Row 3 count.
      .format_count(n_e3a), # Row 4 count.
      .format_count(n_e3b), # Row 5 count.
      .format_count(n_e3c), # Row 6 count.
      .format_count(n_e3d), # Row 7 count.
      .format_count(n_e3e), # Row 8 count.
      .format_count(n_exposed), # Row 9 count.
      .format_count(n_c1), # Row 10 count.
      .format_count(n_c2a), # Row 11 count.
      .format_count(n_c2b), # Row 12 count.
      .format_count(n_c2c), # Row 13 count.
      .format_count(n_c2d), # Row 14 count.
      .format_count(n_c2e), # Row 15 count.
      .format_count(n_control) # Row 16 count.
    ),
    Percentage = c(
      # Percentages follow shell denominators:
      # - exposed branch uses n (row 1), then n_e1, then n_e2
      # - unexposed branch uses n (row 1), then n_c1
      NA_character_, # Row 1 percentage intentionally blank.
      .pct(n_e1, n_total, digits), # Row 2: n_e1 / n.
      .pct(n_e2, n_total, digits), # Row 3: n_e2 / n_e1.
      .pct(n_e3a, n_total, digits), # Row 4: n_e3a / n_e2.
      .pct(n_e3b, n_total, digits), # Row 5: n_e3b / n_e2.
      .pct(n_e3c, n_total, digits), # Row 6: n_e3c / n_e2.
      .pct(n_e3d, n_total, digits), # Row 7: n_e3d / n_e2.
      .pct(n_e3e, n_total, digits), # Row 8: n_e3e / n_e2.
      .pct(n_exposed, n_total, digits), # Row 9: n_exposed / n.
      .pct(n_c1, n_total, digits), # Row 10: n_c1 / n.
      .pct(n_c2a, n_total, digits), # Row 11: n_c2a / n_c1.
      .pct(n_c2b, n_total, digits), # Row 12: n_c2b / n_c1.
      .pct(n_c2c, n_total, digits), # Row 13: n_c2c / n_c1.
      .pct(n_c2d, n_total, digits), # Row 14: n_c2d / n_c1.
      .pct(n_c2e, n_total, digits), # Row 15: n_c2e / n_c1.
      .pct(n_control, n_total, digits) # Row 16: n_control / n.
    ),
    Aux = NA_character_
  )

  .finalize_attrition_table(table_1a, return_na = return_na)
}

#' Table 1(b): Matching statistics for the matched cohort main analysis.
#'
#' @param d3_eligibility D3 eligibility dataset.
#' @param d4_main D4 matched cohort for the main analysis.
#' @param digits Number of decimals for percentages.
#' @param return_na Keep NA values if TRUE; otherwise replace with empty strings.
#'
#' @return data.table with columns Label, N, Percentage, Aux.
create_attrition_table_1b <- function(d3_eligibility, d4_main, digits = 1, return_na = FALSE) {
  # Copy inputs to preserve caller state and coerce to data.table.
  d3 <- data.table::as.data.table(data.table::copy(d3_eligibility))
  d4 <- data.table::as.data.table(data.table::copy(d4_main))

  .assert_required_cols(d3, c("person_id", "eligible_exposed"), "d3_eligibility")
  .assert_required_cols(d4, c("person_id", "group"), "d4_main")

  d3[, eligible_exposed := .as_logical(eligible_exposed)]
  d4[, group := as.character(group)]

  # Use eligible exposed as the cohort denominator for main matching metrics.
  n_cohort <- d3[eligible_exposed == TRUE | eligible_control == TRUE, data.table::uniqueN(person_id)] # Eligible exposed denominator from D3.
  n_matched_exp <- d4[group == "EXPOSED", .N] # Number of matched exposed rows in D4 main.
  n_matched_con <- d4[group == "CONTROL", .N] # Number of matched control rows in D4 main.
  n_unmatched_exp <- d4[group == "UNMATCHED", .N] # Number of unmatched exposed rows in D4 main.
  n_matched <- n_matched_exp + n_matched_con # Total matched rows (exposed + control).

  table_1b <- data.table::data.table(
    Label = c(
      "Matched cohort",
      "  Exposed matched",
      "  Control matched individuals",
      "Exposed unmatched"
    ),
    N = c(
      .format_count(n_matched), # Row 1 count.
      .format_count(n_matched_exp), # Row 2 count.
      .format_count(n_matched_con), # Row 3 count.
      .format_count(n_unmatched_exp) # Row 4 count.
    ),
    Percentage = c(
      NA_character_, # Row 1 percentage intentionally blank.
      .pct(n_matched_exp, n_matched, digits), # Row 2: n_matched_exp / n_cohort.
      .pct(n_matched_con, n_matched, digits), # Row 3: n_matched_con / n_cohort.
      NA_character_ # Row 4: n_unmatched_exp
    ),
    Aux = NA_character_
  )

  .finalize_attrition_table(table_1b, return_na = return_na)
}

#' Table 1(c): Matching statistics for the matched cohort supplementary analysis.
#'
#' @param d4_supplementary D4 matched cohort for supplementary analysis.
#' @param digits Number of decimals for percentages.
#' @param return_na Keep NA values if TRUE; otherwise replace with empty strings.
#'
#' @return data.table with columns Label, N, Percentage, Aux.
create_attrition_table_1c <- function(d4_supplementary, digits = 1, return_na = FALSE) {
  # Copy input to avoid by-reference modifications.
  d4 <- data.table::as.data.table(data.table::copy(d4_supplementary))

  .assert_required_cols(d4, c("person_id", "group"), "d4_supplementary")
  d4[, group := as.character(group)]

  n_matched_exp <- d4[group == "EXPOSED", .N] # Matched exposed rows in supplementary cohort.
  n_matched_con <- d4[group == "CONTROL", .N] # Matched control rows in supplementary cohort.
  n_unmatched_exp <- d4[group == "UNMATCHED", .N] # Unmatched exposed rows in supplementary cohort.
  n_matched <- n_matched_exp + n_matched_con # Total matched rows (exposed + control).

  # Exposed who later appear as control represent control-before-exposure reuse.
  exp_ids <- unique(d4[group == "EXPOSED", person_id]) # Unique exposed person IDs.
  control_ids <- unique(d4[group == "CONTROL", person_id]) # Unique control person IDs.
  n_exp_used_as_control <- length(intersect(exp_ids, control_ids)) # Exposed IDs reused as controls before exposure.

  # Count how many times each control person is selected in matching.
  control_match_counts <- d4[group == "CONTROL", .(N = .N), by = person_id] # Per-control reuse frequency.
  n_unique_control <- control_match_counts[, data.table::uniqueN(person_id)] # Number of distinct controls.

  # Summarize control reuse distribution for shell rows.
  median_n <- if (n_unique_control == 0) NA_real_ else stats::median(control_match_counts$N) # Median reuse count.
  q1_n <- if (n_unique_control == 0) NA_real_ else stats::quantile(control_match_counts$N, 0.25, names = FALSE) # 25th percentile reuse count.
  q3_n <- if (n_unique_control == 0) NA_real_ else stats::quantile(control_match_counts$N, 0.75, names = FALSE) # 75th percentile reuse count.
  min_n <- if (n_unique_control == 0) NA_real_ else min(control_match_counts$N) # Minimum reuse count.
  max_n <- if (n_unique_control == 0) NA_real_ else max(control_match_counts$N) # Maximum reuse count.

  freq_1 <- if (n_unique_control == 0) 0L else control_match_counts[N == 1, .N] # Controls selected exactly once.
  freq_2 <- if (n_unique_control == 0) 0L else control_match_counts[N == 2, .N] # Controls selected exactly twice.
  freq_3 <- if (n_unique_control == 0) 0L else control_match_counts[N == 3, .N] # Controls selected exactly three times.
  freq_4 <- if (n_unique_control == 0) 0L else control_match_counts[N == 4, .N] # Controls selected exactly four times.
  freq_5p <- if (n_unique_control == 0) 0L else control_match_counts[N >= 5, .N] # Controls selected five or more times.

  # Output rows mirror the supplementary shell structure.
  table_1c <- data.table::data.table(
    Label = c(
      "Matched cohort",
      "  Exposed matched",
      "    Served as control before exposure",
      "  Control matched individuals",
      "    Number of distinct control individuals matched",
      "    Number of times control individuals selected for matching",
      "      Median (Q1-Q3)",
      "      Min-Max",
      "      1 time, n (%)",
      "      2 times, n (%)",
      "      3 times, n (%)",
      "      4 times, n (%)",
      "      5 times or more, n (%)",
      "  Exposed unmatched"
    ),
    N = c(
      .format_count(n_matched), # Row 1 count.
      .format_count(n_matched_exp), # Row 2 count.
      .format_count(n_exp_used_as_control), # Row 3 count.
      .format_count(n_matched_con), # Row 4 count.
      .format_count(n_unique_control), # Row 5 count.
      NA_character_, # Row 6 count intentionally blank (section header).
      .format_count(round(median_n, 1)), # Row 7 median value.
      .format_count(min_n), # Row 8 minimum value.
      .format_count(freq_1), # Row 9 count.
      .format_count(freq_2), # Row 10 count.
      .format_count(freq_3), # Row 11 count.
      .format_count(freq_4), # Row 12 count.
      .format_count(freq_5p), # Row 13 count.
      .format_count(n_unmatched_exp) # Row 14 count.
    ),
    Percentage = c(
      NA_character_, # Row 1 percentage intentionally blank.
      .pct(n_matched_exp, n_matched, digits), # Row 2: n_matched_exp / n_matched.
      .pct(n_exp_used_as_control, n_matched, digits), # Row 3: n_exp_used_as_control / n_matched.
      .pct(n_matched_con, n_matched, digits), # Row 4: n_matched_con / n_matched.
      .pct(n_unique_control, n_matched_con, digits), # Row 5: n_unique_control / n_matched_con.
      NA_character_, # Row 6 percentage intentionally blank (section header).
      if (is.na(q1_n)) NA_character_ else .format_count(round(q1_n, 1)), # Row 7 Q1 value placed in Percentage column.
      if (is.na(max_n)) NA_character_ else .format_count(max_n), # Row 8 max value placed in Percentage column.
      .pct(freq_1, n_unique_control, digits), # Row 9: freq_1 / n_unique_control.
      .pct(freq_2, n_unique_control, digits), # Row 10: freq_2 / n_unique_control.
      .pct(freq_3, n_unique_control, digits), # Row 11: freq_3 / n_unique_control.
      .pct(freq_4, n_unique_control, digits), # Row 12: freq_4 / n_unique_control.
      .pct(freq_5p, n_unique_control, digits), # Row 13: freq_5p / n_unique_control.
      NA_character_ # Row 14 percentage intentionally blank.
    ),
    Aux = c(
      NA_character_, # Row 1 Aux.
      NA_character_, # Row 2 Aux.
      NA_character_, # Row 3 Aux.
      NA_character_, # Row 4 Aux.
      NA_character_, # Row 5 Aux.
      NA_character_, # Row 6 Aux.
      if (is.na(q3_n)) NA_character_ else .format_count(round(q3_n, 1)), # Row 7 Q3 value.
      NA_character_, # Row 8 Aux.
      NA_character_, # Row 9 Aux.
      NA_character_, # Row 10 Aux.
      NA_character_, # Row 11 Aux.
      NA_character_, # Row 12 Aux.
      NA_character_, # Row 13 Aux.
      NA_character_ # Row 14 Aux.
    )
  )

  .finalize_attrition_table(table_1c, return_na = return_na)
}
