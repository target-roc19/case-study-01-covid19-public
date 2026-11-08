#' Prepare D4 study cohort inputs
#'
#' Internal helper that returns two cohort variants used by downstream builders:
#' the full cohort output (`cohort_out`) and the study-only subset restricted to
#' EXPOSED/CONTROL rows (`cohort_study`).
#'
#' @param analysis_label Character scalar used in log messages.
#' @param n_pairs Integer scalar number of matched exposed/control pairs when
#'   simulating a cohort.
#' @param start_date Date scalar earliest simulated T0 when simulating a
#'   cohort.
#' @param cohort_data Optional existing cohort object. When supplied, no new
#'   cohort is simulated.
#'
#' @return Named list with `cohort_out` and `cohort_study`.
#' @noRd
prepare_d4_study_cohort <- function(
  analysis_label,
  n_pairs = 500L,
  start_date = as.Date("2021-01-01"),
  cohort_data = NULL
) {
  # Required fields for downstream FUP/TV generation.
  required_cohort_cols <- c(
    "person_id",
    "T0",
    "group",
    "SV_SEX",
    "SV_REGION",
    "SV_SES",
    "SV_PREG_STATUS",
    "SV_CDC_RISK",
    "SV_HU_PC_CONTACT",
    "SV_HU_HOSP_CONTACT",
    "SV_SMOKING"
  )

  if (is.null(cohort_data)) {
    # Simulate EXPOSED/CONTROL matched pairs plus UNMATCHED rows.
    n_matched <- n_pairs * 2L
    n_unmatched <- n_pairs

    cohort_matched <- data.table::data.table(
      person_id = as.character(seq_len(n_matched)),
      T0 = start_date + sample(0:180, n_matched, replace = TRUE),
      group = rep(c("EXPOSED", "CONTROL"), times = n_pairs),
      match_id = rep(seq_len(n_pairs), each = 2L),
      boot_id = 0L
    )

    # Keep one match_id per UNMATCHED row to satisfy downstream schema checks.
    cohort_unmatched <- data.table::data.table(
      person_id = as.character((n_matched + 1L):(n_matched + n_unmatched)),
      T0 = start_date + sample(0:180, n_unmatched, replace = TRUE),
      group = "UNMATCHED",
      match_id = n_pairs + seq_len(n_unmatched),
      boot_id = 0L
    )

    cohort <- data.table::rbindlist(list(cohort_matched, cohort_unmatched), use.names = TRUE)

    # Simulate baseline covariates with simple, fixed distributions.
    cohort[, SV_SEX := sample(c("female", "male"), .N, replace = TRUE, prob = c(0.53, 0.47))]
    cohort[, SV_REGION := sample(paste0("REG_", 1:5), .N, replace = TRUE)]
    cohort[, SV_SES := sample(0:5, .N, replace = TRUE)]
    cohort[, SV_PREG_STATUS := group == "EXPOSED" & SV_SEX == "female" & rbinom(.N, 1L, 0.08) == 1L]
    cohort[, SV_CDC_RISK := sample(0:2, .N, replace = TRUE, prob = c(0.50, 0.30, 0.20))]
    # cohort[, SV_HU_CONTACT := rbinom(.N, 1L, 0.45) == 1L]
    cohort[, SV_HU_PC_CONTACT := rbinom(.N, 1L, 0.45) == 1L]
    cohort[, SV_HU_HOSP_CONTACT := rbinom(.N, 1L, 0.45) == 1L]
    cohort[, SV_SMOKING := sample(0:4, .N, replace = TRUE, prob = c(0.50, 0.20, 0.10, 0.10, 0.10))]

    # Keep only D4_StudyCohort output columns.
    cohort_out <- data.table::copy(cohort[, .(
      person_id,
      T0,
      group,
      match_id,
      boot_id,
      SV_SEX,
      SV_REGION,
      SV_SES,
      SV_PREG_STATUS,
      SV_CDC_RISK,
      # SV_HU_CONTACT,
      SV_HU_PC_CONTACT,
      SV_HU_HOSP_CONTACT,
      SV_SMOKING
    )])

    # For simulated data, full cohort and study subset are identical.
    return(list(cohort_out = cohort_out, cohort_study = cohort_out))
  }

  # Use provided cohort and normalize into a data.table.
  cohort_out <- data.table::as.data.table(data.table::copy(cohort_data))

  # Backfill optional columns if absent in the incoming cohort.
  if (!"match_id" %in% names(cohort_out)) {
    cohort_out[, match_id := NA_integer_]
  }
  if (!"boot_id" %in% names(cohort_out)) {
    cohort_out[, boot_id := 0L]
  }

  required_core_cols <- c("person_id", "T0", "group")
  missing_core_cols <- setdiff(required_core_cols, names(cohort_out))
  if (length(missing_core_cols) > 0) {
    stop(paste0(
      "Existing cohort is missing required column(s): ",
      paste(missing_core_cols, collapse = ", "),
      "."
    ))
  }

  # Backfill optional baseline covariates when absent in existing cohort input.
  if (!"SV_SEX" %in% names(cohort_out)) {
    cohort_out[, SV_SEX := sample(c("female", "male"), .N, replace = TRUE, prob = c(0.53, 0.47))]
  }
  if (!"SV_REGION" %in% names(cohort_out)) {
    cohort_out[, SV_REGION := sample(paste0("REG_", 1:5), .N, replace = TRUE)]
  }
  if (!"SV_SES" %in% names(cohort_out)) {
    cohort_out[, SV_SES := sample(0:5, .N, replace = TRUE)]
  }
  if (!"SV_CDC_RISK" %in% names(cohort_out)) {
    cohort_out[, SV_CDC_RISK := sample(0:2, .N, replace = TRUE, prob = c(0.50, 0.30, 0.20))]
  }
  # if (!"SV_HU_CONTACT" %in% names(cohort_out)) {
  #   cohort_out[, SV_HU_CONTACT := rbinom(.N, 1L, 0.45) == 1L]
  # }
  if (!"SV_SMOKING" %in% names(cohort_out)) {
    cohort_out[, SV_SMOKING := sample(0:4, .N, replace = TRUE, prob = c(0.50, 0.20, 0.10, 0.10, 0.10))]
  }
  # if (!"SV_CCI" %in% names(cohort_out)) {
  #   cohort_out[, SV_CCI := sample(0:2, .N, replace = TRUE, prob = c(0.52, 0.32, 0.16))]
  # }
  if (!"SV_PREG_STATUS" %in% names(cohort_out)) {
    cohort_out[, SV_PREG_STATUS := group == "EXPOSED" & SV_SEX == "female" & rbinom(.N, 1L, 0.08) == 1L]
  }

  # Normalize T0 to Date regardless of input representation.
  if (!inherits(cohort_out$T0, "Date")) {
    if (is.numeric(cohort_out$T0)) {
      cohort_out[, T0 := as.Date(T0, origin = "1970-01-01")]
    } else {
      cohort_out[, T0 := as.Date(T0)]
    }
  }

  # Normalize identifier type and keep EXPOSED/CONTROL for follow-up generation.
  cohort_out[, person_id := as.character(person_id)]
  cohort_study <- data.table::copy(cohort_out[group %in% c("EXPOSED", "CONTROL")])

  if (nrow(cohort_study) == 0L) {
    stop("Existing cohort does not contain any EXPOSED/CONTROL rows.")
  }

  if (nrow(cohort_study) < nrow(cohort_out)) {
    logger::log_info(paste(
      "Using existing cohort for", analysis_label,
      "| retained study rows:", nrow(cohort_study),
      "| skipped non-study rows:", nrow(cohort_out) - nrow(cohort_study)
    ))
  }

  return(list(cohort_out = cohort_out, cohort_study = cohort_study))
}

#' Build D4_StudyCohortFup from study cohort rows
#'
#' Generates event dates, censoring reasons, follow-up lengths, and estimand-
#' specific IPCW fields for each study row.
#'
#' @param cohort_study Data compatible with study-cohort rows (EXPOSED/CONTROL).
#' @param max_fup_days Integer scalar maximum follow-up horizon in days.
#'
#' @return data.table compatible with `D4_StudyCohortFup`.
#' @noRd
build_d4_study_cohort_fup <- function(cohort_study, max_fup_days = 90L) {
  # Work on a copy to avoid mutating caller data.
  cohort <- data.table::copy(data.table::as.data.table(cohort_study))

  # Simulate adherence-related IE1 date among exposed participants.
  cohort[, ie1_second_dose_target_exposed := as.Date(NA)]
  compliant_idx <- cohort[group == "EXPOSED", sample(c(TRUE, FALSE), .N, replace = TRUE, prob = c(0.80, 0.20))]
  cohort[group == "EXPOSED", ie1_second_dose_target_exposed := data.table::fifelse(
    compliant_idx,
    T0 + sample(21:45, .N, replace = TRUE),
    as.Date(NA)
  )]

  # Compliance definition: exposed requires recorded second target dose;
  # controls are always marked compliant by assumption.
  cohort[, compliant_exposed := data.table::fifelse(
    group == "EXPOSED",
    !is.na(ie1_second_dose_target_exposed),
    group == "CONTROL"
  )]

  # Build simple person-level risks based on treatment and baseline risk factors.
  cohort[, risk_covid := data.table::fifelse(group == "EXPOSED", 0.08, 0.11) + 0.010 * SV_CDC_RISK]
  cohort[, risk_death := 0.010 + 0.004 * SV_CDC_RISK]
  cohort[, risk_censor := 0.04 + 0.015 * (SV_HU_PC_CONTACT)]
  cohort[, risk_ie3 := data.table::fifelse(group == "EXPOSED", 0.05, 0.07)]
  cohort[, risk_ie5 := 0.02 + 0.005 * SV_CDC_RISK]

  # Simulate outcome/censoring dates within the follow-up window.
  cohort[, covid_date := data.table::fifelse(
    rbinom(.N, 1L, pmin(risk_covid, 0.85)) == 1L,
    T0 + sample(seq_len(max_fup_days) - 1L, .N, replace = TRUE),
    as.Date(NA)
  )]
  cohort[, death_date := data.table::fifelse(
    rbinom(.N, 1L, pmin(risk_death, 0.25)) == 1L,
    T0 + sample(seq_len(max_fup_days) - 1L, .N, replace = TRUE),
    as.Date(NA)
  )]
  cohort[, loss_to_followup := data.table::fifelse(
    rbinom(.N, 1L, pmin(risk_censor, 0.30)) == 1L,
    T0 + sample(seq_len(max_fup_days) - 1L, .N, replace = TRUE),
    as.Date(NA)
  )]

  # Simulate intercurrent event dates (IE3, IE5).
  cohort[, ie3_date := data.table::fifelse(
    rbinom(.N, 1L, pmin(risk_ie3, 0.35)) == 1L,
    T0 + sample(seq_len(max_fup_days) - 1L, .N, replace = TRUE),
    as.Date(NA)
  )]
  cohort[, ie5_date := data.table::fifelse(
    rbinom(.N, 1L, pmin(risk_ie5, 0.20)) == 1L,
    T0 + sample(seq_len(max_fup_days) - 1L, .N, replace = TRUE),
    as.Date(NA)
  )]

  # Map arm-specific IE3 columns expected by downstream code.
  cohort[, ie3_first_dose_covid_nontarget_exposed := as.Date(NA)]
  cohort[group == "EXPOSED", ie3_first_dose_covid_nontarget_exposed := ie3_date]
  cohort[, ie3_first_dose_covid_any_control := as.Date(NA)]
  cohort[group == "CONTROL", ie3_first_dose_covid_any_control := ie3_date]
  cohort[, ie5_prep_treatment := ie5_date]

  # Add administrative follow-up boundaries.
  cohort[, max_fup_date := T0 + (max_fup_days - 1L)]
  cohort[, end_study_period := as.Date("2022-04-30")]
  cohort[, end_of_data_collection := pmin(end_study_period, T0 + sample(95:140, .N, replace = TRUE))]

  # Utility: pick earliest non-missing date.
  select_first_date <- function(...) {
    vals <- c(...)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) {
      return(as.Date(NA))
    }
    as.Date(min(vals), origin = "1970-01-01")
  }

  # Compute earliest follow-up end date for estimand 2.
  cohort[, general_end_fup_est2 := as.Date(vapply(seq_len(.N), function(i) {
    as.numeric(select_first_date(
      covid_date[i],
      death_date[i],
      loss_to_followup[i],
      max_fup_date[i],
      end_study_period[i],
      end_of_data_collection[i],
      ie3_first_dose_covid_nontarget_exposed[i],
      ie3_first_dose_covid_any_control[i],
      ie5_prep_treatment[i]
    ))
  }, numeric(1)), origin = "1970-01-01")]

  # Resolve reason hierarchy for follow-up stop.
  cohort[, reason_censoring_est2 := data.table::fifelse(
    !is.na(covid_date) & covid_date == general_end_fup_est2, "covid_date",
    data.table::fifelse(
      !is.na(death_date) & death_date == general_end_fup_est2, "death_date",
      data.table::fifelse(
        !is.na(loss_to_followup) & loss_to_followup == general_end_fup_est2, "loss_to_followup",
        data.table::fifelse(
          !is.na(ie3_first_dose_covid_nontarget_exposed) & ie3_first_dose_covid_nontarget_exposed == general_end_fup_est2,
          "ie3_first_dose_covid_nontarget_exposed",
          data.table::fifelse(
            !is.na(ie3_first_dose_covid_any_control) & ie3_first_dose_covid_any_control == general_end_fup_est2,
            "ie3_first_dose_covid_any_control",
            data.table::fifelse(
              !is.na(ie5_prep_treatment) & ie5_prep_treatment == general_end_fup_est2, "ie5_prep_treatment",
              data.table::fifelse(
                max_fup_date == general_end_fup_est2, "max_fup_date",
                data.table::fifelse(end_of_data_collection == general_end_fup_est2, "end_of_data_collection", "end_study_period")
              )
            )
          )
        )
      )
    )
  )]

  # Estimand 1 shares follow-up end but treats death as outcome.
  cohort[, general_end_fup_est1 := general_end_fup_est2]
  cohort[, reason_censoring_est1 := reason_censoring_est2]
  cohort[, event_count_est1 := as.integer(reason_censoring_est1 %in% c("covid_date", "death_date"))]
  cohort[, fup_length_days_est1 := pmax(as.integer(general_end_fup_est1 - T0 + 1L), 0L)]
  cohort[, fup_length_pyr_est1 := fup_length_days_est1 / 365.25]
  cohort[, censor_nonadmin_ipcw := as.integer(
    reason_censoring_est1 %in% c(
      "ie3_first_dose_covid_nontarget_exposed",
      "ie3_first_dose_covid_any_control",
      "ie5_prep_treatment",
      "loss_to_followup"
    )
  )]
  cohort[, censor_admin := as.integer(
    reason_censoring_est1 %in% c(
      "max_fup_date"
    )
  )]

  # Estimand 2 outcome counts only COVID event as outcome.
  cohort[, event_count_est2 := as.integer(reason_censoring_est2 == "covid_date")]
  cohort[, fup_length_days_est2 := pmax(as.integer(general_end_fup_est2 - T0 + 1L), 0L)]
  cohort[, fup_length_pyr_est2 := fup_length_days_est2 / 365.25]

  # Select the FUP output schema expected downstream.
  # Matching/baseline covariates (SV_*) are intentionally excluded; retrieve
  # them from D4_StudyCohort when needed.
  fup <- data.table::copy(cohort[, .(
    person_id,
    T0,
    group,
    match_id,
    end_study_period,
    end_of_data_collection,
    covid_date,
    death_date,
    loss_to_followup,
    ie1_second_dose_target_exposed,
    compliant_exposed,
    ie3_first_dose_covid_nontarget_exposed,
    ie3_first_dose_covid_any_control,
    ie5_prep_treatment,
    max_fup_date,
    general_end_fup_est1,
    reason_censoring_est1,
    event_count_est1,
    fup_length_days_est1,
    fup_length_pyr_est1,
    censor_nonadmin_ipcw,
    censor_admin,
    general_end_fup_est2,
    reason_censoring_est2,
    event_count_est2,
    fup_length_days_est2,
    fup_length_pyr_est2
  )])

  return(fup)
}

#' Build D4_StudyCohortTimeVaryingCovariates from FUP data
#'
#' Expands each person into person-day rows and derives IPCW time-varying
#' indicators for both estimands.
#'
#' @param fup_data Data compatible with `D4_StudyCohortFup`.
#' @param cohort_data Data compatible with `D4_StudyCohort`. Required to
#'   retrieve baseline covariates (`SV_CDC_RISK`, `SV_CCI`, `SV_HU_CONTACT`)
#'   which are no longer stored in `D4_StudyCohortFup`.
#'
#' @return data.table compatible with `D4_StudyCohortTimeVaryingCovariates`.
#' @noRd
build_d4_study_cohort_time_varying_covariates <- function(fup_data, cohort_data) {
  # Work on a copy to preserve caller object.
  fup <- data.table::as.data.table(data.table::copy(fup_data))
  cohort <- data.table::as.data.table(cohort_data)

  # Expand each row to person-day granularity up to estimand-2 follow-up length.
  tv <- fup[, .(time_days = seq_len(fup_length_days_est2)), by = .(person_id, T0, group, match_id)]

  # Bring follow-up end markers and censoring indicators onto time-varying rows.
  tv[fup, fup_end := i.fup_length_days_est2, on = .(person_id, T0, group, match_id)]
  tv[fup, fup_end_est1 := i.fup_length_days_est1, on = .(person_id, T0, group, match_id)]
  tv[fup, censor_nonadmin_ipcw := i.censor_nonadmin_ipcw, on = .(person_id, T0, group, match_id)]

  # Retrieve baseline covariates needed for time-varying simulation from
  # D4_StudyCohort (not FUP, which does not carry matching variables).
  tv[cohort, `:=`(
    SV_CDC_RISK = i.SV_CDC_RISK,
    # SV_CCI = i.SV_CCI,
    SV_HU_PC_CONTACT = i.SV_HU_PC_CONTACT
  ), on = .(person_id, T0, group, match_id)]

  # Mark non-administrative censoring at the end of follow-up.
  tv[, censor_nonadmin := as.integer(time_days == fup_end_est1 & censor_nonadmin_ipcw == 1L)]

  # Simulate time-varying hospitalization indicators.
  tv[, SV_HOSP_LENGTH_TV := as.integer(rbinom(.N, 1L, pmin(0.02 + 0.02 * SV_CDC_RISK, 0.35)) == 1L)]
  tv[, SV_HOSP_LENGTH_TV := as.integer(rbinom(.N, 1L, pmin(0.02 + 0.02 * SV_CDC_RISK, 0.35)) == 1L)]

  # Simulate incident immunosuppressing condition as absorbing process.
  tv[, onset_flag := rbinom(.N, 1L, pmin(0.001 + 0.002 * SV_HU_PC_CONTACT + 0.002 * SV_CDC_RISK, 0.05))]
  tv[, SV_IMMUNOCOMPROMISED_TV := as.integer(cumsum(onset_flag) >= 1L), by = .(person_id, T0)]
  tv[, SV_IMMUNOCOMPROMISED_TV := as.integer(cumsum(onset_flag) >= 1L), by = .(person_id, T0)]

  # Drop intermediate helper columns from final TV output.
  tv[, c("fup_end", "fup_end_est1", "censor_nonadmin_ipcw", "SV_CDC_RISK", "SV_HU_PC_CONTACT", "onset_flag") := NULL]

  return(tv)
}

#' Simulate D4_StudyCohort
#'
#' @param analysis_label Character scalar. Used only for logging.
#' @param seed Integer scalar random seed. If `NULL`, does not reset RNG state.
#' @param n_pairs Integer scalar number of matched exposed/control pairs.
#' @param start_date Date scalar earliest T0 used in the simulation.
#' @param cohort_data Optional existing `D4_StudyCohort` object.
#'
#' @return data.table compatible with `D4_StudyCohort`.
#' @noRd
simulate_d4_study_cohort <- function(
  analysis_label,
  seed = 42L,
  n_pairs = 500L,
  start_date = as.Date("2021-01-01"),
  cohort_data = NULL
) {
  # Allow caller to preserve RNG state by passing seed = NULL.
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Build/validate cohort and return only D4_StudyCohort shape.
  prep <- prepare_d4_study_cohort(
    analysis_label = analysis_label,
    n_pairs = n_pairs,
    start_date = start_date,
    cohort_data = cohort_data
  )

  return(prep$cohort_out)
}

#' Simulate D4_StudyCohortFup
#'
#' @param analysis_label Character scalar. Used only for logging.
#' @param seed Integer scalar random seed. If `NULL`, does not reset RNG state.
#' @param n_pairs Integer scalar number of matched exposed/control pairs.
#' @param max_fup_days Integer scalar maximum follow-up duration.
#' @param start_date Date scalar earliest T0 used in the simulation.
#' @param cohort_data Optional existing `D4_StudyCohort` object.
#'
#' @return data.table compatible with `D4_StudyCohortFup`.
#' @noRd
simulate_d4_study_cohort_fup <- function(
  analysis_label,
  seed = 42L,
  n_pairs = 500L,
  max_fup_days = 90L,
  start_date = as.Date("2021-01-01"),
  cohort_data = NULL
) {
  # Allow caller to preserve RNG state by passing seed = NULL.
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Build/validate cohort and return only D4_StudyCohortFup shape.
  prep <- prepare_d4_study_cohort(
    analysis_label = analysis_label,
    n_pairs = n_pairs,
    start_date = start_date,
    cohort_data = cohort_data
  )

  return(build_d4_study_cohort_fup(
    cohort_study = prep$cohort_study,
    max_fup_days = max_fup_days
  ))
}

#' Simulate D4_StudyCohortTimeVaryingCovariates
#'
#' @param fup_data Data compatible with `D4_StudyCohortFup`.
#' @param cohort_data Data compatible with `D4_StudyCohort`. Required to
#'   retrieve baseline covariates (`SV_CDC_RISK`, `SV_CCI`, `SV_HU_CONTACT`)
#'   which are no longer stored in `D4_StudyCohortFup`.
#' @param seed Integer scalar random seed. If `NULL`, does not reset RNG state.
#'
#' @return data.table compatible with `D4_StudyCohortTimeVaryingCovariates`.
#' @noRd
simulate_d4_study_cohort_time_varying_covariates <- function(
  fup_data,
  cohort_data,
  seed = 42L
) {
  # Allow caller to preserve RNG state by passing seed = NULL.
  if (!is.null(seed)) {
    set.seed(seed)
  }

  return(build_d4_study_cohort_time_varying_covariates(
    fup_data = fup_data,
    cohort_data = cohort_data
  ))
}

#' Parse expected category declarations from metadata
#'
#' Converts metadata `expectedCat` values into explicit category vectors,
#' supporting comma-separated values and selected `get ...` placeholders.
#'
#' @param expected_cat Character scalar from metadata `expectedCat`.
#' @param var_name Optional variable name used for variable-specific defaults.
#' @param t0 Optional Date vector used to derive calendar quarter categories.
#'
#' @return Character vector of parsed category labels.
#'
#' @noRd
parse_expected_categories <- function(expected_cat, var_name = NULL, t0 = NULL) {
  # Missing/empty metadata category declaration.
  if (is.null(expected_cat) || length(expected_cat) == 0L || is.na(expected_cat) || expected_cat == "NA") {
    # Fallback rule for calendar quarter when explicit categories are absent.
    if (!is.null(var_name) && identical(var_name, "CALENDAR_QUARTER") && !is.null(t0)) {
      quarter_num <- ((as.integer(format(as.Date(t0), "%m")) - 1L) %/% 3L) + 1L
      return(sort(unique(paste0(format(as.Date(t0), "%Y"), "_Q", quarter_num))))
    }
    return(character(0))
  }

  # Handle metadata macros such as `get dap_georegion_cats`.
  if (startsWith(expected_cat, "get ")) {
    getter_name <- sub("^get ", "", expected_cat)
    # Synthetic georegion defaults used in simulated data.
    if (identical(getter_name, "dap_georegion_cats")) {
      return(paste0("REG_", 1:5))
    }
    # Build study quarters dynamically from index dates.
    if (identical(getter_name, "study_quarters_all") && !is.null(t0)) {
      quarter_num <- ((as.integer(format(as.Date(t0), "%m")) - 1L) %/% 3L) + 1L
      return(sort(unique(paste0(format(as.Date(t0), "%Y"), "_Q", quarter_num))))
    }
    return(character(0))
  }

  # Standard comma-separated category list.
  trimws(strsplit(expected_cat, ",", fixed = TRUE)[[1]])
}

#' Simulate ordinal/categorical values from a latent score
#'
#' Uses latent-score quantiles to assign observations into ordered categories.
#'
#' @param latent_score Numeric vector driving category assignment.
#' @param categories Character vector of ordered category labels.
#'
#' @return Character vector with one assigned category per observation.
#'
#' @noRd
simulate_ordered_categories <- function(latent_score, categories) {
  # Degenerate case with a single category level.
  if (length(categories) <= 1L) {
    return(rep(categories, length(latent_score)))
  }

  # Equal-frequency breaks across the latent distribution.
  breaks <- stats::quantile(
    latent_score,
    probs = seq(0, 1, length.out = length(categories) + 1L),
    na.rm = TRUE,
    names = FALSE
  )
  breaks[1] <- -Inf
  breaks[length(breaks)] <- Inf

  # Convert latent score into ordered labels.
  as.character(cut(
    latent_score,
    breaks = breaks,
    include.lowest = TRUE,
    labels = categories
  ))
}

#' Simulate binary indicators from risk score
#'
#' Applies a logistic link to the latent risk score and samples Bernoulli draws.
#'
#' @param risk_score Numeric latent risk score.
#' @param base_prob Baseline event probability when risk score is near zero.
#' @param group_shift Additive shift on the log-odds scale.
#'
#' @return Logical vector indicating simulated binary outcomes.
#'
#' @noRd
simulate_binary_indicator <- function(risk_score, base_prob = 0.10, group_shift = 0) {
  # Convert to bounded probabilities via logistic transform.
  prob <- stats::plogis(qlogis(base_prob) + 0.55 * risk_score + group_shift)
  # Draw indicators while clipping extremes for stability.
  stats::runif(length(prob)) < pmin(pmax(prob, 0.01), 0.95)
}

#' Build synthetic D4 covariate table for diagnostics
#'
#' Generates baseline covariates aligned with metadata definitions and available
#' study-cohort fields for ASD diagnostics and plotting workflows.
#'
#' @param cohort_data Data frame/data.table with study cohort rows.
#' @param metadata_files Character vector of metadata CSV paths.
#' @param seed Integer random seed for reproducibility.
#'
#' @return data.table compatible with `D4_StudyCohortCovariates{analysis}`.
#'
#' @noRd
build_d4_study_cohort_covariates <- function(
  cohort_data,
  metadata_files,
  seed = 42L
) {
  # Keep synthetic generation reproducible across runs.
  set.seed(as.integer(seed))

  # Normalize cohort input and enforce Date for index time.
  cohort_dt <- data.table::as.data.table(data.table::copy(cohort_data))
  if (!inherits(cohort_dt$T0, "Date")) {
    cohort_dt[, T0 := as.Date(T0)]
  }

  # Combine metadata registries used to derive variable list and types.
  metadata_tables <- lapply(metadata_files, function(file_path) {
    data.table::fread(file_path, sep = ";")
  })
  metadata_all <- unique(data.table::rbindlist(metadata_tables, use.names = TRUE, fill = TRUE))

  # Start with core cohort identifiers.
  out <- cohort_dt[, .(person_id, T0, group, match_id, boot_id)]
  if (!"boot_id" %in% names(out)) {
    out[, boot_id := 0L]
  }

  # Build latent scores that drive synthetic covariate correlations.
  n_rows <- nrow(out)
  group_exposed <- as.integer(out$group == "EXPOSED")
  base_risk <- stats::rnorm(n_rows, mean = 0.20 * group_exposed, sd = 1)

  # Reuse existing cohort fields when present, otherwise simulate defaults.
  age_years <- round(pmin(pmax(
    18 + 8 * base_risk + 12 * stats::runif(n_rows) + 10 * stats::runif(n_rows),
    18
  ), 90))
  sex_vals <- if ("SV_SEX" %in% names(cohort_dt)) {
    ifelse(cohort_dt$SV_SEX %in% c("female", "F"), "F", "M")
  } else {
    ifelse(stats::runif(n_rows) < 0.53, "F", "M")
  }
  region_vals <- if ("SV_REGION" %in% names(cohort_dt)) as.character(cohort_dt$SV_REGION) else sample(paste0("REG_", 1:5), n_rows, replace = TRUE)
  ses_vals <- if ("SV_SES" %in% names(cohort_dt)) as.character(cohort_dt$SV_SES) else as.character(sample(0:5, n_rows, replace = TRUE))
  smoking_vals <- if ("SV_SMOKING" %in% names(cohort_dt)) as.character(cohort_dt$SV_SMOKING) else as.character(sample(0:4, n_rows, replace = TRUE))
  cdc_vals <- if ("SV_CDC_RISK" %in% names(cohort_dt)) as.character(cohort_dt$SV_CDC_RISK) else as.character(sample(0:2, n_rows, replace = TRUE, prob = c(0.5, 0.3, 0.2)))
  # cci_vals <- if ("SV_CCI" %in% names(cohort_dt)) as.character(cohort_dt$SV_CCI) else as.character(sample(0:2, n_rows, replace = TRUE, prob = c(0.52, 0.32, 0.16)))
  preg_status <- if ("SV_PREG_STATUS" %in% names(cohort_dt)) {
    as.logical(cohort_dt$SV_PREG_STATUS)
  } else {
    rep(FALSE, n_rows)
  }

  # Derive quarter labels from T0.
  quarter_num <- ((as.integer(format(out$T0, "%m")) - 1L) %/% 3L) + 1L
  calendar_quarter <- paste0(format(out$T0, "%Y"), "_Q", quarter_num)

  # Populate key baseline covariates that are frequently used downstream.
  out[, SV_AGE := as.numeric(age_years)]
  out[, SV_AGE_CAT := simulate_ordered_categories(
    latent_score = SV_AGE + stats::rnorm(.N, sd = 2),
    categories = as.character(0:8)
  )]
  out[, SV_SEX := sex_vals]
  out[, POSSIBLE_PREGNANT := SV_SEX == "F" & SV_AGE >= 15 & SV_AGE <= 55]
  out[, SV_PREG_STATUS := POSSIBLE_PREGNANT & preg_status]
  out[SV_PREG_STATUS == FALSE, SV_PREG_TRIMESTER := "0"]
  out[SV_PREG_STATUS == TRUE, SV_PREG_TRIMESTER := sample(c("1", "2", "3"), .N, replace = TRUE)]
  out[, SV_REGION := region_vals]
  out[, SV_SES_STATUS := ses_vals]
  out[, CALENDAR_QUARTER := calendar_quarter]
  out[, SV_SMOKING_STATUS := smoking_vals]
  out[, SV_BMI_3YEARS := simulate_ordered_categories(
    latent_score = base_risk + 0.3 * (SV_SMOKING_STATUS %in% c("1", "2")) + stats::rnorm(.N),
    categories = as.character(0:4)
  )]
  out[, SV_OBESITY := SV_BMI_3YEARS %in% c("3", "4") | simulate_binary_indicator(base_risk, base_prob = 0.08)]
  out[, SV_HU_PRIOR_HOSP_NUM := simulate_ordered_categories(base_risk + stats::rnorm(.N), as.character(0:2))]
  out[, SV_HU_PRIOR_ED_NUM := simulate_ordered_categories(base_risk + 0.15 * group_exposed + stats::rnorm(.N), as.character(0:2))]
  out[, SV_HU_PRIOR_PC_NUM := simulate_ordered_categories(base_risk + 0.30 * group_exposed + stats::rnorm(.N), as.character(0:2))]
  out[, SV_HU_PRIOR_CS_NUM := simulate_ordered_categories(stats::rnorm(.N), as.character(0:2))]
  out[, SV_HU_PRIOR_ANY_COVID_TEST := simulate_ordered_categories(base_risk + 0.2 * group_exposed + stats::rnorm(.N), as.character(0:3))]
  out[, SV_CDC_SCORE := cdc_vals]
  out[, CDC_RISK := cdc_vals]
  # out[, CHARLSON_INDEX := cci_vals]
  out[, SV_CCI := sample(0:2, .N, replace = TRUE, prob = c(0.52, 0.32, 0.16))]

  # Preserve expected alias used by some modeling paths.
  if (!"SV_CCI" %in% names(out)) {
    out[, SV_CCI := CHARLSON_INDEX]
  }

  # Vaccine priming mock categories used by comorbidity metadata.
  primo_status <- sample(c("other", "pfizer", "none"), n_rows, replace = TRUE, prob = c(0.18, 0.52, 0.30))
  # Composite risk score for additional variables.
  risk_score <- scale(as.numeric(out$SV_AGE))[, 1] +
    0.55 * as.numeric(out$CDC_RISK) +
    # 0.65 * as.numeric(out$CHARLSON_INDEX) +
    0.35 * as.numeric(out$SV_HU_PRIOR_PC_NUM)

  # Fill all metadata-declared variables not yet materialized.
  for (i in seq_len(nrow(metadata_all))) {
    var_name <- as.character(metadata_all$var[i])
    var_type <- as.character(metadata_all$type[i])
    expected_cat <- if ("expectedCat" %in% names(metadata_all)) as.character(metadata_all$expectedCat[i]) else NA_character_

    if (var_name %in% names(out)) {
      next
    }

    # Derive mutually exclusive primovaccination flags.
    if (var_name %in% c("other_primo", "pfizer_primo", "no_primo")) {
      out[[var_name]] <- switch(var_name,
        other_primo = primo_status == "other",
        pfizer_primo = primo_status == "pfizer",
        no_primo = primo_status == "none"
      )
      next
    }

    # Numeric variables are generated around the latent risk profile.
    if (var_type %in% c("NUM", "NUM1")) {
      out[[var_name]] <- round(pmin(pmax(45 + 12 * risk_score + stats::rnorm(n_rows, sd = 8), 18), 90), 0)
      next
    }

    # Parse category universe before simulating CAT/TF values.
    categories <- parse_expected_categories(expected_cat = expected_cat, var_name = var_name, t0 = out$T0)

    if (var_type == "TF") {
      # Variable-family-specific baseline probabilities.
      base_prob <- 0.10
      if (startsWith(var_name, "SV_VP_")) {
        base_prob <- 0.18
      } else if (startsWith(var_name, "CU_")) {
        base_prob <- 0.12
      } else if (startsWith(var_name, "F_")) {
        base_prob <- 0.07
      } else if (grepl("HIV|AIDS|SCD|METASTASIS", var_name)) {
        base_prob <- 0.02
      }
      # Simulate logical indicators with optional exposure shift.
      out[[var_name]] <- simulate_binary_indicator(
        risk_score = risk_score,
        base_prob = base_prob,
        group_shift = if (startsWith(var_name, "SV_VP_")) 0.15 else 0.05 * group_exposed
      )
      next
    }

    if (var_type == "CAT") {
      # Default categories for underspecified metadata.
      if (length(categories) == 0L) {
        categories <- as.character(0:2)
      }
      # Simulate categorical levels from latent-score quantiles.
      out[[var_name]] <- simulate_ordered_categories(
        latent_score = risk_score + 0.20 * group_exposed + stats::rnorm(n_rows),
        categories = categories
      )
      next
    }
  }

  out
}
