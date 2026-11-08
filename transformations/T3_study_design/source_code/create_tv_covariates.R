# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_tv_covariates")
  lm$start_capturing_prints()
}

## Config loading ----
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_anchored_datasets <- config_project$create_anchored_datasets

# Path to anchored variables Hive
anchor_hive_dir <- create_anchored_datasets$output$anchoring_dir

## Audit setup ---
picard::audit_start(
  file_name = "create_tv_covariates",
  deap_name = config_project$DEAP_configuration$name
)

logger::log_info("Starting: extracting time-varying covariates")

# ----------------------------
# LOAD METADATA
# ----------------------------

anchoring_window_metadata <- picard::load(
  file_path = create_anchored_datasets$input$metadata_dir,
  file_name = create_anchored_datasets$input$metadata_file
)

# ----------------------------
# EXTRACT AND SAVE PER ANALYSIS
# ----------------------------

for (analysis in c("Main", "Supplementary")) {
  # Run the full TV-covariate build independently per analysis arm.
  logger::log_info(paste0("Extracting TV covariates for analysis: ", analysis))

  # Load the matched cohort and remove unmatched records before expansion.
  d4_study_cohort <- data.table::as.data.table(picard::load(
    file_path = create_anchored_datasets$input$cohort_dir,
    file_name = paste0(create_anchored_datasets$input$cohort_file, analysis, ".parquet")
  ))
  d4_study_cohort <- d4_study_cohort[group != "UNMATCHED"][, .(person_id, T0, match_id, group)]

  # Build dynamic join keys so the code works whether match_id exists or not.
  cohort_key_cols <- intersect(c("person_id", "T0", "group", "match_id"), names(d4_study_cohort))
  # person_id and T0 are mandatory for person-time expansion.
  if (!all(c("person_id", "T0") %in% cohort_key_cols)) {
    stop("Missing required key columns in D4 study cohort for TV covariate expansion.")
  }

  # Always source follow-up end from D4_StudyCohortFup<analysis>.parquet.
  fup_file_name <- paste0(create_anchored_datasets$output$fup_file, analysis, ".parquet")
  d4_study_cohort_fup <- data.table::as.data.table(picard::load(
    file_path = create_anchored_datasets$output$dir,
    file_name = fup_file_name
  ))

  # Determine available keys in the FUP file and validate required keys.
  fup_key_cols <- intersect(c("person_id", "T0", "group", "match_id"), names(d4_study_cohort_fup))
  if (!all(c("person_id", "T0") %in% fup_key_cols)) {
    stop(paste0("Missing required key columns in ", fup_file_name, "."))
  }
  # Ensure the required censoring horizon column exists.
  if (!("end_fup" %in% names(d4_study_cohort_fup))) {
    stop(paste0("Missing required column end_fup in ", fup_file_name, "."))
  }
  if (!("censor_nonadmin_ipcw" %in% names(d4_study_cohort_fup))) {
    stop(paste0("Missing required column censor_nonadmin_ipcw in ", fup_file_name, "."))
  }

  # Keep only merge keys + end_fup + censor_nonadmin_ipcw to avoid pulling unnecessary columns.
  d4_study_cohort_fup <- d4_study_cohort_fup[, c(fup_key_cols, "censor_nonadmin_ipcw", "end_fup"), with = FALSE]
  # Attach end_fup and censor_nonadmin_ipcw to cohort rows using the shared key columns.
  d4_study_cohort <- merge(
    d4_study_cohort,
    d4_study_cohort_fup,
    by = fup_key_cols,
    all.x = TRUE,
    sort = FALSE
  )

  # Fail early if any cohort row cannot be assigned a follow-up end.
  if (any(is.na(d4_study_cohort$end_fup))) {
    stop(paste0("Found missing end_fup values after merge with ", fup_file_name, "."))
  }

  # max_fup is the row-wise horizon used for day-by-day expansion.
  d4_study_cohort[, max_fup := end_fup]
  # Guard against malformed data by forcing max_fup >= T0.
  d4_study_cohort[, max_fup := as.Date(pmax(max_fup, T0))]

  # immunocompromised TV
  # Long-format anchor results: each row is an event date where value is TRUE.
  logger::log_info("Extracting immunocompromised TV covariate from anchoR results")
  tv_immunocompromised <- anchoR::get_anchor_result(
    metadata = anchoring_window_metadata[tv_covariate == TRUE][variable_id == "SV_IMMUNOCOMPROMISED_TV"],
    anchor_hive_path = anchor_hive_dir,
    population = d4_study_cohort[, -"match_id"],
    impute_missing = TRUE,
    result_shape = "long",
    required_population_cols = c("person_id", "T0")
  )

  if (analysis == "Supplementary") {
    # merge back to keep all repeated person_id + T0 rows in the study cohort
    # (e.g., controls in the matching with replacement)
    tv_immunocompromised <- data.table::merge.data.table(
      x = d4_study_cohort[, .(person_id, T0, group, match_id)],
      y = tv_immunocompromised,
      by = c("person_id", "T0"),
      all.x = TRUE
    )
  }

  # hospital length of stay TV
  logger::log_info("Extracting hospital length of stay TV covariate from anchoR results")
  tv_hospitalization <- anchoR::get_anchor_result(
    metadata = anchoring_window_metadata[tv_covariate == TRUE][variable_id == "SV_HOSP_LENGTH_TV"],
    anchor_hive_path = anchor_hive_dir,
    population = d4_study_cohort,
    impute_missing = TRUE,
    result_shape = "long",
    required_population_cols = c("person_id", "T0")
  )

  if (analysis == "Supplementary") {
    # merge back to keep all repeated person_id + T0 rows in the study cohort
    # (e.g., controls in the matching with replacement)
    tv_hospitalization <- data.table::merge.data.table(
      x = d4_study_cohort[, .(person_id, T0, group, match_id)],
      y = tv_hospitalization,
      by = c("person_id", "T0"),
      all.x = TRUE
    )
  }

  # Expand each cohort row into one record per day from T0 to max_fup.
 tv_covariates <- d4_study_cohort[, .(
  max_fup = max_fup[1],
  censor_nonadmin_ipcw = censor_nonadmin_ipcw[1],
  date = structure(
    as.double(seq(T0[1], max_fup[1], by = "day")),
    class = "Date"
  )
), by = cohort_key_cols]

  # IPCW expects day index from T0.
  tv_covariates[, time_days := as.integer(date - T0)]
  # Initialize TV indicators as FALSE; only observed event dates become TRUE.
  tv_covariates[, `:=`(
    SV_IMMUNOCOMPROMISED_TV = FALSE,
    SV_HOSP_LENGTH_TV = FALSE,
    censor_nonadmin = 0L
  )]

  # One horizon row per person/T0 used to clip event dates to follow-up.
  person_fup <- unique(tv_covariates[, .(person_id, T0, max_fup)], by = c("person_id", "T0"))

  # Process immunocompromised events only when non-empty.
  if (nrow(tv_immunocompromised) > 0L) {
    # Keep only join columns needed for stamping TRUE values.
    immunocompromised_events <- data.table::as.data.table(tv_immunocompromised)[, .(person_id, T0, date)]
    # Normalize date type and drop missing event dates.
    immunocompromised_events[, date := as.Date(date)]
    # Remove duplicate events on the same day for the same person/T0.
    immunocompromised_events <- unique(immunocompromised_events[!is.na(date)], by = c("person_id", "T0", "date"))
    # Mark event days as TRUE.
    immunocompromised_events[, SV_IMMUNOCOMPROMISED_TV := TRUE]
    # Keep events inside each person-specific follow-up window.
    immunocompromised_events <- immunocompromised_events[
      person_fup,
      on = .(person_id, T0),
      nomatch = 0L
    ][date <= max_fup, .(person_id, T0, date, SV_IMMUNOCOMPROMISED_TV)]
    # Stamp TRUE onto the dense daily table; all other days remain FALSE.
    tv_covariates[immunocompromised_events, SV_IMMUNOCOMPROMISED_TV := i.SV_IMMUNOCOMPROMISED_TV, on = .(person_id, T0, date)]
  }

  # Process hospitalization events only when non-empty.
  if (nrow(tv_hospitalization) > 0L) {
    # Keep only join columns needed for stamping TRUE values.
    hospitalization_events <- data.table::as.data.table(tv_hospitalization)[, .(person_id, T0, date)]
    # Normalize date type and drop missing event dates.
    hospitalization_events[, date := as.Date(date)]
    # Remove duplicate events on the same day for the same person/T0.
    hospitalization_events <- unique(hospitalization_events[!is.na(date)], by = c("person_id", "T0", "date"))
    # Mark event days as TRUE.
    hospitalization_events[, SV_HOSP_LENGTH_TV := TRUE]
    # Keep events inside each person-specific follow-up window.
    hospitalization_events <- hospitalization_events[
      person_fup,
      on = .(person_id, T0),
      nomatch = 0L
    ][date <= max_fup, .(person_id, T0, date, SV_HOSP_LENGTH_TV)]
    # Stamp TRUE onto the dense daily table; all other days remain FALSE.
    tv_covariates[hospitalization_events, SV_HOSP_LENGTH_TV := i.SV_HOSP_LENGTH_TV, on = .(person_id, T0, date)]
  }

  # Set censor_nonadmin to 1 at end of follow-up if censor_nonadmin_ipcw == 1.
  # This indicator captures censoring due to IE3, IE5, or loss to follow-up.
  tv_covariates[date == max_fup & censor_nonadmin_ipcw == 1L, censor_nonadmin := 1L]

  # Remove helper columns; final output keeps person-time + TV flags.
  tv_covariates[, `:=`(max_fup = NULL, censor_nonadmin_ipcw = NULL)]

  # Persist per-analysis TV covariate file for downstream IPCW pipeline.
  picard::save(
    data = tv_covariates,
    file_path = create_anchored_datasets$output$dir,
    file_name = paste0(create_anchored_datasets$output$tv_cov_file, analysis, ".parquet")
  )

  rm(d4_study_cohort, tv_covariates)
  gc()
  logger::log_info(paste0("Finished extracting TV covariates for analysis: ", analysis))
}

logger::log_info("Time-varying covariates extraction completed")
picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
