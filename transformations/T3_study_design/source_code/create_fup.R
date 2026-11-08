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
  lm$start_script("create_fup")
  lm$start_capturing_prints()
}

## Config loading ----
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_anchored_datasets <- config_project$create_anchored_datasets

# Path to anchored variables Hive
anchor_hive_dir <- create_anchored_datasets$output$anchoring_dir

## Functions ----
source("transformations/T3_study_design/source_code/functions/follow_up_functions.R")

## Audit setup ---
picard::audit_start(
  file_name = "create_fup",
  deap_name = config_project$DEAP_configuration$name
)

logger::log_info("Starting: extracting post-exposure (fup) variables")

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
  logger::log_info(paste0("Extracting fup variables for analysis: ", analysis))

  # Load study cohort for this analysis to get the population to extract fup
  # variables for. We only select person_id and T0 since those are needed for
  # the anchoring, and match_id and group which are needed for some of the
  # censoring rules and to add back to the fup table after extracting from the
  # anchored variables hive.
  d4_study_cohort <- data.table::as.data.table(picard::load(
    file_path = create_anchored_datasets$input$cohort_dir,
    file_name = paste0(create_anchored_datasets$input$cohort_file, analysis, ".parquet")
  ))
  d4_study_cohort <- d4_study_cohort[group != "UNMATCHED"][, .(person_id, match_id, T0, group)]

  # CHECK (remove in final version)
  anchoring_window_metadata[, is_expected_missing := FALSE]

  logger::log_info("Extracting fup variables from anchored Hive using anchoR")
  d4_fup <- anchoR::get_anchor_result(
    metadata = anchoring_window_metadata[fup == TRUE],
    anchor_hive_path = anchor_hive_dir,
    population = d4_study_cohort[, -"match_id"],
    impute_missing = TRUE,
    only_date = TRUE,
    result_shape = "wide",
    required_population_cols = c("person_id", "T0")
  )

  # merge back to keep all repeated person_id + T0 rows in the study cohort
  # (e.g., controls in the matching with replacement)
  d4_fup <- data.table::merge.data.table(
    x = d4_study_cohort[, .(person_id, T0, match_id)],
    y = d4_fup,
    by = c("person_id", "T0"),
    all.x = TRUE
  )

  # Load additional variables if needed -------------------------

  # 1) death_date from D3_PERSONS$death_date
  logger::log_info("Merging in death_date from D3_PERSONS$death_date")
  death_dates <- picard::load(
    file_path = config_project$outputs$dir_d3,
    file_name = "D3_PERSONS.parquet"
  )[, .(person_id, death_date)]
  d4_fup <- data.table::merge.data.table(
    x = d4_fup,
    y = death_dates,
    by = "person_id",
    all.x = TRUE
  )
  rm(death_dates)

  # 2) end_study_date from config_values.yaml
  logger::log_info(paste0(
    "Using end_study_date from config_values.yaml: ",
    config_values$end_study_date
  ))
  end_study_date <- as.Date(config_values$end_study_date)
  d4_fup[, end_study_date := end_study_date]

  # 3) end_data_collection from config_values.yaml
  logger::log_info(paste0(
    "Using end_data_collection from config_values.yaml: ",
    config_values$end_data_collection
  ))
  # if the next line throws an error use end_study_date instead of end_data_collection
  try(
    end_data_collection <- as.Date(config_values$end_data_collection),
    silent = TRUE
  )
  if (!exists("end_data_collection")) {
    logger::log_warn(paste0(
      "end_data_collection not found in config_values.yaml. Using end_study_date instead: ",
      config_values$end_study_date
    ))
    end_data_collection <- as.Date(config_values$end_study_date)
  }
  # end_data_collection <- as.Date(config_values$end_data_collection)
  d4_fup[, end_data_collection := end_data_collection]

  # 4) loss_to_follow_up from D3_SPELLS$end_spell
  logger::log_info("Merging in loss_to_follow_up from D3_SPELLS$end_spell")
  spells <- picard::load(
    file_path = config_project$outputs$dir_d3,
    file_name = "D3_SPELLS.parquet"
  )[, .(person_id, end_spell)]

  # If multiple spells per person raise a warning
  if (nrow(spells) != length(unique(spells$person_id))) {
    logger::log_warn(paste0(
      "Found multiple spells per person in D3_SPELLS. The earliest spell after T0 (with the earliest end_spell date) will be used for each matched case to determine loss_to_follow_up."
    ))
    # For each person take the earliest end_spell after T0 date as the
    # loss_to_follow_up date (in case of multiple spells per person), and
    # keep NAs as is since they indicate no loss to follow-up rather than
    # missing data.

    # Unique row identifier
    d4_fup[, row_id := .I]

    # Merge spells with d4_fup to get end_spell dates for each person_id + T0
    # combination
    tmp <- data.table::merge.data.table(
      d4_fup[, .(row_id, person_id, T0)],
      spells,
      by = "person_id",
      all.x = TRUE,
      allow.cartesian = TRUE
    )

    # For each person_id + T0, take the earliest end_spell date that is after
    # T0 as the loss_to_follow_up date. If there are no end_spell dates after
    # T0, set loss_to_follow_up to NA.
    loss_to_follow_up <- tmp[
      ,
      .(
        loss_to_follow_up = {
          x <- end_spell[!is.na(end_spell) & end_spell > T0[1]]
          if (length(x)) min(x) else as.Date(NA)
        }
      ),
      by = row_id
    ]

    # Merge back the loss_to_follow_up variable to d4_fup using the row_id
    d4_fup[loss_to_follow_up, loss_to_follow_up := i.loss_to_follow_up, on = "row_id"]
    d4_fup[, row_id := NULL]

    rm(tmp, loss_to_follow_up)
  } else {
    d4_fup <- data.table::merge.data.table(
      x = d4_fup,
      y = spells,
      by = "person_id",
      all.x = TRUE
    )
    data.table::setnames(d4_fup, "end_spell", "loss_to_follow_up")
  }
  rm(spells)

  # Compute variables -------------------------

  # 1) compliant_exposed as IF group == EXPOSED THEN !is.na(ie1_second_dose_target_exposed) ELSE NA
  logger::log_info("Computing compliant_exposed variable")
  d4_fup[, compliant_exposed := ifelse(
    group == "EXPOSED",
    !is.na(ie1_second_dose_target_exposed),
    TRUE
  )]

  # 2) max_fup_date as T0 + 90 days
  logger::log_info("Computing max_fup_date as T0 + 90 days")
  d4_fup[, max_fup_date := T0 + 90]

  # 3)Compute composite outcome as the earliest of covid_date and death_date
  logger::log_info("Computing composite outcome variable as the earliest of covid_date and death_date")
  d4_fup[, composite_outcome := as.Date(
    do.call(pmin, c(.SD, na.rm = TRUE))
  ), .SDcols = c("covid_date", "death_date")]

  # Fix group-specific intercurrent events --------------------------

  # Complex concepts are computed before matching. Here we need to replace
  # dates by NA if the concept is present but does not meet the criteria for
  # being considered an intercurrent event for the exposed or control group
  # (e.g., second dose of vaccine for the target-exposed group).

  # Compute censoring dates and reasons for censoring -------------------------

  # For example, ie1_second_dose_target_exposed should be NA for controls and
  # for exposed individuals who do not meet the criteria for being considered
  # as having the intercurrent event (e.g., second dose of vaccine for the
  # target-exposed group). We set it to NA for those individuals here so that
  # they are not incorrectly censored based on this variable in the censoring
  # rules later on.

  logger::log_info("Fixing group-specific intercurrent event variables to set non-qualifying events to NA")

  d4_fup[
    group != "EXPOSED" | (group == "EXPOSED" & is.na(ie1_second_dose_target_exposed)),
    ie1_second_dose_target_exposed := NA
  ]

  # ie2_third_dose_or_booster_exposed
  d4_fup[
    group != "EXPOSED" | (group == "EXPOSED" & is.na(ie2_third_dose_or_booster_exposed)),
    ie2_third_dose_or_booster_exposed := NA
  ]

  # ie3_first_dose_covid_any_control
  d4_fup[
    group != "CONTROL" | (group == "CONTROL" & is.na(ie3_first_dose_covid_any_control)),
    ie3_first_dose_covid_any_control := NA
  ]

  # ie3_first_dose_covid_nontarget_exposed
  d4_fup[
    group != "EXPOSED" | (group == "EXPOSED" & is.na(ie3_first_dose_covid_nontarget_exposed)),
    ie3_first_dose_covid_nontarget_exposed := NA
  ]

  # Estimand-specific censoring reasons
  indiv_censor_vars <- c(
    "covid_date",
    "death_date",
    "loss_to_follow_up",
    "max_fup_date",
    "end_study_date",
    "end_data_collection",
    "ie3_first_dose_covid_nontarget_exposed",
    "ie3_first_dose_covid_any_control",
    "ie5_prep_treatment"
  )

  # Loop over estimands --------------------------
  for (estimand in seq_along(c("estimand1", "estimand2"))) {
    logger::log_info(paste0("Processing variables for estimand ", estimand))

    # Estimand-specific reason_censoring_est*
    logger::log_info("Computing censoring dates and reasons for censoring")
    d4_fup <- censorFupAesiVect(
      aesi_fup_table = d4_fup,
      rw_id = NULL,
      person_id_var = "person_id",
      match_date_var = "T0",
      match_id_var = "match_id",
      group_var = "group",
      ind_censor_vars = indiv_censor_vars,
      pair_censoring = FALSE,
      pair_censor_vars = NULL,
      # infant-denominator pregnancy outcomes
      selective_postpreg_censor = FALSE,
      preg_end_var = NULL,
      postpreg_censor_vars = NULL,
      # return reason for censoring for easier debugging
      return_reason = TRUE
    )

    # rename reason_endfup to reason_censoring
    data.table::setnames(d4_fup, "reason_endfup", "reason_censoring")

    # Estimand-specific FUP variables (end_fup, event_count, event_date, fup_dats, etc.)
    logger::log_info("Computing follow-up variables")
    d4_fup <- calculateFupLengthAesi(
      aesi_fup_table = d4_fup,
      rw_id = NULL, # indicator suffix to be given to computed columns (e.g., "_main")
      censor_date_var = "end_fup", # a single date, date of censoring
      event_date_var = ifelse(estimand == 1, "composite_outcome", "covid_date"), # a single date, date of event occurrence
      start_risk_window_vars = "T0", # a vector of dates denoting start dates of the risk window(s)
      end_risk_window_vars = "max_fup_date", # a vector of dates denoting the end of rw; start_ and end_rw_date form pairs
      scaling = 1
    )

    # rename eventCount to event_count_est1 or event_count_est2 depending on the estimand
    data.table::setnames(d4_fup,
      old = c("eventCount", "reason_censoring", "fup", "pyr"),
      new = c(paste0("event_count_est", estimand), paste0("reason_censoring_est", estimand), paste0("fup_length_days_est", estimand), paste0("fup_length_pyr_est", estimand))
    )
    # # rename eventCount to event_count_est1 or event_count_est2 depending on the estimand
    # data.table::setnames(d4_fup,
    #   old = c("eventCount"),
    #   new = c(paste0("event_count_est", estimand))
    # )
  } # end of the estimand loop

  # compute admin and nonadmin censor flag variables
  # 1) cemsor_nonadmin_ipcw as IF (reason_censoring_est1 %in% c(IE3, IE5, loss_to_followup)) 1 ELSE 0
  logger::log_info("Computing censor_nonadmin_ipcw variable")
  d4_fup[, censor_nonadmin_ipcw := ifelse(
    reason_censoring_est1 %in% c(
      "ie3_first_dose_covid_nontarget_exposed",
      "ie3_first_dose_covid_any_control",
      "ie5_prep_treatment",
      "loss_to_follow_up"
    ),
    1,
    0
  )]

  # 2) censor_admin as everything else (i.e., reason_censoring_est1 not in c(IE3, IE5, loss_to_followup))
  logger::log_info("Computing censor_admin variable")
  d4_fup[, censor_admin := ifelse(
    reason_censoring_est1 %in% c(
      "ie3_first_dose_covid_nontarget_exposed",
      "ie3_first_dose_covid_any_control",
      "ie5_prep_treatment",
      "loss_to_follow_up"
    ),
    0,
    1
  )]

  # Save FUP object
  picard::save(
    data = d4_fup,
    file_path = create_anchored_datasets$output$dir,
    file_name = paste0(create_anchored_datasets$output$fup_file, analysis, ".parquet")
  )

  rm(d4_study_cohort, d4_fup)
  gc()
  logger::log_info(paste0("Finished extracting fup variables for analysis: ", analysis))
}

logger::log_info("Fup extraction completed")
picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
