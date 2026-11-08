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
  lm$start_script("create_baseline_covariates")
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
  file_name = "create_baseline_covariates",
  deap_name = config_project$DEAP_configuration$name
)

logger::log_info("Starting: extracting baseline covariates")

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
  logger::log_info(paste0("Extracting baseline covariates for analysis: ", analysis))

  d4_study_cohort <- data.table::as.data.table(picard::load(
    file_path = create_anchored_datasets$input$cohort_dir,
    file_name = paste0(create_anchored_datasets$input$cohort_file, analysis, ".parquet")
  ))

  # CHECK (remove in final version)
  anchoring_window_metadata[, is_expected_missing := FALSE]

  covariates <- anchoR::get_anchor_result(
    metadata = anchoring_window_metadata[covariate == TRUE],
    anchor_hive_path = anchor_hive_dir,
    population = d4_study_cohort[, -"match_id"],
    impute_missing = TRUE,
    result_shape = "wide"
  )

  # merge back with the study cohort to ensure all individuals are included
  covariates <- data.table::merge.data.table(
    x = d4_study_cohort[, .(person_id, T0, match_id)],
    y = covariates,
    by = c("person_id", "T0"),
    all.x = TRUE
  )

  # Remove any variables with "date_" prefix
  covariates <- covariates[, !grepl("^date_", names(covariates)), with = FALSE]

  # Remove any "value_" prefix from variable names
  data.table::setnames(covariates, old = names(covariates), new = sub("^value_", "", names(covariates)))

  # Add age-group to covariates dataset
  logger::log_info("Calculating age and age categories for baseline covariates")
  spells <- unique(picard::load(
    file_path = config_project$outputs$dir_d3,
    file_name = "D3_SPELLS.parquet"
  )[, .(person_id, birth_date)])
  covariates <- merge(covariates, spells, by = "person_id", all.x = TRUE)
  covariates[, SV_AGE := lubridate::year(T0) - lubridate::year(birth_date) - (format(T0, "%m-%d") < format(birth_date, "%m-%d"))]
  covariates[, SV_AGE_CAT := SV_AGE]

  # Remove birth_date after calculating age
  covariates[, birth_date := NULL]

  # choose concept_ranges source
  concept_ranges <- data.table::fread(file.path("transformations/common_configuration/RWE-BRIDGE/concept_ranges.csv"))[concept_id %in% "PP_AGE_CAT"]
  data.table::setDT(concept_ranges)

  # Add age category using the concept_ranges table
  covariates <- concept_ranges[
    covariates,
    on = .(
      lower_range <= SV_AGE_CAT,
      upper_range > SV_AGE_CAT
    ),
    nomatch = 0L
  ]
  covariates[, SV_AGE_CAT := new_value]
  covariates[, c("lower_range", "upper_range", "new_value", "concept_id", "V6", "description") := NULL]

  # Add squared age
  covariates[, `:=`(
    SV_AGE_SQ = as.numeric(SV_AGE)^2
  )]

  # Add calendar quarter from T0 to covariates dataset
  # covariates[, CALENDAR_QUARTER := lubridate::quarter(d4_study_cohort$T0, with_year = TRUE)]
  covariates[, CALENDAR_QUARTER := paste0(lubridate::year(T0), "_", lubridate::quarter(T0))]

  # SV_CCI: if 0 or 1, then 0; if 2, then 1; if ≥3, then 2
  covariates[, SV_CCI_INT := as.integer(SV_CCI)]
  covariates[, SV_CCI := as.integer(SV_CCI)]
  covariates[, SV_CCI := data.table::fifelse(SV_CCI %in% c(0, 1), 0, data.table::fifelse(SV_CCI == 2, 1, data.table::fifelse(SV_CCI >= 3, 2, SV_CCI)))]

  # # Fix variables with 2+ or 3+ ranges
  # covariates[, SV_HU_HOSP_CONTACT := as.integer(SV_HU_HOSP_CONTACT)]
  # covariates[, SV_HU_HOSP_CONTACT := data.table::fifelse(SV_HU_HOSP_CONTACT >= 2, 2, SV_HU_HOSP_CONTACT)]
  # covariates[, SV_HU_PC_CONTACT := as.integer(SV_HU_PC_CONTACT)]
  # covariates[, SV_HU_PC_CONTACT := data.table::fifelse(SV_HU_PC_CONTACT >= 2, 2, SV_HU_PC_CONTACT)]

  # Fix covid tests
  covariates[, SV_COVID_POSTEST := as.integer(SV_COVID_POSTEST)]
  covariates[, SV_COVID_POSTEST := data.table::fifelse(is.na(SV_COVID_POSTEST), 0, SV_COVID_POSTEST)]
  covariates[, SV_COVID_ANYTEST := as.integer(SV_COVID_ANYTEST)]
  covariates[, SV_COVID_ANYTEST := data.table::fifelse(is.na(SV_COVID_ANYTEST), 0, SV_COVID_ANYTEST)]

  # Recategorize SV_COVID_ANYTEST into 0, 1-2, 3-4, 5+
  covariates[, SV_COVID_ANYTEST := data.table::fifelse(
    SV_COVID_ANYTEST %in% c(0), SV_COVID_ANYTEST,
    data.table::fifelse(
      SV_COVID_ANYTEST %in% c(1, 2), 1,
      data.table::fifelse(SV_COVID_ANYTEST %in% c(3, 4), 2, 3)
    )
  )]

  # Save to disk
  picard::save(
    data = covariates,
    file_path = create_anchored_datasets$output$dir,
    file_name = paste0(create_anchored_datasets$output$baseline_cov_file, analysis, ".parquet")
  )

  rm(d4_study_cohort, covariates)
  gc()
  logger::log_info(paste0("Finished extracting baseline covariates for analysis: ", analysis))
}

logger::log_info("Baseline covariates extraction completed")
picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
