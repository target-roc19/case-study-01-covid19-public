################################################################
# create_baseline_tables ---
# Aim:
# Create baseline descriptive, comorbidity, and comedication tables
# (Tables 2.1.a–f, 2.2.a–f, 2.3.a–f) for the main and supplementary
# matched cohorts across Estimand 1 (unweighted) and Estimand 2 (IPSMW).
#
# Input files: ---
# - configuration/config_project.yaml
# - data/D4_analytic_datasets/D4_StudyCohort{Main,Supplementary}.parquet
# - data/D4_analytic_datasets/D4_StudyCohortCovariates{Main,Supplementary}.parquet
# - data/D4_analytic_datasets/D4_StudyCohort_IPSMW_{Main,Supplementary}.parquet
# - transformations/T3_study_design/metadata/BaselineDescriptives_Metadata.csv
# - transformations/T3_study_design/metadata/BaselineComorbidities_Metadata.csv
# - transformations/T3_study_design/metadata/BaselineComedications_Metadata.csv
#
# Output: ---
# - data/D5_results/{deap_name}_Table2.1{a-f}.csv  (baseline descriptives)
# - data/D5_results/{deap_name}_Table2.2{a-f}.csv  (baseline comorbidities)
# - data/D5_results/{deap_name}_Table2.3{a-f}.csv  (baseline comedications)
# - data/D5_results/D5_ASD_PREWEIGHT_Baseline_{label}.csv  (if save_asds_pre)
################################################################

# Initial setup ---
## Script logger ---
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_baseline_tables")
  lm$start_capturing_prints()
}

logger::log_info("[Baseline tables] - START")
logger::log_info("Start of the create_baseline_tables script.")

library(data.table)
library(stddiff)
library(stringr)
library(DescriptiveTable)

## Config loading ---
config_project <- picard::load_config("configuration/config_project.yaml")
create_baseline_tables_config <- config_project$create_baseline_tables

## Audit setup ---
picard::audit_start(
  file_name = "create_baseline_tables",
  deap_name = config_project$DEAP_configuration$name
)

## Extract config parameters ---
deap_name <- config_project$DEAP_configuration$name
dir_d4 <- create_baseline_tables_config$input$dir_d4
dir_metadata <- create_baseline_tables_config$input$dir_metadata
dir_d5 <- create_baseline_tables_config$output$dir_d5
save_output <- create_baseline_tables_config$output$save_output
save_asds_pre <- create_baseline_tables_config$output$save_asds_pre
ipsmw_weight <- create_baseline_tables_config$ipsmw_weight

## Placeholders: label_lookup and mapping_dictionary ---

# Try to load label_lookup from config-specified file; if not available,
# set to NULL and log a warning.
dir_label_lookup <- create_baseline_tables_config$input$label_lookup_file
label_lookup <- if (!is.null(dir_label_lookup)) {
  as.data.table(picard::load(dir_label_lookup))
} else {
  logger::log_warn(paste0(
    "[Baseline tables] label_lookup_file not specified in config; category labels will not be resolved."
  ))
  NULL
}

## Placeholder: expected_missing ---
# Variables structurally missing for this DAP, to be loaded from metadata.
# expected_missing <- data.table::fread(file.path(dir_metadata, "expected_missing.csv"))[
#   get(deap_name) != "0", variable_id
# ]

# Try to load expected_missing from config-specified file; if not available, set to empty character vector and log a warning.
dir_expected_missing <- create_baseline_tables_config$input$expected_missing_file
expected_missing <- if (!is.null(dir_expected_missing)) {
  as.data.table(picard::load(dir_expected_missing))[
    get(deap_name) != "0", variable_id
  ]
} else {
  logger::log_warn(paste0(
    "[Baseline tables] expected_missing_file not specified in config; no variables will be treated as structurally missing."
  ))
  character(0)
}

## Define table scenarios ---
# letter:     output table suffix (a–f)
# cohort:     Main or Supplementary
# use_ipsmw:  whether to apply IPSMW weights (Estimand 2)
# comparison: "matched"     → EXPOSED vs CONTROL (matched cohort)
#             "all_exposed" → matched EXPOSED vs UNMATCHED EXPOSED
# asd_label:  label appended to the pre-weight ASD output filename
# weighted_stats: counts and descriptive statistics weighted by ipsmw_weight
table_scenarios <- list(
  list(letter = "a", cohort = "Main", use_ipsmw = FALSE, comparison = "matched", asd_label = "Main_Estimand1_ExposedVsControl"),
  list(letter = "b", cohort = "Supplementary", use_ipsmw = FALSE, comparison = "matched", asd_label = "Supplementary_Estimand1_ExposedVsControl"),
  list(letter = "c", cohort = "Main", use_ipsmw = TRUE, comparison = "matched", asd_label = "Main_Estimand2_ExposedVsControl"),
  list(letter = "d", cohort = "Supplementary", use_ipsmw = TRUE, comparison = "matched", asd_label = "Supplementary_Estimand2_ExposedVsControl"),
  list(letter = "e", cohort = "Main", use_ipsmw = FALSE, comparison = "all_exposed", asd_label = "Main_Estimand1_MatchedVsAllExposed"),
  list(letter = "f", cohort = "Supplementary", use_ipsmw = FALSE, comparison = "all_exposed", asd_label = "Supplementary_Estimand1_MatchedVsAllExposed"),
  list(letter = "g", cohort = "Main", use_ipsmw = FALSE, comparison = "censoring", asd_label = "Main_Estimand1_AdminVsNonAdminCensoring"),
  list(letter = "h", cohort = "Main", use_ipsmw = TRUE, comparison = "matched", weighted_stats = TRUE, asd_label = "Main_Estimand2_Weighted_ExposedVsControl"),
  list(letter = "i", cohort = "Supplementary", use_ipsmw = TRUE, comparison = "matched", weighted_stats = TRUE, asd_label = "Supplementary_Estimand2_Weighted_ExposedVsControl"),
  list(letter = "j", cohort = "Main", use_ipsmw = FALSE, comparison = "compliance", asd_label = "Main_Exposed_CompliersVsNonCompliers"),
  list(letter = "k", cohort = "Supplementary", use_ipsmw = FALSE, comparison = "compliance", asd_label = "Supplementary_Exposed_CompliersVsNonCompliers"),
  list(letter = "l", cohort = "Main", use_ipsmw = FALSE, comparison = "compliant_exposed_all_controls", asd_label = "Main_ExposedCompliersVsControls"),
  list(letter = "m", cohort = "Supplementary", use_ipsmw = FALSE, comparison = "compliant_exposed_all_controls", asd_label = "Supplementary_ExposedCompliersVsControls")
)

## Define metadata files and corresponding table numbers ---
metadata_files <- c(
  "BaselineDescriptives_Metadata.csv",
  "BaselineComorbidities_Metadata.csv",
  "BaselineComedications_Metadata.csv"
)
table_numbers <- c("2.1", "2.2", "2.3")
table_numbers_censoring <- c("8.1", "8.2", "8.3")

# Collect ASD tables per scenario (used if save_asds_pre = TRUE)
asd_tables <- list()

# ============================================================
# Main loop: iterate over table scenarios (a–f)
# ============================================================
for (scenario in table_scenarios) {
  logger::log_info(paste0(
    "[Baseline tables] Scenario ", scenario$letter,
    ": cohort=", scenario$cohort,
    ", comparison=", scenario$comparison,
    ", IPSMW=", scenario$use_ipsmw
  ))

  # ----------------------------------------------------------
  # Load and prepare data for this scenario
  # ----------------------------------------------------------
  if (scenario$comparison == "matched") {
    # Scenarios a–d: use covariate file directly (EXPOSED + CONTROL)
    D4_Data <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohortCovariates", scenario$cohort, ".parquet"))
      )
    )
    D4_Data <- D4_Data[group %in% c("EXPOSED", "CONTROL")]
    if ("boot_id" %in% names(D4_Data)) D4_Data <- D4_Data[boot_id == 0]

    # no weights for scenarios a–b; weights added in scenarios c–d if use_ipsmw = TRUE
    weight_col <- NULL

    if (scenario$use_ipsmw) {
      # if the file does not exist, log a warning and skip this scenario
      ipsmw_file <- file.path(dir_d4, paste0("D4_StudyCohort_IPSMW_", scenario$cohort, ".parquet"))
      if (!file.exists(ipsmw_file)) {
        logger::log_warn(paste0(
          "[Baseline tables] IPSMW file not found for scenario ", scenario$letter,
          " (", ipsmw_file, "); skipping this scenario."
        ))
        next
      }

      # Scenarios c–d: merge IPSMW weights and restrict to Estimand 2 population
      D4_IPSMW <- as.data.table(
        arrow::read_parquet(
          file.path(dir_d4, paste0("D4_StudyCohort_IPSMW_", scenario$cohort, ".parquet"))
        )
      )

      # Filter to Estimand 2 population (compliant exposed + all controls)
      # Use mget() to dynamically select the weight column by name
      D4_IPSMW <- D4_IPSMW[
        include_estimand2 == TRUE,
        mget(c("person_id", "T0", "group", "match_id", ipsmw_weight))
      ]

      # Inner join: restricts D4_Data to Estimand 2 population and adds weight column
      merge_keys <- intersect(c("person_id", "T0", "group", "match_id"), intersect(names(D4_Data), names(D4_IPSMW)))
      D4_Data <- merge(D4_Data, D4_IPSMW, by = merge_keys, all.x = FALSE)
      weight_col <- ipsmw_weight
      rm(D4_IPSMW)
    }
  } else if (scenario$comparison == "all_exposed") {
    # Scenarios e–f: matched EXPOSED vs all EXPOSED (matched + unmatched)
    # Base cohort file includes UNMATCHED rows; covariates file may not
    D4_Cohort <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohort", scenario$cohort, ".parquet"))
      )
    )
    D4_Cohort <- D4_Cohort[group %in% c("EXPOSED", "UNMATCHED")]
    if ("boot_id" %in% names(D4_Cohort)) D4_Cohort <- D4_Cohort[boot_id == 0]

    cohort_keep <- intersect(c("person_id", "T0", "group", "match_id"), names(D4_Cohort))
    D4_Cohort <- D4_Cohort[, ..cohort_keep]

    # Duplicate rows to create two comparison groups:
    # - MATCHED_EXPOSED: only originally EXPOSED
    # - ALL_EXPOSED: originally EXPOSED or UNMATCHED
    D4_Cohort_Matched <- copy(D4_Cohort[group == "EXPOSED"])
    D4_Cohort_Matched[, group := "MATCHED_EXPOSED"]
    D4_Cohort_All <- copy(D4_Cohort[group %in% c("EXPOSED", "UNMATCHED")])
    D4_Cohort_All[, group := "ALL_EXPOSED"]
    D4_Cohort <- rbindlist(list(D4_Cohort_Matched, D4_Cohort_All), use.names = TRUE)

    D4_Covariates <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohortCovariates", scenario$cohort, ".parquet"))
      )
    )
    if ("boot_id" %in% names(D4_Covariates)) D4_Covariates <- D4_Covariates[boot_id == 0]

    # Left join: any cohort rows without covariates will have NAs
    D4_Data <- merge(
      D4_Cohort, D4_Covariates,
      by = c("person_id", "T0", "match_id"), all.x = TRUE,
      suffixes = c("", "_cov")
    )
    # Drop duplicate columns introduced by the join (cohort columns take priority)
    dup_cols <- grep("_cov$", names(D4_Data), value = TRUE)
    if (length(dup_cols) > 0) D4_Data[, (dup_cols) := NULL]

    weight_col <- NULL
    rm(D4_Cohort, D4_Covariates, D4_Cohort_Matched, D4_Cohort_All)
  } else if (scenario$comparison == "censoring") {
    # Scenario g: main matched cohort, pooled EXPOSED+CONTROL,
    # grouped by administrative vs non-administrative censoring (estimand 1).
    D4_Fup <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, "D4_StudyCohortFupMain.parquet")
      )
    )
    if ("boot_id" %in% names(D4_Fup)) D4_Fup <- D4_Fup[boot_id == 0]

    overlap_n <- D4_Fup[censor_admin == 1 & censor_nonadmin_ipcw == 1, .N]
    if (overlap_n > 0) {
      logger::log_warn(paste0(
        "[Baseline tables] Found ", overlap_n,
        " rows with both censor_admin==1 and censor_nonadmin_ipcw==1; excluding overlap from Table 8.x grouping."
      ))
    }

    D4_Fup <- D4_Fup[
      (censor_admin == 1 | censor_nonadmin_ipcw == 1)
    ]
    # keep the exposure arm before group is overwritten with censoring status,
    # so we can write Tables 8.x by exposure group
    D4_Fup[, exposure := group]
    D4_Fup[, group := fifelse(censor_nonadmin_ipcw == 1, "NONADMIN_CENSORED", "ADMIN_CENSORED")]
    D4_Fup <- D4_Fup[, .(person_id, T0, exposure, group, match_id)]

    D4_Covariates <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, "D4_StudyCohortCovariatesMain.parquet")
      )
    )
    if ("boot_id" %in% names(D4_Covariates)) D4_Covariates <- D4_Covariates[boot_id == 0]

    D4_Data <- merge(
      D4_Fup, D4_Covariates,
      by = c("person_id", "T0", "match_id"), all.x = TRUE,
      suffixes = c("", "_cov")
    )
    dup_cols <- grep("_cov$", names(D4_Data), value = TRUE)
    if (length(dup_cols) > 0) D4_Data[, (dup_cols) := NULL]

    weight_col <- NULL
    rm(D4_Fup, D4_Covariates, overlap_n)
  } else if (scenario$comparison == "compliance") {
    # Scenarios j–k: exposed compliers vs non-compliers (main and supplementary)
    # Load cohort file for matching variables, fup file for compliant_exposed, and covariates file
    D4_Cohort <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohort", scenario$cohort, ".parquet"))
      )
    )
    D4_Cohort <- D4_Cohort[group == "EXPOSED"]
    if ("boot_id" %in% names(D4_Cohort)) D4_Cohort <- D4_Cohort[boot_id == 0]

    cohort_keep <- intersect(c("person_id", "T0", "group", "match_id"), names(D4_Cohort))
    D4_Cohort <- D4_Cohort[, ..cohort_keep]

    D4_Fup <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohortFup", scenario$cohort, ".parquet"))
      )
    )
    if ("boot_id" %in% names(D4_Fup)) D4_Fup <- D4_Fup[boot_id == 0]

    # Filter to EXPOSED only and keep only necessary columns
    D4_Fup <- D4_Fup[group == "EXPOSED", .(person_id, T0, compliant_exposed, match_id)]

    # Merge cohort with fup to get both matching variables and compliance status
    D4_Cohort_Fup <- merge(
      D4_Cohort, D4_Fup,
      by = c("person_id", "T0", "match_id"), all.x = TRUE,
      suffixes = c("", "_fup")
    )

    # Create two groups based on compliance status
    D4_Complier <- copy(D4_Cohort_Fup[compliant_exposed == TRUE])
    D4_Complier[, group := "ExposedComplier"]
    D4_NonComplier <- copy(D4_Cohort_Fup[compliant_exposed == FALSE])
    D4_NonComplier[, group := "ExposedNonComplier"]
    D4_Cohort_Fup <- rbindlist(list(D4_Complier, D4_NonComplier), use.names = TRUE)
    D4_Cohort_Fup <- D4_Cohort_Fup[, .(person_id, T0, group, match_id)]

    D4_Covariates <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohortCovariates", scenario$cohort, ".parquet"))
      )
    )
    if ("boot_id" %in% names(D4_Covariates)) D4_Covariates <- D4_Covariates[boot_id == 0]

    # Left join: any cohort rows without covariates will have NAs
    D4_Data <- merge(
      D4_Cohort_Fup, D4_Covariates,
      by = c("person_id", "T0", "match_id"), all.x = TRUE,
      suffixes = c("", "_cov")
    )
    # Drop duplicate columns introduced by the join (cohort columns take priority)
    dup_cols <- grep("_cov$", names(D4_Data), value = TRUE)
    if (length(dup_cols) > 0) D4_Data[, (dup_cols) := NULL]

    weight_col <- NULL
    rm(D4_Cohort, D4_Fup, D4_Cohort_Fup, D4_Complier, D4_NonComplier, D4_Covariates)
  } else if (scenario$comparison == "compliant_exposed_all_controls") {
    # Scenarios l–m: compliant exposed vs all controls (Estimand 2 population, unweighted)
    # Load cohort file for matching variables, fup file for compliant_exposed, and covariates file
    D4_Cohort <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohort", scenario$cohort, ".parquet"))
      )
    )
    D4_Cohort <- D4_Cohort[group %in% c("EXPOSED", "CONTROL")]
    if ("boot_id" %in% names(D4_Cohort)) D4_Cohort <- D4_Cohort[boot_id == 0]

    cohort_keep <- intersect(c("person_id", "T0", "group", "match_id"), names(D4_Cohort))
    D4_Cohort <- D4_Cohort[, ..cohort_keep]

    D4_Fup <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohortFup", scenario$cohort, ".parquet"))
      )
    )
    if ("boot_id" %in% names(D4_Fup)) D4_Fup <- D4_Fup[boot_id == 0]

    # Filter fup to compliant exposed only
    D4_Fup <- D4_Fup[group == "EXPOSED" & compliant_exposed == TRUE, .(person_id, T0)]

    # Split cohort into exposed and control
    D4_Exposed <- D4_Cohort[group == "EXPOSED"]
    D4_Control_Cohort <- D4_Cohort[group == "CONTROL"]

    # Inner join exposed with fup to keep only compliant exposed individuals
    D4_Exposed <- merge(
      D4_Exposed, D4_Fup,
      by = c("person_id", "T0"), all.x = FALSE
    )
    D4_Exposed[, group := "ExposedComplier"]

    # Combine compliant exposed with controls
    D4_Cohort <- rbindlist(list(D4_Exposed, D4_Control_Cohort), use.names = TRUE)

    D4_Covariates <- as.data.table(
      arrow::read_parquet(
        file.path(dir_d4, paste0("D4_StudyCohortCovariates", scenario$cohort, ".parquet"))
      )
    )

    if ("boot_id" %in% names(D4_Covariates)) D4_Covariates <- D4_Covariates[boot_id == 0]

    # Left join: any cohort rows without covariates will have NAs
    D4_Data <- merge(
      D4_Cohort, D4_Covariates,
      by = c("person_id", "T0", "match_id"), all.x = TRUE,
      suffixes = c("", "_cov")
    )
    # Drop duplicate columns introduced by the join (cohort columns take priority)
    dup_cols <- grep("_cov$", names(D4_Data), value = TRUE)
    if (length(dup_cols) > 0) D4_Data[, (dup_cols) := NULL]

    weight_col <- NULL
    rm(D4_Cohort, D4_Fup, D4_Exposed, D4_Control_Cohort, D4_Covariates)
  }

  if (nrow(D4_Data) == 0) {
    logger::log_warn(paste0(
      "[Baseline tables] Scenario ", scenario$letter,
      " produced 0 rows after cohort construction; skipping this scenario."
    ))
    rm(D4_Data)
    gc()
    next
  }

  if (uniqueN(D4_Data$group) < 2) {
    logger::log_warn(paste0(
      "[Baseline tables] Scenario ", scenario$letter,
      " has fewer than 2 groups after cohort construction; skipping this scenario."
    ))
    rm(D4_Data)
    gc()
    next
  }

  # Initialize ASD collector for this scenario
  asd_table_scenario <- NULL

  # ============================================================
  # Inner loop: iterate over metadata files (table types)
  # ============================================================
  for (meta_idx in seq_along(metadata_files)) {
    table_numbers_current <- if (scenario$comparison == "censoring") {
      table_numbers_censoring
    } else {
      table_numbers
    }

    metadata_file <- metadata_files[meta_idx]
    table_num <- table_numbers_current[meta_idx]
    metadata_type <- sub("Baseline(Descriptives|Comorbidities|Comedications)_Metadata\\.csv", "\\1", metadata_file)

    logger::log_info(paste0(
      "[Baseline tables] Processing ", metadata_type, " for scenario ", scenario$letter
    ))

    # Load metadata
    table_metadata <- data.table::fread(file.path(dir_metadata, metadata_file))
    table_metadata[, expectedCat := as.character(expectedCat)]

    if ("SV_REGION" %in% table_metadata$var) {
      # Replace any empty string in SV_REGION with "0"
      D4_Data[, SV_REGION := data.table::fifelse(SV_REGION == "", "0", SV_REGION)]
      # Get all values of categorical variables for display in the table, called via metadata
      region_all <- sort(unique(D4_Data$SV_REGION))
      logger::log_info(paste0("SV_REGION in the study cohort: ", paste(region_all, collapse = ", ")))
      # for var SV_REGION, expectedCat equal region_all
      table_metadata[var == "SV_REGION", expectedCat := paste(region_all, collapse = ", ")]
    }
    if ("SV_SES" %in% table_metadata$var) {
      # Get all values of categorical variables for display in the table, called via metadata
      ses_all <- sort(unique(D4_Data$SV_SES))
      logger::log_info(paste0("SV_SES in the study cohort: ", paste(ses_all, collapse = ", ")))
      # for var SV_SES, expectedCat equal ses_all
      table_metadata[var == "SV_SES", expectedCat := paste(ses_all, collapse = ", ")]
    }

    # Resolve "get " categories via label_lookup.
    # When label_lookup is available, create the named objects that
    # DescriptivesTable evaluates internally (e.g. dap_georegion_cats).
    # When label_lookup is NULL, filter those rows out of the metadata so
    # DescriptivesTable does not attempt to evaluate undefined objects.
    created_objects <- character(0)
    metadata_get_rows <- table_metadata[startsWith(expectedCat, "get ")]
    unresolved_get_vars <- character(0)

    # If there are any "get " rows, attempt to resolve them using label_lookup.
    if (nrow(metadata_get_rows) > 0) {
      if (!is.null(label_lookup)) {
        for (i in seq_len(nrow(metadata_get_rows))) {
          cat_name <- sub("^get ", "", metadata_get_rows$expectedCat[i])
          categories <- label_lookup[VarName == metadata_get_rows$var[i], integerVal]

          # If categories are found, assign them to an object in the global environment with the expected name.
          if (length(categories) > 0) {
            assign(cat_name, categories, envir = .GlobalEnv)
            created_objects <- c(created_objects, cat_name)
          } else {
            # If no categories are found for this variable, log a warning and add it to the list of unresolved variables to filter out of the metadata.
            unresolved_get_vars <- c(unresolved_get_vars, metadata_get_rows$var[i])
            logger::log_warn(paste0(
              "[Baseline tables] label_lookup has no categories for variable '",
              metadata_get_rows$var[i],
              "' (",
              cat_name,
              "); skipping this variable."
            ))
          }
        }
      } else {
        # If label_lookup is not available, log a warning and skip all "get " variables.
        logger::log_warn(paste0(
          "[Baseline tables] label_lookup is NULL — skipping ",
          nrow(metadata_get_rows), " variable(s) with 'get ' categories: ",
          paste(metadata_get_rows$var, collapse = ", ")
        ))
        unresolved_get_vars <- metadata_get_rows$var
      }
      # If any "get " variables could not be resolved, filter them out of the metadata.
      if (length(unresolved_get_vars) > 0) {
        table_metadata <- table_metadata[!var %in% unresolved_get_vars]
      }
    }

    # Select only the columns present in both metadata and data.
    # Also filter table_metadata to matching vars so DescriptivesTable does
    # not attempt to access columns that are absent from the data.
    meta_vars <- table_metadata$var
    missing_vars <- setdiff(meta_vars, names(D4_Data))
    if (length(missing_vars) > 0) {
      logger::log_warn(paste0(
        "[Baseline tables] The following variables from metadata are missing in the data and will be skipped: ",
        paste(missing_vars, collapse = ", ")
      ))
    }
    keep_cols <- c("group", intersect(meta_vars, names(D4_Data)))
    if (!is.null(weight_col) && weight_col %in% names(D4_Data)) {
      keep_cols <- c(keep_cols, weight_col)
    }
    if ("exposure" %in% names(D4_Data)) keep_cols <- c(keep_cols, "exposure")
    D4_Table_Input <- D4_Data[, ..keep_cols]
    table_metadata <- table_metadata[var %in% names(D4_Table_Input)]

    # Fix issue with smoking, convert to integer
    if ("SV_SMOKING" %in% table_metadata$var) {
      D4_Table_Input[, SV_SMOKING := as.integer(as.character(SV_SMOKING))]
    }

    # Convert column types to match metadata specification:
    # NUM1 -> numeric, CAT -> character, TF -> logical
    # (this step can be skipped, but in the past we had issues with variable
    # types not correctly inferred by fread or arrow, causing issues).
    for (meta_row in seq_len(nrow(table_metadata))) {
      col <- table_metadata$var[meta_row]
      type <- table_metadata$type[meta_row]
      if (!col %in% names(D4_Table_Input)) next
      if (type == "NUM1") {
        D4_Table_Input[, (col) := as.numeric(get(col))]
      } else if (type == "CAT") {
        D4_Table_Input[, (col) := as.character(get(col))]
      } else if (type == "TF") {
        D4_Table_Input[, (col) := as.logical(get(col))]
      }
    }

    # Build DescriptivesTable call arguments.
    # use_weights accepts FALSE (unweighted) or a column name string (weighted).
    dt_args <- list(
      popdf          = D4_Table_Input,
      table_metadata = table_metadata,
      groupcol       = "group",
      output_format  = "processed",
      calculate_asd  = TRUE,
      keep_varinfo   = FALSE,
      label_lookup   = label_lookup,
      control_types  = FALSE,
      missing_vars   = expected_missing,
      missing_flag   = -99,
      round_decimals = 3,
      use_weights    = if (!is.null(weight_col)) weight_col else FALSE,
      weighted_stats = isTRUE(scenario$weighted_stats),
      output_asd     = TRUE
    )

    tryCatch(
      {
        tableout <- do.call(DescriptiveTable::DescriptivesTable, dt_args)

        # Collect ASD rows (excluding totals)
        tableout$asd$type <- metadata_type
        tableout$asd$scenario <- scenario$letter
        asd_table_scenario <- rbind(asd_table_scenario, tableout$asd[tableout$asd$var != "Total", ])

        # Rename output columns to consistent names
        # Column names from DescriptivesTable follow the pattern V{1,2,3}_{GROUP}
        group_levels <- unique(D4_Table_Input$group)
        if (scenario$comparison == "matched") {
          exposed_level <- if ("EXPOSED" %in% group_levels) "EXPOSED" else group_levels[1]
          other_level <- setdiff(group_levels, exposed_level)[1]
        } else if (scenario$comparison == "all_exposed") {
          exposed_level <- if ("MATCHED_EXPOSED" %in% group_levels) "MATCHED_EXPOSED" else group_levels[1]
          other_level <- if ("ALL_EXPOSED" %in% group_levels) "ALL_EXPOSED" else setdiff(group_levels, exposed_level)[1]
        } else if (scenario$comparison == "compliance") {
          exposed_level <- if ("ExposedComplier" %in% group_levels) "ExposedComplier" else group_levels[1]
          other_level <- if ("ExposedNonComplier" %in% group_levels) "ExposedNonComplier" else setdiff(group_levels, exposed_level)[1]
        } else if (scenario$comparison == "compliant_exposed_all_controls") {
          exposed_level <- if ("ExposedComplier" %in% group_levels) "ExposedComplier" else group_levels[1]
          other_level <- if ("CONTROL" %in% group_levels) "CONTROL" else setdiff(group_levels, exposed_level)[1]
        } else {
          exposed_level <- if ("NONADMIN_CENSORED" %in% group_levels) "NONADMIN_CENSORED" else group_levels[1]
          other_level <- if ("ADMIN_CENSORED" %in% group_levels) "ADMIN_CENSORED" else setdiff(group_levels, exposed_level)[1]
        }

        if (scenario$comparison == "matched") {
          new_names <- c(
            "N_exposed",         "perc_exposed",         "aux_exposed",
            "N_control",         "perc_control",         "aux_control"
          )
        } else if (scenario$comparison == "all_exposed") {
          new_names <- c(
            "N_matched_exposed", "perc_matched_exposed", "aux_matched_exposed",
            "N_all_exposed", "perc_all_exposed", "aux_all_exposed"
          )
        } else if (scenario$comparison == "compliance") {
          new_names <- c(
            "N_exposed_complier",     "perc_exposed_complier",     "aux_exposed_complier",
            "N_exposed_non_complier", "perc_exposed_non_complier", "aux_exposed_non_complier"
          )
        } else if (scenario$comparison == "compliant_exposed_all_controls") {
          new_names <- c(
            "N_exposed_complier",   "perc_exposed_complier",   "aux_exposed_complier",
            "N_control",            "perc_control",            "aux_control"
          )
        } else {
          new_names <- c(
            "N_nonadmin_censored",   "perc_nonadmin_censored",   "aux_nonadmin_censored",
            "N_admin_censored",      "perc_admin_censored",      "aux_admin_censored"
          )
        }
        old_names <- c(
          paste0("V", 1:3, "_", exposed_level),
          paste0("V", 1:3, "_", other_level)
        )
        existing <- old_names %in% names(tableout$table)
        if (any(existing)) {
          data.table::setnames(tableout$table, old_names[existing], new_names[existing])
        }

        # Repeat comparison within each exposure arm and add the
        # column blocks to pooled ones
        if (scenario$comparison == "censoring") {
          strata <- c(exposed = "EXPOSED", control = "CONTROL")
          for (stratum in names(strata)) {
            dt_args$popdf <- D4_Table_Input[exposure == strata[[stratum]]]
            block <- do.call(DescriptiveTable::DescriptivesTable, dt_args)$table
            data.table::setnames(block, old_names[existing], new_names[existing])
            block_cols <- intersect(c(new_names, "asd_1"), names(block))
            data.table::setnames(block, block_cols, paste0(block_cols, "_", stratum))
            block_cols <- paste0(block_cols, "_", stratum)
            tableout$table <- cbind(tableout$table, block[, ..block_cols])
          }
        }

        # Save table
        if (save_output) {
          table_id <- if (scenario$comparison == "censoring") {
            table_num
          } else {
            paste0(table_num, scenario$letter)
          }
          table_file_name <- paste0(deap_name, "_Table", table_id, "_", scenario$asd_label, ".csv")
          logger::log_info(paste0("[Baseline tables] Saving ", table_file_name, " to ", dir_d5))
          write.csv(tableout$table, file.path(dir_d5, table_file_name), row.names = FALSE)
        }

        logger::log_info(paste0(
          "[Baseline tables] Finished ", metadata_type, " scenario ", scenario$letter
        ))
      },
      error = function(e) {
        logger::log_warn(paste0(
          "[Baseline tables] Error generating ", metadata_type, " table for scenario ",
          scenario$letter, ": ", conditionMessage(e)
        ))
      }
    )

    # Remove dynamically assigned "get " category objects
    if (length(created_objects) > 0) rm(list = created_objects, envir = .GlobalEnv)
  }

  # Store ASD table for this scenario
  if (!isTRUE(scenario$weighted_stats)) {
    asd_tables[[scenario$letter]] <- asd_table_scenario
  }

  rm(D4_Data)
  gc()
}

# ============================================================
# Save pre-weight ASD tables (one file per scenario)
# ============================================================
if (save_asds_pre) {
  for (letter in names(asd_tables)) {
    sc <- table_scenarios[[which(sapply(table_scenarios, `[[`, "letter") == letter)]]
    asd_fname <- paste0("D5_ASD_PREWEIGHT_Baseline_", sc$asd_label, ".csv")
    logger::log_info(paste0("[Baseline tables] Saving ASD: ", asd_fname, " to ", dir_d5))
    write.csv(asd_tables[[letter]], file.path(dir_d5, asd_fname), row.names = FALSE)
  }
}

logger::log_info("[Baseline tables] - SUCCESS")

# Cleanup ---
suppressWarnings(rm(
  deap_name, dir_d4, dir_metadata, dir_d5, save_output, save_asds_pre,
  label_lookup, expected_missing,
  table_scenarios, metadata_files, table_numbers, asd_tables,
  table_numbers_censoring,
  scenario, meta_idx, metadata_file, table_num, metadata_type,
  table_metadata, meta_vars, keep_cols, D4_Table_Input, dt_args,
  tableout, group_levels, exposed_level, other_level,
  old_names, new_names, existing, weight_col,
  strata, stratum, block, block_cols,
  table_id, table_numbers_current,
  merge_keys,
  asd_table_scenario, created_objects, metadata_get_rows,
  cohort_keep, dup_cols, letter, sc, asd_fname, table_file_name,
  create_baseline_tables_config
))
gc()
