################################################################################
# create_cohort_analytic_outcomes ---
# Combined script for D5 Cohort Analytic Lookup computation and KM figure output.
#
# PURPOSE:
# Compute analytic lookup rows and KM figures in one metadata-driven pass while
# processing one unique analysis combination at a time to reduce peak memory use.
#
# DESIGN PRINCIPLE:
# - Single loop over unique analysis combos from design metadata
# - Load/process one combo input set at a time (no full preloading of D4 inputs)
# - Build one analytic output row per combo (or fallback -88 row on failure)
# - Produce all KM figures linked to that combo while combo data are in memory
# - Only at the end: bind analytic rows and export summary tables
#
# INPUT FILES:
# - configuration/config_project.yaml
# - data/D4_analytic_datasets/{FUP files from config}
# - data/D4_analytic_datasets/{weight files from config}
# - transformations/T4_statistical_analysis/metadata/cohort_analytic_design.csv
# - transformations/T4_statistical_analysis/metadata/cohort_analytic_tables.csv
#
# OUTPUT FILES:
# - data/D5_results/{DEAP}_D5_CohortAnalyticLookup.csv (full lookup)
# - data/D5_results/{DEAP}_Table*.csv (summary tables 6, 7, and 11)
# - data/D5_results/{DEAP}_Figure*_KM.pdf (KM figures 6)
################################################################################

# ============================================================================
# SECTION 1: ENVIRONMENT INITIALIZATION
# ============================================================================

# Check for LoggerManager (picard pipeline context).
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}

# If inside pipeline, register this script and capture print output to logs.
if (!base::is.null(lm)) {
  lm$start_script("create_cohort_analytic_outcomes")
  lm$start_capturing_prints()
}

library(data.table)
library(ggplot2)

# Start-of-script marker for audit logs and troubleshooting.
logger::log_info("[Cohort Analytic Outcomes] - START")

# Install/load CohortAnalytics package if not available.
if (!requireNamespace("CohortAnalytics", quietly = TRUE)) {
  # pak::pkg_install("github::UMC-Utrecht-RWE/CohortAnalytics@17-add-support-for-model-based-irs-in-exposed-adjusted-irs")
  pak::pkg_install("github::UMC-Utrecht-RWE/CohortAnalytics@6f4520dea7248f982436faef7bf68a8b1a11552b")
}

# Install plot dependencies if needed.
if (!requireNamespace("survminer", quietly = TRUE)) {
  install.packages("survminer")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
if (!requireNamespace("geeM", quietly = TRUE)) {
  install.packages("geeM")
}
if (!requireNamespace("msm", quietly = TRUE)) {
  install.packages("msm")
}
# ============================================================================
# SECTION 2: CONFIGURATION & AUDIT SETUP
# ============================================================================

config_project <- picard::load_config("configuration/config_project.yaml")

# Main analysis configuration.
config_analytic <- config_project$create_cohort_analytic_outcomes

# DAP name for labeling outputs and audit trail; must be set in project config.
this_dap <- config_project$DEAP_configuration$name

# Load helper files already used in separate scripts.
source(file.path(config_analytic$input$dir_functions, "fn_create_summarystats_table.R"))
source(file.path(config_analytic$input$dir_functions, "fn_create_km_figures.R"))
source(file.path(config_analytic$input$dir_functions, "fn_best_worst_scenarios.R"))
source(file.path(config_analytic$input$dir_functions, "fn_mask_counts.R"))

# Start audit trail with script identifier and DAP id.
picard::audit_start(
  file_name = "create_cohort_analytic_outcomes",
  deap_name = this_dap
)

# ============================================================================
# SECTION 3: EXTRACT CONFIGURATION PARAMETERS
# ============================================================================

# Input directories.
# D4: analytic datasets (follow-up + weights), metadata: design/tables CSVs.
dir_d4 <- config_analytic$input$dir_d4
dir_metadata <- config_analytic$input$dir_metadata

# Input file references.
cohort_analytic_design_file <- config_analytic$input$cohort_analytic_design
cohort_analytic_tables_file <- config_analytic$input$cohort_analytic_tables
fup_files <- config_analytic$input$fup_files
weight_files <- config_analytic$input$weight_files

# Analysis settings.
# Outcome label used in output table and plot titles.
aesi_name <- config_analytic$aesi_name
aesi_label <- config_analytic$aesi_label
# Keep a safe default for label if omitted in config.
if (is.null(aesi_label) || !nzchar(aesi_label)) aesi_label <- "Outcome"

# Time column used when IPCW files contain repeated rows over time.
time_col <- config_analytic$time_col
# Fall back to standard project convention.
if (is.null(time_col) || !nzchar(time_col)) time_col <- "time_days"

# Incidence-rate scaling (for example 100000 person-years).
scale_ir <- config_analytic$scale_IR
# Use default if missing or invalid.
if (is.null(scale_ir) || !is.numeric(scale_ir)) scale_ir <- 100000

# Optional fixed KM x-axis horizon; if NULL derive per combo.
default_max_fup_days <- config_analytic$default_max_fup_days
if (is.null(default_max_fup_days) || !is.numeric(default_max_fup_days)) {
  default_max_fup_days <- NULL
}

# Output settings.
# D5 destination and naming behavior for lookup/tables.
output_dir_d5 <- config_analytic$output$dir_d5
output_file_name <- config_analytic$output$output_file_name
table_export_mode <- config_analytic$output$table_export_mode
# Keep compatibility with helper default.
if (is.null(table_export_mode) || !nzchar(table_export_mode)) table_export_mode <- "individual"

# Masking settings.
# apply_masking: logical; if TRUE, small counts in D5 lookup are suppressed.
# mask_threshold: integer; counts strictly below this value are replaced with "<threshold".
apply_masking <- isTRUE(config_analytic$masking$apply_masking)
mask_threshold <- config_analytic$masking$mask_threshold
if (is.null(mask_threshold) || !is.numeric(mask_threshold)) mask_threshold <- 5L
mask_threshold <- as.integer(mask_threshold)

# ============================================================================
# SECTION 4: LOAD ANALYSIS METADATA
# ============================================================================

outcome_design <- picard::load(
  file_path = dir_metadata,
  file_name = cohort_analytic_design_file,
  sep = ";"
)[run_scenario == TRUE]
outcome_parameter_lookup <- picard::load(
  file_path = dir_metadata,
  file_name = cohort_analytic_tables_file
)

outcome_design <- data.table::as.data.table(outcome_design)
outcome_parameter_lookup <- data.table::as.data.table(outcome_parameter_lookup)

# KM id is required for the combined script (lookup + plots together).
if (!("km_figure_id" %in% names(outcome_design))) {
  stop("The design metadata file must include a 'km_figure_id' column.")
}

# Optional metadata column for best/worst-case sensitivity rows.
if (!("sensitivity_scenario" %in% names(outcome_design))) {
  outcome_design[, sensitivity_scenario := NA_integer_]
}
outcome_design[, sensitivity_scenario := as.integer(sensitivity_scenario)]

# ============================================================================
# SECTION 5: PREPARE ANALYSIS COMBINATIONS & TABLE ORDER
# ============================================================================

analysis_combos <- unique(outcome_design[, .(
  source_D4 = source_pop,
  estimand,
  source_weight,
  condition,
  weight_col
)])

# Normalize NA metadata fields so key generation stays deterministic.
analysis_combos[is.na(source_weight), source_weight := ""]
analysis_combos[is.na(condition), condition := ""]
analysis_combos[is.na(weight_col), weight_col := ""]
# Convert estimand explicitly for stable column name construction.
analysis_combos[, estimand := as.integer(estimand)]

# Combo key is the canonical identifier used for all joins and caching.
analysis_combos[, combo_key := paste(
  source_D4,
  paste0("estimand", estimand),
  source_weight,
  condition,
  weight_col,
  sep = "|"
)]

logger::log_info(paste0("[Cohort Analytic Outcomes] Found ", nrow(analysis_combos), " unique analysis combos"))

# Preserve design-driven table ordering.
table_order_lookup <- data.table::rbindlist(
  list(
    # IR tables keep first metric position per design row.
    outcome_design[, .(design_order = .I, metric_order = 1L, table_number = ir_table_id)],
    # RR/BW tables keep second metric position per design row.
    outcome_design[, .(design_order = .I, metric_order = 2L, table_number = rr_table_id)]
  ),
  use.names = TRUE
)

# Remove blank ids before preserving order.
table_order_lookup <- table_order_lookup[!is.na(table_number) & nzchar(table_number)]
# Keep metadata order deterministic for combined exports.
data.table::setorder(table_order_lookup, design_order, metric_order)
# Ordered unique vector used by summary-table helper.
table_order <- unique(table_order_lookup$table_number)

# Validate configured source names before loop.
fup_names <- names(fup_files)
all_weight_names <- names(weight_files)

needed_fup_names <- unique(analysis_combos$source_D4)
missing_fup_names <- setdiff(needed_fup_names, fup_names)
if (length(missing_fup_names) > 0L) {
  # Fail fast if metadata references unknown follow-up datasets.
  stop(paste0("Missing configured FUP files for: ", paste(missing_fup_names, collapse = ", ")))
}

needed_weight_names <- unique(unlist(strsplit(analysis_combos[source_weight != "", source_weight], ",")))
# Trim whitespace around comma-delimited sources.
needed_weight_names <- trimws(needed_weight_names)
# Remove empty tokens if metadata has accidental commas.
needed_weight_names <- unique(needed_weight_names[nzchar(needed_weight_names)])
missing_weight_names <- setdiff(needed_weight_names, all_weight_names)
if (length(missing_weight_names) > 0L) {
  # Fail fast before expensive loop work.
  stop(paste0("Missing configured weight files for: ", paste(missing_weight_names, collapse = ", ")))
}

# ============================================================================
# SECTION 6: LOOP OVER UNIQUE COMBINATIONS (ONE-AT-A-TIME PROCESSING)
# ============================================================================

result_list <- list()
# Shared key columns for FUP/weight merges.
merge_keys <- c("person_id", "T0", "group", "match_id")

# Numeric result fields to fill with fallback code when computation is impossible.
fallback_numeric_cols <- setdiff(names(outcome_parameter_lookup), "tab_group")

for (i in seq_len(nrow(analysis_combos))) {
  # Read one combo definition row from metadata-derived combos.
  combo <- analysis_combos[i]
  # Stable key value for logs/results/joins.
  combo_key <- combo$combo_key
  logger::log_info(paste0("[Cohort Analytic Outcomes] Running combo: ", combo_key))

  # Pull all design rows that share this base combo (NA and/or scenario 1..4).
  combo_rows <- outcome_design[
    source_pop == combo$source_D4 &
      as.integer(estimand) == combo$estimand &
      fifelse(is.na(source_weight), "", source_weight) == combo$source_weight &
      fifelse(is.na(condition), "", condition) == combo$condition &
      fifelse(is.na(weight_col), "", weight_col) == combo$weight_col
  ]

  # Scenario values requested for this base combo.
  # NA (main analysis) must be ordered first so the scenario 3 alias below
  # can reference an already-computed result_list entry.
  scenario_values <- unique(combo_rows$sensitivity_scenario)
  if (length(scenario_values) == 0L) scenario_values <- NA_integer_
  scenario_values <- scenario_values[order(is.na(scenario_values), decreasing = TRUE)]

  # Build full combo keys (including scenario token) used by result lookup joins.
  scenario_keys <- sapply(scenario_values, function(sid) {
    paste(
      combo$source_D4,
      paste0("estimand", combo$estimand),
      combo$source_weight,
      combo$condition,
      combo$weight_col,
      ifelse(is.na(sid), "scenarioNA", paste0("scenario", sid)),
      sep = "|"
    )
  })

  # Prepare per-combo default result state.
  combo_result <- NULL

  # ------------------------------------------------------------------------
  # 6.1: Load and prepare follow-up dataset for current combo
  # ------------------------------------------------------------------------

  # Check if FUP file exists before attempting to load
  fup_file_name <- fup_files[[combo$source_D4]]
  fup_file_path <- file.path(dir_d4, fup_file_name)

  if (!file.exists(fup_file_path)) {
    logger::log_warn(paste0(
      "[Cohort Analytic Outcomes] FUP file does not exist for combo ", combo_key,
      ": ", fup_file_path, "; skipping this combo."
    ))
    # Skip all scenarios for this combo and record fallback rows
    for (scenario_key in scenario_keys) {
      fallback_dt <- data.table::data.table(combo_key = scenario_key)
      fallback_dt[, (fallback_numeric_cols) := -88]
      result_list[[scenario_key]] <- fallback_dt
    }
    next
  }

  fup_dt <- tryCatch(
    # Load current combo's follow-up file only when needed.
    data.table::as.data.table(picard::load(file_path = dir_d4, file_name = fup_file_name)),
    error = function(e) {
      logger::log_error(paste0("Failed to load FUP for ", combo_key, ": ", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(fup_dt)) {
    # If source data cannot be loaded, record fallback row and continue.
    for (scenario_key in scenario_keys) {
      fallback_dt <- data.table::data.table(combo_key = scenario_key)
      fallback_dt[, (fallback_numeric_cols) := -88]
      result_list[[scenario_key]] <- fallback_dt
    }
    next
  }

  if ("boot_id" %in% names(fup_dt)) {
    # Keep primary (non-bootstrap) rows for reporting outputs.
    fup_dt <- fup_dt[boot_id == 0]
  }

  # Build estimand-specific column names once for this combo.
  fup_col <- paste0("fup_length_days_est", combo$estimand)
  pyr_col <- paste0("fup_length_pyr_est", combo$estimand)
  event_col <- paste0("event_count_est", combo$estimand)
  censor_col <- "censor_nonadmin_ipcw"

  if (nzchar(combo$condition)) {
    # Evaluate pair-level condition expression from metadata.
    cond_ok <- tryCatch(
      eval(parse(text = combo$condition), envir = fup_dt),
      error = function(e) {
        logger::log_error(paste0("Condition failed for ", combo_key, ": ", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(cond_ok) || length(cond_ok) != nrow(fup_dt)) {
      # Invalid condition execution means combo cannot be computed.
      for (scenario_key in scenario_keys) {
        fallback_dt <- data.table::data.table(combo_key = scenario_key)
        fallback_dt[, (fallback_numeric_cols) := -88]
        result_list[[scenario_key]] <- fallback_dt
      }
      next
    }

    fup_dt[, cond_ok_row := !is.na(cond_ok) & as.logical(cond_ok)]

    n_drop <- sum(!fup_dt$cond_ok_row)
    if (n_drop > 0L) {
      # Drop only individual rows failing the condition; retain their matched
      # controls so that all control units are preserved. Residual imbalance
      # introduced by this asymmetric exclusion is corrected by IPSMW.
      fup_dt <- fup_dt[cond_ok_row == TRUE]
      logger::log_warn(paste0(
        "Condition '", combo$condition,
        "' dropped ", n_drop, " individual row(s)"
      ))
    }

    fup_dt[, cond_ok_row := NULL]
  }

  # Resolve weight metadata needed for both paths before branching.
  analysis_weight_col <- if (nzchar(combo$weight_col)) combo$weight_col else "ip_weight"
  weight_sources <- if (nzchar(combo$source_weight)) {
    trimws(strsplit(combo$source_weight, ",")[[1]])
  } else {
    character(0)
  }
  # IPCW combos use the long person-day table as the analysis base and pass
  # it directly to compute_rates_cohort, preserving the time-varying weights.
  is_ipcw_combo <- any(grepl("ipcw", weight_sources, ignore.case = TRUE))

  if (is_ipcw_combo) {
    # ------------------------------------------------------------------------
    # 6.2/6.3 (IPCW path): Build long person-day analytic dataset
    # ------------------------------------------------------------------------
    # Compliance filtering has already been applied to fup_dt; the retained
    # person_id set is propagated into the long table so that non-compliant
    # exposed persons are excluded while all control persons are kept.
    included_person_ids <- unique(fup_dt$person_id)

    ipcw_source_name <- weight_sources[grepl("ipcw", weight_sources, ignore.case = TRUE)][1L]

    # Check if IPCW weight file exists before attempting to load
    ipcw_file_name <- weight_files[[ipcw_source_name]]
    ipcw_file_path <- file.path(dir_d4, ipcw_file_name)

    if (!file.exists(ipcw_file_path)) {
      logger::log_warn(paste0(
        "[Cohort Analytic Outcomes] IPCW weight file does not exist for combo ",
        combo_key, ": ", ipcw_file_path, "; skipping this combo."
      ))
      for (scenario_key in scenario_keys) {
        fallback_dt <- data.table::data.table(combo_key = scenario_key)
        fallback_dt[, (fallback_numeric_cols) := -88]
        result_list[[scenario_key]] <- fallback_dt
      }
      next
    }

    ipcw_long <- tryCatch(
      data.table::as.data.table(picard::load(
        file_path = dir_d4,
        file_name = ipcw_file_name
      )),
      error = function(e) {
        logger::log_error(paste0(
          "Failed to load IPCW long table '", ipcw_source_name,
          "' for ", combo_key, ": ", conditionMessage(e)
        ))
        NULL
      }
    )

    if (is.null(ipcw_long)) {
      for (scenario_key in scenario_keys) {
        fallback_dt <- data.table::data.table(combo_key = scenario_key)
        fallback_dt[, (fallback_numeric_cols) := -88]
        result_list[[scenario_key]] <- fallback_dt
      }
      next
    }

    if ("boot_id" %in% names(ipcw_long)) ipcw_long <- ipcw_long[boot_id == 0]

    # Restrict to persons retained after compliance condition.
    ipcw_long <- ipcw_long[person_id %in% included_person_ids]

    if (nrow(ipcw_long) == 0L) {
      logger::log_warn(paste0(
        "No rows remain in IPCW long table after filtering for ", combo_key
      ))
      for (scenario_key in scenario_keys) {
        fallback_dt <- data.table::data.table(combo_key = scenario_key)
        fallback_dt[, (fallback_numeric_cols) := -88]
        result_list[[scenario_key]] <- fallback_dt
      }
      next
    }

    # Initialise composite weight from cumulative trimmed IPCW.
    ipcw_long[, (analysis_weight_col) := ipcw_cum_trim]

    # For Estimand 2: multiply in the baseline IPSMW (time-invariant, one
    # row per person). The merge broadcasts it across all person-days.
    ipsmw_sources <- weight_sources[grepl("ipsmw", weight_sources, ignore.case = TRUE)]
    if (length(ipsmw_sources) > 0L) {
      ipsmw_load_failed <- FALSE
      for (ipsmw_name in ipsmw_sources) {
        # Check if IPSMW weight file exists before attempting to load
        ipsmw_file_name <- weight_files[[ipsmw_name]]
        ipsmw_file_path <- file.path(dir_d4, ipsmw_file_name)

        if (!file.exists(ipsmw_file_path)) {
          logger::log_warn(paste0(
            "[Cohort Analytic Outcomes] IPSMW weight file does not exist for combo ",
            combo_key, ": ", ipsmw_file_path, "; skipping this combo."
          ))
          ipsmw_load_failed <- TRUE
          break
        }

        ipsmw_wt <- tryCatch(
          data.table::as.data.table(picard::load(
            file_path = dir_d4,
            file_name = ipsmw_file_name
          )),
          error = function(e) {
            logger::log_error(paste0(
              "Failed to load IPSMW '", ipsmw_name,
              "' for ", combo_key, ": ", conditionMessage(e)
            ))
            NULL
          }
        )
        if (is.null(ipsmw_wt)) {
          ipsmw_load_failed <- TRUE
          break
        }
        if ("boot_id" %in% names(ipsmw_wt)) ipsmw_wt <- ipsmw_wt[boot_id == 0]
        ipsmw_col <- if ("ipsmw_raw_trunc" %in% names(ipsmw_wt)) {
          "ipsmw_raw_trunc"
        } else if ("ipsmw_raw" %in% names(ipsmw_wt)) {
          "ipsmw_raw"
        } else {
          candidate_cols <- setdiff(names(ipsmw_wt), c(merge_keys, "boot_id"))
          numeric_candidates <- candidate_cols[
            vapply(ipsmw_wt[, ..candidate_cols], is.numeric, logical(1))
          ]
          if (length(numeric_candidates) == 1L) {
            numeric_candidates[1L]
          } else {
            logger::log_error(paste0(
              "Cannot resolve IPSMW column for ", ipsmw_name,
              " in combo ", combo_key
            ))
            ipsmw_load_failed <- TRUE
            NA_character_
          }
        }
        if (isTRUE(ipsmw_load_failed) || is.na(ipsmw_col)) {
          ipsmw_load_failed <- TRUE
          break
        }
        ipsmw_keep <- ipsmw_wt[, c(merge_keys, ipsmw_col), with = FALSE]
        data.table::setnames(ipsmw_keep, ipsmw_col, ".ipsmw_tmp")
        # Left join broadcasts the time-invariant IPSMW across all person-days.
        ipcw_long <- merge(ipcw_long, ipsmw_keep, by = merge_keys, all.x = FALSE)
        ipcw_long[, (analysis_weight_col) := get(analysis_weight_col) * .ipsmw_tmp]
        ipcw_long[, .ipsmw_tmp := NULL]
      }
      if (isTRUE(ipsmw_load_failed)) {
        for (scenario_key in scenario_keys) {
          fallback_dt <- data.table::data.table(combo_key = scenario_key)
          fallback_dt[, (fallback_numeric_cols) := -88]
          result_list[[scenario_key]] <- fallback_dt
        }
        next
      }
    }

    # Each row is exactly 1 person-day; set standard analytic columns so
    # compute_rates_cohort receives the same interface as the standard path.
    ipcw_long[, fup := 1]
    ipcw_long[, pyr := 1 / 365.25]
    ipcw_long[, eventCount := as.integer(outcome_event)]
    ipcw_long[, id := match_id]

    aesifup <- ipcw_long[, c(
      merge_keys, "fup", "pyr", "eventCount", analysis_weight_col, "id"
    ), with = FALSE]
  } else {
    # ------------------------------------------------------------------------
    # 6.2: Harmonize estimand-specific columns (standard person-level path)
    # ------------------------------------------------------------------------
    # Keep only join keys + estimand-specific outcome columns.
    needed_fup_cols <- c(merge_keys, fup_col, pyr_col, event_col)
    if (censor_col %in% names(fup_dt)) {
      needed_fup_cols <- c(needed_fup_cols, censor_col)
    }
    missing_fup_cols <- setdiff(needed_fup_cols, names(fup_dt))

    if (length(missing_fup_cols) > 0L) {
      # Missing columns indicate incomplete/invalid D4 inputs for this combo.
      logger::log_error(paste0(
        "Missing required follow-up columns for ", combo_key, ": ",
        paste(missing_fup_cols, collapse = ", ")
      ))
      for (scenario_key in scenario_keys) {
        fallback_dt <- data.table::data.table(combo_key = scenario_key)
        fallback_dt[, (fallback_numeric_cols) := -88]
        result_list[[scenario_key]] <- fallback_dt
      }
      next
    }

    # Subset to required columns to limit memory footprint.
    aesifup <- fup_dt[, ..needed_fup_cols]
    # Standardized names expected by downstream analytics and KM code.
    rename_old <- c(fup_col, pyr_col, event_col)
    rename_new <- c("fup", "pyr", "eventCount")
    if (censor_col %in% names(aesifup)) {
      rename_old <- c(rename_old, censor_col)
      rename_new <- c(rename_new, "censor_nonadmin")
    }
    data.table::setnames(aesifup, old = rename_old, new = rename_new)
    # Matched-pair id used as robust cluster id in some estimators.
    aesifup[, id := match_id]

    # ------------------------------------------------------------------------
    # 6.3: Build analysis weight (none, single, composite)
    # ------------------------------------------------------------------------
    if (length(weight_sources) == 0L) {
      # Unweighted analysis uses constant weight 1.
      aesifup[, (analysis_weight_col) := 1]
    } else {
      # Track whether any weight step fails so combo can fall back cleanly.
      merge_weight_failed <- FALSE

      for (j in seq_along(weight_sources)) {
        # Current component (for composite this iterates over each source).
        weight_name <- weight_sources[j]

        # Check if weight file exists before attempting to load
        weight_file_name <- weight_files[[weight_name]]
        weight_file_path <- file.path(dir_d4, weight_file_name)

        if (!file.exists(weight_file_path)) {
          logger::log_warn(paste0(
            "[Cohort Analytic Outcomes] Weight file does not exist for combo ",
            combo_key, ": ", weight_file_path, " (source: ", weight_name, "); skipping this combo."
          ))
          merge_weight_failed <- TRUE
          break
        }

        wt <- tryCatch(
          # Load only current weight source for this combo.
          data.table::as.data.table(picard::load(file_path = dir_d4, file_name = weight_file_name)),
          error = function(e) {
            logger::log_error(paste0("Failed to load weight '", weight_name, "' for ", combo_key, ": ", conditionMessage(e)))
            NULL
          }
        )

        if (is.null(wt)) {
          # Immediate fallback trigger when a weight source is unavailable.
          merge_weight_failed <- TRUE
          break
        }

        if ("boot_id" %in% names(wt)) {
          # Keep non-bootstrap rows for reporting outputs.
          wt <- wt[boot_id == 0]
        }

        # Resolve which numeric weight column to read from source file.
        source_col <- if (analysis_weight_col %in% names(wt)) {
          analysis_weight_col
        } else if (grepl("ipsmw", weight_name, ignore.case = TRUE) && "ipsmw_raw" %in% names(wt)) {
          "ipsmw_raw"
        } else if ("ip_weight" %in% names(wt)) {
          "ip_weight"
        } else {
          candidate_cols <- setdiff(names(wt), c(merge_keys, "boot_id", time_col))
          numeric_candidates <- candidate_cols[vapply(wt[, ..candidate_cols], is.numeric, logical(1))]
          if (length(numeric_candidates) == 1L) {
            numeric_candidates[1]
          } else {
            logger::log_error(paste0(
              "Could not resolve source weight column for ", weight_name,
              " (combo ", combo_key, ")"
            ))
            merge_weight_failed <- TRUE
            NA_character_
          }
        }

        if (isTRUE(merge_weight_failed) || is.na(source_col)) {
          # Abort weight merge path and write fallback row.
          merge_weight_failed <- TRUE
          break
        }

        # Keep only merge keys plus resolved source weight column.
        wt_keep <- wt[, c(merge_keys, source_col), with = FALSE]
        # Temporary component column (.w1, .w2, ...).
        temp_col <- paste0(".w", j)
        data.table::setnames(wt_keep, source_col, temp_col)
        # Left join preserves follow-up rows that might have missing weights.
        aesifup <- merge(aesifup, wt_keep, by = merge_keys, all.x = FALSE)
      }

      if (isTRUE(merge_weight_failed)) {
        # Standardized fallback behavior for weight path failures.
        for (scenario_key in scenario_keys) {
          fallback_dt <- data.table::data.table(combo_key = scenario_key)
          fallback_dt[, (fallback_numeric_cols) := -88]
          result_list[[scenario_key]] <- fallback_dt
        }
        next
      }

      weight_tmp_cols <- grep("^\\.w", names(aesifup), value = TRUE)
      if (length(weight_tmp_cols) == 1L) {
        # Single-weight case: promote temporary column to final name.
        data.table::setnames(aesifup, weight_tmp_cols, analysis_weight_col)
      } else {
        # Composite-weight case: multiply all components row-wise.
        aesifup[, (analysis_weight_col) := Reduce(`*`, .SD), .SDcols = weight_tmp_cols]
        # Remove temporary intermediate columns.
        aesifup[, (weight_tmp_cols) := NULL]
      }
    }
  } # end is_ipcw_combo branch

  # ------------------------------------------------------------------------
  # 6.4: Validate and clean analysis dataset
  # ------------------------------------------------------------------------
  aesifup[, invalid_pyr_row := is.na(pyr) | !is.finite(pyr) | pyr <= 0]
  aesifup[, invalid_pyr_pair := any(invalid_pyr_row), by = match_id]
  if (any(aesifup$invalid_pyr_row)) {
    # If desired, the pair-level flag can be used to drop complete matched pairs.
    logger::log_warn(paste0(
      "Found ", sum(aesifup$invalid_pyr_row), " row(s) with invalid person-time in combo ", combo_key,
      "; please revise."
    ))
  }

  aesifup[, invalid_weight_row := is.na(get(analysis_weight_col)) | !is.finite(get(analysis_weight_col))]
  aesifup[, invalid_weight_pair := any(invalid_weight_row), by = match_id]
  if (any(aesifup$invalid_weight_row)) {
    # If desired, the pair-level flag can be used to drop complete matched pairs.
    logger::log_warn(paste0(
      "Found ", sum(aesifup$invalid_weight_row), " row(s) with invalid weights in combo ", combo_key,
      "; please revise."
    ))
  }

  aesifup[, c("invalid_pyr_row", "invalid_pyr_pair", "invalid_weight_row", "invalid_weight_pair") := NULL]

  # If no usable rows remain, return fallback row and skip KM.
  if (nrow(aesifup) == 0L) {
    logger::log_warn(paste0("No valid rows remain for combo ", combo_key, "; writing fallback row."))
    # Keep combo represented in lookup output even when not estimable.
    for (scenario_key in scenario_keys) {
      fallback_dt <- data.table::data.table(combo_key = scenario_key)
      fallback_dt[, (fallback_numeric_cols) := -88]
      result_list[[scenario_key]] <- fallback_dt
    }
    next
  }

  # Standardize columns for estimation and plotting.
  # Set factor level order for consistent CONTROL vs EXPOSED interpretation.
  aesifup[, group := factor(group, levels = c("CONTROL", "EXPOSED"))]
  # Numeric id helps ordering and robust model computations.
  aesifup[, person_id_num := as.numeric(as.factor(person_id))]
  # Stable order for reproducibility.
  data.table::setorder(aesifup, person_id_num, T0)
  # Offset term used by Poisson-style models in CohortAnalytics.
  aesifup[, pyr_offset := log(pyr)]

  # ------------------------------------------------------------------------
  # 6.5: Compute analytic lookup row(s) for each scenario of this base combo
  # ------------------------------------------------------------------------
  for (scenario_id in scenario_values) {
    if (is.na(scenario_id)) {
      logger::log_info(paste0(
        "Computing analysis outcomes for combo ", combo_key
      ))
    } else {
      logger::log_info(paste0(
        "Computing analysis outcomes for sensitivity scenario ", scenario_id,
        ", combo ", combo_key
      ))
    }

    scenario_key <- paste(
      combo$source_D4,
      paste0("estimand", combo$estimand),
      combo$source_weight,
      combo$condition,
      combo$weight_col,
      ifelse(is.na(scenario_id), "scenarioNA", paste0("scenario", scenario_id)),
      sep = "|"
    )

    # # Scenario 3 (both-best) is a mathematical identity on the main analysis:
    # # censored individuals with no event already have eventCount == 0, which
    # # is exactly what apply_best_worst_scenario would set. Re-running the
    # # model would produce the same estimates. Instead, copy the NA-scenario
    # # result and re-stamp its combo_key to avoid a redundant model call.
    # if (!is.na(scenario_id) && scenario_id == 3L) {
    #   na_scenario_key <- paste(
    #     combo$source_D4,
    #     paste0("estimand", combo$estimand),
    #     combo$source_weight,
    #     combo$condition,
    #     combo$weight_col,
    #     "scenarioNA",
    #     sep = "|"
    #   )
    #   if (!is.null(result_list[[na_scenario_key]])) {
    #     aliased <- data.table::copy(result_list[[na_scenario_key]])
    #     aliased[, combo_key := scenario_key]
    #     result_list[[scenario_key]] <- aliased
    #     logger::log_info(paste0(
    #       "[Cohort Analytic Outcomes] Scenario 3 (both-best) aliased from main ",
    #       "analysis for combo ", combo_key
    #     ))
    #   } else {
    #     logger::log_warn(paste0(
    #       "NA-scenario result not found for aliasing scenario 3 in combo ",
    #       combo_key, "; writing fallback row."
    #     ))
    #     fallback_dt <- data.table::data.table(combo_key = scenario_key)
    #     fallback_dt[, (fallback_numeric_cols) := -88]
    #     result_list[[scenario_key]] <- fallback_dt
    #   }
    #   next
    # }

    aesifup_model <- data.table::copy(aesifup)

    # Apply best/worst-case scenario imputation only when scenario id is set.
    if (!is.na(scenario_id)) {
      if (!("censor_nonadmin" %in% names(aesifup_model))) {
        logger::log_error(paste0("Missing censor column ", censor_col, " for scenario combo ", scenario_key))
        fallback_dt <- data.table::data.table(combo_key = scenario_key)
        fallback_dt[, (fallback_numeric_cols) := -88]
        result_list[[scenario_key]] <- fallback_dt
        next
      }
      aesifup_model <- apply_best_worst_scenario(
        fup_dt = aesifup_model,
        event_col = "eventCount",
        censor_col = "censor_nonadmin",
        scenario_id = scenario_id,
        group_col = "group"
      )
    }

    res <- tryCatch(
      # Main model call for incidence/risk measures.
      CohortAnalytics::compute_rates_cohort(
        aesifup_input = aesifup_model,
        fupCol = "fup",
        pyrCol = "pyr",
        pyr_offset = "pyr_offset",
        eventCol = "eventCount",
        iptw = analysis_weight_col,
        target_aesi = ifelse(combo$estimand == 1, "covid_diagnosis_or_death", "covid_diagnosis"),
        type = "incidence",
        CImethod = "wilson",
        risk_type = "incidence",
        CumulativeInc = FALSE,
        comparison_measures = TRUE,
        scale_IR = scale_ir,
        model_based_control = TRUE,
        weighted_IR = TRUE,
        end_risk = NULL,
        output_format = "data.frame",
        minimum_count_for_comparative = 3,
        unique_person_cols = c("person_id", "match_id"),
        model_library = ifelse(is_ipcw_combo,"bigglm","geeM"),
        chunksize = 100000
      ),
      error = function(e) {
        logger::log_error(paste0("Error in compute_rates_cohort for ", scenario_key, ": ", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(res) || nrow(data.table::as.data.table(res)) == 0L) {
      # Uniform fallback when model call fails or yields empty output.
      fallback_dt <- data.table::data.table(combo_key = scenario_key)
      fallback_dt[, (fallback_numeric_cols) := -88]
      combo_result <- fallback_dt
    } else {
      # Attach combo key so later joins map back to design metadata.
      combo_result <- data.table::as.data.table(res)

      # drop columns with a name that contains the string "cuminc" as they are not needed for the final output
      if (!is.null(combo_result) && any(grepl("cuminc", names(combo_result)))) {
        cuminc_cols <- grep("cuminc", names(combo_result), value = TRUE)
        combo_result[, (cuminc_cols) := NULL]
      }

      combo_result[, combo_key := scenario_key]
    }

    result_list[[scenario_key]] <- combo_result
  }

  # ------------------------------------------------------------------------
  # 6.6: Create KM figures for metadata rows linked to current combo
  # ------------------------------------------------------------------------
  km_rows <- outcome_design[
    # Match same combo-defining fields used in analysis_combos/combo_key.
    source_pop == combo$source_D4 &
      as.integer(estimand) == combo$estimand &
      fifelse(is.na(source_weight), "", source_weight) == combo$source_weight &
      fifelse(is.na(condition), "", condition) == combo$condition &
      fifelse(is.na(weight_col), "", weight_col) == combo$weight_col &
      is.na(sensitivity_scenario) &
      !is.na(km_figure_id) & nzchar(km_figure_id)
  ]

  if (nrow(km_rows) == 0L) {
    logger::log_info(paste0("[Cohort Analytic Outcomes] No KM figure requested for combo ", combo_key))
  } else if (uniqueN(aesifup$group) < 2L) {
    # KM requires both exposure groups to be present.
    logger::log_warn(paste0("[Cohort Analytic Outcomes] Fewer than two groups remain for combo ", combo_key, "; skipping KM."))
  } else {
    fit <- tryCatch(
      # Weighted KM fit with robust variance and clustered id.
      survival::survfit(
        survival::Surv(pyr * 365.25, eventCount) ~ group,
        weights = aesifup[[analysis_weight_col]],
        data = aesifup,
        robust = TRUE,
        id = person_id
      ),
      error = function(e) {
        logger::log_error(paste0("KM fit failed for combo ", combo_key, ": ", conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(fit)) {
      max_fup_days <- if (!is.null(default_max_fup_days)) {
        # Use fixed horizon from config when provided.
        default_max_fup_days
      } else {
        # Otherwise derive from observed follow-up in current combo.
        ceiling(max(aesifup$fup, na.rm = TRUE))
      }

      if (!is.finite(max_fup_days) || max_fup_days <= 0) {
        logger::log_warn(paste0("Invalid max follow-up for combo ", combo_key, "; skipping KM."))
      } else {
        # Approximate weekly ticks across follow-up horizon.
        day_ticks_x <- max(1, round(max_fup_days / 7, 0))

        for (k in seq_len(nrow(km_rows))) {
          # Each metadata row can request a separate figure id/title.
          analysis_row <- km_rows[k]
          figure_id <- analysis_row$km_figure_id
          # Clean id for safe filenames.
          figure_id_safe <- gsub("[^A-Za-z0-9._-]", "_", figure_id)

          logger::log_info(paste0("[Cohort Analytic Outcomes] Creating KM figure ", figure_id, " for combo ", combo_key))

          # Prepare figure title
          figure_title <- if (analysis_row$estimand == 1) {
            paste0(
              "Figure ", figure_id, ". Cumulative incidence of composite outcome",
              "\n (", analysis_row$description, ")"
            )
          } else {
            paste0(
              "Figure ", figure_id, ". Weighted cumulative incidence of covid-19 diagnosis",
              "\n (", analysis_row$description, ")"
            )
          }
          figure_title <- paste(
            strwrap(
              figure_title,
              width = 72
            ),
            collapse = "\n"
          )

          # Subtitle carries cohort/estimand/weight context.
          figure_subtitle <- paste(
            strwrap(
              paste0(
                "Cohort: ", analysis_row$cohort,
                " | Estimand: ", analysis_row$estimand,
                " | Weights: ", if (nzchar(analysis_row$source_weight)) analysis_row$source_weight else "none"
              ),
              width = 72
            ),
            collapse = "\n"
          )

          figobj <- tryCatch(
            # Build plot+tables object using existing KM helper.
            kmplot_custom(
              fit = fit,
              aesifup = aesifup,
              figlab = figure_id,
              title = NULL,
              mask_table = FALSE,
              cohort_name = analysis_row$cohort,
              max_fup = max_fup_days,
              day_ticks_x = day_ticks_x,
              ylab_root = "Cumulative Incidence (1-KM)\nper ",
              scale_IR = scale_ir,
              xlab = "Days since index date",
              aesi_label = aesi_label,
              this_dap = this_dap
            ),
            error = function(e) {
              logger::log_error(paste0("KM plotting object failed for figure ", figure_id, ": ", conditionMessage(e)))
              NULL
            }
          )

          if (is.null(figobj)) {
            # Skip only this figure id; continue loop for remaining ids.
            next
          }

          # Apply title/subtitle after helper output creation.
          figobj$plot <- figobj$plot + ggplot2::ggtitle(
            label = figure_title
            # subtitle = figure_subtitle
          )
          # Patchwork alignment fix for plot/table layout.
          figobj <- align_elements(figobj)

          figure_file_name <- paste0(this_dap, "_Figure", figure_id_safe, "_KM.png")
          save_ok <- tryCatch(
            {
              # Save as high-resolution PDF in D5 results folder.
              ggplot2::ggsave(
                filename = figure_file_name,
                path = output_dir_d5,
                plot = figobj,
                width = 15,
                height = 15,
                dpi = 300,
                units = "cm"
              )
              TRUE
            },
            error = function(e) {
              logger::log_error(paste0("Failed saving figure ", figure_id, ": ", conditionMessage(e)))
              FALSE
            }
          )

          if (isTRUE(save_ok)) {
            # Success log for downstream audit traceability.
            logger::log_info(paste0("[Cohort Analytic Outcomes] Saved figure ", figure_id))
          }
        }
      }
    }
  }

  # ------------------------------------------------------------------------
  # 6.7: Explicit per-combo cleanup to reduce memory footprint
  # ------------------------------------------------------------------------
  suppressWarnings(rm(
    # Per-combo objects removed aggressively to limit peak RAM.
    fup_dt, cond_ok, n_drop,
    is_ipcw_combo, included_person_ids,
    ipcw_source_name, ipcw_long,
    ipsmw_sources, ipsmw_load_failed, ipsmw_name, ipsmw_wt, ipsmw_col, ipsmw_keep,
    fup_col, pyr_col, event_col, needed_fup_cols, missing_fup_cols,
    aesifup, analysis_weight_col, weight_sources,
    merge_weight_failed, j, weight_name, wt,
    source_col, wt_keep, temp_col, weight_tmp_cols,
    rename_old, rename_new,
    invalid_pyr, drop_pair_ids, invalid_weight, drop_pair_ids_weight,
    res, combo_result, combo_rows, scenario_values, scenario_keys,
    scenario_id, scenario_key, aesifup_model,
    km_rows, fit, max_fup_days, day_ticks_x,
    k, analysis_row, figure_id, figure_id_safe,
    figure_title, figure_subtitle, figobj, figure_file_name, save_ok,
    fallback_dt
  ))
  # Request garbage collection after each combo.
  gc()
}

# ============================================================================
# SECTION 7: BIND RESULTS & MAP TO TABLE IDS
# ============================================================================

D5_CohortAnalyticLookup <- data.table::rbindlist(
  # Bind all combo outputs (successful and fallback rows).
  result_list,
  use.names = TRUE,
  fill = TRUE
)

logger::log_info(paste0("[Cohort Analytic Outcomes] Bound ", nrow(D5_CohortAnalyticLookup), " result row(s)"))

# Expand design metadata to include both IR and RR table IDs.
outcome_tables <- data.table::rbindlist(
  list(
    # IR table mapping rows.
    outcome_design[, .(
      table_number = ir_table_id,
      title = description,
      analysis_type,
      source_D4 = source_pop,
      estimand,
      source_weight,
      condition,
      weight_col,
      sensitivity_scenario
    )],
    # RR/BW table mapping rows.
    outcome_design[, .(
      table_number = rr_table_id,
      title = description,
      analysis_type,
      source_D4 = source_pop,
      estimand,
      source_weight,
      condition,
      weight_col,
      sensitivity_scenario
    )]
  ),
  use.names = TRUE
)

# Normalize and rebuild combo keys for robust lookup joins.
outcome_tables[is.na(table_number) | !nzchar(table_number), table_number := NA_character_]
outcome_tables <- outcome_tables[!is.na(table_number)]
outcome_tables[is.na(source_weight), source_weight := ""]
outcome_tables[is.na(condition), condition := ""]
outcome_tables[is.na(weight_col), weight_col := ""]
outcome_tables[is.na(sensitivity_scenario), sensitivity_scenario := NA_integer_]
outcome_tables[, estimand := as.integer(estimand)]
outcome_tables[, combo_key := paste(
  source_D4,
  paste0("estimand", estimand),
  source_weight,
  condition,
  weight_col,
  ifelse(is.na(sensitivity_scenario), "scenarioNA", paste0("scenario", sensitivity_scenario)),
  sep = "|"
)]

combo_lookup <- unique(outcome_tables[, .(combo_key, table_number, title, analysis_type)])

D5_CohortAnalyticLookup <- merge(
  # Attach output table ids and labels to each combo result row.
  D5_CohortAnalyticLookup,
  combo_lookup,
  by = "combo_key",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# ============================================================================
# SECTION 8: PREPARE FINAL OUTPUT COLUMNS
# ============================================================================

D5_CohortAnalyticLookup[, tab_id := table_number]
D5_CohortAnalyticLookup[, tab_group := sub("\\..*$", "", table_number)]
D5_CohortAnalyticLookup[, description := title]
D5_CohortAnalyticLookup[, title := NULL]


if (!("aesi" %in% names(D5_CohortAnalyticLookup))) {
  # Guarantee required column for summary table helper.
  D5_CohortAnalyticLookup[, aesi := aesi_name]
}

stat_cols <- setdiff(names(outcome_parameter_lookup), "tab_group")
missing_stat_cols <- setdiff(stat_cols, names(D5_CohortAnalyticLookup))
if (length(missing_stat_cols) > 0L) {
  logger::log_info(paste0("[Cohort Analytic Outcomes] Adding ", length(missing_stat_cols), " missing stat column(s)"))
  # Add absent metadata-specified numeric outputs as NA placeholders.
  D5_CohortAnalyticLookup[, (missing_stat_cols) := NA_real_]
}

# ============================================================================
# SECTION 9: OPTIONAL COUNT MASKING
# ============================================================================

if (apply_masking) {
  logger::log_info(paste0(
    "[Cohort Analytic Outcomes] Applying count masking (threshold = ",
    mask_threshold, ")"
  ))
  # Mask n_pat/n_out counts and suppress person-time when events are masked.
  D5_CohortAnalyticLookup <- apply_count_masking(
    dt = D5_CohortAnalyticLookup,
    threshold = mask_threshold
  )
  logger::log_info("[Cohort Analytic Outcomes] Count masking applied")
} else {
  logger::log_info("[Cohort Analytic Outcomes] Count masking disabled; skipping")
}

# ============================================================================
# SECTION 10: SAVE FULL LOOKUP & EXPORT TABLES
# ============================================================================

# Create a version of summary tables 7 and 11 including vaccine effectivness
# defines as 1 - RR

# Create new columns for VE estimates and confidence intervals based on RR values.
invalid_rows <- (D5_CohortAnalyticLookup$rr_est_crude %in% c(-77, -88, -99) |
  D5_CohortAnalyticLookup$rr_lb_crude %in% c(-77, -88, -99) |
  D5_CohortAnalyticLookup$rr_ub_crude %in% c(-77, -88, -99))
valid_rows <- !invalid_rows
D5_CohortAnalyticLookup[invalid_rows, ve_est_crude := rr_est_crude]
D5_CohortAnalyticLookup[invalid_rows, ve_lb_crude := rr_lb_crude]
D5_CohortAnalyticLookup[invalid_rows, ve_ub_crude := rr_ub_crude]
D5_CohortAnalyticLookup[valid_rows, ve_est_crude := (1 - rr_est_crude) * 100]
D5_CohortAnalyticLookup[valid_rows, ve_lb_crude := (1 - rr_ub_crude) * 100]
D5_CohortAnalyticLookup[valid_rows, ve_ub_crude := (1 - rr_lb_crude) * 100]

# Create new columns for VE estimates and confidence intervals based on adjusted RR values.
invalid_rows <- (D5_CohortAnalyticLookup$rr_est_adj %in% c(-77, -88, -99) |
  D5_CohortAnalyticLookup$rr_lb_adj %in% c(-77, -88, -99) |
  D5_CohortAnalyticLookup$rr_ub_adj %in% c(-77, -88, -99))
valid_rows <- !invalid_rows
D5_CohortAnalyticLookup[invalid_rows, ve_est_adj := rr_est_adj]
D5_CohortAnalyticLookup[invalid_rows, ve_lb_adj := rr_lb_adj]
D5_CohortAnalyticLookup[invalid_rows, ve_ub_adj := rr_ub_adj]
D5_CohortAnalyticLookup[valid_rows, ve_est_adj := (1 - rr_est_adj) * 100]
D5_CohortAnalyticLookup[valid_rows, ve_lb_adj := (1 - rr_ub_adj) * 100]
D5_CohortAnalyticLookup[valid_rows, ve_ub_adj := (1 - rr_lb_adj) * 100]

picard::save(
  # Save full, unfiltered analytic lookup to D5 output file.
  data = D5_CohortAnalyticLookup,
  file_path = output_dir_d5,
  file_name = paste0(this_dap, "_", output_file_name)
)
logger::log_info(paste0("[Cohort Analytic Outcomes] Saved full lookup to ", output_dir_d5))

rounding_cols <- grep("^(ir_|rr_|rd_|cuminc_|py_|ve_)", names(D5_CohortAnalyticLookup), value = TRUE)
# Keep only numeric columns for safe rounding operations.
rounding_cols <- rounding_cols[vapply(D5_CohortAnalyticLookup[, ..rounding_cols], is.numeric, logical(1))]

create_summarystats_table(
  # Metadata-driven export to individual or combined table files.
  metadata = outcome_parameter_lookup,
  varnam_table_group = "tab_group",
  input = D5_CohortAnalyticLookup,
  varnam_description = "description",
  varnam_aesi = "aesi",
  varnam_table_id = "tab_id",
  output_dir = output_dir_d5,
  dap_name = this_dap,
  output_mode = table_export_mode,
  table_order = table_order,
  nsmall = 3,
  rounding_cols = rounding_cols
)

logger::log_info("[Cohort Analytic Outcomes] Summary tables created")

# ============================================================================
# SECTION 11: CLEANUP & FINALIZATION
# ============================================================================

suppressWarnings(rm(
  # Final environment cleanup for pipeline friendliness.
  config_project, config_analytic,
  this_dap,
  dir_d4, dir_metadata,
  cohort_analytic_design_file, cohort_analytic_tables_file,
  fup_files, weight_files,
  output_dir_d5, output_file_name, table_export_mode,
  aesi_name, aesi_label, time_col, scale_ir, default_max_fup_days,
  outcome_design, outcome_parameter_lookup,
  analysis_combos, table_order_lookup, table_order,
  fup_names, all_weight_names,
  needed_fup_names, missing_fup_names,
  needed_weight_names, missing_weight_names,
  result_list, D5_CohortAnalyticLookup,
  outcome_tables, combo_lookup,
  merge_keys, fallback_numeric_cols,
  stat_cols, missing_stat_cols, rounding_cols,
  apply_masking, mask_threshold,
  i, combo, combo_key
))

# Final garbage collection before audit close.
gc()

# Close audit log and write success marker.
picard::audit_end()
logger::log_info("[Cohort Analytic Outcomes] - SUCCESS")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of create_cohort_analytic_outcomes script.")
