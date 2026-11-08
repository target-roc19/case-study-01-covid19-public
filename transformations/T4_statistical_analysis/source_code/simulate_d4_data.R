# ---------------------------------------------------------------
# Step: simulate_d4_data
#
# Purpose:
#   Create modular synthetic D4 IPCW-ready datasets:
#   - D4_StudyCohort
#   - D4_StudyCohortFup
#   - D4_StudyCohortTimeVaryingCovariates
#   - D4_StudyCohortCovariates
#
# Configuration source:
#   - configuration/config_project.yaml
#   - section: simulate_d4_data
# ---------------------------------------------------------------

if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  # Keep script runnable outside the pipeline logger context.
  lm <- NULL
}
if (!base::is.null(lm)) {
  # Start script-scoped logging and capture printed output.
  lm$start_script("simulate_d4_data.R")
  lm$start_capturing_prints()
}

## Source functions ---
# Load modular D4 simulation builders.
source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "fn_simulate_d4_data.R"
))

# Load configuration utilities (I/O and YAML normalization).
source(file.path(
  "transformations",
  "T4_statistical_analysis",
  "source_code",
  "functions",
  "fn_config_utilities.R"
))

# Central project configuration payload.
config_project <- picard::load_config("configuration/config_project.yaml")
simulate_d4_config <- config_project$simulate_d4_data

## Audit setup ---
picard::audit_start(
  file_name = "simulate_d4_data",
  deap_name = config_project$DEAP_configuration$name
)

# Per-analysis defaults (seed and cohort size) from config.
simulation_map <- lapply(simulate_d4_config$analyses_defaults, function(x) {
  list(
    seed = as.integer(x$seed),
    n_pairs = as.integer(x$n_pairs)
  )
})

# Which analyses to run (e.g., Main, Supplementary).
selected_analyses <- normalize_vec_config(simulate_d4_config$analyses)
invalid_analyses <- setdiff(selected_analyses, names(simulation_map))
if (length(invalid_analyses) > 0) {
  stop(paste0("Invalid analyses value(s): ", paste(invalid_analyses, collapse = ", "), "."))
}

# Which dataset objects to build in this run (cohort, fup, time-varying, covariates).
selected_objects <- tolower(normalize_vec_config(simulate_d4_config$objects))
if ("all" %in% selected_objects) {
  # Convenience alias to run everything.
  selected_objects <- c("cohort", "fup", "tv", "covariates")
}
selected_objects <- unique(selected_objects)
# Accept alternate spelling from config if present.
selected_objects[selected_objects == "timevarying"] <- "tv"
allowed_objects <- c("cohort", "fup", "tv", "covariates")
invalid_objects <- setdiff(selected_objects, allowed_objects)
if (length(invalid_objects) > 0) {
  stop(paste0("Invalid objects value(s): ", paste(invalid_objects, collapse = ", "), "."))
}
if (length(selected_objects) == 0) {
  stop("No valid objects selected. Use objects=cohort,fup,tv.")
}

# Optional global overrides applied on top of per-analysis defaults.
seed_override <- scalar_or_na_int(simulate_d4_config$seed_override)
n_pairs_override <- scalar_or_na_int(simulate_d4_config$n_pairs_override)
max_fup_days <- as.integer(simulate_d4_config$max_fup_days)
start_date <- as.Date(simulate_d4_config$start_date)

# Expected single filenames used when loading existing upstream objects.
cohort_file_override <- simulate_d4_config$cohort_file
fup_file_override <- simulate_d4_config$fup_file

## Output setup ---
# Output directory for all generated D4 tables.
out_dir <- simulate_d4_config$output$out_dir
covariate_metadata_files <- unlist(simulate_d4_config$metadata, use.names = FALSE)


#--------------------------------------------------#
# Simulate D4 datasets
#--------------------------------------------------#

# Main simulation loop: process each analysis independently.
for (i in seq_along(selected_analyses)) {
  # Resolve analysis-specific runtime values.
  analysis_label <- selected_analyses[[i]]
  defaults <- simulation_map[[analysis_label]]

  # If overridden, offset seed by analysis index for deterministic variation.
  analysis_seed <- if (is.na(seed_override)) defaults$seed else as.integer(seed_override + i - 1L)
  # Apply global n_pairs override if supplied.
  analysis_n_pairs <- if (is.na(n_pairs_override)) defaults$n_pairs else as.integer(n_pairs_override)

  logger::log_info(paste(
    "simulate_d4_data",
    "| analysis:", analysis_label,
    "| objects:", paste(selected_objects, collapse = ","),
    "| seed:", analysis_seed,
    "| n_pairs:", analysis_n_pairs
  ))

  # Local placeholders reused across generation steps.
  cohort_dt <- NULL
  fup_dt <- NULL

  # Step 1: Generate or load study cohort with baseline characteristics.
  if ("cohort" %in% selected_objects) {
    # Build cohort from synthetic generation path.
    cohort_dt <- simulate_d4_study_cohort(
      analysis_label = analysis_label,
      seed = analysis_seed,
      n_pairs = analysis_n_pairs,
      start_date = start_date,
      cohort_data = NULL
    )

    # Persist cohort output.
    picard::save(
      data = cohort_dt,
      file_path = out_dir,
      file_name = paste0("D4_StudyCohort", analysis_label, ".parquet")
    )
  }

  # Step 2: Generate or load follow-up summary data.
  if ("fup" %in% selected_objects || "tv" %in% selected_objects) {
    if (is.null(cohort_dt)) {
      # Load existing cohort when this run does not generate cohort directly.
      cohort_file <- gsub("\\{analysis\\}", analysis_label, cohort_file_override)
      cohort_dt <- data.table::as.data.table(picard::load(
        file_path = out_dir,
        file_name = cohort_file
      ))
    }

    if ("fup" %in% selected_objects) {
      # Derive follow-up table from cohort: event dates, censoring, durations.
      fup_dt <- simulate_d4_study_cohort_fup(
        analysis_label = analysis_label,
        seed = analysis_seed,
        n_pairs = analysis_n_pairs,
        max_fup_days = max_fup_days,
        start_date = start_date,
        cohort_data = cohort_dt
      )

      # Persist FUP output.
      picard::save(
        data = fup_dt,
        file_path = out_dir,
        file_name = paste0("D4_StudyCohortFup", analysis_label, ".parquet")
      )
    }
  }

  # Step 3: Generate or load time-varying covariates.
  if ("tv" %in% selected_objects) {
    if (is.null(fup_dt)) {
      # Load existing FUP when this run does not generate FUP directly.
      fup_file <- gsub("\\{analysis\\}", analysis_label, fup_file_override)
      fup_dt <- data.table::as.data.table(picard::load(
        file_path = out_dir,
        file_name = fup_file
      ))
    }

    # Derive time-varying covariates from FUP data.
    tv_dt <- simulate_d4_study_cohort_time_varying_covariates(
      fup_data = fup_dt,
      cohort_data = cohort_dt,
      # Separate stream for TV stochastic generation.
      seed = analysis_seed + 100000L
    )

    # Persist TV output.
    picard::save(
      data = tv_dt,
      file_path = out_dir,
      file_name = paste0("D4_StudyCohortTimeVaryingCovariates", analysis_label, ".parquet")
    )
  }

  # Step 4: Generate baseline covariates used by ASD and Figure 3 diagnostics.
  if ("covariates" %in% selected_objects) {
    if (is.null(cohort_dt)) {
      cohort_file <- gsub("\\{analysis\\}", analysis_label, cohort_file_override)
      cohort_dt <- data.table::as.data.table(picard::load(
        file_path = out_dir,
        file_name = cohort_file
      ))
    }

    covariates_dt <- build_d4_study_cohort_covariates(
      cohort_data = cohort_dt,
      metadata_files = covariate_metadata_files,
      seed = analysis_seed + 200000L
    )

    picard::save(
      data = covariates_dt,
      file_path = out_dir,
      file_name = paste0("D4_StudyCohortCovariates", analysis_label, ".parquet")
    )
  }
}

# Clean the environment
objects_to_remove <- c(
  "cohort_dt", "fup_dt", "tv_dt", "covariates_dt",
  "config_project", "simulate_d4_config", "simulation_map",
  "selected_analyses", "selected_objects", "seed_override",
  "n_pairs_override", "max_fup_days", "start_date", "cohort_file_override",
  "fup_file_override", "covariate_metadata_files"
)

rm(list = intersect(objects_to_remove, ls()))
gc()

# Close logs
picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of the simulate_d4_data script.")
