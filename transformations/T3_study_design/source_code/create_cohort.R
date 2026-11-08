################################################################
# create_cohort ---
# Aim:
# Create the D4 study cohort by running the matching pipeline on D3 eligibility
# data, producing main and supplementary cohort outputs for downstream analysis.
#
# Input files: ---
# - configuration/config_project.yaml
# - data/D3_study_variables/D3_Eligibility.parquet
# - transformations/T3_study_design/source_code/functions/simulate_d3_eligibility.R
#
# Output: ---
# - data/D4_analytic_datasets/D4_StudyCohort_Main.parquet
# - data/D4_analytic_datasets/D4_StudyCohort_Supplementary.parquet
# - matching log files and optional intermediate matching outputs
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
  lm$start_script("create_cohort")
  lm$start_capturing_prints()
}

logger::log_info("[Cohort creation] - START")
logger::log_info("Start of the create_cohort script.")

## Config loading ---
# Centralize all create_cohort runtime settings in the project configuration.
config_T3 <- picard::load_config("configuration/config_T3.yaml")
config_project <- picard::load_config("configuration/config_project.yaml")
create_cohort_config <- config_project$create_cohort

## Audit setup ---
picard::audit_start(
  file_name = "create_cohort",
  deap_name = config_project$DEAP_configuration$name
)

## Source functions ---
# Load the local helper used when synthetic D3 input is requested.
source(file.path(config_T3$T3$root, config_T3$T3$functions, "simulate_d3_eligibility.R"))

# Extract variables used in data loading and processing logic
matching_modes <- unlist(create_cohort_config$matching$modes, use.names = FALSE)

## Load or generate synthetic D3 eligibility data ---
# Simulation settings
simulate_d3_eligibility_now <- create_cohort_config$simulation$simulate_d3_eligibility_now

logger::log_info("Creating synthetic input data if configured.")
if (simulate_d3_eligibility_now) {
  logger::log_info("Simulating D3 eligibility data.")
  D3_Eligibility <- simulate_d3_eligibility(
    n = create_cohort_config$simulation$n_simulated_spells,
    start_seed = create_cohort_config$matching$start_seed,
    save_output = create_cohort_config$simulation$save_simulated_d3_eligibility,
    output_dir = create_cohort_config$simulation$output_dir,
    output_file = create_cohort_config$simulation$output_file_name
  )
} else {
  # Use the prepared D3 eligibility extract when simulation is disabled.
  logger::log_info("Loading D3 eligibility data from parquet file.")
  D3_Eligibility <- picard::load(
    file_path = create_cohort_config$input$dir_d3,
    file_name = create_cohort_config$input$input_file_name
  )
}

## Run the matching pipeline for each specified matching mode ---
logger::log_info("Running the matching pipeline.")

for (matching_mode in matching_modes) {
  logger::log_info(paste0("Running the matching pipeline with matching mode: ", matching_mode))

  # Keep filenames aligned with the main and supplementary cohort convention.
  mode_is_without_replacement <- matching_mode == "without_replacement"
  log_prefix <- ifelse(
    mode_is_without_replacement,
    "matching_log_main",
    "matching_log_supp"
  )

  # Suffixes are added to output files to distinguish between main and supplementary cohort outputs when both matching modes are run.
  mode_suffix <- ifelse(mode_is_without_replacement, "Main", "Supplementary")

  # Output files are suffixed to distinguish between main and supplementary cohort outputs when both matching modes are run.
  log_file_name <- paste0(log_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  output_file_name <- paste0(create_cohort_config$output$output_file_name, mode_suffix)

  # Intermediate outputs are also suffixed to distinguish between matching modes when both are run.
  profile_table_name <- paste0(create_cohort_config$intermediate_output$profile_table_name, mode_suffix)
  matching_pop_name <- paste0(create_cohort_config$intermediate_output$matching_pop_name, mode_suffix)

  library(logr)
  # Run one cohort build per matching mode using the shared configuration payload.
  CohortBuilder::build_study_cohort(
    eligible_pop = D3_Eligibility,
    matching_vars = unlist(create_cohort_config$matching$vars, use.names = FALSE),
    dir_matching_db = create_cohort_config$matching$dir_matching_db,
    exposed_batch_size = create_cohort_config$matching$exposed_batch_size,
    control_batch_size = create_cohort_config$matching$control_batch_size,
    n_cores = create_cohort_config$matching$n_cores,
    log_name = log_file_name,
    output_pars = list(
      save_output = create_cohort_config$output$save_output,
      dir_output = create_cohort_config$output$dir_d4,
      output_file_name = output_file_name
    ),
    intermediate_output_pars = list(
      save_intermediate_outputs = create_cohort_config$intermediate_output$save_intermediate_outputs,
      dir_intermediate_outputs = create_cohort_config$intermediate_output$dir_intermediate_outputs,
      profile_table_name = profile_table_name,
      matching_pop_name = matching_pop_name
    ),
    bootstrap_pars = list(
      with_bootstrap = create_cohort_config$bootstrap$with_bootstrap,
      n_bootstraps = create_cohort_config$bootstrap$n_bootstraps,
      dir_bootstrap = create_cohort_config$bootstrap$dir_bootstrap,
      start_seed = create_cohort_config$matching$start_seed
    ),
    input_column_names = list(
      col_person_id = "person_id",
      col_eligible_exposed = "eligible_exposed",
      col_eligible_control = "eligible_control",
      col_matching_status_start = "start_episode",
      col_matching_status_end = "end_episode",
      col_age_iterator = "year_of_birth"
    ),
    output_column_names = list(
      col_person_id = "person_id",
      col_match_id = "match_id",
      col_treatment_group = "group",
      col_T0 = "T0"
    ),
    matching_mode = matching_mode,
    age_offset = create_cohort_config$matching$age_offset
  )
}

# Clean the environment
rm(
  D3_Eligibility, output_file_name, profile_table_name, matching_pop_name,
  log_file_name, mode_suffix, log_prefix, mode_is_without_replacement,
  matching_mode, matching_modes
)
gc()

# Close logs
picard::audit_end()
logger::log_info("[Cohort creation] - SUCCEED")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of the create_cohort script.")
