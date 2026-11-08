################################################################
# create_missingness_table ---
# Aim:
# Create a table of missingness information for the matching variables.
#
# Input files: ---
# - configuration/config_project.yaml
# - data/D3_study_variables/D3_Eligibility.parquet
# - data/D4_analytic_datasets/D4_StudyCohortMain.parquet
# - data/D4_analytic_datasets/D4_StudyCohortSupplementary.parquet
# - transformations/T4_statistical_analysis/source_code/functions/fn_missingness_table.R
#
# Output: ---
# - data/D5_results/<DEAP_name>_Table10_<dataset>.csv (missingness table)
################################################################

# Initial setup ---
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_missingness_table.R")
  lm$start_capturing_prints()
}

# Config loading ---
config_project <- picard::load_config("configuration/config_project.yaml")

dir_d3 <- config_project$outputs$dir_d3
dir_d4 <- config_project$outputs$dir_d4
dir_d5 <- config_project$outputs$dir_d5
matching_vars <- config_project$create_cohort$matching$vars

picard::audit_start(
  file_name = "create_missingness_table",
  deap_name = config_project$DEAP_configuration$name
)

# Load required functions ---
source("transformations/T4_statistical_analysis/source_code/functions/fn_missingness_table.R")

# Load metadata ---
logger::log_info("Loading study variables metadata.")
metadata <- data.table::fread(
  file = "transformations/common_configuration/RWE-BRIDGE/study_variables.csv",
  stringsAsFactors = FALSE
)

# Adjust smoking variable data type in metadata
metadata <- metadata[variable_id %in% matching_vars]
if ("SV_SMOKING" %in% metadata$variable_id) {
  metadata[variable_id == "SV_SMOKING", data_type := "CHAR"]
}

# Load data ---
logger::log_info("Loading D3_Eligibility, D4_StudyCohortMain, and D4_StudyCohortSupplementary data.")
d3_eligibility <- picard::load(
  file_path = dir_d3,
  file_name = "D3_Eligibility.parquet"
)

d4_cohort_main <- picard::load(
  file_path = dir_d4,
  file_name = "D4_StudyCohortMain.parquet"
)

d4_cohort_supplementary <- picard::load(
  file_path = dir_d4,
  file_name = "D4_StudyCohortSupplementary.parquet"
)

# Create missingness table for D3_Eligibility (eligible population) ---
logger::log_info("Creating missingness table for D3_Eligibility (eligible population).")

# Create group column from eligible_exposed and eligible_control
d3_eligibility_with_group <- data.table::copy(d3_eligibility)
d3_eligibility_with_group[, group := NA_character_]
d3_eligibility_with_group[eligible_exposed == TRUE, group := "EXPOSED"]
d3_eligibility_with_group[eligible_control == TRUE, group := "CONTROL"]

# Filter to eligible population only
d3_eligible_only <- d3_eligibility_with_group[eligible_exposed == TRUE | eligible_control == TRUE]

missingness_table_d3_eligible <- create_missingness_table(
  data = d3_eligible_only,
  vars = matching_vars,
  metadata = metadata,
  missing_values = c(NA, "NA", "", NULL),
  group_col = "group",
  exposed_val = "EXPOSED",
  control_val = "CONTROL"
)

# Save the output
picard::save(
  data = missingness_table_d3_eligible,
  file_path = dir_d5,
  file_name = glue::glue("{config_project$DEAP_configuration$name}_Table10_D3Eligible.csv")
)

# Create missingness table for D4_StudyCohortMain (matched population) ---
logger::log_info("Creating missingness table for D4_StudyCohortMain (matched population).")

missingness_table_d4_main <- create_missingness_table(
  data = d4_cohort_main,
  vars = matching_vars,
  metadata = metadata,
  missing_values = c(NA, "NA", "", NULL),
  group_col = "group",
  exposed_val = "EXPOSED",
  control_val = "CONTROL"
)

# Save the output
picard::save(
  data = missingness_table_d4_main,
  file_path = dir_d5,
  file_name = glue::glue("{config_project$DEAP_configuration$name}_Table10_D4Main.csv")
)

# Create missingness table for D4_StudyCohortSupplementary (supplementary population) ---
logger::log_info("Creating missingness table for D4_StudyCohortSupplementary (supplementary population).")

missingness_table_d4_supplementary <- create_missingness_table(
  data = d4_cohort_supplementary,
  vars = matching_vars,
  metadata = metadata,
  missing_values = c(NA, "NA", "", NULL),
  group_col = "group",
  exposed_val = "EXPOSED",
  control_val = "CONTROL"
)

# Save the output
picard::save(
  data = missingness_table_d4_supplementary,
  file_path = dir_d5,
  file_name = glue::glue("{config_project$DEAP_configuration$name}_Table10_D4Supplementary.csv")
)

# Clean up
rm(
  d3_eligibility, d3_eligibility_with_group, d3_eligible_only,
  d4_cohort_main, d4_cohort_supplementary,
  missingness_table_d3_eligible, missingness_table_d4_main, missingness_table_d4_supplementary,
  metadata
)
gc()

picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
