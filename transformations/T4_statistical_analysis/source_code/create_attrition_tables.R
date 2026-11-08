################################################################
# create_attrition_tables ---
# Aim:
# Create Table 1(a), 1(b), and 1(c) attrition tables using D3 eligibility and
# D4 matched cohort outputs.
#
# Input files: ---
# - configuration/config_project.yaml
# - data/D3_study_variables/D3_Eligibility.parquet
# - data/D4_analytic_datasets/D4_StudyCohortMain.parquet
# - data/D4_analytic_datasets/D4_StudyCohortSupplementary.parquet
# - transformations/T4_statistical_analysis/source_code/functions/fn_attrition_tables.R
#
# Output: ---
# - data/D5_results/D5_<DEAP>_Attrition_Table1a.csv
# - data/D5_results/D5_<DEAP>_Attrition_Table1b.csv
# - data/D5_results/D5_<DEAP>_Attrition_Table1c.csv
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
  lm$start_script("create_attrition_tables")
  lm$start_capturing_prints()
}

logger::log_info("[Attrition Tables] - START")
logger::log_info("Start of the create_attrition_tables script.")

## Config loading ---
config_project <- picard::load_config("configuration/config_project.yaml")

## Audit setup ---
picard::audit_start(
  file_name = "create_attrition_tables",
  deap_name = config_project$DEAP_configuration$name
)

## Data loading ---
D3_Eligibility <- picard::load(
  file_path = "data/D3_study_variables",
  file_name = "D3_Eligibility.parquet"
)

# If not available, skip loading D4 matched cohort data and log a warning
if (!file.exists("data/D4_analytic_datasets/D4_StudyCohortMain.parquet")) {
  logger::log_warn("D4_StudyCohortMain matched cohort data not found. Skipping Table 1(b) attrition table creation.")
  D4_StudyCohortMain <- NULL
} else {
  D4_StudyCohortMain <- picard::load(
    file_path = "data/D4_analytic_datasets",
    file_name = "D4_StudyCohortMain.parquet"
  )
}
if (!file.exists("data/D4_analytic_datasets/D4_StudyCohortSupplementary.parquet")) {
  logger::log_warn("D4_StudyCohortSupplementary matched cohort data not found. Skipping Table 1(c) attrition table creation.")
  D4_StudyCohortSupplementary <- NULL
} else {
  D4_StudyCohortSupplementary <- picard::load(
    file_path = "data/D4_analytic_datasets",
    file_name = "D4_StudyCohortSupplementary.parquet"
  )
}

## Source functions ---
source("transformations/T4_statistical_analysis/source_code/functions/fn_attrition_tables.R")

## Output setup ---
output_dir <- config_project$outputs$dir_d5
if (is.null(output_dir) || !nzchar(output_dir)) {
  output_dir <- "data/D5_results"
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

deap_name <- config_project$DEAP_configuration$name
deap_prefix <- ""
if (!is.null(deap_name) && nzchar(trimws(deap_name))) {
  deap_prefix <- paste0(trimws(deap_name), "_")
}


#--------------------------------------------------#
# Create attrition tables
#--------------------------------------------------#

## Table 1.a: Attrition table for entry into the eligible population
table_1a <- create_attrition_table_1a(
  d3_eligibility = D3_Eligibility
)
output_table_1a <- file.path(output_dir, paste0(deap_name, "_Table1a.csv"))
data.table::fwrite(table_1a, output_table_1a, na = "")
logger::log_info(paste("Saved Table 1(a) attrition table to:", output_table_1a))


## Table 1.b: Matching statistics for the matched cohort main analysis (matching without replacement)
if (!is.null(D4_StudyCohortMain)) {
  table_1b <- create_attrition_table_1b(
    d3_eligibility = D3_Eligibility,
    d4_main = D4_StudyCohortMain
  )
  output_table_1b <- file.path(output_dir, paste0(deap_name, "_Table1b.csv"))
  data.table::fwrite(table_1b, output_table_1b, na = "")
  logger::log_info(paste("Saved Table 1(b) attrition table to:", output_table_1b))
}


## Table 1.c: Matching statistics for the matched cohort supplementary analysis (matching with replacement)
if (!is.null(D4_StudyCohortSupplementary)) {
  table_1c <- create_attrition_table_1c(
    d4_supplementary = D4_StudyCohortSupplementary
  )
  output_table_1c <- file.path(output_dir, paste0(deap_name, "_Table1c.csv"))
  data.table::fwrite(table_1c, output_table_1c, na = "")
  logger::log_info(paste("Saved Table 1(c) attrition table to:", output_table_1c))
}


# Clean the environment
objects_to_remove <- c(
  "D3_Eligibility", "D4_StudyCohortMain", "D4_StudyCohortSupplementary",
  "table_1a", "table_1b", "table_1c",
  "output_dir", "deap_name", "deap_prefix",
  "output_table_1a", "output_table_1b", "output_table_1c"
)
rm(list = intersect(objects_to_remove, ls()))
gc()

# Close logs
picard::audit_end()
logger::log_info("[Attrition Tables] - SUCCEESS")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of the create_attrition_tables script.")
