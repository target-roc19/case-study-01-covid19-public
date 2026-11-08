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
  lm$start_script("create_anchored_hive")
  lm$start_capturing_prints()
}

## Config loading ----
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_anchored_datasets <- config_project$create_anchored_datasets

# Set path to concepts database
concepts_hive_dir <- create_anchored_datasets$input$concepts_dir

# Output path for anchored variables in the Hive format
anchor_hive_dir <- create_anchored_datasets$output$anchoring_dir

## Audit setup ---
picard::audit_start(
  file_name = "create_anchored_hive",
  deap_name = config_project$DEAP_configuration$name
)

logger::log_info("Starting: dataset anchoring")

# ----------------------------
# LOAD METADATA
# ----------------------------

# load anchoring window metadata which defines the variables to be anchored,
# their data type, and the time windows for anchoring
anchoring_window_metadata <- picard::load(
  file_path = create_anchored_datasets$input$metadata_dir,
  file_name = create_anchored_datasets$input$metadata_file
)

# ----------------------------
# LOAD DATA
# ----------------------------

analyses <- c("Main", "Supplementary")

# Load all cohorts and collect unique person_id + T0 across analyses.
# Anchoring is run once on this combined population to avoid redundant work
# for individuals present in both analyses.
cohorts <- lapply(analyses, function(analysis) {
  logger::log_info(paste0("Loading study cohort for analysis: ", analysis))
  dt <- data.table::as.data.table(picard::load(
    file_path = create_anchored_datasets$input$cohort_dir,
    file_name = paste0(create_anchored_datasets$input$cohort_file, analysis, ".parquet")
  ))
})
names(cohorts) <- analyses

combined_population <- unique(
  data.table::rbindlist(lapply(cohorts, function(dt) dt[, .(person_id, T0)]), use.names = TRUE),
  by = c("person_id", "T0")
)
logger::log_info(paste0(
  "Combined unique person_id + T0 across all analyses: ",
  nrow(combined_population)
))

# ----------------------------
# ANCHOR ALL VARIABLES (once, on combined population)
# ----------------------------

anchoR::anchor_by_variable(
  population = combined_population,
  metadata = anchoring_window_metadata,
  concepts = concepts_hive_dir,
  anchor_col = "T0",
  anchor_hive_path = anchor_hive_dir
)

logger::log_info("Dataset anchoring completed")
picard::audit_end()

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
