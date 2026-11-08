################################################################
# plot_matches_by_date ---
# Aim:
# Create Figure 2 showing matching statistics among exposed individuals by
# calendar week for the Main and Supplementary analyses.
#
# Input files: ---
# - configuration/config_project.yaml
# - data/D3_study_variables/D3_PERSONS.parquet
# - data/D3_study_variables/D3_Eligibility.parquet
# - transformations/T4_statistical_analysis/source_code/functions/fn_plot_exclusion_flowchart.R
#
# Output: ---
# - data/D5_results/<DEAP>_D5_Figure1_ExclusionFlowchart.png
################################################################

# Initial setup ---
## Load necessary libraries ---
library(ggplot2)
library(data.table)

## Script logger ---
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("plot_matches_by_date")
  lm$start_capturing_prints()
}

logger::log_info("Start of the plot_matches_by_date script.")

## Config loading ---
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

## Audit setup ---
picard::audit_start(
  file_name = "create_attrition_tables",
  deap_name = config_project$DEAP_configuration$name
)

## Source functions ---
source("transformations/T4_statistical_analysis/source_code/functions/fn_plot_exclusion_flowchart.R")

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

## Data loading ---
D3_PERSONS <- picard::load(
  file_path = "data/D3_study_variables",
  file_name = "D3_PERSONS.parquet"
)
D3_Eligibility <- picard::load(
  file_path = "data/D3_study_variables",
  file_name = "D3_Eligibility.parquet"
)

## Build flowchart counts ---

# - [x] "individuals in the data instance": uniqueN(D3_PERSONS)
# - [x] "individuals with missing year of birth": D3_PERSONS$missing_year_of_birth
# - [ ] "individuals aged 16 or older during the enrolment period": we have D3_Eligibility$AGE_16_OR_OLDER, but I reckon it already excluded those who don't belong to the study period. Here we need AGE_16_OR_OLDER at enrolment time ("age_16_or_older_at_enrolment")?.
# - [ ] "not registered in the data source during the study period": Do we already have a variable for this? I assume D3_Eligibility contains all registered in the data source at any point during the study period. How to retrieve those that are not? I imagine it's doable with OSBERVATION_PERIODS but it would be nice to have a cleaner variable
# - [ ] "individuals 16 or older registered in the data source during the study period": I assume uniqueN(D3_Eligibility[AGE_16_OR_OLDER == TRUE]$person_id) will cover it

## Note: These are hardcoded for now, compute from data when available

flowchart_counts <- list(
  individuals_in_data_instance = 5e6,
  missing_year_of_birth = 1e4,
  age_16_or_older_during_enrolment = 4.5e6,
  not_registered_study_period = 0.5e5,
  age_16_or_older_registered_study_period = 3.5e6
)


#--------------------------------------------------#
# Plot exclusion flowchart ---
#--------------------------------------------------#

# Plot exclusion flowchart: Main analysis ---
fig1 <- plot_exclusion_flowchart(
  counts = flowchart_counts,
  title = "Figure 1. Flowchart of patient exclusions"
)

# Save to disk
ggplot2::ggsave(
  filename = file.path(output_dir, paste0(deap_prefix, "Figure1_D5_ExclusionFlowchart.png")),
  plot = fig1,
  width = 12,
  height = 6,
  dpi = 300
)

# Clean the environment
objects_to_remove <- c(
  "D3_PERSONS", "D3_Eligibility", "D3_CONCEPTS", "D3_CONCEPTS_PERSON",
  "D3_PERSONS_ENRICHED", "flowchart_counts", "fig1",
  "output_dir", "deap_name", "deap_prefix"
)
rm(list = intersect(objects_to_remove, ls()))
gc()

# Close logs
picard::audit_end()
logger::log_info("SUCCEESS")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of the plot_exclusion_flowchart script.")
