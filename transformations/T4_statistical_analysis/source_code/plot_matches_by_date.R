################################################################
# plot_matches_by_date ---
# Aim:
# Create Figure 2 showing matching statistics among exposed individuals by
# calendar week for the Main and Supplementary analyses.
#
# Input files: ---
# - configuration/config_project.yaml
# - data/D3_study_variables/D3_Eligibility.parquet
# - data/D4_analytic_datasets/D4_StudyCohortMain.parquet
# - data/D4_analytic_datasets/D4_StudyCohortSupplementary.parquet
# - transformations/T4_statistical_analysis/source_code/functions/fn_plot_matches_by_date.R
#
# Output: ---
# - data/D5_results/<DEAP>_D5_Figure2a_MatchesByDate_Main.png
# - data/D5_results/<DEAP>_D5_Figure2b_MatchesByDate_Supplementary.png
################################################################

# Initial setup ---
## Load necessary libraries ---
library(ggplot2)

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

logger::log_info("[Plot Matches by Date] - START")
logger::log_info("Start of the plot_matches_by_date script.")

## Config loading ---
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")

## Audit setup ---
picard::audit_start(
  file_name = "plot_matches_by_date",
  deap_name = config_project$DEAP_configuration$name
)

## Source functions ---
source("transformations/T4_statistical_analysis/source_code/functions/fn_plot_matches_by_date.R")

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
D3_Eligibility <- picard::load(
  file_path = "data/D3_study_variables",
  file_name = "D3_Eligibility.parquet"
)
D4_StudyCohortMain <- picard::load(
  file_path = "data/D4_analytic_datasets",
  file_name = "D4_StudyCohortMain.parquet"
)
D4_StudyCohortSupplementary <- picard::load(
  file_path = "data/D4_analytic_datasets",
  file_name = "D4_StudyCohortSupplementary.parquet"
)

## Define EXCLUDED cases
excluded_cases <- D3_Eligibility[RECEIVES_EXPOSURE == TRUE & PRIOR_PFIZER_VAC == FALSE & PREV_OTHER_COVID_VAC == FALSE & eligible_exposed == FALSE][, group := "EXCLUDED"][, .(person_id, start_episode, group)]
data.table::setnames(excluded_cases, "start_episode", "T0")

## Define EXPOSED and UNMATCHED cases directly from D4 cohorts
cases_main <- D4_StudyCohortMain[
  group %in% c("EXPOSED", "UNMATCHED"),
  .(person_id, T0, group)
]

cases_supplementary <- D4_StudyCohortSupplementary[
  group %in% c("EXPOSED", "UNMATCHED"),
  .(person_id, T0, group)
]

## Combine EXCLUDED, UNMATCHED, and EXPOSED cases for Main analysis
target_data_main <- data.table::rbindlist(
  list(excluded_cases, cases_main),
  use.names = TRUE,
  fill = TRUE
)
target_data_main[, group := factor(group,
  levels = c("EXCLUDED", "UNMATCHED", "EXPOSED"),
  labels = c("Not eligible", "Unmatched", "Matched")
)]

## Combine EXCLUDED, UNMATCHED, and EXPOSED cases for Supplementary analysis
target_data_supplementary <- data.table::rbindlist(
  list(excluded_cases, cases_supplementary),
  use.names = TRUE,
  fill = TRUE
)
target_data_supplementary[, group := factor(group,
  levels = c("EXCLUDED", "UNMATCHED", "EXPOSED"),
  labels = c("Not eligible", "Unmatched", "Matched")
)]

# Set T0 to Date class
target_data_main[, T0 := as.Date(T0)]
target_data_supplementary[, T0 := as.Date(T0)]

#--------------------------------------------------#
# Plot matches by calendar date
#--------------------------------------------------#

# Plot matches by calendar date: Main analysis ---

fig2a <- plot_counts_by_group_time(
  df = target_data_main,
  group_var = "group",
  binning_var = "T0",
  binning_unit = "week",
  start_date = config_values$start_study_date,
  end_date = config_values$end_study_date,
  breaks_interval = 3,
  deap = deap_prefix,
  cap_at_last_obs = FALSE,
  title = "Figure 2a. Matching statistics among exposed episodes by calendar week for the main analysis",
  observation_unit = "matching episodes"
)

# Save to disk
ggplot2::ggsave(
  filename = file.path(output_dir, paste0(deap_prefix, "Figure2a_MatchesByDate_Main.png")),
  plot = fig2a,
  width = 10,
  height = 6,
  dpi = 300
)

# Plot matches by calendar date: Supplementary analysis ---

fig2b <- plot_counts_by_group_time(
  df = target_data_supplementary,
  group_var = "group",
  binning_var = "T0",
  binning_unit = "week",
  start_date = config_values$start_study_date,
  end_date = config_values$end_study_date,
  breaks_interval = 3,
  deap = deap_prefix,
  cap_at_last_obs = FALSE,
  title = "Figure 2b. Matching statistics among exposed episodes by calendar week for the supplementary analysis",
  observation_unit = "matching episodes"
)

# Save to disk
ggplot2::ggsave(
  filename = file.path(output_dir, paste0(deap_prefix, "Figure2b_MatchesByDate_Supplementary.png")),
  plot = fig2b,
  width = 10,
  height = 6,
  dpi = 300
)

#--------------------------------------------------#
# Create unique individual versions of the plots
#--------------------------------------------------#

## Prepare unique individual data for Main analysis
target_data_main_unique <- target_data_main[, .SD[1], by = .(person_id, group)]

## Prepare unique individual data for Supplementary analysis
target_data_supplementary_unique <- target_data_supplementary[, .SD[1], by = .(person_id, group)]

## Plot unique individuals: Main analysis ---

fig2c <- plot_counts_by_group_time(
  df = target_data_main_unique,
  group_var = "group",
  binning_var = "T0",
  binning_unit = "week",
  start_date = config_values$start_study_date,
  end_date = config_values$end_study_date,
  breaks_interval = 3,
  deap = deap_prefix,
  cap_at_last_obs = FALSE,
  title = "Figure 2c. Matching statistics among unique exposed individuals by calendar week for the main analysis",
  observation_unit = "individuals"
)

# Save to disk
ggplot2::ggsave(
  filename = file.path(output_dir, paste0(deap_prefix, "Figure2c_MatchesByDate_Main_UniqueIndividuals.png")),
  plot = fig2c,
  width = 10,
  height = 6,
  dpi = 300
)

## Plot unique individuals: Supplementary analysis ---

fig2d <- plot_counts_by_group_time(
  df = target_data_supplementary_unique,
  group_var = "group",
  binning_var = "T0",
  binning_unit = "week",
  start_date = config_values$start_study_date,
  end_date = config_values$end_study_date,
  breaks_interval = 3,
  deap = deap_prefix,
  cap_at_last_obs = FALSE,
  title = "Figure 2d. Matching statistics among unique exposed individuals by calendar week for the supplementary analysis",
  observation_unit = "individuals"
)

# Save to disk
ggplot2::ggsave(
  filename = file.path(output_dir, paste0(deap_prefix, "Figure2d_MatchesByDate_Supplementary_UniqueIndividuals.png")),
  plot = fig2d,
  width = 10,
  height = 6,
  dpi = 300
)

# Clean the environment
objects_to_remove <- c(
  "D3_Eligibility", "D4_StudyCohortMain", "D4_StudyCohortSupplementary",
  "output_dir", "deap_name", "deap_prefix"
)
rm(list = intersect(objects_to_remove, ls()))
gc()

# Close logs
picard::audit_end()
logger::log_info("[Plot Matches by Date] - SUCCEESS")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}

logger::log_info("End of the plot_matches_by_date script.")
