################################################################
# create_follow_up_table ---
# Aim:
# Create follow-up, intercurrent events and censoring reasons table
# (Tables 3.a–d) for the main and supplementary matched cohorts across
# Estimand 1 (unweighted) and Estimand 2 (IPSMW).
#
# Input files: ---
# - configuration/config_project.yaml
# - data/D4_analytic_datasets/D4_StudyCohortFup{Main,Supplementary}.parquet
# Output: ---
# - data/D5_results/{deap_name}_Table3{a-d}_{cohort}_{estimand}.csv (fup table)
################################################################

# ----------------------------
# SET UP ENVIRONMENT
# ----------------------------
if (!base::exists("lm", inherits = TRUE) ||
  !base::inherits(lm, "LoggerManager")) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_fup_table.R")
  lm$start_capturing_prints()
}

picard::load_config()

logger::log_info("Starting: Table 3 follow-up, IE and censoring reasons")

# ----------------------------
# LOAD CONFIG
# ----------------------------
logger::log_info("Reading config for follow-up tables")

config_project <- picard::load_config("configuration/config_project.yaml")
deap_name <- config_project$DEAP_configuration$name
fup_config <- config_project$create_anchored_datasets

# Extract config parameters
dir_d4 <- config_project$output$dir_d4
dir_d5 <- config_project$output$dir_d5

# load the functions needed
source("transformations/T4_statistical_analysis/source_code/functions/fn_create_fup_table.R")

# ----------------------------
# TABLE INFORMATION
# ----------------------------
# Scenario definitions for requested outputs:
# a. Main cohort, Estimand 1
# b. Main cohort, Estimand 2
# c. Supplementary cohort, Estimand 1
# d. Supplementary cohort, Estimand 2
table_scenarios <- list(
  list(letter = "a", cohort = "Main", estimand = 1),
  list(letter = "b", cohort = "Main", estimand = 2),
  list(letter = "c", cohort = "Supplementary", estimand = 1),
  list(letter = "d", cohort = "Supplementary", estimand = 2)
)

# intercurrent event variables
ie_vars <- c(
  "ie1_missed_second_dose_target_exposed",
  "ie2_third_dose_or_booster_exposed",
  # "ie3_first_dose_covid_nontarget_exposed",
  # "ie3_first_dose_covid_any_control",
  "ie4_noncovid_any"
  # "ie5_prep_treatment",
  # "death_date"
)

# intercurrent event labels
ie_labels <- c(
  "IE1. Missing second dose of target vaccine (Exposed)",
  "IE2. Third or booster dose of target vaccine (Exposed)",
  # "IE3. Receipt of any non-target COVID-19 vaccine at any time (Exposed)",
  # "IE3. First dose of any COVID-19 vaccine at any time (Control)",
  "IE4. Receipt of any non-COVID-19 vaccine dose"
  # "IE5. Receipt of any other preventative COVID-19 treatment",
  # "IE6. Death"
)

# censoring variables
censor_vars <- c(
  "covid_date",
  "death_date",
  "loss_to_follow_up",
  "max_fup_date",
  "end_study_date",
  # "end_data_collection",
  "ie3_first_dose_covid_nontarget_exposed",
  "ie3_first_dose_covid_any_control",
  "ie5_prep_treatment"
)

# censoring labels
censor_labels <- c(
  "SARS-CoV-2 infection",
  "IE6. Death",
  "Loss to follow-up (i.e., de-registration from the data source)",
  "Administrative end of follow-up at day 90",
  "End of data availability in the data source",
  "IE3. Receipt of any non-target COVID-19 vaccine at any time (Exposed)",
  "IE3. First dose of any COVID-19 vaccine at any time (Control)",
  "IE5. Receipt of any other preventative COVID-19 treatment "
)

# Loop over all cohort/estimand combinations and save each table
for (scenario in table_scenarios) {
  cohort_label <- scenario$cohort
  estimand_n <- scenario$estimand
  estimand_label <- paste0("Estimand ", estimand_n)

  logger::log_info(paste0(
    "Creating follow-up table ", scenario$letter,
    " (", cohort_label, ", ", estimand_label, ")"
  ))

  file_path <- file.path(dir_d4, paste0("D4_StudyCohortFup", cohort_label, ".parquet"))
  d4_fup <- picard::load(file_path, sep = ",")

  logger::log_info(paste("Rows loaded:", nrow(d4_fup)))

  # Estimand 2 population: compliant exposed + all controls
  if (estimand_n == 2) {
    d4_fup <- d4_fup[d4_fup$group == "CONTROL" |
      (d4_fup$group == "EXPOSED" & d4_fup$compliant_exposed == TRUE), ]
    logger::log_info(paste("Rows after Estimand 2 filter:", nrow(d4_fup)))
  }

  # Add IE second dose target exposed missed
  d4_fup[, ie1_missed_second_dose_target_exposed := as.Date(data.table::fifelse(
    group == "EXPOSED" & is.na(ie1_second_dose_target_exposed),
    T0 + 90, NA
  ))]

  # Add fup_length_months_est1 and fup_length_months_est2
  d4_fup[, fup_length_months_est1 := fup_length_days_est1 / 30.44]
  d4_fup[, fup_length_months_est2 := fup_length_days_est2 / 30.44]

  # Output the median, q1, q3 time to second dose vaccination for the exposed
  # calculated as difftime between T0 and ie1_second_dose_target_exposed
  median_time_to_second_dose <- median(d4_fup[group == "EXPOSED"]$ie1_second_dose_target_exposed - d4_fup[group == "EXPOSED"]$T0, na.rm = TRUE)
  q1_time_to_second_dose <- quantile(d4_fup[group == "EXPOSED"]$ie1_second_dose_target_exposed - d4_fup[group == "EXPOSED"]$T0, probs = 0.25, na.rm = TRUE)
  q3_time_to_second_dose <- quantile(d4_fup[group == "EXPOSED"]$ie1_second_dose_target_exposed - d4_fup[group == "EXPOSED"]$T0, probs = 0.75, na.rm = TRUE)
  logger::log_info(paste0(
    "Median time to second dose vaccination in exposed (days): ", median_time_to_second_dose,
    " (Q1: ", q1_time_to_second_dose, ", Q3: ", q3_time_to_second_dose, ")"
  ))

  # For each of the ie_vars, set their value to NA if they occur after the
  # follow-up end date (fup_end_date_est1 or fup_end_date_est2)
  for (ie_var in ie_vars) {
    fup_end_date_var <- "end_fup"
    d4_fup[get(ie_var) > get(fup_end_date_var), (ie_var) := NA]
  }

  # Exposed
  fup_table_exp <- fup_ie_cens_tab(d4_fup[d4_fup$group == "EXPOSED", ],
    fup_time = paste0("fup_length_months_est", estimand_n),
    reas_cens = paste0("reason_censoring_est", estimand_n),
    header = estimand_label,
    ie_vars = ie_vars,
    ie_labels = ie_labels,
    eof_vars = list(),
    eof_labels = c(),
    cens_vars = censor_vars,
    cens_labels = censor_labels,
    row_structure = c("header", "fup", "ie", "cens"),
    section_labels = list(
      fup = "Person-months of follow-up",
      ie = "Occurrence of Intercurrent Events during follow-up, n (%)",
      cens = "Intercurrent events and reasons for end of follow-up*"
    ),
    ie_fup = TRUE,
    cens_fup = TRUE
  )

  # Control
  fup_table_con <- fup_ie_cens_tab(d4_fup[d4_fup$group == "CONTROL", ],
    fup_time = paste0("fup_length_months_est", estimand_n),
    reas_cens = paste0("reason_censoring_est", estimand_n),
    header = estimand_label,
    ie_vars = ie_vars,
    ie_labels = ie_labels,
    eof_vars = list(),
    eof_labels = c(),
    cens_vars = censor_vars,
    cens_labels = censor_labels,
    row_structure = c("header", "fup", "ie", "cens"),
    section_labels = list(
      fup = "Person-months of follow-up",
      ie = "Occurrence of Intercurrent Events during follow-up, n (%)",
      cens = "Intercurrent events and reasons for end of follow-up*"
    ),
    ie_fup = TRUE,
    cens_fup = TRUE
  )

  # Bind together
  fup_table <- cbind(fup_table_exp, fup_table_con[, -1])
  names(fup_table) <- c("label", "exposed_col1", "exposed_col2", "control_col1", "control_col2")

  # Set cells to NA if they don't apply for the group
  fup_table[c(34:36), 2:3] <- "N.A."
  fup_table[c(6:11, 31:33), 4:5] <- "N.A."

  # Save output with short informative label in filename
  output_name <- paste0(
    deap_name,
    "_Table3",
    scenario$letter,
    "_",
    cohort_label,
    "_Estimand",
    estimand_n,
    ".csv"
  )

  picard::save(
    data = fup_table,
    file_path = dir_d5,
    file_name = output_name
  )

  rm(d4_fup, fup_table_exp, fup_table_con, fup_table)
  gc()
}

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
logger::log_info("Completed: follow-up tables for all cohort/estimand scenarios")
