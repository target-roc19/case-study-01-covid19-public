## Create Spells
# Merging observation periods that are overlapping or close (180 days)
# to each other into one observation periods (aka SPELL from this point onwards)

## Aim
# to process in this script: process_observation_periods

## input
# Input 1: d2db:OBSERVATION_PERIODS
# - person_id
# - op_start_date
# - op_end_date
# Input 2: D3_PERSONS.parquet
# - person_id
# - year_of_birth
# - sex_at_intance_creation
# - birth_date
# - death_date
# Input 3: configurations/config_values.yaml
# - end_study_date

## output
# Output 1: D3_SPELLS
# - person_id
# - start_spell
# - end_spell
# - year_of_birth
# - sex_at_instance_creation
# - birth_date
# - death_date
# - with_start_and_end
# - start_before_end

# Initial setup ----
## Script logger ----
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("Processing D3 SPELLS")
  # Start capturing print statements
  lm$start_capturing_prints()
}


logger::log_info("[Create Spells] - START")
### Load configurations ----
picard::load_config()

picard::audit_start(
  file_name = "d3_spells",
  deap_name = config_project$DEAP_configuration$name
)

### Directories ----
logger::log_info("Loading directories...")
root <- config_t2$T2$root
intermediate <- config_t2$T2$intermediate
dir_intermediate_t2 <- file.path(root, intermediate)
data_model <- config_project$DEAP_configuration$data_model
d3_person_file <- config_project$process_observation_periods$D3_person_file
observation_periods_file <- config_project$process_observation_periods$observation_periods_file

dir_d2_db <- config_pipeline$databases$dir_d2_db

## Load input data ----
###  Persons ----
logger::log_info("Loading d3_person_file...")
d3_persons <- picard::load(d3_person_file)

if (data_model == "ConcePTION") {
  ### Observation periods ----
  logger::log_info("Loading observation_periods...")
  d2_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_d2_db)
  observation_periods <- DBI::dbGetQuery(
    d2_db_conn,
    "SELECT * FROM main.OBSERVATION_PERIODS"
  ) |>
    data.table::as.data.table()
  duckdb::dbDisconnect(d2_db_conn, shutdown = TRUE)
} else if (data_model == "OMOP") {
  ### Observation periods ----
  logger::log_info("Loading observation_periods...")
  d2_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_d2_db)
  observation_periods <- DBI::dbGetQuery(
    d2_db_conn,
    "SELECT * FROM main.observation_period"
  ) |>
    data.table::as.data.table()

  # Renaming columns to match the expected names in ConcePTION pipeline
  data.table::setnames(
    observation_periods,
    "observation_period_start_date",
    "op_start_date"
  )
  data.table::setnames(
    observation_periods,
    "observation_period_end_date",
    "op_end_date"
  )

  duckdb::dbDisconnect(d2_db_conn, shutdown = TRUE)
}

### Addtional variables ----
#### Settings ----
logger::log_info("Getting settings...")
end_study_date <- picard::get_date_value(
  config_values$end_study_date
)

start_study_date <- picard::get_date_value(
  config_values$start_study_date
)

recommended_end_date <- picard::get_date_value(
  config_values$recommended_end_date
)

date_creation <- picard::get_date_value(config_values$date_creation)

# Filter out non-date values and compute the minimum
valid_dates <- c(
  recommended_end_date,
  date_creation,
  end_study_date
)

valid_dates[is.infinite(valid_dates)] <- ""

end_date <- base::as.Date(base::min(valid_dates[!base::is.na(valid_dates)]))

picard::audit_add(
  "date selected as end_date: ",
  end_date
)

# Step 1 ----
logger::log_info("Started processing spells...")
picard::audit_add(
  "\nnumber of rows from observation_periods: ",
  base::nrow(observation_periods),
  "\n number of individual persons in observation periods: ",
  base::length(base::unique(observation_periods$person_id))
)

# If missing `OBSERVATION_PERIODS:op_end_date ` AND  `OBSERVATION_PERIODS:op_start_date ` < `end_study_period`, impute `OBSERVATION_PERIODS:op_end_date ` with `config_values:end_study_date`
logger::log_info("Step 1: Imputing observation periods with end_date")

picard::audit_add(
  "\nnumber of rows from observation_periods to be imputed: ",
  base::nrow(observation_periods[
    op_start_date < end_study_date &
      (is.na(op_end_date) |
        is.null(op_end_date))
  ])
)

observation_periods[
  ,
  op_end_date := data.table::fifelse(
    op_start_date < end_study_date &
      (is.na(op_end_date) |
        is.null(op_end_date)),
    end_study_date,
    op_end_date
  )
]

logger::log_info("Step 1...Done")
# Step 2 ----
# Exclude observation periods that have missing both start and end dates
logger::log_info("Step 2: Cleaning empty observation periods")
observation_periods <- observation_periods[
  !(base::is.na(op_start_date) & base::is.na(op_end_date))
]

picard::audit_add(
  base::paste0(
    "number of rows from observation_periods after \n excluding missing BOTH start and end dates: "
  ),
  nrow(observation_periods)
)
logger::log_info("Step 2...Done")

# Step 3 ----
## Creation of SPELLS ----
logger::log_info("Step 3: Creating spells")
function_dir <- file.path(config_t2$T2$root, config_t2$T2$functions)
base::source(file.path(function_dir, "create_spells_function.R"))

# Using the process_observation_periods functions we are going to merge observations
# periods that overlap or have a 180 days gap between them
observation_periods[is.na(op_end_date), op_end_date := end_study_date]

spells <- create_spells(
  dataset = observation_periods,
  id = "person_id",
  start_date = "op_start_date",
  end_date = "op_end_date",
  overlap = FALSE,
  only_overlaps = FALSE,
  gap_allowed = 180,
  replace_missing_end_date = FALSE
)
# colnames(spells)
data.table::setnames(spells, "entry_spell_category", "start_spell")
data.table::setnames(spells, "exit_spell_category", "end_spell")
logger::log_info("Step 3...Done")

# Step 4 ----
## inner join of persons and spells ----
# merge spells with observation_periods using person_id
logger::log_info("Step 4: Merging spells with persons and observation periods")
spells <- data.table::merge.data.table(
  spells,
  observation_periods,
  by = c("person_id", "num_spell")
)

picard::audit_add(
  "number of spells after merging spells with 180d gap: ",
  base::length(base::unique(spells[, c(1:3)]))
)

# D3_PERSONS
d3_cols <- c(
  "person_id",
  "year_of_birth",
  "sex_at_instance_creation",
  "birth_date",
  "death_date",
  "imputed_birth_month",
  "imputed_birth_day",
  "imputed_death_day",
  "imputed_death_month"
)

d3_persons_lim <- d3_persons[, ..d3_cols]

# merge spells with persons
spells <- data.table::merge.data.table(
  spells,
  d3_persons_lim,
  by = "person_id"
)

### Ensuring compatible date columns ----
spells <- picard::set_dates(
  df = spells,
  date_cols = c("start_spell", "end_spell", "birth_date", "death_date")
)

picard::audit_add(
  "number of spells after merging with D3_PERSONS: ",
  base::nrow(spells)
)
logger::log_info("Step 4...Done")
## Step 4.1 ----
### Birth date ----
# We correct the start of the spell checking if this is before the birth_date of a person, if that's the case then start_spell is = to birth_date
logger::log_info("Step 4.1: Correcting start_spell")
picard::audit_add(
  "number of spells to update start_spell with birth_dates: ",
  base::nrow(spells[start_spell < birth_date, ])
)

spells[start_spell < birth_date, start_spell := birth_date]

picard::audit_add(
  "number of spells after updating birth_dates: ",
  base::nrow(spells)
)
logger::log_info("Step 4.1...Done")
## Step 4.2 ----
# We correct the end of the spell checking if this is after the death_date, if that's the case then end_spell = death_date
logger::log_info("Step 4.2: Correcting end_spell")
picard::audit_add(
  "number of spells to update end_spell with death_dates: ",
  base::nrow(spells[end_spell > death_date])
)

spells[end_spell > death_date, end_spell := death_date]

picard::audit_add(
  "number of spells after updating end_spell with death_dates: ",
  base::nrow(spells)
)
logger::log_info("Step 4.2...Done")
## Step 5 ----
# We correct the end of the spell checking if this is empty and the start_spell is before the study date, if that's the case then end_spell = death_date
logger::log_info("Step 5: Correcting end spell")
picard::audit_add(
  "number of spells to update end_spell with end_study_date: ",
  base::nrow(
    spells[
      (is.na(end_spell) | is.null(end_spell)) & start_spell < end_study_date
    ]
  )
)

spells[
  (is.na(end_spell) | is.null(end_spell)) & start_spell < end_study_date,
  end_spell := end_study_date
]

picard::audit_add(
  "number of spells after updating end_spell with death_dates: ",
  base::nrow(spells)
)
logger::log_info("Step 5...Done")
# Step 6 ----
# Selection spells that have with_start_and_end and start_before_end == true
# First creating the two new varibales
logger::log_info("Step 6: Selecting spells")
spells[
  !base::is.na(end_spell),
  start_before_end := base::eval(base::expression(start_spell < end_spell))
]

spells[
  ,
  with_start_and_end := base::eval(
    base::expression(!base::is.na(start_spell) & !base::is.na(end_spell))
  )
]

# Then filtering on them
#* this is just to get the number of exclude spells
excluded_spells <- spells[!with_start_and_end | !start_before_end]
#* Now performing the filtering
spells <- spells[with_start_and_end & start_before_end]

picard::audit_add(
  "number of spells where start_before_end & with_start_and_end: ",
  base::nrow(spells)
)
logger::log_info("Step 6...Done")

# Preparing output ----
logger::log_info("Preparing output...")
cols_of_interest <- c(
  "person_id",
  "start_spell",
  "end_spell",
  # "imputed_end_spell",
  "year_of_birth",
  "sex_at_instance_creation",
  "birth_date",
  "death_date",
  "with_start_and_end",
  "start_before_end"
)

spells <- spells[, ..cols_of_interest]
spells <- base::unique(spells)

dir_d3 <- config_project$outputs$dir_d3


picard::save(
  data = excluded_spells,
  file_path = dir_intermediate_t2,
  file_name = "excluded_spells.parquet",
  create_plot = TRUE
)


picard::save(
  data = spells,
  file_path = dir_d3,
  file_name = "D3_SPELLS.parquet",
  create_plot = TRUE
)
logger::log_info("Output saved")

### Clean up environment ----
base::rm(
  excluded_spells,
  spells,
  d3_cols,
  cols_of_interest,
  observation_periods,
  d3_persons,
  d3_persons_lim,
  d2_db_conn
)
base::gc()
picard::audit_end()
logger::log_info(c("Create Spells - SUCCEED!"))

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
