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
  lm$start_script("create_eligible_pop")
  lm$start_capturing_prints()
}

## Config loading ----
config_project <- picard::load_config("configuration/config_project.yaml")
config_values <- picard::load_config("configuration/config_values.yaml")
create_eligibility <- config_project$create_eligible_pop
test_mode <- config_project$create_eligible_pop$test_mode

## Audit setup ---
picard::audit_start(
  file_name = "create_eligible_pop",
  deap_name = config_project$DEAP_configuration$name
)

logger::log_info("Starting: Computing eligibility")

# ----------------------------
# LOAD DATA
# ----------------------------
possible_batching_dir <- "data/D3_study_variables/D3_MULTIVARIATE_EPISODES"
if (dir.exists(possible_batching_dir)) {
  d3_multivariate_episodes <- data.table::as.data.table(arrow::open_dataset(
    possible_batching_dir,
  ))
} else {
  d3_multivariate_episodes <- data.table::as.data.table(picard::load(
    file_path = create_eligibility$input$dir,
    file_name = create_eligibility$input$file_name
  ))
}


# source functions
source(file.path(
  "transformations",
  "T3_study_design",
  "source_code",
  "functions",
  "compute_d3_eligibility.R"
))

# ----------------------------
# Update SV_HU_CONTACT
# ----------------------------

# There is a bug in the creation of SV_HU_CONTACT. The study variable should assess, on a rolling window, the integer number of source record (PP_HU_CONTACT) in the past year. This is not happening currently.

# Short term solution: Load D3_CONCEPTS_parquet partition for PP_HU_CONTACT and count events
# with a "date" within 365 days before the episode start date, filtering for "value" column
# not empty or "FALSE" (events will say "PRIMARY_CARE" or "HOSPITALISATION").
# Return the counts in the previous year up to the start of each episode. Episodes are defined
# by unique combination of person_id and start_episode columns.

logger::log_info("Correcting SV_HU_CONTACT with rolling window count of healthcare contacts")

# Setup DuckDB connection and register data
db <- duckdb::duckdb()
con <- DBI::dbConnect(db)

tryCatch({
  # Extract unique episodes
  episodes <- d3_multivariate_episodes[, .(person_id, start_episode)] |> unique()

  # Register episodes table
  DBI::dbWriteTable(con, "episodes", episodes, overwrite = TRUE)

  # Query using DuckDB SQL
  hu_contact_summary <- DBI::dbGetQuery(con, "
    SELECT
      e.person_id,
      e.start_episode,
      COALESCE(COUNT(p.person_id), 0) AS SV_HU_CONTACT
    FROM episodes e
    LEFT JOIN read_parquet('data/D3_study_variables/D3_CONCEPTS_parquet/concept_id=PP_HU_CONTACT/**/*.parquet') p
      ON e.person_id = p.person_id
      AND p.date <= e.start_episode
      AND p.date > e.start_episode - INTERVAL 184 DAY
      AND p.value IS NOT NULL
      AND p.value != 'FALSE'
    GROUP BY e.person_id, e.start_episode
    ORDER BY e.person_id, e.start_episode
  ")

  # Convert to data.table
  hu_contact_summary <- data.table::as.data.table(hu_contact_summary)
  hu_contact_summary[, SV_HU_CONTACT := as.integer(SV_HU_CONTACT)]

  # Merge back: update SV_HU_CONTACT or create it if it doesn't exist
  if ("SV_HU_CONTACT" %in% colnames(d3_multivariate_episodes)) {
    logger::log_info("Droping old SV_HU_CONTACT from d3_multivariate_episodes")
    d3_multivariate_episodes[, SV_HU_CONTACT := NULL]
  }
  logger::log_info("Adding SV_HU_CONTACT to d3_multivariate_episodes")
  d3_multivariate_episodes <- merge(
    d3_multivariate_episodes,
    hu_contact_summary,
    by = c("person_id", "start_episode"),
    all.x = TRUE
  )

  # Recategorize: 0 = 0, 1 = 1, 2+ = 2
  d3_multivariate_episodes[, SV_HU_CONTACT := data.table::fifelse(SV_HU_CONTACT >= 2, 2, SV_HU_CONTACT)]

  rm(episodes, hu_contact_summary)

  # ----------------------------
  # Update SV_HU_PC_CONTACT (from HU_GP_CONTACT)
  # ----------------------------
  logger::log_info("Correcting SV_HU_PC_CONTACT with rolling window count of general practitioner contacts")

  hu_pc_contact_summary <- DBI::dbGetQuery(con, "
    SELECT
      e.person_id,
      e.start_episode,
      COALESCE(COUNT(p.person_id), 0) AS SV_HU_PC_CONTACT
    FROM episodes e
    LEFT JOIN read_parquet('data/D3_study_variables/D3_CONCEPTS_parquet/concept_id=HU_GP_CONTACT/**/*.parquet') p
      ON e.person_id = p.person_id
      AND p.date <= e.start_episode
      AND p.date > e.start_episode - INTERVAL 184 DAY
      AND p.value IS NOT NULL
      AND p.value != 'FALSE'
    GROUP BY e.person_id, e.start_episode
    ORDER BY e.person_id, e.start_episode
  ")

  # Convert to data.table
  hu_pc_contact_summary <- data.table::as.data.table(hu_pc_contact_summary)
  hu_pc_contact_summary[, SV_HU_PC_CONTACT := as.integer(SV_HU_PC_CONTACT)]

  # Merge back: update SV_HU_PC_CONTACT or create it if it doesn't exist
  if ("SV_HU_PC_CONTACT" %in% colnames(d3_multivariate_episodes)) {
    logger::log_info("Dropping old SV_HU_PC_CONTACT from d3_multivariate_episodes")
    d3_multivariate_episodes[, SV_HU_PC_CONTACT := NULL]
  }
  logger::log_info("Adding SV_HU_PC_CONTACT to d3_multivariate_episodes")
  d3_multivariate_episodes <- merge(
    d3_multivariate_episodes,
    hu_pc_contact_summary,
    by = c("person_id", "start_episode"),
    all.x = TRUE
  )

  # Recategorize: 0 = 0, 1 = 1, 2+ = 2
  d3_multivariate_episodes[, SV_HU_PC_CONTACT := data.table::fifelse(SV_HU_PC_CONTACT >= 2, 2, SV_HU_PC_CONTACT)]

  rm(hu_pc_contact_summary)

  # ----------------------------
  # Update SV_HU_HOSP_CONTACT (from PP_HU_HOSP_CONTACT)
  # ----------------------------
  logger::log_info("Correcting SV_HU_HOSP_CONTACT with rolling window count of hospitalization contacts")

  hu_hosp_contact_summary <- DBI::dbGetQuery(con, "
    SELECT
      e.person_id,
      e.start_episode,
      COALESCE(COUNT(p.person_id), 0) AS SV_HU_HOSP_CONTACT
    FROM episodes e
    LEFT JOIN read_parquet('data/D3_study_variables/D3_CONCEPTS_parquet/concept_id=PP_HU_HOSP_CONTACT/**/*.parquet') p
      ON e.person_id = p.person_id
      AND p.date <= e.start_episode
      AND p.date > e.start_episode - INTERVAL 184 DAY
      AND p.value IS NOT NULL
      AND p.value != 'FALSE'
    GROUP BY e.person_id, e.start_episode
    ORDER BY e.person_id, e.start_episode
  ")

  # Convert to data.table
  hu_hosp_contact_summary <- data.table::as.data.table(hu_hosp_contact_summary)
  hu_hosp_contact_summary[, SV_HU_HOSP_CONTACT := as.integer(SV_HU_HOSP_CONTACT)]

  # Merge back: update SV_HU_HOSP_CONTACT or create it if it doesn't exist
  if ("SV_HU_HOSP_CONTACT" %in% colnames(d3_multivariate_episodes)) {
    logger::log_info("Dropping old SV_HU_HOSP_CONTACT from d3_multivariate_episodes")
    d3_multivariate_episodes[, SV_HU_HOSP_CONTACT := NULL]
  }
  logger::log_info("Adding SV_HU_HOSP_CONTACT to d3_multivariate_episodes")
  d3_multivariate_episodes <- merge(
    d3_multivariate_episodes,
    hu_hosp_contact_summary,
    by = c("person_id", "start_episode"),
    all.x = TRUE
  )

  # Recategorize: 0 = 0, 1 = 1, 2+ = 2
  d3_multivariate_episodes[, SV_HU_HOSP_CONTACT := data.table::fifelse(SV_HU_HOSP_CONTACT >= 2, 2, SV_HU_HOSP_CONTACT)]

  rm(hu_hosp_contact_summary)
}, finally = {
  DBI::dbDisconnect(con)
  duckdb::duckdb_shutdown(db)
})

logger::log_info("Completed SV_HU_CONTACT, SV_HU_PC_CONTACT, and SV_HU_HOSP_CONTACT corrections")

# Reset key to avoid embedding large indices in schema metadata when saving to parquet
data.table::setkey(d3_multivariate_episodes, NULL)

# ----------------------------
# ELIGIBILITY
# ----------------------------
# if not included already, add STABLE_PRECONDITIONS defined from D3_ELIGIBILIT$SV_HOSP_6WEEKS
if (!"STABLE_PRECONDITIONS" %in% colnames(d3_multivariate_episodes)) {
  logger::log_info("Adding STABLE_PRECONDITIONS from SV_HOSP_6WEEKS")
  d3_multivariate_episodes[, STABLE_PRECONDITIONS := ifelse(SV_HOSP_6WEEKS == TRUE, FALSE, TRUE)]
}

# Compute eligibility flags using the `compute_d3_eligibility` function which
# applies the codebook definitions for `in_eligible_pop`, `eligible_exposed`,
# and# `eligible_control` to the multivariate episode rows. It also keeps
# matching variables for later use in the cohort creation step.
d3_eligibility <- compute_d3_eligibility(d3_multivariate_episodes)

# If not included already, add year_of_birth and/or SV_SEX from D3_PERSONS
cols_to_add <- c(
  if (!"year_of_birth" %in% colnames(d3_eligibility)) "year_of_birth",
  if (!"SV_SEX" %in% colnames(d3_eligibility)) "sex_at_instance_creation"
)
if (length(cols_to_add) > 0) {
  logger::log_info("Adding {paste(cols_to_add, collapse = ', ')} from D3_PERSONS")
  d3_persons <- picard::load(
    file_path = create_eligibility$input$dir,
    file_name = "D3_PERSONS.parquet"
  )
  d3_eligibility <- merge(
    d3_eligibility,
    d3_persons[, c("person_id", cols_to_add), with = FALSE],
    by = "person_id",
    all.x = TRUE
  )
  if ("sex_at_instance_creation" %in% colnames(d3_eligibility)) {
    data.table::setnames(d3_eligibility, "sex_at_instance_creation", "SV_SEX")
  }
  rm(d3_persons)
}

logger::log_info("Finishing: Computing eligibility")
picard::audit_end()

if (test_mode) {
  # Make half of the rows eligible_exposed and half eligible_control for testing purposes (at random)
  set.seed(123)
  d3_eligibility[, eligible_exposed := sample(c(TRUE, FALSE), .N, replace = TRUE)]
  d3_eligibility[, eligible_control := !eligible_exposed]
}

# Check if there are any duplicated person_ids with eligible_exposed == TRUE
duplicated_ids <- d3_eligibility[eligible_exposed == TRUE][duplicated(d3_eligibility[eligible_exposed == TRUE]$person_id)]$person_id
if (length(duplicated_ids) > 0) {
  logger::log_warn("Found duplicated person_ids with eligible_exposed == TRUE. Number of duplicated person_ids: {length(duplicated_ids)}")
  logger::log_warn("Duplicated person_ids: {paste(duplicated_ids[1:min(length(duplicated_ids), 5)], collapse = ', ')}")
} else {
  logger::log_info("No duplicated person_ids with eligible_exposed == TRUE.")
}

# ----------------------------
# SAVE OUTPUT
# ----------------------------

# Remove the "index" attribute from d3_eligibility to avoid embedding large indices in schema metadata when saving to parquet
attr(d3_eligibility, "index") <- NULL

picard::save(
  data = d3_eligibility,
  file_path = create_eligibility$output$dir,
  file_name = create_eligibility$output$file_name
)

# ----------------------------
# CLEAN ENVIRONMENT
# ----------------------------
rm(d3_eligibility)
gc()
