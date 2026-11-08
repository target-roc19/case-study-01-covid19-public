# Processing persons ----
# Aim:
# Processing the person information.
# Imputation of dates and exclusion of cases with missing information:
# death date or birth date

# Input files: ----
# - PERSONS

# Output:
# person_id
# sex_at_instance_creation
# imputed_birth_day
# imputed_birth_month
# imputed_death_day
# imputed_death_month
# birth_date
# death_date
# missing_year_of_birth
# not_missing_year_death
# death_before_birth
# missing_sex
# year_of_birth

# Initial setup ----
## Script logger ----
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("Processing D3 PERSONS")
  # Start capturing print statements
  lm$start_capturing_prints()
}

logger::log_info("[Processing D3 PERSONS] - START")

## Config loading ----
picard::load_config()

## Audit setup ----
# make path between dir_d3 and audit.txt
picard::audit_start(
  file_name = "d3_persons",
  deap_name = config_project$DEAP_configuration$name
)

dir_intermediate_t2 <- base::file.path(
  config_t2$T2$root,
  config_t2$T2$intermediate
)

DEAP <- config_project$DEAP_configuration$name
data_model <- config_project$DEAP_configuration$data_model
dir_d3 <- config_project$output$dir_d3

study_end_date <- picard::get_date_value(config_values$end_study_date)

### Load input data: PERSONS and CDM
# PERSONS
d3_person_folder <- config_project$processing_persons$persons

if (data_model == "ConcePTION") {
  if (base::grepl("duckdb", d3_person_folder)) {
    dir_d2_db <- config_pipeline$databases$dir_d2_db
    d2_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_d2_db)
    persons <- DBI::dbGetQuery(d2_db_conn, "SELECT * FROM main.PERSONS;") |>
      data.table::as.data.table()
    duckdb::dbDisconnect(d2_db_conn, shutdown = TRUE)
  } else {
    persons <- picard::load(d3_person_folder) |>
      data.table::as.data.table()
  }
} else if (data_model == "OMOP") {
  if (base::grepl("duckdb", d3_person_folder)) {
    dir_d2_db <- config_pipeline$databases$dir_d2_db
    d2_db_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_d2_db)
    persons <- DBI::dbGetQuery(d2_db_conn, "SELECT * FROM main.person") |>
      data.table::as.data.table()
    if ("death" %in% DBI::dbListTables(d2_db_conn)) {
      logger::log_info("death table exists, merging person and death")
      deaths <- DBI::dbGetQuery(d2_db_conn, "SELECT * FROM main.death") |>
        data.table::as.data.table()
    } else {
      logger::log_info("death table does not exist, using person table only")
      deaths <- data.table::data.table(
        person_id = integer(),
        death_date = as.Date(character())
      )
    }


    deaths[, `:=`(
      year_of_death = clock::get_year(death_date),
      month_of_death = clock::get_month(death_date),
      day_of_death = clock::get_day(death_date)
    )]

    # Merge person and death
    persons <- data.table::merge.data.table(
      x = persons,
      y = deaths,
      by.x = "person_id",
      by.y = "person_id",
      all.x = TRUE
    )

    persons[
      ,
      sex_at_instance_creation := data.table::fcase(
        gender_concept_id == 8532, "F",
        gender_concept_id == 8507, "M"
      )
    ]

    data.table::setnames(persons, old = c("race_source_value"), new = c("race"))
    duckdb::dbDisconnect(d2_db_conn, shutdown = TRUE)
  } else {
    persons <- picard::load(d3_person_folder) |>
      data.table::as.data.table()
  }
}

# Ensure that the following values are integers
dates_person <- c(
  "day_of_birth",
  "month_of_birth",
  "year_of_birth",
  "day_of_death",
  "month_of_death",
  "year_of_death"
)

persons[,
  (dates_person) := base::lapply(.SD, as.integer),
  .SDcols = dates_person
]

# Step 1 ----
## sex_at_instance_creation ----
# Set NA when sex_at_instance_creation is not F or M
persons[
  ,
  "sex_at_instance_creation" := data.table::fifelse(
    !(sex_at_instance_creation %in% c("M", "F")),
    NA_character_,
    sex_at_instance_creation
  )
]

## Imputed_birth_day ----
# if day_of_birth missing or na, imputed_birth_day equal to TRUE
persons[
  ,
  "imputed_birth_day" := data.table::fifelse(
    !is.na(year_of_birth) & (is.na(day_of_birth) | is.null(day_of_birth)),
    TRUE,
    FALSE
  )
][
  ,
  "day_of_birth" := data.table::fifelse(
    imputed_birth_day == TRUE,
    16,
    day_of_birth
  )
]

## imputed_birth_month ----
# if month_of_birth missing or na, imputed_birth_month equal to TRUE
persons[
  ,
  "imputed_birth_month" := data.table::fifelse(
    !is.na(year_of_birth) & (is.na(month_of_birth) | is.null(month_of_birth)),
    TRUE,
    FALSE
  )
][
  ,
  "month_of_birth" := data.table::fifelse(
    imputed_birth_month == TRUE,
    6,
    month_of_birth
  )
][
  ,
  "month_of_birth" := data.table::fifelse(
    imputed_birth_month == TRUE & day_of_birth == 31,
    5,
    month_of_birth
  )
]

## imputed_death_day ----
# if day_of_death is missing or NA, imputed_death_day equal to TRUE)
persons[
  ,
  "imputed_death_day" := data.table::fifelse(
    !is.na(year_of_death) & (is.na(day_of_death) | is.null(day_of_death)),
    TRUE,
    FALSE
  )
][
  ,
  "day_of_death" := data.table::fifelse(
    imputed_death_day == TRUE,
    16,
    day_of_death
  )
]

## imputed_death_month ----
# if month_of_death is missing or NA, imputed_death_month equal to TRUE)
persons[
  ,
  "imputed_death_month" := data.table::fifelse(
    !is.na(year_of_death) &
      (base::is.na(month_of_death) | base::is.null(month_of_death)),
    TRUE,
    FALSE
  )
][
  ,
  "month_of_death" := data.table::fifelse(
    imputed_death_month == TRUE,
    6,
    month_of_death
  )
][
  ,
  "month_of_death" := data.table::fifelse(
    imputed_death_month == TRUE & day_of_death == 31,
    5,
    month_of_death
  )
]

## create missing year of birth ----
# if year_of_birth missing or na, missing_year_of_birth`` equal to TRUE
persons[
  ,
  "missing_year_of_birth" := data.table::fifelse(
    base::is.na(year_of_birth),
    TRUE,
    FALSE
  )
]
## create not missing year of birth
# not_missing_year_death := ifelse(!(is.na(year_of_death) & (!is.na(day_of_death) & !is.na(month_of_death))), TRUE, FALSE)
persons[
  ,
  "not_missing_year_death" := data.table::fifelse(
    !(is.na(year_of_death) & (!is.na(day_of_death) | !is.na(month_of_death))),
    TRUE,
    FALSE
  )
]

## Create missing sex ----
# Add flag true if sex_at_instance_creation is missing or different from M/F
persons[
  ,
  "missing_sex" := data.table::fifelse(
    is.na(sex_at_instance_creation) |
      is.null(sex_at_instance_creation) |
      !(sex_at_instance_creation %in% c("M", "F")),
    TRUE,
    FALSE
  )
]

# Step 8 ----
## Create birth date ----
# Crate birth date if year_of_birth, month_of_birth, day_of_birth is not missing
persons[
  ,
  "birth_date" := data.table::fifelse(
    !base::is.na(day_of_birth) &
      !base::is.na(month_of_birth) &
      !base::is.na(year_of_birth),
    base::as.Date(base::ISOdate(
      year = year_of_birth,
      month = month_of_birth,
      day = day_of_birth
    )),
    base::as.Date(NA)
  )
]

## Create death date ----
# Combine day_of_death, month_of_death and year_of_death into death_date
persons[
  ,
  "death_date" := data.table::fifelse(
    !base::is.na(day_of_death) &
      !base::is.na(month_of_death) &
      !base::is.na(year_of_death),
    base::as.Date(base::ISOdate(year_of_death, month_of_death, day_of_death)),
    base::as.Date(NA)
  )
]

## Create death before death ----
# True if death_date < birth_date
persons[
  ,
  "death_before_birth" := data.table::fifelse(
    death_date < birth_date,
    TRUE,
    FALSE,
    na = FALSE
  )
]

# Processing ----
## 1. Remove step. N1 := N - n1 ----
# missing_year_of_birth, select FALSE
persons <- persons[missing_year_of_birth == FALSE]

picard::audit_add(
  paste(
    "remove missing year of birth, persons rows:",
    nrow(persons)
  )
)

## 2. Remove step. N2 := N1 - n2 ----
# not_missing_year_death
persons <- persons[not_missing_year_death == TRUE]

picard::audit_add(paste(
  "remove missing year of death, persons rows:",
  nrow(persons)
))

## 3. Remove step. N3 := N2 - n3 ----
# death_before_birth
persons <- persons[death_before_birth != TRUE]

picard::audit_add(paste(
  "remove death_before_birth, persons rows:",
  nrow(persons)
))

## 4. Remove step. N4 := N3 - n4 ----
# Filter  duplicated person_id
persons <- persons[!duplicated(person_id)]

picard::audit_add(paste(
  "remove duplicated person_id, persons rows:",
  nrow(persons)
))

## 5. Remove step. N6 := N5 - n6  ----
# We filter NAs out.
persons <- persons[!is.na(sex_at_instance_creation)]

picard::audit_add(paste(
  "remove missing sex, persons rows:",
  nrow(persons)
))

## 6. Remove step. N7 := N6 - n7  ----
# Inclusion criteria: individuals aged 16 or older
persons <- persons[
  clock::add_years(birth_date, 16) < clock::add_days(study_end_date, -1)
]
picard::audit_add(paste(
  "remove age<16, persons rows:",
  nrow(persons)
))

required_cols_output <- c(
  "person_id",
  "sex_at_instance_creation",
  "imputed_birth_day",
  "imputed_birth_month",
  "imputed_death_day",
  "imputed_death_month",
  "birth_date",
  "death_date",
  "missing_year_of_birth",
  "not_missing_year_death",
  "death_before_birth",
  "missing_sex",
  "year_of_birth"
)

# Output ----
## Creating the output table ----
persons <- persons[, mget(required_cols_output)]

## Saving output ----
### Parquet export ----
picard::save(
  data = persons,
  file_path = dir_d3,
  file_name = "D3_PERSONS.parquet",
  create_plot = TRUE
)

## Cleanup ---
rm(persons)
picard::audit_end()

logger::log_info(c("[Processing D3 PERSONS] - SUCCEED"))

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
