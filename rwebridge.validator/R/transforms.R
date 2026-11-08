transform_system_event_type_concept_id <- function(dt) {
  if (is.null(dt) || !all(c("system", "event_abbreviation", "type") %in% names(dt))) {
    return(dt)
  }
  out <- data.table::copy(dt)
  out[["concept_id"]] <- paste0(out[["system"]], "_", out[["event_abbreviation"]], "_", out[["type"]])
  out
}

transform_events_concept_id <- function(dt) {
  transform_system_event_type_concept_id(dt)
}

transform_procedures_concept_id <- function(dt) {
  transform_system_event_type_concept_id(dt)
}

transform_medicines_concept_id <- function(dt) {
  if (is.null(dt) || !("drug_abbreviation" %in% names(dt))) {
    return(dt)
  }
  out <- data.table::copy(dt)
  out[["concept_id"]] <- toupper(out[["drug_abbreviation"]])
  out
}

apply_concept_transform <- function(dt, transform_name) {
  if (is.null(dt) || is.null(transform_name)) {
    return(dt)
  }

  if (transform_name == "events") {
    return(transform_events_concept_id(dt))
  }
  if (transform_name == "procedures") {
    return(transform_procedures_concept_id(dt))
  }
  if (transform_name == "medicines") {
    return(transform_medicines_concept_id(dt))
  }

  dt
}
