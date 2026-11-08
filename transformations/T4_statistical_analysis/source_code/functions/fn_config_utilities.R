#' Normalize YAML configuration vectors
#'
#' Converts YAML scalar/list values into clean unique character vector.
#' Handles NULL, empty, and whitespace-only entries gracefully.
#'
#' @param value YAML parsed scalar, list, or NULL value.
#'
#' @return Character vector of unique, trimmed values.
#'
#' @noRd
normalize_vec_config <- function(value) {
  # Unlist nested structures to flat vector.
  vals <- unlist(value, use.names = FALSE)
  # Convert to character and remove empty strings.
  vals <- as.character(vals)
  vals <- vals[nzchar(vals)]
  # Return unique, trimmed values.
  unique(trimws(vals))
}

#' Safe scalar-to-integer conversion with NA fallback
#'
#' Converts YAML null or empty to NA_integer_.
#' Used for optional config overrides.
#'
#' @param value Scalar, vector, NULL, or length-0 value.
#'
#' @return Integer scalar (NA_integer_ if input is NULL/empty).
#'
#' @noRd
scalar_or_na_int <- function(value) {
  # YAML null is treated as "no override".
  if (is.null(value) || length(value) == 0L) {
    return(NA_integer_)
  }
  as.integer(value)
}
