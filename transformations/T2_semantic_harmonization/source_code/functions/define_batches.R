#' Create batches from a list of person IDs
#'
#' This function divides a data.table containing person IDs into batches
#' of a specified size for efficient processing.
#'
#' @param list_personids A data.table containing person IDs to be processed
#' @param batch_size Integer specifying the size of each batch (default: 10000)
#'
#' @return The input data.table with an additional column 'batch' indicating
#'         the batch number for each person
#'
#' @examples
#' \dontrun{
#' persons <- data.table(personid = 1:25000)
#' batched_persons <- define_batches(persons, batch_size = 10000)
#' }
#' @export
define_batches <- function(
  list_personids, batch_size = 10000, verbose = TRUE, batch_index = 1
) {
  # Check if input is a data.table
  if (!data.table::is.data.table(list_personids)) {
    if (is.data.frame(list_personids)) {
      # Convert data.frame to data.table
      list_personids <- data.table::as.data.table(list_personids)
      logger::log_info("Input converted from data.frame to data.table")
    } else {
      logger::log_error("Input must be a data.table or a data.frame")
    }
  }

  # Validate batch_size
  if (!is.numeric(batch_size) || batch_size <= 0 || !is.finite(batch_size)) {
    logger::log_error("batch_size must be a positive number")
  }

  # Convert to integer to ensure proper batching
  batch_size <- as.integer(batch_size)

  # Handle empty data.table case
  if (nrow(list_personids) == 0) {
    logger::log_warn("Empty input, no batches created")
    return(list_personids)
  }

  # Create a copy to avoid modifying the input by reference
  result <- data.table::copy(list_personids)

  # Check if 'batch' column already exists and warn
  if ("batch" %in% names(result)) {
    logger::log_warn("Column 'batch' already exists and will be overwritten")
  }

  # Create batch column
  result[
    ,
    batch := rep(batch_index:ceiling(.N / batch_size), each = batch_size)[
      batch_index:.N
    ]
  ]

  # Get number of batches safely
  num_batches <- if (nrow(result) > 0) max(result$batch, na.rm = TRUE) else 0

  # Log the number of batches
  if(verbose){
    logger::log_info(paste0("Number of batches: ", num_batches))
  }
  result
}
