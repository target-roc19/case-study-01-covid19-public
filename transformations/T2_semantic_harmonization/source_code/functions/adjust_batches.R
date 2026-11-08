#' Dynamically adjust batch size and recalculate batches based on processing
#' time
#'
#' This function adjusts the batch size based on the time taken in the previous
#' iteration to ensure processing stays within a target maximum execution time.
#'
#' @param list_personids A data.table containing person IDs to be processed
#' with existing 'batch' column
#' @param iteration_index The current iteration index in the loop
#' @param running_times Vector with time (in seconds) that of the previous
#' iterations took
#' @param initial_batch_size Initial batch size to use for the first iteration
#' (default: 10000)
#' @param target_max_time Maximum target processing time per batch in seconds
#' (default: 30)
#'
#' @return A data.table with updated batch assignments
#'
#' @export
adjust_batches <- function(
  list_personids,
  iteration_index,
  running_times,
  initial_batch_size = 10000,
  target_max_time = 30,
  min_growth_factor = 0.5,
  max_growth_factor = 1.5
) {
  # Check if input is a data.table
  if (!data.table::is.data.table(list_personids)) {
    if (is.data.frame(list_personids)) {
      list_personids <- data.table::as.data.table(list_personids)
      logger::log_info("Input converted from data.frame to data.table")
    } else {
      logger::log_error("Input must be a data.table or a data.frame")
    }
  }

  # Check for required batch column
  if (!"batch" %in% names(list_personids)) {
    logger::log_error(" Column 'batch' is missing from input data")
  }

  # Create a copy to avoid modifying the input by reference
  result <- data.table::copy(list_personids)

  # For the first iteration, use the initial batch size
  if (iteration_index <= 1) {
    new_batch_size <- initial_batch_size
    estimated_time_per_batch <- NA
    logger::log_info(
      paste0(
        "Iteration 1  - Initial batch size: ",
        new_batch_size
      )
    )
  } else {
    # Get the size of the previous batch
    # Fix: Use proper data.table syntax with == instead of =
    previous_batch_size <- result[batch == (iteration_index - 1), .N]
    previous_iteration_time <- running_times[iteration_index - 1]

    # Guard against empty previous batch
    if (previous_batch_size == 0) {
      logger::log_info(
        "Warning - Previous batch was empty, using initial batch size"
      )
      new_batch_size <- initial_batch_size
    } else {
      # Calculate processing time per record on the previous batch
      previous_time_per_record <- previous_iteration_time / previous_batch_size

      # Calculate optimal batch size to meet target max time
      time_ratio <- (target_max_time / previous_iteration_time)

      # Clamp ratio within bounds
      clamped_ratio <- min(
        max(time_ratio, min_growth_factor), max_growth_factor
      )

      new_batch_size <- round(previous_batch_size * clamped_ratio)

      # Boosting the increment
      if (length(running_times) >= 2) {
        if (
          (
            running_times[length(running_times)] - running_times[
              length(running_times) - 1
            ]
          ) /
            mean(running_times) < 0.5
        ) {
          clamped_ratio <- 2
          new_batch_size <- round(previous_batch_size * clamped_ratio)
        }
      }
      # Ensure a minimum batch size of 100
      new_batch_size <- max(100, new_batch_size)

      # Estimate time per batch with new batch size
      estimated_time_per_batch <- new_batch_size * previous_time_per_record

      logger::log_info(paste0(
        "Iteration ", iteration_index,
        " - Previous time: ", round(previous_iteration_time, 2), "s",
        " - Time per record: ", round(previous_time_per_record * 1000, 2),
        "ms", # Convert to ms for readability
        " - Ratio ", clamped_ratio,
        " - Adjusted batch size from ", previous_batch_size,
        " to ", new_batch_size,
        " - Estimated time per batch: ", round(estimated_time_per_batch, 2), "s"
      ))
    }
  }

  # Handle empty data.table case
  if (nrow(result) == 0) {
    logger::log_info(
      "Warning - Empty input, no batches created"
    )
    return(result)
  }

  # Get data already processed in previous iterations
  already_batched_result <- result[batch < iteration_index]

  # Get data still to be processed
  to_be_batched <- result[batch >= iteration_index]

  # If there's data to be rebatched
  if (nrow(to_be_batched) > 0) {
    # Create new batch column with adjusted size
    new_batched_result <- define_batches_with_index(
      to_be_batched,
      batch_size = new_batch_size,
      start_index = iteration_index
    )

    # Combine already processed and newly batched data
    if (nrow(already_batched_result) > 0) {
      result <- data.table::rbindlist(
        list(already_batched_result, new_batched_result), use.names = TRUE
      )
    } else {
      result <- new_batched_result
    }
  }

  result
}

# Define a helper function to update batch numbers
define_batches_with_index <- function(dt, batch_size, start_index) {
  dt[
    ,
    batch := start_index + rep(
      0:(ceiling(.N / batch_size) - 1),
      each = batch_size
    )[seq_len(.N)]
  ]
}
