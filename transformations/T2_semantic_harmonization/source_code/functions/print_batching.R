#' Plot and (optionally) save batching performance metrics
#'
#' This function creates two vertically stacked line plots:
#' one showing batch sizes per iteration and another showing the
#' run time (in seconds) per iteration.
#' Optionally, the combined plot can be saved to a file.
#'
#' @param batch_sizes Numeric vector of batch sizes for each iteration.
#' @param running_times Numeric vector of run times for each iteration.
#' @param path Optional. File path (including extension,
#' e.g., "plot.png" or "plot.pdf") to save the plot.
#' If `NULL`, the plot is displayed but not saved.
#'
#' @return Invisibly returns the combined plot object
#' (useful for further customization or saving).
#' @export
print_batching <- function(batch_sizes, running_times, path = NULL) {
  # Create base data frame
  data <- base::data.frame(
    Iteration = seq_along(batch_sizes),
    Batch_Size = batch_sizes,
    Run_Time = running_times
  )

  # Plot 1: Batch Size
  p1 <- ggplot2::ggplot(data, ggplot2::aes(x = Iteration, y = Batch_Size)) +
    ggplot2::geom_line(color = "darkgreen", linewidth = 1.2) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Batch Size per Iteration",
      x = "Iteration",
      y = "Batch Size"
    )

  # Plot 2: Run Time
  p2 <- ggplot2::ggplot(data, ggplot2::aes(x = Iteration, y = Run_Time)) +
    ggplot2::geom_line(color = "firebrick", linewidth = 1.2) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Run Time per Iteration",
      x = "Iteration",
      y = "Time (seconds)"
    )

  # Stack the plots vertically
  if (!is.null(path)) {
    png(path, width = 8 * 96, height = 8 * 96, res = 300)
    combined_plot <- gridExtra::grid.arrange(p1, p2, ncol = 1)
    dev.off()
  }

  if (!is.null(path)) {
    ggplot2::ggsave(
      path,
      plot = combined_plot,
      width = 8,
      height = 8,
      dpi = 300
    )
  }
}
