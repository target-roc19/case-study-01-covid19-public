#' Plot proportion of non-administratively censored persons by time and group
#'
#' @description
#' Creates a line plot showing the proportion of non-administratively censored
#' persons at each time point, with separate lines for CONTROL and EXPOSED groups.
#' This plot aids in model specification decisions (e.g., polynomial order for time).
#'
#' @param tv_data Data table with time-varying data containing columns for:
#'   person_id, group, time (in days), and censoring indicator.
#' @param time_col Character scalar. Column name for time in days (default: "time_days").
#' @param censor_col Character scalar. Column name for censoring indicator
#'   (default: "censor_nonadmin"). Value of 1 indicates non-administratively censored.
#' @param group_levels Character vector. Groups to include in plot
#'   (default: c("EXPOSED", "CONTROL")).
#' @param analysis_label Character scalar. Label for analysis arm (used in title).
#' @param estimand Integer scalar. Estimand number (used in title).
#' @param deap_name Character scalar. DEAP project name (used in title).
#'
#' @return
#' ggplot object with lines showing proportion of non-administratively censored
#' by time and group.
#'
#' @details
#' The function:
#' - Filters to specified group levels
#' - Groups by time and group
#' - Calculates proportion of persons with censoring indicator == 1
#' - Creates line plot with separate colors for each group
#' - Includes count of at-risk persons as secondary y-axis
#'
#' @noRd
plot_censor_by_time <- function(
  tv_data,
  time_col = "time_days",
  censor_col = "censor_nonadmin",
  group_levels = c("EXPOSED", "CONTROL"),
  analysis_label = "Main",
  estimand = 2L,
  deap_name = "Study"
) {
  # Validate inputs
  if (!inherits(tv_data, "data.table")) {
    tv_data <- data.table::as.data.table(tv_data)
  }

  if (!(time_col %in% names(tv_data))) {
    stop(paste0("Column '", time_col, "' not found in tv_data"))
  }

  if (!(censor_col %in% names(tv_data))) {
    stop(paste0("Column '", censor_col, "' not found in tv_data"))
  }

  # Filter to specified groups
  tv_filtered <- tv_data[group %in% group_levels]

  # Calculate proportion non-censored by time and group
  censor_summary <- tv_filtered[
    ,
    .(
      n_persons = data.table::uniqueN(person_id),
      n_censored = sum(get(censor_col), na.rm = TRUE),
      n_at_risk = .N
    ),
    by = .(time = get(time_col), group)
  ]

  # Calculate proportion
  censor_summary[, prop_censored := n_censored / n_persons]

  # Create plot with separate lines for each group
  p <- ggplot2::ggplot(
    censor_summary,
    ggplot2::aes(x = time, y = prop_censored, color = group, linetype = group)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_color_manual(
      name = "Group",
      values = c("EXPOSED" = "#E69F00", "CONTROL" = "#56B4E9")
    ) +
    ggplot2::scale_linetype_manual(
      name = "Group",
      values = c("EXPOSED" = "solid", "CONTROL" = "dashed")
    ) +
    ggplot2::labs(
      title = paste0(deap_name, " Censoring by Time\n", analysis_label, " - Estimand ", estimand),
      x = "Time (days)",
      y = "Proportion Non-Administratively Censored",
      subtitle = "Separate lines show censoring trajectory for each group"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )

  return(list(plot = p, data = censor_summary))
}
