#' Plot Counts by Group and Time with Optional Capping
#'
#' Creates a bar plot showing counts of individuals grouped by a specified variable
#' and aggregated over time intervals. The x-axis is dynamically labeled and formatted
#' according to the chosen binning unit, and it can be limited to a study period defined
#' by `start_date` and `end_date`. Optionally, the x-axis can be capped at the last
#' binning unit with non-missing observations.
#'
#' @param df A data frame containing the data to plot.
#' @param group_var A string specifying the name of the variable used for grouping.
#' @param binning_var A string specifying the name of the date or time variable to bin.
#' @param binning_unit A string indicating the time unit for binning.
#' Options are `"day"`, `"3 days"`, `"week"` (default), or `"month"`.
#' @param breaks_interval An integer specifying the interval for x-axis breaks, in units of `binning_unit`. Default is `2`.
#' @param start_date A Date object or `NULL`. If provided, defines the start of the study period and sets the minimum x-axis limit. Default is `NULL`.
#' @param end_date A Date object or `NULL`. If provided, defines the end of the study period. Default is `NULL`.
#' @param cap_at_last_obs Logical. If `TRUE`, the plot is capped at the last `binning_unit` with non-missing observations. Default is `FALSE`.
#' @param deap A string representing the name of the Data Expert Access Provider (DEAP) corresponding to the current data. Default is `NULL`.
#' @param title A string specifying the title of the plot. Default is `NULL` (no title).
#' @param observation_unit A string specifying the unit of observation (e.g., "individuals", "cases"). Default is "individuals".
#'
#' @details
#' The function dynamically adjusts the bar width, x-axis labels, and x-axis break
#' intervals based on the chosen `binning_unit`. The counts are calculated by grouping
#' the data frame by the `group_var` and the binned version of the `binning_var`.
#'
#' If `cap_at_last_obs` is `TRUE`, the x-axis is automatically capped at the last
#' time unit with non-missing observations, overriding the `end_date` if it is provided.
#'
#' @return A ggplot object representing the bar plot.
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(lubridate)
#'
#' # Example data
#' example_df <- data.frame(
#'   group = rep(c("Group A", "Group B"), each = 50),
#'   date = sample(seq.Date(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day"), 100, replace = TRUE)
#' )
#'
#' # Generate plot with weekly binning
#' plot_counts_by_group_time(
#'   df = example_df,
#'   group_var = "group",
#'   binning_var = "date",
#'   binning_unit = "week",
#'   breaks_interval = 4,
#'   start_date = as.Date("2022-01-01"),
#'   end_date = as.Date("2022-12-31"),
#'   cap_at_last_obs = TRUE,
#'   title = "Weekly Counts by Group"
#' )
#'
#' @importFrom dplyr mutate group_by summarise case_when n_distinct pull filter
#' @importFrom lubridate floor_date
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal element_text scale_x_date
#' @export
plot_counts_by_group_time <- function(df, group_var, binning_var, binning_unit = "week", breaks_interval = 2,
                                      start_date = NULL, end_date = NULL, cap_at_last_obs = FALSE,
                                      deap = NULL, title = NULL, observation_unit = "individuals") {
  binning_dates <- as.Date(df[[binning_var]])
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
  }
  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
  }

  # Ensure binning variable is in the correct format
  df <- df |>
    dplyr::mutate(binned_date = dplyr::case_when(
      binning_unit == "day" ~ lubridate::floor_date(binning_dates, unit = "day"),
      binning_unit == "3 days" ~ lubridate::floor_date(binning_dates, unit = "day") - lubridate::days(1), # 3-day bins
      binning_unit == "week" ~ lubridate::floor_date(binning_dates, unit = "week"),
      binning_unit == "month" ~ lubridate::floor_date(binning_dates, unit = "month"),
      TRUE ~ lubridate::floor_date(binning_dates, unit = "week") # Default to week if invalid unit
    ))

  df <- df |>
    dplyr::mutate(binned_date = as.Date(binned_date))

  # Count cases by group and binning variable
  df_count <- df |>
    dplyr::group_by(!!rlang::sym(group_var), binned_date) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  # Optionally cap at the last non-missing bin
  if (cap_at_last_obs) {
    last_obs_date <- df_count |>
      dplyr::filter(!is.na(binned_date)) |>
      dplyr::pull(binned_date) |>
      max(na.rm = TRUE)

    # If end_date is NULL or later than the last observed date, cap it
    if (is.null(end_date) || end_date > last_obs_date) {
      end_date <- last_obs_date
    }
  }

  # Define bar width based on binning_unit
  bar_width <- if (binning_unit == "day") {
    1
  } else if (binning_unit == "week") {
    7
  } else if (binning_unit == "3 days") {
    3
  } else {
    30
  }

  # Dynamic labels and breaks for the x-axis based on binning_unit
  date_label_format <- dplyr::case_when(
    binning_unit == "day" ~ "%Y-%m-%d",
    binning_unit == "3 days" ~ "%Y-%m-%d",
    binning_unit == "week" ~ "%Y-%U", # Year-week format
    binning_unit == "month" ~ "%Y-%m",
    TRUE ~ "%Y-%U" # Default to year-week
  )

  date_breaks_interval <- dplyr::case_when(
    binning_unit == "day" ~ paste(breaks_interval, "days"),
    binning_unit == "3 days" ~ paste(breaks_interval * 3, "days"),
    binning_unit == "week" ~ paste(breaks_interval, "weeks"),
    binning_unit == "month" ~ paste(breaks_interval, "months"),
    TRUE ~ paste(breaks_interval, "weeks")
  )

  # Create the plot
  ggplot2::ggplot(df_count, aes(x = binned_date, y = count, fill = !!rlang::sym(group_var))) +
    ggplot2::geom_bar(stat = "identity", color = "black", position = "stack", width = bar_width, linewidth = 0.2) +
    ggplot2::labs(
      title = title,
      x = paste0("Study ", binning_unit),
      y = paste0("Number of ", observation_unit),
      fill = group_var
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") +
    ggplot2::scale_x_date(
      date_labels = date_label_format,
      date_breaks = date_breaks_interval
    ) +
    ggplot2::coord_cartesian(xlim = c(start_date, end_date)) +
    viridis::scale_fill_viridis(discrete = TRUE, option = "mako", begin = 0.2, end = 1.0) +
    ggplot2::theme(
      plot.title = element_text(color = "black", size = 11, lineheight = 1),
      legend.position = "top",
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 11)
    )
}
