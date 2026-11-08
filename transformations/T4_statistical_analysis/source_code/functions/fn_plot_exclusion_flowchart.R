################################################################
# fn_plot_exclusion_flowchart ---
# Aim:
# Build a Figure 1 style exclusion flowchart using precomputed counts.
################################################################

#' Plot exclusion flowchart for Figure 1.
#'
#' @param counts Named list with flowchart counts.
#' Required names:
#' - individuals_in_data_instance
#' - missing_year_of_birth
#' - age_16_or_older_during_enrolment
#' - not_registered_study_period
#' - age_16_or_older_registered_study_period
#' @param title Plot title.
#'
#' @return ggplot object.
plot_exclusion_flowchart <- function(counts, title = "Figure 1. Flowchart of patient exclusions") {
  required_names <- c(
    "individuals_in_data_instance",
    "missing_year_of_birth",
    "age_16_or_older_during_enrolment",
    "not_registered_study_period",
    "age_16_or_older_registered_study_period"
  )

  missing_names <- setdiff(required_names, names(counts))
  if (length(missing_names) > 0) {
    stop(
      sprintf(
        "counts is missing required fields: %s",
        paste(missing_names, collapse = ", ")
      )
    )
  }

  fmt_n <- function(x) {
    format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  box_data <- data.frame(
    id = c("top", "mid", "bot", "right1", "right2"),
    x = c(0.30, 0.30, 0.30, 0.78, 0.78),
    y = c(0.84, 0.52, 0.20, 0.68, 0.36),
    width = c(0.48, 0.48, 0.48, 0.48, 0.48),
    height = c(0.10, 0.11, 0.11, 0.11, 0.11),
    label = c(
      paste0("n = ", fmt_n(counts$individuals_in_data_instance), " individuals in the data instance"),
      paste0(
        "n = ", fmt_n(counts$age_16_or_older_during_enrolment), " individuals aged 16 or older during\n",
        "the enrolment period"
      ),
      paste0(
        "n = ", fmt_n(counts$age_16_or_older_registered_study_period), " individuals 16 or older registered in\n",
        "the data source during the study period"
      ),
      paste0(
        "Exclude: n = ", fmt_n(counts$missing_year_of_birth), " individuals with missing\n",
        "year of birth"
      ),
      paste0(
        "Exclude: n = ", fmt_n(counts$not_registered_study_period), " not registered\nin the data ",
        "source during the study period"
      )
    ),
    stringsAsFactors = FALSE
  )

  flow_color <- "#0B5E83"
  border_color <- "#083E5A"

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = box_data,
      ggplot2::aes(
        xmin = x - width / 2,
        xmax = x + width / 2,
        ymin = y - height / 2,
        ymax = y + height / 2
      ),
      fill = flow_color,
      color = border_color,
      linewidth = 0.8
    ) +
    ggplot2::geom_text(
      data = box_data,
      ggplot2::aes(x = x, y = y, label = label),
      color = "white",
      size = 5,
      lineheight = 1.0
    ) +
    ggplot2::annotate(
      "segment",
      x = 0.30,
      y = 0.79,
      xend = 0.30,
      yend = 0.58,
      color = flow_color,
      linewidth = 0.8,
      arrow = ggplot2::arrow(length = grid::unit(0.16, "cm"), type = "closed")
    ) +
    ggplot2::annotate(
      "segment",
      x = 0.30,
      y = 0.47,
      xend = 0.30,
      yend = 0.26,
      color = flow_color,
      linewidth = 0.8,
      arrow = ggplot2::arrow(length = grid::unit(0.16, "cm"), type = "closed")
    ) +
    ggplot2::annotate(
      "segment",
      x = 0.30,
      y = 0.68,
      xend = 0.57,
      yend = 0.68,
      color = flow_color,
      linewidth = 0.8,
      arrow = ggplot2::arrow(length = grid::unit(0.16, "cm"), type = "closed")
    ) +
    ggplot2::annotate(
      "segment",
      x = 0.30,
      y = 0.36,
      xend = 0.57,
      yend = 0.36,
      color = flow_color,
      linewidth = 0.8,
      arrow = ggplot2::arrow(length = grid::unit(0.16, "cm"), type = "closed")
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_cartesian(xlim = c(0.02, 1.0), ylim = c(0.08, 0.98), clip = "off") +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )

  p
}
