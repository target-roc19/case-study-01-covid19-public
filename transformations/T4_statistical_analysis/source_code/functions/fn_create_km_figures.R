#' Personalized Theme for KM Tables
#'
#' Creates a lightweight ggplot theme used by KM risk/event tables.
#'
#' @param base_size Numeric font size applied to most text elements.
#' @param base_family Character font family passed through for compatibility.
#'
#' @return A `ggplot2` theme object.
personalized_theme <- function(base_size = 8, base_family = "") {
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = base_size),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = base_size),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Add elements from theme_cleantable()
    plot.title = element_text(
      size = base_size,
      hjust = 0
    ),
    plot.subtitle = element_text(size = base_size, face = "italic", hjust = 0),
    plot.caption = element_text(size = base_size, face = "italic"),
    axis.title = element_text(size = base_size),
    axis.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical"
  )
}

#' Build Standardized KM Plot Object
#'
#' Wraps `survminer::ggsurvplot()` to create a standardized cumulative
#' incidence plot with aligned risk and cumulative-event tables.
#'
#' @param fit A fitted survival object, typically from `survival::survfit()`.
#' @param aesifup Analysis dataset used to fit `fit`.
#' @param figlab Figure label identifier.
#' @param title Optional submain title passed to plotting helper.
#' @param mask_table Logical; if `TRUE`, masks low counts in displayed tables.
#' @param cohort_name Character cohort name used in figure context.
#' @param exposed_name Character label for exposed group (currently unused).
#' @param max_fup Numeric maximum follow-up time displayed on x-axis.
#' @param day_ticks_x Numeric spacing for x-axis and table ticks.
#' @param ylab_root Character prefix used to construct y-axis label.
#' @param scale_IR Numeric scaling multiplier for cumulative incidence axis.
#' @param xlab Character x-axis label.
#' @param aesi_label Character AESI label (currently unused inside function body).
#' @param this_dap Character DAP (Data Access Provider) name for figure context.
#'
#' @return A `ggsurvplot` object with updated risk/cumevents table data.
#'
#' @details
#' This function requires `this_dap` to be passed as an explicit parameter.
#' It also relies on `getRiskTable()` for table correction.

kmplot_custom <- function(fit, # ggsurv fit object
                          aesifup = NULL,
                          figlab, # number.letter
                          title = NULL,
                          mask_table = FALSE,
                          cohort_name = NULL, # cohort name in code
                          exposed_name = "Ad26.COV2.S", # label of exposed in legend
                          max_fup, # maximum fup to show
                          day_ticks_x = 30, # . how many days should be one tick mark
                          ylab_root = "Cumulative Incidence (%) ", # y-axis label
                          scale_IR = 10000,
                          xlab = "Days since index date", # x-axis label
                          aesi_label, # name of the aesi in the title
                          this_dap # DAP name for figure context
) {
  # create cohort_figlabel from cohort_name input
  # if(cohort_name == "UNVAX") cohort_figlabel <- "Unvaccinated"
  # if(cohort_name == "MRNA") cohort_figlabel <- "COVID-19 mRNA vaccinees"
  if (is.null(cohort_name)) cohort_figlabel <- "main"

  # ---- figure options -----
  # display name of DAP (passed as parameter)
  if (this_dap == "SIDIAP") database_name <- "SIDIAP."
  if (this_dap == "PHARMO") database_name <- "PHARMO."
  if (this_dap %in% c("FISABIO", "VID")) database_name <- "VID."
  if (this_dap == "NHR") database_name <- "NHR."
  if (this_dap == "CDPR") database_name <- "CDPR."
  if (this_dap == "ALL") database_name <- "ALL."
  if (TRUE) database_name <- this_dap

  # create ylab from root and scaling
  ylab <- paste0(ylab_root, formatC(scale_IR, format = "d", big.mark = ","), " person-years")

  # # manipulate names of the groups for dislay purposes
  names(fit$strata)[names(fit$strata) == "group=CONTROL"] <- "Unexposed"
  names(fit$strata)[names(fit$strata) == "group=EXPOSED"] <- "Exposed"

  # Extract cumulative incidence values for time <= max_fup
  surv_data <- data.frame(time = fit$time, surv = fit$surv, lower = fit$lower)
  surv_data <- surv_data[surv_data$time <= max_fup, ] # Keep only risk window days

  # Compute maximum cumulative incidence (1 - survival probability)
  if (all(is.na(surv_data$lower))) {
    upper <- NULL # We'll skip ylim if it's NULL
  } else {
    upper <- max(1 - surv_data$lower, na.rm = TRUE) * scale_IR
  }

  # Define common plot options
  plot_args <- list(
    fit = fit,
    dat = aesifup,
    conf.int = TRUE,
    palette = c("dodgerblue3", "firebrick3"),
    fun = function(y) (scale_IR * (1 - y)),
    xlim = c(0, max_fup),
    ylab = ylab,
    xlab = xlab,
    submain = title,
    legend.title = " ",
    censor = FALSE,
    risk.table = TRUE,
    cumevents = TRUE,
    tables.height = 0.2,
    risk.table.y.text.col = FALSE,
    risk.table.y.text = TRUE,
    risk.table.font.y = 6,
    risk.table.title = "Number at risk",
    cumevents.y.text = TRUE,
    cumevents.y.text.col = FALSE,
    cumevents.font.y = 6,
    cumevents.title = "Cumulative number of events",
    tables.theme = personalized_theme(),
    font.submain = c(8),
    font.x = c(8),
    font.y = c(8),
    font.title = 10,
    fontsize = 2.5,
    font.tickslab = c(8, "plain")
  )

  # Only add ylim if 'upper' is defined
  if (!is.null(upper) && is.finite(upper)) {
    plot_args$ylim <- c(0, upper)
  }

  # Obtain figure correct adjusted n.risk and n.event
  plot_args$break.time.by <- 1
  figobj1 <- do.call(survminer::ggsurvplot, plot_args)

  # Obtain final figure with correct number of ticks
  plot_args$break.time.by <- day_ticks_x
  figobj <- do.call(survminer::ggsurvplot, plot_args)

  # Obtain fixed risk table
  risk_table <- getRiskTable(figobj = figobj1, max_fup = max_fup, day_ticks_x = day_ticks_x, digits = 0)

  # Add fixed risk table to output figure
  figobj$table$layers[[1]]$data <- risk_table
  risk_table$n.event <- risk_table
  figobj$cumevents$layers[[1]]$data <- risk_table

  # optionally, mask events table
  if (mask_table == TRUE) {
    figobj <- unserialize(serialize(figobj, NULL))
    figobj$cumevents$layers[[1]]$data$cum.n.event[figobj$cumevents$layers[[1]]$data$cum.n.event %in% 1:4] <- "<5"
    figobj$table$layers[[1]]$data$llabels[figobj$table$layers[[1]]$data$llabels %in% 1:4] <- "<5"
  }

  return(figobj)
}

#' Grid Draw Method for ggsurvplot
#'
#' Provides an S3 method that allows `grid::grid.draw()` to render
#' `ggsurvplot` objects without opening a new page.
#'
#' @param x A `ggsurvplot` object.
#'
#' @return Invisibly returns the drawn plot via `print.ggsurvplot`.
grid.draw.ggsurvplot <- function(x) {
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}

#' Clean AESI Label Text
#'
#' Normalizes AESI labels by removing asterisks, lowercasing, and restoring
#' selected proper names.
#'
#' @param label Character AESI label.
#'
#' @return A cleaned character string.
clean_aesi_label <- function(label) {
  # Remove asterisks
  label <- gsub("\\*", "", label)

  # Convert to lowercase
  label <- tolower(label)

  # Capitalize proper names manually
  proper_names <- c("Bell's palsy", "Guillain-Barre syndrome") # Add more names if needed
  for (name in proper_names) {
    label <- gsub(tolower(name), name, label, ignore.case = TRUE)
  }

  return(label)
}

#' Build Corrected Risk Table from ggsurvplot
#'
#' Reconstructs risk/event table values from the survfit-derived plotting data,
#' then filters to requested time ticks.
#'
#' @param figobj A `ggsurvplot` object with table and survival plot data.
#' @param max_fup Numeric maximum follow-up time for filtering displayed ticks.
#' @param day_ticks_x Numeric spacing between retained time ticks.
#' @param digits Integer number of digits used for rounding output values.
#'
#' @return A base `data.frame` containing corrected risk/event table values.
getRiskTable <- function(figobj, max_fup = NULL, day_ticks_x = 1, digits = 2) {
  # Replace bugged estimates by adjusted estimates
  risk_table <- dplyr::left_join(
    # Table
    figobj$table$data |>
      dplyr::mutate(time = as.integer(as.character(time))) |>
      dplyr::select(-c(n.risk, n.censor, n.event)),
    # Figure
    figobj$data.survplot |>
      dplyr::mutate(time = as.integer(as.character(time))) |>
      dplyr::select(strata, time, n.risk, n.censor, n.event),
    by = dplyr::join_by(strata == strata, time == time)
  ) |>
    # filter(strata == "Exposed") |>
    # By group
    dplyr::group_by(strata) |>
    # Fill n.censor and n.event at time point 0, n.risk as the max
    dplyr::mutate(
      n.risk = dplyr::if_else(time == 0 & is.na(n.risk), max(n.risk, na.rm = TRUE), n.risk),
      n.censor = dplyr::if_else(is.na(n.censor), 0, n.censor),
      n.event = dplyr::if_else(is.na(n.event), 0, n.event),
    ) |>
    dplyr::ungroup() |>
    # If gap in n.risk, then it has to be constant for that step
    tidyr::fill(n.risk, .direction = "up", .by = "strata") |>
    # By group
    dplyr::group_by(strata) |>
    # If any NAs remaining, they must be 0
    dplyr::mutate(
      n.risk = dplyr::if_else(is.na(n.risk), 0, n.risk)
    ) |>
    # Update labels
    dplyr::mutate(llabels = n.risk) |>
    # Recalculate the cumulative numbers with the correct estimates
    dplyr::mutate(
      cum.n.censor = round(cumsum(tidyr::replace_na(n.censor, 0)), digits),
      cum.n.event = round(cumsum(tidyr::replace_na(n.event, 0)), digits),
      n.risk = round(n.risk, digits),
      n.event = round(n.event, digits),
      n.censor = round(n.censor, digits),
      llabels = round(n.risk, digits),
      strata_size = max(n.risk),
      pct.risk = round(n.risk / strata_size * 100, digits)
    ) |>
    dplyr::ungroup() |>
    # Select only ticks to be included in the plot
    dplyr::filter(time %in% seq(0, max_fup, day_ticks_x)) |>
    dplyr::select(strata, time, n.risk, pct.risk, n.event, cum.n.event, n.censor, cum.n.censor, strata_size, llabels)

  return(as.data.frame(risk_table))
}

#' Align KM Plot and Table Elements
#'
#' Applies a patchwork-based layout workaround to align `ggsurvplot` main plot,
#' risk table, and cumulative-event table.
#'
#' @param figobj A `ggsurvplot` object.
#'
#' @return A combined `patchwork` plot object.
align_elements <- function(figobj) {
  # a weird issue with the latest survminer, that the plot and the table does not align well.
  # this is reported by many people, and using the package 'patchwork' is a easy pathcy solution.
  tab <- figobj$table + figobj$cumevents + patchwork::plot_layout(ncol = 1, nrow = 2)
  figobj <- figobj$plot + tab + patchwork::plot_layout(ncol = 1, nrow = 2)

  return(figobj)
}

#' Compute Monthly Event and At-Risk Counts
#'
#' Computes exposed/control counts for events and population at risk at month
#' `m`, using `fup_months` and `eventCount` in the input dataset.
#'
#' @param dt A `data.table` with at least `group`, `person_id`, `fup_months`,
#'   and `eventCount` columns.
#' @param m Integer month index.
#'
#' @return A named list with `n_out_exp`, `n_out_con`, `n_pat_exp`, and
#'   `n_pat_con`.
compute_monthly_counts <- function(dt, m) {
  dt_m <- copy(dt)

  # at-risk at start of month m
  dt_m[, at_risk := fup_months >= (m - 1)]

  # event by end of month m
  dt_m[, event_m := as.integer(eventCount == 1 & fup_months <= m)]

  list(
    n_out_exp = sum(dt_m[group == "EXPOSED", event_m], na.rm = TRUE),
    n_out_con = sum(dt_m[group == "CONTROL", event_m], na.rm = TRUE),
    n_pat_exp = uniqueN(dt_m[group == "EXPOSED" & at_risk == TRUE, person_id]),
    n_pat_con = uniqueN(dt_m[group == "CONTROL" & at_risk == TRUE, person_id])
  )
}
