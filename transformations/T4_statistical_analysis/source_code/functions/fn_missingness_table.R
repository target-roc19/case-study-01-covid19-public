#' Create Missingness Assessment Table
#'
#' Generate a comprehensive missingness table showing patterns and observed distributions
#' for both categorical and numeric variables across overall, exposed, and control groups.
#'
#' @param data Data frame containing the variables to assess
#' @param vars Character vector of variable names to assess
#' @param metadata Data frame containing variable metadata with columns: variable_id, data_type
#' @param missing_values Vector of values to treat as missing (default: NA, "NA", 0, "")
#' @param group_col Name of the grouping column (e.g., "group")
#' @param exposed_val Value indicating exposed group (e.g., "EXPOSED")
#' @param control_val Value indicating control group (e.g., "CONTROL")
#'
#' @return Data frame with missingness assessment table
#'
#' @import data.table
#' @import glue
#'
create_missingness_table <- function(
  data,
  vars,
  metadata,
  missing_values = c(NA, "NA", 0, ""),
  group_col = "group",
  exposed_val = "EXPOSED",
  control_val = "CONTROL"
) {
  # Convert to data.table if needed
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Ensure metadata is a data.table
  if (!data.table::is.data.table(metadata)) {
    metadata <- data.table::as.data.table(metadata)
  }

  # Initialize results list
  results_list <- list()

  # Process each variable
  for (var_name in vars) {
    # Check if variable exists in data
    if (!var_name %in% names(data)) {
      logger::log_warn(glue::glue("Variable {var_name} not found in data. Skipping."))
      next
    }

    # Get variable info from metadata
    var_meta <- metadata[variable_id == var_name]
    if (nrow(var_meta) == 0) {
      logger::log_warn(glue::glue("Variable {var_name} not found in metadata. Skipping."))
      next
    }

    var_type <- var_meta$data_type[1]
    var_label <- var_name # Use variable_id as label

    # Extract data for this variable and group
    var_data <- data[, list(
      value = get(var_name),
      group = get(group_col)
    ), by = seq_len(nrow(data))]
    var_data[, seq_len := NULL]

    # Identify missing values
    var_data[, is_missing := value %in% missing_values | is.na(value)]

    # Calculate overall statistics
    overall_total <- nrow(var_data)
    overall_missing <- sum(var_data$is_missing)
    overall_pct_missing <- round(100 * overall_missing / overall_total, 1)

    # Calculate by group
    by_group_stats <- var_data[, list(
      n_missing = sum(is_missing),
      n_total = .N,
      pct_missing = round(100 * sum(is_missing) / .N, 1)
    ), by = group]

    exposed_stat <- by_group_stats[group == exposed_val]
    control_stat <- by_group_stats[group == control_val]

    exposed_missing <- ifelse(nrow(exposed_stat) > 0, exposed_stat$n_missing[1], 0)
    exposed_total <- ifelse(nrow(exposed_stat) > 0, exposed_stat$n_total[1], 0)
    exposed_pct <- ifelse(nrow(exposed_stat) > 0, exposed_stat$pct_missing[1], NA)

    control_missing <- ifelse(nrow(control_stat) > 0, control_stat$n_missing[1], 0)
    control_total <- ifelse(nrow(control_stat) > 0, control_stat$n_total[1], 0)
    control_pct <- ifelse(nrow(control_stat) > 0, control_stat$pct_missing[1], NA)

    # Add header row for the variable with missingness statistics
    results_list[[length(results_list) + 1]] <- data.table::data.table(
      characteristic = var_label,
      overall_combined = glue::glue("{overall_missing}/{overall_total} ({overall_pct_missing}%)"),
      exposed_combined = glue::glue("{exposed_missing}/{exposed_total} ({exposed_pct}%)"),
      control_combined = glue::glue("{control_missing}/{control_total} ({control_pct}%)"),
      imputed_dist_overall = "",
      imputed_dist_exposed = "",
      imputed_dist_control = "",
      imputed_range_overall = "",
      imputed_range_exposed = "",
      imputed_range_control = "",
      var_type = var_type
    )

    # Filter to non-missing values for distribution calculations
    var_data_complete <- var_data[is_missing == FALSE]

    # Determine if variable is numeric or categorical based on data_type
    is_numeric <- var_type %in% c("INT", "NUM", "FLOAT", "DOUBLE")
    is_categorical <- var_type %in% c("BOOL", "CAT", "CHAR")

    if (is_categorical) {
      # Categorical or Boolean variables - show distribution by category

      if (nrow(var_data_complete) > 0) {
        categories <- unique(var_data_complete$value)
        categories <- categories[!is.na(categories) & categories != ""]
        categories <- sort(as.character(categories))

        for (cat in categories) {
          # Overall distribution
          overall_cat_n <- sum(var_data_complete$value == cat)
          overall_cat_pct <- round(100 * overall_cat_n / nrow(var_data_complete), 1)
          overall_dist <- glue::glue("{overall_cat_n} ({overall_cat_pct}%)")

          # Exposed distribution
          exposed_data <- var_data_complete[group == exposed_val]
          exposed_cat_n <- sum(exposed_data$value == cat)
          exposed_cat_pct <- ifelse(nrow(exposed_data) > 0,
            round(100 * exposed_cat_n / nrow(exposed_data), 1),
            NA
          )
          exposed_dist <- ifelse(nrow(exposed_data) > 0,
            glue::glue("{exposed_cat_n} ({exposed_cat_pct}%)"),
            ""
          )

          # Control distribution
          control_data <- var_data_complete[group == control_val]
          control_cat_n <- sum(control_data$value == cat)
          control_cat_pct <- ifelse(nrow(control_data) > 0,
            round(100 * control_cat_n / nrow(control_data), 1),
            NA
          )
          control_dist <- ifelse(nrow(control_data) > 0,
            glue::glue("{control_cat_n} ({control_cat_pct}%)"),
            ""
          )

          # Add category row
          results_list[[length(results_list) + 1]] <- data.table::data.table(
            characteristic = glue::glue("   {var_label} = {cat}"),
            overall_combined = overall_dist,
            exposed_combined = exposed_dist,
            control_combined = control_dist,
            imputed_dist_overall = "",
            imputed_dist_exposed = "",
            imputed_dist_control = "",
            imputed_range_overall = "",
            imputed_range_exposed = "",
            imputed_range_control = "",
            var_type = var_type
          )
        }
      }
    } else if (is_numeric) {
      # Numeric variables - show IQR and range

      if (nrow(var_data_complete) > 0) {
        # Overall statistics
        overall_q1 <- quantile(var_data_complete$value, 0.25, na.rm = TRUE)
        overall_q3 <- quantile(var_data_complete$value, 0.75, na.rm = TRUE)
        overall_median <- quantile(var_data_complete$value, 0.5, na.rm = TRUE)
        overall_min <- min(var_data_complete$value, na.rm = TRUE)
        overall_max <- max(var_data_complete$value, na.rm = TRUE)
        overall_dist <- glue::glue("{overall_median} ({overall_q1}, {overall_q3})")
        overall_range <- glue::glue("({overall_min}, {overall_max})")

        # Exposed statistics
        exposed_data <- var_data_complete[group == exposed_val]
        if (nrow(exposed_data) > 0) {
          exposed_q1 <- quantile(exposed_data$value, 0.25, na.rm = TRUE)
          exposed_q3 <- quantile(exposed_data$value, 0.75, na.rm = TRUE)
          exposed_median <- quantile(exposed_data$value, 0.5, na.rm = TRUE)
          exposed_min <- min(exposed_data$value, na.rm = TRUE)
          exposed_max <- max(exposed_data$value, na.rm = TRUE)
          exposed_dist <- glue::glue("{exposed_median} ({exposed_q1}, {exposed_q3})")
          exposed_range <- glue::glue("({exposed_min}, {exposed_max})")
        } else {
          exposed_dist <- ""
          exposed_range <- ""
        }

        # Control statistics
        control_data <- var_data_complete[group == control_val]
        if (nrow(control_data) > 0) {
          control_q1 <- quantile(control_data$value, 0.25, na.rm = TRUE)
          control_q3 <- quantile(control_data$value, 0.75, na.rm = TRUE)
          control_median <- quantile(control_data$value, 0.5, na.rm = TRUE)
          control_min <- min(control_data$value, na.rm = TRUE)
          control_max <- max(control_data$value, na.rm = TRUE)
          control_dist <- glue::glue("{control_median} ({control_q1}, {control_q3})")
          control_range <- glue::glue("({control_min}, {control_max})")
        } else {
          control_dist <- ""
          control_range <- ""
        }

        # Update header row with numeric distributions
        results_list[[length(results_list)]]$overall_combined <- glue::glue("{overall_missing}/{overall_total} ({overall_pct_missing}%), {overall_dist}")
        results_list[[length(results_list)]]$exposed_combined <- glue::glue("{exposed_missing}/{exposed_total} ({exposed_pct}%), {exposed_dist}")
        results_list[[length(results_list)]]$control_combined <- glue::glue("{control_missing}/{control_total} ({control_pct}%), {control_dist}")

        # Add range row for numeric variables
        results_list[[length(results_list) + 1]] <- data.table::data.table(
          characteristic = glue::glue("   Range (min, max)"),
          overall_combined = overall_range,
          exposed_combined = exposed_range,
          control_combined = control_range,
          imputed_dist_overall = "",
          imputed_dist_exposed = "",
          imputed_dist_control = "",
          imputed_range_overall = "",
          imputed_range_exposed = "",
          imputed_range_control = "",
          var_type = var_type
        )
      }
    }
  }

  # Combine all results
  if (length(results_list) == 0) {
    logger::log_warn("No results to return. Check variable names and metadata.")
    return(data.table::data.table())
  }

  # Ensure all columns are character type before combining
  results_list <- lapply(results_list, function(x) {
    x[, `:=`(
      characteristic = as.character(characteristic),
      overall_combined = as.character(overall_combined),
      exposed_combined = as.character(exposed_combined),
      control_combined = as.character(control_combined),
      imputed_dist_overall = as.character(imputed_dist_overall),
      imputed_dist_exposed = as.character(imputed_dist_exposed),
      imputed_dist_control = as.character(imputed_dist_control),
      imputed_range_overall = as.character(imputed_range_overall),
      imputed_range_exposed = as.character(imputed_range_exposed),
      imputed_range_control = as.character(imputed_range_control),
      var_type = as.character(var_type)
    )]
    return(x)
  })

  results_dt <- data.table::rbindlist(results_list, fill = TRUE, use.names = TRUE)

  # Select final columns and remove var_type
  results_dt <- results_dt[, list(
    Characteristic = characteristic,
    `Overall (N missing/N total, observed)` = overall_combined,
    `Exposed (N missing/N total, observed)` = exposed_combined,
    `Control (N missing/N total, observed)` = control_combined,
    `Imputed distribution overall` = imputed_dist_overall,
    `Imputed distribution exposed` = imputed_dist_exposed,
    `Imputed distribution control` = imputed_dist_control,
    `Range (min, max) imputed values overall` = imputed_range_overall,
    `Range (min, max) imputed values exposed` = imputed_range_exposed,
    `Range (min, max) imputed values control` = imputed_range_control
  )]

  return(results_dt)
}
