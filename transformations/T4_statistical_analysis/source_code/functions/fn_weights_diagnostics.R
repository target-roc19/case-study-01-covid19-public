#' Summarize IPTW distribution by treatment group
#'
#' Creates a compact table with central tendency, spread, and percentile-based
#' diagnostics for a stabilized weight column.
#'
#' @param p_matrix Data table/data frame containing group and weight columns.
#' @param groups Character vector with group labels to summarize.
#' @param weight_col Character scalar with weight column name.
#' @param custom_truncation Numeric length-2 vector with optional custom
#'   truncation cutoffs to print in the final row.
#' @param digits Integer number of decimals used in output rounding.
#'
#' @return Data frame with two value columns per group (V1/V2 style).
#'
#' @noRd
describe_iptw <- function(
  p_matrix,
  groups = c("CONTROL", "EXPOSED"),
  weight_col = "ipsmw_raw",
  custom_truncation = c(NA, NA),
  digits = 2
) {
  # Build group-level summaries and bind them side-by-side.
  for (gname in groups) {
    # Core moments and quantiles for the selected weight column.
    mean_weight <- mean(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)
    median_weight <- median(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)
    sd_weight <- sd(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)
    quants <- quantile(
      p_matrix[group == gname, get(weight_col)],
      c(custom_truncation[1], 0.25, 0.75, custom_truncation[2]),
      na.rm = TRUE
    )
    min_weight <- min(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)
    max_weight <- max(p_matrix[group == gname, get(weight_col)], na.rm = TRUE)

    # Assemble one temporary summary table per group.
    gtemp <- data.frame(
      labels = c(
        "Mean (SD)",
        "Q1-Q3",
        "Median",
        "Range, Min-Max",
        paste0("P",custom_truncation[1]*100,"-P",custom_truncation[2]*100),
        "If truncated, cut-offs"
      ),
      V1 = round(c(
        mean_weight,
        quants[2],
        median_weight,
        min_weight,
        quants[1],
        custom_truncation[1]
      ), digits),
      V2 = round(c(
        sd_weight,
        quants[3],
        NA,
        max_weight,
        quants[4],
        custom_truncation[2]
      ), digits)
    )

    # Rename value columns to keep group identity when cbind-ing.
    colnames(gtemp)[c(2, 3)] <- paste0(gname, c("_V1", "_V2"))

    if (gname == groups[1]) {
      gout <- gtemp
    } else {
      # Drop duplicate labels column for subsequent groups.
      gout <- cbind(gout, gtemp[, -1])
    }
  }

  return(gout)
}

#' Compute a weighted mean with missing-value handling
#'
#' @param x Numeric vector.
#' @param w Numeric weight vector.
#'
#' @return Weighted mean scalar.
#'
#' @noRd
weighted_mean_safe <- function(x, w) {
  # Only non-missing x contributes to denominator.
  sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}

#' Compute a weighted variance with safety checks
#'
#' @param x Numeric vector.
#' @param w Numeric weight vector.
#'
#' @return Weighted variance scalar or `NA_real_` when undefined.
#'
#' @noRd
weighted_var_safe <- function(x, w) {
  # Restrict both vectors to non-missing covariate values.
  w_use <- w[!is.na(x)]
  x_use <- x[!is.na(x)]
  # Variance requires at least two usable observations and positive weight sum.
  if (length(x_use) <= 1L || sum(w_use) <= 0) {
    return(NA_real_)
  }
  mu <- weighted_mean_safe(x_use, w_use)
  sum(w_use * (x_use - mu)^2) / sum(w_use)
}

#' Compute ASD for numeric covariates
#'
#' @param x_treated Numeric values in exposed group.
#' @param x_control Numeric values in control group.
#' @param w_treated Numeric weights for exposed group.
#' @param w_control Numeric weights for control group.
#'
#' @return Absolute standardized difference scalar.
#'
#' @noRd
compute_asd_numeric <- function(x_treated, x_control, w_treated, w_control) {
  # Group-specific weighted means/variances.
  mu_treated <- weighted_mean_safe(x_treated, w_treated)
  mu_control <- weighted_mean_safe(x_control, w_control)
  var_treated <- weighted_var_safe(x_treated, w_treated)
  var_control <- weighted_var_safe(x_control, w_control)
  # Pooled SD denominator used in ASD definition.
  pooled_sd <- sqrt((var_treated + var_control) / 2)
  if (!is.finite(pooled_sd) || pooled_sd <= 0) {
    return(NA_real_)
  }
  abs(mu_treated - mu_control) / pooled_sd
}

#' Compute ASD for categorical covariates
#'
#' Uses binary-form ASD for two levels and Mahalanobis-like distance for
#' multi-level categories.
#'
#' @param x_treated Categorical values in exposed group.
#' @param x_control Categorical values in control group.
#' @param w_treated Numeric weights for exposed group.
#' @param w_control Numeric weights for control group.
#'
#' @return Absolute standardized difference scalar.
#'
#' @noRd
compute_asd_categorical <- function(x_treated, x_control, w_treated, w_control) {
  # Build common level set across groups.
  levels_all <- sort(unique(c(as.character(x_treated), as.character(x_control))))
  levels_all <- levels_all[!is.na(levels_all)]
  if (length(levels_all) <= 1L) {
    return(NA_real_)
  }

  prop_vec <- function(x, w, levels_all) {
    # Convert to character to standardize comparisons.
    x_chr <- as.character(x)
    valid <- !is.na(x_chr)
    x_chr <- x_chr[valid]
    w <- w[valid]
    # No usable observations for this subgroup.
    if (length(x_chr) == 0L || sum(w) <= 0) {
      return(rep(NA_real_, length(levels_all)))
    }
    # Weighted proportion per level.
    vapply(levels_all, function(level_name) {
      sum(w[x_chr == level_name]) / sum(w)
    }, numeric(1))
  }

  p_treated <- prop_vec(x_treated, w_treated, levels_all)
  p_control <- prop_vec(x_control, w_control, levels_all)
  if (any(!is.finite(p_treated)) || any(!is.finite(p_control))) {
    return(NA_real_)
  }

  # Two-level closed-form ASD.
  if (length(levels_all) == 2L) {
    p1 <- p_treated[1]
    p0 <- p_control[1]
    denom <- sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
    if (!is.finite(denom) || denom <= 0) {
      return(NA_real_)
    }
    return(abs(p1 - p0) / denom)
  }

  # Multi-level ASD based on reduced probability vectors.
  p_treated_reduced <- p_treated[-length(p_treated)]
  p_control_reduced <- p_control[-length(p_control)]
  s_treated <- diag(p_treated_reduced) - tcrossprod(p_treated_reduced)
  s_control <- diag(p_control_reduced) - tcrossprod(p_control_reduced)
  # Average covariance; small ridge on diagonal for invertibility.
  sigma_mat <- (s_treated + s_control) / 2
  diag(sigma_mat) <- diag(sigma_mat) + 1e-08
  diff_vec <- p_treated_reduced - p_control_reduced

  as.numeric(sqrt(t(diff_vec) %*% solve(sigma_mat, diff_vec)))
}

#' Compute ASD for one covariate split by exposure group
#'
#' @param x Covariate vector.
#' @param group Group vector containing `CONTROL`/`EXPOSED`.
#' @param weights Optional numeric weight vector.
#'
#' @return Absolute standardized difference scalar.
#'
#' @noRd
compute_group_asd <- function(x, group, weights = NULL) {
  # Unweighted path defaults to all ones.
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }

  # Keep complete observations in the two-group comparison universe.
  keep <- !is.na(group) & !is.na(weights) & group %in% c("CONTROL", "EXPOSED")
  x <- x[keep]
  group <- group[keep]
  weights <- weights[keep]

  # Split by analysis group.
  x_treated <- x[group == "EXPOSED"]
  x_control <- x[group == "CONTROL"]
  w_treated <- weights[group == "EXPOSED"]
  w_control <- weights[group == "CONTROL"]

  if (length(x_treated) == 0L || length(x_control) == 0L) {
    return(NA_real_)
  }

  # Dispatch based on covariate type.
  if (is.numeric(x)) {
    return(compute_asd_numeric(x_treated, x_control, w_treated, w_control))
  }

  compute_asd_categorical(x_treated, x_control, w_treated, w_control)
}

#' Compute ASD table across all metadata covariates
#'
#' @param covariate_data Data with at least identifiers, group, and covariates.
#' @param metadata_files Character vector of metadata CSV files.
#' @param weight_data Optional table containing person-level weights.
#' @param weight_col Optional weight column name in `weight_data`.
#'
#' @return Data frame with `var`, `label`, `type`, and `asd_1`.
#'
#' @noRd
compute_covariate_asd_table <- function(
  covariate_data,
  metadata_files,
  weight_data = NULL,
  weight_col = NULL
) {
  # Normalize inputs.
  cov_df <- data.table::as.data.table(data.table::copy(covariate_data))
  metadata_tables <- lapply(metadata_files, function(file_path) {
    data.table::fread(file_path, sep = ";")
  })
  metadata_all <- unique(data.table::rbindlist(metadata_tables, use.names = TRUE, fill = TRUE))

  # Join weights when weighted ASD is requested.
  if (!is.null(weight_data) && !is.null(weight_col)) {
    w_df <- data.table::as.data.table(data.table::copy(weight_data))
    join_cols <- intersect(c("person_id", "T0", "group", "match_id"), intersect(names(cov_df), names(w_df)))
    cov_df <- base::merge(cov_df, w_df[, c(join_cols, weight_col), with = FALSE], by = join_cols, all.x = FALSE)
    # Default missing weights to 1 to avoid dropping rows silently.
    cov_df[is.na(get(weight_col)), (weight_col) := 1]
    weights <- cov_df[[weight_col]]
  } else {
    weights <- rep(1, nrow(cov_df))
  }

  # Iterate over metadata variable list and compute one ASD per covariate.
  result_list <- vector("list", nrow(metadata_all))
  for (i in seq_len(nrow(metadata_all))) {
    var_name <- as.character(metadata_all$var[i])
    # Skip metadata variables that are absent from current covariate data.
    if (!(var_name %in% names(cov_df))) {
      next
    }

    x <- cov_df[[var_name]]
    # Treat logical variables as categorical in ASD calculations.
    if (is.logical(x)) {
      x <- as.character(x)
    }

    result_list[[i]] <- data.frame(
      var = var_name,
      label = as.character(metadata_all$label[i]),
      type = as.character(metadata_all$type[i]),
      asd_1 = compute_group_asd(x = x, group = cov_df$group, weights = weights),
      stringsAsFactors = FALSE
    )
  }

  # Bind results and return empty schema if all variables were skipped.
  out <- do.call(rbind, Filter(Negate(is.null), result_list))
  if (is.null(out)) {
    return(data.frame(var = character(), label = character(), type = character(), asd_1 = numeric()))
  }
  rownames(out) <- NULL
  out
}

#' Collapse IPCW long table to one person-level weight row
#'
#' Selects the last time row within each person/T0/group(/match) stratum,
#' preserving final cumulative weights for weighted ASD calculations.
#'
#' @param ipcw_data IPCW long-format table.
#'
#' @return data.table with one row per person/T0/group(/match).
#'
#' @noRd
extract_person_level_ipcw_weights <- function(ipcw_data) {
  # Normalize and sort to make `.SD[.N]` deterministic.
  ipcw_dt <- data.table::as.data.table(data.table::copy(ipcw_data))
  order_cols <- intersect(c("person_id", "T0", "group", "match_id", "time_days"), names(ipcw_dt))
  if (length(order_cols) > 0) {
    data.table::setorderv(ipcw_dt, order_cols)
  }

  # Keep final row per subject stratum.
  by_cols <- intersect(c("person_id", "T0", "group", "match_id"), names(ipcw_dt))
  ipcw_dt[, .SD[.N], by = by_cols]
}

#' Build Table 4 rows with prevalence and OR (95% CI)
#'
#' @param variable_map Data table with variable metadata. Must include
#'   variable_id, variable_in_data, label, and source_name.
#' @param coef_table Data table with coefficient summaries from final model.
#'   Must include coefficient, estimate, std_error, and optionally exp_estimate.
#' @param source_lookup Named list mapping source_name values to data frames.
#' @param prevalence_digits Integer decimals for prevalence percentages.
#' @param or_digits Integer decimals for OR and CI rounding.
#'
#' @return Data table with one row per variable category:
#'   variable_id, variable_label, category_level, prevalence_n,
#'   prevalence_pct, OR, OR_ci_lower_95, OR_ci_upper_95.
#'
#' @noRd
build_table4_prevalence_or <- function(
  variable_map,
  coef_table,
  source_lookup,
  prevalence_digits = 1,
  or_digits = 2
) {
  var_map <- as.data.frame(variable_map)
  coef_df <- as.data.frame(coef_table)

  required_var_cols <- c("variable_id", "variable_in_data", "label", "source_name")
  missing_var_cols <- setdiff(required_var_cols, names(var_map))
  if (length(missing_var_cols) > 0) {
    stop(paste0(
      "variable_map is missing required column(s): ",
      paste(missing_var_cols, collapse = ", "),
      "."
    ))
  }

  required_coef_cols <- c("coefficient", "estimate", "std_error")
  missing_coef_cols <- setdiff(required_coef_cols, names(coef_df))
  if (length(missing_coef_cols) > 0) {
    stop(paste0(
      "coef_table is missing required column(s): ",
      paste(missing_coef_cols, collapse = ", "),
      "."
    ))
  }

  if (!"exp_estimate" %in% names(coef_df)) {
    coef_df$exp_estimate <- exp(coef_df$estimate)
  }

  coef_df$exp_estimate_lower <- exp(coef_df$estimate + qnorm(0.025) * coef_df$std_error)
  coef_df$exp_estimate_upper <- exp(coef_df$estimate + qnorm(0.975) * coef_df$std_error)
  coef_df$OR <- round(coef_df$exp_estimate, or_digits)
  coef_df$OR_ci_lower_95 <- round(coef_df$exp_estimate_lower, or_digits)
  coef_df$OR_ci_upper_95 <- round(coef_df$exp_estimate_upper, or_digits)

  var_map <- unique(var_map[, c("variable_id", "variable_in_data", "label", "source_name")])
  out_rows <- vector("list", nrow(var_map))

  for (i in seq_len(nrow(var_map))) {
    variable_id <- as.character(var_map$variable_id[i])
    variable_in_data <- as.character(var_map$variable_in_data[i])
    variable_label <- as.character(var_map$label[i])
    source_name <- as.character(var_map$source_name[i])

    source_dt <- source_lookup[[source_name]]
    if (is.null(source_dt)) {
      stop(paste0("Missing source data for source_name='", source_name, "'."))
    }

    source_df <- as.data.frame(source_dt)
    if (!(variable_in_data %in% names(source_df))) {
      stop(paste0(
        "Column '", variable_in_data,
        "' not found in source data '", source_name,
        "' for variable ", variable_id, "."
      ))
    }

    # Keep observed categories only; prevalence denominator is non-missing.
    values <- source_df[[variable_in_data]]
    values <- values[!is.na(values)]

    if (length(values) == 0L) {
      prevalence_dt <- data.frame(
        category_level = NA_character_,
        prevalence_n = NA_integer_,
        prevalence_pct = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      tab <- table(values)
      prevalence_dt <- data.frame(
        category_level = as.character(names(tab)),
        prevalence_n = as.integer(tab),
        stringsAsFactors = FALSE
      )
      prevalence_dt$prevalence_pct <- round(
        100 * prevalence_dt$prevalence_n / sum(prevalence_dt$prevalence_n),
        prevalence_digits
      )
      prevalence_dt <- prevalence_dt[, c("category_level", "prevalence_n", "prevalence_pct")]
    }

    coef_sub <- coef_df[startsWith(coef_df$coefficient, variable_in_data), , drop = FALSE]
    if (nrow(coef_sub) > 0) {
      prefix_len <- nchar(variable_in_data)
      coef_sub$category_level <- substring(coef_sub$coefficient, prefix_len + 1L)
      coef_sub <- coef_sub[, c("category_level", "OR", "OR_ci_lower_95", "OR_ci_upper_95")]
      prevalence_dt <- base::merge(
        x = prevalence_dt,
        y = coef_sub,
        by = "category_level",
        all.x = TRUE,
        sort = FALSE
      )
    } else {
      prevalence_dt$OR <- NA_real_
      prevalence_dt$OR_ci_lower_95 <- NA_real_
      prevalence_dt$OR_ci_upper_95 <- NA_real_
    }
    prevalence_dt$variable_id <- variable_id
    prevalence_dt$variable_label <- variable_label

    out_rows[[i]] <- prevalence_dt[, c(
      "variable_id",
      "variable_label",
      "category_level",
      "prevalence_n",
      "prevalence_pct",
      "OR",
      "OR_ci_lower_95",
      "OR_ci_upper_95"
    )]
  }

  if (length(out_rows) == 0L) {
    return(data.frame(
      variable_id = character(0),
      variable_label = character(0),
      category_level = character(0),
      prevalence_n = integer(0),
      prevalence_pct = numeric(0),
      OR = numeric(0),
      OR_ci_lower_95 = numeric(0),
      OR_ci_upper_95 = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, out_rows)
  rownames(out) <- NULL
  out
}

#' Build Figure 3 ASD comparison panel
#'
#' Creates a right-panel style ASD plot with before/after adjustment points,
#' sorted by largest pre-adjustment imbalance.
#'
#' @param pre_asd Data frame with pre-adjustment ASDs.
#' @param post_asd Data frame with post-adjustment ASDs.
#' @param top_n Integer number of top variables to display.
#'
#' @return ggplot object.
#'
#' @noRd
build_asd_comparison_plot <- function(pre_asd, post_asd, top_n = 15L) {
  # Normalize inputs to plain data frames for predictable manipulation.
  pre_df <- as.data.frame(pre_asd)
  post_df <- as.data.frame(post_asd)

  # Label rows by adjustment method and stack together.
  pre_df$method <- "Before adjustment"
  post_df$method <- "After adjustment"
  plot_df <- rbind(pre_df, post_df)
  # Keep finite ASD values only.
  plot_df <- plot_df[is.finite(plot_df$asd_1), , drop = FALSE]
  # Fallback to variable name when label is missing.
  plot_df$label <- ifelse(
    nzchar(plot_df$label),
    plot_df$label,
    plot_df$var
  )
  # Lightweight label cleanup and frailty annotation.
  plot_df$label <- trimws(plot_df$label)
  plot_df$label <- ifelse(startsWith(plot_df$var, "F_"), paste0(plot_df$label, " (frailty)"), plot_df$label)
  # Wrap long labels for plotting on the y-axis after coord_flip.
  plot_df$wrapped_label <- vapply(plot_df$label, function(x) paste(strwrap(x, width = 36), collapse = "\n"), character(1))

  # Determine plotting order by pre-adjustment ASD magnitude.
  ordered_labels <- unique(
    plot_df[plot_df$method == "Before adjustment", c("wrapped_label", "asd_1")]
    [order(-plot_df[plot_df$method == "Before adjustment", "asd_1"], plot_df[plot_df$method == "Before adjustment", "wrapped_label"]), "wrapped_label"]
  )
  # Keep top-N rows for readability.
  ordered_labels <- ordered_labels[seq_len(min(top_n, length(ordered_labels)))]
  plot_df <- plot_df[plot_df$wrapped_label %in% ordered_labels, , drop = FALSE]
  plot_df$wrapped_label <- factor(plot_df$wrapped_label, levels = rev(ordered_labels))
  plot_df$method <- factor(plot_df$method, levels = c("Before adjustment", "After adjustment"))

  # Build two-method ASD point plot with 0.1 threshold guide.
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = wrapped_label, y = asd_1, color = method, shape = method)
  ) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::geom_hline(yintercept = 0.1, color = "black", linewidth = 0.5, linetype = "dashed") +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = NULL,
      y = "Absolute standardized difference",
      color = NULL,
      shape = NULL,
      title = "Top Covariate Imbalance"
    ) +
    ggplot2::scale_shape_manual(values = c(1, 16)) +
    ggplot2::scale_color_manual(values = c("darkblue", "darkblue")) +
    ggplot2::theme(legend.position = "bottom")
}
