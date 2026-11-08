# ---------------------------------------------------------------------------
# fn_analytic_lookup2.R
# Minimal helpers for create_cohort_analytic_lookup2
# ---------------------------------------------------------------------------

#' Create Summary Statistics Tables
#'
#' Uses metadata to select and export table columns from a wide input table.
#' Supports individual table exports or one combined file per table group.
#'
#' @param metadata A `data.frame`/`data.table` describing which columns to
#'   include by table group. Must include `varnam_table_group`.
#' @param varnam_table_group Character column name identifying table groups.
#' @param input A wide `data.frame`/`data.table` containing summary statistics.
#' @param varnam_aesi Character column name containing AESI/outcome labels.
#' @param varnam_table_id Character column name containing final table IDs.
#' @param varnam_description Character column name containing row descriptions.
#' @param output_dir Character directory where CSV files are written.
#' @param dap_name Character prefix used in output filenames.
#' @param output_mode Character; either `"individual"` (one file per table id)
#'   or `"combined"` (one file per group).
#' @param table_order Optional character vector specifying desired table-id order.
#' @param nsmall Optional integer decimal precision for formatting.
#' @param rounding_cols Optional character vector of numeric columns to round and
#'   format when `nsmall` is supplied.
#' @param naming_suffix Optional character suffix appended to output filenames.
#' @param ... Additional arguments reserved for forward compatibility.
#'
#' @return Invisibly returns `NULL`; writes one or more CSV files to
#'   `output_dir`.
create_summarystats_table <- function(metadata,
                                      varnam_table_group = "tab_group",
                                      input,
                                      varnam_aesi = "aesi",
                                      varnam_table_id = "tab_id",
                                      varnam_description = "description",
                                      output_dir,
                                      dap_name,
                                      output_mode = c("individual", "combined"),
                                      table_order = NULL,
                                      nsmall = NULL,
                                      rounding_cols = NULL,
                                      naming_suffix = "",
                                      ...) {
  work_input <- data.table::copy(input)
  metadata <- data.table::setDT(metadata)
  work_input <- data.table::setDT(work_input)
  output_mode <- match.arg(output_mode)

  if (!(varnam_description %in% names(work_input)) && "title" %in% names(work_input)) {
    work_input[, (varnam_description) := title]
  }

  stopifnot(
    "No table-group column found in input." = varnam_table_group %in% colnames(work_input)
  )
  stopifnot(
    "No description column found in input." = varnam_description %in% colnames(work_input)
  )

  if (!is.null(nsmall)) {
    stopifnot("Provide rounding_cols when nsmall is set." = !is.null(rounding_cols))
    work_input[, (rounding_cols) := lapply(.SD, round, digits = nsmall), .SDcols = rounding_cols]
    work_input[, (rounding_cols) := lapply(.SD, format, nsmall = nsmall), .SDcols = rounding_cols]
  }

  work_input <- data.frame(work_input)
  work_input[work_input == "-77.00"] <- "NR"
  work_input[work_input == " -77.00"] <- "NR"
  work_input <- data.table::as.data.table(work_input)

  for (this_grouping in unique(metadata[[varnam_table_group]])) {
    this_lookup <- metadata[get(varnam_table_group) == this_grouping, ]
    this_lookup[, (varnam_table_group) := NULL]

    these_summaries <- c(
      varnam_description,
      varnam_table_id,
      varnam_aesi,
      names(this_lookup[, which(colSums(this_lookup) > 0), with = FALSE])
    )
    these_summaries <- unique(these_summaries)

    df_table_group <- work_input[get(varnam_table_group) == this_grouping, these_summaries, with = FALSE]
    col_order <- c(varnam_description, setdiff(names(df_table_group), varnam_description))
    data.table::setcolorder(df_table_group, col_order)

    if (!is.null(table_order)) {
      group_order <- table_order[table_order %in% unique(df_table_group[[varnam_table_id]])]
      remaining_ids <- setdiff(unique(df_table_group[[varnam_table_id]]), group_order)
      ordered_ids <- c(group_order, remaining_ids)
    } else {
      ordered_ids <- unique(df_table_group[[varnam_table_id]])
    }

    if (nrow(df_table_group) == 0) {
      logger::log_info(paste0("No cohort input for ", this_grouping, naming_suffix, "."))
      print(paste0("No cohort input for ", this_grouping, naming_suffix, "."))
    } else {
      if (output_mode == "individual") {
        for (this_final_table in ordered_ids) {
          data.table::fwrite(
            df_table_group[get(varnam_table_id) == this_final_table],
            file = file.path(output_dir, paste0(dap_name, "_Table", this_final_table, naming_suffix, ".csv"))
          )
        }
      } else {
        df_table_group[, "table_export_order" := match(get(varnam_table_id), ordered_ids)]
        data.table::setorder(df_table_group, "table_export_order")
        df_table_group[, "table_export_order" := NULL]
        data.table::fwrite(
          df_table_group,
          file = file.path(output_dir, paste0(dap_name, "_Table", this_grouping, naming_suffix, ".csv"))
        )
      }
      logger::log_info(paste0("Table group ", this_grouping, naming_suffix, " created."))
      print(paste0("Table group ", this_grouping, naming_suffix, " created."))
    }
  }
}
