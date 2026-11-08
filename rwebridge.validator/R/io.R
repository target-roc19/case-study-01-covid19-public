check_file_exists <- function(filepath) {
  !is.null(filepath) && file.exists(filepath) && file.size(filepath) > 0
}

safe_read_file <- function(filepath) {
  if (is.null(filepath)) {
    return(NULL)
  }

  tryCatch(
    {
      ext <- tolower(tools::file_ext(filepath))
      if (ext == "csv") {
        dt <- data.table::fread(filepath)
      } else if (ext == "rds") {
        obj <- readRDS(filepath)
        if (!is.data.frame(obj)) {
          return(NULL)
        }
        dt <- data.table::as.data.table(obj)
      } else {
        return(NULL)
      }

      if (nrow(dt) == 0) {
        return(NULL)
      }

      is_missing_cell <- function(value) {
        is.null(value) || length(value) == 0 || all(is.na(value))
      }
      empty_rows <- vapply(
        seq_len(nrow(dt)),
        function(row_index) {
          all(vapply(dt, function(column) is_missing_cell(column[[row_index]]), logical(1)))
        },
        logical(1)
      )
      dt <- dt[!empty_rows]
      if (nrow(dt) == 0) {
        return(NULL)
      }
      dt
    },
    error = function(e) {
      NULL
    }
  )
}

find_file_with_extension <- function(data_dir, base_name) {
  if (is.null(base_name)) {
    return(NULL)
  }

  if (file.exists(base_name)) {
    return(base_name)
  }

  csv_path <- file.path(data_dir, paste0(base_name, ".csv"))
  rds_path <- file.path(data_dir, paste0(base_name, ".rds"))

  if (check_file_exists(csv_path)) {
    return(csv_path)
  }
  if (check_file_exists(rds_path)) {
    return(rds_path)
  }

  NULL
}
