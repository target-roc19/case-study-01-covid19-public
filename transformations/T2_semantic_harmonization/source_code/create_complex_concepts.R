logger::log_info("[Complex Concepts] - STARTING")

### Initial setup
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("create_complex_concepts.R")
  lm$start_capturing_prints()
}

picard::load_config()

# Loads functions needed ------------------
files <- list.files(
  config_project$create_complex_concepts$functions,
  pattern = "\\.[Rr]$",
  full.names = TRUE
)
invisible(lapply(files, source))

# prepare directories ------------------
input_dir <- config_project$create_complex_concepts$input_dir
output_dir <- config_project$create_complex_concepts$output_dir

# prepare metadata ------------------
metadata <- picard::load(
  file_path = config_pipeline$common$dir_bridge,
  file_name = "concept_preprocessing.csv"
)

# creating a list of lists, grouped by concept_id and function
# where each inner list contains the
# necessary information to create a complex concept the reason for this is that
# we want to be able to easily iterate over this list
# and call the appropriate function for each complex concept
metadata_list <- metadata[
  `function` != "",
  .(
    complex_concept_name = data.table::first(concept_id),
    sub_concept_ids = list(sub_concept_id),
    func_ = data.table::first(`function`)
  ),
  by = .(concept_id, `function`)
] |>
  base::split(by = c("concept_id", "function"))

# Optional: set a concept name to resume processing from that point.
# If empty, processing starts from the beginning.
start_complex_concept_name <- ""
if (!base::is.null(config_project$create_complex_concepts$start_complex_concept_name)) {
  start_complex_concept_name <- config_project$create_complex_concepts$start_complex_concept_name
}

if (identical(start_complex_concept_name, "")) {
  start_index <- 1L
} else {
  concept_names <- vapply(
    metadata_list,
    function(x) as.character(x$complex_concept_name),
    character(1)
  )
  matched_index <- which(concept_names == as.character(start_complex_concept_name))

  if (length(matched_index) == 0L) {
    stop(
      paste0(
        "start_complex_concept_name '",
        start_complex_concept_name,
        "' was not found in metadata_list."
      )
    )
  }

  start_index <- matched_index[1]
  logger::log_info(paste0(
    "Resuming from complex concept: ",
    start_complex_concept_name,
    " (index ",
    start_index,
    "/",
    length(metadata_list),
    ")"
  ))
}

# orchestrator for creating complex concepts
for (i in seq.int(from = start_index, to = length(metadata_list))) {
  concept_id <- metadata_list[[i]]$complex_concept_name
  func_ <- metadata_list[[i]]$func_
  if (exists(func_, mode = "function")) {
    logger::log_info(paste0(
      "Processing concept: ",
      concept_id,
      " with function: ",
      func_
    ))
    tryCatch(
      {
        do.call(
          what = get(func_, mode = "function"),
          args = list(
            input_dir = input_dir,
            output_dir = output_dir,
            complex_concept_name = metadata_list[[i]]$complex_concept_name,
            sub_concept_ids = unlist(metadata_list[[i]]$sub_concept_ids)
          )
        )
      },
      error = function(e) {
        stop(e)
      },
      warning = function(w) {
        logger::log_warn(paste0(
          "Warning while processing concept: ",
          concept_id,
          " with function: ",
          func_,
          " - ",
          w
        ))
      }
    )
  } else {
    logger::log_info(
      paste0("Function ", func_, " does not exist. Skipping.")
    )
  }
}

# clean up memory
rm(metadata, metadata_list)
gc()
if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
