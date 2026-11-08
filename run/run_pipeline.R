#!/usr/bin/env Rscript

# ------------------------------------------------------------
# VAC4EU Semantic Harmonization Pipeline Runner (Master)
# Orchestrates all phases using the picard::pipeline class.
# ------------------------------------------------------------
if (TRUE) {
  if (requireNamespace("renv", quietly = TRUE)) {
    renv::activate()
    status <- renv::status()
    if (!isTRUE(status$synchronized)) {
      message("Restoring renv environment...")
      renv::restore(prompt = FALSE)
    } else {
      message("renv environment is up to date.")
    }
  }
} else {
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }
  pak::pkg_install("github::UMC-Utrecht-RWE/picard@v1.2.4")
  pak::pkg_install("github::UMC-Utrecht-RWE/T2-DataManagementModel@0.2.1")
  pak::pkg_install("github::UMC-Utrecht-RWE/CohortBuilder@v0.1.5")
  pak::pkg_install("github::UMC-Utrecht-RWE/episodeR@v0.0.3")
  pak::pkg_install("github::UMC-Utrecht-RWE/DescriptiveTable@v0.3.0")
  pak::pkg_install("github::UMC-Utrecht-RWE/anchoR@v1.4.1.1")
  pak::pkg_install("github::UMC-Utrecht-RWE/CohortAnalytics@v0.0.2")
}

# --- Setup the environment ---
lm <- picard:::LoggerManager$new()
picard::track_file_changes(path = "logs") # nolint
lm$configure(verbose = "Normal") # Low or High
logger::log_info("Loading master pipeline")

# ---- Instantiate master pipeline ----
logger::log_debug("Instantiating master pipeline class")
pipeline <- picard::pipeline$new(
  config_pipeline = "configuration/config_pipeline.yaml"
)

###############################################

dir.create(file.path(tempdir(), "duckdb", "temp"), recursive = T)

picard::load_config()
logger::log_debug("Running master pipeline")
pipeline$run()
