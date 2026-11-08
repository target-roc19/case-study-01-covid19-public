# --- Setup the environment ---
lm$start_step_logger("T4")
logger::log_info("Loading T4 pipeline")

# ---- Instantiating T4 classs ----
logger::log_debug("Instantiating T4 pipeline class")
pipeline <- picard::t4_pipeline$new(
  config_t4 = "configuration/config_T4.yaml"
)

# ---- Run the T4 pipeline ----
logger::log_debug("Running T4 pipeline")
pipeline$run()
lm$end_step_logger()
