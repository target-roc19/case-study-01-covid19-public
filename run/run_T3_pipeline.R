# --- Setup the environment ---
lm$start_step_logger("T3")
logger::log_info("Loading T3 pipeline")

# ---- Instantiating T3 classs ----
logger::log_debug("Instantiating T3 pipeline class")
pipeline <- picard::t3_pipeline$new(
  config_t3 = "configuration/config_T3.yaml"
)

# ---- Run the T3 pipeline ----
logger::log_debug("Running T3 pipeline")
pipeline$run()
lm$end_step_logger()
