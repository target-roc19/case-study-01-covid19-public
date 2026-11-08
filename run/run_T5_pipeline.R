# --- Setup the environment ---
lm$start_step_logger("T5")
logger::log_info("Loading T5 pipeline")

# ---- Instantiating T5 classs ----
logger::log_debug("Instantiating T5 pipeline class")
pipeline <- picard::t5_pipeline$new(
  config_t5 = "configuration/config_T5.yaml"
)

# ---- Run the T5 pipeline ----
logger::log_debug("Running T5 pipeline")
pipeline$run()
lm$end_step_logger()
