# --- Setup the environment ---
lm$start_step_logger("T2")
logger::log_info("Loading T2 pipeline")

# ---- Instantiating T2 classs ----
logger::log_debug("Instantiating T2 pipeline class")
pipeline <- picard::t2_pipeline$new(
  config_t2 = "configuration/config_T2.yaml"
)

# ---- Run the T2 pipeline ----
logger::log_debug("Running T2 pipeline")
pipeline$run()

lm$end_step_logger()
