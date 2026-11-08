#' Generate Simulated Eligibility Data
#'
#' This function generates a synthetic `D3_Eligibility`-like dataset that follows
#' the variable names and eligibility rules described in the D3 codebook.
#'
#' @param n Integer. Number of rows to generate in the synthetic dataset. Default is 10,000.
#' @param start_seed Integer. Seed for reproducibility of random number generation. Default is 42.
#' @param save_output Logical. If TRUE, the generated dataset will be saved to the specified directory.
#' @param output_dir Character. Directory where the dataset should be saved if `save_output` is TRUE.
#' @param output_file Character. Name of the output file (without extension). Default is `"D3_Eligibility"`.
#'
#' @return A data.table containing a simulated `D3_Eligibility` dataset.
#'
#' @details
#' The output includes the eligibility variables from the codebook
#' (`AGE_16_OR_OLDER`, `ENROLLED_6_MONTHS`, `STABLE_PRECONDITIONS`,
#' `SV_HOSP_6WEEKS`, `PREV_OTHER_COVID_VAC`, `SV_PRIOR_PREP_COVID`,
#' `SV_IMMUNOCOMPROMISED`, `PRIOR_PFIZER_VAC`, `in_eligible_pop`, `RECEIVES_EXPOSURE`,
#' `eligible_exposed`, `eligible_control`) plus matching variables.
#' If `save_output` is TRUE, the generated dataset is saved in `.parquet`
#' format in `output_dir`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a synthetic dataset with default parameters
#' dataset <- generate_eligibility_data()
#'
#' # Generate and save a dataset
#' dataset <- generate_eligibility_data(
#'   n = 5000,
#'   start_seed = 123,
#'   save_output = TRUE,
#'   output_dir = "data/",
#'   output_file = "SyntheticEligibility"
#' )
#' }
simulate_d3_eligibility <- function(n = 10000, start_seed = 42, save_output, output_dir, output_file = "D3_Eligibility") {
  logr::log_print(paste0("[MATCHING] - Creating synthetic ", output_file))

  set.seed(start_seed)

  n_people <- max(ceiling(n / 4), 100)
  starts <- sample(seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by = "day"), n, replace = TRUE)

  D3_ELIGIBILITY <- data.table::data.table(
    person_id = sample(sprintf("Subject_%06d", seq_len(n_people)), n, replace = TRUE),
    start_episode = starts,
    AGE_16_OR_OLDER = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.88, 0.12)),
    ENROLLED_6_MONTHS = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.92, 0.08)),
    STABLE_PRECONDITIONS = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.35, 0.65)),
    SV_HOSP_6WEEKS = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.10, 0.90)),
    PREV_OTHER_COVID_VAC = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.08, 0.92)),
    SV_PRIOR_PREP_COVID = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.05, 0.95)),
    SV_IMMUNOCOMPROMISED = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.07, 0.93)),
    PRIOR_PFIZER_VAC = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.20, 0.80)),
    RECEIVES_EXPOSURE = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.20, 0.80)),
    year_of_birth = sample(1920:2015, n, replace = TRUE),
    SV_SEX = sample(c("female", "male"), n, replace = TRUE, prob = c(0.52, 0.48)),
    SV_REGION = sample(c("north", "south", "east", "west"), n, replace = TRUE),
    SV_SES = sample(c("low", "middle", "high"), n, replace = TRUE, prob = c(0.30, 0.50, 0.20)),
    SV_PREG_STATUS = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.04, 0.96)),
    SV_CDC_RISK = sample(0:2, n, replace = TRUE, prob = c(0.50, 0.32, 0.18)),
    SV_HU_CONTACT = sample(0:6, n, replace = TRUE),
    SV_SMOKING = sample(0:3, n, replace = TRUE),
    SV_CCI = sample(0:2, n, replace = TRUE, prob = c(0.50, 0.32, 0.18))
  )

  # Apply the D3 codebook eligibility logic exactly.
  D3_ELIGIBILITY[, in_eligible_pop := AGE_16_OR_OLDER &
    ENROLLED_6_MONTHS &
    (STABLE_PRECONDITIONS == FALSE | (STABLE_PRECONDITIONS == TRUE & SV_HOSP_6WEEKS == FALSE)) &
    !PREV_OTHER_COVID_VAC &
    !SV_PRIOR_PREP_COVID &
    !SV_IMMUNOCOMPROMISED &
    !PRIOR_PFIZER_VAC]

  D3_ELIGIBILITY[, `:=`(
    eligible_exposed = in_eligible_pop & RECEIVES_EXPOSURE,
    eligible_control = in_eligible_pop & !RECEIVES_EXPOSURE
  )]

  # Exposed units have end_episode == start_episode, otherwise draw finite control spells.
  D3_ELIGIBILITY[, end_episode := as.Date(
    ifelse(RECEIVES_EXPOSURE, start_episode, start_episode + sample(7:90, .N, replace = TRUE))
  )]

  data.table::setcolorder(D3_ELIGIBILITY, c(
    "person_id",
    "start_episode",
    "end_episode",
    "AGE_16_OR_OLDER",
    "ENROLLED_6_MONTHS",
    "STABLE_PRECONDITIONS",
    "SV_HOSP_6WEEKS",
    "PREV_OTHER_COVID_VAC",
    "SV_PRIOR_PREP_COVID",
    "SV_IMMUNOCOMPROMISED",
    "PRIOR_PFIZER_VAC",
    "in_eligible_pop",
    "RECEIVES_EXPOSURE",
    "eligible_exposed",
    "eligible_control",
    "year_of_birth",
    "SV_SEX",
    "SV_REGION",
    "SV_SES",
    "SV_PREG_STATUS",
    "SV_CDC_RISK",
    "SV_HU_CONTACT",
    "SV_SMOKING",
    "SV_CCI"
  ))

  logr::log_print(paste0("[MATCHING] - ", output_file, " created successfully"))

  if (save_output) {
    logr::log_print(paste0("[MATCHING] - Saving simulated data to ", output_dir, "/", output_file, ".parquet"))
    arrow::write_parquet(D3_ELIGIBILITY, paste0(file.path(output_dir, output_file), ".parquet"))
    logr::log_print(paste0("[MATCHING] - ", output_file, " saved successfully"))
  }

  return(D3_ELIGIBILITY)
}
