#' Compute D3 Eligibility Flags
#'
#' Computes D3 eligibility indicators from multivariate episode rows using the
#' codebook definitions for `in_eligible_pop`, `eligible_exposed`, and
#' `eligible_control`.
#'
#' @param d3_multivariate_episodes A data.table containing one or more episode
#' rows per person with required CAPS columns.
#'
#' @return A data.table with all input columns plus:
#' `in_eligible_pop`, `RECEIVES_EXPOSURE`, `eligible_exposed`, and
#' `eligible_control`.
#'
#' @details
#' This function assumes eligibility input columns are provided with CAPS names:
#' `AGE_16_OR_OLDER`, `ENROLLED_6_MONTHS`, `STABLE_PRECONDITIONS`,
#' `SV_HOSP_6WEEKS`, `PREV_OTHER_COVID_VAC`, `SV_PRIOR_PREP_COVID`,
#' `SV_IMMUNOCOMPROMISED`, `PRIOR_PFIZER_VAC`, and `SV_VP_TARGET_COVID`.
#'
#' Eligibility is computed as:
#' `AGE_16_OR_OLDER & ENROLLED_6_MONTHS &`
#' `(STABLE_PRECONDITIONS == FALSE | (STABLE_PRECONDITIONS == TRUE & SV_HOSP_6WEEKS == FALSE)) &`
#' `!PREV_OTHER_COVID_VAC & !SV_PRIOR_PREP_COVID & !SV_IMMUNOCOMPROMISED & !PRIOR_PFIZER_VAC`.
#'
#' `eligible_exposed` is `in_eligible_pop & RECEIVES_EXPOSURE` and
#' `eligible_control` is `in_eligible_pop & !RECEIVES_EXPOSURE` where
#' `RECEIVES_EXPOSURE` is defined by `SV_VP_TARGET_COVID`.
#'
#' @export
compute_d3_eligibility <- function(d3_multivariate_episodes) {
  d3_multivariate_episodes[
    ,
    in_eligible_pop := AGE_16_OR_OLDER == TRUE &
      ENROLLED_6_MONTHS == TRUE &
      SV_HOSP_6WEEKS == FALSE &
      PREV_OTHER_COVID_VAC == FALSE &
      SV_PRIOR_PREP_COVID == FALSE &
      SV_IMMUNOCOMPROMISED == FALSE &
      PRIOR_PFIZER_VAC == FALSE
  ]

  d3_multivariate_episodes[, eligible_exposed := in_eligible_pop == TRUE & RECEIVES_EXPOSURE == TRUE]
  d3_multivariate_episodes[, eligible_control := in_eligible_pop == TRUE & RECEIVES_EXPOSURE == FALSE]

  # start_episode and end_episode have format date hour minute second,
  # but we only need date for the analysis, so we will convert to date format
  d3_multivariate_episodes[, start_episode := as.Date(start_episode)]
  d3_multivariate_episodes[, end_episode := as.Date(end_episode)]

  return(d3_multivariate_episodes)
}
