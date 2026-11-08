testthat::test_that("fit_ps_model normalizes convergence for glm and speedglm", {
  source("../transformations/T4_statistical_analysis/source_code/functions/CohortPropensity/fit_ps_model.R")

  test_data <- data.frame(
    y = c(0, 1, 0, 1),
    x = c(0, 0, 1, 1)
  )

  glm_fit <- fit_ps_model(
    predictors = c("x"),
    cohort_covariates = test_data,
    outcome_col = "y",
    use_speedglm = FALSE
  )

  speedglm_fit <- fit_ps_model(
    predictors = c("x"),
    cohort_covariates = test_data,
    outcome_col = "y",
    use_speedglm = TRUE
  )

  testthat::expect_true(isTRUE(glm_fit$converged))
  testthat::expect_true(isTRUE(speedglm_fit$converged))
  testthat::expect_true("converged" %in% names(glm_fit))
  testthat::expect_true("converged" %in% names(speedglm_fit))
})
