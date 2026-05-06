testthat::test_that("`summary.collinear_selection()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors, package = "spatialData")
  vi_predictors_numeric <- identify_numeric_variables(
    df = vi_smol,
    predictors = vi_predictors
  )$valid

  #normal usage
  x <- collinear(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_output(
    z <- summary(x),
    regexp = "response"
  )

  testthat::expect_true(
    all(x$selection %in% z)
  )
})
