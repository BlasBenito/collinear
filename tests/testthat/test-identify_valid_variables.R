testthat::test_that("`identify_valid_variables()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors)

  x <- identify_valid_variables(
    df = vi_smol,
    predictors = vi_predictors
  )

  testthat::expect_true(
    all(x$numeric %in% vi_predictors_numeric)
  )

  testthat::expect_true(
    all(x$categorical %in% vi_predictors_categorical)
  )

  x <- identify_valid_variables(
    df = vi_smol,
    responses = vi_predictors
  )

  testthat::expect_true(
    all(x$numeric %in% vi_predictors_numeric)
  )

  testthat::expect_true(
    all(x$categorical %in% vi_predictors_categorical)
  )

  x <- identify_valid_variables(
    df = vi_smol,
    responses = "vi_numeric",
    predictors = vi_predictors
  )

  testthat::expect_true(
    all(x$numeric %in% c("vi_numeric", vi_predictors_numeric))
  )

  testthat::expect_true(
    all(x$categorical %in% vi_predictors_categorical)
  )
})
