testthat::test_that("`collinear_stats()` works", {

  data(
    vi_smol,
    vi_predictors,
    vi_predictors_numeric
    )

  #general usage

  #from predictors dataframe
  x <- collinear_stats(
    df = vi_smol,
    predictors = vi_predictors_numeric[1:10],
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(colnames(x) %in% c("method", "statistic", "value"))
  )


  #from cor_df results
  x <- cor_df(
    df = vi_smol,
    predictors = vi_predictors_numeric[1:10]
  ) |>
    collinear_stats(
      quiet = TRUE
    )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(colnames(x) %in% c("method", "statistic", "value"))
  )

  testthat::expect_true(
    all(c("correlation", "vif") %in% x$method)
  )

  #edge cases
  testthat::expect_error(
    x <- collinear_stats(),
    regexp = "argument 'df' cannot be NULL"
  )


})
