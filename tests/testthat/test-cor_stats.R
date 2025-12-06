testthat::test_that("`cor_stats()` works", {
  testthat::skip_on_cran()
  data(
    vi_smol,
    vi_predictors,
    vi_predictors_numeric
  )

  #general usage

  #from predictors dataframe
  x <- cor_stats(
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
    cor_stats()

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(colnames(x) %in% c("method", "statistic", "value"))
  )

  #edge cases
  testthat::expect_error(
    x <- cor_stats(),
    regexp = "argument 'df' cannot be NULL"
  )
})
