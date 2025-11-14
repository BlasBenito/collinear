testthat::test_that("`vif_stats()` works", {

  data(
    vi_smol,
    vi_predictors_numeric
    )

  #general usage

  #from predictors dataframe
  x <- vif_stats(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(colnames(x) %in% c("method", "statistic", "value"))
  )

  #from m results
  m <- cor_matrix(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  x <- vif_stats(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    quiet = TRUE,
    m = m
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(colnames(x) %in% c("method", "statistic", "value"))
  )

  #edge cases
  testthat::expect_error(
    x <- vif_stats(),
    regexp = "argument 'df' cannot be NULL"
  )


})
