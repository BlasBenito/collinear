testthat::test_that("`cor_stats()` works", {

  data(
    vi_smol,
    vi_predictors,
    vi_predictors_categorical
    )

  #general usage

  #from predictors dataframe
  x <- cor_stats(
    df = vi_smol,
    predictors = vi_predictors[1:10],
    quiet = TRUE
  )

  testthat::expect_true(
    !is.data.frame(x)
  )

  testthat::expect_true(
    is.list(x) && all(c("correlation", "stats") %in% names(x))
  )

  testthat::expect_true(
    is.data.frame(x$correlation) && all(c("x", "y", "correlation", "metric") %in% colnames(x$correlation))
  )

  testthat::expect_true(
    is.data.frame(x$stats) && all(c("statistic", "correlation") %in% colnames(x$stats))
  )

  #from cor_df results
  x <- cor_df(
    df = vi_smol,
    predictors = vi_predictors[1:10]
  ) |>
    cor_stats(
      quiet = TRUE
    )

  testthat::expect_true(
    is.data.frame(x) && all(c("statistic", "correlation") %in% colnames(x))
  )

  #edge cases
  testthat::expect_error(
    x <- cor_stats(),
    regexp = "argument 'df' cannot be NULL"
  )

  testthat::expect_warning(
    x <- cor_stats(
      df = vi_smol[1:9, vi_predictors[1:10]],
      quiet = TRUE
    ),
    regexp = "argument 'df' has fewer than 10 rows"
  )

  testthat::expect_message(
    x <- cor_stats(
      df = vi_smol[1:29, vi_predictors[1:10]],
      quiet = FALSE
    ),
    regexp = "argument 'df' has fewer than 30 rows"
  )


})
