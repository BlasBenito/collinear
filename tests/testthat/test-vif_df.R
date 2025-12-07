testthat::test_that("`vif_df()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors)

  testthat::expect_message(
    x <- vif_df(
      df = vi_smol,
      predictors = vi_predictors[1:10]
    ),
    regexp = "converted the following character columns to factor"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- vif_df(
      df = vi_smol,
      predictors = vi_predictors[1:10]
    ),
    regexp = "may bias the multicollinearity analysis"
  ) |>
    suppressMessages()

  testthat::expect_warning(
    x <- vif_df(
      df = vi_smol,
      predictors = vi_predictors[1:10],
      quiet = TRUE
    ),
    regexp = "may bias the multicollinearity analysis"
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(names(x) %in% c("predictor", "vif"))
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors[1:10])
  )

  # edge cases ----

  #no arguments
  testthat::expect_error(
    x <- vif_df(
      df = NULL,
      predictors = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #single predictor
  testthat::expect_message(
    x <- vif_df(
      df = vi_smol,
      predictors = vi_predictors[1]
    ),
    regexp = "only one valid predictor"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == 1
  )

  testthat::expect_equal(x$vif, 0, tolerance = 1e-10)
})
