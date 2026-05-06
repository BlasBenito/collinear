testthat::test_that("`f_auto()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors, package = "spatialData")
  vi_predictors_numeric <- identify_numeric_variables(
    df = vi_smol,
    predictors = vi_predictors
  )$valid
  vi_predictors_categorical <- identify_categorical_variables(
    df = vi_smol,
    predictors = vi_predictors
  )$valid

  responses_vector <- c(
    "vi_numeric",
    "vi_counts",
    "vi_categorical",
    "vi_binomial",
    "vi_factor"
  )

  predictors_vector <- c(
    "vi_predictors",
    "vi_predictors_categorical",
    "vi_predictors_numeric"
  )

  collinear_functions <- ls(
    getNamespace("collinear"),
    all.names = TRUE
  )

  for (response in responses_vector) {
    for (predictor in predictors_vector) {
      testthat::expect_no_message(
        x <- f_auto(
          df = vi_smol,
          response = response,
          predictors = get(predictor),
          quiet = TRUE
        )
      )

      testthat::expect_true(
        x %in% collinear_functions
      )
    }
  }

  #edge cases
  testthat::expect_error(
    x <- f_auto(
      df = vi_smol,
      response = NULL,
      predictors = vi_predictors_numeric,
      quiet = FALSE
    ),
    regexp = "argument 'response' must not be NULL"
  ) |>
    suppressMessages()

  #edge cases
  testthat::expect_message(
    x <- f_auto(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
      quiet = FALSE
    ),
    regexp = "selected function"
  ) |>
    suppressMessages()
})
