testthat::test_that("`validate_arg_df()` works", {

  data(vi, vi_predictors)

  df <- vi[1:1000, ]

  #no arguments
  testthat::expect_error(
    x <- validate_arg_df(
      df = NULL,
      response = "vi_numeric",
      predictors = vi_predictors,
      quiet = FALSE
    )
  )

  testthat::expect_message(
    x <- validate_arg_df(
      df = vi,
      response = NULL,
      predictors = NULL,
      quiet = FALSE
    ),
    regexp = "replaced"
  )

  #few rows
  testthat::expect_error(
    x <- validate_arg_df(
      df = df[1:2, ],
      predictors = vi_predictors
    ),
    regexp = "has fewer than 3 rows"
  )

  testthat::expect_warning(
    x <- cor_select(
      df = df[1:9, ],
      predictors = vi_predictors
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- cor_select(
      df = df[1:29, ],
      predictors = vi_predictors
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()


  #normal usage
  testthat::expect_message(
    x <- validate_arg_df(
      df = df,
      response = "vi_numeric",
      predictors = vi_predictors,
      quiet = FALSE
    ),
    regexp = "replaced"
  )

  testthat::expect_true(attributes(x)$validated)

  testthat::expect_true(
    all(c("vi_numeric", vi_predictors) %in% colnames(x))
  )

  #only one predictor
  x <- validate_arg_df(
    df = df,
    response = NULL,
    predictors = vi_predictors[1],
    quiet = FALSE
  )

  testthat::expect_true(attributes(x)$validated)

  testthat::expect_true(
    all(vi_predictors[1] %in% colnames(x))
  )

})
