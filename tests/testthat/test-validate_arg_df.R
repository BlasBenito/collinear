testthat::test_that("`validate_arg_df()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors_numeric)

  #no arguments
  testthat::expect_error(
    x <- validate_arg_df(
      df = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  testthat::expect_message(
    x <- validate_arg_df(
      df = vi_smol,
      responses = NULL,
      predictors = NULL
    ),
    regexp = "arguments 'responses' and 'predictors' are NULL, skipping validation of column values"
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  #few rows
  testthat::expect_error(
    x <- validate_arg_df(
      df = vi_smol[1:2, ]
    ),
    regexp = "has fewer than 3 rows"
  )

  testthat::expect_message(
    x <- validate_arg_df(
      df = vi_smol[1:9, ]
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  testthat::expect_true(
    nrow(x) == 9
  )

  testthat::expect_message(
    x <- validate_arg_df(
      df = vi_smol[1:29, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 30 rows"
  )

  testthat::expect_true(
    nrow(x) == 29
  )

  testthat::expect_error(
    x <- validate_arg_df(
      df = vi_smol[, 0],
      predictors = vi_predictors_numeric
    ),
    regexp = "argument 'df' has zero columns"
  )

  testthat::expect_error(
    x <- validate_arg_df(
      df = vi_smol[, vi_predictors_numeric[1], drop = FALSE],
      predictors = vi_predictors_numeric[1]
    ),
    regexp = "argument 'df' has one valid column, multicollinearity analysis cannot be performed"
  )

  testthat::expect_error(
    x <- validate_arg_df(
      df = stats::lm
    ),
    regexp = "cannot coerce argument 'df' to class 'data.frame'"
  ) |>
    suppressMessages()

  testthat::expect_error(
    x <- validate_arg_df(
      df = FALSE
    ),
    regexp = "cannot coerce argument 'df' to class 'data.frame'"
  ) |>
    suppressMessages()

  #logical columns
  vi_smol$logical <- sample(
    c(TRUE, FALSE),
    size = nrow(vi_smol),
    replace = TRUE
  )

  testthat::expect_message(
    x <- validate_arg_df(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = c(vi_predictors_numeric, "logical"),
      quiet = FALSE
    ),
    regexp = "converted the following logical columns to numeric"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.numeric(x$logical)
  )

  testthat::expect_message(
    x <- validate_arg_df(
      df = vi_smol,
      responses = "hola",
      predictors = "adios",
      quiet = FALSE
    ),
    regexp = "argument 'responses' does not contain column names of 'df'"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  testthat::expect_message(
    x <- validate_arg_df(
      df = vi_smol,
      responses = "hola",
      predictors = "adios",
      quiet = FALSE
    ),
    regexp = "arguments 'responses' and 'predictors' are NULL, skipping validation of column values"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  testthat::expect_warning(
    x <- validate_arg_df(
      df = vi_smol,
      responses = "hola",
      predictors = "adios",
      quiet = FALSE
    ),
    regexp = "none of the 'predictors' are column names of 'df'"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  x <- validate_arg_df(
    df = vi_smol,
    responses = "vi_numeric",
    predictors = vi_predictors_numeric,
    quiet = FALSE
  )

  testthat::expect_true(attributes(x)$validated)

  testthat::expect_true(
    all(c("vi_numeric", vi_predictors_numeric) %in% colnames(x))
  )

  #pass a validated data frame
  x <- validate_arg_df(
    df = x
  )

  testthat::expect_true(attributes(x)$validated)

  #only one predictor
  x <- validate_arg_df(
    df = vi_smol,
    responses = NULL,
    predictors = vi_predictors_numeric[1],
    quiet = FALSE
  )

  testthat::expect_true(attributes(x)$validated)

  testthat::expect_true(
    all(vi_predictors_numeric[1] %in% colnames(x))
  )

  #zero variance predictors
  vi_smol$zero_variance <- 1

  testthat::expect_message(
    x <- validate_arg_df(
      df = vi_smol,
      responses = NULL,
      predictors = c(vi_predictors_numeric, "zero_variance"),
      quiet = FALSE
    ),
    regexp = "invalid numeric predictors due to near-zero variance"
  ) |>
    suppressMessages()

  testthat::expect_false(
    "zero_variance" %in% colnames(x)
  )

  #categorical
  testthat::expect_message(
    x <- validate_arg_df(
      df = vi_smol,
      predictors = vi_predictors_categorical
    ),
    regexp = "converted the following character columns to factor"
  )
})
