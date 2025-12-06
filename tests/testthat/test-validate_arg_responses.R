testthat::test_that("`validate_arg_responses()` works", {
  testthat::skip_on_cran()

  data(vi_smol)

  #no arguments
  testthat::expect_error(
    x <- validate_arg_responses(
      df = NULL,
      responses = NULL,
      function_name = NULL,
      quiet = FALSE
    )
  )

  #valid use case
  x <- validate_arg_responses(
    df = vi_smol,
    responses = "vi_numeric"
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  #validated input
  x <- validate_arg_responses(
    df = vi_smol,
    responses = x
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  #several responses and max_responses

  #invalid
  testthat::expect_error(
    x <- validate_arg_responses(
      df = vi_smol,
      responses = c("vi_numeric", "vi_categorical", "vi_binomial"),
      max_responses = "hola"
    ),
    regexp = "argument 'max_responses' must be a integer"
  )

  #valid use case
  x <- validate_arg_responses(
    df = vi_smol,
    responses = c("vi_numeric", "vi_categorical", "vi_binomial"),
    max_responses = 3
  )

  testthat::expect_true(
    all(c("vi_numeric", "vi_categorical", "vi_binomial") %in% x)
  )

  #invalid use case
  testthat::expect_message(
    x <- validate_arg_responses(
      df = vi_smol,
      responses = c("vi_numeric", "vi_categorical", "vi_binomial"),
      max_responses = 2
    ),
    regexp = "must be of length"
  )

  testthat::expect_true(
    all(c("vi_numeric", "vi_categorical") %in% x)
  )

  #response is NULL
  x <- validate_arg_responses(
    df = vi_smol,
    responses = NULL
  )

  testthat::expect_true(
    is.null(x)
  )

  #response not in df
  testthat::expect_message(
    x <- validate_arg_responses(
      df = vi_smol,
      responses = "hola"
    ),
    regexp = "argument 'responses' does not contain column names of 'df'"
  )

  testthat::expect_true(
    is.null(x)
  )

  testthat::expect_message(
    x <- validate_arg_responses(
      df = vi_smol,
      responses = c("hola", "vi_numeric")
    ),
    regexp = "the following values of argument 'responses' are not column names of 'df' and will be ignored"
  )

  testthat::expect_true(
    x == "vi_numeric"
  )

  #wrong response and max_responses
  testthat::expect_message(
    x <- validate_arg_responses(
      df = vi_smol,
      responses = "hola",
      max_responses = 1
    ),
    regexp = "argument 'response' does not contain column names of 'df'"
  )

  testthat::expect_true(
    is.null(x)
  )

  #all null
  testthat::expect_error(
    response <- validate_arg_responses(
      df = NULL,
      responses = NULL
    )
  )
})
