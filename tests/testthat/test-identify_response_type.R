testthat::test_that("`identify_response_type()` works", {

  data(vi_smol)

  testthat::expect_error(
    x <- identify_response_type(
      df = NULL,
      response = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  testthat::expect_error(
    x <- identify_response_type(
      df = vi_smol,
      response = NULL
    ),
    regexp = "argument 'response' must not be NULL"
  )

  x <- identify_response_type(
    df = vi_smol,
    response = "vi_numeric",
    quiet = TRUE
  )

  testthat::expect_true(
    x == "continuous-high"
  )


  x <- identify_response_type(
    df = vi_smol,
    response = "vi_counts",
    quiet = TRUE
  )

  testthat::expect_true(
    x == "integer-high"
  )

  x <- identify_response_type(
    df = vi_smol,
    response = "vi_binomial",
    quiet = TRUE
  )

  testthat::expect_true(
    x == "integer-binomial"
  )

  x <- identify_response_type(
    df = vi_smol,
    response = "vi_categorical",
    quiet = TRUE
  )

  testthat::expect_true(
    x == "categorical"
  )

  x <- identify_response_type(
    df = vi_smol,
    response = "vi_factor",
    quiet = TRUE
  )

  testthat::expect_true(
    x == "categorical"
  )

  vi_smol$response_constant <- 1

  testthat::expect_error(
    x <- identify_response_type(
      df = vi_smol,
      response = "response_constant"
    ),
    regexp = "argument 'df' has no valid columns"
  ) |>
    suppressMessages()

  vi_smol$response_binary <- c(1.1, 2.1)

  testthat::expect_message(
    x <- identify_response_type(
      df = vi_smol,
      response = "response_binary"
    ),
    regexp = "argument 'response' names a numeric non-integer column with two unique values"
  ) |>
    suppressMessages()

  vi_smol$response_binary <- c(1, 2)

  x <- identify_response_type(
    df = vi_smol,
    response = "response_binary",
    quiet = TRUE
  )

  testthat::expect_true(
    x == "integer-binary"
  )

  vi_smol$response_trinary <- sample(c(1.1, 2.1, 3.1), size = nrow(vi_smol), replace = TRUE)

  testthat::expect_message(
    x <- identify_response_type(
      df = vi_smol,
      response = "response_trinary"
    ),
    regexp = "argument 'response' names a numeric non-integer column with 5 or fewer values"
  ) |>
    suppressMessages()

  vi_smol$response_trinary <- sample(c(1, 2, 3), size = nrow(vi_smol), replace = TRUE)

  x <- identify_response_type(
    df = vi_smol,
    response = "response_trinary",
    quiet = TRUE
  )

  testthat::expect_true(
    x == "integer-low"
  )


})
