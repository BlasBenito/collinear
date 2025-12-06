testthat::test_that("`identify_logical_variables()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors, vi_predictors_numeric)

  #invalid logical
  df <- vi_smol
  df$logical_invalid <- TRUE

  #valid logical
  df$logical_valid <- sample(
    x = c(TRUE, FALSE),
    size = nrow(vi_smol),
    replace = TRUE
  )

  testthat::expect_message(
    x <- identify_logical_variables(
      df = df,
      predictors = c(
        vi_predictors,
        "logical_invalid",
        "logical_valid"
      )
    ),
    regexp = "invalid logical predictors"
  )

  testthat::expect_true(
    "logical_invalid" == x$invalid
  )

  testthat::expect_true(
    "logical_valid" == x$valid
  )

  x <- identify_logical_variables(
    df = vi_smol,
    predictors = vi_predictors_numeric
  )

  testthat::expect_true(
    all(is.null(x$valid), is.null(x$invalid))
  )

  ##
  testthat::expect_message(
    x <- identify_logical_variables(
      df = df,
      responses <- c(
        "logical_invalid",
        "logical_valid"
      ),
      predictors = vi_predictors
    ),
    regexp = "invalid logical variables"
  )

  testthat::expect_true(
    "logical_invalid" == x$invalid
  )

  testthat::expect_true(
    "logical_valid" == x$valid
  )

  ##
  testthat::expect_message(
    x <- identify_logical_variables(
      df = df,
      responses <- c(
        "logical_invalid",
        "logical_valid"
      )
    ),
    regexp = "invalid logical responses"
  )

  testthat::expect_true(
    "logical_invalid" == x$invalid
  )

  testthat::expect_true(
    "logical_valid" == x$valid
  )

  ##
  testthat::expect_error(
    x <- identify_logical_variables(
      df = vi_smol
    ),
    regexp = "there are no variables to identify"
  )

  ##
  x <- identify_logical_variables(
    df = vi_smol,
    predictors = vi_predictors_numeric
  )

  testthat::expect_true(
    all(is.null(x$valid), is.null(x$invalid))
  )
})
