testthat::test_that("`identify_numeric_variables()` works", {

  data(vi_smol, vi_predictors_numeric, vi_predictors_categorical, vi_predictors)

  df <- vi_smol
  df$zero_variance <- 1
  df$logical <- TRUE

  predictors <- c(
    vi_predictors,
    "zero_variance",
    "logical"
  )

  #identify all categoricals
  testthat::expect_message(
    x <- identify_numeric_variables(
      df = df,
      predictors = predictors,
      quiet = FALSE
    ),
    regexp = "invalid numeric predictors due to near-zero variance"
  )

  testthat::expect_true(
    is.list(x) && all(c("valid", "invalid") %in% names(x))
  )

  testthat::expect_true(
    "zero_variance" %in% x$invalid
  )

  testthat::expect_false(
    "zero_variance" %in% x$valid
  )

  testthat::expect_false(
    ("logical" %in% x$valid) && ("logical" %in% x$invalid)
  )

  ##
  testthat::expect_message(
    x <- identify_numeric_variables(
      df = df,
      responses = c(
        "zero_variance",
        "logical"
      ),
      predictors = vi_predictors,
      quiet = FALSE
    ),
    regexp = "invalid numeric variables due to near-zero variance"
  )

  testthat::expect_true(
    is.list(x) && all(c("valid", "invalid") %in% names(x))
  )

  testthat::expect_true(
    "zero_variance" %in% x$invalid
  )

  testthat::expect_false(
    "zero_variance" %in% x$valid
  )

  testthat::expect_false(
    ("logical" %in% x$valid) && ("logical" %in% x$invalid)
  )

  ##
  testthat::expect_message(
    x <- identify_numeric_variables(
      df = df,
      responses = c(
        "zero_variance",
        "logical"
      ),
      quiet = FALSE
    ),
    regexp = "invalid numeric responses due to near-zero variance"
  )

  testthat::expect_true(
    is.list(x) && all(c("valid", "invalid") %in% names(x))
  )

  testthat::expect_true(
    "zero_variance" %in% x$invalid
  )

  testthat::expect_false(
    "zero_variance" %in% x$valid
  )

  testthat::expect_false(
    ("logical" %in% x$valid) && ("logical" %in% x$invalid)
  )

  #no predictors
  testthat::expect_error(
    x <- identify_numeric_variables(
      df = vi_smol,
      predictors = NULL,
      quiet = FALSE
    ),
    regexp = "there are no variables to identify"
  )


  x <- identify_numeric_variables(
    df = vi_smol,
    predictors = vi_predictors_categorical,
    quiet = FALSE
  )

  testthat::expect_null(
    x$valid
  )

  testthat::expect_null(
    x$invalid
  )

})
