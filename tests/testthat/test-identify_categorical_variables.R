testthat::test_that("`identify_categorical_variables()` works", {

  data(vi_smol, vi_predictors, vi_predictors_categorical)

  df <- vi_smol
  df$zero_cardinality <- "a"
  df$full_cardinality <- as.character(seq_len(nrow(df)))
  df$logical <- TRUE

  predictors <- c(
    vi_predictors,
    "zero_cardinality",
    "full_cardinality",
    "logical"
  )

  #identify all categoricals
  testthat::expect_message(
    x <- identify_categorical_variables(
      df = df,
      predictors = predictors,
      quiet = FALSE
    ),
    regexp = "invalid categorical predictors due to degenerate cardinality"
  )

  testthat::expect_true(
    is.list(x) && all(c("valid", "invalid") %in% names(x))
  )

  testthat::expect_true(
    all(c("zero_cardinality", "full_cardinality") %in% x$invalid)
  )

  testthat::expect_false(
    all(c("zero_cardinality", "full_cardinality") %in% x$valid)
  )

  testthat::expect_false(
    ("logical" %in% x$valid) && ("logical" %in% x$invalid)
  )

  ##
  #identify all categoricals
  testthat::expect_message(
    x <- identify_categorical_variables(
      df = df,
      responses = c("zero_cardinality", "full_cardinality"),
      predictors = vi_predictors_categorical,
      quiet = FALSE
    ),
    regexp = "invalid categorical variables due to degenerate cardinality"
  )

  testthat::expect_message(
    x <- identify_categorical_variables(
      df = df,
      responses = c("zero_cardinality", "full_cardinality"),
      quiet = FALSE
    ),
    regexp = "invalid categorical responses due to degenerate cardinality"
  )

  testthat::expect_true(
    is.list(x) && all(c("valid", "invalid") %in% names(x))
  )

  testthat::expect_true(
    all(c("zero_cardinality", "full_cardinality") %in% x$invalid)
  )

  testthat::expect_false(
    all(c("zero_cardinality", "full_cardinality") %in% x$valid)
  )

  testthat::expect_false(
    ("logical" %in% x$valid) && ("logical" %in% x$invalid)
  )

  #no predictors
  x <- identify_categorical_variables(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    quiet = FALSE
  )

  testthat::expect_null(
    x$valid
  )

  testthat::expect_null(
    x$invalid
  )

  testthat::expect_error(
    x <- identify_categorical_variables(
      df = vi_smol,
      quiet = FALSE
    ),
    regexp = "there are no variables to identify"
  )


  testthat::expect_null(
    x$valid
  )

  testthat::expect_null(
    x$invalid
  )


})
