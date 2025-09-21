testthat::test_that("`identify_predictors_categorical()` works", {

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
    x <- identify_predictors_categorical(
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

  predictors_numeric <- identify_predictors_numeric(
    df = df,
    predictors = predictors,
    quiet = TRUE
  )

  testthat::expect_false(
    any(predictors_numeric %in% c(x$valid, x$invalid))
  )

  #no predictors
    x <- identify_predictors_categorical(
      df = df,
      predictors = NULL,
      quiet = FALSE
    )

    testthat::expect_null(
      x$valid
    )

    testthat::expect_null(
      x$invalid
    )

    x <- identify_predictors_categorical(
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

})

testthat::test_that("`identify_predictors_numeric()` works", {

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
    x <- identify_predictors_numeric(
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

  #no predictors
  x <- identify_predictors_numeric(
    df = df,
    predictors = NULL,
    quiet = FALSE
  )

  testthat::expect_null(
    x$valid
  )

  testthat::expect_null(
    x$invalid
  )

  x <- identify_predictors_numeric(
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

testthat::test_that("`identify_predictors_zero_variance()` works", {

  data(vi_smol, vi_predictors)
  vi_smol$zv_1 <- 1
  vi_smol$zv_2 <- runif(n = nrow(vi_smol), min = 0, max = 1e-04)
  vi_predictors <- c(vi_predictors, "zv_1", "zv_2")

  zero.variance.predictors <- identify_predictors_zero_variance(
    df = vi_smol,
    predictors = vi_predictors
  )

  testthat::expect_true(
    is.character(zero.variance.predictors),
    info = "Result should be a character vector."
  )

  testthat::expect_true(
    length(zero.variance.predictors) == 2
  )

})


testthat::test_that("`identify_predictors()` works", {

  predictors <- identify_predictors(
    df = vi_smol,
    predictors = vi_predictors
  )

  testthat::expect_true(
    all(predictors$numeric %in% vi_predictors_numeric)
  )

  testthat::expect_true(
    all(predictors$categorical %in% vi_predictors_categorical)
  )

})

