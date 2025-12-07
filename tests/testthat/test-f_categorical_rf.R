testthat::test_that("f_categorical_rf() works", {
  testthat::skip_on_cran()
  data(vi_smol)

  response <- "vi_categorical"
  numeric_predictor <- "swi_mean"
  categorical_predictor <- "koppen_zone"
  factor_predictor <- "vi_factor"

  #numeric predictor ----

  #perfect result
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[response]]
  )

  testthat::expect_equal(
    f_categorical_rf(df = df),
    1,
    tolerance = 1e-10
  )

  #imperfect result
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[numeric_predictor]]
  )

  testthat::expect_true(
    f_categorical_rf(df = df) < 1
  )

  #numeric predictor ----
  df <- data.frame(
    y = vi_smol[[numeric_predictor]],
    x = vi_smol[[categorical_predictor]]
  )

  testthat::expect_error(
    f_categorical_rf(df = df),
    regexp = "column 'y' of dataframe 'df' must be character or factor"
  )

  #wrong column names ----
  df <- data.frame(
    a = vi_smol[["vi_categorical"]],
    b = vi_smol[["koppen_zone"]]
  )

  testthat::expect_error(
    f_categorical_rf(df = df),
    regexp = "dataframe 'df' must have the column names 'x' and 'y'."
  )

  #cross validation ----

  #one iteration
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[numeric_predictor]]
  )

  set.seed(1)

  x0 <- f_categorical_rf(df = df)

  set.seed(1)

  x1 <- f_categorical_rf(
    df = df,
    cv_training_fraction = 1,
    cv_iterations = 1
  )

  testthat::expect_true(
    x0 == x1
  )

  x2 <- f_categorical_rf(
    df = df,
    cv_training_fraction = 0.9,
    cv_iterations = 10
  )

  testthat::expect_true(
    length(x2) == 10
  )

  x3 <- f_categorical_rf(
    df = df,
    cv_training_fraction = 0.5,
    cv_iterations = 10
  )

  testthat::expect_true(
    length(x3) == 10
  )

  x4 <- f_categorical_rf(
    df = df,
    cv_training_fraction = 0.25,
    cv_iterations = 10
  )

  testthat::expect_true(
    length(x4) == 10
  )

  testthat::expect_true(
    mean(x4) < mean(x3)
  )

  testthat::expect_true(
    mean(x4) < mean(x2)
  )

  testthat::expect_true(
    mean(x4) < mean(x1)
  )

  set.seed(2)

  x5 <- f_categorical_rf(
    df = df,
    cv_training_fraction = 0.25,
    cv_iterations = 10
  )

  testthat::expect_true(
    mean(x4) != mean(x5)
  )

  set.seed(2)

  x6 <- f_categorical_rf(
    df = df,
    cv_training_fraction = 0.25,
    cv_iterations = 10
  )

  testthat::expect_true(
    mean(x5) == mean(x6)
  )

  #categorical predictor
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[categorical_predictor]]
  )

  x_categorical <- f_categorical_rf(
    df = df,
    cv_training_fraction = 0.5,
    cv_iterations = 10
  )

  testthat::expect_true(
    length(x_categorical) == 10
  )

  #factor predictor
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[factor_predictor]]
  )

  x_factor <- f_categorical_rf(
    df = df,
    cv_training_fraction = 0.5,
    cv_iterations = 10
  )

  testthat::expect_true(
    length(x_factor) == 10
  )
})
