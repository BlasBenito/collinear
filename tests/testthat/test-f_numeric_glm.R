testthat::test_that("f_numeric_glm() works", {
  testthat::skip_on_cran()

  data(vi_smol)

  response <- "vi_numeric"
  numeric_predictor <- "swi_mean"
  categorical_predictor <- "koppen_zone"
  factor_predictor <- "vi_factor"

  #numeric predictor ----

  #perfect result
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[response]]
  )

  testthat::expect_true(
    f_numeric_glm(df = df) == 1
  )

  #imperfect result
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[numeric_predictor]]
  )

  testthat::expect_true(
    f_numeric_glm(df = df) < 1
  )

  #categorical response and predictor ----
  df <- data.frame(
    y = vi_smol[[categorical_predictor]],
    x = vi_smol[[categorical_predictor]]
  )

  testthat::expect_error(
    f_numeric_glm(df = df),
    regexp = "column 'y' of dataframe 'df' must be numeric"
  )

  #categorical predictor ----
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[categorical_predictor]]
  )

  testthat::expect_true(
    f_numeric_glm(df = df) < 1
  )

  #wrong column names ----
  df <- data.frame(
    a = vi_smol[["vi_categorical"]],
    b = vi_smol[["koppen_zone"]]
  )

  testthat::expect_error(
    f_numeric_glm(df = df),
    regexp = "dataframe 'df' must have the column names 'x' and 'y'."
  )

  #cross validation ----

  #one iteration
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[numeric_predictor]]
  )

  x0 <- f_numeric_glm(df = df)

  set.seed(1)

  x1 <- f_numeric_glm(
    df = df,
    cv_training_fraction = 1,
    cv_iterations = 1
  )

  testthat::expect_true(
    x0 == x1
  )

  x2 <- f_numeric_glm(
    df = df,
    cv_training_fraction = 0.9,
    cv_iterations = 100
  )

  testthat::expect_true(
    length(x2) == 100
  )

  x3 <- f_numeric_glm(
    df = df,
    cv_training_fraction = 0.5,
    cv_iterations = 100
  )

  testthat::expect_true(
    length(x3) == 100
  )

  x4 <- f_numeric_glm(
    df = df,
    cv_training_fraction = 0.25,
    cv_iterations = 100
  )

  testthat::expect_true(
    length(x4) == 100
  )

  testthat::expect_true(
    mean(x4) < mean(x3)
  )

  set.seed(2)

  x5 <- f_numeric_glm(
    df = df,
    cv_training_fraction = 0.25,
    cv_iterations = 100
  )

  set.seed(2)

  x6 <- f_numeric_glm(
    df = df,
    cv_training_fraction = 0.25,
    cv_iterations = 100
  )

  testthat::expect_true(
    mean(x5) == mean(x6)
  )

  #categorical predictor
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[categorical_predictor]]
  )

  x_categorical <- f_numeric_glm(
    df = df,
    cv_training_fraction = 0.5,
    cv_iterations = 100
  )

  testthat::expect_true(
    length(x_categorical) == 100
  )

  #factor predictor
  df <- data.frame(
    y = vi_smol[[response]],
    x = vi_smol[[factor_predictor]]
  )

  x_factor <- f_numeric_glm(
    df = df,
    cv_training_fraction = 0.5,
    cv_iterations = 100
  )

  testthat::expect_true(
    length(x_factor) == 100
  )
})
