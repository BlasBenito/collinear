testthat::test_that("f_numeric_rpart() works", {

  data(vi_smol)

  #perfect result
  df <- data.frame(
    y = vi_smol[["vi_numeric"]],
    x = vi_smol[["vi_numeric"]]
  ) |>
    na.omit()

  testthat::expect_true(
    f_numeric_rpart(df = df) > 0.96
  )

  df <- data.frame(
    y = vi_smol[["vi_numeric"]],
    x = vi_smol[["swi_mean"]]
  ) |>
    na.omit()

  testthat::expect_true(
    f_numeric_rpart(df = df) < 1
  )

  df <- data.frame(
    y = vi_smol[["vi_categorical"]],
    x = vi_smol[["koppen_zone"]]
  ) |>
    na.omit()

  testthat::expect_error(
    f_numeric_rpart(df = df),
    regexp = "columns 'x' and 'y' of dataframe 'df' must be numeric"
  )

  df <- data.frame(
    a = vi_smol[["vi_categorical"]],
    b = vi_smol[["koppen_zone"]]
  ) |>
    na.omit()

  testthat::expect_error(
    f_numeric_rpart(df = df),
    regexp = "dataframe 'df' must have the column names 'x' and 'y'."
  )


  #cross validation
  df <- data.frame(
    y = vi_smol[["vi_numeric"]],
    x = vi_smol[["swi_mean"]]
  )

  x0 <- f_numeric_rpart(
    df = df
  )

  set.seed(1)

  x1 <- f_numeric_rpart(
    df = df,
    cv_training_fraction = 1,
    cv_iterations = 1
  )

  testthat::expect_true(
    x0 == x1
  )

  x2 <- f_numeric_rpart(
    df = df,
    cv_training_fraction = 0.9,
    cv_iterations = 100
  )

  testthat::expect_true(
    length(x2) == 100
  )

  x3 <- f_numeric_rpart(
    df = df,
    cv_training_fraction = 0.5,
    cv_iterations = 100
  )

  testthat::expect_true(
    length(x3) == 100
  )

  testthat::expect_true(
    mean(x3) < mean(x2)
  )

  x4 <- f_numeric_rpart(
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

  testthat::expect_true(
    mean(x4) < mean(x2)
  )

  testthat::expect_true(
    mean(x4) < mean(x1)
  )

  set.seed(2)

  x5 <- f_numeric_rpart(
    df = df,
    cv_training_fraction = 0.25,
    cv_iterations = 100
  )

  testthat::expect_true(
    mean(x4) != mean(x5)
  )

  set.seed(2)

  x6 <- f_numeric_rpart(
    df = df,
    cv_training_fraction = 0.25,
    cv_iterations = 100
  )

  testthat::expect_true(
    mean(x5) == mean(x6)
  )



})
