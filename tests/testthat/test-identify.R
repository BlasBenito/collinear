testthat::test_that("`identify_predictors_categorical()` works", {

    data(vi, vi_predictors)

    non.numeric.predictors <- identify_predictors_categorical(
      df = vi,
      predictors = vi_predictors
    )

    testthat::expect_true(
      is.character(non.numeric.predictors),
      info = "Result should be a character vector."
    )

    testthat::expect_true(
      length(non.numeric.predictors) == 12
    )

})

testthat::test_that("`identify_predictors_numeric()` works", {

    data(vi, vi_predictors)
    numeric.predictors <- identify_predictors_numeric(
      df = vi,
      predictors = vi_predictors
    )

    testthat::expect_true(
      is.character(numeric.predictors),
      info = "Result should be a character vector."
    )

    testthat::expect_true(
      length(numeric.predictors) == 49
    )

})

testthat::test_that("`identify_predictors_zero_variance()` works", {

    data(vi, vi_predictors)
    vi$zv_1 <- 1
    vi$zv_2 <- runif(n = nrow(vi), min = 0, max = 1e-04)
    vi_predictors <- c(vi_predictors, "zv_1", "zv_2")

    zero.variance.predictors <- identify_predictors_zero_variance(
      df = vi,
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
    df = vi,
    predictors = vi_predictors
  )

  testthat::expect_true(
    all(predictors$numeric %in% vi_predictors_numeric)
  )

  testthat::expect_true(
    all(predictors$categorical %in% vi_predictors_categorical)
  )

})

