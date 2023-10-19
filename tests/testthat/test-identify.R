testthat::test_that("`identify_non_numeric_predictors()` works", {

    data(vi, vi_predictors)

    non.numeric.predictors <- identify_non_numeric_predictors(
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

testthat::test_that("`identify_numeric_predictors()` works", {

    data(vi, vi_predictors)
    numeric.predictors <- identify_numeric_predictors(
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

testthat::test_that("`identify_zero_variance_predictors()` works", {

    data(vi, vi_predictors)
    vi$zv_1 <- 1
    vi$zv_2 <- runif(n = nrow(vi), min = 0, max = 1e-04)
    vi_predictors <- c(vi_predictors, "zv_1", "zv_2")

    zero.variance.predictors <- identify_zero_variance_predictors(
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
