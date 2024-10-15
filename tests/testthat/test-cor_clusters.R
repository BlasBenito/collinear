testthat::test_that("`cor_clusters()` works", {

  vi <- vi[1:1000, ]
  predictors <- vi_predictors[1:15]

  df <- cor_clusters(
    df = vi,
    predictors = predictors
    )

  testthat::expect_true(
    is.data.frame(df)
    )

  testthat::expect_true(
    all(c("predictor", "cluster") %in% colnames(df))
  )

  testthat::expect_true(
    class(df$predictor) == "character"
  )


  testthat::expect_true(
    class(df$cluster) == "integer"
  )

  testthat::expect_true(
    max(df$cluster) < nrow(df)
  )


})
