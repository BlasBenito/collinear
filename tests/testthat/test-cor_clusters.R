testthat::test_that("`cor_clusters()` works", {

  data(vi_smol, vi_predictors_numeric)

  out <- cor_clusters(
    df = vi_smol,
    predictors = vi_predictors_numeric
    )

  testthat::expect_true(
    is.data.frame(out$df)
    )

  testthat::expect_true(
    all(c("predictor", "cluster") %in% colnames(out$df))
  )

  testthat::expect_true(
    class(out$df$predictor) == "character"
  )


  testthat::expect_true(
    class(out$df$cluster) == "integer"
  )

  testthat::expect_true(
    max(out$df$cluster) < nrow(vi_smol)
  )

  testthat::expect_true(
    inherits(x = out$hclust, what = "hclust")
  )


})
