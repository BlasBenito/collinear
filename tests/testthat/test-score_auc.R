testthat::test_that("`score_auc()` works", {
  testthat::skip_on_cran()

  data(vi_smol)

  #perfect prediction
  x <- score_auc(
    o = vi_smol$vi_binomial,
    p = vi_smol$vi_numeric
  )

  testthat::expect_true(
    x == 1
  )

  #random prediction
  x <- score_auc(
    o = vi_smol$vi_binomial,
    p = runif(n = nrow(vi_smol))
  )

  testthat::expect_true(
    x < 1
  )

  #error
  testthat::expect_error(
    x <- score_auc(
      o = runif(n = nrow(vi_smol)),
      p = vi_smol$vi_binomial
    )
  )

  testthat::expect_error(
    x <- score_auc(
      o = NULL,
      p = vi_smol$vi_binomial
    )
  )

  testthat::expect_error(
    x <- score_auc(
      o = vi_smol$vi_binomial,
      p = NULL
    )
  )
})
