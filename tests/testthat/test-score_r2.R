testthat::test_that("`score_r2()` works", {
  testthat::skip_on_cran()

  data(vi_smol)

  #perfect prediction
  x <- score_r2(
    o = vi_smol$vi_numeric,
    p = vi_smol$vi_numeric
  )

  testthat::expect_equal(x, 1, tolerance = 1e-10)

  #random prediction
  x <- score_r2(
    o = vi_smol$vi_binomial,
    p = runif(n = nrow(vi_smol))
  )

  testthat::expect_true(
    x < 1
  )

  #error
  testthat::expect_error(
    x <- score_r2(
      o = NULL,
      p = vi_smol$vi_binomial
    )
  )

  testthat::expect_error(
    x <- score_r2(
      o = vi_smol$vi_binomial,
      p = NULL
    )
  )
})
