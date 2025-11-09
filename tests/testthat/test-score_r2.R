testthat::test_that("`score_r2()` works", {

  data(vi_smol)

    #perfect prediction
    x <- score_r2(
      o = vi_smol$vi_numeric,
      p = vi_smol$vi_numeric
      )

    testthat::expect_true(
      x == 1
    )

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
