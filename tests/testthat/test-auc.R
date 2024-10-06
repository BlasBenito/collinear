testthat::test_that("`auc()` works", {

  data(vi)

    #perfect prediction
    x <- auc(
      o = vi$vi_binomial,
      p = vi$vi_numeric
      )

    testthat::expect_true(
      x == 1
    )

    #random prediction
    x <- auc(
      o = vi$vi_binomial,
      p = runif(n = nrow(vi))
    )

    testthat::expect_true(
      x < 1
    )

    #error
    testthat::expect_error(
      x <- auc(
        o = runif(n = nrow(vi)),
        p = vi$vi_binomial
      )
    )

    testthat::expect_error(
      x <- auc(
        o = NULL,
        p = vi$vi_binomial
      )
    )

    testthat::expect_error(
      x <- auc(
        o = vi$vi_binomial,
        p = NULL
      )
    )

})
