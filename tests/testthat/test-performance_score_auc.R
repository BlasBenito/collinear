testthat::test_that("`performance_score_auc()` works", {

  data(vi)

    #perfect prediction
    x <- performance_score_auc(
      o = vi$vi_binomial,
      p = vi$vi_numeric
      )

    testthat::expect_true(
      x == 1
    )

    #random prediction
    x <- performance_score_auc(
      o = vi$vi_binomial,
      p = runif(n = nrow(vi))
    )

    testthat::expect_true(
      x < 1
    )

    #error
    testthat::expect_error(
      x <- performance_score_auc(
        o = runif(n = nrow(vi)),
        p = vi$vi_binomial
      )
    )

    testthat::expect_error(
      x <- performance_score_auc(
        o = NULL,
        p = vi$vi_binomial
      )
    )

    testthat::expect_error(
      x <- performance_score_auc(
        o = vi$vi_binomial,
        p = NULL
      )
    )

})
