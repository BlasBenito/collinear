testthat::test_that("`performance_score_auc()` works", {

  data(vi_smol)

    #perfect prediction
    x <- performance_score_auc(
      o = vi_smol$vi_binomial,
      p = vi_smol$vi_numeric
      )

    testthat::expect_true(
      x == 1
    )

    #random prediction
    x <- performance_score_auc(
      o = vi_smol$vi_binomial,
      p = runif(n = nrow(vi_smol))
    )

    testthat::expect_true(
      x < 1
    )

    #error
    testthat::expect_error(
      x <- performance_score_auc(
        o = runif(n = nrow(vi_smol)),
        p = vi_smol$vi_binomial
      )
    )

    testthat::expect_error(
      x <- performance_score_auc(
        o = NULL,
        p = vi_smol$vi_binomial
      )
    )

    testthat::expect_error(
      x <- performance_score_auc(
        o = vi_smol$vi_binomial,
        p = NULL
      )
    )

})
