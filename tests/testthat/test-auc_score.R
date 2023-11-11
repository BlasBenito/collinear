testthat::test_that("`auc_score()` works", {

  data(vi)

    #perfect prediction
    auc <- auc_score(
      observed = vi$vi_binary,
      predicted = vi$vi_mean
      )

    testthat::expect_true(
      auc == 1
    )

    #random prediction
    auc <- auc_score(
      observed = vi$vi_binary,
      predicted = runif(n = nrow(vi))
    )

    testthat::expect_true(
      auc < 1
    )

    #error
    testthat::expect_error(
      auc <- auc_score(
        observed = runif(n = nrow(vi)),
        predicted = vi$vi_binary
      )
    )

    testthat::expect_error(
      auc <- auc_score(
        observed = NULL,
        predicted = vi$vi_binary
      )
    )

    testthat::expect_error(
      auc <- auc_score(
        observed = vi$vi_binary,
        predicted = NULL
      )
    )

})
