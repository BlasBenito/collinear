testthat::test_that("`auc_score()` works", {

  data(vi)

    #perfect prediction
    weights <- case_weights(
      x = vi$vi_binary
      )

    testthat::expect_true(
      length(unique(weights)) == 2
    )

    testthat::expect_error(
      weights <- case_weights(
        x = NULL
      )
    )

})
