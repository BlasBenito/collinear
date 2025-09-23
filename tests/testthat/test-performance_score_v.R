testthat::test_that("`performance_score_r2()` works", {

  data(vi_smol)

    #perfect prediction
    x <- performance_score_v(
      o = vi_smol$vi_categorical,
      p = vi_smol$vi_categorical
      )

    testthat::expect_true(
      x == 1
    )

    #random prediction
    x <- performance_score_v(
      o = vi_smol$vi_categorical,
      p = vi_smol$koppen_zone
    )

    testthat::expect_true(
      x < 1
    )

    #error
    testthat::expect_error(
      x <- performance_score_v(
        o = NULL,
        p = vi_smol$vi_categorical
      )
    )

    testthat::expect_error(
      x <- performance_score_v(
        o = vi_smol$vi_categorical,
        p = NULL
      )
    )

})
