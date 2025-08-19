testthat::test_that("`case_weights()` works", {

  data(vi)

  #binomial weights
  weights <- case_weights(
    x = vi$vi_binomial
  )

  testthat::expect_true(
    length(unique(weights)) == 2
  )

  #multinomial weights
  weights <- case_weights(
    x = vi$vi_categorical
  )

  testthat::expect_true(
    all(names(weights) %in% unique(vi$vi_categorical))
  )

  testthat::expect_error(
    weights <- case_weights(
      x = NULL
    )
  )

})
