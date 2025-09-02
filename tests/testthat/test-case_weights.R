testthat::test_that("`case_weights()` works", {

  tol <- 0.000001

  #algorithmic changes
  #numeric vector
  y <- case_weights(
    x = c(0, 0, 1, 1)
  )

  testthat::expect_true(
    all(y - 0.25 < tol)
  )

  testthat::expect_true(
    sum(y) - 1 < tol
  )

  #logical vector
  y <- case_weights(
    x = c(TRUE, TRUE, FALSE, FALSE)
  )

  testthat::expect_true(
    all(y - 0.25 < tol)
  )

  testthat::expect_true(
    sum(y) - 1 < tol
  )

  #character vector
  y <- case_weights(
    x = c("a", "a", "b", "c")
  )

  testthat::expect_true(
    sum(y) - 1 < tol
  )

  testthat::expect_true(
    min(y) - 0.1666667 < tol
  )

  testthat::expect_true(
    max(y) - 0.3333333 < tol
  )

  #other behaviors
  testthat::expect_error(
    weights <- case_weights(
      x = NULL
    ),
    regexp = "argument 'x' cannot be NULL"
  )

  #works with logical
  weights <- case_weights(
    x = c(TRUE, TRUE, FALSE, FALSE)
  )


  testthat::expect_true(
    all(weights - 0.25 < tol)
  )

  testthat::expect_true(
    sum(weights) - 1 < tol
  )

  #controlled values
  weights <- case_weights(
    x = c(1, 1, 2, 2)
  )

  #error
  testthat::expect_error(
    weights <- case_weights(
      x = c(NA, TRUE, FALSE, FALSE)
    ),
    regexp = "NA, Inf, -Inf, or NaN values are not allowed when 'x' is numeric or logical"
  )


  testthat::expect_true(
    all(weights - 0.25 < tol)
  )

  testthat::expect_true(
    sum(weights) - 1 < tol
  )

  #errors
  testthat::expect_error(
    weights <- case_weights(
      x = c(NA, NaN, Inf, -Inf, 2, 2)
    ),
    regexp = "NA, Inf, -Inf, or NaN values are not allowed when 'x' is numeric or logical"
  )

  data(vi)

  #binomial weights
  weights <- case_weights(
    x = vi$vi_binomial
  )

  testthat::expect_true(
    length(unique(weights)) == 2
  )

  testthat::expect_true(
    sum(weights) - 1 < tol
  )

  #multinomial weights
  weights <- case_weights(
    x = vi$vi_categorical
  )

  testthat::expect_true(
    all(names(weights) %in% unique(vi$vi_categorical))
  )

  testthat::expect_true(
    sum(weights) - 1 < tol
  )

  #non valid values in multinomial
  vi$vi_categorical[1:4] <- c(NA, Inf, -Inf, NaN)

  weights <- case_weights(
    x = vi$vi_categorical
  )

  testthat::expect_true(
    all(c("NA", "Inf", "-Inf", "NaN") %in% names(weights))
  )

})
