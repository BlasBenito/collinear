testthat::test_that("`validate_arg_max_cor()` works", {
  testthat::skip_on_cran()

  x <- validate_arg_max_cor(
    max_cor = NULL,
    quiet = FALSE
  )

  testthat::expect_null(x)

  testthat::expect_message(
    x <- validate_arg_max_cor(
      max_cor = c(0.5, 0.6),
      quiet = FALSE
    ),
    regexp = "argument 'max_cor' must be of length one, using value '0.5'"
  )

  testthat::expect_equal(as.numeric(x), 0.5, tolerance = 1e-10)

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_no_message(
    x <- validate_arg_max_cor(
      max_cor = x,
      quiet = FALSE
    )
  )

  testthat::expect_message(
    x <- validate_arg_max_cor(
      max_cor = "hola",
      quiet = FALSE
    ),
    regexp = "argument 'max_cor' is non-numeric, resetting it to '0.7'"
  )

  testthat::expect_equal(as.numeric(x), 0.7, tolerance = 1e-10)

  testthat::expect_message(
    x <- validate_arg_max_cor(
      max_cor = 1.2,
      quiet = FALSE
    ),
    regexp = "is outside its valid range"
  )

  testthat::expect_equal(as.numeric(x), 0.7, tolerance = 1e-10)

  testthat::expect_true(
    attributes(x)$validated
  )
})
