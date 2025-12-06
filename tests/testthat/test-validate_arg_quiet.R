testthat::test_that("`validate_arg_quiet()` works", {
  testthat::skip_on_cran()

  testthat::expect_true(
    validate_arg_quiet(
      quiet = TRUE
    )
  )

  testthat::expect_false(
    validate_arg_quiet(
      quiet = FALSE
    )
  )

  testthat::expect_false(
    validate_arg_quiet()
  )

  testthat::expect_message(
    x <- validate_arg_quiet(
      quiet = "a"
    ),
    regexp = "argument 'quiet' must be logical"
  )

  testthat::expect_true(
    x == FALSE
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  x <- validate_arg_quiet(
    quiet = x
  )

  testthat::expect_true(
    attributes(x)$validated
  )
})
