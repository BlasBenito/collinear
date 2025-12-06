testthat::test_that("`validate_arg_encoding_method()` works", {
  testthat::skip_on_cran()

  testthat::expect_message(
    x <- validate_arg_encoding_method(
      encoding_method = "hola"
    ),
    regexp = "argument 'encoding_method' is not valid, resetting it to 'loo'"
  )

  testthat::expect_true(
    x == "loo"
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_no_message(
    x <- validate_arg_encoding_method(
      encoding_method = "hola",
      quiet = TRUE
    )
  )

  testthat::expect_true(
    x == "loo"
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  #passing a validated input
  x <- validate_arg_encoding_method(
    encoding_method = x,
    quiet = TRUE
  )

  #passing a NULL
  testthat::expect_message(
    x <- validate_arg_encoding_method(
      encoding_method = NULL
    ),
    regexp = "argument 'encoding_method' is NULL, skipping target encoding"
  )

  #with overwrite
  testthat::expect_message(
    x <- validate_arg_encoding_method(
      encoding_method = c("loo", "mean"),
      overwrite = TRUE
    ),
    regexp = "only one encoding method allowed when 'overwrite = TRUE', using method: 'loo'"
  )
})
