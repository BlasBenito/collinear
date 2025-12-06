testthat::test_that("`validate_arg_quiet()` works", {
  testthat::skip_on_cran()

  x <- validate_arg_f(
    f = f_auto,
    f_name = "f_auto"
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    attributes(x)$name == "f_auto"
  )

  #pass validated
  x <- validate_arg_f(
    f = x
  )

  #pass NULL
  x <- validate_arg_f(
    f = NULL
  )

  testthat::expect_true(
    is.null(x)
  )

  #pass non-function
  testthat::expect_error(
    x <- validate_arg_f(
      f = "hola"
    ),
    regexp = "argument 'f' must be a uquoted function name without parentheses"
  )

  x <- validate_arg_f(f = f_count_rf)

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_error(
    x <- validate_arg_f(f = stats::lm),
    regexp = "the function 'f' must have the argument 'df'"
  )
})
