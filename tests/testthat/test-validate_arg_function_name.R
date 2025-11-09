testthat::test_that("`validate_arg_function_name()` works", {

  x <- validate_arg_function_name(
    default_name = NULL,
    function_name = NULL
  )

  testthat::expect_null(x)

  x <- validate_arg_function_name(
    default_name = "hola",
    function_name = NULL
  )

  testthat::expect_true(
    x == "hola"
  )

  x <- validate_arg_function_name(
    default_name = "hola",
    function_name = "adios"
  )

  testthat::expect_true(
    grepl(pattern = "hola", x = x)
  )

  testthat::expect_true(
    grepl(pattern = "adios", x = x)
  )

})
