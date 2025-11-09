testthat::test_that("`validate_arg_max_vif()` works", {


  testthat::expect_message(
    x <- validate_arg_max_vif(
      max_vif = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'max_vif' is NULL, skipping VIF filtering"
  )

  testthat::expect_null(x)

  testthat::expect_message(
    x <- validate_arg_max_vif(
      max_vif = c(5, 10),
      quiet = FALSE
    ),
    regexp = "argument 'max_vif' must be of length one, using value '5'"
  )

  testthat::expect_true(
    x == 5
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_no_message(
    x <- validate_arg_max_vif(
      max_vif = x,
      quiet = FALSE
    )
  )


  testthat::expect_message(
    x <- validate_arg_max_vif(
      max_vif = "hola",
      quiet = FALSE
    ),
    regexp = "argument 'max_vif' is non-numeric, resetting it to to '5'"
  )

  testthat::expect_true(
    x == 5
  )

  testthat::expect_message(
    x <- validate_arg_max_vif(
      max_vif = 12,
      quiet = FALSE
    ),
    regexp = "is outside its valid range"
  )

  testthat::expect_true(
    x == 5
  )

  testthat::expect_true(
    attributes(x)$validated
  )



})
