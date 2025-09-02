testthat::test_that("`validate_arg_preference_order()` works", {

  data(
    vi_smol,
    vi_predictors,
    vi_predictors_numeric
    )

  #no arguments
  testthat::expect_error(
    preference_order <- validate_arg_preference_order(
      predictors = NULL,
      preference_order = NULL,
      preference_order_auto = NULL,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'predictors' must be validated"
  )

  #predictors not validated
  testthat::expect_error(
    preference_order <- validate_arg_preference_order(
      predictors = vi_predictors[1:5],
      preference_order = NULL,
      preference_order_auto = NULL,
      quiet = FALSE
    ),
    regexp = "must be validated"
  )

  #predictors validated
  predictors <- validate_arg_predictors(
    df = vi_smol,
    predictors = vi_predictors
  )

  testthat::expect_message(
    preference_order <- validate_arg_preference_order(
      predictors = predictors,
      preference_order = NULL,
      preference_order_auto = NULL,
      quiet = FALSE
    ),
    regexp = "using argument 'predictors' as preference order"
  )

  #missing predictor
  testthat::expect_message(
    preference_order <- validate_arg_preference_order(
      predictors = predictors,
      preference_order = c(vi_predictors, "hola"),
      preference_order_auto = vi_predictors,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "hola"
  )

  testthat::expect_true(
    attributes(preference_order)$validated
  )

  testthat::expect_true(
    all(preference_order %in% vi_predictors)
  )

  #using data frame from preference_order()
  preference_df <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_no_message(
    preference_order <- validate_arg_preference_order(
      predictors = predictors,
      preference_order = preference_df,
      preference_order_auto = vi_predictors,
      function_name = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(preference_order)$validated
  )

  testthat::expect_true(
    all(preference_order %in% vi_predictors)
  )


})
