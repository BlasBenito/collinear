testthat::test_that("`validate_arg_preference_order()` works", {

  data(
    vi_smol,
    vi_predictors_numeric
  )

  #no arguments
  testthat::expect_error(
    x <- validate_arg_preference_order(
      df = NULL,
      predictors = NULL,
      preference_order = NULL,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #df only
  x <- validate_arg_preference_order(
    df = vi_smol[, vi_predictors_numeric[1:5]],
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric[1:5])
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    all(vi_predictors_numeric[1:5] %in% x$predictor)
  )

  #df and predictors
  x <- validate_arg_preference_order(
    df = vi_smol,
    response = NULL,
    predictors = vi_predictors_numeric[1:5],
    preference_order = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    all(vi_predictors_numeric[1:5] %in% x$predictor)
  )

  #using preference_order
  x <- validate_arg_preference_order(
    df = vi_smol,
    response = NULL,
    predictors = vi_predictors_numeric[1:10],
    preference_order = vi_predictors_numeric[1:10],
    quiet = FALSE
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    all(vi_predictors_numeric[1:10] %in% x$predictor)
  )

  #incomplete preference order
  x <- validate_arg_preference_order(
    df = vi_smol,
    response = NULL,
    predictors = vi_predictors_numeric[1:10],
    preference_order = vi_predictors_numeric[10:5],
    quiet = FALSE
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    x$predictor[1] == vi_predictors_numeric[10]
  )

  testthat::expect_true(
    all(vi_predictors_numeric[1:10] %in% x$predictor)
  )


  #using data frame from preference_order()
  preference_df <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:12],
    quiet = TRUE
  )

  x <- validate_arg_preference_order(
    df = vi_smol,
    predictors = vi_predictors_numeric[1:10],
    preference_order = preference_df,
    function_name = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    length(setdiff(preference_df$predictor, vi_predictors_numeric[1:10])) == 2
  )

  #using response
  preference_df <- preference_order(
    df = vi_smol,
    responses = c("vi_numeric", "vi_binomial"),
    predictors = vi_predictors_numeric[1:12],
    quiet = TRUE
  )

  x <- validate_arg_preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:10],
    preference_order = preference_df,
    function_name = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )

  testthat::expect_true(
    length(setdiff(preference_df$predictor, vi_predictors_numeric[1:10])) == 2
  )

  #edge cases
  preference_df <- preference_order(
    df = vi_smol,
    responses = c("vi_numeric", "vi_binomial"),,
    predictors = vi_predictors_numeric[1:12],
    quiet = TRUE
  )

  testthat::expect_message(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response = "wrong_response",
      predictors = vi_predictors_numeric[1:10],
      preference_order = preference_df,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'response' does not contain column names of 'df'"
  )

  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )



})
