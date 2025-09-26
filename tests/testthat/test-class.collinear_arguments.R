testthat::test_that("`class.collinear_arguments()` works", {

  data(vi_smol, vi_predictors)

  testthat::expect_error(
    x <- class.collinear_arguments(
      df = NULL,
      responses = NULL,
      predictors = NULL,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      f_name = NULL,
      max_cor = NULL,
      max_vif = NULL,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  testthat::expect_error(
    x <- class.collinear_arguments(
      df = vi_smol,
      responses = NULL,
      predictors = NULL,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      f_name = NULL,
      max_cor = NULL,
      max_vif = NULL,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "arguments 'max_cor' and 'max_vif' cannot be NULL at once"
  )

  testthat::expect_message(
    x <- class.collinear_arguments(
      df = vi_smol,
      responses = NULL,
      predictors = NULL,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      f_name = NULL,
      max_cor = 0.5,
      max_vif = 2.5,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "Inf, -Inf, or NaN values"
  )

  #dataframes are not the same
  testthat::expect_false(
    isTRUE(all.equal(
      target = vi_smol,
      current = x$df
    ))
  )

  #predictors are df colnames
  testthat::expect_true(
    all(x$predictors %in% colnames(vi_smol))
  )


  testthat::expect_message(
    x <- class.collinear_arguments(
      df = vi_smol,
      responses = NULL,
      predictors = NULL,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      f_name = NULL,
      max_cor = 10,
      max_vif = 20,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'max_cor' is outside its valid range"
  ) |>
    suppressMessages()

  testthat::expect_true(
    x$max_cor == 0.7
  )

  testthat::expect_message(
    x <- class.collinear_arguments(
      df = vi_smol,
      responses = NULL,
      predictors = NULL,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      f_name = NULL,
      max_cor = 10,
      max_vif = 20,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'max_vif' is outside its valid range"
  ) |>
    suppressMessages()

  testthat::expect_true(
    x$max_vif == 5
  )


  testthat::expect_message(
    x <- class.collinear_arguments(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_categorical,
      encoding_method = "lol",
      preference_order = NULL,
      f = NULL,
      f_name = NULL,
      max_cor = 10,
      max_vif = 20,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'encoding_method' is not valid"
  ) |>
    suppressMessages()

  testthat::expect_true(
    x$encoding_method == "loo"
  )

  testthat::expect_message(
    x <- class.collinear_arguments(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_categorical,
      encoding_method = "loo",
      preference_order = c("hola"),
      f = NULL,
      f_name = NULL,
      max_cor = 10,
      max_vif = 20,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "character vector 'preference_order' does not contain"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- class.collinear_arguments(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_categorical,
      encoding_method = "loo",
      preference_order = data.frame(
        response = "vi_numeric",
        predictor = "hola"
      ),
      f = NULL,
      f_name = NULL,
      max_cor = 10,
      max_vif = 20,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "character vector 'preference_order' does not contain"
  ) |>
    suppressMessages()

})
