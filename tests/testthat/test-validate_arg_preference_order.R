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

  # df only ----
  testthat::expect_message(
    x <- validate_arg_preference_order(
      df = vi_smol[, vi_predictors_numeric],
      response = NULL,
      predictors = NULL,
      preference_order = NULL,
      quiet = FALSE
    ),
    regexp = "from lower to higher multicollinearity"
  ) |>
    suppressMessages()

  testthat::expect_true(
    unique(x$metric) == "1 - R-squared"
  )

  testthat::expect_true(
    unique(x$f) == "stats::cor()"
  )

  #pass validated arg
  x <- validate_arg_preference_order(
    df = vi_smol,
    response = NULL,
    predictors = vi_predictors_numeric,
    preference_order = x
  )


  #df and response, no predictors ----
  testthat::expect_message(
    x <- validate_arg_preference_order(
      df = vi_smol[, c("vi_numeric", vi_predictors_numeric)],
      response = "vi_numeric",
      predictors = NULL,
      preference_order = NULL,
      quiet = FALSE
    ),
    regexp = "from lower to higher multicollinearity"
  ) |>
    suppressMessages()

  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )


  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    all(vi_predictors_numeric %in% x$predictor)
  )

  #df and two responses
  testthat::expect_message(
    x <- validate_arg_preference_order(
      df = vi_smol[, c("vi_numeric", "vi_binomial", vi_predictors_numeric)],
      response = c("vi_numeric", "vi_binomial"),
      predictors = vi_predictors_numeric,
      preference_order = vi_predictors_numeric,
      quiet = FALSE
    ),
    regexp = "argument 'response' must be of length 1, using response"
  ) |>
    suppressMessages()

  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )

  #auto
  testthat::expect_message(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric[1:10],
      preference_order = "auto",
      quiet = FALSE
    ),
    regexp = "value 'auto' for the argument 'preference_order' is deprecated"
  ) |>
    suppressMessages()


  #preference_order vector ----
  x <- validate_arg_preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:10],
    preference_order = vi_predictors_numeric[1:10],
    quiet = FALSE
  )

  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric[1:10])
  )

  testthat::expect_true(
    all(vi_predictors_numeric[1:10] %in% x$predictor)
  )

  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )

  testthat::expect_true(
    is.na(unique(x$f))
  )

  testthat::expect_true(
    unique(x$metric) == "user_preference"
  )


  #incomplete preference order vector ----
  testthat::expect_message(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric[1:10],
      preference_order = vi_predictors_numeric[10:5],
      quiet = FALSE
    ),
    regexp = "4 'predictors' from lower to higher multicollinearity"
  )


  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric[1:10])
  )

  testthat::expect_true(
    x$predictor[1] == vi_predictors_numeric[10]
  )

  testthat::expect_true(
    all(vi_predictors_numeric[1:10] %in% x$predictor)
  )

  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )

  testthat::expect_true(
    all(unique(x$metric) %in% c("user_preference", "1 - R-squared"))
  )


  #preference_order dataframe ----
  preference_df <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:12],
    quiet = TRUE
  )

  #missing one predictor
  x <- validate_arg_preference_order(
    df = vi_smol,
    response  = "vi_numeric",
    predictors = vi_predictors_numeric[1:13],
    preference_order = preference_df,
    quiet = FALSE
  )

  testthat::expect_true(
    all(vi_predictors_numeric[1:13] %in% x$predictor)
  )

  #wrong response
  testthat::expect_error(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response  = "vi_binomial",
      predictors = vi_predictors_numeric[1:12],
      preference_order = preference_df,
      quiet = FALSE
    ),
    regexp = "argument 'response' does not match the column 'response' of the dataframe 'preference_order'"
  ) |>
    suppressMessages()

  #wrong column names
  testthat::expect_error(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response  = "vi_numeric",
      predictors = vi_predictors_numeric[1:10],
      preference_order = preference_df[, c("response", "predictor")],
      quiet = FALSE
    ),
    regexp = "dataframe 'preference_order' must have these columns"
  ) |>
    suppressMessages()

  #wrong predictors
  testthat::expect_error(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response  = "vi_numeric",
      predictors = vi_predictors_numeric[13:20],
      preference_order = preference_df,
      quiet = FALSE
    ),
    regexp = "does not contain any 'predictors'"
  ) |>
    suppressMessages()


  x <- validate_arg_preference_order(
    df = vi_smol,
    response  = "vi_numeric",
    predictors = vi_predictors_numeric[1:10],
    preference_order = preference_df,
    quiet = FALSE
  )


  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric[1:10])
  )

  testthat::expect_true(
    x$predictor[1] == preference_df[1, "predictor"]
  )

  testthat::expect_true(
    all(vi_predictors_numeric[1:10] %in% x$predictor)
  )

  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )

  testthat::expect_true(
    unique(x$f) == "f_numeric_glm"
  )


  #filtering responses ----
  preference_df <- preference_order(
    df = vi_smol,
    responses = c("vi_numeric", "vi_binomial"),
    predictors = vi_predictors_numeric[1:12],
    quiet = TRUE
  )

  testthat::expect_true(
    all(c("vi_numeric", "vi_binomial") %in% preference_df$response)
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
    attributes(x)$validated
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric[1:10])
  )

  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )

  testthat::expect_true(
    length(setdiff(preference_df$predictor, vi_predictors_numeric[1:10])) == 2
  )

  #wrong response ----
  preference_df <- preference_order(
    df = vi_smol,
    responses = c("vi_numeric", "vi_binomial"),
    predictors = vi_predictors_numeric[1:12],
    quiet = TRUE
  )

  testthat::expect_error(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response = "wrong_response",
      predictors = vi_predictors_numeric[1:10],
      preference_order = preference_df,
      function_name = NULL,
      quiet = TRUE
    ),
    regexp = "dataframe 'preference_order' contains more than one response"
  )

  #several responses ----
  testthat::expect_message(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response = c("vi_numeric", "vi_binomial"),
      predictors = vi_predictors_numeric[1:10],
      preference_order = preference_df,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'response' must be of length 1"
  )


  testthat::expect_true(
    attributes(x)$validated
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric[1:10])
  )


  testthat::expect_true(
    unique(x$response) == "vi_numeric"
  )

  #no response ----
  preference_df <- preference_order(
    df = vi_smol,
    response = NULL,
    predictors = vi_predictors_numeric[1:12],
    quiet = TRUE
  )

  testthat::expect_error(
    x <- validate_arg_preference_order(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric[1:12],
      preference_order = preference_df,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'response' does not match the column 'response' of the dataframe 'preference_order'"
  )


})
