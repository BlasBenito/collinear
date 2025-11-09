testthat::test_that("`preference_order_wrapper()` works", {

  #load data
  data(
    vi,
    vi_smol,
    vi_predictors,
    vi_predictors_numeric,
    vi_predictors_categorical,
    vi_responses
  )


  # PREFERENCE ORDER ----

  ## no target encoding ----

  ### invalid character vector ----
  preference_order <- c(
    "hola",
    "adios"
  )

  testthat::expect_message(
    x <- preference_order_wrapper(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
      preference_order = preference_order,
      f = NULL,
      f_name = NULL,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "vector 'preference_order' does not contain valid column names in 'df'"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.null(x)
  )

  ### valid character vector ----

  #uncorrelated, must show up in selection
  preference_order <- c(
    "soil_soc",
    "growing_season_temperature"
  )

  x <- preference_order_wrapper(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    preference_order = preference_order,
    f = NULL,
    f_name = NULL,
    function_name = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(preference_order %in% x$predictor)
  )

  ### valid dataframe ----
  preference_df <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    f = f_numeric_glm,
    quiet = TRUE
  )

  x <- preference_order_wrapper(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    preference_order = preference_df,
    f = NULL,
    f_name = NULL,
    function_name = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    all.equal(
      target = x,
      current = preference_df
    )
  )

  testthat::expect_true(
    x$f[1] == "f_numeric_glm"
  )


  ### invalid data frame ----
  preference_df$response <- "hola"
  preference_df$f <- NULL

  testthat::expect_message(
    x <- preference_order_wrapper(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
      preference_order = preference_df,
      f = NULL,
      f_name = NULL,
      function_name = NULL,
      quiet = FALSE
    ),
    regexp = "dataframe 'preference_order' requires the columns"
  ) |>
    suppressMessages()

  testthat::expect_null(
    x
  )


  ### NULL response ----
  #same as above!
  preference <- c(
    "soil_soc",
    "growing_season_temperature"
  )

  x <- preference_order_wrapper(
    df = vi_smol,
    response = NULL,
    predictors = vi_predictors_numeric,
    preference_order = preference,
    f = NULL,
    f_name = NULL,
    function_name = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    all(preference %in% x$predictor)
  )


  ### f_auto ----
  testthat::expect_message(
    x <- preference_order_wrapper(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
      preference_order = NULL,
      f = f_auto,
      quiet = FALSE
    ),
    regexp = "f_numeric_glm"
  ) |>
    suppressMessages()

  testthat::expect_true(
    all(x$f == "f_numeric_glm")
  )


  ### f_numeric_rf ----
  x <- preference_order_wrapper(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    preference_order = NULL,
    f = f_numeric_rf,
    f_name = "f_numeric_rf",
    quiet = TRUE
  )

  testthat::expect_true(
    all(x$f == "f_numeric_rf")
  )

  ### bad function name ----
  my_f <- function(){return(NULL)}

  testthat::expect_error(
    x <- preference_order_wrapper(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors,
      preference_order = NULL,
      f = my_f,
      quiet = TRUE
    ),
    regexp = "to receive a data frame with the column names"
  )

  ### character function name ----
  testthat::expect_error(
    x <- preference_order_wrapper(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors,
      preference_order = NULL,
      f = "my_f",
      quiet = TRUE
    ),
    regexp = "must be a uquoted function name"
  )




})
