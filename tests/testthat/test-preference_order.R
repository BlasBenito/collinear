testthat::test_that("`preference_order()` works", {

  expected_colnames <- c(
    "response",
    "predictor",
    "preference",
    "f",
    "metric"
  )

  data(
    vi_smol,
    vi_predictors,
    vi_predictors_categorical,
    vi_predictors_numeric
  )

  #several responses
  responses <- c(
    "vi_numeric",
    "vi_counts",
    "vi_binomial",
    "vi_categorical",
    "vi_factor"
  )

  #full use case

  #test that one message per response is printed
  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = responses,
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE
    ),
    regexp = "processing response 'vi_numeric'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = responses,
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "processing response 'vi_counts'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = responses,
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "processing response 'vi_binomial'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = responses,
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "processing response 'vi_categorical'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = responses,
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "processing response 'vi_factor'"
  ) |>
    suppressMessages()


  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(responses %in% x$response)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  #f NULL
  testthat::expect_message(
    x <- preference_order(
      df = df,
      response = "vi_numeric",
      predictors = vi_predictors,
      f = NULL,
      warn_limit = NULL
    ),
    regexp = "argument 'f' is NULL"
  )

  #numeric response

  ##all types
  x <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors,
    f = f_r2_rf,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_r2_rf"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )


  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "f_r2_rf"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_r2_rf"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )


  #count response

  ## all types
  x <- preference_order(
    df = vi_smol,
    response = "vi_counts",
    predictors = vi_predictors_numeric,
    f = f_r2_glm_poisson,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_r2_glm_poisson"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  #wrong f function
  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = "vi_counts",
      predictors = vi_predictors,
      f = f_auto,
      warn_limit = NULL,
      quiet = FALSE
    ),
    regexp = "f_r2_rf"
  ) |>
    suppressMessages()

  #binomial response
  x <- preference_order(
    df = vi_smol,
    response = "vi_binomial",
    predictors = vi_predictors_numeric,
    f = f_auc_glm_binomial,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_auc_glm_binomial"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = "vi_binomial",
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "f_auc_rf"
  ) |>
    suppressMessages()

  #categorical response and predictors
  x <- preference_order(
    df = vi_smol,
    response = "vi_categorical",
    predictors = vi_predictors_categorical,
    f = f_v,
    quiet = FALSE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_v"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_categorical)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = "vi_categorical",
      predictors = vi_predictors_categorical,
      f = f_auto,
      warn_limit = NULL,
      quiet = FALSE
    ),
    regexp = "f_v"
  )


  #categorical response and categorical and numeric predictors
  x <- preference_order(
    df = vi_smol,
    response = "vi_categorical",
    predictors = vi_predictors_numeric,
    f = f_v_rf_categorical,
    warn_limit = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_v_rf_categorical"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = "vi_categorical",
      predictors = vi_predictors_numeric,
      f = f_auto,
      warn_limit = NULL,
      quiet = FALSE
    ),
    regexp = "f_v_rf_categorical"
  ) |>
    suppressMessages()

  #warn limit
  testthat::expect_warning(
    x <- preference_order(
      df = vi_smol,
      response = "vi_categorical",
      predictors = vi_predictors_numeric,
      f = f_auto,
      warn_limit = 0.5,
      quiet = FALSE
    ),
    regexp = "predictors with associations to 'vi_categorical' higher than 'warn_limit'"
  ) |>
    suppressMessages()



})
