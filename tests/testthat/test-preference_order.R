testthat::test_that("`preference_order()` works", {

  expected_colnames <- c(
    "response",
    "predictor",
    "f",
    "preference"
  )

  data(
    vi,
    vi_predictors,
    vi_predictors_categorical,
    vi_predictors_numeric
  )

  #subsets to limit example run time
  df <- vi[1:1000, ]
  vi_predictors <- vi_predictors[1:10]
  vi_predictors_numeric <- vi_predictors_numeric[1:10]
  vi_predictors_categorical <- vi_predictors_categorical[1:10]

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
      df = df,
      response = responses,
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "processing response 'vi_numeric'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- preference_order(
      df = df,
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
      df = df,
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
      df = df,
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
      df = df,
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
    is.list(x)
  )

  testthat::expect_true(
    all(names(x) %in% responses)
  )

  testthat::expect_true(
    all(colnames(x[[1]]) %in% expected_colnames)
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
    df = df,
    response = "vi_numeric",
    predictors = vi_predictors,
    f = f_r2_rf,
    quiet = FALSE,
    warn_limit = NULL
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
      df = df,
      response = "vi_numeric",
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "f_r2_rf"
  )


  #count response

  ## all types
  x <- preference_order(
    df = df,
    response = "vi_counts",
    predictors = vi_predictors_numeric,
    f = f_r2_glm_poisson,
    warn_limit = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_r2_glm_poisson"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  #wrong f function
  testthat::expect_message(
    x <- preference_order(
      df = df,
      response = "vi_counts",
      predictors = vi_predictors,
      f = f_auto,
      warn_limit = NULL,
      quiet = FALSE
    ),
    regexp = "f_r2_rf"
  )

  #binomial response
  x <- preference_order(
    df = df,
    response = "vi_binomial",
    predictors = vi_predictors_numeric,
    f = f_auc_glm_binomial,
    warn_limit = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_auc_glm_binomial"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  testthat::expect_message(
    x <- preference_order(
      df = df,
      response = "vi_binomial",
      predictors = vi_predictors,
      f = f_auto,
      quiet = FALSE,
      warn_limit = NULL
    ),
    regexp = "f_auc_rf"
  )

  #categorical response and predictors
  x <- preference_order(
    df = df,
    response = "vi_categorical",
    predictors = vi_predictors_categorical,
    f = f_v,
    warn_limit = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_v"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  testthat::expect_message(
    x <- preference_order(
      df = df,
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
    df = df,
    response = "vi_categorical",
    predictors = vi_predictors_numeric,
    f = f_v_rf_categorical,
    warn_limit = NULL,
    quiet = FALSE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_v_rf_categorical"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  testthat::expect_message(
    x <- preference_order(
      df = df,
      response = "vi_categorical",
      predictors = vi_predictors_numeric,
      f = f_auto,
      warn_limit = NULL,
      quiet = FALSE
    ),
    regexp = "f_v_rf_categorical"
  )



})
