testthat::test_that("`preference_order()` works", {

  expected_colnames <- c(
    "response",
    "predictor",
    "f",
    "metric",
    "score",
    "rank"
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

  #empty args

  testthat::expect_error(
    x <- preference_order(
      df = NULL,
      responses = NULL,
      predictors = NULL,
      f = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #default behavior without response and f
  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      responses = NULL,
      predictors = vi_predictors_numeric,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "predictors' from lower to higher multicollinearity"
  ) |>
    suppressMessages()


    testthat::expect_true(
      unique(x$metric) == "1 - R-squared"
    )

    testthat::expect_true(
      unique(x$response) == "none"
    )


    #no predictors
    x <- preference_order(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = NULL,
      f = f_auto,
      quiet = TRUE
    )

    #all df column names but vi_numeric
    testthat::expect_true(
      sum(colnames(vi_smol) %in% x$predictor) == ncol(vi_smol) - 1
    )

    testthat::expect_true(
      !"vi_numeric" %in% x$predictor
    )

    testthat::expect_true(
      "vi_numeric" %in% x$response
    )

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors,
      f = f_auto,
      cv_iterations = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'cv_iterations' must be a positive integer."
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors,
      f = f_auto,
      cv_iterations = 1,
      cv_training_fraction = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'cv_training_fraction' must be a numeric between 0.1 and 1"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors,
      f = f_auto,
      cv_iterations = 1,
      cv_training_fraction = 1,
      seed = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'seed' is invalid, resetting it to '1'"
  ) |>
    suppressMessages()

  #full use case

  #test that one message per response is printed
  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = c("vi_numeric", "vi_binomial"),
      predictors = vi_predictors_numeric,
      f = f_auto,
      quiet = FALSE
    ),
    regexp = "processing response 'vi_numeric'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- preference_order(
      df = vi_smol,
      response = c("vi_numeric", "vi_binomial"),
      predictors = vi_predictors_numeric,
      f = f_auto,
      quiet = FALSE
    ),
    regexp = "processing response 'vi_binomial'"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(c("vi_numeric", "vi_binomial") %in% x$response)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )



  #numeric response

  ##all types
  x <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors,
    f = f_numeric_rf,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_numeric_rf"
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
      quiet = FALSE
    ),
    regexp = "f_numeric_rf"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_numeric_rf"
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
    f = f_count_glm,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_count_glm"
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(x) %in% expected_colnames)
  )

  #wrong f function
  testthat::expect_error(
    x <- preference_order(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
      f = f_categorical_rf,
      quiet = FALSE
    ),
    regexp = "column 'y' of dataframe 'df' must be character or factor"
  ) |>
    suppressMessages()

  #binomial response
  x <- preference_order(
    df = vi_smol,
    response = "vi_binomial",
    predictors = vi_predictors_numeric,
    f = f_binomial_glm,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_binomial_glm"
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
      quiet = FALSE
    ),
    regexp = "f_binomial_rf"
  ) |>
    suppressMessages()

  #categorical response and predictors
  x <- preference_order(
    df = vi_smol,
    response = "vi_categorical",
    predictors = vi_predictors_categorical,
    f = f_categorical_rf,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_categorical_rf"
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
      quiet = FALSE
    ),
    regexp = "f_categorical_rf"
  )|>
    suppressMessages()


  #categorical response and categorical and numeric predictors
  x <- preference_order(
    df = vi_smol,
    response = "vi_categorical",
    predictors = vi_predictors_numeric,
    f = f_categorical_rf,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    unique(x$f) == "f_categorical_rf"
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
      quiet = FALSE
    ),
    regexp = "f_categorical_rf"
  ) |>
    suppressMessages()

  #custom function
  f_rsquared <- function(df, ...){
      stats::cor(
        x = df$x,
        y = df$y,
        use = "complete.obs"
      )^2
  }

  x <- preference_order(
    df = vi_smol,
    responses = "vi_numeric",
    predictors = vi_predictors_numeric[1:10],
    f = f_rsquared
  )

  testthat::expect_true(
    all(x$metric == "custom")
  )


})
