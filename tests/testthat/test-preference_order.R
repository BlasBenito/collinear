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
    vi_predictors_categorical
  )

  #subsets to limit example run time
  vi <- vi[1:1000, ]
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

  df_preference <- preference_order(
    df = vi,
    response = responses,
    predictors = vi_predictors,
    quiet = TRUE,
    warn_limit = NULL
  )

  testthat::expect_true(
    is.list(df_preference)
  )

  testthat::expect_true(
    all(names(df_preference) %in% responses)
  )

  testthat::expect_true(
    all(colnames(df_preference[[1]]) %in% expected_colnames)
  )

  #numeric response
  testthat::expect_message(
    df_preference <- preference_order(
      df = vi,
      response = "vi_numeric",
      predictors = vi_predictors,
      f = NULL,
      warn_limit = NULL
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.data.frame(df_preference)
  )

  testthat::expect_true(
    ncol(df_preference) == length(expected_colnames)
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% expected_colnames)
  )


  #count response
  df_preference <- preference_order(
    df = vi,
    response = "vi_counts",
    predictors = vi_predictors_numeric,
    f = f_r2_glm_poisson,
    warn_limit = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(df_preference)
  )

  testthat::expect_true(
    ncol(df_preference) == length(expected_colnames)
  )


  testthat::expect_true(
    nrow(df_preference) >= 1
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% expected_colnames)
  )




  #binomial response
  df_preference <- preference_order(
    df = vi,
    response = "vi_binomial",
    predictors = vi_predictors_numeric,
    f = f_auc_glm_binomial,
    warn_limit = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(df_preference)
  )

  testthat::expect_true(
    ncol(df_preference) == length(expected_colnames)
  )


  testthat::expect_true(
    nrow(df_preference) >= 1
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% expected_colnames)
  )



  #categorical response and predictors
  df_preference <- preference_order(
    df = vi,
    response = "vi_categorical",
    predictors = vi_predictors_categorical,
    f = f_v,
    warn_limit = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(df_preference)
  )

  testthat::expect_true(
    ncol(df_preference) == length(expected_colnames)
  )


  testthat::expect_true(
    nrow(df_preference) >= 1
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors_categorical)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% expected_colnames)
  )

  #categorical response and categorical and numeric predictors
  df_preference <- preference_order(
    df = vi,
    response = "vi_categorical",
    predictors = vi_predictors_numeric,
    f = f_v_rf_categorical,
    warn_limit = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(df_preference)
  )

  testthat::expect_true(
    ncol(df_preference) == length(expected_colnames)
  )


  testthat::expect_true(
    nrow(df_preference) >= 1
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% expected_colnames)
  )


})
