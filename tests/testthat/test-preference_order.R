testthat::test_that("`preference_order()` works", {

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
    ncol(df_preference) == 2
  )


  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% c("predictor", "preference"))
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
    ncol(df_preference) == 2
  )


  testthat::expect_true(
    nrow(df_preference) >= 1
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% c("predictor", "preference"))
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
    ncol(df_preference) == 2
  )


  testthat::expect_true(
    nrow(df_preference) >= 1
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% c("predictor", "preference"))
  )


  #categorical response and predictors
  df_preference <- preference_order(
    df = vi,
    response = "vi_category",
    predictors = vi_predictors_categorical,
    f = f_v,
    warn_limit = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(df_preference)
  )

  testthat::expect_true(
    ncol(df_preference) == 2
  )


  testthat::expect_true(
    nrow(df_preference) >= 1
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors_categorical)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% c("predictor", "preference"))
  )




  #categorical response and categorical and numeric predictors
  df_preference <- preference_order(
    df = vi,
    response = "vi_category",
    predictors = vi_predictors_numeric,
    f = f_v_rf_categorical,
    warn_limit = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    is.data.frame(df_preference)
  )

  testthat::expect_true(
    ncol(df_preference) == 2
  )


  testthat::expect_true(
    nrow(df_preference) >= 1
  )

  testthat::expect_true(
    nrow(df_preference) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(colnames(df_preference) %in% c("predictor", "preference"))
  )

})
