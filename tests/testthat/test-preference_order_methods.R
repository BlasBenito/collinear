testthat::test_that("Preference order methods work.", {

  #load example data
  data(vi)

  #reduce size to speed-up example
  vi <- vi[1:1000, ]

  #continuous response and predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi[["vi_numeric"]],
    x = vi[["swi_max"]]
  ) |>
    na.omit()

  # Continuous response

  #Pearson R-squared
  testthat::expect_true(
    is.numeric( f_r2_pearson(df = df))
  )

  #Spearman R-squared
  testthat::expect_true(
    is.numeric(f_r2_spearman(df = df))
  )


  #R-squared of a gaussian gam
  testthat::expect_true(
    is.numeric(f_r2_glm_gaussian(df = df))
  )


  #gaussian glm with second-degree polynomials
  testthat::expect_true(
    is.numeric(f_r2_glm_gaussian_poly2(df = df))
  )


  #R-squared of a gaussian gam
  testthat::expect_true(
    is.numeric(f_r2_gam_gaussian(df = df))
  )


  #recursive partition tree
  testthat::expect_true(
    is.numeric(f_r2_rpart(df = df))
  )


  #random forest model
  testthat::expect_true(
    is.numeric(f_r2_rf(df = df))
  )






  #integer counts response and continuous predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi[["vi_counts"]],
    x = vi[["swi_max"]]
  ) |>
    na.omit()

  #GLM model with Poisson family
  testthat::expect_true(
    is.numeric( f_r2_glm_poisson(df = df))
  )


  #GLM model with second degree polynomials and Poisson family
  testthat::expect_true(
    is.numeric(f_r2_glm_poisson_poly2(df = df))
  )


  #GAM model with Poisson family
  testthat::expect_true(
    is.numeric(f_r2_gam_poisson(df = df))
  )




  #integer counts response and continuous predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi[["vi_binomial"]],
    x = vi[["swi_max"]]
  ) |>
    na.omit()

  #AUC of GLM with binomial response and weighted cases
  testthat::expect_true(
    is.numeric(f_auc_glm_binomial(df = df))
  )


  #AUC of GLM as above plus second degree polynomials
   testthat::expect_true(
     is.numeric(f_auc_glm_binomial_poly2(df = df))
   )


  #AUC of binomial GAM with weighted cases
  testthat::expect_true(
    is.numeric(f_auc_gam_binomial(df = df))
  )


  #AUC of recursive partition tree with weighted cases
  testthat::expect_true(
    is.numeric(f_auc_rpart(df = df))
  )


  #AUC of random forest with weighted cases
  testthat::expect_true(
    is.numeric(f_auc_rf(df = df))
  )


  #categorical response and predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi[["vi_factor"]],
    x = vi[["soil_type"]]
  ) |>
    na.omit()

  #Cramer's V of Random Forest model
  testthat::expect_true(
    is.numeric(  f_v(df = df))
  )


  #categorical response and numeric predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi[["vi_factor"]],
    x = vi[["swi_mean"]]
  ) |>
    na.omit()

  #Cramer's V of Random Forest model
  testthat::expect_true(
    is.numeric(  f_v_rf_categorical(df = df))
  )

})
