testthat::test_that("Preference order methods work.", {

  #load example data
  data(vi_smol)

  #continuous response and predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi_smol[["vi_numeric"]],
    x = vi_smol[["vi_numeric"]]
  ) |>
    na.omit()

  # Continuous response

  #Pearson R-squared
  testthat::expect_true(
    f_r2_pearson(df = df) == 1
  )

  #Spearman R-squared
  testthat::expect_true(
    f_r2_spearman(df = df) == 1
  )


  #R-squared of a gaussian gam
  testthat::expect_true(
    f_r2_glm_gaussian(df = df) == 1
  )


  #gaussian glm with second-degree polynomials
  testthat::expect_true(
    f_r2_glm_gaussian_poly2(df = df) == 1
  )


  #R-squared of a gaussian gam
  testthat::expect_true(
    f_r2_gam_gaussian(df = df) == 1
  )


  #recursive partition tree
  testthat::expect_true(
    f_r2_rpart(df = df) < 1
  )


  #random forest model
  testthat::expect_true(
    f_r2_rf(df = df) < 1
  )

  #continuous response and predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi_smol[["vi_numeric"]],
    x = vi_smol[["swi_mean"]]
  ) |>
    na.omit()

  # Continuous response

  #Pearson R-squared
  testthat::expect_true(
    f_r2_pearson(df = df) < 1
  )

  #Spearman R-squared
  testthat::expect_true(
    f_r2_spearman(df = df) < 1
  )


  #R-squared of a gaussian gam
  testthat::expect_true(
    f_r2_glm_gaussian(df = df) < 1
  )


  #gaussian glm with second-degree polynomials
  testthat::expect_true(
    f_r2_glm_gaussian_poly2(df = df) < 1
  )


  #R-squared of a gaussian gam
  testthat::expect_true(
    f_r2_gam_gaussian(df = df) < 1
  )


  #recursive partition tree
  testthat::expect_true(
    f_r2_rpart(df = df) < 1
  )


  #random forest model
  testthat::expect_true(
    f_r2_rf(df = df) < 1
  )


  #integer counts response and continuous predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi_smol[["vi_counts"]],
    x = vi_smol[["vi_counts"]]
  ) |>
    na.omit()

  #GLM model with Poisson family
  testthat::expect_true(
    f_r2_glm_poisson(df = df) < 1
  )


  #GLM model with second degree polynomials and Poisson family
  testthat::expect_true(
    f_r2_glm_poisson_poly2(df = df) < 1
  )


  #GAM model with Poisson family
  testthat::expect_true(
    f_r2_gam_poisson(df = df) < 1
  )

  df <- data.frame(
    y = vi_smol[["vi_counts"]],
    x = vi_smol[["swi_mean"]]
  ) |>
    na.omit()

  #GLM model with Poisson family
  testthat::expect_true(
    f_r2_glm_poisson(df = df) < 1
  )


  #GLM model with second degree polynomials and Poisson family
  testthat::expect_true(
    f_r2_glm_poisson_poly2(df = df) < 1
  )


  #GAM model with Poisson family
  testthat::expect_true(
    f_r2_gam_poisson(df = df) < 1
  )



  #integer counts response and continuous predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi_smol[["vi_binomial"]],
    x = vi_smol[["vi_binomial"]]
  ) |>
    na.omit()

  #AUC of GLM with binomial response and weighted cases
  testthat::expect_true(
    f_auc_glm_binomial(df = df) == 1
  )


  #AUC of GLM as above plus second degree polynomials
   testthat::expect_true(
     f_auc_glm_binomial_poly2(df = df) == 1
   )


  #AUC of binomial GAM with weighted cases
  testthat::expect_error(
    f_auc_gam_binomial(df = df),
    regexp  = "A term has fewer unique covariate combinations"
  )


  #AUC of recursive partition tree with weighted cases
  testthat::expect_true(
    f_auc_rpart(df = df) == 1
  )


  #AUC of random forest with weighted cases
  testthat::expect_true(
    is.numeric(f_auc_rf(df = df))
  )


  df <- data.frame(
    y = vi_smol[["vi_binomial"]],
    x = vi_smol[["swi_max"]]
  ) |>
    na.omit()

  #AUC of GLM with binomial response and weighted cases
  testthat::expect_true(
    f_auc_glm_binomial(df = df) < 1
  )


  #AUC of GLM as above plus second degree polynomials
  testthat::expect_true(
    f_auc_glm_binomial_poly2(df = df) < 1
  )


  #AUC of binomial GAM with weighted cases
  testthat::expect_true(
    f_auc_gam_binomial(df = df) < 1
  )


  #AUC of recursive partition tree with weighted cases
  testthat::expect_true(
    f_auc_rpart(df = df) < 1
  )


  #AUC of random forest with weighted cases
  testthat::expect_true(
    f_auc_rf(df = df) < 1
  )



  #categorical response and predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi_smol[["vi_factor"]],
    x = vi_smol[["vi_factor"]]
  ) |>
    na.omit()

  #Cramer's V of Random Forest model
  testthat::expect_true(
    f_v(df = df) == 1
  )

  testthat::expect_true(
    f_v_rf_categorical(df = df) == 1
  )


  #categorical response and numeric predictor
  #to data frame without NAs
  df <- data.frame(
    y = vi_smol[["vi_factor"]],
    x = vi_smol[["swi_mean"]]
  ) |>
    na.omit()

  #Cramer's V of Random Forest model
  testthat::expect_true(
    f_v(df = df) < 1
  )

  testthat::expect_true(
    f_v_rf_categorical(df = df) < 1
  )

})
