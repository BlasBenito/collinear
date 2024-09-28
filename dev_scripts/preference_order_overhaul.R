#'
#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #continuous response and predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_numeric"]],
#'   x = vi[["swi_max"]]
#' ) |>
#'   na.omit()
#'
#' # Continuous response
#'
#' #Pearson R-squared
#' f_r2_pearson(df = df)
#'
#' #Spearman R-squared
#' f_r2_spearman(df = df)
#'
#' #R-squared of a gaussian gam
#' f_r2_glm_gaussian(df = df)
#'
#' #gaussian glm with second-degree polynomials
#' f_r2_glm_gaussian_poly2(df = df)
#'
#' #R-squared of a gaussian gam
#' f_r2_gam_gaussian(df = df)
#'
#' #recursive partition tree
#' f_r2_rpart(df = df)
#'
#' #random forest model
#' f_r2_rf(df = df)





#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #integer counts response and continuous predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_counts"]],
#'   x = vi[["swi_max"]]
#' ) |>
#'   na.omit()
#'
#' #GLM model with Poisson family
#' f_r2_glm_poisson(df = df)
#'
#' #GLM model with second degree polynomials and Poisson family
#' f_r2_glm_poisson_poly2(df = df)
#'
#' #GAM model with Poisson family
#' f_r2_gam_poisson(df = df)



#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #integer counts response and continuous predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_binomial"]],
#'   x = vi[["swi_max"]]
#' ) |>
#'   na.omit()
#'
#' #AUC of GLM with binomial response and weighted cases
#' f_auc_glm_binomial(df = df)
#'
#' #AUC of GLM as above plus second degree polynomials
#' f_auc_glm_binomial_poly2(df = df)
#'
#' #AUC of binomial GAM with weighted cases
#' f_auc_gam_binomial(df = df)
#'
#' #AUC of recursive partition tree with weighted cases
#' f_auc_rpart(df = df)
#'
#' #AUC of random forest with weighted cases
#' f_auc_rf(df = df)

#CATEGORICAL RESPONSE

#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #categorical response and predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_factor"]],
#'   x = vi[["soil_type"]]
#' ) |>
#'   na.omit()
#'
#' #Cramer's V of Random Forest model
#' f_cramerv_rf_categorical(df = df)
#'
#' #categorical response and numeric predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_factor"]],
#'   x = vi[["swi_mean"]]
#' ) |>
#'   na.omit()
#'
#' #Cramer's V of Random Forest model
#' f_cramerv_rf_categorical(df = df)





f_cramerv_rf_categorical <- function(df){

  df[["y"]] <- as.factor(df[["y"]])

  m <- ranger::ranger(
    formula = y ~ x,
    data = df,
    case.weights = case_weights(
      x = df[["y"]]
    ),
    num.threads = 1,
    num.trees = 100,
    min.node.size = ceiling(nrow(df)/100),
    seed = 1
  )

  p <- stats::predict(
    object = m,
    data = df
  )$predictions

  cramer_v(
    x = df[["y"]],
    y = p,
    check_input = FALSE
  )

}

