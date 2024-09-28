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
#'
#' #tree models manage counts without issues too
#' f_r2_rpart(df = df)
#' f_r2_rf(df = df)






