#' Example Data With Different Response and Predictor Types
#'
#' @description
#'
#' The response variable is a Vegetation Index encoded in different ways to help highlight the package capabilities:
#' \itemize{
#'  \item `vi_numeric`: continuous vegetation index values in the range 0-1.
#'  \item `vi_counts`: simulated integer counts created by multiplying `vi_numeric` by 1000 and coercing the result to integer.
#'  \item `vi_binomial`: simulated binomial variable created by transforming `vi_numeric` to zeros and ones.
#'  \item `vi_categorical`: character variable with the categories "very_low", "low", "medium", "high", and "very_high", with thresholds located at the quantiles of `vi_numeric`.
#'  \item `vi_factor`: `vi_categorical` converted to factor.
#' }
#'
#' The names of all predictors (continuous, integer, character, and factors) are in [vi_predictors].
#'
#' @usage data(vi)
#' @seealso [vi_predictors]
#'
#' @format Data frame with 30.000 rows and 68 columns.
#' @family example_data
"vi"

#' All Predictor Names in Example Data Frame vi
#'
#' @usage data(vi_predictors)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors"

#' All Numeric Predictor Names in Example Data Frame vi
#'
#' @usage data(vi_predictors_numeric)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors_numeric"

#' All Categorical and Factor Predictor Names in Example Data Frame vi
#'
#' @usage data(vi_predictors_categorical)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors_categorical"


#' One response and four predictors with varying levels of multicollinearity
#'
#' Data frame with known relationship between responses and predictors useful
#' to illustrate multicollinearity concepts. Created from [vi] using the code
#' shown in the example.
#'
#' Columns:
#' \itemize{
#'   \item `y`: response variable generated from `a * 0.75 + b * 0.25 + noise`.
#'   \item `a`: most important predictor of `y`, uncorrelated with `b`.
#'   \item `b`: second most important predictor of `y`, uncorrelated with `a`.
#'   \item `c`: generated from `a + noise`.
#'   \item `d`: generated from `(a + b)/2 + noise`.
#' }
#'
#' These are variance inflation factors of the predictors in `toy`.
#' variable vif
#'  b       4.062
#'  d       6.804
#'  c       13.263
#'  a       16.161
#'
#' @usage data(toy)
#'
#' @format Data frame with 2000 rows and 5 columns.
#' @family example_data
"toy"
