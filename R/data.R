#' Example Data With Different Response and Predictor Types
#'
#' @description
#'
#' The response variable is a Vegetation Index encoded in different ways to help highlight the package capabilities:
#' \itemize{
#'  \item \code{vi_numeric}: continuous vegetation index values in the range 0-1.
#'  \item \code{vi_counts}: simulated integer counts created by multiplying \code{vi_numeric} by 1000 and coercing the result to integer.
#'  \item \code{vi_binomial}: simulated binomial variable created by transforming \code{vi_numeric} to zeros and ones.
#'  \item \code{vi_categorical}: character variable with the categories "very_low", "low", "medium", "high", and "very_high", with thresholds located at the quantiles of \code{vi_numeric}.
#'  \item \code{vi_factor}: \code{vi_categorical} converted to factor.
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

#' Small Version (100 rows) of the Dataset \code{vi}
#'
#' @description
#'
#' Same as dataset [vi], but with only 100 rows. Used in code examples to shorten runtime.
#'
#' @usage data(vi_smol)
#' @seealso [vi_predictors]
#'
#' @format Data frame with 100 rows and 68 columns.
#' @family example_data
"vi_smol"

#' Response Names in Example Data Frame `vi`
#'
#' @usage data(vi_responses)
#' @seealso [vi]
#'
#' @format Character vector with response names.
#' @family example_data
"vi_responses"

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
#'   \item \code{y}: response variable generated from \code{a * 0.75 + b * 0.25 + noise}.
#'   \item \code{a}: most important predictor of \code{y}, uncorrelated with \code{b}.
#'   \item \code{b}: second most important predictor of \code{y}, uncorrelated with \code{a}.
#'   \item \code{c}: generated from \code{a + noise}.
#'   \item \code{d}: generated from \code{(a + b)/2 + noise}.
#' }
#'
#' These are variance inflation factors of the predictors in \code{toy}.
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
