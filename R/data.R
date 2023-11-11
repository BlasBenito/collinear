#' 30.000 records of responses and predictors all over the world
#'
#' @usage data(vi)
#' @seealso [vi_predictors]
#'
#' @format Data frame with 30.000 rows and 68 columns.
"vi"

#' Predictor names in data frame 'vi'
#'
#' @usage data(vi_predictors)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
"vi_predictors"


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
#' @examples
#'
#' library(collinear)
#' library(dplyr)
#' data(vi)
#' set.seed(1)
#' toy <- vi |>
#'   dplyr::slice_sample(n = 2000) |>
#'   dplyr::transmute(
#'     a = soil_clay,
#'     b = humidity_range
#'   ) |>
#'   scale() |>
#'   as.data.frame() |>
#'   dplyr::mutate(
#'     y = a * 0.75 + b * 0.25 + runif(n = dplyr::n(), min = -0.5, max = 0.5),
#'     c = a + runif(n = dplyr::n(), min = -0.5, max = 0.5),
#'     d = (a + b) / 2 + runif(n = dplyr::n(), min = -0.5, max = 0.5)
#'   ) |>
#'   dplyr::transmute(y, a, b, c, d)
#'
#' @usage data(toy)
#'
#' @format Data frame with 2000 rows and 5 columns.
"toy"
