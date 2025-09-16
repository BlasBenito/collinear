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

#' Results of a Collinearity-Filtering Simulation
#'
#' A data frame summarising 10,000 random draws used to benchmark
#' the functions [cor_select()] and [vif_select()]. Each row records the input sampling parameters and the resulting feature-selection metrics.
#'
#' The source data is [vi] plus 200 synthetic columns, for a total of 249 columns and 30.000 rows.
#'
#' @format A data frame with 9354 rows and 6 variables:
#' \describe{
#'   \item{in_columns}{Number of predictor columns sampled.}
#'   \item{in_rows}{Number of rows sampled (≥30 per column).}
#'   \item{max_cor}{Maximum allowed pairwise correlation supplied to [cor_select()].}
#'   \item{max_vif}{Smallest VIF threshold at which [vif_select()] produced the most similar to [cor_select()] for the given \code{max_cor}.}
#'   \item{out_selection_length}{Number of variables returned from the multicollinearity filtering.}
#'   \item{out_selection_jaccard}{Jaccard similarity between the sets selected by the correlation and VIF procedures.}
#' }
#'
#' @examples
#' data(experiment_df)
#' str(experiment_df)
"experiment_df"

#' GAM Relating Maximum Correlation to VIF Threshold
#'
#' A fitted generalized additive model describing \code{max_vif} as a function of \code{max_cor} in [experiment_df].
#'
#' The model was fitted with a smooth term in \code{max_cor} (basis dimension \code{k = 9}) and cubic weights proportional to the Jaccard similarity between the results of [cor_select()] and [vif_select()] to emphasise cases with high agreement between the two selection methods.
#'
#' \preformatted{
#' gam_cor_to_vif <- mgcv::gam(
#'  formula = max_vif ~ s(max_cor, k = 9),
#'  weights = experiment_df$out_selection_jaccard^3,
#'  data = experiment_df,
#'  select = TRUE
#'  )
#'}
#' Model performance:
#' \itemize{
#'   \item Adjusted R-squared: 0.918
#'   \item Deviance explained: 91.8\%
#'   \item Estimated scale: 0.238
#'   \item Effective degrees of freedom for smooth: 7.47 (Ref.df = 8)
#'   \item F-statistic for smooth term: 13,101 (p < 2e-16)
#' }
#'
#' @format A \code{\link[mgcv]{gam}} object.
#'
#' @source Generated internally from \link{experiment_df}.
#' @examples
#' data(gam_cor_to_vif)
#' plot(gam_cor_to_vif, shade = TRUE)
"gam_cor_to_vif"

#' Equivalence Table of Maximum Correlation and VIF Threshold
#'
#' A look-up table giving the predicted VIF threshold (\code{max_vif})
#' required for [vif_select()] to match the feature selection of
#' [cor_select()] at a given maximum allowed pairwise correlation
#' (\code{max_cor}).
#'
#' Values were generated by applying \code{mgcv::predict.gam} to the fitted model [gam_cor_to_vif] and rounding to two decimal places.
#'
#' @format A data frame with 91 rows and 2 numeric columns:
#' \describe{
#'   \item{max_cor}{Maximum allowed pairwise correlation, from 0.10 to 1.00 in steps of 0.01.}
#'   \item{max_vif}{Predicted VIF threshold corresponding to each \code{max_cor}.}
#' }
#'
#' @examples
#' data(equivalence_cor_vif)
#' head(equivalence_cor_vif)
#' plot(max_vif ~ max_cor, data = equivalence_cor_vif, type = "l")
"equivalence_cor_vif"
