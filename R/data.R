#' Example Data With Different Response and Predictor Types
#'
#' @description
#'
#' The response variable is a Vegetation Index encoded in different ways to help highlight the package capabilities:
#' \itemize{
#'  \item \code{vi_numeric}: continuous vegetation index values in the range 0-1.
#'  \item \code{vi_counts}: simulated integer counts created by multiplying \code{vi_numeric} by 1000 and coercing the result to integer.
#'  \item \code{vi_binomial}: simulated integer binomial variable created by transforming \code{vi_numeric} to zeros and ones.
#'  \item \code{vi_categorical}: character variable with the categories "very_low", "low", "medium", "high", and "very_high", with thresholds located at the quantiles of \code{vi_numeric}.
#'  \item \code{vi_factor}: \code{vi_categorical} converted to factor.
#' }
#'
#' The names of all predictors (continuous, integer, character, and factors) are in [vi_predictors].
#'
#' @usage data(vi)
#' @seealso [vi_predictors]
#'
#' @format dataframe with 30.000 rows and 68 columns.
#' @family example_data
"vi"

#' Small Version (100 rows) of the Dataset \code{vi}
#'
#' @description
#'
#' Same as dataset [vi], but with only 610 rows. Used in code examples to shorten runtime.
#'
#' @usage data(vi_smol)
#' @seealso [vi_predictors]
#'
#' @format dataframe with 100 rows and 68 columns.
#' @family example_data
"vi_smol"

#' Response Names in Example Dataframe `vi`
#'
#' @usage data(vi_responses)
#' @seealso [vi]
#'
#' @format Character vector with response names.
#' @family example_data
"vi_responses"

#' All Predictor Names in Example Dataframe vi
#'
#' @usage data(vi_predictors)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors"

#' All Numeric Predictor Names in Example Dataframe vi
#'
#' @usage data(vi_predictors_numeric)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors_numeric"

#' All Categorical and Factor Predictor Names in Example Dataframe vi
#'
#' @usage data(vi_predictors_categorical)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors_categorical"


#' One response and four predictors with varying levels of multicollinearity
#'
#' dataframe with known relationship between responses and predictors useful
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
#' @format dataframe with 2000 rows and 5 columns.
#' @family example_data
"toy"

#' Results of a Collinearity-Filtering Simulation
#'
#' A dataframe summarizing 10,000 experiments comparing the output of [cor_select()] and [vif_select()]. Each row records the input sampling parameters and the resulting feature-selection metrics.
#'
#' The source data is [vi] plus 200 synthetic columns, for a total of 249 columns and 30.000 rows.
#'
#' @format A dataframe with 9354 rows and 6 variables:
#' \describe{
#'   \item{input_rows}{Number of rows in the input data.}
#'   \item{input_predictors}{Number of predictors in the input data.}
#'   \item{output_predictors}{Number of predictors selected by [cor_select()].}
#'   \item{input_max_cor}{Maximum allowed pairwise correlation supplied to [cor_select()].}
#'   \item{output_max_vif}{Smallest VIF threshold at which [vif_select()] produced the most similar to [cor_select()] for the given \code{input_max_cor}.}
#'   \item{out_selection_length}{Number of variables returned from the multicollinearity filtering.}
#'   \item{jaccard_cor_vs_vif_selection}{Jaccard similarity between the predictors selected [cor_select()] and [vif_select()].}
#' }
#'
#' @examples
#' data(experiment_cor_vs_vif)
#' str(experiment_cor_vs_vif)
"experiment_cor_vs_vif"


#' Testing Capabilities of [collinear()] to Auto Configure \code{max_cor} and \code{max_vif}
#'
#' A dataframe with 10,000 experiment to test the capability of [collinear()] to automatically reduce multicollinearity when \code{max_cor} and \code{max_vif} are NULL.
#'
#' The source data is [vi] plus 200 synthetic columns, for a total of 249 columns and 30.000 rows that are randomly subset on each iteration.
#'
#' @format A dataframe with 10000 rows and 7 variables:
#' \describe{
#'   \item{input_rows}{Number of rows in the input data.}
#'   \item{input_predictors}{Number of predictors in the input data.}
#'   \item{output_predictors}{Number of predictors selected by [collinear()].}
#'   \item{input_cor_median}{Median correlation of the input predictors as returned by [collinear_stats()].}
#'   \item{output_cor_median}{Median correlation of the selected predictors.}
#'   \item{input_vif_max}{Maximum VIF of the input predictors as returned by [collinear_stats()].}
#'   \item{output_vif_max}{Maximum VIF of the selected predictors.}
#' }
#'
#' @examples
#' data(experiment_collinear_auto)
#' str(experiment_collinear_auto)
"experiment_collinear_auto"

#' GAM Relating Maximum Correlation to VIF Threshold
#'
#' A fitted generalized additive model describing \code{max_vif} as a function of \code{max_cor} in [experiment_cor_vs_vif].
#'
#' The model was fitted with a smooth term in \code{max_cor} (basis dimension \code{k = 9}) and cubic weights proportional to the Jaccard similarity between the results of [cor_select()] and [vif_select()] to emphasize cases with high agreement between the two selection methods.
#'
#' \preformatted{
#' gam_cor_to_vif <- mgcv::gam(
#'  formula = output_max_vif ~ s(input_max_cor, k = 9),
#'  weights = experiment_cor_vs_vif$out_selection_jaccard^3,
#'  data = experiment_cor_vs_vif,
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
#' @source Generated internally from \link{experiment_cor_vs_vif}.
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
#' @format A dataframe with 91 rows and 2 numeric columns:
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
