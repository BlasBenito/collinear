#' Large example dataframe
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

#' Small example dataframe
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

#' Vector of response names in `vi` and `vi_smol`
#'
#' @usage data(vi_responses)
#' @seealso [vi]
#'
#' @format Character vector with response names.
#' @family example_data
"vi_responses"

#' Vector of all predictor names in `vi` and `vi_smol`
#'
#' @usage data(vi_predictors)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors"

#' Vector of numeric predictor names in `vi` and `vi_smol`
#'
#' @usage data(vi_predictors_numeric)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors_numeric"

#' Vector of categorical predictors in `vi` and `vi_smol`
#'
#' @usage data(vi_predictors_categorical)
#' @seealso [vi]
#'
#' @format Character vector with predictor names.
#' @family example_data
"vi_predictors_categorical"


#' Toy dataframe with varying levels of multicollinearity
#'
#' Dataframe with known relationship between responses and predictors useful to illustrate multicollinearity concepts.
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

#' Dataframe resulting from experiment to test the automatic selection of multicollinearity thresholds
#'
#' A dataframe summarizing 10,000 experiments validating the adaptive multicollinearity threshold system in [collinear()]. Each row records input data characteristics and the resulting multicollinearity metrics after filtering.
#'
#' The source data is a synthetic dataframe with 500 columns and 10,000 rows generated using \code{distantia::zoo_simulate()} with correlated time series (\code{independent = FALSE}, \code{seasons = 0}).
#'
#' Each iteration randomly subsets 10-100 predictors and 30-100 rows per predictor, then applies [collinear()] with automatic threshold configuration to assess:
#' \itemize{
#'   \item Whether output VIF stays bounded between ~2.5 and ~7.5
#'   \item How the system adapts to different correlation structures
#'   \item How predictor retention scales with input size
#' }
#'
#' @format A dataframe with 10,000 rows and 9 variables:
#' \describe{
#'   \item{input_rows}{Number of rows in the input data subset.}
#'   \item{input_predictors}{Number of predictors in the input data subset.}
#'   \item{output_predictors}{Number of predictors retained after filtering.}
#'   \item{input_cor_q75}{75th percentile of pairwise correlations in the input data.}
#'   \item{output_cor_q75}{75th percentile of pairwise correlations in the selected predictors.}
#'   \item{input_cor_max}{Maximum pairwise correlation in the input data.}
#'   \item{output_cor_max}{Maximum pairwise correlation in the selected predictors.}
#'   \item{input_vif_max}{Maximum VIF in the input data.}
#'   \item{output_vif_max}{Maximum VIF in the selected predictors.}
#' }
#' @family experiments
#' @usage data(experiment_adaptive_thresholds)
#' @examples
#' data(experiment_adaptive_thresholds)
#' str(experiment_adaptive_thresholds)
"experiment_adaptive_thresholds"

#' Dataframe with results of experiment comparing correlation and VIF thresholds
#'
#' A dataframe summarizing 10,000 experiments comparing the output of [cor_select()] and [vif_select()]. Each row records the input sampling parameters and the resulting feature-selection metrics.
#'
#' The source data is a synthetic dataframe with 500 columns and 10,000 rows generated using \code{distantia::zoo_simulate()} with correlated time series (\code{independent = FALSE}).
#'
#' Each iteration randomly subsets 10-50 predictors and 30-100 rows per predictor, applies [cor_select()] with a random \code{max_cor} threshold, then finds the \code{max_vif} value that maximizes Jaccard similarity between the two selections.
#'
#' @format A dataframe with 10,000 rows and 6 variables:
#' \describe{
#'   \item{input_rows}{Number of rows in the input data subset.}
#'   \item{input_predictors}{Number of predictors in the input data subset.}
#'   \item{output_predictors}{Number of predictors selected by [vif_select()] at the best-matching \code{max_vif}.}
#'   \item{max_cor}{Maximum allowed pairwise correlation supplied to [cor_select()].}
#'   \item{max_vif}{VIF threshold at which [vif_select()] produced the highest Jaccard similarity with [cor_select()] for the given \code{max_cor}.}
#'   \item{out_selection_jaccard}{Jaccard similarity between the predictors selected by [cor_select()] and [vif_select()].}
#' }
#' @family experiments
#' @usage data(experiment_cor_vs_vif)
#' @examples
#' data(experiment_cor_vs_vif)
#' str(experiment_cor_vs_vif)
"experiment_cor_vs_vif"


#' GAM describing the relationship between correlation and VIF thresholds
#'
#' A fitted generalized additive model describing \code{max_vif} as a function of \code{max_cor} in [experiment_cor_vs_vif].
#'
#' The model parameters (basis dimension \code{k} and weight exponent) were selected via optimization, filtering for models in the top 90\% of R-squared and bottom 10\% of effective degrees of freedom to balance fit quality and parsimony.
#'
#' The final model uses squared Jaccard similarity as weights to emphasize cases with high agreement between [cor_select()] and [vif_select()].
#'
#'
#' Model performance:
#' \itemize{
#'   \item Adjusted R-squared: 0.834
#'   \item Deviance explained: 83.4\%
#'   \item Effective degrees of freedom for smooth: ~6
#' }
#'
#' @format A \code{\link[mgcv]{gam}} object.
#' @family experiments
#' @usage data(gam_cor_to_vif)
#' @source Generated internally from \link{experiment_cor_vs_vif}.
#' @examples
#' data(gam_cor_to_vif)
#' plot(gam_cor_to_vif, shade = TRUE)
"gam_cor_to_vif"


#' Prediction of the model `gam_cor_to_vif` across correlation values
#'
#' Dataframe with predicted VIF threshold corresponding to a given correlation threshold..
#'
#' Values were generated by applying \code{mgcv::predict.gam()} to the fitted model [gam_cor_to_vif] and rounding to three decimal places.
#'
#' @format A dataframe with 901 rows and 2 numeric columns:
#' \describe{
#'   \item{max_cor}{Maximum allowed pairwise correlation, from 0.10 to 1.00 in steps of 0.001.}
#'   \item{max_vif}{Predicted VIF threshold corresponding to each \code{max_cor}.}
#' }
#'
#' @family experiments
#' @usage data(prediction_cor_to_vif)
#' @examples
#' data(prediction_cor_to_vif)
#' head(prediction_cor_to_vif)
#' plot(max_vif ~ max_cor, data = prediction_cor_to_vif, type = "l")
"prediction_cor_to_vif"
