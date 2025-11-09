#' Dataframe of Functions to Compute Preference Order
#'
#' @return dataframe
#' @export
#' @autoglobal
#' @family preference_order_tools
#' @examples
#' f_functions()
f_functions <- function(){

  f_list <- list(
    c("f_numeric_glm", "numeric", "mixed", "stats::glm(y ~ x, family = gaussian(link = 'identity'))", "R-squared"),
    c("f_numeric_gam", "numeric", "numeric", "mgcv::gam(y ~ s(x), family = gaussian(link = 'identity'))", "R-squared"),
    c("f_numeric_rf", "numeric", "mixed", "ranger::ranger(y ~ x)", "R-squared"),
    c("f_count_rf", "integer", "mixed", "ranger::ranger(y ~ x)", "R-squared"),
    c("f_count_glm", "integer", "mixed", "stats::glm(y ~ x, family = poisson(link = 'log'))", "R-squared"),
    c("f_count_gam", "integer", "mixed", "mgcv::gam(y ~ s(x), family = poisson(link = 'log'))", "R-squared"),
    c("f_binomial_glm", "binomial", "mixed", "stats::glm(y ~ x, family = quasibinomial(link = 'logit'), weights = case_weights(y))", "AUC"),
    c("f_binomial_gam", "binomial", "numeric", "mgcv::gam(y ~ s(x), family = quasibinomial(link = 'logit'), weights = collinear::case_weights(y))", "AUC"),
    c("f_binomial_rf", "binomial", "mixed", "ranger::ranger(y ~ x, case.weights = collinear::case_weights(y))", "AUC"),
    c("f_categorical_rf", "categorical", "mixed", "ranger::ranger(y ~ x, case.weights = collinear::case_weights(y))", "Cramer's V")
  )

  f_df <- f_list |>
    as.data.frame() |>
    t() |>
    as.data.frame()

  rownames(f_df) <- NULL

  colnames(f_df) <- c(
    "name",
    "response_type",
    "predictors_types",
    "expression",
    "metric"
  )

  f_df

}
