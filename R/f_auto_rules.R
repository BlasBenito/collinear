#' Rules Used by \code{f_auto()} to Select an \code{f} Function
#'
#' @description
#' Dataframe with rules used by [f_auto()] to select the function \code{f} in [f_functions()] to compute preference order in [preference_order()]. In most cases, random forest is selected as base model to provide homogeneous results across case types.
#'
#'
#' @return dataframe
#' @family preference_order_tools
#' @export
#' @autoglobal
#' @examples
#' f_auto_rules()
f_auto_rules <- function(){

  f_list <- list(
    c("f_numeric_glm", "continuous-binary", "numeric"),
    c("f_numeric_rf", "continuous-binary", "categorical"),
    c("f_numeric_rf", "continuous-binary", "mixed"),
    c("f_numeric_glm", "continuous-low", "numeric"),
    c("f_numeric_rf", "continuous-low", "categorical"),
    c("f_numeric_rf", "continuous-low", "mixed"),
    c("f_numeric_glm", "continuous-high", "numeric"),
    c("f_numeric_rf", "continuous-high", "categorical"),
    c("f_numeric_rf", "continuous-high", "mixed"),
    c("f_binomial_glm", "integer-binomial", "numeric"),
    c("f_binomial_rf", "integer-binomial", "categorical"),
    c("f_binomial_rf", "integer-binomial", "mixed"),
    c("f_count_glm", "integer-binary", "numeric"),
    c("f_count_rf", "integer-binary", "categorical"),
    c("f_count_rf", "integer-binary", "mixed"),
    c("f_count_glm", "integer-low", "numeric"),
    c("f_count_rf", "integer-low", "categorical"),
    c("f_count_rf", "integer-low", "mixed"),
    c("f_count_glm", "integer-high", "numeric"),
    c("f_count_rf", "integer-high", "categorical"),
    c("f_count_rf", "integer-high", "mixed"),
    c("f_categorical_rf", "categorical", "numeric"),
    c("f_categorical_rf", "categorical", "mixed"),
    c("f_categorical_rf", "categorical", "categorical")
  )

  f_df <- f_list |>
    as.data.frame() |>
    t() |>
    as.data.frame()

  rownames(f_df) <- NULL

  colnames(f_df) <- c(
    "name",
    "response_type",
    "predictors_type"
  )

  f_df

}
