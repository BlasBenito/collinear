#' Autoconfigure Argument \code{max_cor}
#'
#' If \code{max_cor = NULL}, computes the average pairwise correlation between all predictors as \code{x} and returns \code{max(x, 0.58)}. The value \code{0.58} is selected because it roughly matches a VIF of 2.5 according to the model [gam_cor_to_vif], but it can be overriden by the user by selecting a lower value as input for \code{max_cor}. Otherwise, it checks \code{max_cor} with [validate_arg_max_cor()] and returns the result.
#'
#' @inheritParams collinear
#' @param max_cor (optional, numeric or NULL) Maximum correlation allowed between pairs of \code{predictors}. Default: NULL
#' @inheritParams validate_arg_quiet
#'
#' @returns numeric
#' @export
#' @autoglobal
#' @examples
#' max_cor <- autoconfig_arg_max_cor(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric,
#'   max_cor = NULL
#' )
#'
#' max_cor
autoconfig_arg_max_cor <- function(
    df = NULL,
    predictors = NULL,
    max_cor = NULL,
    quiet = FALSE,
    ...
){

  min_cor <- 0.58

  function_name <- validate_arg_function_name(
    default_name = "collinear::autoconfig_arg_max_cor()",
    ... = ...
  )

  if(
    is.numeric(max_cor) &&
    (max_cor > 0.99 || max_cor < 0.1)
  ){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": invalid 'max_cor' value, setting it to NULL to proceed with the autoconfiguration."
        )

      }

      max_cor <- NULL

  }

  #configure max_cor
  if(is.null(max_cor)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": autoconfiguring 'max_cor' ..."
      )

    }

    correlation_stats <- cor_stats(
      df = df,
      predictors = predictors,
      quiet = TRUE
    )

    correlation_mean <- correlation_stats$stats[
      correlation_stats$stats$statistic == "mean",
      "value"
    ]

    max_cor <- max(
      min_cor,
      round(x = correlation_mean, digits = 2)
    )

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": 'max_cor' = ",
        max_cor
      )

      if(max_cor > 0.90){

        message(
          "\n",
          function_name,
          ": the average pairwise correlation in this dataset is very high, automated multicollinearity filtering might yield suboptimal results in this case."
        )

      }

    }

  }

  max_cor <- validate_arg_max_cor(
    max_cor = max_cor,
    function_name = function_name,
    quiet = quiet
  )

  max_cor

}
