#' Autoconfigure Argument \code{max_cor}
#'
#' If \code{max_cor = NULL}, computes the average pairwise correlation between all predictors as \code{x} and returns \code{max(x, 0.5)}. Otherwise, it checks \code{max_cor} with [validate_arg_max_cor()] and returns the result.
#'
#' @inheritParams collinear
#' @param max_cor (optional, numeric or NULL) Maximum correlation allowed between pairs of \code{predictors}.
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
    function_name = NULL,
    quiet = FALSE
){

  default_function_name <- "collinear::autoconfig_arg_max_cor()"

  if(is.null(function_name)){
    function_name <- default_function_name
  } else {
    function_name <- paste0(
      function_name,
      " > \n    └── ",
      default_function_name
    )
  }

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
      0.5,
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
