#' Validate Argument \code{predictors}
#'
#' @description
#' Internal function. Validates the argument \code{predictors} as follows:
#' \itemize{
#'   \item Returns \code{predictors} as-is if it has the attribute \code{validated = TRUE}.
#'   \item Stops if \code{df} is \code{NULL}, using [validate_arg_df_not_null()].
#'   \item If \code{predictors} is \code{NULL}, uses all column names in \code{df} except those in the \code{response} argument.
#'   \item Removes any elements in \code{predictors} that are also in \code{response}.
#'   \item Ignores \code{predictors} not present as column names in \code{df}, issuing a message if \code{quiet = FALSE}.
#'   \item If \code{df} has at least 10 rows:
#'     \itemize{
#'       \item Removes numeric predictors with near zero variance, with message if \code{quiet = FALSE}.
#'       \item Removes categorical predictors with constant values, with message if \code{quiet = FALSE}.
#'     }
#'   \item Adds the attribute \code{validated = TRUE} to the returned vector.
#' }
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#'
#' @return character vector: predictor names
#' @examples
#' data(vi, vi_predictors)
#'
#' predictors <- validate_arg_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' attributes(predictors)$validated
#'
#' @autoglobal
#' @family data_validation
#' @export
validate_arg_predictors <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    function_name = NULL,
    quiet = FALSE
){

  if(is.null(function_name)){
    function_name <- "collinear::validate_arg_predictors()"
  }

  #if already validated, return it
  if(isTRUE(attr(x = predictors, which = "validated"))){
    return(predictors)
  }

  #if df is NULL, stop
  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  #if predictors is NULL, use colnames(df)
  if(is.null(predictors)){

    predictors <- setdiff(
      x = colnames(df),
      y = response
    )

  }

  #remove response from predictors
  predictors <- setdiff(
    x = predictors,
    y = response
  )

  #identify wrongly named predictors
  predictors.missing <- setdiff(
    x = predictors,
    y = colnames(df)
  )

  if(length(predictors.missing) > 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": these 'predictors' are not column names of 'df' and will be ignored:\n - ",
        paste(
          predictors.missing,
          collapse = "\n - "
        )
      )

    }

    #getting predictors in df only
    predictors <- intersect(
      x = predictors,
      y = colnames(df)
    )

  }

  if(nrow(df) >= 10){

    #removing zero variance predictors
    predictors.zero.variance <- identify_predictors_zero_variance(
      df = df,
      predictors = predictors
    )

    if(length(predictors.zero.variance) > 0){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": these predictors have near zero variance and will be ignored: \n - ",
          paste0(
            predictors.zero.variance,
            collapse = "\n - "
          )
        )

      }

      predictors <- setdiff(
        predictors,
        predictors.zero.variance
      )

    }

    #removing constant categoricals
    predictors.constant <- predictors[
      vapply(
        X = df[, predictors, drop = FALSE],
        FUN = function(x){
          length(unique(x)) == 1
        },
        FUN.VALUE = logical(1)
      )
    ]

    if(length(predictors.constant) > 0){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": these predictors have constant values and will be ignored: \n - ",
          paste0(
            predictors.constant,
            collapse = "\n - "
          )
        )

      }

      predictors <- setdiff(
        predictors,
        predictors.constant
      )

    }

  }

  if(length(predictors) == 0){

    warning(
      function_name,
      ": no predictors available, returning NULL.",
      call. = FALSE
    )

    return(NULL)

  }

  attr(
    x = predictors,
    which = "validated"
  ) <- TRUE

  predictors

}
