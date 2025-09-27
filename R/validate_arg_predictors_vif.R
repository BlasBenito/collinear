#' Validate Argument \code{predictors} for VIF Analysis
#'
#' @description
#' Internal function to assess whether the input arguments \code{df} and \code{predictors} result in data dimensions suitable for a VIF analysis. Expects argument \code{df} to be validated with [validate_arg_df()].
#'
#' The function performs the following actions:
#' \itemize{
#'   \item Returns \code{predictors} immediately if it has the attribute \code{validated_vif = TRUE}.
#'   \item If \code{predictors} does not have the attribute \code{validated = TRUE}, it is validated with [validate_arg_predictors()].
#'   \item Stops if \code{df} is \code{NULL}, using [validate_arg_df_not_null()].
#'   \item Identifies numeric predictors using [identify_predictors_numeric()]. If none are found, issues a warning and returns \code{NULL}.
#'   \item If the number of numeric predictors is 1, it returns the input with a message and skips VIF filtering.
#'   \item If the number of rows in \code{df} is less than 10 times the number of numeric predictors minus one, it issues a warning and returns a reduced set of predictors based on available rows. If no predictors can be retained, it returns \code{NULL}.
#'   \item Non-numeric predictors are dropped with a message.
#'   \item Adds the attributes \code{validated = TRUE} and \code{validated_vif = TRUE} to the returned predictor vector.
#' }
#'
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#'
#' @return character vector: predictors names
#' @export
#' @family data_validation
#' @autoglobal
#' @examples
#' data(vi, vi_predictors)
#'
#' predictors <- validate_arg_predictors_vif(
#'   df = vi,
#'   predictors = vi_predictors,
#'   function_name = "vif_df()"
#' )
#'
#'  attributes(predictors)$validated
#'  attributes(predictors)$validated_vif
validate_arg_predictors_vif <- function(
    df = NULL,
    predictors = NULL,
    function_name = NULL,
    quiet = FALSE
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_predictors_vif()",
    function_name = function_name
  )

  #if df is NULL, stop
  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(isTRUE(attr(x = predictors, which = "validated_vif"))){
    return(predictors)
  }

  if(!isTRUE(attr(x = predictors, which = "validated"))){

    predictors <- validate_arg_predictors(
      df = df,
      response = NULL,
      predictors = predictors,
      function_name = function_name,
      quiet = quiet
    )

    if(is.null(predictors)){
      return(NULL)
    }

    if(length(predictors) == 1){
      return(predictors)
    }

  }

  #predictors
  predictors_types <- identify_predictors(
    df = df,
    predictors = predictors,
    function_name = function_name
  )

  predictors_numeric <- predictors_types$numeric

  predictors_lost <- c(
    predictors_types$categorical,
    predictors_types$logical,
    predictors_types$zero_variance
  )

  if(length(predictors_lost) > 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": these predictors are categorical, logical, or have near-zero variance and will be ignored: \n - ",
        paste(predictors_lost, collapse = "\n - ")
      )

    }

  }

  if(length(predictors_numeric) > 1){

    #minimum number of required rows
    min.rows <- 10 * (length(predictors_numeric) - 1)

    #manageable number of predictors with rows in df
    min.predictors <- floor(
      nrow(
        stats::na.omit(df[, predictors, drop = FALSE])
        )
      /10
      )

    #if fewer rows than required
    if(nrow(df) < min.rows){

      #restrict predictors to a manageable number
      predictors_subset <- predictors_numeric[1:min.predictors]

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": not enough rows in `df` to process all predictors. VIF analysis will be applied to these ones: \n - ",
          paste(predictors_subset, collapse = "\n - ")
        )

      }

      predictors_numeric <- predictors_subset

    }

  } else if(length(predictors_numeric) == 0){

    return(NULL)

  } else if(length(predictors_numeric) == 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": only one numeric predictor in argument 'predictors', skipping VIF filtering."
      )

    }

  }

  attr(
    x = predictors_numeric,
    which = "validated"
  ) <- TRUE

  attr(
    x = predictors_numeric,
    which = "validated_vif"
  ) <- TRUE

  predictors_numeric

}
