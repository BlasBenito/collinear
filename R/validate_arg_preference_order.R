#' Validate Argument \code{preference_order}
#'
#' @description
#' Internal function to validate the argument \code{preference_order}.
#'
#' It performs the following actions:
#' \itemize{
#'   \item Returns \code{preference_order} as-is if already tagged with \code{validated = TRUE}.
#'   \item Stops if \code{preference_order_auto} is \code{NULL}.
#'   \item Stops if \code{predictors} is not validated.
#'   \item Uses \code{preference_order_auto} if \code{preference_order} is \code{NULL}.
#'   \item Extracts variable names from a data frame with column \code{predictor}, if applicable.
#'   \item Filters \code{preference_order} to keep only variables in \code{predictors}.
#'   \item Appends missing predictors in the order defined by \code{preference_order_auto}.
#'   \item Tags the result with the attribute \code{validated = TRUE}.
#' }
#'
#'
#' @inheritParams collinear
#' @param preference_order_auto (required, character vector) names of the predictors in the automated preference order returned by [vif_select()] or [cor_select()]
#' @inheritParams validate_arg_quiet
#'
#' @return character vector: ranked variable names
#' @export
#' @family data_validation
#' @autoglobal
#' @examples
#'   data(vi, vi_predictors)
#'
#'   predictors <- validate_arg_predictors(
#'     df = vi,
#'     predictors = vi_predictors
#'   )
#'
#'   my_preference_order <- c(
#'     "swi_max",
#'     "swi_min",
#'     "swi_deviance" #does not exist
#'   )
#'
#'   my_order <- validate_arg_preference_order(
#'     predictors = predictors,
#'     preference_order = my_preference_order,
#'     preference_order_auto = vi_predictors
#'   )
#'
#'   #has my_order first
#'   #excludes non-existing columns
#'   #all other variables ordered according to preference_order_auto
#'   my_order
validate_arg_preference_order <- function(
    predictors = NULL,
    preference_order = NULL,
    preference_order_auto = NULL,
    function_name = NULL,
    quiet = FALSE
){

  if(isTRUE(attr(x = preference_order, which = "validated"))){
    return(preference_order)
  }

  if(is.null(preference_order_auto)){
    stop(
      "\n",
      function_name,
      ": argument 'preference_order_auto' cannot be NULL.",
      call. = FALSE
    )
  }


  if(!isTRUE(attr(x = predictors, which = "validated"))){

    stop(
      "\n",
      function_name,
      ": argument 'predictors' must be validated with 'collinear::validate_args_predictors()'.",
      call. = FALSE
    )

  }

  if(is.null(preference_order)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": ranking predictors from lower to higher multicollinearity."
      )

    }

    return(preference_order_auto)

  }

  #check if preference_order comes from preference_order()
  if(is.data.frame(preference_order)){

    if("predictor" %in% colnames(preference_order)){

    } else {

      stop(
        "\n",
        function_name,
        ": argument 'preference_order' must be the output of 'preference_order()' or a data frame with a column named 'predictor'.",
        call. = FALSE
      )
    }

  }

  #subset preference_order in predictors

  preference_order_arg <- preference_order

  preference_order <- intersect(
    x = preference_order,
    y = predictors
  )

  predictors.missing <- setdiff(
    x = preference_order_arg,
    y = preference_order
  )

  if(length(predictors.missing) > 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": these columns in 'preference_order' are not in 'predictors' and will be ignored:\n - ",
        paste(
          predictors.missing,
          collapse = "\n - "
        )
      )

    }

  }

  #if there are variables not in preference_order
  #add them in the order of preference_order.auto
  if(length(preference_order) < length(predictors)){

    not_in_preference_order <- setdiff(
      x = predictors,
      y = preference_order
    )

    preference_order <- c(
      preference_order,
      intersect(
        x = preference_order_auto,
        y = not_in_preference_order
      )
    )

  }

  attr(
    x = preference_order,
    which = "validated"
  ) <- TRUE

  preference_order

}
