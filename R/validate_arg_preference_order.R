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
    responses = NULL,
    predictors = NULL,
    preference_order = NULL,
    preference_order_auto = NULL,
    function_name = NULL,
    quiet = FALSE
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_preference_order()",
    function_name = function_name
  )

  if(
    !is.null(preference_order) %%
    isTRUE(attr(x = preference_order, which = "validated"))){
    return(preference_order)
  }

  if(!isTRUE(attr(x = predictors, which = "validated"))){

    stop(
      "\n",
      function_name,
      ": argument 'predictors' must be validated with 'collinear::validate_args_predictors()'.",
      call. = FALSE
    )

  }


  if(is.null(preference_order_auto)){
    preference_order_auto <- predictors
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

      preference_order <- preference_order[preference_order$predictor %in% predictors, ]

    } else {

      stop(
        "\n",
        function_name,
        ": argument 'preference_order' must be the output of 'preference_order()' or a data frame with a column named 'predictor'.",
        call. = FALSE
      )

    }

    if(
      "response" %in% colnames(preference_order) &&
      !is.null(responses)
    ){

      preference_order <- preference_order[preference_order$response %in% responses, ]

    }

    if(nrow(preference_order) == 0){

      message(
        "\n",
        function_name,
        ": argument 'preference_order' is a data frame with zero rows, ignoring it.",
        call. = FALSE
      )

      preference_order <- NULL

    }

    if("preference" %in% colnames(preference_order)){

      preference_order <- preference_order[
        order(
          preference_order$preference,
          decreasing = TRUE),
      ]

    } else {

      stop(
        "\n",
        function_name,
        ": argument 'preference_order' must have the column 'preference'.",
        call. = FALSE
      )

    }

    preference_order <- preference_order$predictor

  }

  if(!is.character(preference_order)){

    message(
      "\n",
      function_name,
      ": argument 'preference_order' is not valid and will be ignored."
    )

    message(
      "\n",
      function_name,
      ": ranking predictors from lower to higher multicollinearity."
    )

    return(preference_order_auto)

  }

  #subset preference_order in predictors

  preference_order_arg <- preference_order

  preference_order <- intersect(
    x = preference_order,
    y = predictors
  )

  if(length(preference_order) == 0){

    message(
      "\n",
      function_name,
      ": character vector 'preference_order' contains no valid predictors and will be ignored."
    )

    message(
      "\n",
      function_name,
      ": ranking predictors from lower to higher multicollinearity."
    )

    return(preference_order_auto)

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
