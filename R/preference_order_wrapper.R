#' Internal Wrapper of \code{preference_order()} Within \code{collinear()}
#'
#' @description
#' Internal function to manage the argument \code{preference_order} within [collinear()]. Users should use [preference_order()] instead.
#'
#' @inheritParams collinear
#'
#' @return character vector or NULL
#' @export
#' @autoglobal
#' @family preference_order_tools
preference_order_wrapper <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    f = NULL,
    quiet = FALSE
){

  function_name <- "collinear::collinear()"

  # NULL ----
  if(
    is.null(preference_order) &&
    (is.null(f) || is.null(response))
  ){

    return(NULL)

  }

  # list ----
  if(is.list(preference_order)){

    ## NULL response ----
    if(is.null(response)){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'preference_order' of class 'list' requires a valid 'response' argument. Ignoring it."
        )

      }

      return(NULL)

    }

    ## valid response ----
    if(response %in% names(preference_order)){

      return(preference_order[[response]])

    }

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'preference_order' of class 'list' does not have an element named '", response, "' and will be ignored."
      )

    }

    return(NULL)

  }


  # data frame ----
  if(is.data.frame(preference_order)){

    if(all(c("predictor", "f") %in% colnames(preference_order))){

      return(preference_order)

    } else {

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'preference_order' of class 'data.frame' does not have any of the columns 'predictor' and 'f', ignoring it."
        )

      }

      preference_order <- NULL

    }

  }


  # character vector ----
  if(is.character(preference_order)){

    if("auto" %in% preference_order){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": value 'auto' for the argument 'preference_order' is deprecated, ignoring it."
        )

      }

      preference_order <- setdiff(
        x = preference_order,
        y = "auto"
      )

    }

    ##valid vector ----
    preference_order <- intersect(
      x = preference_order,
      y = predictors
    )

    if(length(preference_order) > 0){

      #return data frame
      return(
        data.frame(
          response = response,
          predictor = preference_order,
          f = NA,
          preference = length(preference_order):1
        )
      )

    } else {

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": vector 'preference_order' does not contain valid column names in 'df' and will be ignored."
        )

      }

      return(NULL)

    }

  }

  if(is.null(f)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'f' is NULL, skipping computation of preference order."
      )

    }

    return(NULL)

  }

  if(quiet == FALSE){

    message(
      "\n",
      function_name,
      ": computing preference order with function '",
      attributes(f)$name, "'."
    )

  }

  preference_order <- preference_order(
    df = df,
    response = response,
    predictors = predictors,
    f = f,
    quiet = quiet
  )

  f_name <- unique(preference_order$f)

  preference_order <- preference_order$predictor

  attr(
    x = preference_order,
    which = "f_name"
  ) <- f_name

  preference_order

}
