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
  # cor_select and vif_select rank predictors by their multicollinearity
  if(
    is.null(preference_order) &&
    (is.null(f) || is.null(response))
  ){

    return(NULL)

  }

  # character vector ----
  if(is.character(preference_order)){

    #remove "auto"
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
      n <- length(preference_order)

      if(is.null(response)){
        response <- NA
      }

        preference_order <- data.frame(
          response = rep(
            x = response,
            times = n
          ),
          predictor = preference_order,
          f = rep(
            x = NA,
            times = n
          ),
          preference = seq(
            from = 1,
            to = 0,
            length.out = n
          )
        )

        attr(
          x = preference_order,
          which = "validated"
        ) <- TRUE

        return(preference_order)

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


  # list ----
  if(is.list(preference_order) &&
     isTRUE(attr(x = preference_order, which = "validated"))
     ){

    preference_order <- preference_order[[response]]

    attr(
      x = preference_order,
      which = "validated"
    ) <- TRUE

    return(preference_order)

  } else {

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": list 'preference_order' does not contain an obect named after 'response' and will be ignored."
      )

    }

    return(NULL)

  }


  # data frame ----
  if(
    is.data.frame(preference_order) &&
    isTRUE(attr(x = preference_order, which = "validated"))
    ){

   return(preference_order)

  } else {

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": dataframe 'preference_order' is not valid and will be ignored."
      )

    }

    return(NULL)

  }


  if(is.null(f)){

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

  preference_order

}
