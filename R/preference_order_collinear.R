
#' Preference Order Argument in collinear()
#'
#' @description
#' Internal function to manage the argument `preference_order` in [collinear()].
#'
#' @inheritParams collinear
#'
#' @return character vector or NULL
#' @export
#' @autoglobal
#' @family preference_order_tools
preference_order_collinear <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    f = NULL,
    quiet = FALSE
){

  if(is.null(preference_order)){
    return(NULL)
  }

  # class character ----
  if(is.character(preference_order)){

    ## auto preference order ----
    if(preference_order[1] == "auto"){

      return(
        preference_order(
          df = df,
          response = response,
          predictors = predictors,
          f = f,
          quiet = quiet,
          warn_limit = NULL
        )$predictor
      )

    }

    # custom with missing predictors ----
    if(!all(predictors %in% preference_order)){

      missing_predictors <- setdiff(
        x = predictors,
        y = preference_order
      )

      preference_missing_predictors <-           preference_order(
        df = df,
        response = response,
        predictors = missing_predictors,
        f = f,
        quiet = quiet,
        warn_limit = NULL
      )$predictor

      return(
        c(
          preference_order,
          preference_missing_predictors
        )
      )

    }

    # custom preference order ----

    if(quiet == FALSE){

      message(
        "\ncollinear::collinear(): using custom preference order vector."
      )

    }

    return(preference_order)

  } #end of class character

  # data frame ----
  if(
    is.data.frame(preference_order) &&
    "predictor" %in% colnames(preference_order)
  ){

    if(quiet == FALSE){

      message(
        "\ncollinear::collinear(): using preference order data frame."
      )

    }

    return(preference_order$predictor)

  }

  # list ----
  if(is.list(preference_order)){

    ## list with response ----
    if(response %in% names(preference_order)){

      if(quiet == FALSE){

        message(
          "\ncollinear::collinear(): selecting data frame '", response, "' fron preference order list."
        )

      }

      return(preference_order[[response]]$predictor)

    } else {

      if(quiet == FALSE){

        message(
          "\ncollinear::collinear(): input preference order list does not have data for the response '", response, "'."
        )

      }

      return(
        preference_order(
          df = df,
          response = response,
          predictors = predictors,
          f = f,
          quiet = quiet,
          warn_limit = NULL
        )$predictor
      )

    }

  }

  NULL

}
