#' Internal Wrapper of \code{preference_order()} Within \code{collinear()}
#'
#' @description
#' Internal function to manage the argument \code{preference_order} within [collinear()]. Users should use [preference_order()] instead.
#'
#' @inheritParams collinear
#' @inheritParams f_auto
#' @inheritParams validate_arg_f
#' @examples
#' data(vi_smol, vi_predictors_numeric)
#'
#' x <- preference_order_wrapper(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = vi_predictors_numeric,
#'   preference_order = c(
#'     "soil_soc",
#'     "growing_season_temperature"
#'   ),
#'   quiet = TRUE
#' )
#'
#' x
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
    f_name = NULL,
    function_name = NULL,
    quiet = FALSE
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::preference_order_wrapper()",
    function_name = function_name
  )

  # NULL ----
  # cor_select and vif_select rank predictors by their multicollinearity
  if(
    is.null(preference_order) &&
    (is.null(f) || is.null(response))
  ){

    return(NULL)

  }

  # list ----
  if(
    is.list(preference_order) &&
    !is.data.frame(preference_order)
    ){

    if(
      !is.null(response) &&
      response %in% names(preference_order)
      ){

     return(preference_order[[response]])

    } else {

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": list 'preference_order' does not contain an object named after 'response' and will be ignored."
        )

      }

      return(NULL)

    }

  }


  #data frame
  if(is.data.frame(preference_order)){

    if(all(c("predictor", "response", "preference", "f") %in% colnames(preference_order))){

      if(response %in% unique(preference_order$response)){

        return(preference_order)

      } else {

        if(quiet == FALSE){

          message(
            "\n",
            function_name,
            ": column 'response' of the dataframe 'preference_order' does not contain a valid 'response' and will be ignored."
          )

        }

        return(NULL)

      }

    } else {

      message(
        "\n",
        function_name,
        ": dataframe 'preference_order' requires the columns 'predictor', 'response', 'preference', and 'f'."
      )

      return(NULL)

    }

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

        preference_df <- data.frame(
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
          x = preference_df,
          which = "validated"
        ) <- TRUE

        return(preference_df)

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

    return(NULL)

  }

  preference_df <- preference_order(
    df = df,
    responses = response,
    predictors = predictors,
    f = f,
    quiet = quiet,
    function_name = function_name
  )

  if(!is.null(f_name) && all(preference_df$f == "f")){
    preference_df$f <- f_name
  }

  preference_df

}
