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
#' @inheritParams validate_arg_quiet
#' @inheritParams target_encoding_lab
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
#'   df = vi_smol,
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
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    function_name = NULL,
    quiet = FALSE
){

  if(
    isTRUE(attr(x = preference_order, which = "validated"))){
    return(preference_order)
  }

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_preference_order()",
    function_name = function_name
  )

  df <- validate_arg_df(
    df = df,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  predictors <- validate_arg_predictors(
    df = df,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  #dataframe
  if(
    is.data.frame(preference_order) &&
    all(
      c(
        "response",
        "predictor",
        "preference",
        "f",
        "metric"
      ) %in% colnames(preference_order)
    )
  ){

    #subset predictors only
    preference_order <- preference_order[preference_order$predictor %in% predictors, ]

    #subset response
    if(
      !is.null(response) &&
      is.character(response) &&
      length(response) == 1 &&
      response %in% colnames(df)
    ){

      preference_order <- preference_order[preference_order$response %in% response, ]

    }


  }


  #character
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

    #keep ones in predictors
    preference_order <- intersect(
      x = preference_order,
      y = predictors
    )

    #generate dataframe
    if(is.null(response)){
      response <- NA
    }

    preference_order <- data.frame(
      response = response,
      predictor = preference_order,
      preference = seq(
        from = 1,
        to = 0,
        length.out = length(preference_order)
      ),
      f = NA,
      metric = "custom_rank"
    )

  }

  if(is.null(preference_order)){

    #generate dataframe
    if(is.null(response)){
      response <- NA
    }

    preference_order <- data.frame(
      response = NA,
      predictor = NA,
      preference = NA,
      f = NA,
      metric = NA
    ) |>
      stats::na.omit()


  }


  if(nrow(preference_order) < length(predictors)){

    predictors_missing <- setdiff(
      x = predictors,
      y = preference_order$predictor
    )

    m <- cor_matrix(
      df = df,
      predictors = predictors,
      function_name = function_name
    ) |>
      abs()

    x <- m |>
      colSums() |>
      sort() |>
      names()

    preference_order_default <- data.frame(
      response = NA,
      predictor = x,
      preference = NA,
      f = NA,
      metric = "multicollinearity_rank"
    )

    preference_order <- rbind(
      preference_order,
      preference_order_default
    )

    preference_order$preference <- seq(
      from = 1,
      to = 0,
      length.out = nrow(preference_order)
    )

  }

  rownames(preference_order) <- NULL

  attr(
    x = preference_order,
    which = "validated"
  ) <- TRUE

  preference_order

}
