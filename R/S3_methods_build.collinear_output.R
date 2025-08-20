#' Builds Object of Class \code{collinear_output}
#'
#' @inheritParams collinear
#'
#' @return Object of class \code{collinear_output}:
#'
#' \itemize{
#'   \item If \code{response} is \code{NULL} or has length 1: an object of class \code{collinear_output} with a custom print method [print.collinear_output]. It contains:
#'     \itemize{
#'       \item \code{response} (\code{character} or \code{NULL}): Name of the response variable.
#'       \item \code{predictors} (\code{character} or \code{NULL}): Names of predictors considered in multicollinearity filtering.
#'       \item \code{selection} (\code{character} or \code{NULL}): Names of selected, non-collinear predictors.
#'       \item \code{df} (\code{data.frame} or \code{NULL}): Data frame including columns in \code{response} and \code{selection}. If target encoding was applied, categorical variables will be numeric.
#'       \item \code{arguments} (\code{list}): A list of arguments used in the call: \code{encoding_method}, \code{preference_order}, \code{f}, \code{max_cor}, and \code{max_vif}
#'     }
#'
#'   \item If \code{response} is a character vector of length >= 2: an object of class \code{collinear_list}, a named list of \code{"collinear_output"} objects, one per response variable.
#' }
#' @export
#' @autoglobal
build.collinear_output <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    selection = NULL,
    quiet = FALSE
){

  # preference order ----
  preference_order_list <- NULL

  if(is.data.frame(preference_order)){

    f_name <- unique(preference_order$f)
    f_expression <- NULL
    f_metric <- NULL

    f_df <- f_functions()
    f_df <- f_df[f_df$name == f_name, ]

    if(nrow(f_df) > 0){

      f_expression <- f_df$expression
      f_metric <- f_df$preference_metric

    }


    preference_order_list <- list(
      df = preference_order,
      f = list(
        name = f_name,
        expression = f_expression,
        metric = f_metric
      )
    )

  }

  # main ----
  out <- list(
    response = paste(response),
    selection = paste(selection)
  )

  # formulas ----
  ### formulas ----
  if(
    all(
      c(
        !is.null(response),
        !is.null(selection),
        !is.null(df)
      )
    )
  ){

    response_type <- identify_predictors(
      df = df,
      predictors = response
    ) |>
      unlist() |>
      names()

    selection_type <- identify_predictors(
      df = df,
      predictors = selection
    )

    out$formulas <- list()

    #general formula
    general_formula <- model_formula(
      df = df,
      response = response,
      predictors = selection,
      quiet = quiet
    )

    #general formula name
    general_formula_name <- c(
      numeric = "linear",
      logical = "binomial",
      categorical = "classification"
    )[[response_type]]

    out$formulas[[general_formula_name]] <- general_formula

    #add smooth formula if relevant
    if(
      response_type == "numeric" &&
      length(selection_type$numeric) > 0
    ){

      out$formulas[["smooth"]] <- model_formula(
        df = df,
        response = response,
        predictors = selection,
        term_f = "s",
        quiet = quiet
      )

    }

  }

  #add remaining items
  out$predictors <- paste(predictors)
  out$df <- df
  out$preference <- preference_order_list
  out$timestamp <- Sys.time()

  ## class ----
  class(out) <- c(
    class(out),
    "collinear_output"
  )

  out


}
