#' Builds Object of Class \code{collinear_selection}
#'
#' @inheritParams collinear
#'
#' @return Object of class \code{collinear_selection}
#' @export
#' @autoglobal
build.collinear_selection <- function(
    df = NULL,
    response = NULL,
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
  out <- list()

  if(!is.null(response)){
    out$response <- paste(response)
  }

  if(!is.null(selection)){
    out$selection <- paste(selection)
  }

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

  if(!is.null(df)){
    out$df <- df
  }

  if(!is.null(preference_order_list)){
    out$preference <- preference_order_list
  }

  ## class ----
  class(out) <- c(
    class(out),
    "collinear_selection"
  )

  out


}
