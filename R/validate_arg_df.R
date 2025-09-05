#' Validate Argument \code{df}
#'
#' @description
#' Internal function to validate the integrity of the argument \code{df}.
#'
#' It performs the following actions:
#' \itemize{
#'   \item Returns \code{df} as-is if already tagged with \code{validated = TRUE}.
#'   \item Stops if \code{df} is \code{NULL}, not coercible to a data frame, or has zero rows.
#'   \item Drops geometry column if present.
#'   \item Subsets \code{df} to the \code{responses} and \code{predictors} columns.
#'   \item Converts logical columns to numeric.
#'   \item Replaces \code{NaN}, \code{Inf}, and \code{-Inf} values with \code{NA} in numeric columns.
#'   \item Reorders columns to match original \code{responses} + \code{predictors} order.
#'   \item Tags the result with the attribute \code{validated = TRUE}.
#' }
#'
#' @inheritParams collinear
#' @inheritParams f_auto
#' @inheritParams validate_arg_quiet
#' @return data frame
#' @examples
#'
#' data(vi, vi_predictors)
#'
#' vi <- validate_arg_df(
#'   df = vi,
#'   responses = "vi_numeric",
#'   predictors = vi_predictors,
#'   function_name = "f()",
#'   quiet = FALSE
#' )
#'
#' attributes(vi)$validated
#' @autoglobal
#' @family data_validation
#' @export
validate_arg_df <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    function_name = NULL,
    quiet = FALSE
){

  if(is.null(function_name)){
    function_name <- "collinear::validate_arg_df()"
  }

  preference_order_wrapper

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(isTRUE(attr(x = df, which = "validated"))){
    return(df)
  }

  #handle coercion to df
  if(is.data.frame(df) == FALSE){

    df <- tryCatch(
      {
        as.data.frame(df)
      },
      error = function(e){
        stop(
          "\n",
          function_name,
          ": argument 'df' must be of class 'data .frame' or coercible to 'data.frame'.",
          call. = FALSE
        )
      }
    )

  }

  #warning if not enough rows
  if(ncol(df) > 1){

    if(nrow(df) < 3){

      stop(
        function_name,
        ": argument 'df' has fewer than 3 rows, multicollinearity analysis is not feasible.",
        call. = FALSE
      )

    } else if(nrow(df) < 10){

      warning(
        "\n",
        function_name,
        ": argument 'df' has fewer than 10 rows. Results may be unstable due to low sample size.",
        call. = FALSE
      )

    } else if(nrow(df) < 30){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'df' has fewer than 30 rows. Results may be unstable due to low sample size."
        )

      }

    }

  }


  if(ncol(df) == 0){

    stop(
      "\n",
      function_name,
      ": argument 'df' has 0 columns.",
      call. = FALSE
    )

  }

  df <- drop_geometry_column(
    df = df,
    quiet = quiet
  )

  if(is.null(predictors)){
    predictors <- colnames(df)
  }

  #subset valid columns
  selected_columns <- unique(
    c(
      responses,
      predictors
    )
  )

  if(is.null(selected_columns)){

    selected_columns <- colnames(df)

  } else {

    selected_columns <- intersect(
      x = colnames(df),
      y = c(
        responses,
        selected_columns
      )
    )

  }

  if(length(selected_columns) == 0){

    stop(
      function_name,
      ": no valid columns in argument 'df'.",
      call. = FALSE
    )

  }

  df <- df[, selected_columns, drop = FALSE]

  column_order <- selected_columns

  #identify predictors
  column_types <- identify_predictors(
    df = df,
    predictors = selected_columns
  )

  #logicals to numeric
  if(length(column_types$logical) > 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": converted the following logical columns to numeric:\n -",
        paste0(column_types$logical, collapse = "\n - ")
      )

    }

    df <- rapply(
      object = df,
      f = as.numeric,
      classes = c("logical"),
      how = "replace"
    )

  }

  if(length(column_types$numeric) > 0){

    df_numeric <- df[, column_types$numeric, drop = FALSE]


    # replace inf with NA ----
    n_inf <- sum(
      vapply(
        X = df_numeric,
        FUN = function(x) sum(!is.finite(x)),
        FUN.VALUE = integer(1))
    )

    if(n_inf > 0){

      if(quiet == FALSE){

        #identify involved columns
        columns_inf <- colnames(df_numeric)[
          vapply(
            X = df_numeric,
            FUN = function(x) any(!is.finite(x)),
            FUN.VALUE = logical(1)
          )
        ]

        message(
          "\n",
          function_name,
          ": replaced ", n_inf, " Inf, -Inf, or NaN values with NA in these columns: \n - ",
          paste0(columns_inf, collapse = "\n - ")
        )

      }

      # replace Inf, -Inf and NaN with NA
      is.na(df_numeric) <- vapply(
        X = df_numeric,
        FUN = function(x) !is.finite(x),
        FUN.VALUE = logical(nrow(df_numeric))
      )

    }

  } else {
    df_numeric <- df[, 0]
  }

  #recover non-numeric columns
  df <- cbind(
    df_numeric,
    df[, c(column_types$categorical, column_types$logical), drop = FALSE]
  )

  #reorder columns
  column_order <- intersect(
    x = colnames(df),
    y = column_order
  )

  df <- df[, column_order, drop = FALSE]


  attr(
    x = df,
    which = "validated"
  ) <- TRUE

  df

}
