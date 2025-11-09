#' Validate Structure and Values of Argument \code{df}
#'
#' @description
#' Internal function to validate the integrity of the argument \code{df}. It ensures that the dataframe has suitable dimensions for a multicollinearity analysis, transforms logical columns to numeric, and converts \code{NaN}, \code{Inf} and \code{-Inf} to NA. Additionally, it checks the values of \code{responses} and \code{predictors} if these arguments are provided.
#'
#' @inheritParams collinear
#' @inheritParams f_auto
#' @inheritParams validate_arg_quiet
#' @return dataframe
#' @examples
#'
#' data(vi_smol, vi_predictors)
#'
#' df <- validate_arg_df(
#'   df = vi_smol,
#'   responses = "vi_numeric",
#'   predictors = vi_predictors_numeric,
#'   quiet = FALSE
#' )
#'
#' attributes(vi)$validated
#' @autoglobal
#' @family argument_validation
#' @export
validate_arg_df <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    quiet = FALSE,
    function_name = NULL
){

  if(isTRUE(attr(x = df, which = "validated"))){
    return(df)
  }

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_df()",
    function_name = function_name
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  quiet <- validate_arg_quiet(
    quiet = quiet,
    function_name = function_name
  )

  df <- drop_geometry_column(
    df = df,
    quiet = quiet,
    function_name = function_name
  )

  #handle coercion to df
  if(is.data.frame(df) == FALSE){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'df' is not a dataframe, attempting coercion."
      )

    }

    df <- tryCatch(
      {
        out <- as.data.frame(df)

        if(all(dim(out)) == 1){
          stop()
        }

      },
      error = function(e){
        stop(
          "\n",
          function_name,
          ": cannot coerce argument 'df' to class 'data.frame'.",
          call. = FALSE
        )
      }
    )

  }

  #stop if no columns
  if(ncol(df) == 0){

    stop(
      "\n",
      function_name,
      ": argument 'df' has zero columns.",
      call. = FALSE
    )

  }

  if(ncol(df) == 1){

    stop(
        "\n",
        function_name,
        ": argument 'df' has one valid column, multicollinearity analysis cannot be performed.",
        call. = FALSE
    )

  }

    #error if not enough rows
    if(nrow(df) < 3){

      stop(
        function_name,
        ": argument 'df' has fewer than 3 rows, multicollinearity analysis cannot be performed.",
        call. = FALSE
      )

      #warning if not enough rows
    } else if(nrow(df) < 10){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'df' has fewer than 10 rows, multicollinearity analysis may be unreliable due to insufficient sample size."
        )

      }

      #message if not enough rows
    } else if(nrow(df) < 30){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'df' has fewer than 30 rows, results of multicollinearity filtering may be statistically fragile."
        )

      }

    }

  responses <- validate_arg_responses(
    df = df,
    responses = responses,
    quiet = quiet,
    function_name = function_name
  )

  if(!is.null(predictors)){

    predictors <- validate_arg_predictors(
      df = df,
      responses = responses,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }

  if(all(is.null(c(responses, predictors)))){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": arguments 'responses' and 'predictors' are NULL, skipping validation of column values."
      )

    }

    return(df)

  }


  #subset valid columns
  selected_columns <- unique(c(responses, predictors))

  #general data validation
  df <- df[, selected_columns, drop = FALSE]

  column_order <- selected_columns


  #identify predictors types
  column_types <- identify_valid_variables(
    df = df,
    responses = responses,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  if(length(
    c(
      column_types$numeric,
      column_types$categorical
    )
  ) == 0
  ){

    stop(
      "\n",
      function_name,
      ": argument 'df' has no valid columns.",
      call. = FALSE
    )

  }

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

    #convert logical to numeric
    df <- rapply(
      object = df,
      f = as.numeric,
      classes = c("logical"),
      how = "replace"
    )

    #update identification of predictors
    column_types <- identify_valid_variables(
      df = df,
      predictors = selected_columns,
      function_name = function_name
    )

    selected_columns <- c(
      column_types$numeric,
      column_types$categorical
    )

  }

  #replace invalid numeric values
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

    #recover non-numeric columns
    df <- cbind(
      df_numeric,
      df[, c(column_types$categorical), drop = FALSE]
    )

    #reorder columns
    column_order <- intersect(
      x = column_order,
      y = colnames(df)
    )

    df <- df[, column_order, drop = FALSE]

  }

  attr(
    x = df,
    which = "validated"
  ) <- TRUE

  df

}
