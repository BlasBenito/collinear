#' Checks That Argument \code{df} Is Not NULL
#'
#' @description
#' Internal function to validate the default value of the argument \code{df}.
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#' @return data frame
#' @autoglobal
#' @family data_validation
#' @export
#' @examples
#' data(vi)
#' df <- validate_arg_df_not_null(
#'   df = vi,
#'   function_name = "f()"
#'   )
validate_arg_df_not_null <- function(
    df = NULL,
    function_name = NULL
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_df_not_null()",
    function_name = function_name
  )

  if(is.null(df)){

    stop(
      "\n",
      function_name,
      ": argument 'df' cannot be NULL",
      call. = FALSE
    )

  }

  df

}
