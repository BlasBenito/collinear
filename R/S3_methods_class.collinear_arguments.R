#' Builds \code{class.collinear_arguments}
#'
#' @description
#' Internal function to validate the arguments of of a [collinear()] call.
#'
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_f
#'
#' @returns list:
#' - \code{df}: validated data frame.
#' - \code{responses}: character vector of response names or NULL
#' - \code{predictors}: character vector of predictor names.
#' - \code{encoding_method}: character string or NULL.
#' - \code{preference_order}: character vector, data frame resulting from [preference_order], or NULL.
#' - \code{f}: function or NULL.
#' - \code{f_name}: character string with the name of \code{f} or NULL.
#' - \code{max_cor}: numeric or NULL.
#' - \code{max_vif}: numeric or NULL,
#' - \code{timestamp}: execution time and date.
#' - \code{quiet}: logical.
#' @seealso [print.collinear_arguments()]
#' @family S3_methods
#' @autoglobal
#' @export
class.collinear_arguments <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
    f_name = NULL,
    max_cor = 0.75,
    max_vif = 5,
    function_name = NULL,
    quiet = FALSE
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::class.collinear_arguments()",
    function_name = function_name
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  ## max_cor max_vif ----
  if(all(is.null(c(max_cor, max_vif)))){
    stop(
      function_name,
      ": arguments 'max_cor' and 'max_vif' cannot be NULL at once.",
      call. = FALSE
    )
  }

  if(!is.null(max_cor)){
    max_cor <- validate_arg_max_cor(
      max_cor = max_cor,
      function_name = function_name,
      quiet = quiet
    )
  }

  if(!is.null(max_vif)){
    max_vif <- validate_arg_max_vif(
      max_vif = max_vif,
      function_name = function_name,
      quiet = quiet
    )
  }

  ## quiet ----
  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  ## df ----
  df <- validate_arg_df(
    df = df,
    responses = responses,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  ## response ----
  if(!is.null(responses)){

    responses_original <- responses

    responses <- intersect(
      x = colnames(df),
      y = responses_original
    )

    responses_missing <- setdiff(
      x = responses,
      y = responses_original
    )

    if(length(responses_missing) > 0 && quiet == FALSE){

      message(
        "\n",
        function_name,
        ": the following values of the argument 'responses' are not column names of 'df' and will be ignored: \n - ",
        paste(responses_missing, collapse = "\n - ")
      )

    }

  }

  ## encoding_method ----
  encoding_method <- validate_arg_encoding_method(
    encoding_method = encoding_method,
    overwrite = TRUE,
    function_name = function_name,
    quiet = quiet
  )

  ## predictors ----
  predictors <- validate_arg_predictors(
    df = df,
    response = NULL,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  ## preference_order ----
  if(!is.null(preference_order)){

    preference_order_auto <- vif_df(
      df = df,
      predictors = predictors,
      quiet = TRUE
    )

    preference_order <- validate_arg_preference_order(
      responses = responses,
      predictors = predictors,
      preference_order = preference_order,
      preference_order_auto = rev(preference_order_auto$predictor),
      function_name = function_name,
      quiet = quiet
    )

    if(!is.null(f)){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": resetting 'f' to NULL (overridden by 'preference_order')."
        )

      }


      f <- NULL
      f_name <- NULL

    }

  }


  ## f ----
  if(!is.null(f)){

    f <- validate_arg_f(
      f = f,
      f_name = f_name,
      function_name = function_name
    )

    f_name <- attributes(f)$name

  }


  #store args list for output
  args <- list(
    df = df,
    responses = responses,
    predictors = predictors,
    encoding_method = encoding_method,
    preference_order = preference_order,
    f = f,
    f_name = f_name,
    max_cor = max_cor,
    max_vif = max_vif,
    timestamp = Sys.time(),
    quiet = quiet
  )

  class(args) <- c(
    class(args),
    "collinear_arguments"
  )

  args

}
