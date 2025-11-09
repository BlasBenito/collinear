#' Identify Valid Categorical Variables
#'
#' @description
#' Identifies valid and invalid character or factor variables. Invalid categorical predictors are those with a single category, or as many categories as cases (full-cardinality).
#'
#'
#' @inheritParams identify_valid_variables
#' @return list:
#' \itemize{
#'   \item \code{valid}: character vector with valid categorical predictor names.
#'   \item \code{invalid}: character vector with invalid categorical predictor names due to degenerate cardinality (1 or \code{nrow(df)} categories).
#' }
#' @examples
#'
#' data(vi_smol, vi_predictors)
#'
#' #create an invalid categorical
#' vi_smol$invalid_categorical <- "a"
#'
#' x <- identify_categorical_variables(
#'   df = vi_smol,
#'   responses = "vi_categorical",
#'   predictors = vi_predictors
#' )
#'
#' x$valid
#' x$invalid
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_categorical_variables <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_categorical_variables()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  quiet <- validate_arg_quiet(
    quiet = quiet,
    function_name = function_name
  )

  if(!is.null(responses)){

    responses <- validate_arg_responses(
      df = df,
      responses = responses,
      quiet = quiet,
      function_name = function_name
    )

  }

  if(!is.null(predictors)){

    predictors <- validate_arg_predictors(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }


  vars_string <- if (!is.null(predictors) && !is.null(responses)) {
    "variables"
  } else if (!is.null(predictors)) {
    "predictors"
  } else if (!is.null(responses)) {
    "responses"
  } else {
    "variables"
  }

  predictors <- c(responses, predictors)

  if(is.null(predictors) || length(predictors) == 0){

    stop(
      "\n",
      function_name,
      ": there are no ",
      vars_string,
      " to identify.",
      call. = FALSE
    )

  }

  out_list <- list(
    valid = NULL,
    invalid = NULL
  )

  #subset character or factor
  predictors_categorical_all <- predictors[
    vapply(
      X = df[, predictors, drop = FALSE],
      FUN = function(x){
        is.character(x) || is.factor(x)
      },
      FUN.VALUE = logical(1)
    )
  ] |>
    stats::na.omit()

  if(length(predictors_categorical_all) == 0){
    return(out_list)
  }

  n <- nrow(df)

  #keep predictors with unique length different than one or nrow(df)
  length_unique <- vapply(
    X = df[, predictors_categorical_all, drop = FALSE],
    FUN = function(x){length(unique(x))},
    FUN.VALUE = integer(1)
  )

  predictors_categorical_valid <- predictors_categorical_all[
    !(length_unique %in% c(1, n))
  ]

  predictors_categorical_invalid <- setdiff(
    x = predictors_categorical_all,
    y = predictors_categorical_valid
  )

  if(
    quiet == FALSE &&
    length(predictors_categorical_invalid) > 0
  ){

    message(
      "\n",
      function_name,
      ": invalid categorical ",
      vars_string,
      " due to degenerate cardinality:\n - ",
      paste(
        predictors_categorical_invalid,
        collapse = "\n - "
      )
    )

  }

  if(length(predictors_categorical_valid) > 0){
    out_list$valid <- predictors_categorical_valid
  }

  if(length(predictors_categorical_invalid) > 0){
    out_list$invalid <- predictors_categorical_invalid
  }

  out_list

}
