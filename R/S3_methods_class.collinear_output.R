#' Builds Class \code{class.collinear_output}
#'
#' Builds the object resulting from a [collinear()] call.
#'
#' @param collinear_selection (required, list) Output of  [class.collinear_selection()]. Default: NULL
#' @param collinear_arguments (required, list) Output of [class.collinear_arguments()]. Default: NULL
#'
#' @returns list:
#' \itemize{
#'   \item One or several objects of class [class.collinear_selection].
#'   \item One object of class [class.collinear_arguments]
#' }
#' @family S3_methods
#' @autoglobal
#' @export
class.collinear_output <- function(
    collinear_selection = NULL,
    collinear_arguments = NULL
){

  if(any(is.null(c(collinear_selection, collinear_arguments)))){

    stop(
      "collinear::class.collinear_output(): none of the arguments 'collinear_selection' and 'collinear_arguments' can be NULL.",
      call. = FALSE
    )

  }

  if(!inherits(x = collinear_arguments, what = "collinear_arguments")){

    stop(
      "collinear::class.collinear_output(): argument 'collinear_arguments' must be of the class 'collinear_arguments'.",
      call. = FALSE
    )

  }

  check_collinear_selection <- lapply(
    X = collinear_selection,
    FUN = class
  ) |>
    unlist() |>
    unique()

  if(!"collinear_selection" %in% check_collinear_selection){
    stop(
      "collinear::class.collinear_output(): argument 'collinear_selection' must be a list containing objects of class 'collinear_selection'.",
      call. = FALSE
    )
  }

  collinear_selection$arguments <- collinear_arguments

  class(collinear_selection) <- c(
    class(collinear_selection),
    "collinear_output"
  )

  collinear_selection

}
