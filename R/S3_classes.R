#' Class \code{collinear_output}
#'
#' List resulting from a [collinear()] call.
#'
#' When argument \code{response} is NULL, the list has two elements:
#' \itemize{
#'   \item \code{result}: list of class [collinear_selection] with the results of the multicollinearity filtering.
#'   \item \code{arguments}: list of class [collinear_arguments] with the validated arguments used in the call. These may differ from the user input, and are provided for transparency.
#' }
#'
#' The class has two printing methods:
#' \itemize{
#'   \item [print.collinear_outpout()]: applies [print.collinear_selection()] and [print.collinear_arguments] to the object.
#'   \item [summary.collinear_output()]: shows a summary of the multicollinearity filtering results via [summary.collinear_selection()].
#' }
#'
#' @name collinear_output-class
#' @aliases collinear_output
#' @rdname collinear_output-class
#' @exportClass collinear_output
NULL

#' Class \code{collinear_selection}
#'
#' List with the arguments used in a [collinear()] call.
#'
#' @name collinear_selection-class
#' @aliases collinear_selection
#' @rdname collinear_selection-class
#' @exportClass collinear_selection
NULL


#' Class \code{collinear_arguments}
#'
#' List with the arguments used in a [collinear()] call.
#'
#' @name collinear_arguments-class
#' @aliases collinear_arguments
#' @rdname collinear_arguments-class
#' @exportClass collinear_arguments
NULL


