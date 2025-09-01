#' Class \code{collinear_selection}
#' TODO: update class definition
#'
#' Output of the `collinear()` function when \code{response} is NULL or length 1. It has the custom print method [print.collinear_selection].
#'
#' @slot response (character or NULL): Name of the response variable.
#' @slot predictors (character vector or NULL): Names of predictors involved in multicollinearity filtering.
#' @slot selection (character vector or NULL): Names of selected non-collinear predictors.
#' @slot df (data frame or NULL): Data frame with all variables in \code{response} and \code{selection}.
#' @slot args (list): Values of arguments used in the function call: \code{encoding_method}, \code{preference_order}, \code{f}, \code{max_cor}, and \code{max_vif}.
#'
#' @name collinear_selection-class
#' @aliases collinear_selection
#' @rdname collinear_selection-class
#' @exportClass collinear_selection
NULL


#' Class \code{collinear_list}
#' TODO: update class definition to collinear_arguments
#' Output of [collinear()] when \code{response} is a character vector of length two or more. Named list of [collinear_selection] objects, one for each response variable.
#'
#' @name collinear_list-class
#' @aliases collinear_list
#' @rdname collinear_list-class
#' @exportClass collinear_list
NULL

#' TODO: add remaining classes
