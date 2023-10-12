#' The collinear package
#'
#' An R package for multicollinearity management
#'
#' @docType package
#' @keywords internal
#' @aliases collinear-package
#' @keywords internal
#' @importFrom stats cor na.omit sd rnorm chisq.test lm coef
#' @importFrom dplyr all_of select rename mutate case_when transmute arrange inner_join group_by n ungroup filter pull arrange desc summarize
#' @importFrom future plan nbrOfFreeWorkers multisession sequential
#' @importFrom future.apply future_lapply
"_PACKAGE"
