#' The collinear package
#'
#' An R package for multicollinearity management
#'
#' @docType package
#' @keywords internal
#' @aliases collinear-package
#' @keywords internal
#' @importFrom stats cor na.omit sd rnorm chisq.test
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr all_of select rename mutate case_when transmute arrange inner_join group_by n ungroup filter pull arrange desc summarize
#' @importFrom tibble rownames_to_column
"_PACKAGE"
