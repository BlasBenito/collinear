#to ensure there are no check NOTES about spatialData

#' Declare spatialData
#' @returns dataset spatialData::vi_smol
#' @autoglobal
#' @keywords internal
declare_spatialData <- function() {
  spatialData::vi_smol
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("collinear ", utils::packageVersion("collinear"))
}
