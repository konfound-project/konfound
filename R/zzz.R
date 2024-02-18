#' Package Initialization Functions and Utilities
#'
#' These functions are used for initializing the package environment and
#' providing utility functions for the package.
#'
#' @name zzz
#' @aliases zzz
#' @rdname zzz
#' @importFrom utils globalVariables
if (getRversion() >= "2.15.1") utils::globalVariables(c("inference", "key", 
                                                        "replace_null_cases", 
                                                        "percent_bias", "val"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) and in Frank (2000).\nFor more information visit http://konfound-it.com.")
}
