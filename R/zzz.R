## quiets concerns (notes) of R CMD check re: the vars that are evaluated using non-standard evaluation
if (getRversion() >= "2.15.1") utils::globalVariables(c("inference", "key", "replace_null_cases", "percent_bias", "val"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) and in Frank (2000).\nFor more information visit http://konfound-it.com.")
}

#' Open interactive web application for konfound
#' @details Open the Shiny interactive web application in a browser
#' @return Launches a web browser
#' @export

launch_shiny <- function() {
  utils::browseURL("http://konfound-it.com")
}

# addresses concerns (notes) of R CMD check re: the vars that are evaluated using non-standard evaluation
# if (getRversion() >= "2.15.1") utils::globalVariables(c("itcv", "term", "unstd_beta1", "var_name", "x", "y"))
