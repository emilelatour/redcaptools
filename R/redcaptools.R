#' redcaptools: Misc tools for working with REDCap data in R
#'
#' This package contains a some functions that I find handy when working with
#' REDCap data.
#'
#' @examples
#' # Example usage:
#' library(redcaptools)
#'
#' @docType package
#' @name redcaptools
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
## From infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".",
      "field_label",
      "is_checkbox",
      "field_name",
      "clean_label",
      "checkbox_choice",
      "nn"))

}






