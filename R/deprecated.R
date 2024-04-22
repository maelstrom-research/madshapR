#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [bookdown_open()] instead of `open_visual_report()`.
#'
#' @name deprecated
#' @keywords internal
#' @import dplyr
#' @importFrom lifecycle deprecate_warn
#' @export
open_visual_report <- function(...) {
  
  deprecate_warn(
    "1.0.2", "open_visual_report(to)", "bookdown_open(bookdown_path)")
  # Unquote-splice to avoid argument matching
  bookdown_open(bookdown_path = ...)
  
}

#' @title
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [madshapR_website()] instead of `madshapR_help()`.
#'
#' @name deprecated
#' @keywords internal
#' @import dplyr
#' @importFrom lifecycle deprecate_warn
#' @export
madshapR_help <- function() {
  
  deprecate_warn(
    "1.0.4", "madshapR_help()", "madshapR_website()")
  
  # Unquote-splice to avoid argument matching
  
  madshapR_website()
  
}
