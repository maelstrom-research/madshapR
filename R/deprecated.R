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

