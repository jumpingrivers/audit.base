#' Debugging helper functions
#'
#' @param debug_level An integer, 0, 1, 2.
#' @export
get_suppress = function(debug_level) {
  if (debug_level == 0) suppressMessages
  else function(expr) expr
}

#' @rdname get_suppress
#' @export
get_debug_level = function(debug_level) {
  # match.arg doesn't work on integers
  debug_level = match.arg(as.character(debug_level), as.character(0:2))
  as.integer(debug_level)
}
