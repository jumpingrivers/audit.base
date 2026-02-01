#' R6 Helper functions
#'
#' These functions are used by the health check function
#' to initialise and check
#' @param dir Directory of the config file
#' @param file Config file name
#' @param pkg_name E.g. uat.base description
#' @param export Name of the R6 class to initialise
#' @export
init_r6_checks = function(dir, file, pkg_name) {
  exports = getNamespaceExports(pkg_name)
  check_exports = sort(exports[stringr::str_starts(exports, "check_")])
  r6_inits = purrr::map(
    check_exports,
    init_r6_check,
    dir = dir,
    file = file,
    pkg_name
  )
  purrr::discard(r6_inits, ~ is.null(.x))
}

#' @rdname init_r6_checks
#' @export
init_r6_check = function(export, dir, file, pkg_name) {
  obj = eval(parse(text = export), envir = asNamespace(pkg_name))
  if (inherits(obj, "R6ClassGenerator")) {
    obj = obj$new(dir, file)
  } else {
    obj = NULL
  }
  obj
}
