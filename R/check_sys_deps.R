#' Checking System Dependencies
#'
#' Many R packages require additional system dependencies.
#' This check, obtains a list of system libraries installed on the connect server, then
#' determines which R packages can't be installed.
#' @param os Output from /etc/os-release
#' @param libs Output from apt list --installed
#' @param debug_level Integer - 0, 1, 2
#' @details Suppress parameter will be changed in a near-future MR to verbose levels
#' @export
check_sys_deps = function(os, libs, debug_level = 0:2) {
  debug_level = get_debug_level(debug_level)
  cli::cli_h2("Systems Libraries")

  # Clean up response
  pkg_look_up = get_os_sys_deps(os)

  installed_sys_deps = stringr::str_match(libs, "^[^/]*")[, 1]

  # Determine which libs are missing
  missing_libs = pkg_look_up[!(pkg_look_up$sys_libs %in% installed_sys_deps), ]
  missing_libs = dplyr::arrange(missing_libs, .data$sys_libs, .data$r_pkg_names)
  cli::cli_alert_info("Unable to install {nrow(missing_libs)} CRAN packages")

  if (nrow(missing_libs) > 0) {
    sys_libs = unique(missing_libs$sys_libs) #nolint
    cli::cli_alert_info("Missing sys_libs: {sys_libs}")
    cli::cli_alert_info("Note: this is not necessarily a bad thing")
  }
  return(missing_libs)
}

# Contents of /etc/os-release
get_os_sys_deps = function(os) {
  ids = stringr::str_match(os, "^ID=(.*)")[, 2]
  id = tolower(ids[!is.na(ids)])
  # TODO: Handle non ubuntu servers

  version = stringr::str_match(os, "^VERSION_ID=(.*)")[, 2]
  version = stringr::str_remove_all(version, "\"")
  version = stringr::str_replace_all(version, "\\.", "_")
  version = version[!is.na(version)]
  pkg_dir = system.file("extdata", "sys_deps", package = "audit.base",
                        mustWork = TRUE)
  pre_req_path = file.path(pkg_dir, paste0("pre-req-", id, "-", version, ".txt"))

  clean_posit_pre_req_file(pre_req_path)
}

clean_posit_pre_req_file = function(pre_req_path) {
  pre_req = readLines(pre_req_path)
  # Pkg names are on a line starting with "#"
  pkg_names = which(stringr::str_starts(pre_req, "#"))
  # Repeat the package name and clean up
  r_pkg_names = rep(pre_req[pkg_names],
                    times = diff(c(pkg_names, length(pre_req) + 1)))
  r_pkg_names = stringr::str_match(r_pkg_names, "^# ([^\\W]*)")[, 2]

  # Combine pkg name and associated install value
  pkg_libs = tibble::tibble(r_pkg_names, sys_libs = pre_req)
  # Assumes Ubuntu
  pkg_libs = dplyr::filter(pkg_libs, stringr::str_starts(.data$sys_libs, "apt-get"))
  pkg_libs$sys_libs = stringr::str_remove_all(pkg_libs$sys_libs, "apt-get install -y ")
  pkg_libs
}
