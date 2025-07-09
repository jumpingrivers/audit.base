#' Checking System Dependencies
#'
#' Many R packages require additional system dependencies.
#' This check, obtains a list of system libraries installed on the connect server, then
#' determines which R packages can't be installed.
#' @param os_release Output from /etc/os-release
#' @param installed_libs A vector of packages, e.g. output apt list --installed, or
#' `get_installed_libs()`
#' @param debug_level Integer - 0, 1, 2
#' @details Suppress parameter will be changed in a near-future MR to verbose levels
#' @export
check_sys_deps = function(os_release, installed_libs, debug_level = 0:2) {
  debug_level = get_debug_level(debug_level)
  cli::cli_h2("Systems Libraries")
  cli::cli_alert_info("This may take a few minutes as we are deploying a plumber API onto Connect")
  # Clean up response
  pkg_look_up = get_os_sys_deps(os_release)

  # Determine which libs are missing
  missing_libs = pkg_look_up[!(pkg_look_up$sys_libs %in% installed_libs), ]
  missing_libs = dplyr::arrange(missing_libs, .data$sys_libs, .data$pkg)
  cli::cli_alert_info("Unable to install {nrow(missing_libs)} CRAN packages")

  if (nrow(missing_libs) > 0) {
    sys_libs = unique(missing_libs$sys_libs) # nolint
    cli::cli_alert_info("Missing sys_libs: {sys_libs}")
    cli::cli_alert_info("Note: this is not necessarily a bad thing")
  }
  return(missing_libs)
}

# Contents of /etc/os-release
get_os_sys_deps = function(os_release) {
  os_release_df = os_release_to_df(os_release)
  version_id = os_release_df[os_release_df$name == "version_id", ]$value
  if (is_ubuntu(os_release_df)) {
    id = "ubuntu"
  } else if (is_redhat(os_release_df)) {
    id = "redhat"
    version_id = stringr::str_remove(version_id, "\\..*")
  } else if (is_centos(os_release_df)) {
    id = "centos"
    version_id = stringr::str_remove(version_id, "\\..*")
  } else {
    cli::cli_abort("This OS isn't supported")
  }
  reqs = get_pkg_requirements(distribution = id, release = version_id)
  reqs[!is.na(reqs$sys_libs), ]
}

#' Determines Installed Sys Libs
#'
#' Run on Posit workbench. Determines the system libs that
#' are installed on a machine.
#' @export
get_installed_libs = function() {
  os_release = get_os_release()
  os_release_df = os_release_to_df(os_release)
  if (is_ubuntu(os_release_df)) {
    libs = processx::run("apt", args = c("list", "--installed"))
    libs = stringr::str_split(libs$stdout, "\n")[[1]][-1]
  } else if (is_redhat(os_release_df) || is_centos(os_release_df)) {
    libs = processx::run("yum", args = c("list", "installed"))$stdout
    libs = stringr::str_split(libs, "\n")[[1]][-(1:2)]
  } else {
    cli::cli_abort("We don't support this OS yet")
  }
  clean_libs(os_release, libs)
}

#' @export
#' @inheritParams check_sys_deps
#' @param libs A vector of installed sys deps
#' @rdname get_installed_libs
clean_libs = function(os_release, libs) {
  os_release_df = os_release_to_df(os_release)
  if (is_ubuntu(os_release_df)) {
    libs = stringr::str_match(libs, "^[^/]*")[, 1]
  } else if (is_redhat(os_release_df) || is_centos(os_release_df)) {
    libs = stringr::str_match(libs, "^[^\\.]*")[, 1]
  } else {
    cli::cli_abort("We don't support this OS yet")
  }
  sort(libs)
}
