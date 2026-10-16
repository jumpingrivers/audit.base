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
  # Clean up response
  pkg_look_up = get_os_sys_deps(os_release)
  # get_os_sys_deps() and clean_libs() have warned why
  if (!is.data.frame(pkg_look_up) || anyNA(installed_libs)) {
    return(NA)
  }

  # Determine which libs are missing
  missing_libs = pkg_look_up[!(pkg_look_up$sys_libs %in% installed_libs), ]
  missing_libs = dplyr::arrange(missing_libs, .data$sys_libs, .data$pkg)
  cli::cli_alert_info("Unable to install {nrow(missing_libs)} CRAN package{?s}")

  if (nrow(missing_libs) > 0) {
    sys_libs = unique(missing_libs$sys_libs) # nolint
    cli::cli_alert_info("Missing sys_libs: {sys_libs}")
    cli::cli_alert_info("Note: this is not necessarily a bad thing")
  }
  missing_libs
}

# Contents of /etc/os-release
get_os_sys_deps = function(os_release) {
  os_release_df = os_release_to_df(os_release)
  system = get_p3m_system(os_release_df)
  if (is.na(system$distribution)) {
    os_name = os_release_value(os_release_df, "pretty_name") # nolint
    cli::cli_warn("Can't check system libraries: Posit Package Manager doesn't support {os_name}")
    return(NA)
  }
  reqs = get_pkg_requirements(distribution = system$distribution, release = system$release)
  if (!is.data.frame(reqs)) {
    return(NA)
  }
  reqs = reqs[!is.na(reqs$sys_libs), ]
  if (system$distribution %in% c("ubuntu", "debian")) {
    reqs$sys_libs = switch_ubuntu_aliases(reqs$sys_libs)
  }
  reqs
}

#' Determines Installed Sys Libs
#'
#' Run on Posit workbench. Determines the system libs that
#' are installed on a machine.
#' @export
get_installed_libs = function() {
  os_release = get_os_release()
  os_release_df = os_release_to_df(os_release)
  tool = get_package_tool(os_release_df)
  if (identical(tool, "apt")) {
    libs = processx::run("apt", args = c("list", "--installed"))
    libs = stringr::str_split(libs$stdout, "\n")[[1]][-1]
  } else if (identical(tool, "rpm")) {
    libs = processx::run("rpm", args = c("-qa", "--qf", "%{NAME}\\n"))$stdout
    libs = stringr::str_split(libs, "\n")[[1]]
  } else {
    libs = character(0)
  }
  clean_libs(os_release, libs)
}

#' @export
#' @inheritParams check_sys_deps
#' @param libs A vector of installed sys deps
#' @rdname get_installed_libs
clean_libs = function(os_release, libs) {
  os_release_df = os_release_to_df(os_release)
  tool = get_package_tool(os_release_df)
  if (identical(tool, "apt")) {
    libs = stringr::str_match(libs, "^[^/]*")[, 1]
    libs = switch_ubuntu_aliases(libs)
  } else if (identical(tool, "rpm")) {
    # rpm -qa gives bare names; `yum list installed` lines start name.arch.
    # Names can contain dots (java-1.8.0-openjdk), so strip only the architecture.
    libs = libs[!stringr::str_detect(libs, "^(Installed Packages|Last metadata)")]
    libs = stringr::str_extract(libs, "^\\S+")
    libs = stringr::str_remove(libs, "\\.(x86_64|noarch|i[3-6]86|aarch64|ppc64le|s390x)$")
    libs = libs[!is.na(libs) & nzchar(libs)]
  } else {
    os_name = os_release_value(os_release_df, "pretty_name") # nolint
    cli::cli_warn("Can't list the installed system libraries on {os_name}")
    return(NA_character_)
  }
  sort(libs)
}

# In ubuntu, pkgs can have virtual packages.Just tidy up
switch_ubuntu_aliases = function(libs) {
  fname = system.file("extdata", "lib-aliases.csv", package = "audit.base", mustWork = TRUE)
  aliases = readr::read_csv(fname, col_types = c("c", "c"))
  for (i in seq_len(nrow(aliases))) {
    libs[libs == aliases[i, ]$pkg1] = aliases[i, ]$pkg2
  }
  libs
}
