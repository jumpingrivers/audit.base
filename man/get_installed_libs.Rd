% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_sys_deps.R
\name{get_installed_libs}
\alias{get_installed_libs}
\alias{clean_libs}
\title{Determines Installed Sys Libs}
\usage{
get_installed_libs()

clean_libs(os_release, libs)
}
\arguments{
\item{os_release}{Output from /etc/os-release}

\item{libs}{A vector of installed sys deps}
}
\description{
Run on Posit workbench. Determines the system libs that
are installed on a machine.
}
