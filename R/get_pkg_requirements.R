# https://packagemanager.rstudio.com/__api__/swagger/index.html#/default/get_repos
get_pkg_requirements = function(distribution = c("ubuntu", "redhat", "centos"),
                                release = c("18.04", "20.04", "22.04", "24.04", "7", "8", "9"),
                                base_url = "https://packagemanager.rstudio.com/__api__/repos/",
                                repo_id = 2) {
  distribution = match.arg(distribution)
  release = match.arg(release)
  config_url = glue::glue("{repo_id}/sysreqs?all=true&distribution={distribution}&release={release}") # nolint
  url = glue::glue("{base_url}{config_url}")
  res = httr::GET(url)
  r = httr::content(res)

  purrr::map_df(
    r$requirements,
    ~ tibble::tibble(pkg = .x$name, sys_libs = unlist(.x$requirements$packages))
  )
}
