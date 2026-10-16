# https://packagemanager.posit.co/__api__/swagger/index.html#/default/get_repos
# NA, with P3M's reason as a warning, when P3M doesn't support the system
get_pkg_requirements = function(
  distribution,
  release,
  base_url = "https://packagemanager.posit.co/__api__/repos/",
  repo_id = 2
) {
  config_url = glue::glue(
    "{repo_id}/sysreqs?all=true&distribution={distribution}&release={release}"
  ) # nolint
  url = glue::glue("{base_url}{config_url}")
  cli::cli_alert_info(
    "Connecting to {url} for system requirements"
  )

  res = httr::GET(url, httr::timeout(60))
  r = httr::content(res)
  if (httr::http_error(res)) {
    reason = if (is.list(r) && is.character(r$error)) r$error else httr::http_status(res)$message # nolint
    cli::cli_warn(
      "Posit Package Manager can't list system requirements for {distribution} {release}: {reason}"
    )
    return(NA)
  }

  purrr::map_df(
    r$requirements,
    ~ tibble::tibble(pkg = .x$name, sys_libs = unlist(.x$requirements$packages))
  )
}
