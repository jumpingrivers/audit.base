# https://gitlab.com/jumpingrivers/services/de/spd/infrastructure-template/-/blob/5c584fced32a6fc8fd7b25b3ea78f6fb7a8bd7ca/template/ansible/scripts/versions.sh
create_software_tibble = function() {
  r = get_latest_versions_from_posit("r")
  py = get_latest_versions_from_posit("python")
  # Drop latest to get all releases
  q = jsonlite::read_json("https://api.github.com/repos/quarto-dev/quarto-cli/releases/latest")
  quarto = c("1.0.38", "1.1.189", "1.2.475", stringr::str_remove(q$name, "^v"))

  software_tibble = tibble::tibble(
    software = rep(
      c("r", "python", "quarto"),
      c(length(r), length(py), length(quarto))
    ),
    version = c(r, py, quarto)
  )
  # Use package_version to get better sorting
  software_tibble %>%
    dplyr::mutate(tmp_version = package_version(.data$version)) %>%
    dplyr::arrange(.data$software, dplyr::desc(.data$tmp_version)) %>%
    dplyr::select(-"tmp_version")
}

get_latest_versions_from_posit = function(type = c("r", "python")) {
  if (!requireNamespace("jsonlite")) {
    cli::cli_alert_info("Missing package - jsonlite")
    return(invisible(NULL))
  }
  type = match.arg(type)
  url = glue::glue("https://cdn.posit.co/{type}/versions.json")
  versions = unlist(jsonlite::read_json(url))
  versions = unname(versions)
  tibble::tibble(patch = get_patch(versions), major = get_major(versions), versions = versions) %>%
    dplyr::filter(!is.na(.data$patch)) %>%
    dplyr::arrange(.data$major, -.data$patch) %>%
    dplyr::group_by(.data$major) %>%
    dplyr::mutate(patch = max(.data$patch)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(versions)
}
