# https://gitlab.com/jumpingrivers/services/de/spd/infrastructure-template/
# -/blob/5c584fced32a6fc8fd7b25b3ea78f6fb7a8bd7ca/template/ansible/scripts/versions.sh
get_latest_versions_remote = function() {
  r = get_latest_versions_from_posit("r")
  py = get_uv_python_versions()
  quarto = get_quarto_versions()

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

# The latest patch of each Quarto minor release. Most releases are prereleases, so the newest 100
# reach back several minors; older minors come from the fixed list
get_quarto_versions = function() {
  releases = fetch_json("https://api.github.com/repos/quarto-dev/quarto-cli/releases?per_page=100")
  stable = purrr::discard(releases, ~ isTRUE(.x$prerelease))
  tags = stringr::str_remove(purrr::map_chr(stable, "tag_name"), "^v")
  old = c("1.0.38", "1.1.189", "1.2.475", "1.3.450", "1.4.557", "1.5.57", "1.6.43")
  latest_patch_per_minor(c(tags, old))
}

latest_patch_per_minor = function(versions) {
  versions = unique(versions[stringr::str_detect(versions, "^[0-9]+\\.[0-9]+\\.[0-9]+$")])
  parts = matrix(as.integer(stringr::str_split_fixed(versions, "\\.", 3)), ncol = 3)
  newest_first = order(parts[, 1], parts[, 2], parts[, 3], decreasing = TRUE)
  versions = versions[newest_first]
  versions[!duplicated(parts[newest_first, 1:2, drop = FALSE])]
}

get_latest_versions_from_posit = function(type = c("r", "python")) {
  type = match.arg(type)
  url = glue::glue("https://cdn.posit.co/{type}/versions.json")
  versions = unlist(fetch_json(url))
  versions = unname(versions)
  tibble::tibble(
    patch = get_patch(versions),
    major = get_major(versions),
    versions = versions
  ) %>%
    dplyr::filter(!is.na(.data$patch)) %>%
    dplyr::arrange(.data$major, -.data$patch) %>%
    dplyr::group_by(.data$major) %>%
    dplyr::mutate(patch = max(.data$patch)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(versions)
}

# JSON from a URL as nested lists
fetch_json = function(url) {
  jsonlite::fromJSON(url, simplifyVector = FALSE)
}
