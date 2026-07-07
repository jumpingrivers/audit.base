get_uv_python_versions = function() {
  url = "https://raw.githubusercontent.com/astral-sh/uv/main/crates/uv-python/download-metadata.json"
  raw_json = jsonlite::fromJSON(url, simplifyVector = FALSE)

  if (length(raw_json) == 0) {
    warning("Fetched Python JSON is empty -- check the URL/branch/tag.")
    return(NULL)
  }

  versions = purrr::imap_dfr(raw_json, function(entry, key) {
    tibble::tibble(
      key = key,
      name = entry$name %||% NA_character_,
      major = entry$major %||% NA_integer_,
      minor = entry$minor %||% NA_integer_,
      patch = entry$patch %||% NA_integer_,
      prerelease = entry$prerelease %||% NA_character_,
      version = paste(
        entry$major %||% NA,
        entry$minor %||% NA,
        entry$patch %||% NA,
        sep = "."
      ) |>
        paste0(entry$prerelease %||% ""),
      os = entry$os %||% NA_character_,
      libc = entry$libc %||% NA_character_,
      arch_family = entry$arch$family %||% NA_character_,
    )
  })
  versions |>
    dplyr::filter(
      !nzchar(.data$prerelease),
      .data$os == "linux",
      .data$libc == "gnu",
      .data$arch_family == "x86_64"
    ) |>
    dplyr::distinct(.data$major, .data$minor, .data$patch, .data$version) |>
    dplyr::group_by(.data$major, .data$minor) |>
    dplyr::filter(.data$patch == max(.data$patch)) |>
    dplyr::arrange(-.data$major, -.data$minor) |>
    dplyr::pull(.data$version)
}
