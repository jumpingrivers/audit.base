#' Quarto Helper Functions
#'
#' In general not used by users.
#' @param out Output from check
#' @export
get_quarto_server_header = function(out) {
  headers = out$server_headers$headers
  headers = dplyr::bind_rows(headers, get_posit_headers(headers))
  headers = dplyr::filter(headers, .data$primary_header)
  headers = dplyr::arrange(headers, dplyr::desc(.data$status)) %>%
    dplyr::mutate(
      header_docs = purrr::map(.data$documentation, ~ htmltools::a(href = .x, "(docs)")),
      message = purrr::map2(
        message, .data$header_docs,
        ~ gt::html(paste(.x, as.character(.y)))
      )
    ) %>%
    dplyr::mutate(value = ifelse(is.na(.data$value), "-", .data$value)) %>%
    dplyr::distinct()
  dplyr::select(headers, -"documentation", -"header_docs", -"primary_header")
}

#' @rdname get_quarto_server_header
#' @export
get_quarto_sys_deps = function(out) {
  sys_deps = out$sys_deps
  sys_deps %>%
    dplyr::group_by(.data$sys_libs) %>%
    dplyr::reframe(
      pkg = paste(sort(.data$pkg), collapse = ", "),
      n = length(.data$sys_libs)
    )
}

#' @rdname get_quarto_server_header
#' @export
get_quarto_software_versions = function(out) {
  software = out$versions
  software = dplyr::select(software, "software", "version", "installed_version", "upgrade")
  software$installed_version = ifelse(is.na(software$installed_version),
    "Not installed", software$installed_version
  )
  software
}

#' @rdname get_quarto_server_header
#' @export
get_quarto_deploy = function(out) {
  deploy = out$results
  deploy$type = paste0(deploy$group, ": ", deploy$short)
  deploy = dplyr::arrange(deploy, !.data$passed, .data$group)
  deploy
}

#' @rdname get_quarto_server_header
#' @inheritParams audit_posit_version
#' @export
get_quarto_posit_version_msg = function(out, type = c("connect", "workbench", "drivers")) {
  posit_version = out$posit_version
  row_number = lookup_version(posit_version, type)
  if (is.na(row_number)) {
    msg = "Posit {type} (v{posit_version}), isn't in our database.
    This could be because we've missed it or it's really old."
  } else if (row_number > 1L) {
    versions = get_posit_versions(type = type)
    newer_versions = versions[seq_len(row_number - 1), ] # nolint
    no_of_versions = length(unique(newer_versions$version)) # nolint
    no_of_cves = sum(!is.na(newer_versions$cve)) # nolint
    msg = "Posit {type} is out of date (v{posit_version}).
             There are {no_of_versions} newer versions that fix {no_of_cves} CVEs.
             The latest version is v{versions[1, 1]}."
  } else {
    msg = "Posit {type} is up to date."
  }
  glue::glue(msg)
}
